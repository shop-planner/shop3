(in-package :common-lisp-user)

(defpackage hddl-translator
  (:nicknames #:shop-hddl #:hddl-shop)
  (:export #:hddl-plan #:print-hddl-plan)
  (:use :common-lisp :alexandria :iterate))

(in-package :hddl-translator)

(deftype only-values (&rest value-spec)
  `(values ,@value-spec &optional))

(deftype only-value (value-spec)
  `(values ,value-spec &optional))

(defstruct decomposition-record
  (node-id -1 :type fixnum)
  task
  (method-name nil :type symbol)
  (children () :type list ; of integers
   ))

(defun tree-node-task (node)
  (cond ((typep node 'shop:primitive-node)
         (shop:primitive-node-task node))
        ((typep node 'shop:complex-node)
         (shop:complex-node-task node))
        ((plan-tree:tree-node-p node)
         (plan-tree:tree-node-task node))
        (t (error 'type-error :expected-type '(or shop:primitive-node shop:complex-node plan-tree:tree-node)
                  :datum node))))

(defun complex-node-task (node)
  (cond ((shop:complex-node-p node)
         (shop:complex-node-task node))
        ((typep node 'plan-tree:complex-tree-node)
         (plan-tree:tree-node-task node))
        (t (error 'type-error :expected-type '(or shop:complex-node plan-tree:complex-tree-node)
                  :datum node))))

(defun complex-node-p (node)
  (or (shop:complex-node-p node)
      (typep node 'plan-tree:complex-tree-node)))

(defun complex-node-reduction-label (node)
  (cond ((shop:complex-node-p node)
         (shop:complex-node-reduction-label node))
        ((typep node 'plan-tree:complex-tree-node)
         (plan-tree:complex-tree-node-method-name node))
        (t (error 'type-error :expected-type '(or shop:complex-node plan-tree:complex-tree-node)
                  :datum node))))

;;; for translation to HDDL, pseudo nodes like unordered and ordered nodes
;;; need to be removed from the plan tree. The RESOLVE-EXTENDED-PLAN-TREE-CHILD(REN)
;;; functions do that
(declaim (ftype (function (plan-tree:tree-node)
                          #-allegro
                          (only-value list) ; .. of tree nodes
                          #+allegro
                          (values list)
                          )
                resolve-extended-plan-tree-child)
          (ftype (function (list) ; of tree nodes
                           #-allegro (only-value list) ;  list of tree nodes
                           #+allegro (values list)
                          )
                resolve-extended-plan-tree-children))

(defun resolve-extended-plan-tree-children (children)
  (alexandria:mappend #'resolve-extended-plan-tree-child children))


(defun resolve-extended-plan-tree-child (child)
  (etypecase child
    ((or plan-tree:ordered-tree-node plan-tree:unordered-tree-node)
     (resolve-extended-plan-tree-children (plan-tree:complex-tree-node-children child)))
    (plan-tree:tree-node (list child))))

(defun complex-node-children (node)
  (cond ((shop:complex-node-p node)
         (shop:complex-node-children node))
        ((typep node 'plan-tree:complex-tree-node)
         (resolve-extended-plan-tree-children (plan-tree:complex-tree-node-children node)))
        (t (error 'type-error :expected-type '(or shop:complex-node plan-tree:complex-tree-node)
                              :datum node))))

(defun hddl-plan (plan tree)
  "Take a SHOP PLAN and TREE (really a forest) as input and produce an
HDDL plan encoded as an s-expression."
  (multiple-value-bind (indexed-plan task-indices)
      (index-shop-plan (shop:shorter-plan plan))
    (let ((next-index (1+ (caar (last indexed-plan))))
          (root-tasks (forest-roots tree))
          roots decompositions)
      (labels ((task-index (task)
                 (if-let ((value (gethash task task-indices)))
                   value
                   (prog1
                       (setf (gethash task task-indices) next-index)
                     (incf next-index))))
               (node-index (node)
                 (task-index (tree-node-task node))))
        (setf roots
              (iter (for root in root-tasks)
                (setf (gethash root task-indices) next-index)
                (collecting next-index)
                (incf next-index)))
        (setf decompositions
              (let ((open (etypecase tree
                            (list tree)
                            (plan-tree:top-node (resolve-extended-plan-tree-child tree))))
                    retval
                    (visited (make-hash-table :test 'eql)))
                (iter
                  (while open)
                  (as node = (pop open))
                  (as id = (task-index (tree-node-task node)))
                  (unless (gethash id visited)
                    (when (complex-node-p node)
                      (setf (gethash id visited) t)
                      (let ((children (complex-node-children node)))
                        (iter (for child in children)
                          (with index)
                          (unless (shop::internal-operator-p
                                   (shop:task-name (tree-node-task child)))
                            (setf index (node-index child))
                            (when (complex-node-p child)
                              (push child open))
                            (collecting index into child-indices))
                          (finally (push (make-decomposition-record :node-id id
                                                                    :task (complex-node-task node)
                                                                    :method-name (complex-node-reduction-label node)
                                                                    :children child-indices)
                                         retval)
                                   (setf open (append children open))))))))
                (reverse retval))))
      `(:hddl-plan
        :actions ,indexed-plan
        :roots ,roots
        :decompositions ,decompositions
        ))))

#-allegro
(declaim (ftype (function (symbol) (only-value symbol))
                hddl-action-name))
(defun hddl-action-name (shop-action-name)
  "Return a new action name for HDDL.  Typically the SHOP
action name only with any leading exclamation marks removed.

Takes symbol as argument and returns symbol in same package."
  (let* ((name (symbol-name shop-action-name))
         (new-name (string-left-trim '(#\!) name)))
    (nth-value 0 (intern new-name (symbol-package shop-action-name)))))


(defun print-hddl-plan (hddl-plan &optional (stream t))
  "Takes an HDDL plan, an S-EXPRESSION produced by HDDL-PLAN,
and prints it to STREAM in the IPC format."
  (destructuring-bind (keyword &rest plan) hddl-plan
    (assert (eq keyword :hddl-plan))
    (format stream "~&==>~%")
    (let ((*print-right-margin* most-positive-fixnum)
          (*print-case* :downcase))
     ;; print the plan steps
     (iter (for (i . act) in (getf plan :actions))
       (format stream "~d (~a~{ ~a~})~%"
               i (hddl-action-name (first act)) (rest act)))
      ;; print the plan decompositions
      (format stream "~&root ~{~d~^ ~}~%" (getf plan :roots))
      (let ((tree-decompositions (getf plan :decompositions)))
        (iter (for decomp in tree-decompositions)
          (format stream "~d ~a -> ~a~{ ~d~}~%"
                  (decomposition-record-node-id decomp)
                  (decomposition-record-task decomp)
                  (decomposition-record-method-name decomp)
                  (decomposition-record-children decomp)))))
    (format stream "~&<==~%")
    (finish-output stream)))

#-allegro
(declaim (ftype (function (list) (values list hash-table &optional))
                index-shop-plan))
(defun index-shop-plan (action-list)
  (let ((hash-table (make-hash-table :test 'eq))
        (assoc-table
          (iter (for a in action-list)
            (as i from 1)
            (collecting (cons i a)))))
    (iter (for (i . act) in assoc-table)
      (setf (gethash act hash-table) i))
    (values assoc-table hash-table)))

(defun forest-roots (plan-tree)
  (mapcar #'tree-node-task
          (etypecase plan-tree
            (plan-tree::top-node
             (resolve-extended-plan-tree-children (plan-tree:complex-tree-node-children plan-tree)))
            (list plan-tree))))
