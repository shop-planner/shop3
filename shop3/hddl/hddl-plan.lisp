(in-package :common-lisp-user)

(defpackage hddl-translator
  (:nicknames #:shop-hddl #:hddl-shop)
  (:export #:hddl-plan)
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


(defun hddl-plan (plan tree)
  "Take a SHOP PLAN and TREE (really a forest) as input and produce an
HDDL plan encoded as an s-expression."
  (multiple-value-bind (indexed-plan task-indices)
      (index-shop-plan (shop:remove-costs plan))
    (let ((next-index (1+ (caar (last indexed-plan))))
          (root-tasks (forest-roots tree))
          roots decompositions)
      (labels ((task-index (task)
                 (if-let ((value (gethash task task-indices)))
                   value
                   (setf (gethash task task-indices) next-index)))
                (node-index (node)
                  (task-index (shop:tree-node-task node))))
        (setf roots
              (iter (for root in root-tasks)
                (setf (gethash root task-indices) next-index)
                (collecting next-index)
                (incf next-index)))
        (setf decompositions
              (let ((open tree)
                    retval
                    (visited (make-hash-table :test 'eql)))
               (iter
                 (while open)
                 (as node = (pop open))
                 (as id = (task-index (shop:tree-node-task node)))
                 (unless (gethash id visited)
                   (when (shop:complex-node-p node)
                     (setf (gethash id visited) t)
                     (let ((children (shop:complex-node-children node)))
                       (iter (for child in children)
                         (as index = (node-index child))
                         (collecting index into child-indices)
                         (finally (push (make-decomposition-record :node-id id
                                                                   :task (shop:complex-node-task node)
                                                                   :method-name (shop:complex-node-reduction-label node)
                                                                   :children child-indices)
                                        retval)
                                  (setf open (append children open))))))))
                (reverse retval))))
      `(:hddl-plan
        :actions ,indexed-plan
        :roots ,roots
        :decompositions ,decompositions
        ))))


(defun print-hddl-plan (hddl-plan &optional (stream t))
  "Takes an HDDL plan, an S-EXPRESSION produced by HDDL-PLAN,
and prints it to STREAM in the IPC format."
  (destructuring-bind (keyword &rest plan) hddl-plan
    (assert (eq keyword :hddl-plan))
    (format stream "~&==>~%")
    ;; print the plan steps
    (iter (for (i . act) in (getf plan :actions))
          (format stream "~d ~a~%" i act))
    ;; print the plan decompositions
    (format stream "~&root ~{~d~^ ~}~%" (getf plan :roots))
    (let ((tree-decompositions (getf plan :decompositions)))
      (iter (for decomp in tree-decompositions)
        (format stream "~d ~a -> ~a~{ ~d~}~%"
                (decomposition-record-node-id decomp)
                (decomposition-record-task decomp)
                (decomposition-record-method-name decomp)
                (decomposition-record-children decomp))))
    (format stream "~&<==~%")
    (finish-output stream)))

(declaim (ftype (function (list) (values list hash-table &optional))
                index-shop-plan))
(defun index-shop-plan (action-list)
  (let ((hash-table (make-hash-table :test 'eq))
        (assoc-table (pairlis
                      (alexandria:iota (length action-list))
                      action-list)))
    (iter (for (i . act) in assoc-table)
      (setf (gethash act hash-table) i))
    (values assoc-table hash-table)))

(defun forest-roots (plan-tree)
  (mapcar #'shop:tree-node-task plan-tree))
