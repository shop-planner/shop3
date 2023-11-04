;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Code for translating SHOP plans (currently only as generated by ESS/
;;;  FIND-PLANS-STACK) into HDDL format plans.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2023/10/31:rpg] Created.
;;;
;;;---------------------------------------------------------------------------


(in-package :common-lisp-user)

(defpackage hddl-translator
  (:nicknames #:shop-hddl #:hddl-shop)
  (:export #:hddl-plan #:print-hddl-plan)
  (:use :common-lisp :alexandria :iterate))

(in-package :hddl-translator)

;;;---------------------------------------------------------------------------
;;; Type declarations for specifying function return values that make
;;; SBCL happy.
;;;---------------------------------------------------------------------------

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

;;;---------------------------------------------------------------------------
;;; API that is intended to work for both original SHOP plan trees and ESS
;;; SHOP plan trees.  Not really exercised right now, because it's proven too
;;; hard to support HDDL output from classic SHOP.
;;;---------------------------------------------------------------------------

(defun tree-node-task (node)
  (cond ((typep node 'shop:primitive-node)
         (shop:primitive-node-task node))
        ((typep node 'shop:complex-node)
         (shop:complex-node-task node))
        ((plan-tree:tree-node-p node)
         (let ((task (plan-tree:tree-node-task node))
               (expanded-task (plan-tree:tree-node-expanded-task node)))
           (cond ((shop:groundp expanded-task)
                  expanded-task)
                 ((shop:groundp task)
                  task)
                 (t (error "Task for tree node ~a is not ground." node)))))
        (t (error 'type-error :expected-type '(or shop:primitive-node shop:complex-node plan-tree:tree-node)
                              :datum node))))

(defun complex-node-task (node)
  (cond ((or (shop:complex-node-p node)
             (typep node 'plan-tree:complex-tree-node))
         (tree-node-task node))
        (t (error 'type-error :expected-type '(or shop:complex-node plan-tree:complex-tree-node)
                              :datum node))))

(defun complex-node-p (node)
  (or (shop:complex-node-p node)
      (typep node 'plan-tree:complex-tree-node)))

(defun primitive-node-p (node)
  (or (shop:primitive-node-p node)
      (typep node 'plan-tree:primitive-tree-node)))

(defun complex-node-reduction-label (node)
  (cond ((shop:complex-node-p node)
         (shop:complex-node-reduction-label node))
        ((typep node 'plan-tree:complex-tree-node)
         (plan-tree:complex-tree-node-method-name node))
        (t (error 'type-error :expected-type '(or shop:complex-node plan-tree:complex-tree-node)
                              :datum node))))

;;;---------------------------------------------------------------------------
;;; End of plan tree abstraction API
;;;---------------------------------------------------------------------------


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

;;(defvar *node-rewrite-fun*)


(defun resolve-extended-plan-tree-children (children)
  (alexandria:mappend #'resolve-extended-plan-tree-child children))


(defun resolve-extended-plan-tree-child (child)
  "If CHILD is a pseudo-node (e.g., ordered or unordered), skip it and
return its children instead.  Needed for ESS plan trees.
  Also handle domain-specific rewriting of the plan-tree.
  This function will always return a LIST of children."
  #+ignore(when *node-rewrite-fun*
    (when-let ((rewritten (funcall *node-rewrite-fun* child)))
      (return-from resolve-extended-plan-tree-child
        (resolve-extended-plan-tree-children rewritten))))
  (etypecase child
    ((or plan-tree:pseudo-node plan-tree:top-node)
     (resolve-extended-plan-tree-children (plan-tree:complex-tree-node-children child)))
    (plan-tree:tree-node (list child))))

(defun complex-node-children (node)
  (cond ((shop:complex-node-p node)
         (shop:complex-node-children node))
        ((typep node 'plan-tree:complex-tree-node)
         (resolve-extended-plan-tree-children (plan-tree:complex-tree-node-children node)))
        (t (error 'type-error :expected-type '(or shop:complex-node plan-tree:complex-tree-node)
                              :datum node))))

(defvar *task-indices*)
(defvar *next-index*)

(declaim (ftype (function (list) #-allegro (only-values fixnum boolean)
                                 #+allegro (values fixnum boolean))
                task-index)
         ;; FIXME: could give better type for parameter below
         (ftype (function (t) #-allegro (only-values fixnum boolean)
                              #+allegro (values fixnum boolean))
                node-index))
(defun task-index (task)
  (if-let ((value (gethash task *task-indices*)))
    (values value t)
    (progn
        (setf (gethash task *task-indices*) *next-index*)
      (values (the fixnum (incf *next-index*)) nil))))

(defun node-index (node)
  (task-index (tree-node-task node)))

(defun hddl-plan (plan tree)
  "Take a SHOP PLAN and TREE (really a forest) as input and produce an
HDDL plan encoded as an s-expression.  Note that currently only the extended
plan trees produced by `find-plans-stack` can be used with this function.
Classic SHOP plans do not contain all the required information."
  (let ((*next-index* 1))
   (multiple-value-bind (indexed-plan *task-indices*)
       (index-shop-plan (shop:shorter-plan plan))
     (let ((*next-index* (1+ (caar (last indexed-plan))))
           (root-tasks (forest-roots tree))
           roots decompositions)
       ;; (format t "~&*next-index* = ~d~%Root tasks are: ~S~%" *next-index* root-tasks)
       (setf roots
             (iter (for root in root-tasks)
               (as i = (task-index root))
               ;; (format t "~&Root = ~S index = ~d~%" i root)
               (collecting i)))
       (setf decompositions (plan-tree->decompositions tree))
       `(:hddl-plan
         :actions ,indexed-plan
         :roots ,roots
         :decompositions ,decompositions
         )))))

(defun plan-tree->decompositions (tree)
  (let* ((open (etypecase tree
                (list tree)
                (plan-tree:top-node (resolve-extended-plan-tree-child tree))))
         (top-nodes (copy-list open)))
    ;; (format t "~&Starting to compute decompositions:~%")
    ;; (iter (for x in top-nodes)
    ;;   (format t "~&~T~S = ~d~%"
    ;;           x (node-index x)))
    ;; first pass for indexing
    (iter
      (while open)
      (as node = (pop open))
      (with found)
      ;; Don't index internal operators
      (unless (shop::internal-operator-p
               (shop:task-name (tree-node-task node)))
        (setf found (nth-value 1 (task-index (tree-node-task node))))
        (cond ((primitive-node-p node)
               (unless found
                 (error "Found new primitive node: all primitive nodes should be indexed already.")))
              ((typep node 'plan-tree:pseudo-node)
               (error "Tried to index a pseudo-node."))
              ((complex-node-p node)
               (when (and found (not (find node top-nodes :test 'eq)))
                 (error "Found a previously indexed complex node ~A in indexing pass." node))
               (let* ((children (complex-node-children node))
                      (cc (remove-if #'primitive-node-p children)))
                 (appendf open cc)))))))

  (let ((open (etypecase tree
                (list tree)
                (plan-tree:top-node (resolve-extended-plan-tree-child tree))))
        ;; note that visited is 0-indexed and the indices have 1 as their origin.
        (visited (make-array (1- *next-index*) :element-type 'boolean :initial-element nil))
        retval)
    (flet ((set-visited (i)
             (setf (aref visited (1- i)) t))
           (arr-index->index (i)
             (1+ i)))
      (iter
        (while open)
        (as node = (pop open))
        (with id) (with found)
        (multiple-value-setq (id found) (task-index (tree-node-task node)))
        (unless found
          (error "All nodes should have been indexed before the pass to construct the decomposition records."))
        (set-visited id)   ; convert 1-based to 0
        (when (complex-node-p node)
          ;; children here have been resolved so that pseudo-nodes
          ;; have been skipped
          (let ((children (complex-node-children node)))
            (iter (for child in children)
              (with cindex) (with found)
              (unless (shop::internal-operator-p
                       (shop:task-name (tree-node-task child)))
                (multiple-value-setq (cindex found)
                  (node-index child))
                (unless found
                  (error "Unable to find an index for node ~a child of ~a"
                         child node))
                (if (complex-node-p child)
                    (push child open)
                    ;; must mark primitive nodes here
                    (set-visited cindex))
                (collecting cindex into child-indices))
              (finally
               (push (make-decomposition-record :node-id id
                                                :task (tree-node-task node)
                                                :method-name (complex-node-reduction-label node)
                                                :children child-indices)
                     retval))))))
      (unless (every #'identity visited)
        (let ((unvisited (iter (for x in-vector visited with-index i)
                           (unless x (collecting (arr-index->index i)))))) ; correct zero-based to 1-based
          (error "Some tree node~p ~:[was~;were~] not visited when building the decomposition records: ~{~d~^,~}"
                 (length unvisited)
                 (> (length unvisited) 1)
                 unvisited)))
      (sort retval #'< :key #'(lambda (dr) (decomposition-record-node-id dr))))))

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
        (iter (for decomp in (sort (copy-list tree-decompositions) #'<
                                   :key #'(lambda (x) (decomposition-record-node-id x))))
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
