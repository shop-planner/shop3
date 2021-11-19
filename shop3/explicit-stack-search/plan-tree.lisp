(defpackage plan-tree
  (:use common-lisp iterate)
  (:export #:dependency
           #:establisher
           #:consumer
           #:prop
           #:tree-node
           #:tree-node-task
           #:tree-node-expanded-task
           #:tree-node-dependencies
           #:tree-node-parent
           #:primitive-tree-node
           #:make-primitive-tree-node
           #:complex-tree-node
           #:make-complex-tree-node
           #:complex-tree-node-children
           #:top-node
           #:make-top-node
           #:pseudo-node
           #:unordered-tree-node
           #:make-unordered-tree-node
           #:make-ordered-tree-node
           #:ordered-tree-node
           #:make-dependency
           ;; finders
           #:find-plan-step
           #:find-task-in-tree
           #:find-tree-node-if

           #:copy-plan-tree
           ))

(in-package :plan-tree)

(defstruct (dependency (:conc-name nil))
  establisher
  consumer
  prop
  )

;;; this is an "abstract" class and should never be directly instantiated --
;;; only primitive-tree-node and complext-tree-node should be instantiated.
(defstruct tree-node
  task
  expanded-task
  dependencies
  parent
  )

(defstruct (primitive-tree-node (:include tree-node))
  )

(defstruct (complex-tree-node (:include tree-node))
  children
  )

(defstruct (top-node (:include complex-tree-node))
  lookup-table
  )


(defstruct (pseudo-node (:include complex-tree-node)))

;;; maybe should revise this and have complex-tree-node as mixin, since
;;; ordered-tree-node and unordered-tree-node have neither TASK nor
;;; DEPENDENCIES.
(defstruct (ordered-tree-node (:include pseudo-node)))

(defstruct (unordered-tree-node (:include pseudo-node)))

;;; FIXME: this could probably be expanded to also emit the
;;; PRINT-OBJECT method header, instead of just the code that goes in
;;; it.
(defmacro print-unreadably ((&rest args) &body body)
  "This macro is for defining a method for printing un-readably, and
deferring to the built-in method when trying to print readably.
Particularly useful for structures, but could be generally applicable."
  `(if *print-readably*
       (call-next-method)
       (print-unreadable-object ,args
         ,@body)))


(defmethod print-object ((d dependency) str)
  (print-unreadably (d str)
    (if (eq (establisher d) :init)
        (format str "init")
        (format str "~A"
                (tree-node-task (establisher d))))
    (format str " -> ~A"
            (prop d))))

(defmethod print-object ((d primitive-tree-node) str)
  (print-unreadably (d str)
    (format str "Primitive: ~S"
            (or (tree-node-expanded-task d) (tree-node-task d)))
    #+ignore(when (tree-node-dependencies d)
      (format str " :DEPENDENCIES ~S "(tree-node-dependencies d)))))

(defmethod print-object ((d top-node) str)
  (print-unreadably (d str)
    (format str " TOP-NODE CHILDREN: ~A"
            (complex-tree-node-children d))))


(defmethod print-object ((d complex-tree-node) str)
  (print-unreadably (d str)
    (format str "Complex: ~S :CHILDREN ~A"
            (or (tree-node-expanded-task d)
                (tree-node-task d))
            (complex-tree-node-children d))
    #+ignore(when (tree-node-dependencies d)
      (format str " :DEPENDENCIES ~S "(tree-node-dependencies d)))))


(defmethod print-object ((d ordered-tree-node) str)
  (print-unreadably (d str)
    (format str "Ordered CHILDREN: ~A"
            (complex-tree-node-children d))))

(defmethod print-object ((d unordered-tree-node) str)
  (print-unreadably (d str :type t)
    (format str "Unordered CHILDREN: ~A"
            (complex-tree-node-children d))))

(defun find-plan-step (task plan-tree &optional plan-tree-hash)
  (labels ((tree-search (plan-tree)
             (etypecase plan-tree
               (primitive-tree-node
                (when (eq task (tree-node-task plan-tree))
                  plan-tree))
               (complex-tree-node
                (iter (for tree-node in (complex-tree-node-children plan-tree))
                  (as result = (tree-search tree-node))
                  (when result (return result))
                  (finally (return nil)))))))
    (or
     (if plan-tree-hash
         (find-task-in-tree task plan-tree-hash)
         (tree-search plan-tree))
     (error "No tree node for task ~S in ~S" task plan-tree))))

(declaim (ftype (function (cons &optional hash-table plan-tree:tree-node)
                          (values plan-tree:tree-node &optional))
                find-task-in-tree))
(defun find-task-in-tree (task &optional hash-table plan-tree)
  "Return the PLAN-TREE:TREE-NODE in TREE corresponding to TASK."
  (let ((task (shop2::strip-task-sexp task)))
    (cond (hash-table
           (or
            (gethash task hash-table)
            (error "No plan tree node for task ~S" task)))
          (plan-tree
           (labels ((tree-search (plan-tree)
                      (if (or (eq task (tree-node-task plan-tree))
                              (eq task (tree-node-expanded-task plan-tree)))
                          plan-tree
                          (etypecase plan-tree
                            (primitive-tree-node nil)
                            (complex-tree-node
                             (iter (for tree-node in (complex-tree-node-children plan-tree))
                                   (as result = (tree-search tree-node))
                                   (when result (return-from find-task-in-tree result))))))))
             (or
              (tree-search plan-tree)
              (error "No plan tree node for task ~S" task))))
          (t (error "Must pass either hash-table or plan-tree to FIND-TASK-IN-TREE.")))))

(defun find-tree-node-if (function plan-tree)
  (labels ((tree-search (plan-tree)
             (if (funcall function plan-tree)
                 plan-tree
                 (etypecase plan-tree
                   (primitive-tree-node nil)
                   (complex-tree-node
                    (iter (for tree-node in (complex-tree-node-children plan-tree))
                      (as result = (tree-search tree-node))
                      (when result (return-from find-tree-node-if result))))))))
    (tree-search plan-tree)))

(defgeneric copy-plan-tree-node (node)
  (:method ((node primitive-tree-node))
    (copy-primitive-tree-node node))
  (:method ((node top-node))
    (copy-top-node node))
  (:method ((node ordered-tree-node))
    (copy-ordered-tree-node node))
  (:method ((node unordered-tree-node))
    (copy-unordered-tree-node node))
  (:method ((node complex-tree-node))
    (copy-complex-tree-node node)))


(declaim
 (ftype
  (function (top-node hash-table hash-table)
            (values top-node hash-table &optional))
  copy-plan-tree))

(defun copy-plan-tree (plan-tree lookup-table translation-table)
  "Make a new copy of PLAN-TREE (indexed by LOOKUP-TABLE), and using the
input TRANSLATION-TABLE, which translates old primitive tasks to new primitive
tasks."
  (let ((tree-translation-table (make-hash-table :test #'eq))
        (new-lookup-table (make-hash-table :test #'eq :size (hash-table-size lookup-table)))
        new-root)
    (flet ((translate-node (node)
             (or (gethash node tree-translation-table)
                 (error "Tree translation table has no translation for node ~s" node))))
      (labels ((phase-one (node)
                 ;; make a new node and index the copy against the original node
                 (let ((new-node (copy-plan-tree-node node)))
                   (setf (gethash node tree-translation-table) new-node)
                   (when (typep new-node 'top-node)
                     (assert (null new-root))
                     (setf new-root new-node))
                   ;; no recursion for primitive nodes...
                   (if (typep node 'primitive-tree-node)
                       (setf (tree-node-task new-node)
                             (or (gethash (tree-node-task node) translation-table)
                                 (error "Translation table has no translation for primitive task ~s" (tree-node-task node))))
                       (progn
                         (setf (tree-node-task new-node) (copy-tree (tree-node-task node)))
                         (dolist (c (complex-tree-node-children node))
                           (phase-one c))))))
               ;; FIXME: need to rewrite the dependencies, too
               (phase-two (node)
                 ;; make a new node and index the copy against the original node
                 (let ((new-node (gethash node tree-translation-table)))
                   ;; copy the dependencies
                   (setf (tree-node-dependencies new-node)
                         (mapcar #'translate-node (tree-node-dependencies node)))

                   ;; no recursion needed for primitive nodes...
                   (unless (typep node 'primitive-tree-node)
                     ;; otherwise copy the children
                     (setf (complex-tree-node-children new-node)
                           (mapcar #'translate-node (complex-tree-node-children node)))
                     ;; and then update all the children
                     (dolist (c (complex-tree-node-children node))
                       (phase-one c))))))

        (phase-one plan-tree)
        (phase-two plan-tree)
        (iter (for (task node) in-hashtable lookup-table)
          (as indexing-task =
              (if (primitive-tree-node-p node)
                  (gethash task translation-table)
                  task))
          (setf (gethash indexing-task new-lookup-table)
                (translate-node node)))
        (assert new-root)
        (setf (top-node-lookup-table new-root) new-lookup-table)
        (values new-root new-lookup-table)))))
