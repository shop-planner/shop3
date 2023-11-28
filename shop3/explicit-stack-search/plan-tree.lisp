(in-package :plan-tree)


;;;---------------------------------------------------------------------------
;;; Helpers for MAKE-LOAD-FORM for the plan tree.
;;;---------------------------------------------------------------------------

(defvar *table-for-load-form*)
(defvar *node-list*)

(defgeneric make-instantiator (obj)
  (:documentation "Return an s-expression instantiating a copy of OBJ.
The returned form will *not* include links to other objects to make the
result loadable")
  (:method (obj)
    (error "No method for instantiating object ~s" obj))
  (:method ((obj symbol))
    (cond ((eq obj :init) :init)
          ((equalp (symbol-name obj) (symbol-name '#:top))
           `(quote ,obj))
          (t (error "Unexpected symbol in plan tree: ~s" obj))))
  (:method ((obj list))
    "LISTS should be tasks or propositions"
    `(quote ,obj)))

(defgeneric cross-links-for (varname val table)
  (:documentation "Return a list of forms that set any
cross-links for VAL using information in TABLE."))

(defgeneric slot-fillers (obj)
  (:method (obj)
    (error "No method for computing slot fillers for object ~s" obj)))

;;;---------------------------------------------------------------------------
;;; DEPENDENCY structures
;;;---------------------------------------------------------------------------


(defstruct (dependency (:conc-name nil))
  establisher
  consumer
  prop
  )

(defmethod make-instantiator ((obj dependency))
  `(make-dependency :prop ,(slot-value-translator
                            (prop obj))))

(defmethod cross-links-for ((var-name symbol) (obj dependency) (table hash-table))
  `((setf (consumer ,var-name) ,(slot-value-translator (consumer obj) table)
          (establisher ,var-name) ,(slot-value-translator (establisher obj) table))))



;;; this is an "abstract" class and should never be directly instantiated --
;;; only subclasses should be instantiated.
(defstruct tree-node
  task
  expanded-task                         ; the substituted method head.
                                        ; should always be NIL for primitive tasks.
  dependencies ;; what does this tree node depend on -- dependencies IN
  parent
  )

(defmethod cross-links-for ((var-name symbol) (obj tree-node) (table hash-table))
  `((setf (tree-node-dependencies ,var-name)
          (list
           ,@(mapcar #'(lambda (x) (slot-value-translator x table))
                     (tree-node-dependencies obj))))))

(defstruct (primitive-tree-node (:include tree-node))
  )

(defun slot-value-translator (val &optional (table *table-for-load-form*))
  (cond ((null val) NIL)
        ((and (symbolp val)
              (or (eq val :INIT) (equalp (symbol-name val) (symbol-name '#:top))))
         val)
        (t (or (gethash val table)
               (error "No table entry for value ~s" val)))))

(defmethod slot-fillers ((obj tree-node))
  `(:task
    ,(slot-value-translator (tree-node-task obj))
    :expanded-task
    ,(slot-value-translator (tree-node-task obj))))

(defun make-cross-links (&optional (table *table-for-load-form*))
  (iter (for (val var) in-hashtable table)
    (unless (listp val)
     (appending
      (cross-links-for var val table)))))

(defmethod make-instantiator ((obj primitive-tree-node))
  `(make-primitive-tree-node ,@ (slot-fillers obj)))

(defstruct (complex-tree-node (:include tree-node))
  (children nil :type list)
  ;; FIXME: Move this slot to a structure that is guaranteed to have a method name?
  ;; the pseudo-tree children won't...
  (method-name nil :type (or null symbol)))



(defmethod make-instantiator ((obj complex-tree-node))
  `(make-complex-tree-node ,@ (slot-fillers obj)))

(defmethod cross-links-for ((var-name symbol) (obj complex-tree-node) (table hash-table))
  (append (call-next-method)
          `((setf (complex-tree-node-children ,var-name)
                  (list
                   ,@(mapcar #'(lambda (x) (slot-value-translator x table))
                           (complex-tree-node-children obj)))))))


(defstruct (top-node (:include complex-tree-node))
  ;; table from plan s-expressions to nodes.
  (lookup-table nil :type (or hash-table null)))

(defmethod make-instantiator ((obj top-node))
  `(make-top-node ,@ (slot-fillers obj)))

(defmethod slot-fillers ((obj top-node))
  (let ((slot-fillers (call-next-method)))
    (remf slot-fillers :task)
    (remf slot-fillers :expanded-task)
    (append `(:task (quote ,(top-node-task obj)))
            slot-fillers)))

(defmethod make-load-form ((obj top-node) &optional environment)
  (declare (ignorable environment))
  (let ((*table-for-load-form* (make-hash-table :test #'eq))
        (*node-list* nil))
    (make-table-entries obj)
    `(let* ,(obj-bindings *table-for-load-form*)
       ,@(make-cross-links *table-for-load-form*)
       ,(gethash obj *table-for-load-form*))))

(defgeneric make-table-entries (node)
  (:documentation "Traverse the plan tree, populating *TABLE-FOR-LOAD-FORM* dynamic variable
and building a toplogically sorted list of nodes."))

(defun obj-bindings (hash-table)
  "Return an ordered list of variable-name instantiator pairs for use in a LET form."
  (append
   (iter (for (item var-name) in-hashtable hash-table)
     ;; proposition or task
     (when (listp item)
       (collecting `(,var-name ',item))))
   (iter (for (item var-name) in-hashtable hash-table)
     (unless (listp item)
       (collecting `(,var-name ,(make-instantiator item)))))))



(defun insert-if-necessary (obj &optional (table *table-for-load-form*))
  "Make an entry in TABLE for OBJ if it's not already there."
  (unless (eq obj :init)
    (unless (gethash obj table nil)
      (setf (gethash obj table)
            (cond ((typep obj 'tree-node)
                   (gensym "NODE"))
                  ((typep obj 'dependency)
                   (gensym "DEP"))
                  ((listp obj)
                   (gensym "PROP"))
                  (t (gensym "OTHER")))))))

(defmethod make-table-entries ((obj tree-node))
  (push obj *node-list*)
  (insert-if-necessary obj)
  (when-let (task (tree-node-task obj))
    ;; avoid TOP symbol
    (unless (symbolp task)
      (insert-if-necessary task)))
  (when-let (etask (tree-node-expanded-task obj))
    (unless (symbolp etask)
      (insert-if-necessary etask)))
  (when-let (parent (tree-node-parent obj))
    (insert-if-necessary parent))
  (iter (for dep in (tree-node-dependencies obj))
    (make-table-entries dep)))


(defmethod make-table-entries ((obj dependency))
  (insert-if-necessary obj)
  (unless (eq (establisher obj) :init)
    (insert-if-necessary (establisher obj)))
  (insert-if-necessary (consumer obj))
  (insert-if-necessary (prop obj)))


(defmethod make-table-entries ((obj complex-tree-node))
  (call-next-method)
  (mapc #'make-table-entries (complex-tree-node-children obj)))

;; FIXME: likely this should be a pseudo-node
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

(defmethod print-object ((node complex-tree-node) stream)
  (print-unreadably (node stream)
    (print-unreadable-object (node stream :type t :identity nil)
      (format stream "~A --~A-> ~a"
              (tree-node-task node)
              (alexandria:if-let ((method-name (complex-tree-node-method-name node)))
                method-name "")
              (let* ((children (complex-tree-node-children node))
                     (n (length children)))
                (ecase n
                  (0 "no children")
                  (1
                   (let ((child (first children)))
                     (etypecase child
                       ((or ordered-tree-node unordered-tree-node)
                        (let ((n (length (complex-tree-node-children child))))
                          (format nil "~d ~a child~:[~;ren~]"
                                  n
                                  (if (typep child 'ordered-tree-node)
                                      "ordered"
                                      "unordered")
                                  (> n 1))))
                       (primitive-tree-node "1 primitive child")
                       (complex-tree-node "1 complex child"))))))))))


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


;; (defmethod print-object ((d complex-tree-node) str)
;;   (print-unreadably (d str)
;;     (format str "Complex: ~S :CHILDREN ~A"
;;             (or (tree-node-expanded-task d)
;;                 (tree-node-task d))
;;             (complex-tree-node-children d))
;;     #+ignore(when (tree-node-dependencies d)
;;       (format str " :DEPENDENCIES ~S "(tree-node-dependencies d)))))


(defmethod print-object ((d ordered-tree-node) str)
  (print-unreadably (d str)
    (format str "Ordered CHILDREN: ~A"
            (complex-tree-node-children d))))

(defmethod print-object ((d unordered-tree-node) str)
  (print-unreadably (d str :type t)
    (format str "Unordered CHILDREN: ~A"
            (complex-tree-node-children d))))

;;;---------------------------------------------------------------------------
;;; s-expression representation for plan tree
;;;---------------------------------------------------------------------------

(defgeneric plan-tree->sexp (plan-tree)
  (:documentation "Translate PLAN-TREE into an s-expression for export to other systems.")
  (:method ((top top-node))
    (mapcar #'plan-tree->sexp (complex-tree-node-children top)))
  (:method  ((cn complex-tree-node))
     `(,(or (tree-node-expanded-task cn) (tree-node-task cn)) . ,(mapcar #'plan-tree->sexp (complex-tree-node-children cn))))
  (:method ((on ordered-tree-node))
    (mapcar #'plan-tree->sexp (complex-tree-node-children on)))
  (:method ((un unordered-tree-node))
    (mapcar #'plan-tree->sexp (complex-tree-node-children un)))
  (:method  ((pn primitive-tree-node))
    (or (primitive-tree-node-task pn) (primitive-tree-node-expanded-task pn))))


(defun find-plan-step (task plan-tree &optional plan-tree-hash)
  (labels ((tree-search (plan-tree)
             (etypecase plan-tree
               (primitive-tree-node
                (when (or (eq task (tree-node-task plan-tree))
                          (eq task (tree-node-expanded-task plan-tree)))
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

(defun all-primitive-nodes (plan-tree)
  (let (retval)
    (labels ((tree-search (plan-tree)
               (etypecase plan-tree
                 (primitive-tree-node (push plan-tree retval))
                 (complex-tree-node
                  (mapc #'tree-search (complex-tree-node-children plan-tree))))))
      (tree-search plan-tree)
      retval)))

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
