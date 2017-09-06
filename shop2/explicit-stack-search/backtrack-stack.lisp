(in-package :shop2)

(defgeneric do-backtrack (stack-entry state)
  (:documentation "Update the STATE to restore the
items recorded in STACK-ENTRY."))

(defclass stack-entry ()
  ()
  (:documentation "\"Abstract\" stack entry class, ancestor to all real stack-entry
classes."))

(defclass bottom-of-stack (stack-entry)
  ()
  )

(defclass choice-entry (stack-entry)
  ((mode
    :initarg :mode
    :reader mode
    )
   (current-task
    :initarg :current-task
    :reader current-task
    )
   (alternatives
    :initarg :alternatives
    :reader alternatives
    ))
  )

(defmethod print-object ((obj choice-entry) str)
  (cond ((>= *verbose* 2)
         (print-unreadable-object (obj str :type t)
           (format str "~A alternatives: ~A"
                   (current-task obj) (alternatives obj))))
        ((= *verbose* 1)
         (print-unreadable-object (obj str :type t)
           (format str "~A"
                   (current-task obj))))
        (t
         (print-unreadable-object (obj str :type t :identity t)))))



(defclass state-tag (stack-entry)
  ((tag
    :initarg :tag
    :reader tag
    ))
  )

(defclass prim-state-expand (stack-entry)
  ((top-tasks
    :initarg :top-tasks
    :reader top-tasks
    )
   (tasks
    :initarg :tasks
    :reader tasks
    )
   (protections
    :initarg :protections
    :reader protections
    )
   (partial-plan
    :initarg :partial-plan
    :reader partial-plan
    )
   (unifier
    :initarg :unifier
    :reader unifier
    )
   (partial-plan-cost
    :initarg :partial-plan-cost
    :reader partial-plan-cost
    ))
  )


(defclass add-child-to-tree (stack-entry)
  ((parent
   :initarg :parent
   :initform nil
   :reader parent
   )
  (child
   :initarg :child
   :initform nil
   :reader child
   ))
  )

(defclass add-dependencies (stack-entry)
  ((dependencies
    :initarg :dependencies
    :reader dependencies
    )))




(defclass method-instantiation (stack-entry)
  ((unifier
    :initarg :unifier
    :reader unifier
    )
   (top-tasks
      :initarg :top-tasks
      :reader top-tasks
      )
   (tasks
    :initarg :tasks
    :reader tasks
    ))
  )

(defclass record-expansion (stack-entry)
  ((tree-node
    :initarg :tree-node
    :reader tree-node
    ))
  (:documentation "Record the expansion of a TASK into an EXPANDED-TASK.
What this means is that we are recording the match of the TASK as a
template against a standardized EXPANDED-TASK from the plan library.")
  )




;;;---------------------------------------------------------------------------
;;; DO-BACKTRACK methods
;;;---------------------------------------------------------------------------
(defmethod do-backtrack ((entry state-tag) (state search-state))
  (retract-state-changes (world-state state) (tag entry))
  (when *record-dependencies-p*
    (delete-tag-map (tag entry))))

(defmethod do-backtrack ((entry method-instantiation) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (unifier state) (unifier entry))
  (decf (depth state)))

(defmethod do-backtrack ((entry add-dependencies) (state search-state))
  (mapc #'(lambda (dep)
            (let ((consumer (plan-tree:consumer dep)))
              (setf (plan-tree:tree-node-dependencies consumer)
                (delete dep (plan-tree:tree-node-dependencies consumer) :test 'eq))))
        (dependencies entry)))

(defmethod do-backtrack ((entry add-child-to-tree) (state search-state))
  (with-slots (parent child) entry
    (assert (member child (plan-tree:complex-tree-node-children parent)))
    (remove-subtree-from-table (plan-tree-lookup state) child)
    (setf (plan-tree:complex-tree-node-children parent)
          (delete child (plan-tree:complex-tree-node-children parent))
          (plan-tree:tree-node-parent child) nil)))

(defmethod do-backtrack ((entry prim-state-expand) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (protections state) (protections entry)
        (partial-plan state) (partial-plan entry)
        (unifier state) (unifier entry)
        (cost state) (partial-plan-cost entry))
  (decf (depth state)))


(defmethod do-backtrack ((entry choice-entry) (state search-state))
  (setf (mode state) (mode entry)
        (current-task state) (current-task entry)
        (alternatives state) (alternatives entry)))


(defmethod do-backtrack ((entry record-expansion) (state search-state))
  (setf (gethash (tree-node entry) (plan-tree-lookup state)) nil)
  (setf (plan-tree:tree-node-expanded-task (tree-node entry)) nil)
  (values))


;;;---------------------------------------------------------------------------
;;; Constructors
;;;---------------------------------------------------------------------------

;;; keyword arguments just here so that it's easy to see what you can pass to
;;; this function.
(defun make-cs-state (&rest arglist &key mode alternatives current-task)
  (declare (ignorable mode alternatives current-task))
  (apply 'make-instance 'choice-entry
         arglist))

(defun make-world-state-tag (&rest arglist &key tag)
  (declare (ignorable tag))
  (apply 'make-instance 'state-tag
         arglist))

(defun make-prim-state-expand (&rest arglist
                                     &key top-tasks
                                     tasks protections partial-plan
                                     unifier partial-plan-cost
                                       parent child)
  (declare (ignorable top-tasks
                      tasks protections partial-plan
                      unifier partial-plan-cost parent child))
  (apply 'make-instance 'prim-state-expand
         arglist))

(defun make-method-instantiation (&rest arglist &key unifier top-tasks tasks)
  (declare (ignorable unifier top-tasks tasks))
  (apply 'make-instance 'method-instantiation
         arglist))

(defun make-update-parent (&key old-parent)
  (assert old-parent)
  (make-instance 'update-parent :old-parent old-parent))

(defun make-add-dependencies (dependencies)
  (make-instance 'add-dependencies
    :dependencies dependencies))


(defun make-add-child-to-tree (&key parent child)
  (make-instance 'add-child-to-tree
                 :parent parent
                 :child child))

(defun make-record-expansion (task-node)
  (make-instance 'record-expansion :tree-node task-node))
