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

(defmethod do-backtrack ((entry choice-entry) (state search-state))
  (setf (mode state) (mode entry)
        (current-task state) (current-task entry)
        (alternatives state) (alternatives entry)))

(defclass state-tag (stack-entry)
  ((tag
    :initarg :tag
    :reader tag
    ))
  )

(defmethod do-backtrack ((entry state-tag) (state search-state))
  (retract-state-changes (world-state state) (tag entry)))

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

(defmethod do-backtrack ((entry prim-state-expand) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (protections state) (protections entry)
        (partial-plan state) (partial-plan entry)
        (unifier state) (unifier entry)
        (cost state) (partial-plan-cost entry))
  (decf (depth state)))

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

(defmethod do-backtrack ((entry method-instantiation) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (unifier state) (unifier entry))
  (decf (depth state)))


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
                                     unifier partial-plan-cost)
  (declare (ignorable top-tasks
                      tasks protections partial-plan
                      unifier partial-plan-cost))
  (apply 'make-instance 'prim-state-expand
         arglist))

(defun make-method-instantiation (&rest arglist &key unifier top-tasks tasks)
  (declare (ignorable unifier top-tasks tasks))
  (apply 'make-instance 'method-instantiation
         arglist))
