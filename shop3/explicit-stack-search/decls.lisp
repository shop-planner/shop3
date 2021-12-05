(in-package :shop2)

(defclass search-state ()
  (
   (mode
    :initarg :mode
    :accessor mode
    :documentation "Holds the mode -- the name -- of the current
\"instruction\" in the explicit search virtual machine."
    :type symbol
    )
   (current-task
    :initarg :current-task
    :accessor current-task
    :type (or list null)
    :documentation "The currently active task, if that's meaningful
in this search MODE."
    )
   (alternatives
    :initarg :alternatives
    :accessor alternatives
    )
   ;; world state
   (world-state
    :initarg :state
    :initarg :world-state
    :accessor state
    :accessor world-state
    :documentation "SHOP2 world state object."
    )
   (protections
    :initarg :protections
    :accessor protections
    :initform NIL
    :documentation "Set of protections in the current
state."
    )
   ;; plan-so-far
   (tasks
    :initarg :tasks
    :accessor tasks
    :documentation "Current task network."
    )
   (top-tasks
    :initarg :top-tasks
    :accessor top-tasks
    :documentation "Current set of tasks with no predecessors;
eligible to be planned."
    )
   (partial-plan
    :initarg :partial-plan
    :accessor partial-plan
    :initform nil
    :documentation "List: current plan prefix."
    )
   (cost
    :initarg :cost
    :accessor cost
    :type number
    :initform 0
    :documentation "Cost of partial-plan."
    )
   (unifier
    :initarg :unifier
    :accessor unifier
    :initform nil
    )
   (depth
    :initarg :depth
    :accessor depth
    :type integer
    :initform 0
    :documentation "Depth in search.  Used by the tracing
functions."
    )
   (backtrack-stack
    :initarg :backtrack-stack
    :accessor backtrack-stack
    :initform (list (make-instance 'bottom-of-stack))
    )
   (plans-found
    :initarg :plans-found
    :initform nil
    :accessor plans-found
    )
   (plan-tree
    :initarg :plan-tree
    :reader plan-tree
    )
   (plan-tree-lookup
    :initform (make-hash-table :test 'eq)
    :reader plan-tree-lookup
    )
   ))



(defclass plan-return ()
  ((plan
    :initarg :plan
    :reader plan
    )
   (tree
    :initarg :tree
    :reader tree
    :type (or null plan-tree:top-node)
    )
   (lookup-table
    :initarg :lookup-table
    :reader lookup-table
    )
   (replay-table
    :initarg :replay-table
    :reader replay-table
    )
   (world-state
    :initarg :world-state
    :reader world-state
    )
   (search-state
    :initarg :search-state
    :reader search-state
    )
   )
  (:documentation "Store the multiple return aspects for a single SHOP plan."))


(defmethod plan-cost ((pr plan-return))
  (plan-cost (plan pr)))

(defmacro verbose-format (&rest args)
  (let ((threshold (if (integerp (first args))
                       (pop args)
                       1)))
    `(when (>= *verbose* ,threshold) (format t ,@args))))

(defmacro appendf (place value)
  `(setf ,place
         (append ,place ,value)))

(define-condition search-failed ()
  ()
  (:documentation "Condition to be signaled when the system has backtracked
to the bottom of the stack."))
