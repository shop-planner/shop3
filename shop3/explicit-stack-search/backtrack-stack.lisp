(in-package :shop)

(defgeneric do-backtrack (stack-entry state)
  (:documentation "Update the STATE to restore the
items recorded in STACK-ENTRY."))

(defclass stack-entry ()
  ()
  (:documentation "\"Abstract\" stack entry class, ancestor to all real stack-entry
classes."))

(defclass bottom-of-stack (stack-entry)
  ()
  (:documentation "Sentinel value for the bottom of the backtrack stack."))

(defclass choice-entry (stack-entry)
  ((mode
    :initarg :mode
    :reader mode
    :type symbol
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
   :type (or plan-tree:complex-tree-node null)
   )
  (child
   :initarg :child
   :initform nil
   :reader child
   :type (or plan-tree:tree-node null)
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
    :type plan-tree:tree-node
    ))
  (:documentation "Record the expansion of a TASK into an EXPANDED-TASK.
What this means is that we are recording the match of the TASK as a
template against a standardized EXPANDED-TASK from the plan library.")
  )

(defclass record-expansion-for-replay (stack-entry)
  ((task
    :initarg :task
    :reader task
    :type list)
   (method-id
    :initarg :method-id
    :reader method-id
    :type symbol)
   )
  (:documentation "Record the expansion of TASK according to METHOD-ID.
This may later be used by the analogical replay code."))



;;;---------------------------------------------------------------------------
;;; DO-BACKTRACK methods
;;;---------------------------------------------------------------------------
(defmethod do-backtrack ((entry state-tag) (state search-state))
  #+ignore(format t "~&Before retracting state tag ~D world state tag is ~d and state is:"
          (tag entry) (shop2.common::tagged-state-tags-info-tag (shop2::world-state state)))
  #+ignore(shop2:print-current-state :state (world-state state) :sorted t)
  (retract-state-changes (world-state state) (tag entry))
  #+ignore(format t "~&After retracting state tag ~D world state tag is ~d and state is:"
          (tag entry) (shop2.common::tagged-state-tags-info-tag (shop2::world-state state)))
  #+ignore(shop2:print-current-state :state (world-state state) :sorted t)
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

(defmethod do-backtrack ((entry record-expansion-for-replay) (state search-state))
  (with-slots (task method-id) entry
    (%delete-a-decomposition *domain* task method-id))
  (values))


;;;---------------------------------------------------------------------------
;;; Constructors
;;; These are just thin wrappers around MAKE-INSTANCE.
;;; I have added explicit keyword arguments to make it more clear
;;; what arguments need to be supplied.
;;;---------------------------------------------------------------------------
;;; STACK-CONSTRUCTOR accepts:
;;; NAME-ARG which is either a symbol of the form MAKE-<FOO>
;;;   to construct a FOO class object, or
;;;   (<arbitrary-name> <class-name>) which will create a
;;;   function named <arbitrary-name> that will create an object
;;;   of type <class-name)
;;; a &REST argument that is a list of keyword argument specifications.
;;;   These will be either keyword argument names (symbols) or two-element
;;;   lists, where the first element is the name, and the second is a type
;;;   specifier.  Names do *not* need to be in the keyword package --
;;;   the macro will take care of that.
;;; Also generates a type declaration for the function.
(defmacro stack-constructor (name-arg &rest arglist)
  (let (class-name name)
   (if (listp name-arg)
       (cond
         ((= (length name-arg) 2)
          (setf name (first name-arg)
                class-name (second name-arg)))
         ((= (length name-arg) 1)
          (setf name (first name-arg)))
         (t (error "Unexpected name-arg: expected one or two element list. Got ~s" name-arg)))
       (setf name name-arg))
    (let ((class (if class-name class-name
                     (let ((prefix (subseq (symbol-name name) 0 5))
                           (class-name (subseq (symbol-name name) 5)))
                       (assert (string-equal "make-" prefix))
                       (intern class-name :shop)))))
      (multiple-value-bind (instance-arglist keywords keyword-types)
       (iter (for arg in arglist)
         (for name = (if (symbolp arg) arg
                         (progn (assert (and (listp arg) (= (length arg) 2)))
                                (first arg))))
         (for type = (if (symbolp arg) t
                         (progn (assert (and (listp arg) (= (length arg) 2)))
                                (second arg))))
         (as keyword = (intern (symbol-name name) :keyword))
         (appending (list (intern (symbol-name name) :keyword) name) into instance-arglist)
         (collecting (list keyword type) into keyword-types)
         (collecting name into keywords)
         (finally (return (values instance-arglist keywords keyword-types))))
        `(progn
           (declaim (ftype (function ,(cons '&key keyword-types) (values ,class &optional)) ,name))
           (defun ,name ,(cons '&key keywords)
             ,(format nil "Constructor for ~s objects." class)
             (make-instance ',class
                            ,@instance-arglist)))))))

(stack-constructor (make-cs-state choice-entry) mode alternatives current-task)

(stack-constructor (make-world-state-tag state-tag) tag)

(stack-constructor make-prim-state-expand
                   top-tasks
                   tasks protections partial-plan
                   unifier partial-plan-cost)

(stack-constructor make-method-instantiation unifier top-tasks tasks)

(stack-constructor make-add-dependencies dependencies)

(stack-constructor make-add-child-to-tree
                   (parent plan-tree:complex-tree-node)
                   (child plan-tree:tree-node))

(stack-constructor make-record-expansion tree-node)
(stack-constructor make-record-expansion-for-replay (task list) (method-id symbol))
