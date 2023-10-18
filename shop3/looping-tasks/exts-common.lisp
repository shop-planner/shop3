(in-package :shop)

(defclass looping-mixin ()
  ()
  (:documentation "Mixin domain class for HTNs with LOOP tasks in them."))

(defclass looping-domain (looping-mixin domain)
  ())


(defmethod seek-plans-task ((domain looping-mixin) task1 state tasks
                            top-tasks
                            partial-plan
                            partial-plan-cost depth which-plans
                            protections
                            unifier)
  (trace-print :tasks (get-task-name task1) state
               "~2%Depth ~s, trying task ~s"
               depth
               (apply-substitution task1 unifier))
  ;;    (y-or-n-p "continue?")
  (cond
    ((primitivep (get-task-name task1))
     (seek-plans-primitive domain task1 state tasks top-tasks
                           partial-plan partial-plan-cost depth which-plans
                           protections
                           unifier))

    ;; I can directly design this in the NONPRIMITIVE but just not to
    ;; complicate that function and possibly break it.
    ((looping-p (get-task-name task1))
     (seek-plans-loop domain task1 state tasks top-tasks
                      partial-plan partial-plan-cost depth
                      which-plans protections
                      unifier))

    (t
     (seek-plans-nonprimitive domain task1 state tasks top-tasks
                              partial-plan partial-plan-cost depth
                              which-plans protections
                              unifier
                              ))))

(defun save-reduction (ess-search-state reduction)
  (when *enhanced-plan-tree*
    (with-slots (backtrack-stack plan-tree-lookup) ess-search-state
      (iter
       (for task in reduction)
       (let* ((parent (plan-tree:find-task-in-tree (current-task ess-search-state)
                                                   (plan-tree-lookup ess-search-state)))
              (child (make-plan-tree-for-task-net task
                                                  parent plan-tree-lookup)))

         ;; MAKE-PLAN-TREE-FOR-TASK-NET as a side-effect, links PARENT and CHILD.
         (push (make-add-child-to-tree :parent parent
                                       :child child)
               backtrack-stack)))))
  ess-search-state)

(defun new-symbol (sym &optional (pkg-desig :shop))
  (gentemp (symbol-name sym) pkg-desig))

(defun rename-sym (sym renaming-table)
  (cond
    ((variablep sym)
     (unless (gethash sym renaming-table)
       (setf (gethash sym renaming-table)
             (new-symbol sym)))
     (gethash sym renaming-table))

    ((listp sym)
     (iter
      (for sym1 in sym)
      (collect (rename-sym sym1 renaming-table) into new-symlist)
      (finally (return new-symlist))))

    (t sym)))

(defun resymbol-task (tsk renaming-table)
  (iter
   (for sym in tsk)
   (collect (rename-sym sym renaming-table) into renamed-tsk)
   (finally (return renamed-tsk))))

(defun generate-reduction (domain reduction subtasks)
  (let ((renaming-table (make-hash-table :test #'equal)))
    (loop for tsk in subtasks
       as new-tsk = (resymbol-task tsk renaming-table)

       do (shop2::set-variable-property domain new-tsk)
         (setf reduction (append reduction (list new-tsk))))
    reduction))
