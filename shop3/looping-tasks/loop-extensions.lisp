(in-package :shop2)

(defclass looping-domain (domain)
     ()
  (:documentation "Mixin domain class for HTNs with LOOP tasks in them."))

;; Extend ESS stack class:
(defclass loop-state-expand (stack-entry)
  ((top-tasks
    :initarg :top-tasks
    :reader top-tasks
    )
   (tasks
    :initarg :tasks
    :reader tasks
    )
   (unifier
    :initarg :unifier
    :reader unifier
    ))
   )

(defun make-loop-state-expand (&rest arglist
                                      &key top-tasks tasks unifier)
  (declare (ignorable top-tasks
                      tasks unifier))
  (apply 'make-instance 'loop-state-expand arglist))

(defmethod do-backtrack ((entry loop-state-expand) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (unifier state) (unifier entry))
  (decf (depth state)))

(defmethod unfold-loop-task ((domain looping-domain)
                             ess-search-state)
;  (format t "~%Unfolding now...")
  (with-slots (top-tasks tasks current-task
                         unifier backtrack-stack
                         world-state)
              ess-search-state
     
     ;; (format t "~%Saving backtrack state...")
     (push (make-loop-state-expand :top-tasks top-tasks
                                   :tasks tasks
                                   :unifier unifier)
           backtrack-stack)
     
     ;; (format t "~%Start to expand now...")
     (multiple-value-bind (success tasks1 top-tasks1 unifier1)      ;one set of dependencies...
         ;; This should not call SEEK-PLANS anymore...
         (expand-loop :ess domain current-task world-state tasks top-tasks
                      unifier ess-search-state)
       
       ;; (format t "~%Success: ~s" success)
       (when success
         (setf top-tasks top-tasks1
               tasks tasks1
               unifier unifier1)
         t))))


(defmethod seek-plans-task ((domain looping-domain) task1 state tasks
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

(defmethod expand-loop ((engine (eql :ess))
                        domain task1 state tasks top-tasks
                        in-unifier &optional ess-search-state)

  (let* ((task-body (apply-substitution (shop2::get-task-body task1) in-unifier))
         reductions)

    (ecase (first (second task-body))
      (:cond
        (let* ((pre (apply-substitution (rest (second
                                               task-body))
                                        in-unifier))
               (unifiers
                (find-satisfiers pre
                                 state nil 0
                                 :domain domain)))
;         (format t "~%State atoms: ~s" (state-atoms state))
;         (format t "~%Pre: ~s" pre)
;         (format t "~%Unifiers: ~s" unifiers)
;         (format t "~%In unifier: ~s" in-unifier)
          (iter
           (for u in unifiers)
           (let* ((u1 (compose-substitutions u in-unifier))
                  ;(condition (apply-substitution (rest (second task-body)) u1))
                  (subtasks (apply-substitution (rest (third
                                                       task-body)) u1)))
             (format t "~%Subtasks: ~s" subtasks)
             (setf reductions (generate-reductions domain reductions
                                                   subtasks))
             (when *enhanced-plan-tree*
               (with-slots (backtrack-stack)
                           ess-search-state
;                 (format t "~%Current task: ~s" (current-task ess-search-state))
;                 (setf (current-task ess-search-state)
;                       (apply-substitution current-task u1))
;                 (format t "~%Current task: ~s" (current-task ess-search-state))
                                        ;              (format t "~%Reductions: ~s" reductions)
                  (iter
                   (for reduction in reductions)
;                  (format t " ~%Reduction in the tree: ~s" reduction)
                   (let* ((parent (plan-tree:find-task-in-tree
                                   (current-task ess-search-state)
                                   (plan-tree-lookup ess-search-state)))
                          (child (make-plan-tree-for-task-net reduction
                                                              parent (plan-tree-lookup ess-search-state))))
                     
                     ;; MAKE-PLAN-TREE-FOR-TASK-NET as a side-effect, links PARENT and CHILD.
                     (push (make-add-child-to-tree :parent
                                                   (if (typep child 'plan-tree::complex-tree-node)
                                                       (plan-tree::complex-tree-node-task
                                                        child)
                                                       (plan-tree::primitive-tree-node-task
                                                        child))
                                                   :child
                                                   (if (typep child 'plan-tree::complex-tree-node)
                                                       (plan-tree::complex-tree-node-children
                                                        child)
                                                       (list (plan-tree::primitive-tree-node-task
                                                              child)))
                                                   )
                           backtrack-stack)
                     ))))))
          (format t "~%Loop task decomposed...~%")
          ))
     
      (:for
       (cond
         ((member :from (second task-body)) ; it's a FROM loop of  some
                                        ; sort, supporting FROM TO for
                                        ; now. 
          (let* ((start (second (member :from (second task-body))))
                 (end (second (member :to (second task-body))))
                 (index-var (second (second task-body)))
                 )
            (iter
             (for i from start to end)
             (let* ((u `(,(make-binding index-var i)))
                    (subtasks (apply-substitution (rest (third
                                                         task-body)) u)))
               (setf reductions (generate-reductions domain reductions subtasks))))))

         ((member :in (second task-body)) ; FOR over elements of a
                                        ; list

          (let ((loop-var (second (second task-body)))
                (element-list (apply-substitution
                               (second (member :in (second task-body)))
                               in-unifier)))
                                       
            (loop for el in element-list
                  do (let ((u `(,(make-binding loop-var el))))
                       (ecase (first (third task-body))
                         (:ordered
                             (let ((subtasks
                                    (apply-substitution
                                     (apply-substitution (rest
                                                          (third task-body))
                                                         u)
                                     in-unifier)))
                               (setf reductions (generate-reductions
                                                 domain reductions subtasks))))
                         (:cond
                           (let ((unifiers
                                  (find-satisfiers
                                   (apply-substitution (rest (third task-body))
                                                       (compose-substitutions u in-unifier))
                                   state nil 0
                                   :domain domain)))
                             (iter
                              (for u2 in unifiers)
                              (let* ((u1
                                      (compose-substitutions u
                                                             (compose-substitutions
                                                              u2
                                                              in-unifier)))
                                     (subtasks (apply-substitution
                                                (rest (fourth task-body)) u1)))
                                (setf reductions (generate-reductions
                                                  domain reductions subtasks))))))))))))))

    (setf reductions (remove :ordered reductions))
    (unless reductions 
      (return-from expand-loop (values nil nil nil nil)))
    (setf reductions (push :ordered reductions))
    (multiple-value-bind (top-tasks1 tasks1)
        ;; Need to figure the internals of this method with the
        ;; LOOP task before use:
        (shop2::apply-method-bindings task1 top-tasks tasks reductions
                                      in-unifier)
      ;; RETURN:
      (values t tasks1 top-tasks1 in-unifier))))

(defmethod expand-loop ((engine (eql :shop2))
                        domain task1 state tasks top-tasks
                        in-unifier &optional ess-search-state)
  
  (declare (ignore ess-search-state))
  (let* ((task-body (apply-substitution (get-task-body task1) in-unifier))
         reductions)

    (setf reductions
          (expand-loop-body (first (second task-body)) task-body
                            domain state in-unifier nil))
    (setf reductions (remove :ordered reductions))
    (unless reductions 
      (return-from expand-loop (values nil nil nil nil)))

    (setf reductions (push :ordered reductions))
    (multiple-value-bind (top-tasks1 tasks1)
        ;; Need to figure the internals of this method with the
        ;; LOOP task before use:
        (apply-method-bindings task1 top-tasks tasks reductions
                               in-unifier)
      ;; RETURN:
      (values t tasks1 top-tasks1 in-unifier))))

(defun loop-item (key task-body)
  (second (member key task-body)))
  
(defun loop-body-item (key task-body)
  ;; Drop the item keyword (e.g., :COND)
  (rest (assoc key (rest task-body))))

(defun loop-body-item-inner (key task-body)
  ;; Get the inner construct values
  (second (assoc key task-body)))

(defmethod expand-loop-body ((body-key (eql :cond))
                             task-body domain state
                             in-unifier search-state)
  (declare (ignorable search-state))
  ;; TASK-BODY is the form (:LOOP (:COND ....) (:ORDERED ...))
  (let* ((loop-condition (loop-body-item :cond task-body))
         (unifiers
          (find-satisfiers loop-condition state nil 0
                           :domain domain))
         reductions)

    (iter
     (for u in unifiers)
     (let* ((u1 (compose-substitutions u in-unifier))
            ;; This is the ordered task list?
            (loop-body
             (apply-substitution
              (loop-body-item :ordered task-body)
              u1)))
       (setf reductions (generate-reductions domain reductions
                                             loop-body))))
    reductions))

(defmethod expand-loop-body ((body-key (eql :for))
                             task-body domain state 
                             in-unifier search-state)
  (declare (ignorable search-state state))

  (let (reductions
        (loop-item (loop-body-item body-key task-body)))
    (cond
      ;; it's a :FROM loop of some sort, supporting FROM TO for now.  
      ((loop-body-item-inner :from loop-item)
       (let* ((start (loop-body-item-inner :from loop-item))
              (end (loop-body-item-inner :to loop-item))
              (index-var (loop-body-item-inner :var loop-item)))

         (iter
          (for i from start to end)
          (let* ((u `(,(make-binding index-var i)))
                 (loop-body (apply-substitution (loop-body-item :ordered task-body)
                                                u)))
            (setf reductions (generate-reductions domain reductions
                                                  loop-body))))))

      ;; FOR over elements of a list
      ((loop-body-item-inner :in loop-item)

       (let ((loop-var (loop-body-item-inner :var loop-item))
             (element-list (apply-substitution
                            (loop-body-item-inner :in loop-item)
                            in-unifier)))

;        (format t "~%Loop var: ~s" loop-var)
;        (format t "~%Loop element list: ~s" element-list)
;        (read)
         
         (iter
          (for el in element-list)
          (let* ((u `(,(make-binding loop-var el)))
                 (loop-body (apply-substitution (loop-body-item :ordered task-body)
                                                u)))
            (setf reductions (append reductions
                                     (generate-reductions domain reductions loop-body))))))))

    ;; RETURN
    reductions))

(defmethod expand-loop-body ((body-key (eql :cond2))
                             task-body domain state
                             in-unifier search-state)
  (declare (ignorable search-state))

  (let ((unifiers
         (find-satisfiers
          (apply-substitution (rest (third task-body))
                              in-unifier)
          state nil 0
          :domain domain))
        reductions)
    (iter
     (for u2 in unifiers)
     (let* ((u1 (compose-substitutions u2 in-unifier))
            (subtasks (apply-substitution
                       (rest (fourth task-body)) u1)))
       (setf reductions (generate-reductions domain reductions subtasks))))
    reductions))


(defmethod seek-plans-loop ((domain looping-domain) task1 state tasks
                            top-tasks partial-plan partial-plan-cost depth
                            which-plans protections
                            in-unifier)
  
  (multiple-value-bind ( success tasks1 top-tasks1 in-unifier1)
      (expand-loop :shop2 domain task1 state tasks top-tasks in-unifier)

    (unless success
      (return-from seek-plans-loop nil))
    ;; (format t "~%Tasks1: ~a" tasks1)
    ;; (format t "~%Top-Tasks1: ~a" top-tasks1)
    (seek-plans domain state tasks1 top-tasks1 partial-plan
                partial-plan-cost (1+ depth) which-plans
                protections in-unifier1)))

(defun looping-p (task-name)
  (equal task-name :loop))

(defun new-symbol (sym)
  (intern (string (gensym (string sym)))))

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

(defun generate-reductions (domain reductions subtasks)
;  (format t "~%Reductions: ~s" reductions)
  (let ((renaming-table (make-hash-table :test #'equal)))
    (loop for tsk in subtasks
          as new-tsk = (resymbol-task tsk renaming-table)
                                
          do (shop2::set-variable-property domain new-tsk)
             (setf reductions (append reductions (list new-tsk))))

;    (format t "~%Reductions: ~s" reductions)
 ;   (read)
    reductions))  

