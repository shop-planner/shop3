(in-package :shop)

(defclass conditional-state-expand (stack-entry)
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

(declaim (ftype (function (&rest t
                           &key (:top-tasks t) (:tasks t) (:unifier t))
                          (values conditional-state-expand &optional))
                make-conditional-state-expand))
(defun make-conditional-state-expand (&rest arglist
                                      &key top-tasks tasks unifier)
  (declare (ignorable top-tasks
                      tasks unifier))
  (apply 'make-instance 'conditional-state-expand arglist))


(defun conditional-p (task-name)
  (member task-name '(:if :when :unless)))

(defmethod do-backtrack ((entry conditional-state-expand) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (unifier state) (unifier entry))
  (decf (depth state)))

(defmethod expand-conditional-task ((domain looping-domain)
                             ess-search-state)
  (dbg-lp "~%Expanding the conditional now...")
  (with-slots (top-tasks tasks current-task
                         unifier backtrack-stack
                         world-state)
              ess-search-state
     
     (dbg-lp "~%Saving backtrack state: ~s" tasks)
     (push (make-conditional-state-expand :top-tasks top-tasks
                                   :tasks tasks
                                   :unifier unifier)
           backtrack-stack)
     
     (dbg-lp "~%Start to expand now...")
     (multiple-value-bind (success tasks1 top-tasks1 unifier1)      ;one set of dependencies...
         ;; This should not call SEEK-PLANS anymore...
         (expand-conditional :ess domain current-task world-state tasks top-tasks
                      unifier ess-search-state)
       
       (when success
         (setf top-tasks top-tasks1
               tasks tasks1
               unifier unifier1)
         t))))

(defmethod expand-conditional ((engine (eql :ess))
			       domain task1 state tasks top-tasks
			       in-unifier &optional ess-search-state)

  (let* ((task-body (apply-substitution (get-task-body task1) in-unifier))
         reductions)

    (setf reductions
          (expand-conditional-body (first task-body) task-body
				   domain state in-unifier nil))

    (dbg-lp "~%Expanded...: ~s" reductions)
    (setf reductions (remove :ordered reductions))
    (unless reductions 
      (return-from expand-conditional (values nil nil nil nil)))

    (setf ess-search-state (save-reduction ess-search-state reductions))
    (setf reductions (push :ordered reductions))

    (multiple-value-bind (top-tasks1 tasks1)
        ;; Need to figure the internals of this method with the
        ;; LOOP task before use:
        (apply-method-bindings task1 top-tasks tasks reductions
                               in-unifier)

      (dbg-lp "~%Top-tasks1: ~s" top-tasks1)
      ;; RETURN:
      (values t tasks1 top-tasks1 in-unifier))))


;; These should/could replace the LOOP-BODY functionality above eventually, but not as it is; some alignments
;; must be made for the loop syntax processing. 
(defun block-body-condition (key task-body)
  (second (assoc key (rest task-body))))

(defun block-body-item1 (key task-body)
  (rest (assoc key (rest task-body))))

(defun block-body-item2 (key task-body)
  (assoc key (rest task-body)))

(defmethod expand-conditional-body ((body-key (eql :if))
				    task-body domain state
				    in-unifier search-state)
  (declare (ignorable search-state))
  ;; TASK-BODY is the form (:IF (:COND ....) (:ORDERED ...) (:ELSE...))

    (dbg-lp "~%Hello??")

  (let* ((if-condition (block-body-condition :cond task-body))
         (unifiers
	   (find-satisfiers if-condition state
			    :domain domain))
         reduction)

    (if unifiers
	(iter
	 (for u in unifiers)
	 (let* ((u1 (compose-substitutions in-unifier u))
		(then-body
		  (apply-substitution
		   (block-body-item1 :ordered task-body)
		   u1)))
	   (setf reduction (generate-reduction domain reduction
						 then-body))))
	;; ELSE -- do not use the unifiers from the IF condition; those should bind
	;; the variables in the THEN clause. Instead, 
	(let* ((else-body (block-body-item2 :else task-body))
	       (else-tasks
		 (apply-substitution
		  (block-body-item1 :ordered else-body)
		  in-unifier)))
	  (dbg-lp "~%ELSE tasks: ~s" else-tasks)
	  (setf reduction (generate-reduction domain reduction
						else-tasks))))
    reduction))

(defmethod expand-conditional-body ((body-key (eql :when))
				    task-body domain state
				    in-unifier search-state)
  (declare (ignorable search-state))
  ;; TASK-BODY is the form (:WHEN (:COND ....) (:ORDERED ...))
  (let* ((when-condition (block-body-condition :cond task-body))
         (unifiers
	          (find-satisfiers when-condition state :domain domain))
         reduction)
    (iter
     (for u in unifiers)
     (let* ((u1 (compose-substitutions in-unifier u))
            (when-body
	            (apply-substitution
                     (block-body-item1 :ordered task-body)
	              u1)))       
       (setf reduction (generate-reduction domain reduction when-body))))
    
    (unless reduction
      (setf reduction `(:ordered (:task !!inop))))
    (dbg-lp "~%WHEN Reduction: ~s" reduction)
    reduction))

(defmethod expand-conditional-body ((body-key (eql :unless))
				    task-body domain state
				    in-unifier search-state)
  (declare (ignorable search-state))
  ;; TASK-BODY is the form (:UNLESS (:COND ....) (:ORDERED ...))
  (let* ((unless-condition (block-body-condition :cond task-body))
         (unifiers
	   (find-satisfiers `(not ,unless-condition) state
			    :domain domain))
         reduction)
    (iter
     (for u in unifiers)
     (let* ((u1 (compose-substitutions in-unifier u))
            ;; This is the ordered task list?
            (unless-body
	      (apply-substitution
	       (block-body-item1 :ordered task-body)
	       u1)))

       
       (setf reduction (generate-reduction domain reduction
					   unless-body))))

    (unless reduction
      (setf reduction `(:ordered (:task !!inop))))
    reduction))

(defmethod seek-plans-conditional ((domain looping-domain) task1 state tasks
				    top-tasks partial-plan partial-plan-cost depth
				    which-plans protections
				    in-unifier)
  (multiple-value-bind ( success tasks1 top-tasks1 in-unifier1)
      (expand-conditional :shop2 domain task1 state tasks top-tasks in-unifier)

    (unless success
      (return-from seek-plans-conditional nil))
    (seek-plans domain state tasks1 top-tasks1 partial-plan
                partial-plan-cost (1+ depth) which-plans
                protections in-unifier1)))
