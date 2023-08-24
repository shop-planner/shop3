(in-package :shop)

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

(defun looping-p (task-name)
  (equal task-name :loop))

(defmethod do-backtrack ((entry loop-state-expand) (state search-state))
  (setf (top-tasks state) (top-tasks entry)
        (tasks state) (tasks entry)
        (unifier state) (unifier entry))
  (decf (depth state)))

(defmethod unfold-loop-task ((domain looping-domain)
                             ess-search-state)
  (shop-trace "Unfolding now...~%")
  (with-slots (top-tasks tasks current-task
                         unifier backtrack-stack
                         world-state)
              ess-search-state
      (trace-print :loop nil ess-search-state "Saving backtrack state...~%")
;     (push (make-loop-state-expand :top-tasks top-tasks
 ;                                  :tasks tasks
  ;                                 :unifier unifier)
   ;        backtrack-stack)
    (trace-print :loop backtrack-stack ess-search-state "~%Backtrack stack: ~s" backtrack-stack)
    (trace-print :loop nil ess-search-state "~%Start to expand now...")
    (multiple-value-bind (success tasks1 top-tasks1 unifier1)      ;one set of dependencies...
        ;; This should not call SEEK-PLANS anymore...
        (expand-loop :ess domain current-task world-state tasks top-tasks
                     unifier ess-search-state)

      (when success
        (setf top-tasks top-tasks1
              tasks tasks1
              unifier unifier1)
        t))))


(defmethod expand-loop ((engine (eql :ess))
                        domain task1 state tasks top-tasks
                        in-unifier &optional ess-search-state)
  (let* ((task-body (apply-substitution (get-task-body task1) in-unifier))
         (reduction                     ; Single ground reduction --
           ;; what does it mean to have multiple reductions in a loop body? We are not going to backtrack
           ;; into a loop body...
           (expand-loop-body (first (second task-body)) task-body
                             domain state in-unifier nil)))
    (setf reduction (remove :ordered reduction))
    (unless reduction
      (return-from expand-loop (values nil nil nil nil)))

    (trace-print :loop reduction ess-search-state "~%Loop reduction: ~s" reduction)
    (save-reduction ess-search-state reduction)
    (setf reduction (push :ordered reduction))

    (multiple-value-bind (top-tasks1 tasks1)
        ;; Need to figure the internals of this method with the
        ;; LOOP task before use:
        (apply-method-bindings task1 top-tasks tasks reduction
                               in-unifier)
      ;; RETURN:
      (values t tasks1 top-tasks1 in-unifier))))



#|
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
                (find-satisfiers pre state
                                 :domain domain)))
;         (format t "~%State atoms: ~s" (state-atoms state))
;         (format t "~%Pre: ~s" pre)
         (format t "~%Unifiers: ~s" unifiers)
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
             (setf ess-search-state (record-reductions ess-search-state reductions))
             ))
          (format t "~%Loop task decomposed...~%")
          ))

      (:for
       (cond
         ((assoc :from (rest (second task-body))) ; it's a FROM loop of  some
                                        ; sort, supporting FROM TO for
                                        ; now.
          (let* ((start (second (assoc :from (rest (second task-body)))))
                 (end (second (assoc :to (rest (second task-body)))))
                 (index-var (second (assoc :var (rest (second task-body)))))
                 )
            (iter
             (for i from start to end)
             (let* ((u `(,(make-binding index-var i)))
                    (subtasks (apply-substitution (rest (third
                                                         task-body)) u)))
               (setf reductions (generate-reductions domain reductions subtasks))))))

         ((assoc :in (rest (second task-body))) ; FOR over elements of a
                                        ; list

          (let ((loop-var (second (assoc :var (rest (second task-body)))))
                (element-list (apply-substitution
                               (second (assoc :in (rest (second task-body))))
                               in-unifier)))
            (format t "~%Element list in FOR: ~s" element-list)
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
                                   state :domain domain)))
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
                                                  domain reductions subtasks))))))))))))

       (setf ess-search-state (record-reductions ess-search-state reductions))
       ))

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

|#

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
          (find-satisfiers loop-condition state
                           :domain domain))
         reduction)
    (iter
     (for u in unifiers)
     (let* ((u1 (compose-substitutions in-unifier u))
            ;; This is the ordered task list?
            (loop-body
             (apply-substitution
              (loop-body-item :ordered task-body)
              u1)))
       (setf reduction (generate-reduction domain reduction
                                             ;; One possible reduction:
                                             loop-body))))
    reduction))

(defmethod expand-loop-body ((body-key (eql :for))
                             task-body domain state in-unifier search-state)
  (declare (ignorable search-state state))

  (let (unfolded-loop
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
            (setf unfolded-loop
                  (generate-reduction domain unfolded-loop
                                      loop-body))))))

      ;; FOR over elements of a list
      ((loop-body-item-inner :in loop-item)
       (let ((loop-var (loop-body-item-inner :var loop-item))
             (element-list (apply-substitution
                            (loop-body-item-inner :in loop-item)
                            in-unifier))
             (loop-body (loop-body-item :ordered task-body)))

         (iter
          (for el in element-list)
          (let* ((u0 `(,(make-binding loop-var el)))
                 (loop-body-instantiated
                   (apply-substitution (copy-list loop-body) u0))
                 (loop-condition (loop-body-item :cond task-body)))

            ;; Modularize this into a function to share with EXPAND-LOOP-BODY(:COND)
            (when loop-condition
              (let* ((unifiers
                       (find-satisfiers loop-condition state
                                        :domain domain))
                     ;; Hard-coded deterministic choice -- the current design does not allow
                     ;; allow backtracking over loop bindings:
                     (u (first unifiers))
                     (u1 (compose-substitutions in-unifier u)))
                ;; This is the ordered task list?
                (setf loop-body-instantiated
                      (apply-substitution loop-body-instantiated
                                          u1))))

            (setf unfolded-loop (generate-reduction domain unfolded-loop
                                                    loop-body-instantiated))
            )))))
    ;; RETURN
    unfolded-loop))

(defmethod seek-plans-loop ((domain looping-domain) task1 state tasks
                            top-tasks partial-plan partial-plan-cost depth
                            which-plans protections
                            in-unifier)

  (multiple-value-bind ( success tasks1 top-tasks1 in-unifier1)
      (expand-loop :shop2 domain task1 state tasks top-tasks in-unifier)

    (unless success
      (return-from seek-plans-loop nil))
    (seek-plans domain state tasks1 top-tasks1 partial-plan
                partial-plan-cost (1+ depth) which-plans
                protections in-unifier1)))
