;;; -*- Mode: common-lisp; package: shop2; -*-

(in-package :shop2)

(defvar *test-explicit-state-search* nil
  "When testing, do we use standard SHOP2, or the explicit
state variant?")

(defun find-plans-stack (problem &key domain (verbose 0))
  "Top level search function for explicit-state search in SHOP2.
Does not support the full range of options supported by SHOP2: only
supports finding the first solution to PROBLEM.  To comply with SHOP2,
though, always returns a list of plans."
  (let* ((*plan-tree* nil)
         (*verbose* verbose)
         (problem (find-problem problem t))
         (domain (cond (domain
                        (etypecase domain
                          (symbol
                           (find-domain domain :error))
                          (domain domain)))
                       ((domain-name problem)
                        (find-domain (domain-name problem) :error))
                       (*domain* *domain*)
                       (t
                        (error "Domain not supplied and problem does not specify domain."))))
         (state (apply 'make-initial-state domain
                       (default-state-type domain)
                       (problem->state domain problem)))
         (tasks (get-tasks problem))
         (search-state (make-instance 'search-state
                                      :world-state state
                                      :tasks tasks
                                      :top-tasks (get-top-tasks tasks))))

    (set-variable-property domain tasks)
    (seek-plans-stack search-state domain)))

(defun seek-plans-stack (state domain &optional (which-plans :first))
  "Workhorse function for FIND-PLANS-STACK.  Executes the SHOP2 search
virtual machine, cycling through different virtual instructions depending
on the value of the MODE slot of STATE."
  (declare (ignorable which-plans))
  ;; kick off the stack VM
  (setf (mode state) 'check-for-done)
  (catch 'search-failed
    (iter
      (verbose-format "~&State is: ~a. Mode is: ~a.~%" state (mode state))
      (ecase (mode state)
        (check-for-done
         (if (empty-p state)
             ;; because we are doing HTN Planning, if there are no tasks, we have a plan.
             (setf (mode state) 'extract-plan)
             ;; (cond ((check-for-done state which-plans)
             ;;        (setf (mode state) 'extract-plan))
             ;;       (t (stack-backtrack state))))
             (setf (mode state) 'check-for-immediate-task)))
        (check-for-immediate-task
         (cond ((immediate-tasks state)
                (let ((state (prepare-choose-immediate-task-state state)))
                  (setf (mode state) 'pop-immediate-task)))
               (t
                (setf (mode state) 'prepare-to-choose-toplevel-task))))
        (pop-immediate-task
         (if (choose-immediate-task-state state)
             (setf (mode state) 'expand-task)
             (stack-backtrack state)))
        (prepare-to-choose-toplevel-task
         (let ((tasks (task-sorter domain (top-tasks state) (unifier state))))
           (unless tasks (error "Should never get to ~A with no top-tasks." (mode state)))
           (setf (alternatives state) tasks
                 (mode state) 'pop-toplevel-task)))
        (pop-toplevel-task
         (if (choose-toplevel-task state)
             (setf (mode state) 'expand-task)
             (stack-backtrack state)))
        (expand-task
         (let ((task (current-task state)))
           (trace-print :tasks (get-task-name task) (world-state state)
               "~2%Depth ~s, trying task ~s"
               (depth state)
               (apply-substitution task (unifier state)))
           (if (primitivep (get-task-name task))
               (setf (mode state) 'expand-primitive-task)
               (setf (mode state) 'prepare-to-choose-method))))
        (expand-primitive-task
         (if (expand-primitive-state state domain)
             (progn
               (setf (mode state) 'check-for-done)
               (incf (depth state)))

             (stack-backtrack state)))
        (prepare-to-choose-method
         (let* ((task-name (get-task-name (current-task state)))
                (methods (methods domain task-name)))
           ;; possibly the following should be a continuable error.
           (unless methods (error "No methods for complex task ~a" task-name))
           (setf (alternatives state) methods)
           (setf (mode state) 'choose-method)))
        (choose-method
         (if (choose-method-state state domain)
             (setf (mode state) 'choose-method-bindings)
             (progn
               (let ((task1 (current-task state))
                     (depth (depth state))
                     (state (world-state state)))
                 (trace-print :tasks (get-task-name task1) state
                              "~2%Depth ~s, backtracking from task~%      task ~s"
                              depth
                              task1))
             (stack-backtrack state))))
        (choose-method-bindings
           (if (choose-method-bindings-state state)
               (progn
                 (setf (mode state) 'check-for-done)
                 (incf (depth state)))
               (stack-backtrack state)))
        (extract-plan
         (return
           (check-plans-found state)))))))

;;; Stubs of support functions
(defun CHOOSE-METHOD-BINDINGS-STATE (state)
  (with-slots (alternatives backtrack-stack
               current-task
               top-tasks tasks) state
    (when alternatives            ; method alternatives remain
      (let ((method-body-unifier (pop alternatives)))
        (destructuring-bind ((label . reduction) . unifier)
            method-body-unifier
          (push (make-cs-state :mode (mode state)
                               :current-task current-task
                               :alternatives alternatives)
                backtrack-stack)
          (push (make-method-instantiation
                 :unifier (unifier state)
                 :top-tasks top-tasks
                 :tasks tasks)
                backtrack-stack)
          (multiple-value-setq (top-tasks tasks)
            (apply-method-bindings current-task top-tasks tasks
                                   reduction unifier))
          (trace-print :methods label (world-state state)
                       "~2%Depth ~s, applying method ~s~%      task ~s~% reduction ~s"
                         (depth state) label current-task reduction)
          (setf (unifier state) unifier)))
      t)))
(defun CHOOSE-METHOD-STATE (state domain)
  (with-slots (alternatives backtrack-stack) state
    (when alternatives            ; method alternatives remain
      (let ((method (pop alternatives)))
        (push (make-cs-state :mode (mode state)
                             :current-task (current-task state)
                             :alternatives alternatives)
              backtrack-stack)
        (multiple-value-bind (expansions unifiers)
            (apply-method domain (world-state state)
                          (get-task-body (current-task state))
                          method (protections state)
                          (depth state) (unifier state))
          (when expansions
            (setf alternatives (mapcar #'cons expansions unifiers))
            t))))))
(defun EXPAND-PRIMITIVE-STATE (state domain)
  ;; first we need to record what we will need to pop...
  (with-slots (top-tasks tasks protections partial-plan
               current-task depth
               unifier cost backtrack-stack
               world-state)
      state
    (push (make-prim-state-expand :top-tasks top-tasks
                                  :tasks tasks
                                  :protections protections
                                  :partial-plan partial-plan
                                  :unifier unifier
                                  :partial-plan-cost cost)
          backtrack-stack)
    (multiple-value-bind (success top-tasks1 tasks1 protections1 planned-action unifier1 tag prim-cost)
        (seek-plans-primitive-1 domain current-task world-state tasks top-tasks depth protections unifier)
      (when success
        (setf top-tasks top-tasks1
              tasks tasks1
              protections protections1
              partial-plan (cons prim-cost (cons planned-action (partial-plan state)))
              unifier unifier1)
        (incf cost prim-cost)
        (push (make-world-state-tag :tag tag) (backtrack-stack state))
        t))))
(defun CHOOSE-TOPLEVEL-TASK (state)
  (when (alternatives state)
    (with-slots (current-task alternatives) state
      ;; heuristic already applied here.
      (setf current-task (pop alternatives))
      (push (make-cs-state :alternatives alternatives
                           :current-task current-task
                           :mode (mode state))
            (backtrack-stack state))
      state)))
(defun IMMEDIATE-TASKS (state)
  (get-immediate-list (top-tasks state)))
(defun CHECK-FOR-DONE (state which-plans)
  (let ((*plans-found* (plans-found state)))
    (when-done t)))
(defun EMPTY-P (state)
  (with-slots (top-tasks) state
    (or (null top-tasks) (equal top-tasks '(NIL)))))
;;; FIXME: for now we just extract the plan -- as if we only are
;;; finding the first plan.  Simplification to get things done
;;; more quickly.
(defun check-plans-found (state)
  (with-slots (partial-plan) state
    (when partial-plan
      (list ; comply with FIND-PLANS return type by returning a list of plans
       ;; in this case always a singleton or nil.
       (strip-NOPs (reverse partial-plan))))))
(defun prepare-choose-immediate-task-state (state)
  (let ((immediates (immediate-tasks state)))
    (setf (alternatives state) immediates)
    state))
(defun choose-immediate-task-state (state)
  (when (alternatives state)
    (with-slots (current-task alternatives) state
      (setf current-task
            (choose-immediate-task alternatives (unifier state)))
      (setf alternatives (remove current-task alternatives :test 'eq))
      (push (make-cs-state :alternatives alternatives
                           :current-task current-task
                           :mode (mode state))
            (backtrack-stack state))
      state)))
(defun stack-backtrack (state)
  "Chronological backtracking only, for now."
  (verbose-format "~&Backtracking:~%")
  (iter (for entry = (pop (backtrack-stack state)))
    (verbose-format "~T~a~%" entry)
    (when (typep entry 'bottom-of-stack)
      (throw 'search-failed nil))
    (do-backtrack entry state)
    (when (typep entry 'choice-entry)
      (return t))))
