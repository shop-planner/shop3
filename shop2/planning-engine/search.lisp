;;; -*- Mode: common-lisp; package: shop2; -*-
;;;
;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;;
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;;
;;; The Original Code is SHOP2.
;;;
;;; The Initial Developer of the Original Code is the University of
;;; Maryland. Portions created by the Initial Developer are Copyright (C)
;;; 2002,2003 the Initial Developer. All Rights Reserved.
;;;
;;; Additional developments made by Robert P. Goldman, John Maraist.
;;; Portions created by Drs. Goldman and Maraist are Copyright (C)
;;; 2004-2007 SIFT, LLC.  These additions and modifications are also
;;; available under the MPL/GPL/LGPL licensing terms.
;;;
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.
;;; ----------------------------------------------------------------------

;;; Smart Information Flow Technologies Copyright 2006-2007 Unpublished work
;;;
;;; GOVERNMENT PURPOSE RIGHTS
;;;
;;; Contract No.         FA8650-06-C-7606,
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;;
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.


(in-package :shop2)

;;; SEEK-PLANS is the basic planning engine.  Here are its arguments:
;;;  - STATE is the current state;
;;;  - TASKS is the list of remaining tasks to accomplish;
;;;  - TOP-TASKS is the agenda of tasks to be worked first,
;;;    based on the :immediate keyword
;;;  - PARTIAL-PLAN is the partial plan accumulated so far, with the
;;;    steps listed in reverse order;
;;;  - PARTIAL-PLAN-COST is the total cost of the partial plan
;;;  - DEPTH is the current level of recursive calls to SEEK-PLANS;
;;;  - WHICH-PLANS has the same meaning as the WHICH argument to FIND-PLANS
;;; Rather than returning the plans, SEEK-PLANS puts them into the dynamically
;;; scoped variable *PLANS-FOUND*.  Ordinarily I'd consider this to be poor
;;; coding style, but in this case it makes the coding significantly cleaner.
;;;
;;; Seek-plans has been broken down into the following lower level
;;; functions, below: seek-plans-task, seek-plans-primitive,
;;;                   seek-plans-nonprimitive, seek-plans-null
(let (#+allegro (compiler:tail-call-non-self-merge-switch t))
(defmethod seek-plans ((domain domain) state tasks top-tasks partial-plan partial-plan-cost
                         depth which-plans protections
                         unifier)
  (when (time-expired-p)
    (if *print-stats* (format t "~%Terminating because the time limit has expired."))
    (throw *internal-time-tag* nil))

  (setq *current-plan* partial-plan)
  (setq *current-tasks* tasks)
  ;; moved this setf here, to try to get access to the state earlier.
  (setf *current-state* state)
  (setq *expansions* (1+ *expansions*))

  (cond
    ;; if no top-tasks to accomplish, then we have an answer
    ((or (null top-tasks) (equal top-tasks '(NIL)))
     (seek-plans-null domain state which-plans partial-plan partial-plan-cost depth
                      unifier))

    ;; else if we've hit the depth bound, then fail
    ((and *depth-cutoff* (>= depth *depth-cutoff*)))

    ;; else look at the agenda
    (t
     (let ((immediate-tasks (get-immediate-list top-tasks)))
       (if immediate-tasks
           (let ((task1 (choose-immediate-task immediate-tasks unifier)))
             ;; in temporal planning we may be unable to choose a
             ;; schedulable immediate task, in which case we must fail
             ;; and backtrack. [2004/04/05:rpg]
             (when task1
               (seek-plans-task domain
                task1 state tasks top-tasks partial-plan partial-plan-cost
                depth which-plans protections
                                        ;tree
                unifier))
             ;; rewrote this as if-then-else, to make it clearer...
             (when (and *plans-found* (eq which-plans :first)
                        (not (optimize-continue-p which-plans)))
               (return-from seek-plans nil))
             (cond (task1

                    (trace-print :tasks (get-task-name task1) state
                                 "~2%Depth ~s, backtracking from task~%      task ~s"
                                 depth
                                 task1)
                    (backtrack "Task ~S failed" task1))
                   (t
                    (trace-print :tasks (get-task-name task1) state
                                 "~2%Depth ~s, unable to choose task from immediate task-list, backtracking~%"
                                 depth)
                    (backtrack "Couldn't choose from immediate task-list"))
                   ))
           ;; no :IMMEDIATE tasks, so do the rest of the tasks on the agenda
           (if *hand-steer*
               (let ((task1 (user-choose-task top-tasks unifier)))
                 (seek-plans-task domain task1 state tasks top-tasks partial-plan
                                  partial-plan-cost depth which-plans
                                  protections
                                  unifier)
                 (when (and *plans-found* (eq which-plans :first)
                            (not (optimize-continue-p which-plans)))
                   (return-from seek-plans nil))
                 (trace-print :tasks (get-task-name task1) state
                              "~2%Depth ~s, backtracking from task ~s"
                              depth
                              task1)
                 (backtrack "Task ~S failed" task1))
               (loop for task1 in (task-sorter domain top-tasks unifier)
                     while task1
                     do (seek-plans-task domain task1 state tasks top-tasks partial-plan
                             partial-plan-cost depth which-plans
                             protections
                             unifier)
                     when (and *plans-found* (eq which-plans :first)
                                             (not (optimize-continue-p which-plans)))
                       do (return-from seek-plans nil)
                     do (trace-print :tasks (get-task-name task1) state
                              "~2%Depth ~s, backtracking from task ~s"
                              depth
                              task1)
                        (backtrack "Task ~S failed" task1)))))))))

(defun choose-immediate-task (immediate-tasks unifier)
  "Which of the set of IMMEDIATE-TASKS should SHOP2 work on
first?  Defaults to the first element, unless the user intervenes."
  (if *hand-steer*
      (user-choose-task immediate-tasks unifier t)
      (car immediate-tasks)))

(defmethod task-sorter ((domain domain) task-list unifier)
  "The default task-sorter method simply loops through the
elements of task-list."
  (declare (ignore unifier))
  task-list)

(defun user-choose-task (task-list unifier &optional (immediate nil))
  "Function called to allow a user to choose the next task for expansion, instead
of SHOP2."
  (let ((num-tasks (length task-list)))
    (format t "~&Choose a~:[ ~;n immediate ~] task for expansion:~%"
            immediate)
    (if (= num-tasks 1)
        (let ((input (y-or-n-p "~&Only one candidate ~:[~;immediate ~] task for expansion:~%~T~S~%Proceed?~%"
                               immediate (apply-substitution (first task-list) unifier))))
          (if input
              (first task-list)
              (if (y-or-n-p "Abort planning?")
                  (throw 'user-done nil)
                  (user-choose-task task-list immediate))))
        (progn
          (loop for i from 0
                for task in task-list
                for bound-task = (apply-substitution task unifier)
                do (format t "~D - ~S~%" i bound-task))
          (let ((input (read)))
            (if (and (typep input 'fixnum)
                     (>= input 0)
                     (< input num-tasks))
                (nth input task-list)
                (user-choose-task task-list immediate)))))))

(let (#+allegro (compiler:tail-call-non-self-merge-switch t))
(defmethod seek-plans-task (domain task1 state tasks top-tasks partial-plan
                              partial-plan-cost depth which-plans
                              protections
                              unifier
                              )
  (let ((task-name (get-task-name task1))
        (task-body (get-task-body task1)))
    (trace-print :tasks task-name state
                 "~2%Depth ~s, trying task ~s"
                 depth
                 (apply-substitution task1 unifier))
;;    (y-or-n-p "continue?")
    (if (primitivep task-name)
        (seek-plans-primitive domain task1 task-name task-body state tasks top-tasks
                              partial-plan partial-plan-cost depth which-plans
                              protections
                              unifier
                              )
        (seek-plans-nonprimitive domain task1 task-name task-body state tasks top-tasks
                                 partial-plan partial-plan-cost depth
                                 which-plans protections
                                 unifier
                                 )))))

;;; so we can use etypecase in seek-plans-primitive
(deftype operator ()
  '(satisfies operator-p))
(deftype pddl-action ()
  '(satisfies pddl-action-p))

(let (#+allegro (compiler:tail-call-non-self-merge-switch t))
(defmethod seek-plans-primitive ((domain domain) task1 task-name task-body state tasks top-tasks
                                 partial-plan partial-plan-cost depth which-plans protections unifier)
;;; I commented out the following two lines since we are getting the domain object from the first parameter right now
;;; [2006/08/09:ugur]
;;  (declare (special *domain*))
;;  (let ((m (operator *domain* task-name)))
  (let ((m (operator domain task-name)))
    (unless m
      (error "No operator for task ~s" task1))
    (multiple-value-bind
        (result1 tag protections cost operator-unifier)
        ;; I can't yet turn this into method dispatch, because the
        ;; operators and pddl-actions, so far, are only :list type
        ;; structs --- this because they used to be simply lists.
        ;; Changing this will require tracking places where
        ;; unification happens and fixing them. [2006/07/30:rpg]
        (etypecase m
            (operator
             (apply-operator domain state task-body m protections depth
                             unifier))
            ;; pddl action
            (pddl-action
             (apply-action state task-body m protections depth
                             unifier)))
      (when (eql result1 'fail)
        (return-from seek-plans-primitive nil))

      (when *plan-tree*
        (record-operator task1 result1 operator-unifier))

      (multiple-value-bind (top-tasks1 tasks1)
          (delete-task-top-list top-tasks tasks task1)

        (let ((new-cost (+ cost partial-plan-cost)))
          (when (and *optimize-cost*
                     (not (acceptable-cost-p new-cost)))
            (trace-print :operators task-name state
                         "~2%Depth ~s, backtracking from operator ~s because the plan costs too much~%     task ~s~%     cost ~s"
                         depth task-name (cdr task1) new-cost)
            (backtrack "Exceeded plan cost with operator ~S" task-name)
            (retract-state-changes state tag)
            (return-from seek-plans-primitive nil))

          (seek-plans domain state tasks1 top-tasks1
                      (cons cost (cons result1 partial-plan))
                      new-cost (1+ depth) which-plans protections
                      operator-unifier))
        (retract-state-changes state tag)
        nil)))))

(let (#+allegro (compiler:tail-call-non-self-merge-switch t))
(defmethod seek-plans-nonprimitive ((domain domain) task1 task-name task-body state tasks
                                      top-tasks partial-plan partial-plan-cost
                                      depth which-plans protections
                                      in-unifier
                                      )
  (let ((methods (methods domain task-name)))
    (unless methods
      (error (make-condition 'no-method-for-task :task-name task-name)))
    (dolist (m methods)
      (multiple-value-bind (result1 unifier1)
          (apply-method domain state task-body m protections depth in-unifier)
        (when result1
          (loop for lr in result1
                as u1 in unifier1
                for label = (car lr)
                for r = (cdr lr)
                with tasks1 and top-tasks1
                when *plan-tree*
                  do (record-reduction task1 r u1)
                do (trace-print :methods label state
                         "~2%Depth ~s, applying method ~s~%      task ~s~%   precond ~s~% reduction ~s"
                         depth label task1 (fourth m) r)
                   (trace-print :tasks task-name state
                         "~2%Depth ~s, reduced task ~s~% reduction ~s"
                         depth task1 r)
                   (setq top-tasks1 (replace-task-top-list top-tasks task1 r))
                   (let ((new-task-net (replace-task-main-list tasks task1 r)))
                     (setq tasks1 new-task-net))
                   (seek-plans domain state tasks1 top-tasks1 partial-plan
                        partial-plan-cost (1+ depth) which-plans
                        protections
                        u1)
                when (and *plans-found* (eq which-plans :first)
                                        (not (optimize-continue-p which-plans)))
                  do (return-from seek-plans-nonprimitive nil))))))))

;;; Called when there are no top level tasks to run
(defmethod seek-plans-null ((domain domain) state which-plans partial-plan partial-plan-cost depth unifier)
  ;; note that ACCEPTABLE-COST is actually a misnomer.  If you aren't
  ;; thinking about cost at all (e.g., you are just trying to find the
  ;; first plan, this will end up being T). [2004/09/14:rpg]


  (let ((acceptable-cost (acceptable-cost-p partial-plan-cost))
        (final-plan (strip-NOPs (reverse partial-plan))))

    (trace-print :plans nil state
                 "~2%Depth ~D, have found a plan with cost ~D~%*optimize-cost* = ~S, *optimal-cost* = ~A~%"
                 depth partial-plan-cost
                 *optimize-cost* *optimal-cost*)
    ;; This hook is useful for external routines that invoke SHOP2
    ;;  and want to do something whenever a plan is found.  For example,
    ;;  the Java / SHOP2 interface uses this to store the final state
    ;;  of the plan.
    (when (fboundp 'plan-found-hook)
      (funcall (fdefinition 'plan-found-hook)
               state which-plans final-plan partial-plan-cost depth))


    ;; I believe the condition here boils down to "this is a redundant
    ;; plan --- you only want the first optimal plan, and this one
    ;; you've found has exactly the same cost as a previously found
    ;; one." [2005/01/06:rpg]
    (if (and *optimize-cost* acceptable-cost
             (eq which-plans :first)
             (equal *optimal-cost* partial-plan-cost))
        (trace-print :plans nil state
                     "~4TAlready have a plan with identical cost.~%")
        ;; old body of the UNLESS
        (progn
          (if (and *optimize-cost* acceptable-cost)
              (progn
                (trace-print :plans nil state
                             "~4TCost of new plan is acceptable.~%")
                (when (or (null *optimal-cost*)
                          (< partial-plan-cost *optimal-cost*))
                  (trace-print :plans nil state
                               "~4TStoring new plan as optimal plan.~%")
                  (setq *optimal-plan* final-plan)
                  (setq *optimal-cost* partial-plan-cost))


                (cond ((member which-plans '(:first :shallowest))
                       (trace-print :plans nil state "~4TThrowing away old plans.~%")
                       (dump-previous-plans!))
                      (t
                      (trace-print :plans nil state "~4TFiltering out other plans of higher cost.~%")
                      (dump-higher-cost-plans!))))
              (when *optimize-cost*
                (trace-print :plans nil state
                           "~4TCost of new plan is UNacceptable.~%")))
          (cond
            ((or (eq which-plans :all-shallowest)
                 (and (eq which-plans :shallowest) *optimize-cost*))
             (when (not (equal *depth-cutoff* depth))
               (dump-previous-plans!)
               (setq *depth-cutoff* depth)))
            ((eq which-plans :shallowest)
             (setq *depth-cutoff* (1- depth))
             (dump-previous-plans!)))
          (when acceptable-cost
            (trace-print :plans nil state "~4TStoring new plan in *plans-found*~%")
            (store-plan! final-plan state unifier))
            )))

    ;; should this better be (values)? [2004/02/11:rpg]
    nil)

(defun store-plan! (plan state unifier)
  (push-last plan *plans-found*)
  (push-last (copy-state state) *states-found*)
  (push-last unifier *unifiers-found*))

;;; helpers for SEEK-PLANS-NULL [2005/01/07:rpg]
(defun dump-previous-plans! ()
  "Clear out the data structures (special variables)
we use to return plans."
  (setf *plans-found* nil
        *unifiers-found* nil
        *states-found* nil))

(defun dump-higher-cost-plans! ()
  (loop for plan in *plans-found*
        for unifier in *unifiers-found*
        for state in *states-found*
        when (acceptable-cost-p (plan-cost plan))
          collect plan into plans
        and collect unifier into unifiers
        and collect state into states
        finally (setf *plans-found* plans
                      *unifiers-found* unifiers
                      *states-found* states)))

;;; end helpers for SEEK-PLANS-NULL

;;; get-immediate-list returns the first member in tasklist tl that has the
;;; first task with the keyword :immediate
;;; Furthermore, all iNOP's are considered immediate, because they don't do
;;; anything so you might as well run them right away (this is just a minor
;;; efficiency tweak if you have a domain with lots of unordered tasks which
;;; reduce to iNOP's).  - Murdock 12/01
(defun get-immediate-list (tl)
  (dolist (obj tl)
    (if (or (eq :immediate (second obj)) (eq '!!inop (second obj)))
      (return-from get-immediate-list (list obj))))
  (return-from get-immediate-list nil))

;;;---------------------------------------------------------------------------
;;; This should only be called on plans with COSTS IN THEM!!!!!
;;;---------------------------------------------------------------------------
(defun shorter-plan (plan)
  "Removes the internal operators from a plan sequence, and
returns the resulting new sequence.  AFAICT, this also removes the costs.
Non-destructive."
  (cond
   ((null plan) nil)
   ((internal-operator-p (first (first plan)))
    (shorter-plan (rest (rest plan))))
   (t
    (cons (first plan) (shorter-plan (rest (rest plan)))))))

(defun remove-costs (plan-and-costs)
  "The SHOP2 plans come with the operators interspersed with their costs.
This function just throws away the costs."
  (loop with planlist = plan-and-costs
        for (operator cost . rest) = planlist
        while planlist
        do (assert (numberp cost))
        collect operator
        do (setf planlist rest)))

;;; This function returns true iff additional optimization needs to be done.
(defun optimize-continue-p (which)
  (assert (or (eq which :first) (eq which :all)))
  (cond
   ((not *optimize-cost*) nil)
   ;; added this so that we can terminate immediately if we're
   ;; optimizing and only need one plan. [2005/01/10:rpg]
   ((and (eq which :first) (zerop *optimal-cost*)) nil)
   ((and (numberp *optimize-cost*)
         (numberp *optimal-cost*))
    (>= *optimal-cost* *optimize-cost*))
   (t t)))

;;; This function returns true iff a given cost is acceptible under the
;;;  current optimization requirements.
(defun acceptable-cost-p (cost)
  (cond
   ((numberp *optimize-cost*)
    (<= cost *optimize-cost*))
   ((and *optimize-cost*
         (not (eq *optimal-plan* 'fail)))
    (<= cost *optimal-cost*))
   (t t)))

;;; This function returns true iff there is a time limit and it has expired.
(defun time-expired-p ()
  (if *internal-time-limit*
      (>= (- (get-internal-run-time) *start-run-time*)
          *internal-time-limit*)
    nil))
