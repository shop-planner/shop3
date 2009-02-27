(in-package :shop2-user)

;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This domain structured to test to see if there are still ;;;
;;;    systematic problems with deep plan trees involving "synthetic
;;;    attributes": that is, task parameters that are bound from
;;;    states, but must be propagated throughout the HTN.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2004/02/10:rpg] Created.
;;;
;;;---------------------------------------------------------------------------


(defdomain test-tree-problems
    (
     (:method (top-level-task ?param)
	      ;; no preconditions
	      ()
	      ;;task net with two sub-tasks, both complex
	      (:ordered
	       (subtask1 ?param)
	       (subtask2 ?param)))

     (:method (subtask1 ?p)
	      ()
	      (!bind-param ?p))

     (:method (subtask2 ?p)
	      ()
	      (!consume-param ?p))

     (:operator (!bind-param ?p)
		;; preconditions
		((parameter-value ?p))
		;; leaves the world state as it is...
		()
		())

     (:operator (!consume-param ?p)
		;; make sure the variable binding has been propagated
		((bound ?p))
		()
		())

     (:- (bound ?p)
	 (not (unbound ?p)))

     ;; :unbound will only unify with the parameter if it hasn't
     ;; otherwise been bound...
     (:- (unbound :unbound) ())))

(defproblem test-binding-problem
    ((parameter-value foo))
  (top-level-task ?p))


(defun tree-binding-problems-test ()
  (multiple-value-bind (plans something trees)
      (find-plans 'test-binding-problem :plan-tree t)
    (declare (ignore something))
    (and (equalp plans '(((!BIND-PARAM FOO) 1.0 (!CONSUME-PARAM FOO) 1.0)))
	 (equalp trees
		 '((((TOP-LEVEL-TASK FOO)
		     ((SUBTASK1 FOO)
		      (1.0 (!BIND-PARAM FOO) 0))
		     ((SUBTASK2 FOO)
		      (1.0 (!CONSUME-PARAM FOO) 1)))))))))

      
    