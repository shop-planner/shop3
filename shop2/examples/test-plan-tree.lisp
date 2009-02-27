;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This domain illustrates a bug in tree extraction where the tree
;;;    subtasks are not correctly linked to parent tasks.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2004/02/05:rpg] Created.
;;;
;;;---------------------------------------------------------------------------
(in-package :shop2-user)
(defdomain check-plan-tree-structure
    (
     (:method (test-task ?s ?e)
	      ;; preconditions
	      ()
	      (:ordered (!!record-start ?s)
			(:ordered (!subtask1)
				  (!subtask2))
			(!!record-end ?e)))

     (:method (test-task2 ?s ?e)
	      ;; preconditions
	      ()
	      (:ordered (!!record-start ?s)
			(!subtask1)
			(!subtask2)
			(!!record-end ?e)))

     (:method (test-task3 ?s ?e)
	      ;; preconditions
	      ()
	      (:ordered (!!record-start ?s)
			(!subtask1)
			(subtask3 ?s3 ?e3)
			(!!record-end ?e)))

     (:operator (!subtask1)
		((time ?t)
		 (assign ?t1 (call + ?t 10)))
		;; delete
		((time ?t))
		;; add
		((time ?t1)))

     (:operator (!subtask2)
		((time ?t)
		 (assign ?t1 (call + ?t 20)))
		;; delete
		((time ?t))
		;; add
		((time ?t1)))

     (:method (subtask3 ?s ?e)
	      ()
	      (:ordered (!!record-start ?s)
			(!subtask1)
			(!subtask2)
			(!!record-end ?e)))

     (:operator (!!record-start ?s)
		((time ?s))
		()
		())

     (:operator (!!record-end ?e)
		((time ?e))
		()
		())))

(defproblem simple-plan-tree-test
    ((time 0))
  (test-task ?s ?e))
	      
(defproblem non-recursive-plan-tree-test
    ((time 0))
  (test-task2 ?s ?e))


(defproblem recursive-plan-tree-test
    ((time 0))
  (test-task3 ?s ?e))

(defun test-plan-tree-test ()
  (and 
   (multiple-value-bind (plans ignore-me trees)
       (find-plans 'simple-plan-tree-test :plan-tree t)
     (declare (ignore ignore-me))
     (and (equalp (first plans)
		  '((!!RECORD-START 0) 1.0 (!SUBTASK1) 1.0 (!SUBTASK2) 1.0 (!!RECORD-END 30) 1.0))
	  (equalp (first trees)
		  '(((TEST-TASK 0 30) (1.0 (!!RECORD-START 0) 0) (1.0 (!SUBTASK1) 1)
		     (1.0 (!SUBTASK2) 2) (1.0 (!!RECORD-END 30) 3))))))
   (multiple-value-bind (plans ignore-me trees)
       (find-plans 'non-recursive-plan-tree-test :plan-tree t)
     (declare (ignore ignore-me))
     (and (equalp (first plans)
		  '((!!RECORD-START 0) 1.0 (!SUBTASK1) 1.0 (!SUBTASK2) 1.0 (!!RECORD-END 30) 1.0))
	  (equalp (first trees)
		  '(((TEST-TASK2 0 30) (1.0 (!!RECORD-START 0) 0) (1.0 (!SUBTASK1) 1)
		     (1.0 (!SUBTASK2) 2) (1.0 (!!RECORD-END 30) 3))))
	  )
     )
   (multiple-value-bind (plans ignore-me trees)
       (find-plans 'recursive-plan-tree-test :plan-tree t)
     (declare (ignore ignore-me))
     (and (equalp (first plans)
		  '((!!RECORD-START 0) 1.0 (!SUBTASK1) 1.0 (!!RECORD-START 10) 1.0 (!SUBTASK1)
		    1.0 (!SUBTASK2) 1.0 (!!RECORD-END 40) 1.0 (!!RECORD-END 40) 1.0))
	  (equalp (first trees)
		  '(((TEST-TASK3 0 40)
		     (1.0 (!!RECORD-START 0) 0)
		     (1.0 (!SUBTASK1) 1)
		     ((SUBTASK3 10 40)
		      (1.0 (!!RECORD-START 10) 2)
		      (1.0 (!SUBTASK1) 3)
		      (1.0 (!SUBTASK2) 4)
		      (1.0 (!!RECORD-END 40) 5))
		     (1.0 (!!RECORD-END 40) 6))))))))
		    
     
  
