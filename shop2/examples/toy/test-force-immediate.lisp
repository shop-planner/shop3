(in-package :shop2-user)

;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This SHOP2 domain seems to illustrate a problem with using
;;;    FORCE-IMMEDIATE-REDUCTION.  Intuitively, it seems that there
;;;    should be a plan for (top-level), viz: ((!a1) (!c1) (!a2)
;;;    (!c2)), but SHOP2 cannot find this plan.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2004/04/05:rpg] Created.
;;;
;;;---------------------------------------------------------------------------


(defdomain test-force-immediate
    (
     (:method (top-level)
	      ()
	      (:unordered
	       (:ordered (a) (b))
	       (:ordered (c) (d))))

     (:method (a)
	      ()
	      (:ordered (!a1) (!a2)))

     (:method (c)
	      ()
	      (:ordered (!c1) (!c2)))

     (:operator (!a1)
		()
		()
		;; add 
		((a1)))

     (:operator (!a2)
		;; preconditions
		((a1)(c1))
		()
		())

     (:operator (!c1)
		()
		()
		;; add
		((c1)))

     (:operator (!c2)
		;; preconditions
		((a1)(c1))
		()
		())

     (:method (b)
	      ()
	      ((!b1)))

     (:method (d)
	      ()
	      ((!d1)))

     (:operator (!b1)
		()
		()
		())

     (:operator (!d1)
		()
		()
		())))

(make-problem 'test-immediate
	      nil
	      '(top-level))
		
     

     
	      
			 
		
     

     
	      
			 