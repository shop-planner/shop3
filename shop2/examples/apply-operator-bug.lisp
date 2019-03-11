(in-package :shop-user)

(defdomain display-buggy-operator-variable-bindings
    (
     (:method (should-fail ?x)
	      ()
	      (:ordered
	       (!bind-variable ?x)
	       (!check-variable ?x)))

     (:method (should-succeed ?x)
	      ()
	      (:ordered
	       (!bind-variable ?x)
	       (!check-bound ?x)))

     (:operator (!bind-variable ?x)
		;; preconditions
		((assign ?x 12))
		()
		())

     (:operator (!check-bound ?x)
		;; preconditions
		((call pprint-t (quote ?x))
		 (call = (quote ?x) 12))
		()
		())


     (:operator (!check-variable ?x)
		;; preconditions
		((call pprint-t (quote ?x))
		 (call variablep (quote ?x)))
		()
		())))

(defun pprint-t (x)
  "Otherwise PPRINT returns NIL and makes the precondition
fail."
  (pprint x)
  t)

(defproblem this-plan-should-fail
    ()
    (should-fail ?x))

(defproblem this-plan-should-work
    ()
    (should-succeed ?x))

(defun apply-operator-bug-test ()
  (and 
   (let ((result (find-plans 'this-plan-should-fail)))
     (null result))
   (let ((result (find-plans 'this-plan-should-work)))
     (equalp (first result) '((!BIND-VARIABLE 12) 1.0 (!CHECK-BOUND 12) 1.0)))))