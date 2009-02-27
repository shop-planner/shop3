(in-package :shop2-user)
; This file illustrates the use of arguments to an operator that are
;  not bound in the invocation of the operator.  These arguments
;  become bound in the precondition of that operator, either directly
;  using assign or indirectly by matching to a term in the problem
;  state.

(defdomain ab (
  (:operator (!a1 ?n1 ?n2 ?n3 ?n4)
	     ((eval (< ?n1 ?n2))
	      (foo ?n3 ?n1)
	      (assign ?n4 (+ ?n1 ?n2)))
	     ((foo ?n1 ?n2))
	     ((foo ?n3 ?n4)))

  (:method (a)
	   (foo ?x ?y)
	   ((!a1 ?x ?y ?q ?w)))))
	   
(defproblem p1 ab
  ((foo 3 7) (foo 8 3) (foo -2)) ((a)))

(find-plans 'p1 :verbose :plans :which :all)
