(in-package :shop-user)
; This file illustrates some uses of ordered and unordered task lists.
; A couple of notes about this file:

; - The problems p1 and p2 are different ways of accomplishing the
;   same effect.  The former combines the operators and ordering
;   constraints in one big method.  The latter divides this
;   information across multiple methods.  Problem p3 also performs the
;   same operators but does so under a different starting state.

; - The ordering constraints require that !b1 always be invoked before
;   !b2, but do not impose any restrictions on where in the process !a1
;   and !a2 occur.

; - Operator !a1 asserts the fact foo on the given argument and
;   operators !a2 retracts that fact.  This fact is a precondition for
;   !b1.  This interaction restricts the range of possible orderings
;   for valid plans.

; - The find-plans commands below are invoked with the :which :all
;   argument to illustrate all possible orderings that can be produced
;   for these examples.  In practice, however, you will most often
;   want to use :which :first (the default) to just find a single plan
;   (since finding all possible plans is typically very expensive or
;   even infinitely expensive for most realistic domains).

(defdomain ab (
  (:operator (!a1 ?x) () () ((foo ?x)))
  (:operator (!a2 ?x) () ((foo ?x)) ())
  (:operator (!b1 ?x) (foo ?x) () ())
  (:operator (!b2 ?x) () () ())

  (:method (a-and-b ?x)
	   ()
	   (:unordered (:unordered (!a1 ?x) (!a2 ?x))
		       (:ordered (!b1 ?x) (!b2 ?x))))

  (:method (a ?x)
	   ()
	   (:unordered (!a1 ?x) (!a2 ?x)))
	   
  (:method (b ?x)
	   ()
	   (:ordered (!b1 ?x) (!b2 ?x)))))

(defproblem p1 ab
  () (a-and-b smurf))

(defproblem p2 ab
  () (:unordered (a smurf) (b smurf)))

(defproblem p3 ab
  ((foo smurf)) (a-and-b smurf))

(find-plans 'p1 :verbose :plans :which :all)
(find-plans 'p2 :verbose :plans :which :all)
(find-plans 'p3 :verbose :plans :which :all)
