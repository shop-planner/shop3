(in-package :shop2-user)
; This file illustrates the use of the :sort-by keyword in SHOP2.
;  When that keyword is used, alternative possible bindings for a given
;  method are sorted by the value bound to the specified variable.

(defun test-compare (x1 x2)
  (destructuring-bind (qual1 . val1) x1
    (destructuring-bind (qual2 . val2) x2
      (if (eq qual1 'preferred)
	  (if (eq qual2 'unpreferred) t
	    (> val1 val2))
	(if (eq qual2 'preferred) nil
	  (> val1 val2))))))

(defdomain ab (
  (:operator (!a1 ?num) () () ())
  (:operator (!b1 ?num) () () ())

  (:method (a)
	   (foo ?x)
	   ((!a1 ?x)))
	   
  (:method (b)
	   (:sort-by ?x (foo ?x))
	   ((!b1 ?x)))

  ;; check that numeric predicates work even with 
  (:method (b1)
	   (:sort-by ?x #'> (foo ?x))
	   ((!b1 ?x)))


  (:method (c)
	   (:sort-by (* ?x ?x) #'> (foo ?x))
	   ((!b1 ?x)))

  ;; unfortunately, eval causes problem when working with stuff other
  ;; than numbers. [2004/03/26:rpg]
  (:method (d)
	   (:sort-by ?key #'test-compare
		     (and (foo ?x) (quality ?x ?val)
			  (assign ?key (cons ?val ?x))))
	   ((!b1 ?x)))

  (:method (d-fixed)
	   (:sort-by (quote ?key)
		     #'test-compare
		     (and (foo ?x) (quality ?x ?val)
			  (assign ?key (cons (quote ?val) ?x))))
	   ((!b1 ?x)))
  ))

(defun test-compare (x1 x2)
  (destructuring-bind (qual1 . val1) x1
    (destructuring-bind (qual2 . val2) x2
      (if (eq qual1 'preferred)
	  (if (eq qual2 'unpreferred) t
	    (> val1 val2))
	(if (eq qual2 'preferred) nil
	  (> val1 val2))))))

(defproblem p1 ab
  ((foo 1) (foo 3) (foo -2)) ((a)))

(defproblem p2 ab
  ((foo 1) (foo 3) (foo -2)) ((b)))

(defproblem p2-reverse ab
  ((foo 1) (foo 3) (foo -2)) ((b1)))

(defproblem p3 ab
  ((foo 1) (foo 3) (foo -2)) ((c)))

(defproblem p4 ab
  ((foo 1) (quality 1 preferred)
   (foo 3) (quality 3 unpreferred)
   (foo -2) (quality -2 preferred))
  ((d)))

(defproblem p5 ab
  ((foo 1) (quality 1 preferred)
   (foo 3) (quality 3 unpreferred)
   (foo -2) (quality -2 preferred))
  ((d-fixed)))


(find-plans 'p1 :verbose :plans :which :first)
(find-plans 'p1 :verbose :plans :which :all)
(find-plans 'p2 :verbose :plans :which :first)
(find-plans 'p2 :verbose :plans :which :all)
(find-plans 'p2-reverse :verbose :plans :which :first)
(find-plans 'p2-reverse :verbose :plans :which :all)
(find-plans 'p3 :verbose :plans :which :first)
(find-plans 'p3 :verbose :plans :which :all)
(find-plans 'p4 :verbose :plans :which :first)
(find-plans 'p4 :verbose :plans :which :all)
(find-plans 'p5 :verbose :plans :which :first)
(find-plans 'p5 :verbose :plans :which :all)

