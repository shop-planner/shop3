(in-package :shop2-user)
; This extremely simple example shows some of the most essential
;   features of SHOP2.

(defdomain basic-example (
  (:operator (!pickup ?a) () () ((have ?a)))
  (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())

  (:method (swap ?x ?y)
    ((have ?x))
    ((!drop ?x) (!pickup ?y))
    ((have ?y))
    ((!drop ?y) (!pickup ?x)))))

(defproblem problem1 basic-example
  ((have banjo)) ((swap banjo kiwi)))

(find-plans 'problem1 :verbose :plans)
