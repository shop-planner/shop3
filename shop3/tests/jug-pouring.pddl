(define (domain jug-pouring)
  (:requirements :typing :fluents)
  (:types jug)
  (:functions
   (amount ?j -jug) (capacity ?j -jug))
  (:action empty
    :parameters (?jug1 ?jug2 - jug)
    :precondition (>= (- (capacity ?jug2) (amount ?jug2)) (amount ?jug1))
    :effect (and (assign (amount ?jug1) 0)
                 (assign (amount ?jug2)
                         (+ (amount ?jug1) (amount ?jug2))))))