(define (domain zeno-travel)
(:requirements :durative-actions :typing)
(:types aircraft person city flevel - object)
(:predicates (at ?x - (either person aircraft) ?c - city)
             (in ?p - person ?a - aircraft)
	     (fuel-level ?a - aircraft ?l - flevel)
	     (next ?l1 ?l2 - flevel))


(:durative-action board
 :parameters (?p - person ?a - aircraft ?c - city)
 :duration (= ?duration 20)
 :condition (and (at start (at ?p ?c))
                 (over all (at ?a ?c)))
 :effect (and (at start (not (at ?p ?c)))
              (at end (in ?p ?a))))

(:durative-action debark
 :parameters (?p - person ?a - aircraft ?c - city)
 :duration (= ?duration 30)
 :condition (and (at start (in ?p ?a))
                 (over all (at ?a ?c)))
 :effect (and (at start (not (in ?p ?a)))
              (at end (at ?p ?c))))

(:durative-action fly 
 :parameters (?a - aircraft ?c1 ?c2 - city ?l1 ?l2 - flevel)
 :duration (= ?duration 180)
 :condition (and (at start (at ?a ?c1))
                 (at start (fuel-level ?a ?l1))
		 (at start (next ?l2 ?l1)))
 :effect (and (at start (not (at ?a ?c1)))
              (at end (at ?a ?c2))
              (at end (not (fuel-level ?a ?l1)))
              (at end (fuel-level ?a ?l2)))) 
                                  
(:durative-action zoom
 :parameters (?a - aircraft ?c1 ?c2 - city ?l1 ?l2 ?l3 - flevel)
 :duration (= ?duration 100)
 :condition (and (at start (at ?a ?c1))
                 (at start (fuel-level ?a ?l1))
		 (at start (next ?l2 ?l1))
		 (at start (next ?l3 ?l2))
		)
 :effect (and (at start (not (at ?a ?c1)))
              (at end (at ?a ?c2))
              (at end (not (fuel-level ?a ?l1)))
              (at end (fuel-level ?a ?l3))
	)
) 

(:durative-action refuel
 :parameters (?a - aircraft ?c - city ?l - flevel ?l1 - flevel)
 :duration (= ?duration 73)
 :condition (and (at start (fuel-level ?a ?l))
                 (at start (next ?l ?l1))
                 (over all (at ?a ?c)))
 :effect (and (at end (fuel-level ?a ?l1)) (at end (not (fuel-level ?a ?l)))))


)
