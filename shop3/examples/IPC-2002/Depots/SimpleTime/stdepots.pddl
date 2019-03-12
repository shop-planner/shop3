(define (domain Depot)
(:requirements :typing :durative-actions)
(:types place locatable - object
	depot distributor - place
        truck hoist surface - locatable
        pallet crate - surface)

(:predicates (at ?x - locatable ?y - place) 
             (on ?x - crate ?y - surface)
             (in ?x - crate ?y - truck)
             (lifting ?x - hoist ?y - crate)
             (available ?x - hoist)
             (clear ?x - surface))


	
(:durative-action Drive
:parameters (?x - truck ?y - place ?z - place) 
:duration (= ?duration 10)
:condition (and (at start (at ?x ?y)))
:effect (and (at start (not (at ?x ?y))) (at end (at ?x ?z))))

(:durative-action Lift
:parameters (?x - hoist ?y - crate ?z - surface ?p - place)
:duration (= ?duration 1)
:condition (and (over all (at ?x ?p)) (at start (available ?x)) (at start (at ?y ?p)) (at start (on ?y ?z)) (at start (clear ?y)))
:effect (and (at start (not (at ?y ?p))) (at start (lifting ?x ?y)) (at start (not (clear ?y))) (at start (not (available ?x))) 
             (at start (clear ?z)) (at start (not (on ?y ?z)))))

(:durative-action Drop 
:parameters (?x - hoist ?y - crate ?z - surface ?p - place)
:duration (= ?duration 1)
:condition (and (over all (at ?x ?p)) (over all (at ?z ?p)) (over all (clear ?z)) (over all (lifting ?x ?y)))
:effect (and (at end (available ?x)) (at end (not (lifting ?x ?y))) (at end (at ?y ?p)) (at end (not (clear ?z))) (at end (clear ?y))
		(at end (on ?y ?z))))

(:durative-action Load
:parameters (?x - hoist ?y - crate ?z - truck ?p - place)
:duration (= ?duration 3)
:condition (and (over all (at ?x ?p)) (over all (at ?z ?p)) (over all (lifting ?x ?y)))
:effect (and (at end (not (lifting ?x ?y))) (at end (in ?y ?z)) (at end (available ?x))))

(:durative-action Unload 
:parameters (?x - hoist ?y - crate ?z - truck ?p - place)
:duration (= ?duration 4)
:condition (and (over all (at ?x ?p)) (over all (at ?z ?p)) (at start (available ?x)) (at start (in ?y ?z)))
:effect (and (at start (not (in ?y ?z))) (at start (not (available ?x))) (at start (lifting ?x ?y))))

)
