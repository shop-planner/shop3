(define (problem ZTRAVEL-1-3)
(:domain zeno-travel)
(:objects
	plane1 - aircraft
	person1 - person
	person2 - person
	person3 - person
	city0 - city
	city1 - city
	city2 - city
	)
(:init
	(at plane1 city0)
	(= (slow-speed plane1) 192)
	(= (fast-speed plane1) 287)
	(= (capacity plane1) 6830)
	(= (fuel plane1) 1773)
	(= (slow-burn plane1) 3)
	(= (fast-burn plane1) 11)
	(= (refuel-rate plane1) 470)
	(at person1 city2)
	(at person2 city1)
	(at person3 city2)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 627)
	(= (distance city0 city2) 998)
	(= (distance city1 city0) 627)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 631)
	(= (distance city2 city0) 998)
	(= (distance city2 city1) 631)
	(= (distance city2 city2) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at plane1 city2)
	(at person1 city1)
	(at person3 city2)
	))

(:metric minimize (+ (* 1 (total-time))  (* 0.001 (total-fuel-used))))
)
