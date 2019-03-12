(define (problem ZTRAVEL-1-2)
(:domain zeno-travel)
(:objects
	plane1 - aircraft
	person1 - person
	person2 - person
	city0 - city
	city1 - city
	city2 - city
	)
(:init
	(at plane1 city0)
	(= (slow-speed plane1) 198)
	(= (fast-speed plane1) 449)
	(= (capacity plane1) 10232)
	(= (fuel plane1) 3956)
	(= (slow-burn plane1) 4)
	(= (fast-burn plane1) 15)
	(= (refuel-rate plane1) 2904)
	(at person1 city0)
	(at person2 city2)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 678)
	(= (distance city0 city2) 775)
	(= (distance city1 city0) 678)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 810)
	(= (distance city2 city0) 775)
	(= (distance city2 city1) 810)
	(= (distance city2 city2) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at plane1 city1)
	(at person1 city0)
	(at person2 city2)
	))

(:metric minimize (+ (* 4 (total-time))  (* 0.005 (total-fuel-used))))
)
