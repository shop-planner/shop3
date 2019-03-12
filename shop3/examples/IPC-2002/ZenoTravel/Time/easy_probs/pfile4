(define (problem ZTRAVEL-2-5)
(:domain zeno-travel)
(:objects
	plane1 - aircraft
	plane2 - aircraft
	person1 - person
	person2 - person
	person3 - person
	person4 - person
	person5 - person
	city0 - city
	city1 - city
	city2 - city
	)
(:init
	(at plane1 city2)
	(= (slow-speed plane1) 154)
	(= (fast-speed plane1) 393)
	(= (capacity plane1) 5304)
	(= (fuel plane1) 1048)
	(= (slow-burn plane1) 2)
	(= (fast-burn plane1) 7)
	(= (refuel-rate plane1) 566)
	(at plane2 city2)
	(= (slow-speed plane2) 176)
	(= (fast-speed plane2) 362)
	(= (capacity plane2) 6148)
	(= (fuel plane2) 973)
	(= (slow-burn plane2) 2)
	(= (fast-burn plane2) 4)
	(= (refuel-rate plane2) 2312)
	(at person1 city0)
	(at person2 city1)
	(at person3 city0)
	(at person4 city0)
	(at person5 city2)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 834)
	(= (distance city0 city2) 743)
	(= (distance city1 city0) 834)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 502)
	(= (distance city2 city0) 743)
	(= (distance city2 city1) 502)
	(= (distance city2 city2) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at plane1 city0)
	(at person2 city2)
	(at person3 city0)
	(at person4 city1)
	(at person5 city2)
	))

(:metric minimize (+ (* 4 (total-time))  (* 0.003 (total-fuel-used))))
)
