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
	city3 - city
	)
(:init
	(at plane1 city2)
	(= (slow-speed plane1) 108)
	(= (fast-speed plane1) 183)
	(= (capacity plane1) 5577)
	(= (fuel plane1) 873)
	(= (slow-burn plane1) 2)
	(= (fast-burn plane1) 6)
	(= (refuel-rate plane1) 1180)
	(at plane2 city1)
	(= (slow-speed plane2) 116)
	(= (fast-speed plane2) 292)
	(= (capacity plane2) 2911)
	(= (fuel plane2) 283)
	(= (slow-burn plane2) 1)
	(= (fast-burn plane2) 3)
	(= (refuel-rate plane2) 1876)
	(at person1 city0)
	(at person2 city0)
	(at person3 city3)
	(at person4 city1)
	(at person5 city2)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 917)
	(= (distance city0 city2) 811)
	(= (distance city0 city3) 641)
	(= (distance city1 city0) 917)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 972)
	(= (distance city1 city3) 757)
	(= (distance city2 city0) 811)
	(= (distance city2 city1) 972)
	(= (distance city2 city2) 0)
	(= (distance city2 city3) 554)
	(= (distance city3 city0) 641)
	(= (distance city3 city1) 757)
	(= (distance city3 city2) 554)
	(= (distance city3 city3) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at person1 city3)
	(at person2 city1)
	(at person3 city3)
	(at person4 city3)
	(at person5 city1)
	))

(:metric minimize (+ (* 1 (total-time))  (* 0.005 (total-fuel-used))))
)
