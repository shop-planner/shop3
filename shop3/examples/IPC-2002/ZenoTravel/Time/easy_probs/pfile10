(define (problem ZTRAVEL-3-8)
(:domain zeno-travel)
(:objects
	plane1 - aircraft
	plane2 - aircraft
	plane3 - aircraft
	person1 - person
	person2 - person
	person3 - person
	person4 - person
	person5 - person
	person6 - person
	person7 - person
	person8 - person
	city0 - city
	city1 - city
	city2 - city
	city3 - city
	city4 - city
	)
(:init
	(at plane1 city0)
	(= (slow-speed plane1) 159)
	(= (fast-speed plane1) 274)
	(= (capacity plane1) 15412)
	(= (fuel plane1) 1458)
	(= (slow-burn plane1) 5)
	(= (fast-burn plane1) 14)
	(= (refuel-rate plane1) 2618)
	(at plane2 city4)
	(= (slow-speed plane2) 142)
	(= (fast-speed plane2) 360)
	(= (capacity plane2) 9628)
	(= (fuel plane2) 2728)
	(= (slow-burn plane2) 4)
	(= (fast-burn plane2) 13)
	(= (refuel-rate plane2) 5700)
	(at plane3 city2)
	(= (slow-speed plane3) 169)
	(= (fast-speed plane3) 362)
	(= (capacity plane3) 9038)
	(= (fuel plane3) 667)
	(= (slow-burn plane3) 4)
	(= (fast-burn plane3) 8)
	(= (refuel-rate plane3) 6138)
	(at person1 city3)
	(at person2 city3)
	(at person3 city4)
	(at person4 city4)
	(at person5 city1)
	(at person6 city0)
	(at person7 city1)
	(at person8 city0)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 808)
	(= (distance city0 city2) 932)
	(= (distance city0 city3) 760)
	(= (distance city0 city4) 697)
	(= (distance city1 city0) 808)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 559)
	(= (distance city1 city3) 673)
	(= (distance city1 city4) 742)
	(= (distance city2 city0) 932)
	(= (distance city2 city1) 559)
	(= (distance city2 city2) 0)
	(= (distance city2 city3) 938)
	(= (distance city2 city4) 978)
	(= (distance city3 city0) 760)
	(= (distance city3 city1) 673)
	(= (distance city3 city2) 938)
	(= (distance city3 city3) 0)
	(= (distance city3 city4) 803)
	(= (distance city4 city0) 697)
	(= (distance city4 city1) 742)
	(= (distance city4 city2) 978)
	(= (distance city4 city3) 803)
	(= (distance city4 city4) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at plane1 city2)
	(at person1 city1)
	(at person2 city2)
	(at person3 city3)
	(at person4 city1)
	(at person5 city0)
	(at person6 city3)
	(at person7 city4)
	(at person8 city3)
	))

(:metric minimize (+ (* 4 (total-time))  (* 0.002 (total-fuel-used))))
)
