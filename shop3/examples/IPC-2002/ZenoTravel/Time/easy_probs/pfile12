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
	city5 - city
	)
(:init
	(at plane1 city2)
	(= (slow-speed plane1) 101)
	(= (fast-speed plane1) 263)
	(= (capacity plane1) 6193)
	(= (fuel plane1) 255)
	(= (slow-burn plane1) 2)
	(= (fast-burn plane1) 7)
	(= (refuel-rate plane1) 1540)
	(at plane2 city3)
	(= (slow-speed plane2) 148)
	(= (fast-speed plane2) 204)
	(= (capacity plane2) 4406)
	(= (fuel plane2) 1966)
	(= (slow-burn plane2) 2)
	(= (fast-burn plane2) 5)
	(= (refuel-rate plane2) 1042)
	(at plane3 city5)
	(= (slow-speed plane3) 187)
	(= (fast-speed plane3) 211)
	(= (capacity plane3) 2938)
	(= (fuel plane3) 569)
	(= (slow-burn plane3) 1)
	(= (fast-burn plane3) 2)
	(= (refuel-rate plane3) 1092)
	(at person1 city4)
	(at person2 city4)
	(at person3 city0)
	(at person4 city4)
	(at person5 city1)
	(at person6 city2)
	(at person7 city5)
	(at person8 city5)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 861)
	(= (distance city0 city2) 851)
	(= (distance city0 city3) 738)
	(= (distance city0 city4) 578)
	(= (distance city0 city5) 659)
	(= (distance city1 city0) 861)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 935)
	(= (distance city1 city3) 851)
	(= (distance city1 city4) 752)
	(= (distance city1 city5) 903)
	(= (distance city2 city0) 851)
	(= (distance city2 city1) 935)
	(= (distance city2 city2) 0)
	(= (distance city2 city3) 954)
	(= (distance city2 city4) 777)
	(= (distance city2 city5) 850)
	(= (distance city3 city0) 738)
	(= (distance city3 city1) 851)
	(= (distance city3 city2) 954)
	(= (distance city3 city3) 0)
	(= (distance city3 city4) 596)
	(= (distance city3 city5) 738)
	(= (distance city4 city0) 578)
	(= (distance city4 city1) 752)
	(= (distance city4 city2) 777)
	(= (distance city4 city3) 596)
	(= (distance city4 city4) 0)
	(= (distance city4 city5) 853)
	(= (distance city5 city0) 659)
	(= (distance city5 city1) 903)
	(= (distance city5 city2) 850)
	(= (distance city5 city3) 738)
	(= (distance city5 city4) 853)
	(= (distance city5 city5) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at person1 city2)
	(at person2 city1)
	(at person3 city1)
	(at person4 city4)
	(at person5 city4)
	(at person6 city1)
	(at person7 city3)
	(at person8 city4)
	))

(:metric minimize (+ (* 3 (total-time))  (* 0.003 (total-fuel-used))))
)
