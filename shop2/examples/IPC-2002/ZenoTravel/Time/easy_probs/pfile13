(define (problem ZTRAVEL-3-10)
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
	person9 - person
	person10 - person
	city0 - city
	city1 - city
	city2 - city
	city3 - city
	city4 - city
	city5 - city
	)
(:init
	(at plane1 city4)
	(= (slow-speed plane1) 100)
	(= (fast-speed plane1) 122)
	(= (capacity plane1) 2326)
	(= (fuel plane1) 205)
	(= (slow-burn plane1) 1)
	(= (fast-burn plane1) 2)
	(= (refuel-rate plane1) 1496)
	(at plane2 city3)
	(= (slow-speed plane2) 117)
	(= (fast-speed plane2) 188)
	(= (capacity plane2) 12132)
	(= (fuel plane2) 1469)
	(= (slow-burn plane2) 4)
	(= (fast-burn plane2) 9)
	(= (refuel-rate plane2) 1004)
	(at plane3 city3)
	(= (slow-speed plane3) 195)
	(= (fast-speed plane3) 212)
	(= (capacity plane3) 5204)
	(= (fuel plane3) 1532)
	(= (slow-burn plane3) 2)
	(= (fast-burn plane3) 7)
	(= (refuel-rate plane3) 814)
	(at person1 city1)
	(at person2 city2)
	(at person3 city1)
	(at person4 city4)
	(at person5 city5)
	(at person6 city1)
	(at person7 city0)
	(at person8 city2)
	(at person9 city1)
	(at person10 city5)
	(= (distance city0 city0) 0)
	(= (distance city0 city1) 619)
	(= (distance city0 city2) 565)
	(= (distance city0 city3) 886)
	(= (distance city0 city4) 596)
	(= (distance city0 city5) 766)
	(= (distance city1 city0) 619)
	(= (distance city1 city1) 0)
	(= (distance city1 city2) 561)
	(= (distance city1 city3) 756)
	(= (distance city1 city4) 760)
	(= (distance city1 city5) 980)
	(= (distance city2 city0) 565)
	(= (distance city2 city1) 561)
	(= (distance city2 city2) 0)
	(= (distance city2 city3) 657)
	(= (distance city2 city4) 702)
	(= (distance city2 city5) 639)
	(= (distance city3 city0) 886)
	(= (distance city3 city1) 756)
	(= (distance city3 city2) 657)
	(= (distance city3 city3) 0)
	(= (distance city3 city4) 546)
	(= (distance city3 city5) 510)
	(= (distance city4 city0) 596)
	(= (distance city4 city1) 760)
	(= (distance city4 city2) 702)
	(= (distance city4 city3) 546)
	(= (distance city4 city4) 0)
	(= (distance city4 city5) 850)
	(= (distance city5 city0) 766)
	(= (distance city5 city1) 980)
	(= (distance city5 city2) 639)
	(= (distance city5 city3) 510)
	(= (distance city5 city4) 850)
	(= (distance city5 city5) 0)
	(= (total-fuel-used) 0)
	(= (boarding-time) 0.3)
	(= (debarking-time) 0.6)
)
(:goal (and
	(at plane1 city4)
	(at person1 city4)
	(at person2 city5)
	(at person3 city4)
	(at person4 city0)
	(at person5 city2)
	(at person6 city3)
	(at person8 city0)
	(at person9 city3)
	(at person10 city4)
	))

(:metric minimize (+ (* 1 (total-time))  (* 0.003 (total-fuel-used))))
)
