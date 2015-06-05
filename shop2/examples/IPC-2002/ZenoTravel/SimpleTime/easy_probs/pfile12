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
	fl0 - flevel
	fl1 - flevel
	fl2 - flevel
	fl3 - flevel
	fl4 - flevel
	fl5 - flevel
	fl6 - flevel
	)
(:init
	(at plane1 city2)
	(fuel-level plane1 fl3)
	(at plane2 city3)
	(fuel-level plane2 fl6)
	(at plane3 city5)
	(fuel-level plane3 fl2)
	(at person1 city4)
	(at person2 city4)
	(at person3 city0)
	(at person4 city4)
	(at person5 city1)
	(at person6 city2)
	(at person7 city5)
	(at person8 city5)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
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

(:metric minimize (total-time))
)
