(define (problem ZTRAVEL-3-7)
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
	(at plane1 city4)
	(fuel-level plane1 fl4)
	(at plane2 city4)
	(fuel-level plane2 fl3)
	(at plane3 city1)
	(fuel-level plane3 fl5)
	(at person1 city4)
	(at person2 city2)
	(at person3 city2)
	(at person4 city0)
	(at person5 city2)
	(at person6 city2)
	(at person7 city5)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal (and
	(at plane1 city1)
	(at person1 city4)
	(at person2 city1)
	(at person3 city2)
	(at person4 city2)
	(at person5 city2)
	(at person6 city4)
	(at person7 city0)
	))

(:metric minimize (total-time))
)
