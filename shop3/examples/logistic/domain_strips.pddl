(define (domain logistics-strips)
  (:requirements :strips) 
  (:predicates 	(OBJ ?obj)
	       	(TRUCK ?truck)
               	(LOCATION ?loc)
		(AIRPLANE ?airplane)
                (CITY ?city)
                (AIRPORT ?airport)
                (airplane-at ?airplane ?loc)
                (truck-at ?truck ?loc)
                (obj-at ?obj ?loc)
                (in-airplane ?obj ?airplane)
                (in-truck ?obj ?truck)
		(in-city ?obj ?city)
                ;;(different ?loc1 ?loc2)
                )
 
  ; (:types )		; default object

(:action LOAD-TRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
   (truck-at ?truck ?loc) (obj-at ?obj ?loc))
  :effect
   (and (not (obj-at ?obj ?loc)) (in-truck ?obj ?truck)))

(:action LOAD-AIRPLANE
  :parameters
   (?obj
    ?airplane
    ?loc)
  :precondition
   (and (OBJ ?obj) (AIRPLANE ?airplane) (LOCATION ?loc)
   (obj-at ?obj ?loc) (airplane-at ?airplane ?loc))
  :effect
   (and (not (obj-at ?obj ?loc)) (in-airplane ?obj ?airplane)))

(:action UNLOAD-TRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
        (truck-at ?truck ?loc) (in-truck ?obj ?truck))
  :effect
   (and (not (in-truck ?obj ?truck)) (obj-at ?obj ?loc)))

(:action UNLOAD-AIRPLANE
  :parameters
   (?obj
    ?airplane
    ?loc)
  :precondition
   (and (OBJ ?obj) (AIRPLANE ?airplane) (LOCATION ?loc)
        (in-airplane ?obj ?airplane) (airplane-at ?airplane ?loc))
  :effect
   (and (not (in-airplane ?obj ?airplane)) (obj-at ?obj ?loc)))

(:action DRIVE-TRUCK
  :parameters
   (?truck
    ?loc-from
    ?loc-to)
  :precondition
   (and (TRUCK ?truck) (LOCATION ?loc-from) (LOCATION ?loc-to) 
        (truck-at ?truck ?loc-from)
        ;(different ?loc-to ?loc-from)
        )
  :effect
   (and (not (truck-at ?truck ?loc-from)) (truck-at ?truck ?loc-to)))

(:action FLY-AIRPLANE
  :parameters
   (?airplane
    ?loc-from
    ?loc-to)
  :precondition
   (and (AIRPLANE ?airplane) (AIRPORT ?loc-from) (AIRPORT ?loc-to)
	(airplane-at ?airplane ?loc-from)
        ;(different ?loc-to ?loc-from)
        )
  :effect
   (and (not (airplane-at ?airplane ?loc-from)) (airplane-at ?airplane ?loc-to)))
)