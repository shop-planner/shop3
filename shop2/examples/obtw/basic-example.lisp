(in-package :shop2-user)
;;; This extremely simple example shows some ideas on how to produce OBTWs

(defdomain basic-example (
  ;; Basic operator example for calling out.			     
  (:operator (!create-unit ?a) 
       ((eval (excl:run-shell-command "ls"))) 
       () 
       ((have ?a)))

   ;; Transportation operators for moving units around.
    (:operator (!embark-truck ?unit ?truck ?loc)
               ((unit-at ?unit ?loc))
               ((in-truck ?unit ?truck)))

    (:operator (!disembark-truck ?unit ?truck ?loc)
               ((in-truck ?unit ?truck))
               ((unit-at ?unit ?loc)))

    (:operator (!embark-airplane ?unit ?airplane ?loc)
               ((unit-at ?unit ?loc)
                (:protection (airplane-at ?airplane ?loc)))
               ((in-airplane ?unit ?airplane)))

    (:operator (!disembark-airplane ?unit ?airplane ?loc)
               ((in-airplane ?obj ?airplane))
               ((unit-at ?unit ?loc)))

    (:operator (!drive-truck ?truck ?loc-from ?loc-to)
               ((truck-at ?truck ?loc-from))
               ((truck-at ?truck ?loc-to)))

    (:operator (!fly-airplane ?airplane ?airport-from ?airport-to)
               ((airplane-at ?airplane ?airport-from))
               ((airplane-at ?airplane ?airport-to)))

    (:operator (!put-unit ?unit ?coordx ?coordy)
	 ((eval (excl:run-shell-command "osascript examples/obtw/putunit.scpt")))
	 () ())

  (:method (conduct-cordon-search ?x ?y) 
       ()
       ((!put-unit unit1 100 100) 
	    (cordon-element) (search-element) (reserve-element)))

  ;; I would ignore this and instead of raising a failure in SHOP2, I'd put an OBTW on the PPT
  ;; slide.
  (:method (reserve-element)
       () ())

  (:method (cordon-element ?target-inner ?target-outer)
       ()
       ((establish-inner-cordon ?target-inner) (establish-outer-cordon ?target-outer)))

  (:method (establish-inner-cordon ?target-loc)
       ((available ?unit) (unit-at ?unit ?loc) (short-distance ?loc ?target-loc) )
       ((move-with-truck ?unit ?loc ?target-loc))

       ((available ?unit) (unit-at ?unit ?loc))
       ((move-with-plane ?unit ?loc ?target-loc)))

  (:method (establish-outer-cordon ?target-loc)
       ((available ?unit) (unit-at ?unit ?loc) (short-distance ?loc ?target-loc))
       ((move-with-truck ?unit ?loc ?target-loc))

       ((available ?unit) (unit-at ?unit ?loc))
       ((move-with-plane ?unit ?loc ?target-loc)))

  (:method (move-with-truck ?unit ?from ?to)
       ()
       ((!embark-truck ?unit ?from ?to) (drive-truck ?truck ?from ?to)
	    (!disembark-truck ?truck ?unit ?to)))

  (:method (move-with-plane ?unit ?from ?to)
       ()
       ((!embark-plane ?unit ?from ?to) (fly-plane ?plane ?from ?to)
	    (!disembark-plane ?plane ?unit ?to)))
       
  (:method (search-element)
       ()
       ((position-search-team) (position-security-team) (position-sof)))

  (:method (position-search-team ?unit)
       () 
       ((!put-unit ?unit ?coordx ?coordy)))

  (:method (position-security-team ?unit)
       () 
       ((!put-unit ?unit ?coordx ?coordy)))

   (:method (position-sof)
	()
	((mwd-team) (site-exploitation)))

  (:method (mwd-team)
       () ())

  (:method (site-exploitation)
       () ())

))

(defproblem problem1 basic-example
  ((have banjo)) 
   (conduct-cordon-search x y))

(find-plans 'problem1 :verbose :plans)
