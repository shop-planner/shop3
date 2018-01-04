(defpackage shop2-rovers
  (:use common-lisp shop2))
(in-package :shop2-rovers)

(defdomain (rover :type pddl-domain)
    (
     (:requirements :typing)
     (:types rover waypoint store camera mode lander objective)
     (:predicates (at ?x - rover ?y - waypoint) 
                  (at_lander ?x - lander ?y - waypoint)
                  (can_traverse ?r - rover ?x - waypoint ?y - waypoint)
                  (equipped_for_soil_analysis ?r - rover)
                  (equipped_for_rock_analysis ?r - rover)
                  (equipped_for_imaging ?r - rover)
                  (empty ?s - store)
                  (have_rock_analysis ?r - rover ?w - waypoint)
                  (have_soil_analysis ?r - rover ?w - waypoint)
                  (full ?s - store)
                  (calibrated ?c - camera ?r - rover) 
                  (supports ?c - camera ?m - mode)
                  (available ?r - rover)
                  (visible ?w - waypoint ?p - waypoint)
                  (have_image ?r - rover ?o - objective ?m - mode)
                  (communicated_soil_data ?w - waypoint)
                  (communicated_rock_data ?w - waypoint)
                  (communicated_image_data ?o - objective ?m - mode)
                  (at_soil_sample ?w - waypoint)
                  (at_rock_sample ?w - waypoint)
                  (visible_from ?o - objective ?w - waypoint)
                  (store_of ?s - store ?r - rover)
                  (calibration_target ?i - camera ?o - objective)
                  (on_board ?i - camera ?r - rover)
                  (channel_free ?l - lander)
                  )
     
     (:action navigate
      :parameters (?x - rover ?y - waypoint ?z - waypoint) 
      :precondition (and (can_traverse ?x ?y ?z) (available ?x) (at ?x ?y) 
                         (visible ?y ?z)
                         )
      :effect (and (not (at ?x ?y)) (at ?x ?z)
                   )
              )
     
     (:action sample_soil
      :parameters (?x - rover ?s - store ?p - waypoint)
      :precondition (and (at ?x ?p) (at_soil_sample ?p)
                         (equipped_for_soil_analysis ?x) (store_of ?s ?x) (empty ?s)
                         )
      :effect (and (not (empty ?s)) (full ?s) (have_soil_analysis ?x ?p) (not (at_soil_sample ?p))
                   )
              )
     
     (:action sample_rock
      :parameters (?x - rover ?s - store ?p - waypoint)
      :precondition (and (at ?x ?p) (at_rock_sample ?p) (equipped_for_rock_analysis ?x) (store_of ?s ?x)(empty ?s)
                         )
      :effect (and (not (empty ?s)) (full ?s) (have_rock_analysis ?x ?p) (not (at_rock_sample ?p))
                   )
              )
     
     (:action drop
      :parameters (?x - rover ?y - store)
      :precondition (and (store_of ?y ?x) (full ?y)
                         )
      :effect (and (not (full ?y)) (empty ?y)
                   )
              )
     
     (:action calibrate
      :parameters (?r - rover ?i - camera ?t - objective ?w - waypoint)
      :precondition (and (equipped_for_imaging ?r) (calibration_target ?i ?t) (at ?r ?w) (visible_from ?t ?w)(on_board ?i ?r)
                         )
      :effect (calibrated ?i ?r) 
              )
     
     (:action take_image
      :parameters (?r - rover ?p - waypoint ?o - objective ?i - camera ?m - mode)
      :precondition (and (calibrated ?i ?r)
                         (on_board ?i ?r)
                         (equipped_for_imaging ?r)
                         (supports ?i ?m)
                         (visible_from ?o ?p)
                         (at ?r ?p)
                         )
      :effect (and (have_image ?r ?o ?m)(not (calibrated ?i ?r))
                   )
              )
     
     (:action communicate_soil_data
      :parameters (?r - rover ?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
      :precondition (and (at ?r ?x)
                         (at_lander ?l ?y)
                         (have_soil_analysis ?r ?p) 
                         (visible ?x ?y)
                         (available ?r)
                         (channel_free ?l)
                         )
      :effect (and (not (available ?r))
                   (not (channel_free ?l))
                   (channel_free ?l)
                   (communicated_soil_data ?p)
                   (available ?r)
                   )
              )
     
     (:action communicate_rock_data
      :parameters (?r - rover ?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
      :precondition (and (at ?r ?x)(at_lander ?l ?y)(have_rock_analysis ?r ?p)
                         (visible ?x ?y)(available ?r)(channel_free ?l)
                         )
      :effect (and (not (available ?r))(not (channel_free ?l))(channel_free ?l)(communicated_rock_data ?p)(available ?r)
                   )
              )
     
     (:action communicate_image_data
      :parameters (?r - rover ?l - lander ?o - objective ?m - mode ?x - waypoint ?y - waypoint)
      :precondition (and (at ?r ?x)(at_lander ?l ?y)(have_image ?r ?o ?m)(visible ?x ?y)(available ?r)(channel_free ?l)
                         )
      :effect (and (not (available ?r))(not (channel_free ?l))(channel_free ?l)(communicated_image_data ?o ?m)(available ?r)
                   )
              )

     ;; I am writing these methods after the OPENSTACKS ones:
     (:method (assert-goals nil)
       ()
       ())

     (:method (assert-goals (?goal . ?goals))
       ()
       (:ordered (!!assert (goal ?goal))
                 (assert-goals ?goals))
       )

     (:method (plan)
       ((:goal (and . ?goals)))
       ((:ordered (assert-goals ?goals)
                  (plan-for-goals))))
     
     (:method (plan-for-goals)
       ((goal (communicated_soil_data ?goal-loc)))
       ((communicated_soil_data ?goal-loc ?rover)
        (plan-for-goals))

       ((goal (communicated_rock_data ?goal-loc)))
       ((communicated_soil_data ?goal-loc ?rover)
        (plan-for-goals))

       ((goal (communicated_image_data ?obj ?mode)))
       ((communicated_soil_data ?obj ?mode ?rover)
        (plan-for-goals))

       termination-case
       ()
       ())

     (:method (empty-store ?s ?rover)
       Case1
       ((empty ?s))
       ()
       
       Case2
       ()
       ((!drop ?rover ?s ?start 1)))
     
     (:method (navigate ?rover ?to)
       Case1
       ((at ?rover ?to))
       ()
       Case2
       ((at ?rover ?from) (path ?rover ?from ?to ?path))
       ((move ?rover ?from ?path)))

     ;; this just traverses over the computed PATH
     (:method (move ?rover ?from nil)
       ()
       ())

     (:method (move ?rover ?from (?first . ?rest))
       ()
       ((!navigate ?rover ?from ?first)
        (move ?rover ?first ?rest)))

     (:method (communicated_soil_data ?goal-loc ?rover)
       ((store_of ?s ?rover))
       ((navigate ?rover ?goal-loc)
        (:immediate empty-store ?s ?rover) 
        (:immediate !sample_soil ?rover ?s ?goal-loc)
        (communicate SOIL ?analysis-loc ?rover-loc ?lander-loc ?rover))
       (:immediate !!retract ((goal (COMMUNICATED_SOIL_DATA ?goal-loc)))))

     (:method (communicate SOIL ?analysis-loc ?second ?goal-loc ?rover)
       ((at ?rover ?rover-loc) (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_soil_data ?rover ?l ?analysis-loc ?rover-loc
                                ?lander-loc))
       ;; Otherwise, go somewhere where the lander is visible
       ((at ?rover ?loc) (at_lander ?l ?lander-loc)
        (visible ?new-loc ?lander-loc)
        (different ?loc ?new-loc))

       ((navigate ?rover ?new-loc) 
        (!communicate_soil_data ?rover ?l ?analysis-loc ?new-loc
                                ?lander-loc)))

     (:method (communicated_rock_data ?goal-loc ?rover)
       ((store_of ?s ?rover))
       ((navigate ?rover ?goal-loc) 
        (:immediate empty-store ?s ?rover) 
        (:immediate !sample_rock ?rover ?s ?goal-loc ?start 8)
        (:immediate !!retract ((goal (COMMUNICATED_ROCK_DATA ?goal-loc))))
        (communicate ROCK ?analysis-loc ?rover-loc ?lander-loc
                     ?rover)))

     (:method (communicate ROCK ?analysis-loc ?second ?goal-loc ?rover)
       ((at ?rover ?rover-loc) (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_soil_data ?rover ?l ?analysis-loc ?rover-loc
                                ?lander-loc))
       ;; Otherwise, go somewhere where the lander is visible
       ((at ?rover ?loc) (at_lander ?l ?lander-loc)
        (visible ?new-loc ?lander-loc)
        (different ?loc ?new-loc))
       
       ((navigate ?rover ?new-loc) 
        (!communicate_soil_data ?rover ?l ?analysis-loc ?new-loc
                                ?lander-loc)))
     

     (:method (communicated_image_data ?obj ?mode ?rover)
       ((on_board ?camera ?rover)
        (supports ?camera ?mode)
        (calibrated ?camera ?rover))
       
       ((!take_image ?rover ?goal-loc ?obj ?camera ?mode)
        (!!retract ((goal (COMMUNICATED_IMAGE_DATA ?obj ?mode))))
        (communicate IMAGE ?rover-loc ?lander-loc
                     ?rover ?obj ?mode))


       ;; First calibrate:
       ((on_board ?camera ?rover)
        (supports ?camera ?mode)
        (calibration_target ?camera ?t-obj))
       
       ((!calibrate ?rover ?camera ?t-obj ?goal-loc)
        (!take_image ?rover ?goal-loc ?obj ?camera ?mode)
        (!!retract ((goal (COMMUNICATED_IMAGE_DATA ?obj ?mode))))
        (communicate IMAGE ?rover-loc ?lander-loc
                     ?rover ?obj ?mode)))


     (:method (communicate IMAGE ?rover-loc ?lander-loc
               ?rover ?obj ?mode)
       ((at ?rover ?rover-loc) (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_image_data ?rover ?l ?obj ?mode ?rover-loc
                                 ?lander-loc))
       ;; Otherwise, go somewhere where the lander is visible
       ((at ?rover ?loc) (at_lander ?l ?lander-loc)
        (visible ?new-loc ?lander-loc)
        (different ?loc ?new-loc))

       ((navigate ?rover ?new-loc) 
        (!communicate_image_data ?rover ?l ?obj ?mode ?new-loc
                                 ?lander-loc)))



     ;; State axioms
     (:- (same ?x ?x) nil)
     (:- (different ?x ?y) ((not (same ?x ?y))))
     
     
     ;; This is a simple implementation that looks for an existence of a
     ;; path, not necessarily a shortest or best path.
     (:- (path ?rover ?from ?from nil)
         nil)
     
     (:- (path ?rover ?from ?to (?to . nil))
         ((can_traverse ?rover ?from ?to)))
     
     (:- (path ?rover ?from ?to (?path . ?path1))
         ((can_traverse ?rover ?from ?to1)
          (path ?rover ?to1 ?to ?path1)))
     ))
