(defpackage :shop3-rovers
  (:use common-lisp shop3)
  (:nicknames #:shop2-rovers #:shop-rovers)
  (:intern
   #:communicated_image_data
   #:communicated_rock_data
   #:communicated_soil_data

   ;; rewrite for goals
   #:communicate_image_data
   #:communicate_rock_data
   #:communicate_soil_data))
(in-package :shop3-rovers)

(defclass pure-pddl-domain (pure-logic-domain-mixin pddl-domain)
  ())

(defdomain (rover :type pure-pddl-domain)
    (
     (:requirements :typing)
     (:static
      can_traverse
      equipped_for_soil_analysis
      equipped_for_rock_analysis
      equipped_for_imaging
      supports
      visible
      visible_from
      store_of
      on_board)
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
      :parameters (?r - rover ?l - lander
                      ;; the location from which ?r took the soil data
                      ?p - waypoint
                      ;; the location of the rover
                      ?x - waypoint
                      ;; the location of the lander
                      ?y - waypoint)
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
      :parameters (?r - rover ?l - lander ?o - objective ?m - mode
                      ;; rover position
                      ?x - waypoint
                      ;; lander location
                      ?y - waypoint)
      :precondition (and (at ?r ?x)(at_lander ?l ?y)(have_image ?r ?o ?m)(visible ?x ?y)(available ?r)(channel_free ?l))
      :effect (and (not (available ?r))
                   (not (channel_free ?l))
                   (channel_free ?l)
                   (communicated_image_data ?o ?m)
                   (available ?r)
                   )
              )

     ;; three imperatives that are used as in-memory representation of goals:
     ;; (COMMUNICATE_SOIL_DATA ?GOAL-LOC)
     ;; (COMMUNICATE_ROCK_DATA ?GOAL-LOC)
     ;; (COMMUNICATE_IMAGE_DATA ?OBJ ?MODE)

     ;; TOP LEVEL TASK:
     (:pddl-method (achieve-goals)
       (communicate_soil_data ?goal-loc)
       (:ordered
        (communicated_soil_data ?goal-loc ?_rover)
        (achieve-goals)))

     (:pddl-method (achieve-goals)
       (communicate_rock_data ?goal-loc)
       (:ordered
        (communicated_rock_data ?goal-loc ?_rover)
        (achieve-goals)))

     (:method (achieve-goals)
       (communicate_image_data ?obj ?mode)
       (:ordered
        (communicated_image_data ?obj ?mode ?_rover)
        (achieve-goals)))

     (:pddl-method (achieve-goals)
        (and (forall (?goal-loc - waypoint) (not (communicate_soil_data ?goal-loc)))
             (forall (?goal-loc - waypoint)(not (communicate_rock_data ?goal-loc)))
             (forall (?obj - objective)
                     (forall (?m - mode)
                             (not (communicate_image_data ?obj ?m)))))
         ())

     (:method (empty-store ?s ?_rover)
       already-empty
       ((empty ?s))
       ())
       
     (:method (empty-store ?s ?rover)
       drop-to-empty
       ((not (empty ?s)))
       ((!drop ?rover ?s)))
     
     (:method (navigate ?rover ?to)
       already-there
       ((at ?rover ?to))
       ())

     (:method (navigate ?rover ?to)
       go-there
       ((not (at ?rover ?to))
        (at ?rover ?from)
        (assign ?visited nil)
        (path ?rover ?from ?to ?path ?visited))
       ((move ?rover ?from ?path)))

     ;; this just traverses over the computed PATH
     (:method (move ?_rover ?_from nil)
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
        ;; FIXME: shouldn't there be a protection of the store until the communication is done?
        (:immediate communicate soil ?goal-loc ?_rover-loc ?rover)
        (:immediate !!retract ((COMMUNICATE_SOIL_DATA ?goal-loc)))))

     (:method (communicated_rock_data ?goal-loc ?rover)
       ((store_of ?s ?rover))
       ((navigate ?rover ?goal-loc) 
        (:immediate empty-store ?s ?rover) 
        (:immediate !sample_rock ?rover ?s ?goal-loc)
        (:immediate communicate ROCK ?goal-loc ?_rover-loc ?rover)
        (:immediate !!retract ((COMMUNICATE_ROCK_DATA ?goal-loc)))))

     (:method (communicated_image_data ?obj ?mode ?rover)
       ((on_board ?camera ?rover)
        (supports ?camera ?mode)
        (at_lander ?_lander ?lander-loc))
       ((calibrate-camera ?rover ?camera)
        (get-line-of-sight ?rover ?obj ?photo-loc)
        (!take_image ?rover ?photo-loc ?obj ?camera ?mode)
        ;; navigate to a transmission location and transmit
        (communicate-image ?photo-loc ?lander-loc ?rover ?obj ?mode)
        (:immediate !!retract ((COMMUNICATE_IMAGE_DATA ?obj ?mode)))))

     (:method (calibrate-camera ?rover ?camera)
       ((calibrated ?camera ?rover))
       ())

     (:method (calibrate-camera ?rover ?camera)
       ((not (calibrated ?camera ?rover))
        (calibration_target ?camera ?calibration-obj)
        (visible_from ?calibration-obj ?calibration-loc))
       (:ordered (navigate ?rover ?calibration-loc)
                 (!calibrate ?rover ?camera ?calibration-obj ?calibration-loc)))

     (:method (get-line-of-sight ?rover ?obj ?photo-loc)
       have-line-of-sight
       ((at ?rover ?photo-loc)
        (visible_from ?obj ?photo-loc))
       ())

     (:method (get-line-of-sight ?rover ?obj ?photo-loc)
       need-line-of-sight
       ((at ?rover ?rover-loc)
        (not (visible_from ?obj ?rover-loc))
        (visible_from ?obj ?photo-loc))
       (:ordered (navigate ?rover ?photo-loc)))


     ;; HELPERS
     ;; the following shows a need for some higher-order method constructs

     (:method (communicate soil ?analysis-loc ?rover-loc ?rover)
            have-line-of-sight
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_soil_data ?rover ?l ?analysis-loc ?rover-loc
                                ?lander-loc)))


     (:method (communicate soil ?analysis-loc ?rover-loc ?rover)
       go-to-line-of-sight
       ;; Otherwise, go somewhere where the lander is visible
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (not (visible ?rover-loc ?lander-loc))
        ;; FIXME: should pick a *good* location, instead of any location that has vi
        (visible ?new-loc ?lander-loc))
       ((navigate ?rover ?new-loc)
        (!communicate_soil_data ?rover ?l ?analysis-loc ?new-loc
                                ?lander-loc)))

     (:method (communicate rock ?analysis-loc ?rover-loc ?rover)
       have-line-of-sight
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_rock_data ?rover ?l ?analysis-loc ?rover-loc
                                ?lander-loc)))

     (:method (communicate rock ?analysis-loc ?rover-loc ?rover)
       go-to-line-of-sight
       ;; Otherwise, go somewhere where the lander is visible
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (not (visible ?rover-loc ?lander-loc))
        ;; FIXME: should pick a *good* location, instead of any location that has vi
        (visible ?new-loc ?lander-loc))
       ((navigate ?rover ?new-loc)
        (!communicate_rock_data ?rover ?l ?analysis-loc ?new-loc
                                ?lander-loc)))

     (:method (communicate image ?analysis-loc ?rover-loc ?rover)
       have-line-of-sight
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_image_data ?rover ?l ?analysis-loc ?rover-loc
                                ?lander-loc)))

     (:method (communicate image ?analysis-loc ?rover-loc ?rover)
       go-to-line-of-sight
       ;; Otherwise, go somewhere where the lander is visible
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (not (visible ?rover-loc ?lander-loc))
        ;; FIXME: should pick a *good* location, instead of any location that has vi
        (visible ?new-loc ?lander-loc))
       ((navigate ?rover ?new-loc)
        (!communicate_image_data ?rover ?l ?analysis-loc ?new-loc
                                ?lander-loc)))

     ;; end of helpers



     (:method (communicate-IMAGE ?rover-loc ?lander-loc
               ?rover ?obj ?mode)
       communicate
       ((at ?rover ?rover-loc)
        (at_lander ?l ?lander-loc)
        (visible ?rover-loc ?lander-loc))
       ((!communicate_image_data ?rover ?l ?obj ?mode ?rover-loc
                                 ?lander-loc)))

     (:method (communicate-IMAGE ?rover-loc ?lander-loc
               ?rover ?obj ?mode)
       relocate-then-communicate
       ((at ?rover ?loc)
        (at_lander ?l ?lander-loc)
        (not (visible ?rover-loc ?lander-loc))
        (visible ?new-loc ?lander-loc)
        (different ?loc ?new-loc))

       ((navigate ?rover ?new-loc) 
        (!communicate_image_data ?rover ?l ?obj ?mode ?new-loc
                                 ?lander-loc)))

     (:op (!!retract ?g)
          :delete ?g)

     ;; State axioms
     (:- (same ?x ?x) nil)
     (:- (different ?x ?y) ((not (same ?x ?y))))
     
     
     ;; This is a simple implementation that looks for an existence of a
     ;; path, not necessarily a shortest or best path.
     (:- (path ?_rover ?from ?from nil ?_visited)
         nil)
     
     (:- (path ?rover ?from ?to (?to . nil) ?_visited)
         ((not (same ?from ?to))
          (can_traverse ?rover ?from ?to)))
     
     (:- (path ?rover ?from ?to (?to1 . ?path1) ?visited)
         ((not (same ?from ?to))
          (not (can_traverse ?rover ?from ?to))
          (can_traverse ?rover ?from ?to1)
          (not (eval (member '?to1 '?visited)))
          (path ?rover ?to1 ?to ?path1 (?from . ?visited))))
     ))
