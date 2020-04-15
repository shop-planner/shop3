(in-package :shop-user)

(defclass metric-domain (fluents-mixin pure-logic-domain-mixin pddl-domain)
  ())

(defdomain (metric-rovers :type metric-domain)
    ((:include rover #. (asdf:system-relative-pathname "shop3" "examples/rovers/metric/num-rovers.pddl"))

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
    ((in ?rover ?to))
    ())

  (:method (navigate ?rover ?to)
    go-there
    ((not (in ?rover ?to))
     (in ?rover ?from)
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
    ((in ?rover ?photo-loc)
     (visible_from ?obj ?photo-loc))
    ())

  (:method (get-line-of-sight ?rover ?obj ?photo-loc)
    need-line-of-sight
    ((in ?rover ?rover-loc)
     (not (visible_from ?obj ?rover-loc))
     (visible_from ?obj ?photo-loc))
    (:ordered (navigate ?rover ?photo-loc)))


  ;; HELPERS
  ;; the following shows a need for some higher-order method constructs

  (:method (communicate soil ?analysis-loc ?rover-loc ?rover)
    have-line-of-sight
    ((in ?rover ?rover-loc)
     (at_lander ?l ?lander-loc)
     (visible ?rover-loc ?lander-loc))
    ((!communicate_soil_data ?rover ?l ?analysis-loc ?rover-loc
                             ?lander-loc)))


  (:method (communicate soil ?analysis-loc ?rover-loc ?rover)
    go-to-line-of-sight
    ;; Otherwise, go somewhere where the lander is visible
    ((in ?rover ?rover-loc)
     (at_lander ?l ?lander-loc)
     (not (visible ?rover-loc ?lander-loc))
     ;; FIXME: should pick a *good* location, instead of any location that has vi
     (visible ?new-loc ?lander-loc))
    ((navigate ?rover ?new-loc)
     (!communicate_soil_data ?rover ?l ?analysis-loc ?new-loc
                             ?lander-loc)))

  (:method (communicate rock ?analysis-loc ?rover-loc ?rover)
    have-line-of-sight
    ((in ?rover ?rover-loc)
     (at_lander ?l ?lander-loc)
     (visible ?rover-loc ?lander-loc))
    ((!communicate_rock_data ?rover ?l ?analysis-loc ?rover-loc
                             ?lander-loc)))

  (:method (communicate rock ?analysis-loc ?rover-loc ?rover)
    go-to-line-of-sight
    ;; Otherwise, go somewhere where the lander is visible
    ((in ?rover ?rover-loc)
     (at_lander ?l ?lander-loc)
     (not (visible ?rover-loc ?lander-loc))
     ;; FIXME: should pick a *good* location, instead of any location that has vi
     (visible ?new-loc ?lander-loc))
    ((navigate ?rover ?new-loc)
     (!communicate_rock_data ?rover ?l ?analysis-loc ?new-loc
                             ?lander-loc)))

  (:method (communicate image ?analysis-loc ?rover-loc ?rover)
    have-line-of-sight
    ((in ?rover ?rover-loc)
     (at_lander ?l ?lander-loc)
     (visible ?rover-loc ?lander-loc))
    ((!communicate_image_data ?rover ?l ?analysis-loc ?rover-loc
                              ?lander-loc)))

  (:method (communicate image ?analysis-loc ?rover-loc ?rover)
    go-to-line-of-sight
    ;; Otherwise, go somewhere where the lander is visible
    ((in ?rover ?rover-loc)
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
    ((in ?rover ?rover-loc)
     (at_lander ?l ?lander-loc)
     (visible ?rover-loc ?lander-loc))
    ((!communicate_image_data ?rover ?l ?obj ?mode ?rover-loc
                              ?lander-loc)))

  (:method (communicate-IMAGE ?rover-loc ?lander-loc
            ?rover ?obj ?mode)
    relocate-then-communicate
    ((in ?rover ?loc)
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
