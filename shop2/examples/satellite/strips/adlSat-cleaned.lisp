(in-package :shop2-user)
;;
;;  Satellete Domain:  STRIPS  (Version 1.0)
;;

(defdomain (adlSat :type pddl-domain)
    (
     (:requirements :equality :typing :conditional-effects :negative-preconditions)
     (:types satellite direction instrument mode)
     
     (:action turn_to
      :parameters (?s - satellite ?d_new - direction ?d_prev - direction)
      :precondition (and (pointing ?s ?d_prev)
                         (not (= ?d_new ?d_prev))
                         )
      :effect (and  (pointing ?s ?d_new)
                    (not (pointing ?s ?d_prev))
                    )
              )
     
     (:action switch_on
      :parameters (?i - instrument ?s - satellite)
              
      :precondition (and (on_board ?i ?s) 
                         (power_avail ?s)
                         )
      :effect (and (power_on ?i)
                   (when (calibrated ?i) (not (calibrated ?i)))
                   (not (power_avail ?s))
                   )
              
              )

     
     (:action switch_off
      :parameters (?i - instrument ?s - satellite)
              
      :precondition (and (on_board ?i ?s)
                         (power_on ?i) 
                         )
      :effect (and (not (power_on ?i))
                   (power_avail ?s)
                   )
              )

     (:action calibrate
      :parameters (?s - satellite ?i - instrument ?d - direction)
      :precondition (and (on_board ?i ?s)
                         (calibration_target ?i ?d)
                         (pointing ?s ?d)
                         (power_on ?i)
                         )
      :effect (calibrated ?i)
              )


     (:action take_image
      :parameters (?s - satellite ?d - direction ?i - instrument ?m - mode)
      :precondition (and (calibrated ?i)
                         (on_board ?i ?s)
                         (supports ?i ?m)
                         (power_on ?i)
                         (pointing ?s ?d)
                         (power_on ?i)
                         )
      :effect (when (not (have_image ?d ?m)) (have_image ?d ?m))
              )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;
  ;;; Main Method ;;;
  ;;;;;;;;;;;;;;;;;;;

     (:method (have_image ?d ?m)
       ((on_board ?i ?s))
       (:ordered
        (prepare-instrument ?s ?i)
        (take-image ?s ?i ?d ?m)))
     

     (:method (take-image ?s ?i ?d ?m)
       ((pointing ?s ?d))
       (:ordered (!take_image ?s ?d ?i ?m))
       ((pointing ?s ?d_prev) (different ?d_prev ?d))
       (:ordered (!turn_to ?s ?d ?d_prev)
                 (!take_image ?s ?d ?i ?m))
       )

     ;; prepare-instrument = power_on + calibrated
     (:method (prepare-instrument ?s ?i)
       ()
       (:ordered (turn_on_instrument ?s ?i)
                 (calibrate_instrument ?s ?i))
       )

     (:method (turn_on_instrument ?s ?i)
       ((power_on ?i))
       ()
       ((power_avail ?s))
       (:ordered (!switch_on ?i ?s))
       ((power_on ?j))  ;; ?i != ?j
       (:ordered (!switch_off ?j ?s)
                 (!switch_on ?i ?s))
       )

     (:method (calibrate_instrument ?s ?i)  ;; assume power_on
       ((calibrated ?i))  ;; assume after turn_on_instrument
       ()
       ((pointing ?s ?d)
        (calibration_target ?i ?d))
       (:ordered (!calibrate ?s ?i ?d))
       ((pointing ?s ?d2) 
        (calibration_target ?i ?d)
        )  ;; ?d != ?d2
       (:ordered (!turn_to ?s ?d ?d2)
                 (!calibrate ?s ?i ?d)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AXIOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; basic axioms

     (:- (same ?x ?x) nil)
     (:- (different ?x ?y) ((not (same ?x ?y))))
     (:- (member ?a ?L) ((eval (member '?a '?L))))
     (:- (not-member ?a ?L) ((not (eval (member '?a '?L)))))
     (:- (not-member-equal ?a ?L) ((not (eval (member '?a '?L :test #'equal)))))
     (:- (fail) ((eval nil)))

     ))

