;;
;;  Satellete Domain:  SimpleTime  (Version 1.0)
;;

;; functions for debugging

(defun print-last-action ()
  (let ((tplan (strip-nops *current-plan*))
        (fplan nil)
        (i 1))
    (do* ((cc (pop tplan) (pop tplan))
          (tt (pop tplan) (pop tplan)))
         ((not cc))
      (when (and (not (eql (car tt) '!!INOP)))
        (push tt fplan))
    )
    (if (> (length fplan) 0)
      (format t "~A: ~A~%" (length fplan) (car (last fplan)))
      (format t "0: no action yet~%")
    )
    t
  )
)

(defun print-config ()
  (let* ((satellite-list (query-current-state 'SATELLITE))
         (instrument-list (query-current-state 'INSTRUMENT))
         (direction-list (query-current-state 'DIRECTION))
         (mode-list (query-current-state 'MODE))
         (on_board-list (query-current-state 'on_board))
         (supports-list (query-current-state 'supports))
         (pointing-list (query-current-state 'pointing))
         (power_avail-list (query-current-state 'power_avail))
         (power_on-list (query-current-state 'power_on))
         (calibrated-list (query-current-state 'calibrated))
         (have_image-list (query-current-state 'have_image))
         (calibration_target-list (query-current-state 'calibration_target))

         satellite
         instrument
         mode)

    (dolist (satellite-atom satellite-list)
      (setf satellite (second satellite-atom))
      (format t "Configuration of ~A:" satellite)
      (format t "~%")
      (dolist (on_board-atom on_board-list)
        (when (eql (third on_board-atom) satellite)
          (setf instrument (second on_board-atom))
          (format t "  ~A:" instrument)
          (dolist (supports-atom supports-list)
            (when (eql (second supports-atom) instrument)
              (setf mode (third supports-atom))
              (format t " ~A" mode)
            )
          )
          (dolist (calibration_target-atom calibration_target-list)
            (when (eql (second calibration_target-atom) instrument)
              (format t " cal:~A" (third calibration_target-atom))
            )
          )
          (format t "~%")
        )
      )
    )
    (format t "~%")
  )
  t
)


(defun print-satellite ()
  (let* ((satellite-list (query-current-state 'SATELLITE))
         (instrument-list (query-current-state 'INSTRUMENT))
         (direction-list (query-current-state 'DIRECTION))
         (mode-list (query-current-state 'MODE))
         (on_board-list (query-current-state 'on_board))
         (supports-list (query-current-state 'supports))
         (pointing-list (query-current-state 'pointing))
         (power_avail-list (query-current-state 'power_avail))
         (power_on-list (query-current-state 'power_on))
         (calibrated-list (query-current-state 'calibrated))
         (have_image-list (query-current-state 'have_image))
         (calibration_target-list (query-current-state 'calibration_target))
         (power-which-list (query-current-state 'power-which))

         (satellite-time-list (query-current-state 'satellite-time))

         satellite
         instrument)

    (format t "--- State of the satellites ---~%")
    (dolist (satellite-atom satellite-list)
      (let* ((satellite (second satellite-atom))
             stime
             pointing
             power-which)

        (dolist (satellite-time-atom satellite-time-list)
          (when (eql (second satellite-time-atom) satellite)
            (setf stime (third satellite-time-atom))))

        (dolist (pointing-atom pointing-list)
          (when (eql (second pointing-atom) satellite)
            (setf pointing (third pointing-atom))))

        (dolist (power-which-atom power-which-list)
          (when (eql (second power-which-atom) satellite)
            (setf power-which (third power-which-atom))))

        (format t "~A ~12T ~A ~20T ~A ~28T ~A ~35T ~A ~50T ~A~%"
          satellite
          (/ (fround (* stime 100.0)) 100)
          "--"
          "--" pointing power-which)
      )
    )
    (format t "~A images taken. ~%" (length have_image-list))
    (format t "~%")
  )
  t
)

(defdomain adlsimpleTimeSat (

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPERATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:operator (!turn_to ?s ?d_new ?d_prev ?time 5)
    ((satellite-time ?s ?time)
     (assign ?new-time (+ ?time 5))
     (pointing ?s ?d_prev)
     (different ?d_new ?d_prev))
    ((pointing ?s ?d_prev)
     (satellite-time ?s ?time))
    ((pointing ?s ?d_new)
     (satellite-time ?s ?new-time)))

  (:operator (!switch_on ?i ?s ?time 2)
    ((satellite-time ?s ?time)
     (assign ?new-time (+ ?time 2))
     (on_board ?i ?s)
     (power_avail ?s))
    ((power_avail ?s)
     (power-which ?s nil)
     (forall nil (calibrated ?i) ((calibrated ?i)))
     (satellite-time ?s ?time))
    ((power_on ?i)
     (power-which ?s ?i)
     (satellite-time ?s ?new-time)))

  (:operator (!switch_off ?i ?s ?time 1)
    ((satellite-time ?s ?time)
     (assign ?new-time (+ ?time 1))
     (on_board ?i ?s)
     (power_on ?i))
    ((power_on ?i)
     (power-which ?s ?i)
     (satellite-time ?s ?time))
    ((power_avail ?s)
     (power-which ?s nil)
     (satellite-time ?s ?new-time)))

  (:operator (!calibrate ?s ?i ?d ?time 5)
    ((satellite-time ?s ?time)
     (assign ?new-time (+ ?time 5))
     (on_board ?i ?s)
     (calibration_target ?i ?d)
     (pointing ?s ?d)
     (power_on ?i))
    ((satellite-time ?s ?time))
    ((calibrated ?i)
     (satellite-time ?s ?new-time)))

  (:operator (!take_image ?s ?d ?i ?m ?time 7)
    ((satellite-time ?s ?time)
     (assign ?new-time (+ ?time 7))
     (calibrated ?i)
     (on_board ?i ?s)
     (supports ?i ?m)
     (power_on ?i)
     (pointing ?s ?d))
    ((satellite-time ?s ?time))
    ((have_image ?d ?m)  ;; no when
     (satellite-time ?s ?new-time)))

  (:operator (!!delay ?s)
    ((satellite-time ?s ?time)
     (assign ?new-time (+ ?time 0.01)))
    ((satellite-time ?s ?time))
    ((satellite-time ?s ?new-time)))

  (:operator (!!create-satellite-time)
    ()
    ()
    ((forall (?s)
             (and (satellite ?s))
             ((satellite-time ?s 0)))
     (satellite-time aa)
    )
  )

  (:operator (!!create-power-which)
    ()
    ()
    ((forall (?s)
             (and (power_avail ?s))
             ((power-which ?s nil)))
     (forall (?s ?i)
             (and (power_on ?i) (on_board ?i ?s))
             ((power-which ?s ?i)))
    )
  )

  (:operator (!!create-node-task)
    ()
    ()
    ((forall (?d ?m ?i ?s)
             (and (wanted-image ?d ?m) (supports ?i ?m) (on_board ?i ?s))
             ((node-task ?d ?m ?i ?s))))
  )

  (:operator (!!retract-node-task ?d ?m)
    ()
    ((forall (?i ?s)
             (and (node-task ?d ?m ?i ?s))
             ((node-task ?d ?m ?i ?s))))
    ()
  )

  (:operator (!!assert ?x) () () (?x) 0)
  (:operator (!!unassert ?x) () (?x) () 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;
  ;;; Main Method ;;;
  ;;;;;;;;;;;;;;;;;;;

  (:method (have_image ?d ?m)
    ()
    (:ordered (!!assert (wanted-image ?d ?m)))
  )
  
  (:method (pointing ?s ?d)
    ()
    (:ordered (!!assert (final-pointing ?s ?d)))
  )
  
  (:method (main)
    ()
    (:ordered ; (println "START >>>")
              (init-current-state)
              ; (print-config)
              ; (println "==== Initial State ===============================")
              ; (print-satellite)
              ; (println "==== Planning ====================================")
              (scheduling)
              ; (println "==== Final State =================================")
              ; (print-satellite)
    )
  )

  ;;
  ;; Initialization
  ;;

  (:method (init-current-state)
    ()
    (:ordered (!!create-satellite-time)
              (!!create-power-which)
    )
  )

  ;;
  ;; scheduling
  ;;


  (:method (scheduling)
    ()
    (:ordered (do-scheduling)))

  ;;
  ;; full-fledged scheduling
  ;;

  (:method (do-scheduling)
    ()
    (:ordered (!!create-node-task)
              (do-scheduling2)
    )
  )

  (:method (do-scheduling2)
    (:first (NODE-TASK ?d ?m ?i ?s))  ;; if more tasks exists
    (:ordered (exe-node-task)
              (do-scheduling2)
    )
    ()  ; otherwise, there is no more tasks
    (:ordered (final-pointing)) ;; may lead to backtracking
  )

  (:method (exe-node-task)
    (:sort-by ?time
      #'<
      (and (satellite-time ?s ?time))
    )
    (:ordered (assign-task ?s)
              (do-scheduling2)
    )
  )

  (:method (assign-task ?s)
    ((power-which ?s ?i)
     (NODE-TASK ?d ?m ?i ?s))
    (:ordered (take-image ?s ?i ?d ?m)
              (!!retract-node-task ?d ?m)
    )
    ((NODE-TASK ?d ?m ?i ?s))
    (:ordered (prepare-instrument ?s ?i)
              (take-image ?s ?i ?d ?m)
              (!!retract-node-task ?d ?m)
    )
  )


  ;;
  ;; low level methods
  ;;

  (:method (take-image ?s ?i ?d ?m)
    ((pointing ?s ?d))
    (:ordered (!take_image ?s ?d ?i ?m ?time ?dur))
    ((pointing ?s ?d_prev))
    (:ordered (!turn_to ?s ?d ?d_prev ?time ?dur)
              (!take_image ?s ?d ?i ?m ?time ?dur))
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
    (:ordered (!!delay ?s) ;; I don't know why I need this
              (!switch_on ?i ?s ?time ?dur))
    ((power_on ?j))  ;; ?i != ?j
    (:ordered (!!delay ?s) ;; I don't know why I need this
              (!switch_off ?j ?s ?time ?dur)
              (!!delay ?s)
              (!switch_on ?i ?s ?time ?dur))
  )

  (:method (calibrate_instrument ?s ?i)  ;; assume power_on
    ((calibrated ?i))  ;; assume after turn_on_instrument
    ()
    ((pointing ?s ?d)
     (calibration_target ?i ?d))
    (:ordered ; (!!delay ?s)   ;;  no delay if calibrate_instrument
                               ;;  always comes after turn_on_instrument
              (!calibrate ?s ?i ?d ?time ?dur))
    ((pointing ?s ?d2)
     (calibration_target ?i ?d))  ;; ?d != ?d2
    (:ordered (!turn_to ?s ?d ?d2 ?time ?dur)
              (!!delay ?s)
              (!calibrate ?s ?i ?d ?time ?dur))
  )

  (:method (final-pointing)
    ((final-pointing ?s ?d)
     (pointing ?s ?d))
    (:ordered (!!unassert (final-pointing ?s ?d))
              (final-pointing))
    ((final-pointing ?s ?d)
     (pointing ?s ?d2)) ;; ?d != ?d2
    (:ordered (!!unassert (final-pointing ?s ?d))
              (!turn_to ?s ?d ?d2 ?time ?dur)
              (final-pointing))
    ()
    ()
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; methods for debugging ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  (:method (print-last-action)
;           ((eval (print-last-action))) ())
;  (:method (print-config)
;           ((eval (print-config))) ())
;  (:method (print-satellite)
;           ((eval (print-satellite))) ())
;
;  (:method (print-current-state)
;           ((eval (print-current-state))) ())
;  (:method (print-current-plan)
;           ((eval (print-current-plan))) ())
;  (:method (print-atom ?x)
;           ((eval (not (format t "~A~%" (query-current-state '?x))))) ())
;
;  (:method (println)
;           ((eval (not (format t "~%")))) ())
;  (:method (println ?s)
;           ((eval (not (format t "~A~%" '?s)))) ())
;  (:method (println ?s1 ?s2)
;           ((eval (not (format t "~A~A~%" '?s1 '?s2)))) ())
;  (:method (println ?s1 ?s2 ?s3)
;           ((eval (not (format t "~A~A~A~%" '?s1 '?s2 '?s3)))) ())
;  (:method (println ?s1 ?s2 ?s3 ?s4)
;           ((eval (not (format t "~A~A~A~A~%" '?s1 '?s2 '?s3 '?s4)))) ())
;  (:method (println ?s1 ?s2 ?s3 ?s4 ?s5)
;           ((eval (not (format t "~A~A~A~A~A~%" '?s1 '?s2 '?s3 '?s4 '?s5)))) ())


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

  ;;; axioms for debugging

;  (:- (println) ((eval (progn (format t "~%") t))))
;  (:- (println ?s) ((eval (progn (format t "~A~%" '?s) t))))
;  (:- (println ?s1 ?s2) ((eval (progn (format t "~A~A~%" '?s1 '?s2) t))))
;  (:- (println ?s1 ?s2 ?s3) ((eval (progn
;        (format t "~A~A~A~%" '?s1 '?s2 '?s3) t))))
;  (:- (println ?s1 ?s2 ?s3 ?s4) ((eval (progn
;        (format t "~A~A~A~A~%" '?s1 '?s2 '?s3 '?s4) t)))) 
;  (:- (println ?s1 ?s2 ?s3 ?s4 ?s5) ((eval (progn
;        (format t "~A~A~A~A~A~%" '?s1 '?s2 '?s3 '?s4 '?s5) t)))) 
;  (:- (println ?s1 ?s2 ?s3 ?s4 ?s5 ?s6) ((eval (progn
;        (format t "~A~A~A~A~A~A~%" '?s1 '?s2 '?s3 '?s4 '?s5 '?s6) t)))) 


;;;;;;;;;;;;;;;
;;; THE END ;;;
;;;;;;;;;;;;;;;

))

