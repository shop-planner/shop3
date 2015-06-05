;;
;;  Rover Domain:  SimpleTime 
;;

(defdomain rover
 (
 (:operator (!navigate ?rover ?y ?z ?start ?duration)
  (and (can_traverse ?rover ?y ?z) (available ?rover) (at ?rover ?y) (visible ?y ?z)
       (max ?rover ?max) (assign ?start (+ ?max 0.01)) (assign ?end (+ ?start ?duration)) (timestamp ?rover ?old))
  ((at ?rover ?y) 
   (forall (?camera  ?oldval) ((on_board ?camera ?rover) (timestamp ?camera ?oldval)) ((timestamp ?camera ?oldval))) 
   (forall (?store ?oldval) ((store_of ?store ?rover) (timestamp ?store ?oldval)) ((timestamp ?store ?oldval))) 
   (max ?rover ?max)
   (timestamp ?rover ?old)
  )
 ((at ?rover ?z)
   (forall (?camera) ((on_board ?camera ?rover)) ((timestamp ?camera ?end))) 
   (forall (?store) ((store_of ?store ?rover)) ((timestamp ?store ?end))) 
   (max ?rover ?end)
   (timestamp ?rover ?end)
 )
 )
 
 (:operator (!sample_soil ?x ?s ?p ?start ?duration)
   (and (at ?x ?p) (at_soil_sample ?p) (equipped_for_soil_analysis ?x)
        (store_of ?s ?x) (empty ?s) 
        (timestamp ?s ?time) (max ?x ?max)
        (assign ?start (+ ?time 0.01))
        (assign ?end (+ ?start ?duration))
        (assign ?newmax (if (> ?end ?max) ?end ?max)) 
   )                
   ((empty ?s) (at_soil_sample ?p) (timestamp ?s ?time) (max ?x ?max))
   ((full ?s) (have_soil_analysis ?x ?p ?end) (timestamp ?s ?end) (max ?x ?newmax))
 )
 
 (:operator (!sample_rock ?x ?s ?p ?start ?duration)
   (and (at ?x ?p) (at_rock_sample ?p) (equipped_for_rock_analysis ?x)
        (store_of ?s ?x) (empty ?s)
        (timestamp ?s ?time) (max ?x ?max)
        (assign ?start (+ ?time 0.01))
        (assign ?end (+ ?start ?duration))
        (assign ?newmax (if (> ?end ?max) ?end ?max)) 
   )                

   ((empty ?s) (at_rock_sample ?p) (timestamp ?s ?time) (max ?x ?max))
   ((full ?s) (have_rock_analysis ?x ?p ?end) (timestamp ?s ?end) (max ?x ?newmax))
)
(:operator (!drop ?x ?y ?start ?duration)
  (and  (store_of ?y ?x) (full ?y)
        (timestamp ?y ?time) (max ?x ?max)
        (assign ?start (+ ?time 0.01))
        (assign ?end (+ ?start ?duration))
        (assign ?newmax (if (> ?end ?max) ?end ?max)) 
   ) 
   ((full ?y) (timestamp ?y ?time) (max ?x ?max))
   ((empty ?y) (timestamp ?y ?end) (max ?x ?newmax)))

(:operator (!calibrate ?r ?i ?t ?w ?start ?duration)
   (and (equipped_for_imaging ?r) (calibration_target ?i ?t) (at ?r ?w)
        (visible_from ?t ?w) (on_board ?i ?r) 
        (timestamp ?i ?time) (max ?r ?max)
        (assign ?start (+ ?time 0.01))
        (assign ?end (+ ?start ?duration))
        (assign ?newmax (if (> ?end ?max) ?end ?max)) 
    )
   ((timestamp ?i ?time) (max ?r ?max))
   ((calibrated ?i ?r) (timestamp ?i ?end) (max ?r ?newmax) )
)

(:operator (!take_image ?r ?p ?o ?i ?m ?start ?duration)
   (and (calibrated ?i ?r) (on_board ?i ?r) (equipped_for_imaging ?r)
        (supports ?i ?m) (visible_from ?o ?p) (at ?r ?p)
        (timestamp ?i ?time) (max ?r ?max)
        (assign ?start (+ ?time 0.01))
        (assign ?end (+ ?start ?duration))
        (assign ?newmax (if (> ?end ?max) ?end ?max)) 
)
   ((calibrated ?i ?r)(timestamp ?i ?time) (max ?r ?max))
   ((have_image ?r ?o ?m ?end) (timestamp ?i ?end) (max ?r ?newmax)))

(:operator (!communicate_soil_data ?r ?l ?p ?x ?y ?start ?duration)
   (and (at ?r ?x) (at_lander ?l ?y)
        (have_soil_analysis ?r ?p ?t) (visible ?x ?y) 
        (max ?r ?max) 
        (timestamp ?r ?time)        (timestamp ?l ?Ltime)
        (assign ?start (+ (max ?time ?ltime ?t) 0.01)) 
        (assign ?end (+ ?start ?duration)) 
        (assign ?newmax (if (> ?end ?max) ?end ?max)) )
   ((timestamp ?r ?time) (max ?r ?max) (timestamp ?l ?Ltime))
   ((communicated_soil_data ?p) (timestamp ?r ?end) (timestamp ?l ?end) (max ?r ?newmax)))

(:operator (!communicate_rock_data ?r ?l ?p ?x ?y ?start ?duration)
   (and (at ?r ?x) (at_lander ?l ?y)
        (have_rock_analysis ?r ?p ?t) (visible ?x ?y) 
        (max ?r ?max) 
        (timestamp ?r ?time)
        (timestamp ?l ?Ltime)
        (assign ?start (+ (max ?time ?Ltime ?t) 0.01)) 
        (assign ?end (+ ?start ?duration)) 
        (assign ?newmax (if (> ?end ?max) ?end ?max)) )
   
   ((timestamp ?r ?time) (max ?r ?max) (timestamp ?l ?Ltime))
   ((communicated_rock_data ?p)  (timestamp ?r ?end) (timestamp ?l ?end) (max ?r ?newmax)))

(:operator (!communicate_image_data ?r ?l ?o ?m ?x ?y ?start ?duration)
   (and (at ?r ?x) (at_lander ?l ?y)
        (have_image ?r ?o ?m ?t) (visible ?x ?y) 
        (max ?r ?max) 
        (timestamp ?r ?time)         (timestamp ?l ?Ltime)
        (assign ?start (+ (max ?time ?ltime ?t) 0.01)) 
        (assign ?end (+ ?start ?duration)) 
        (assign ?newmax (if (> ?end ?max) ?end ?max)) )
 
   ((timestamp ?r ?time) (max ?r ?max) (timestamp ?l ?Ltime))
   ((communicated_image_data ?o ?m) (timestamp ?r ?end) (timestamp ?l ?end) (max ?r ?newmax)))

;;;;;;;;;;;;;;;;;;;;
;; BOOK-KEEPING OPS
;;;;;;;;;;;;;;;;;;;;
(:operator (!!assert ?g ) 
          () 
          () 
           ?g 
    0)      
(:operator (!!ra ?D ?A ) 
          () 
          ?D 
          ?A 
           0)   
(:operator (!!set-connectivity-info ?problem-name)
          ()
          () 
          ((forall (?r ?g)
              (and (rover ?r)  (assign ?g (make-all-pair-shortest-path '?r)))
              ((shortest-path ?r ?g)) ))     
          0    
)     

;;;;;;;;;;;
;; AXIOMS
;;;;;;;;;;;

(:- (same ?x ?x) nil)
(:- (different ?x ?y) ((not (same ?x ?y))))
(:-(dropcost ?store ?cost)
    ((full ?store) (assign ?cost 1))
    ((assign ?cost 0))
)

(:- (schedule (COMMUNICATED_SOIL_DATA ?goal-loc) ?rover ?goal-loc ?cost)
    ( 
     (equipped_for_soil_analysis ?rover)
     (store_of ?store ?rover)
     (at ?rover ?loc)
     (timestamp ?store ?storetime)
     (dropcost ?store ?droptime)
     (navigation-cost ?rover ?loc ?goal-loc ?n-cost)
     (assign ?cost (+ (max '?n-cost ?storetime) 10 ?droptime))
    )
 )   

(:- (schedule (COMMUNICATED_ROCK_DATA ?goal-loc) ?rover ?goal-loc ?cost)
    ( 
     (equipped_for_rock_analysis ?rover)
     (store_of ?store ?rover)
     (timestamp ?store ?storetime)
     (dropcost ?store ?droptime)
     (at ?rover ?loc)
     (navigation-cost ?rover ?loc ?goal-loc ?n-cost)
     (assign ?cost (+ (max '?n-cost ?storetime ) 8 ?droptime))
    )
)   

(:- (schedule (COMMUNICATED_IMAGE_DATA ?obj ?mode) ?rover  ?goal-loc ?cost)
    ( 
     (equipped_for_imaging ?rover)
     (on_board ?camera ?rover)
     (supports ?camera ?mode)
     (calibrated ?camera ?rover)
     (timestamp ?camera ?camera-time)
     (visible_from ?obj ?goal-loc)
     (at ?rover ?loc)
     (navigation-cost ?rover ?loc ?goal-loc ?n-cost)
     (assign ?cost (+ (max '?n-cost ?camera-time) 7))
    )
)   

(:- (schedule (COMMUNICATED_IMAGE_DATA ?obj ?mode) ?rover  ?goal-loc ?cost)
    ( 
     (equipped_for_imaging ?rover)
     (on_board ?camera ?rover)
     (supports ?camera ?mode)
     (not (calibrated ?camera ?rover))
     (calibration_target ?camera ?t-obj)
     ;(timestamp ?camera ?camera-time)
     (visible_from ?t-obj ?goal-loc)
     (at ?rover ?loc)
     (navigation-cost ?rover ?loc ?goal-loc ?n-cost)
     (assign ?cost (+ '?n-cost  5))
    )
)   



(:- (schedule (JUST-COMMUNICATE ?type ?first ?second) ?rover  ?goal-loc ?cost)
    ( 
     (have-analysis ?rover ?type ?first ?second ?time)
     (timestamp ?lander ?ltime)
     (timestamp ?rover ?rtime)
     (at ?rover ?loc)
     (at_lander ?lander ?loc2)
     (visible ?loc2 ?goal-loc)
     (navigation-cost ?rover ?loc ?goal-loc ?n-cost)
     (assign ?cost (+ (max '?n-cost ?ltime ?rtime ?time) (if (eql '?type 'IMAGE ) 15 10)))
    )
)   


(:- (navigation-cost ?rover ?from ?to ?cost)
     
    ((same ?from ?to) (assign ?cost 0))
    ((shortest-path ?rover ?g)
     (assign ?path (extract-shortest-path '?g '?from '?to))
     (eval '?path)
     (max ?rover ?max)
     (assign ?cost (+ ?max (* (length '?path) 5)))
     
     )
)
    
(:- (have-analysis ?rover SOIL ?loc nil ?time)
   ((have_soil_analysis ?rover ?loc ?time))   
)
(:- (have-analysis ?rover ROCK ?loc nil ?time)
   ((have_rock_analysis ?rover ?loc ?time))   
)
(:- (have-analysis ?rover IMAGE ?obj ?mode ?time)
   ((have_image ?rover ?obj ?mode ?time))   
)
    
(:- (path ?rover ?from ?from  nil)
    nil
)
(:- (path ?rover ?from ?to  ?path)
     ((shortest-path ?rover ?g)
      (assign ?path (extract-shortest-path '?g '?from '?to))
      (eval '?path)))

;;;;;;;;;;;
;; METHODS
;;;;;;;;;;
(:method (achieve-goals (?first . ?rest))
CASE1    ()
         ((!!assert ((goal ?first)))  (:immediate achieve-goals ?rest) )
)
(:method (achieve-goals nil)
CASE2    ()
         ((plan))
)

(:method (plan)
Case1    (:sort-by ?cost #'<
           ((goal ?goal)
           (schedule ?goal ?rover ?goal-loc ?cost) ))                  
                   
         ((do ?goal ?goal-loc ?rover) (plan))
Case2    ()
         ()
)

(:method (empty-store ?s ?rover)
Case1    ((empty ?s))
         ()
Case2    ()
         ((!drop ?rover ?s ?start 1))
)         
(:method (navigate ?rover ?to)
Case1    ((at ?rover ?to))
         ()
Case2    ((at ?rover ?from)
          (path ?rover ?from ?to ?path))
         ((move ?rover ?from ?path))
)

(:method (move ?rover ?from nil)
         ()
         ()
)

(:method (move ?rover ?from ((?v1 ?first) . ?rest))
         ()
         ((!navigate ?rover ?from ?first ?start 5) (:immediate move ?rover ?first ?rest))
)


(:method (do (COMMUNICATED_SOIL_DATA ?goal-loc) ?goal-loc ?rover)
Case1    ((store_of ?s ?rover))
         ((navigate ?rover ?goal-loc) 
          (:immediate empty-store ?s ?rover) 
          (:immediate !sample_soil ?rover ?s ?goal-loc ?start 10)
          (:immediate !!ra ((goal (COMMUNICATED_SOIL_DATA ?goal-loc))) 
                           ((goal (JUST-COMMUNICATE SOIL ?goal-loc nil)))) )
 )        

(:method (do (COMMUNICATED_ROCK_DATA ?goal-loc) ?goal-loc ?rover)
Case2    ((store_of ?s ?rover))
         ((navigate ?rover ?goal-loc) 
          (:immediate empty-store ?s ?rover) 
          (:immediate !sample_rock ?rover ?s ?goal-loc ?start 8)
          (:immediate !!ra ((goal (COMMUNICATED_ROCK_DATA ?goal-loc)))
                           ((goal (JUST-COMMUNICATE ROCK ?goal-loc nil)))) )
)        

(:method (do (COMMUNICATED_IMAGE_DATA ?obj ?mode) ?goal-loc ?rover)
Case1    (:sort-by ?time #'<
          ((on_board ?camera ?rover)
           (supports ?camera ?mode)
           (calibrated ?camera ?rover)  
           (timestamp ?camera ?time)))
         
         ((navigate ?rover ?goal-loc) 
         (:immediate !take_image ?rover ?goal-loc ?obj ?camera ?mode ?start 7)
          (:immediate !!ra ((goal (COMMUNICATED_IMAGE_DATA ?obj ?mode)))
                           ((goal (JUST-COMMUNICATE IMAGE ?obj ?mode)))) )       
Case2    ( :sort-by ?time #'<
          ((on_board ?camera ?rover)
           (supports ?camera ?mode)
           (timestamp ?camera ?time)
           (calibration_target ?camera ?t-obj) ))
         
         ((navigate ?rover ?goal-loc) 
          (!calibrate ?rover ?camera ?t-obj ?goal-loc ?start 5))
)


(:method (do (JUST-COMMUNICATE ?type ?first ?second) ?goal-loc ?rover)
Case2    ()
         ((navigate ?rover ?goal-loc) 
         (comunicate ?type ?first ?second ?goal-loc ?rover)
          (:immediate !!ra ((goal (JUST-COMMUNICATE ?type ?first ?second))) () ) )
)

(:method (comunicate  IMAGE ?first ?second ?goal-loc ?rover)
Case1    ((AT_LANDER ?l ?y))
         ((!communicate_image_data ?rover ?l ?first ?second ?goal-loc ?y ?start 15))         
)

(:method (comunicate  SOIL ?first ?second ?goal-loc ?rover)
Case2    ((AT_LANDER ?l ?y) )
         ((!communicate_soil_data ?rover ?l ?first ?goal-loc ?y ?start 10))         
)

(:method (comunicate  ROCK ?first ?second ?goal-loc ?rover)
Case3    ((AT_LANDER ?l ?y) )
         ((!communicate_rock_data ?rover ?l ?first ?goal-loc ?y ?start 10))         
)

;;;; DEBUGGING
;; debugging

  (:method (print-current-state)
           ((eval (print-current-state)))
           ())

  (:method (print-current-plan)
           ((eval (print-current-plan)))
           ())

  (:method (print-last-action)
           ((eval (print-last-action)))
           ())

  (:method (println)
           ((eval (not (format t "~%"))))
           ())

  (:method (println ?s)
           ((eval (not (format t "~s ~%" '?s))))
           ())

  (:method (println ?s1 ?s2)
           ((eval (not (format t "~s ~s~%" '?s1 '?s2))))
           ())

  (:method (println ?s1 ?s2 ?s3)
           ((eval (not (format t "~s ~s ~s ~%" '?s1 '?s2 '?s3))))
           ())

  (:method (println ?s1 ?s2 ?s3 ?s4)
           ((eval (not (format t "~s ~s ~s ~s~%" '?s1 '?s2 '?s3 '?s4))))
           ())
   
   ;;; Chiu's axioms for debug

  (:- (println) ((eval (progn (format t "~%") t))))
  (:- (println ?s) ((eval (progn (format t "~A~%" '?s) t))))
  (:- (println ?s1 ?s2) ((eval (progn (format t "~A~A~%" '?s1 '?s2) t))))
  (:- (println ?s1 ?s2 ?s3) ((eval (progn (format t "~A~A~A~%" '?s1 '?s2 '?s3) t))))
  (:- (println ?s1 ?s2 ?s3 ?s4) ((eval (progn (format t "~A~A~A~A~%" '?s1 '?s2 '?s3 '?s4) t))))
   ;;;;;;;;;;;;;;
   

)

)



