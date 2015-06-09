(in-package :shop2-user)
;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;



(defdomain ZENOTRAVEL
  (
   
   (:- (same ?x ?x) ())         
   (:- (different ?x ?y) ((not (same ?x ?y))))

   (:-(possible-person-in ?city)
       ((person ?p) (at ?p ?city) (goal ?p ?city2) (different ?city2 ?city)) )

   (:operator (!!cost ?end)
       ((maxtime ?max) 
        (assign ?newmax (eval (if (< ?max ?end)
                                  ?end
                                ?max))))
       ((maxtime ?max))
       ((maxtime ?newmax))
       (- ?newmax ?max))
   
   (:method (board ?p ?a ?c)
	   ((timestamp ?a ?start))
           ((!board ?p ?a ?c ?start 20) 
	    (:immediate !!cost (call + ?start 1))))

   (:operator (!board ?p ?a ?c ?start ?duration)
        ((person ?p) (aircraft ?a) (city ?c) 
	 (at ?a ?c) (at ?p ?c) 
	 (ptimestamp ?a ?pmax)
    	 (assign ?newpmax (max ?pmax (+ ?start ?duration 0.01L0))))
        ((ptimestamp ?a ?pmax) (at ?p ?c) (dest ?a ?c))
        ((ptimestamp ?a ?newpmax) (in ?p ?a))
        0.001)          

   (:method (debark ?p ?a ?c)
        ((timestamp ?a ?start))
        ((!debark ?p ?a ?c ?start 30) 
	 (:immediate !!cost (call + ?start 1))))

   (:operator (!debark ?p ?a ?c ?start ?duration)
      ((person ?p) (aircraft ?a) (city ?c) 
       (at ?a ?c) (in ?p ?a) 
       (ptimestamp ?a ?pmax)
       (assign ?newpmax (max ?pmax (+ ?start ?duration 0.01L0))))
      ((ptimestamp ?a ?pmax) (in ?p ?a) (dest ?a ?c))
      ((ptimestamp ?a ?newpmax) (at ?p ?c))
    0.001)

  (:method (refuel ?a ?c)
           ((timestamp ?a ?start) (ptimestamp ?a ?pmax)
            (eval (< (- ?pmax ?start) 73))
            (assign ?end (+ ?start 73 0.01L0))
            (assign ?newpmax (max ?pmax ?end)))
           ((!!ra ((ptimestamp ?a ?pmax)) 
                 ((ptimestamp ?a ?newpmax)))
            (:immediate !refuel ?a ?c ?l1 ?l2 ?start 73)
            (:immediate !!cost ?end))
           ((timestamp ?a ?astart) (ptimestamp ?a ?pmax)
	    (assign ?start (max ?astart ?pmax))
            (assign ?end (+ ?start 73 0.01L0))
            (assign ?newpmax (max ?pmax ?end)))
           ((!!ra ((ptimestamp ?a ?pmax)) 
                 ((ptimestamp ?a ?newpmax)))
            (:immediate !refuel ?a ?c ?l1 ?l2 ?start 73)
            (:immediate !!cost ?end))
	   
	   )
           
  (:operator (!refuel ?a ?c ?l ?l1 ?start ?duration)
      ((aircraft ?a) (city ?c)
       (at ?a ?c) (fuel-level ?a ?l)
       (next ?l ?l1))
      ((fuel-level ?a ?l))
      ((fuel-level ?a ?l1))
  0.001)

  (:method (zoom ?a ?c1 ?c2)
    ((timestamp ?a ?astart) (ptimestamp ?a ?pmax) (assign ?duration 100)
     (assign ?start (max ?pmax ?astart)) 
     (assign ?end (+ ?start ?duration 0.01L0)) )
    (
     (!!ra ((timestamp ?a ?astart) (ptimestamp ?a ?pmax)) 
          ((ptimestamp ?a 0) (timestamp ?a ?end)) )
     (:immediate !zoom ?a ?c1 ?c2 ?l1 ?l2 ?l3 ?start ?duration)
     (:immediate !!cost ?end)))
     
  (:operator (!zoom ?a ?c1 ?c2 ?l1 ?l2 ?l3 ?start ?duration)
     ((aircraft ?a) (city ?c1) (city ?c2)
      (at ?a ?c1) (fuel-level ?a ?l1)
      (next ?l2 ?l1) (next ?l3 ?l2)
     )
     ((at ?a ?c1) (fuel-level ?a ?l1))
     ((at ?a ?c2) (fuel-level ?a ?l3))
  0.001)
     
  (:method (fly ?a ?c1 ?c2)
    ((timestamp ?a ?astart) (ptimestamp ?a ?pmax) (assign ?duration 180)
     (assign ?start (max ?pmax ?astart)) 
     (assign ?end (+ ?start ?duration 0.01L0)) )
    (
     (!!ra ((timestamp ?a ?astart) (ptimestamp ?a ?pmax)) 
          ((ptimestamp ?a 0) (timestamp ?a ?end)) )
     (:immediate !fly ?a ?c1 ?c2 ?l1 ?l2 ?start ?duration)
     (:immediate !!cost ?end)))
     
  (:operator (!fly ?a ?c1 ?c2 ?l1 ?l2 ?start ?duration)
     ((aircraft ?a) (city ?c1) (city ?c2)
        (at ?a ?c1) (fuel-level ?a ?l1)
	(next ?l2 ?l1)
	)
      ((at ?a ?c1) (fuel-level ?a ?l1)) 
      ((at ?a ?c2) (fuel-level ?a ?l2))
   0.001)
   
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
 
;;;;
;;;;; Main Methods
;;;;;

 (:method (transport-person ?p ?c)
    Case1 ((at ?p ?c))
          ())

 (:method (transport-person ?p ?c2)
    Case2 ((at ?p ?c1) 
           (at ?a ?c1)
           (aircraft ?a))
          ((!!assert ((dest ?a ?c1)))
            (:immediate board ?p ?a ?c1)
            (!!assert ((dest ?a ?c2))) 
            (:immediate upper-move-aircraft ?a ?c2)
            (:immediate debark ?p ?a ?c2)))

 (:method (transport-person ?p ?c2)
    Case3 (
	   (at ?p ?c1) 
           (aircraft ?a) 
           (at ?a ?c3) 
	   (different ?c1 ?c3)
           (forall (?c) ((dest ?a ?c)) ((same ?c ?c1))) 
           (imply ((different ?c3 ?c1)) 
                       (not (possible-person-in ?c3)))
;	   (travel-info ?a ?time ?style1 ?style2)
          )
          (
            (!!assert ((dest ?a ?c1))) 
            (:immediate upper-move-aircraft ?a ?c1)
            (:immediate board ?p ?a ?c1)
            (!!assert ((dest ?a ?c2))) 
            (:immediate upper-move-aircraft ?a ?c2)
            (:immediate debark ?p ?a ?c2)
           ))
   
; (:- (travel-info ?a ?time)
;     FASTFAST
;     ((fuel-level ?a ?level0)
;      (next ?level1 ?level0)
;      (next ?level2 ?level1)
;      (next ?level3 ?level2)
;      (next ?level4 ?level3)
;      (assign ?time 200))
;     FASTSLOW
;     ((fuel-level ?a ?level0)
;      (next ?level1 ?level0)
;      (next ?level2 ?level1)
;      (next ?level3 ?level2)
;      (assign ?time 280))
;     SLOWSLOW
;     ((fuel-level ?a ?level0)
;      (next ?level1 ?level0)
;      (next ?level2 ?level1)
;      (assign ?time 360))
;
      
      
 (:method (upper-move-aircraft ?a ?c)
     Case1  ((at ?a ?c))
            ()
     Case2  ((at ?a ?somecity)
	     (assign ?style 'fast))
            ((move-aircraft ?a ?somecity ?c ?style)))
           
  (:method (move-aircraft ?a ?c1 ?c2 fast)
             ((fuel-level ?a ?l1)
	      (next ?l2 ?l1)
	      (next ?l3 ?l2)
	      )
             ((zoom ?a ?c1 ?c2))
             ((fuel-level ?a ?l1)
	      (next ?l2 ?l1)
	      )
	     ((refuel ?a ?c1) (:immediate zoom ?a ?c1 ?c2))
	     ()
	     ((refuel ?a ?c1)(:immediate refuel ?a ?c1)(:immediate zoom ?a ?c1 ?c2))
 )

  (:method (move-aircraft ?a ?c1 ?c2 slow)
             ((fuel-level ?a ?l)
	      (next ?lnext ?l))
             ((fly ?a ?c1 ?c2))
	     ()
	     ((refuel ?a ?c1) 
	      (:immediate fly ?a ?c1 ?c2))
  )
            
  (:method (transport-aircraft ?a ?c)
            ((not (no-use ?a)))
            ((!!assert ((no-use ?a))) 
             (:immediate upper-move-aircraft ?a ?c))
             (:immediate !!ra ((no-use ?a)) ()))
  

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
