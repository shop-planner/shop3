;;;;;;;;;;;;;;;;

(defparameter *tc* nil)
(defparameter *fc* nil)


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
	   ((timestamp ?a ?start) (boarding-time ?duration))
           ((!board ?p ?a ?c ?start ?duration) 
	    (:immediate !!cost (call + ?start ?duration))))

   (:operator (!board ?p ?a ?c ?start ?duration)
        ((person ?p) (aircraft ?a) (city ?c) 
	 (at ?a ?c) (at ?p ?c) 
	 (ptimestamp ?a ?pmax)
    	 (assign ?newpmax (max ?pmax (+ ?start ?duration 0.01L0))))
        ((ptimestamp ?a ?pmax) (at ?p ?c) (dest ?a ?c))
        ((ptimestamp ?a ?newpmax) (in ?p ?a))
        0.001)          

   (:method (debark ?p ?a ?c)
        ((timestamp ?a ?start) (debarking-time ?duration))
        ((!debark ?p ?a ?c ?start ?duration) 
	 (:immediate !!cost (call + ?start ?duration))))

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
            (capacity ?a ?cap) (fuel ?a ?fuel) (refuel-rate ?a ?rate)
	    (eval (> ?cap ?fuel))
;	    (assign ?duration (float (/ (ceiling (* 100 (float (/ (- ?cap ?fuel) ?rate)))) 100)))
	    (assign ?duration (float (/ (- ?cap ?fuel) ?rate)))
            (assign ?end (+ ?start ?duration 0.01L0))
            (assign ?newpmax (max ?pmax ?end)))
           ((!!ra ((ptimestamp ?a ?pmax)) 
                 ((ptimestamp ?a ?newpmax)))
            (:immediate !refuel ?a ?c ?start ?duration)
            (:immediate !!cost ?end)))
           
  (:operator (!refuel ?a ?c ?start ?duration)
      ((aircraft ?a) (city ?c) (at ?a ?c) 
       (fuel ?a ?fuel) (capacity ?a ?cap))
      ((fuel ?a ?fuel))
      ((fuel ?a ?cap))
  0.001)

  (:method (zoom ?a ?c1 ?c2)
    ((timestamp ?a ?astart) (ptimestamp ?a ?pmax) 
     (distance ?c1 ?c2 ?dist) (fast-speed ?a ?speed)
     (fuel ?a ?fuel) (fast-burn ?a ?burn)
     (eval (>= ?fuel (* ?dist ?burn)))
;     (assign ?duration (float (/ (ceiling (* 100 (float (/ ?dist ?speed)))) 100)))
     (assign ?duration  (float (/ ?dist ?speed)))
     (assign ?start (max ?pmax ?astart)) 
     (assign ?end (+ ?start ?duration 0.01L0)) )
    (
     (!!ra ((timestamp ?a ?astart) (ptimestamp ?a ?pmax)) 
          ((ptimestamp ?a 0) (timestamp ?a ?end)))
     (:immediate !zoom ?a ?c1 ?c2 ?start ?duration)
     (:immediate !!cost ?end)))
     
  (:operator (!zoom ?a ?c1 ?c2 ?start ?duration)
     ((aircraft ?a) (city ?c1) (city ?c2)
      (at ?a ?c1) (distance ?c1 ?c2 ?dist) (fast-burn ?a ?burn)
      (total-fuel-used ?total-fuel)
      (assign ?new-total (+ ?total-fuel (* ?dist ?burn)))
      (fuel ?a ?fuel)
      (assign ?new-fuel (- ?fuel (* ?dist ?burn)))
     )
     ((at ?a ?c1) (total-fuel-used ?total-fuel) (fuel ?a ?fuel) )
     ((at ?a ?c2) (total-fuel-used ?new-total) (fuel ?a ?new-fuel))
  0.001)
     
  (:method (fly ?a ?c1 ?c2)
    ((timestamp ?a ?astart) (ptimestamp ?a ?pmax) 
     (distance ?c1 ?c2 ?dist) (slow-speed ?a ?speed)
     (fuel ?a ?fuel) (slow-burn ?a ?burn)
     (eval (>= ?fuel (* ?dist ?burn)))
;     (assign ?duration (float (/ (ceiling (* 100 (float (/ ?dist ?speed)))) 100)))
     (assign ?duration (float (/ ?dist ?speed)))
     (assign ?start (max ?pmax ?astart)) 
     (assign ?end (+ ?start ?duration 0.01L0)) )
    (
     (!!ra ((timestamp ?a ?astart) (ptimestamp ?a ?pmax)) 
          ((ptimestamp ?a 0) (timestamp ?a ?end)) )
     (:immediate !fly ?a ?c1 ?c2 ?start ?duration)
     (:immediate !!cost ?end)))
     
  (:operator (!fly ?a ?c1 ?c2 ?start ?duration)
     ((aircraft ?a) (city ?c1) (city ?c2)
      (at ?a ?c1) (distance ?c1 ?c2 ?dist) (slow-burn ?a ?burn)
      (total-fuel-used ?total-fuel)
      (assign ?new-total (+ ?total-fuel (* ?dist ?burn)))
      (fuel ?a ?fuel)
      (assign ?new-fuel (- ?fuel (* ?dist ?burn)))
      )
      ((at ?a ?c1)(total-fuel-used ?total-fuel)(fuel ?a ?fuel))
      ((at ?a ?c2)(total-fuel-used ?new-total)(fuel ?a ?new-fuel))
   0.001)
   

  (:operator (!!preprocessing ?problem-name)
;	      ((not (eval (go-go ?problem-name))))
	      ((totaltime-coeff ?tc) (fuelused-coeff ?fc)
               (eval (setf *tc* ?tc))
               (eval (setf *fc* ?fc)))
	      ()
	      ())

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
    Case2 (:sort-by ?rate #'>
	   ((at ?p ?c1) 
            (at ?a ?c1)
            (aircraft ?a)
	    (refuel-rate ?a ?rate)))
          ((!!assert ((dest ?a ?c1)))
            (:immediate board ?p ?a ?c1)
            (!!assert ((dest ?a ?c2))) 
            (:immediate upper-move-aircraft-no-style ?a ?c2)
            (:immediate debark ?p ?a ?c2)))

 (:method (transport-person ?p ?c2)
    Case3 (:sort-by ?cost #'<
	   ((at ?p ?c1) 
           (aircraft ?a) 
           (at ?a ?c3) 
	   (different ?c1 ?c3)
           (forall (?c) ((dest ?a ?c)) ((same ?c ?c1))) 
           (imply ((different ?c3 ?c1)) 
                       (not (possible-person-in ?c3)))
	   (travel-cost-info ?a ?c3 ?c1 ?cost ?style)
          ))
          (
            (!!assert ((dest ?a ?c1))) 
            (:immediate upper-move-aircraft ?a ?c1 ?style)
            (:immediate board ?p ?a ?c1)
            (!!assert ((dest ?a ?c2))) 
            (:immediate upper-move-aircraft-no-style ?a ?c2)
            (:immediate debark ?p ?a ?c2)
           ))
   
 (:method (upper-move-aircraft ?a ?c ?style)
     Case1  ((at ?a ?c))
            ()
     Case2  ((at ?a ?somecity))
            ((move-aircraft ?a ?somecity ?c ?style)))
           
 (:method (upper-move-aircraft-no-style ?a ?c)
     Case1  ((at ?a ?c))
            ()
     Case2  (:sort-by ?cost #'<
	     ((at ?a ?somecity)
	      (travel-cost-info ?a ?somecity ?c ?cost ?style)
	     ))
            ((move-aircraft ?a ?somecity ?c ?style)))
           
 (:- (travel-cost-info ?a ?from ?to ?cost slow)
     CASE1
     ((capacity ?a ?cap) (distance ?from ?to ?dist)
      (slow-burn ?a ?burn) (eval (< ?cap (* ?dist ?burn)))
      (assign ?cost most-positive-fixnum)
      )
     CASE2
     ((distance ?from ?to ?dist) (fuel ?a ?fuel) (refuel-rate ?a ?rate)
      (slow-burn ?a ?burn) (slow-speed ?a ?speed)
      (eval (>= ?fuel (* ?dist ?burn)))
;      (assign ?cost (+ (* *tc* 
;			  (float (/ ?dist ?speed)))
;		       (* *fc*
;			  (* ?dist ?burn))))
      (assign ?cost (float (/
			     (+ (* *tc* 
				   (float (/ ?dist ?speed)))
				(* *fc*
				   (* ?dist ?burn)))
			     ?rate)))
      )
     CASE3
     ((capacity ?a ?cap)(distance ?from ?to ?dist) (fuel ?a ?fuel)
      (slow-burn ?a ?burn) (slow-speed ?a ?speed) (refuel-rate ?a ?rate)
;      (eval (>= ?fuel (* ?dist ?burn)))
;      (assign ?cost (+ (* *tc*
;			  (+
;			    (float (/ ?dist ?speed))
;			    (float (/ (- ?cap ?fuel)
;				      ?rate))))
;		       (* *fc*
;			  (* ?dist ?burn))))
      (assign ?cost (float (/ 
			     (+ (* *tc*
				   (+
				     (float (/ ?dist ?speed))
				     (float (/ (- ?cap ?fuel)
					       ?rate))))
				(* *fc*
				   (* ?dist ?burn)))
			     ?rate)))
      ))

 (:- (travel-cost-info ?a ?from ?to ?cost fast)
     CASE1
     ((capacity ?a ?cap) (distance ?from ?to ?dist)
      (fast-burn ?a ?burn) (eval (< ?cap (* ?dist ?burn)))
      (assign ?cost most-positive-fixnum)
      )
     CASE2
     ((distance ?from ?to ?dist) (fuel ?a ?fuel) (refuel-rate ?a ?rate)
      (fast-burn ?a ?burn) (fast-speed ?a ?speed)
      (eval (>= ?fuel (* ?dist ?burn)))
;      (assign ?cost (+ (* *tc* 
;			  (float (/ ?dist ?speed)))
;		       (* *fc*
;			  (* ?dist ?burn))))
      (assign ?cost (float (/
			     (+ (* *tc* 
				   (float (/ ?dist ?speed)))
				(* *fc*
				   (* ?dist ?burn)))
			     ?rate)))
      )
     CASE3
     ((capacity ?a ?cap)(distance ?from ?to ?dist) (fuel ?a ?fuel)
      (fast-burn ?a ?burn) (fast-speed ?a ?speed) (refuel-rate ?a ?rate)
;      (eval (>= ?fuel (* ?dist ?burn)))
;      (assign ?cost (+ (* *tc*
;			  (+
;			    (float (/ ?dist ?speed))
;			    (float (/ (- ?cap ?fuel)
;				      ?rate))))
;		       (* *fc*
;			  (* ?dist ?burn))))
      (assign ?cost (float (/ 
			     (+ (* *tc*
				   (+
				     (float (/ ?dist ?speed))
				     (float (/ (- ?cap ?fuel)
					       ?rate))))
				(* *fc*
				   (* ?dist ?burn)))
			     ?rate)))
      ))
      


	      

; (:- (travel-path-info ?a ?from ?to ?path)
;     ((assign ?path
;	     (extract-shortest-path (second (assoc '?a *GRAPH*))
;					  '?from '?to))))
;
; (:method (move-aircraft ?a nil)
;	  ()
;	  ())
;
; (:method (move-aircraft ?a ((?from ?to slow) . ?rest))
 (:method (move-aircraft ?a ?from ?to slow)
          ((distance ?from ?to ?dist) (slow-burn ?a ?burn)
	   (fuel ?a ?fuel)
	   (eval (> ?fuel (* ?dist ?burn)))
	  )
          ((fly ?a ?from ?to)) 
	  ()
	  ((refuel ?a ?from) 
	   (:immediate fly ?a ?from ?to))
  )
            
 ;(:method (move-aircraft ?a ((?from ?to fast) . ?rest))
 (:method (move-aircraft ?a ?from ?to fast)
          ((distance ?from ?to ?dist) (fast-burn ?a ?burn)
	   (fuel ?a ?fuel)
	   (eval (> ?fuel (* ?dist ?burn)))
	  )
          ((zoom ?a ?from ?to))
	  
	  ()
	  ((refuel ?a ?from) 
	   (:immediate zoom ?a ?from ?to))
	  
  )
            
  (:method (transport-aircraft ?a ?c)
	     ((not (no-use ?a)))
            ((!!assert ((no-use ?a)))
             (:immediate upper-move-aircraft-no-style ?a ?c)
             (:immediate !!ra ((no-use ?a)) ())))
  

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
