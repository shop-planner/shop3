(defdomain depot
 ((:operator (!drive ?x ?y ?z ?start ?time)
	 ((time ?x ?time1) (track ?x ?time2) (assign ?start (eval (max ?time1 ?time2)))  (distance ?y ?z ?distance) (speed ?x ?speed) (assign ?time (eval (float (/ ?distance ?speed))))  (assign ?change (eval (+ ?start ?time 0.02))))
	 ((at ?x ?y)
          (time ?x ?time1)
	  (track ?x ?time2))
   ((at ?x ?z)
     (time ?x ?change)
    (track ?x 0)))

  (:operator (!lift ?x ?y ?z ?p ?start ?time)
	     ((time ?x ?time1) 
	      (time ?y ?time2) 
	      (time ?z ?time3)
	      (assign ?start (eval (max ?time1 ?time2))) (assign ?time 1) (assign ?change (eval (+ ?start ?time 0.02))))
   ((at ?y ?p) (clear ?y) (available ?x) (on ?y ?z) (time ?x ?time1) (time ?y ?time2) (time ?z ?time3)
	 (forall (?u) (last-moved ?u) ((last-moved ?u))))
   ((lifting ?x ?y) (time ?x ?change) (time ?y ?change) (time ?z ?change) (clear ?z) (forbidden ?y ?z) (last-moved ?y)))

  (:operator (!drop ?x ?y ?z ?p ?start ?time)
	     ((time ?x ?time1) (time ?y ?time2) (time ?z ?time3) (assign ?start (eval (max ?time1 ?time2 ?time3))) (assign ?time 1) (assign ?change (eval (+ ?start ?time 0.02)))) 
   ((lifting ?x ?y) (clear ?z) (time ?x ?time1) (time ?x ?time2) (time ?z ?time3) (forall (?t) (forbidden ?t ?y) ((forbidden ?t ?y))))
   ((available ?x) (time ?x ?change) (time ?y ?change) (time ?z ?change) (at ?y ?p) (clear ?y) (on ?y ?z)))

   (:method (load-truck ?x ?y ?z ?p)
	   ((time ?x ?time1) (time ?y ?time2) (time ?z ?time3) (weight ?y ?weight) (power ?x ?power) (assign ?time (eval (float (/ ?weight ?power))))
	    (assign ?start (eval (max ?time1 ?time2 ?time3))))
	   ((!load ?x ?y ?z ?p ?start ?time)
	    (update ?z ?start ?time)))

  (:method (update ?z ?start ?time)
	   ((track ?z ?time1)
	    (assign ?change (eval (+ ?start ?time 0.02)))
	    (eval (> ?change ?time1)))
	   ((!!update-track ?z ?time1 ?change))
	   nil
	   nil)
 
  (:operator (!!update-track ?x ?time1 ?time2)
	     ()
	     ((track ?x ?time1))
	     ((track ?x ?time2)))

  (:operator (!load ?x ?y ?z ?p ?start ?time)
	     ((time ?x ?time1) (time ?y ?time2)  (assign ?change (eval (+ ?start ?time 0.02))))
   ((lifting ?x ?y) (time ?x ?time1) (time ?y ?time2))
	 ((in ?y ?z)  (time ?x ?change) (time ?y ?change)  (available ?x)))

   (:method (unload-truck ?x ?y ?z ?p)
	   ((time ?x ?time1) (time ?y ?time2) (weight ?y ?weight) (power ?x ?power) (assign ?time (eval (float (/ ?weight ?power))))
	    (time ?z ?time3) (assign ?start (eval (max ?time1 ?time2 ?time3))))
	   ((!unload ?x ?y ?z ?p ?start ?time)
	    (update ?z ?start ?time)))

  (:operator (!unload ?x ?y ?z ?p ?start ?time)
	     ((time ?x ?time1) (time ?y ?time2) (assign ?change (eval (+ ?start ?time 0.02))))
   ((in ?y ?z) (available ?x) (time ?x ?time1) (time ?y ?time2))
	 ((lifting ?x ?y) (time ?x ?change) (time ?y ?change) ))

	(:operator (!!assert ?g)
		()
		?g
		0) 

	(:operator (!!remove ?g)
		?g
		()
		0)

	
	(:method (achieve-goals ?goals)
		()
		((assert-goals ?goals nil) (find-strayer) (find-dont-move)
		(find-final-loc) (find-all-movables) (add-time-stamp) (move-crate) (is-anything-left)))

	(:method (add-time-stamp)
		 ()
		 ((add-time-pallet) (add-time-crate) (add-time-truck) (add-time-hoist)))

	(:method (add-time-pallet)
		 (:first (pallet ?x)
			 (not (time ?x 0)))
		 ((!!set-time ?x 0) (add-time-pallet))
		 nil
		 nil)

	(:method (add-time-crate)
		 (:first (crate ?x)
			 (not (time ?x 0)))
		 ((!!set-time ?x 0) (add-time-crate))
		 nil
		 nil)

	(:method (add-time-truck)
		 (:first (truck ?x)
			 (not (time ?x 0)))
		 ((!!set-time ?x 0) (!!set-track ?x 0) (add-time-truck))
		 nil
		 nil)

	(:method (add-time-hoist)
		 (:first (hoist ?x)
			 (not (time ?x 0)))
		 ((!!set-time ?x 0) (add-time-hoist))
		 nil
		 nil)
			
	(:operator (!!set-track ?x ?time)
		   ()
		   ()
		   ((track ?x ?time)))

	(:operator (!!set-time ?x ?time)
		   ()
		   ()
		   ((time ?x ?time)))

	(:method (assert-goals (?goal . ?goals) ?out)
		()
		((assert-goals ?goals ((goal ?goal) . ?out))))

	(:method (assert-goals nil ?out)
		()
		((!!assert ?out)))
	
	;; Find those surfaces which do not have any goal associated with them.
	(:method (find-strayer)
		(:first (crate ?x) (not (goal (on ?x ?y))) (not (strayer ?x)))
		((!!assert ((strayer ?x))) (find-strayer))

		nil
		nil)

  ;; Find those surfaces which are in their final position and therefore
	;; don't need to be moved.
	(:method (find-dont-move)
		(:first (goal (on ?x ?y)) (not (need-to-move ?x)))
		((!!remove ((goal (on ?x ?y)))) (find-dont-move))

		nil
		nil)

	;; Find the place each crate not in its final position is supposed to end
	;; up
	(:method (find-final-loc)
		(:first (goal (on ?x ?y)) (not (final-loc ?x ?p1)) (final-loc ?y ?p2))
		;Decomposition
		((!!assert ((final-loc ?x ?p2))) (find-final-loc))

		(:first (goal (on ?x ?y)) (not (final-loc ?x ?p1))
		(not (goal (on ?y ?z))) (not (strayer ?y)) (at ?y ?p2))
		;Decomposition
		((!!assert ((final-loc ?x ?p2))) (find-final-loc))

		nil
		nil)

	;; Check if ?x can be moved to its final position on ?y right now
	(:method (is-movable ?x ?y)
    ((not (stack ?x ?y)) (same-loc ?x ?y) (clear ?x))
    ((!!remove ((goal (on ?x ?y)))) (!!assert ((stack ?x ?y))))

		((not (unload-stack ?x ?y ?z)) (in ?x ?t) (same-loc ?t ?y))
    ((!!remove ((goal (on ?x ?y)))) (!!assert ((unload-stack ?x ?y ?t))))

		((not (putdown ?x ?y ?z)) (lifting ?h ?x) (same-loc ?h ?y))
    ((!!remove ((goal (on ?x ?y)))) (!!assert ((putdown ?x ?y ?h))))

		((not (deliver-stack ?x ?y ?z)) (in ?x ?t) (not (unload-stack ?u ?v ?t)))
    ((!!remove ((goal (on ?x ?y)))) (!!assert ((deliver-stack ?x ?y ?t))))

		nil
		((!!assert ((checked ?x ?y)))))

	;; Find those blocks which can be moved to their final position right now
	(:method (find-all-movables)
		(:first (goal (on ?x ?y)) (not (checked ?x ?y)) (not (goal (on ?y ?z))) 
		(not (strayer ?y)) (clear ?y))
		; Decomposition
		((is-movable ?x ?y) (find-all-movables))

		nil
		nil)

  (:method (finalize ?x)
		(:first (goal (on ?y ?x)))
		((is-movable ?y ?x))

		nil
		nil)

	;; Is ?z movable to its final position?
	(:method (check ?z)
		(:first (goal (on ?z ?x)) (not (goal (on ?x ?y))) (not (strayer ?x))
		(clear ?x))
		; Decomposition
		((is-movable ?z ?x))

		nil
		nil)
	
	;; Is the thing supposed to end up on top of ?z ready to be moved to
	;; its final position?
	(:method (check2 ?z)
		(:first (not (strayer ?z)) (not (goal (on ?z ?y))) (goal (on ?x ?z)))
		((is-movable ?x ?z))

		nil
		nil)

  ;; Move a crate to its final position if possible, move a crate out of the
	;; way otherwise, look for the new opportunities created by this move
	(:method (move-crate)
		(:first (stack ?x ?y) (at ?x ?p) (hoist ?h) (available ?h)
		(at ?h ?p) (on ?x ?z))
		;Decomposition
		((!!remove ((stack ?x ?y))) (!lift ?h ?x ?z ?p ?startl ?timel)
		(!drop ?h ?x ?y ?p ?startd ?timed) (finalize ?x) (check ?z) (check2 ?z) (move-crate))

		(:first (unload-stack ?x ?y ?t) (at ?t ?p) (hoist ?h) (available ?h)
		(at ?h ?p)) 
		;Decomposition
		((!!remove ((unload-stack ?x ?y ?t))) (unload-truck ?h ?x ?t ?p)
		(!drop ?h ?x ?y ?p ?startd ?timed) (finalize ?x) (move-crate))
		
		(:first (putdown ?x ?y ?h) (at ?h ?p))
		;Decomposition
		((!!remove ((putdown ?x ?y ?h))) (!drop ?h ?x ?y ?p ?startd ?timed) (finalize ?x) 
		(move-crate))

		(:first (deliver-stack ?x ?y ?t) (at ?t ?p1) (at ?y ?p2) (hoist ?h)
		(at ?h ?p2) (different ?p1 ?p2) (available ?h))
		;Decomposition
		((!!remove ((deliver-stack ?x ?y ?t))) (!drive ?t ?p1 ?p2 ?startd ?timed)
		(unload-truck ?h ?x ?t ?p2) (!drop ?h ?x ?y ?p2 ?startd2 ?timed2) (finalize ?x) (move-crate))

		(:first (clear ?x) (not (last-moved ?x)) (goal (on ?x ?y)) (at ?x ?p)
		(final-loc ?x ?p) (clear ?z) (at ?z ?p) (different ?x ?z)
		(not (forbidden ?x ?z)) (bottom-of-tower ?x ?v) (not (above ?z ?v))
		(good-tower ?x ?z) (hoist ?h) (available ?h) (at ?h ?p) (on ?x ?u))
		;Decomposition
		((!lift ?h ?x ?u ?p ?startl ?timel) (!drop ?h ?x ?z ?p ?startd ?timed) (check ?u) (check2 ?u)
		(move-crate))

		(:first (clear ?x) (not (last-moved ?x)) (goal (on ?x ?y)) (at ?x ?p)
		(final-loc ?x ?p) (clear ?z) (at ?z ?p) (different ?x ?z)
		(not (forbidden ?x ?z)) (bottom-of-tower ?x ?v) (not (above ?z ?v))
		(hoist ?h) (available ?h) (at ?h ?p) (on ?x ?u))
		;Decomposition
		((!lift ?h ?x ?u ?p ?startl ?timel) (!drop ?h ?x ?z ?p ?startd ?timed) (check ?u) (check2 ?u)
		(move-crate))
	
		(:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p) (truck ?t) (at ?t ?p)
		(hoist ?h) (available ?h) (at ?h ?p) (on ?x ?z))
		;Decomposition
		((!lift ?h ?x ?z ?p ?startl ?timel) (load-truck ?h ?x ?t ?p) (check ?x) (check ?z)
		(check2 ?z) (move-crate))
	
		(:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p) (final-loc ?x ?p)
		(hoist ?h1) (available ?h1) (at ?h1 ?p) (hoist ?h2) (at ?h2 ?p)
		(available ?h2) (different ?h1 ?h2) (on ?x ?z))
		;Decomposition
		((!lift ?h1 ?x ?z ?p ?startl ?timel) (check ?x) (check ?z) (check2 ?z) (move-crate))

		(:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p1) (truck ?t) (at ?t ?p2)
		(different ?p1 ?p2) (forall (?c) (in ?c ?t) (not (final-loc ?c ?p2)))
		(hoist ?h) (available ?h) (at ?h ?p1) (on ?x ?z))
		;Decomposition
		((!drive ?t ?p2 ?p1 ?startd ?timed) (!lift ?h ?x ?z ?p1 ?startl ?timel) (load-truck ?h ?x ?t ?p1) (check ?x)
		(check ?z) (check2 ?z) (move-crate))

		(:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p1) (truck ?t) (at ?t ?p2)
		((different ?p1 ?p2) hoist ?h) (available ?h) (at ?h ?p1) (on ?x ?z))
		;Decomposition
		((!drive ?t ?p2 ?p1 ?startd ?timed) (!lift ?h ?x ?z ?p1 ?startl ?timel) (load-truck ?h ?x ?t ?p1) (check ?x)
		(check ?z) (check2 ?z) (move-crate))
		
		(:first (goal (on ?u ?v)) (strayer ?x) (on ?x ?w) (clear ?y)
		(not (goal (on ?z ?y))) (not (strayer ?y)) (at ?y ?p))
		;Decomposition
		((!!assert ((goal (on ?x ?y)) (final-loc ?x ?p))) (!!remove ((strayer ?x)))
		(assert-final-loc ?x ?p) (move-crate))

		(:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p) (hoist ?h1)
		(available ?h1) (at ?h1 ?p) (hoist ?h2) (at ?h2 ?p) (available ?h2)
		(different ?h1 ?h2) (on ?x ?z))
		;Decomposition
		((!lift ?h1 ?x ?z ?p ?start ?time) (check ?z) (check2 ?z) (move-crate))

		(:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p) (truck ?t)
		(at ?t ?p) (hoist ?h) (available ?h) (at ?h ?p) (on ?x ?z))
		;Decomposition
		((!lift ?h ?x ?z ?p ?start ?time) (load-truck ?h ?x ?t ?p) (check ?z) (check2 ?z)
		(move-crate))
	
		(:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p1) (truck ?t)
		(at ?t ?p2) (different ?p1 ?p2)
		(forall (?c) (in ?c ?t) (not (final-loc ?c ?p2)))
		(hoist ?h) (available ?h) (at ?h ?p1) (on ?x ?z))
		;Decomposition
		((!drive ?t ?p2 ?p1 ?startd ?timed) (!lift ?h ?x ?z ?p1 ?startl ?timel) (load-truck ?h ?x ?t ?p1) (check ?z)
		(check2 ?z) (move-crate))

		(:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p1) (truck ?t)
		(at ?t ?p2) (different ?p1 ?p2) (hoist ?h) (available ?h) (at ?h ?p1)
		(on ?x ?z))
		;Decomposition
		((!drive ?t ?p2 ?p1 ?startd ?timed) (!lift ?h ?x ?z ?p1 ?startl ?timel) (load-truck ?h ?x ?t ?p1) (check ?z)
		(check2 ?z) (move-crate))

		nil
		nil)

	;; Assert new final-loc predicates after a strayted crate is assigned a goal
	(:method (assert-final-loc ?x ?p)
		((supposed-to-be-above ?y ?x) (not (final-loc ?y ?p)))
		((!!assert ((final-loc ?y ?p))) (assert-final-loc ?x ?p))
		
		nil
		nil)

	;; There is no more legal move left. If we have already achieved all the
	;; goals, return the plan. Otherwise, fail
	(:method (is-anything-left)
		(not (goal (on ?x ?y)))
		nil)

	;; Axioms that provide typing
  (:- (place ?x) (depot ?x))
	(:- (place ?x) (distributor ?x))
  (:- (locatable ?x) (truck ?x))
	(:- (locatable ?x) (hoist ?x))
  (:- (locatable ?x) (surface ?x))
	(:- (surface ?x) (pallet ?x))
  (:- (surface ?x) (crate ?x))

	;; Other axioms
	(:- (different ?x ?y) ((not (same ?x ?y))))
	(:- (same ?x ?x) nil)

	(:- (same-loc ?x ?y) ((at ?x ?p) (at ?y ?p)))

	;; To find the crate supposed to end up in the bottom of a tower in which
	;; a specified crate is supposed to end up. Crates already in their final
	;; position are not considered
	(:- (bottom-of-tower ?x ?x) ((goal (on ?x ?y)) (not (goal (on ?y ?z)))))
	(:- (bottom-of-tower ?x ?y) ((goal (on ?x ?z)) (bottom-of-tower ?z ?y)))

	;; Whether a crate is above another crate in the current state of the world
	(:- (above ?x ?x) nil)
	(:- (above ?x ?y) ((on ?z ?y) (above ?x ?z)))

	;; Whether the tower on top of which ?y lies is a good tower with respect
	;; to ?x, i.e. none of the crates supposed to end up beneath ?x is in that
	;; tower
	(:- (good-tower ?x ?y)
			((forall (?z) (supposed-to-be-above ?x ?z) (not (above ?y ?z)))))
	(:- (supposed-to-be-above ?x ?y) ((goal (on ?x ?y))))
	(:- (supposed-to-be-above ?x ?z)
			((goal (on ?y ?z)) (supposed-to-be-above ?x ?y)))
	
  (:- (need-to-move ?x)
	;; need to move x if x needs to go from one surface to another   
	((on ?x ?y) (goal (on ?x ?z)) (different ?y ?z))         
	;; need to move x if x is on y and y needs to be clear
	((on ?x ?y) (goal (clear ?y)))
	;; need to move x if x is on z and something else needs to be on z
	((on ?x ?z) (goal (on ?y ?z)) (different ?x ?y))         
	;; need to move x if x is on something else that needs to be moved 
	((on ?x ?w) (need-to-move ?w)))))
