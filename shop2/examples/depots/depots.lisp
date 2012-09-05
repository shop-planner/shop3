(in-package :shop2-user)

(defun define-depot-domain ()
  (let ((*define-silently* t))
    (defdomain (depot :redefine-ok t)
       ((:operator (!drive ?x ?y ?z)
                   ((at ?x ?y))
                   ((at ?x ?z)))

        (:operator (!lift ?x ?y ?z ?p)
                   ((at ?y ?p) (clear ?y) (available ?x) (on ?y ?z) 
                    (forall (?u) (last-moved ?u) ((last-moved ?u))))
                   ((lifting ?x ?y) (clear ?z) (forbidden ?y ?z) (last-moved ?y)))

        (:operator (!drop ?x ?y ?z ?p)
                   ((lifting ?x ?y) (clear ?z) (forall (?t) (forbidden ?t ?y) ((forbidden ?t ?y))))
                   ((available ?x) (at ?y ?p) (clear ?y) (on ?y ?z)))

        (:operator (!load ?x ?y ?z ?p)
                   ((lifting ?x ?y))
                   ((in ?y ?z) (available ?x)))

        (:operator (!unload ?x ?y ?z ?p)
                   ((in ?y ?z) (available ?x))
                   ((lifting ?x ?y)))

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
            (find-final-loc) (find-all-movables) (move-crate) (is-anything-left)))

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
                                        ; ?x is directly movable from ?z to ?x
           (:first (stack ?x ?y) (at ?x ?p) (hoist ?h) (available ?h)
                   (at ?h ?p) (on ?x ?z))
                                        ;Decomposition
           ((!!remove ((stack ?x ?y))) (!lift ?h ?x ?z ?p)
            (!drop ?h ?x ?y ?p) (finalize ?x) (check ?z) (check2 ?z) (move-crate))

                                        ; ?x is in a truck and can be moved directly to its final position on
                                        ; top of ?y
           (:first (unload-stack ?x ?y ?t) (at ?t ?p) (hoist ?h) (available ?h)
                   (at ?h ?p))
                                        ;Decomposition
           ((!!remove ((unload-stack ?x ?y ?t))) (!unload ?h ?x ?t ?p)
            (!drop ?h ?x ?y ?p) (finalize ?x) (move-crate))
           
                                        ; ?x is held by a hoist and can be moved directly to its final position
           (:first (putdown ?x ?y ?h) (at ?h ?p))
                                        ;Decomposition
           ((!!remove ((putdown ?x ?y ?h))) (!drop ?h ?x ?y ?p) (finalize ?x) 
            (move-crate))

                                        ; ?x can be delivered and stacked on its final position
           (:first (deliver-stack ?x ?y ?t) (at ?t ?p1) (at ?y ?p2) (hoist ?h)
                   (at ?h ?p2) (available ?h))
                                        ;Decomposition
           ((!!remove ((deliver-stack ?x ?y ?t))) (!drive ?t ?p1 ?p2)
            (!unload ?h ?x ?t ?p2) (!drop ?h ?x ?y ?p2) (finalize ?x) (move-crate))

                                        ; If ?x is supposed to end up in the same place it is right now but
                                        ; its beneath surface is not free, move it into top of another 'good'
                                        ; and 'not forbidden' tower
           (:first (clear ?x) (not (last-moved ?x)) (goal (on ?x ?y)) (at ?x ?p)
                   (final-loc ?x ?p) (clear ?z) (at ?z ?p) (different ?x ?z)
                   (not (forbidden ?x ?z)) (bottom-of-tower ?x ?v) (not (above ?z ?v))
                   (good-tower ?x ?z) (hoist ?h) (available ?h) (at ?h ?p) (on ?x ?u))
                                        ;Decomposition
           ((!lift ?h ?x ?u ?p) (!drop ?h ?x ?z ?p) (check ?u) (check2 ?u)
            (move-crate))

                                        ; Same as the above, only we don't care if tower is good
           (:first (clear ?x) (not (last-moved ?x)) (goal (on ?x ?y)) (at ?x ?p)
                   (final-loc ?x ?p) (clear ?z) (at ?z ?p) (different ?x ?z)
                   (not (forbidden ?x ?z)) (bottom-of-tower ?x ?v) (not (above ?z ?v))
                   (hoist ?h) (available ?h) (at ?h ?p) (on ?x ?u))
                                        ;Decomposition
           ((!lift ?h ?x ?u ?p) (!drop ?h ?x ?z ?p) (check ?u) (check2 ?u)
            (move-crate))
           
                                        ; Use the truck as a mean to move ?x out of the way
           (:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p) (truck ?t) (at ?t ?p)
                   (hoist ?h) (available ?h) (at ?h ?p) (on ?x ?z))
                                        ;Decomposition
           ((!lift ?h ?x ?z ?p) (!load ?h ?x ?t ?p) (check ?x) (check ?z)
            (check2 ?z) (move-crate))
           
                                        ; Use the extra hoist as a mean to move ?x out of the way
           (:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p) (final-loc ?x ?p) 
                   (hoist ?h1) (available ?h1) (at ?h1 ?p) (hoist ?h2) (at ?h2 ?p)
                   (available ?h2) (different ?h1 ?h2) (on ?x ?z))
                                        ;Decomposition
           ((!lift ?h1 ?x ?z ?p) (check ?x) (check ?z) (check2 ?z) (move-crate))

                                        ; Bring a truck to use it as a mean to move ?x out of the way
           (:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p1) (truck ?t) (at ?t ?p2)
                   (different ?p1 ?p2) (forall (?c) (in ?c ?t) (not (final-loc ?c ?p2)))
                   (hoist ?h) (available ?h) (at ?h ?p1) (on ?x ?z))
                                        ;Decomposition
           ((!drive ?t ?p2 ?p1) (!lift ?h ?x ?z ?p1) (!load ?h ?x ?t ?p1) (check ?x)
            (check ?z) (check2 ?z) (move-crate))

                                        ; The same as above, only we are not being 'picky' about the truck we want
                                        ; to use
           (:first (clear ?x) (goal (on ?x ?y)) (at ?x ?p1) (truck ?t) (at ?t ?p2)
                   (different ?p1 ?p2) (hoist ?h) (available ?h) (at ?h ?p1) (on ?x ?z))
                                        ;Decomposition
           ((!drive ?t ?p2 ?p1) (!lift ?h ?x ?z ?p1) (!load ?h ?x ?t ?p1) (check ?x)
            (check ?z) (check2 ?z) (move-crate))
           
                                        ; Assert an appropriate goal for a strayer block to move it out of the way
                                        ; in the next steps
           (:first (goal (on ?u ?v)) (strayer ?x) (on ?x ?w) (clear ?y)
                   (not (goal (on ?z ?y))) (not (strayer ?y)) (at ?y ?p))
                                        ;Decomposition
           ((!!assert ((goal (on ?x ?y)) (final-loc ?x ?p))) (!!remove ((strayer ?x)))
            (assert-final-loc ?x ?p) (move-crate))

                                        ; Using the extra hoist to move a strayer of the way
           (:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p) (hoist ?h1)
                   (available ?h1) (at ?h1 ?p) (hoist ?h2) (at ?h2 ?p) (available ?h2)
                   (different ?h1 ?h2) (on ?x ?z))
                                        ;Decomposition
           ((!lift ?h1 ?x ?z ?p) (check ?z) (check2 ?z) (move-crate))

                                        ; Using a truck to move a strayer out of the way
           (:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p) (truck ?t)
                   (at ?t ?p) (hoist ?h) (available ?h) (at ?h ?p) (on ?x ?z))
                                        ;Decomposition
           ((!lift ?h ?x ?z ?p) (!load ?h ?x ?t ?p) (check ?z) (check2 ?z)
            (move-crate))
           
                                        ; Bringing a truck to move a strayer out of the way
           (:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p1) (truck ?t)
                   (at ?t ?p2) (different ?p1 ?p2)
                   (forall (?c) (in ?c ?t) (not (final-loc ?c ?p2)))
                   (hoist ?h) (available ?h) (at ?h ?p1) (on ?x ?z))
                                        ;Decomposition
           ((!drive ?t ?p2 ?p1) (!lift ?h ?x ?z ?p1) (!load ?h ?x ?t ?p1) (check ?z)
            (check2 ?z) (move-crate))

                                        ; Bringing a truck to move a strayer out of the way, not being picky
                                        ; about which truck to use
           (:first (goal (on ?u ?v)) (strayer ?x) (clear ?x) (at ?x ?p1) (truck ?t)
                   (at ?t ?p2) (different ?p1 ?p2) (hoist ?h) (available ?h) (at ?h ?p1)
                   (on ?x ?z))
                                        ;Decomposition
           ((!drive ?t ?p2 ?p1) (!lift ?h ?x ?z ?p1) (!load ?h ?x ?t ?p1) (check ?z)
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
        
        ;; Whether a crate is in the way of another crate and thus must be moved
        (:- (need-to-move ?x)
            ;; need to move x if x needs to go from one surface to another   
            ((on ?x ?y) (goal (on ?x ?z)) (different ?y ?z))         
            ;; need to move x if x is on y and y needs to be clear
            ((on ?x ?y) (goal (clear ?y)))
            ;; need to move x if x is on z and something else needs to be on z
            ((on ?x ?z) (goal (on ?y ?z)) (different ?x ?y))         
            ;; need to move x if x is on something else that needs to be moved 
            ((on ?x ?w) (need-to-move ?w)))))))

(eval-when (:load-toplevel)
  (define-depot-domain))
