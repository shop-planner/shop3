(in-package :shop-user)
;;; This file contains a SHOP domain representation of the block-stacking
;;; algorithm from the following paper:
;;;    N. Gupta and D. Nau, On the complexity of blocks-world planning,
;;;    Artificial Intelligence 56(2-3):223-254, August 1992.


;;; ------------------------------------------------------------------------
;;; Declare all the data
;;; ------------------------------------------------------------------------

(defun define-blocks-domain ()
  (let (( *define-silently* t)
        (shop2::*ignore-singleton-variables* t))
    (defdomain (blocks-normal :redefine-ok t)
       (
        ;; basic block-stacking operators

        (:operator (!pickup ?a)
                   ((clear ?a) (on-table ?a))
                   ((holding ?a)))

        (:operator (!putdown ?b)
                   ((holding ?b))
                   ((on-table ?b) (clear ?b)))

        (:operator (!stack ?c ?d)
                   ((holding ?c) (clear ?d))
                   ((on ?c ?d) (clear ?c)))

        (:operator (!unstack ?e ?f)
                   ((clear ?e) (on ?e ?f))
                   ((holding ?e) (clear ?f)))

        ;; book-keeping methods & ops, to keep track of what needs to be done
        (:operator (!!assert ?g)
                   ()
                   ?g
                   ;; Since !!ASSERT isn't a real blocks-world operator, make its cost 0
                   0)

        (:operator (!!remove ?g)
                   ?g
                   ()
                   ;; Since !!REMOVE isn't a real blocks-world operator, make its cost 0
                   0)

        ;; The method for the top-layer task
        (:method (achieve-goals ?goals)
           ()
           ((assert-goals ?goals nil)
            (find-nomove) (add-new-goals) (find-movable) (move-block) (verify-solution)))

        (:method (verify-solution)
          (and (setof ?g (goal ?g) ?gs)
               (all-true ?gs))
          ()
          )

        (:- (all-true nil)
            ())

        (:- (all-true (?g . ?rest))
            (and ?g
                 (all-true ?rest)))

        (:method (assert-goals (?goal . ?goals) ?out)
           ()
           ((assert-goals ?goals ((goal ?goal) . ?out))))

        (:method (assert-goals nil ?out)
           ()
           ((!!assert ?out)))

        ;; Find those blocks which don't need to be moved.
        ;; This is called once in the beginning of the process.
        ;; Blocks in their final positions are distinguished with a
        ;; dont-move predicate in the world state
        (:method (find-nomove)
           (:first (block ?x) (not (dont-move ?x)) (not (need-to-move ?x)))
           ((!!assert ((dont-move ?x))) (find-nomove))
           nil
           nil)

        ;; Find blocks with no assosiated goals and add an appropriate goal
        ;; (on-table ?x) for those blocks if they have to be moved, i.e. if
        ;; they are on the way of something else. Otherwise, we can simply
        ;; ignore them
        (:method (add-new-goals)
           (:first (block ?x) (not (dont-move ?x)) (not (goal (on-table ?x)))
                   (not (goal (on ?x ?y))))
           ;;Decomposition
           ((!!assert ((goal (on-table ?x)))) (add-new-goals))

           nil
           nil)

        ;; Find all those blocks which can be moved to their final position
        ;; directly in the initial state of the world. Such blocks are marked
        ;; with either a put-on-table predicate or a stack-on-block predicate,
        ;; depending on their associated goal
        (:method (find-movable)
           (:first (clear ?x) (not (dont-move ?x))
                   (goal (on-table ?x)) (not (put-on-table ?x)))
                                        ; Decomposition
           ((!!assert ((put-on-table ?x))) (find-movable))

           (:first (clear ?x) (not (dont-move ?x)) (goal (on ?x ?y))
                   (not (stack-on-block ?x ?y)) (dont-move ?y) (clear ?y))
                                        ;Decomposition
           ((!!assert ((stack-on-block ?x ?y))) (find-movable))

           nil
           nil)

        ;; Check if the thing that is supposed to end up on top of ?x is ready
        ;; to go there. This is called whenever we move block ?x to its final
        ;; position.
        (:method (check ?x)
           (:first (goal (on ?y ?x)) (clear ?y))
           ((!!assert ((stack-on-block ?y ?x))))
           nil
           nil)

        ;; Check if the thing that is supposed to end up on top of ?x is ready
        ;; to go there. This is called whenever something is removed from top
        ;; of the ?x. Note that here, we must check if ?x is in final position,
        ;; while in the latter method we were sure that it was and thus we did
        ;; not need a verification.
        (:method (check2 ?x)
           (:first (dont-move ?x) (goal (on ?y ?x)) (clear ?y))
           ((!!assert ((stack-on-block ?y ?x))))
           nil
           nil)

        ;; Check if x can go to where it is supposed to end up. This is again
        ;; called whenever something is removed from top of the ?x, making it
        ;; able to move around.
        (:method (check3 ?x)
           (:first (dont-move ?x))
           nil
           (:first (goal (on ?x ?y)) (clear ?y) (dont-move ?y))
           ((!!assert ((stack-on-block ?x ?y))))
           (:first (goal (on-table ?x)))
           ((!!assert ((put-on-table ?x))))
           nil
           nil)

        ;; Just an efficiency trick, to avoid calculating things twice
        ;; This trick is a general technique while working with SHOP. If there
        ;; are several possible decompositions for a task and they have some
        ;; preconditions in common, one can "factor" those preconditions and
        ;; add a new level in the task hierarchy whose precondition is the
        ;; set of common preconditions. This way, one may avoid calculating
        ;; the shared preconditions for several times. Here, the stack-on-block
        ;; is the shared precondition in the move-block method
        (:method (move-block1 ?x ?z)
           method-for-moving-x-from-y-to-z
           (:first (on ?x ?y))
                                        ;Decomposition
           ((!unstack ?x ?y) (!stack ?x ?z)
            (!!assert ((dont-move ?x)))
            (!!remove ((stack-on-block ?x ?z)))
            (check ?x) (check2 ?y) (check3 ?y))

           method-for-moving-x-from-table-to-z
           nil
                                        ; Decomposition
           ((!pickup ?x) (!stack ?x ?z)
            (!!assert ((dont-move ?x)))
            (!!remove ((stack-on-block ?x ?z)))
            (check ?x)))

        ;; This is the main method. It first moves the blocks that are directly
        ;; movable to their final positions to their final position. Doing so
        ;; may make other blocks directly movable to their final positions.
        ;; Thus this method checks such possibilities using methods check, check1
        ;; and check2 and then calls itself to simulate an iteration. If there is
        ;; no such block, it means that we are done with the planning (A direct
        ;; result of the fact that blocks world problems are always solvable).
        (:method (move-block)
           (:first (stack-on-block ?x ?y))
           ((move-block1 ?x ?y) (move-block))

           method-for-moving-x-from-y-to-table
           (:first (put-on-table ?x) (on ?x ?y))
                                        ;Decomposition
           ((!unstack ?x ?y) (!putdown ?x)
            (!!assert ((dont-move ?x)))
            (!!remove ((put-on-table ?x)))
            (check ?x) (check2 ?y) (check3 ?y) (move-block))

           method-for-moving-x-out-of-the-way
           (:first (clear ?x) (not (dont-move ?x)) (on ?x ?y))
                                        ;Decomposition
           ((!unstack ?x ?y) (!putdown ?x)
            (check2 ?y) (check3 ?y) (move-block))

           termination-method-branch
           nil
           nil)

        ;; state axioms

        (:- (same ?x ?x) nil)

        ;; Finds the blocks that must be moved, because they are blocking other
        ;; blocks' way.
        (:- (need-to-move ?x)
            ;; need to move x if x needs to go from one block to another
            ((on ?x ?y) (goal (on ?x ?z)) (not (same ?y ?z)))
            ;; need to move x if x needs to go from table to block
            ((on-table ?x) (goal (on ?x ?z)))
            ;; need to move x if x needs to go from block to table
            ((on ?x ?y) (goal (on-table ?x)))
            ;; need to move x if x is on y and y needs to be clear
            ((on ?x ?y) (goal (clear ?y)))
            ;; need to move x if x is on z and something else needs to be on z
            ((on ?x ?z) (goal (on ?y ?z)) (not (same ?x ?y)))
            ;; need to move x if x is on something else that needs to be moved
            ((on ?x ?w) (need-to-move ?w)))
        ))))

(eval-when (:load-toplevel :execute)
  (define-blocks-domain))

