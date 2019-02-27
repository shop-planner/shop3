;; openstacks, strips version
(in-package :shop-openstacks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass pure-pddl-domain (pddl-domain pure-logic-domain-mixin)
  ()
  )
)

(defdomain (openstacks-sequencedstrips-ADL :type pddl-domain)
    (
     (:requirements :typing :adl :action-costs)
     (:types order product count)
     (:predicates (includes ?o - order ?p - product)
                  (waiting ?o - order)
                  (started ?o - order)
                  (shipped ?o - order)
                  (made ?p - product)
                  (stacks-avail ?s - count)
                  (next-count ?s ?ns - count))

;;     (:functions (total-cost) - number)
     
     (:action make-product
      :parameters (?p - product)
      :precondition (and (not (made ?p))
                         (forall (?o - order)
                                 (imply (includes ?o ?p)
                                        (started ?o))))
      :effect (made ?p))

     (:action start-order
      :parameters (?o - order ?avail ?new-avail - count)
      :precondition (and (waiting ?o)
                         (stacks-avail ?avail)
                         (next-count ?new-avail ?avail))
      :effect (and (not (waiting ?o))
                   (started ?o)
                   (not (stacks-avail ?avail))
                   (stacks-avail ?new-avail))
              )

     (:action ship-order
      :parameters (?o - order ?avail ?new-avail - count)
      :precondition (and (started ?o)
                         (forall (?p - product)
                                 (imply (includes ?o ?p) (made ?p)))
                         (stacks-avail ?avail)
                         (next-count ?avail ?new-avail))
      :effect (and (not (started ?o))
                   (shipped ?o)
                   (not (stacks-avail ?avail))
                   (stacks-avail ?new-avail))
              )

     (:action open-new-stack
      :parameters (?open ?new-open - count)
      :precondition (and (stacks-avail ?open)
                         (next-count ?open ?new-open))
      :effect (and (not (stacks-avail ?open))
                   (stacks-avail ?new-open)
                   ;; (increase (total-cost) 1)
                   )
              )

     (:method (assert-goals nil)
       ()
       ())

     (:method (assert-goals (?goal . ?goals))
       ()
       (:ordered (!!assert (goal ?goal))
                 (assert-goals ?goals))
      )

     (:method (plan)
       ((:goal (and . ?goals)))
       ((:ordered (assert-goals ?goals)
                  (open-all-stacks)
                  (plan-for-goals))))

     (:method (open-all-stacks)
       open-one-stack
       ((stacks-avail ?n)
        (next-count ?n ?n1))
       (:ordered (!open-new-stack ?n ?n1)
                 (open-all-stacks))
       done
       ()
       ()
       )

     (:method (plan-for-goals)
       ((goal (shipped ?order))
        (not (shipped ?order)))
      (:ordered (one-step) (plan-for-goals))
      ()
      ((verify-orders)))

     (:method (one-step)
       ;; prefer to ship an order, if possible...
       ((goal (shipped ?o))
        (not (shipped ?o))
        (forall (?p) (includes ?o ?p) (made ?p)))
       ((ship-products ?o))
       (:sort-by ?h
                 (and (goal (shipped ?o))
                      (not (shipped ?o))
                      (includes ?o ?p)
                      (not (made ?p))
                      (ship-cost-heuristic ?p ?h)))
       ((make-product ?p))
       done
       ()
       ()
       )

     (:method (make-product ?p)
       ()
       (:ordered (start-orders ?p)
                 (!make-product ?p)))

     (:method (start-orders ?p)
       ((includes ?o ?p)
        (not (started ?o)))
       ((start-an-order ?o)
        (start-orders ?p))
       done
       ()
       ())

     (:method (verify-orders)
       ((goal (shipped ?order))
        (not (shipped ?order)))
       (:eval (error "complete plan does not satisfy goals.  State is:" shop2:*current-state*))
       ()
       ())

     (:method (start-an-order ?order)
       ((stacks-avail ?next)
        (next-count ?count ?next))
       ((!start-order ?order ?next ?count)))

     (:method (ship-products ?order)
       ((stacks-avail ?count)
        (next-count ?count ?next))
       ((!ship-order ?order ?count ?next))
       )

     (:op (!!assert ?fact)
      :add (?fact))

     (:op (!!delete ?fact)
      :delete (?fact))

     (:- (ship-cost-heuristic ?p ?h)
         ((setof ?o (and (includes ?o ?p) (not (started ?o))) ?os)
          (order-costs ?os ?h 0))
         )

     (:- (order-costs ?os ?h ?hin)
         ((= ?os (?o . ?os1))
          (order-cost ?o ?h1)
          (assign ?h2 (+ ?h1 ?hin))
          (order-costs ?os1 ?h ?h2))
         ((= ?os nil)
          (= ?h ?hin)))

     (:- (order-cost ?o ?h)
         ((started ?o)
          (product-cost ?o ?pc)
          (assign ?h (1+ ?pc)))
         ((not (started ?o))
          (product-cost ?o ?h)))

     (:- (product-cost ?o ?c)
         ((setof ?p
                 (and (includes ?o ?p)
                      (not (made ?p)))
                 ?ps)
          (assign ?c (length '?ps))))
     )
  )

(defdomain (openstacks-sequencedstrips-ADL-included
            :type pure-pddl-domain
            :source-pddl-domain
            #.(merge-pathnames "domain-nocosts.pddl" (or *compile-file-truename* *load-truename*)))
    ((:include  openstacks-sequencedstrips-ADL-nocosts "domain-nocosts.pddl")
     (:static includes next-count)
     
     (:pddl-method (plan)
       (exists (?o - order)
               (not (shipped ?o)))
       (:ordered
        (open-all-stacks)
        (plan-for-goals)))

     (:pddl-method (open-all-stacks)
       open-a-stack-and-recurse
       (exists (?n ?n1 - count)
               (and (stacks-avail ?n)
                    (next-count ?n ?n1)))
       (:ordered (open-new-stack ?n ?n1)
                 (open-all-stacks)))

     (:pddl-method (open-all-stacks)
       done-opening-stacks
       (and (stacks-avail ?n)
            ;; FIXME: should implement rewriting of negated existentials...
            #+ignore
            (not (exists (?n1 - count)
                         (next-count ?n ?n1)))
            (forall (?n1 - count)
              (not (next-count ?n ?n1))))
       ())

     (:pddl-method (open-new-stack ?n ?n1)
        (and (stacks-avail ?n)
             (next-count ?n ?n1))
        (!open-new-stack ?n ?n1))       

     (:pddl-method (plan-for-goals)
       ship-one-order
       (exists (?o - order)
               (not (shipped ?o)))
       (:ordered (one-step) (plan-for-goals)))

     (:pddl-method (plan-for-goals)
       done-shipping-orders
       (forall (?order - order) 
               (shipped ?order))
       ())

     (:method (one-step)
       ship-order
       ;; prefer to ship an order, if possible...
       (and (order ?o)
            (not (shipped ?o))
            ;; this is a kind of cheaty way to avoid the FORALL dependency check.
            (ready-to-ship ?o)
            (started ?o)
            #+ignore (forall ?p (includes ?o ?p) (made ?p)))
       (ship-an-order ?o))

     (:method (one-step)
       repair-an-order
       (and (order ?o)
            (not (shipped ?o))
            (not (started ?o))
            (ready-to-ship ?o)
            (next-count ?avail-prime ?avail))
       (!start-order ?o ?avail ?avail-prime))

     (:pddl-method (one-step)
       (forall (?order - order)
         (imply (not (shipped ?order))
                (exists (?p - product)
                   (and (includes ?order ?p)
                        (not (made ?p))))))
       (make-a-product))

     ;; here's a problem -- we want the product to be made only if
     ;; there are no products we can ship.  But we can't make that a
     ;; precondition for this method.
     (:method (make-a-product)
       (:sort-by ?h
                 (and
                  (includes ?o ?p)
                  (not (shipped ?o))
                  (not (made ?p))
                  (ship-cost-heuristic ?p ?h)))
       ((make-product ?p)))

     (:method (ship-an-order ?order)
       (and (order ?order)
            (not (shipped ?order))
            (forall ?p (includes ?order ?p) (made ?p))
            (stacks-avail ?avail)
            (next-count ?avail ?new))
       (!ship-order ?order ?avail ?new))

     (:method (make-product ?p)
       ()
       (:ordered (start-orders ?p)
                 (!make-product ?p)))

     (:pddl-method (start-orders ?p)
       start-an-order-and-recurse
       (exists (?o - order)
         (and (includes ?o ?p)
              (not (started ?o))))
       (:ordered (start-an-order-for ?p ?order)
        (start-orders ?p)))

     (:method (start-an-order-for ?p ?o)
       ((includes ?o ?p)
        (not (started ?o))
        (stacks-avail ?avail)
        (next-count ?avail-prime ?avail))
       (!start-order ?o ?avail ?avail-prime))

     (:pddl-method (start-orders ?p)
       done-starting-orders
       (forall (?o - order)
               (imply (includes ?o ?p)
                      (started ?o)))
       ())

#|
     (:method (verify-orders)
       orders-incorrect
       ((exists (?order - order) 
          (not (shipped ?order))))
       (:eval (error "complete plan does not satisfy goals.  State is:" shop2:*current-state*)))

     (:method (verify-orders)
       orders-correct
       ((forall (?order - order) 
          (shipped ?order)))
       ())
|#

     (:method (start-an-order ?order)
       ((stacks-avail ?next)
        (next-count ?count ?next))
       ((!start-order ?order ?next ?count)))

     (:method (ship-products ?order)
       ((stacks-avail ?count)
        (next-count ?count ?next))
       ((!ship-order ?order ?count ?next))
       )

     (:op (!!assert ?fact)
      :add (?fact)
      :cost 0.0)

     (:op (!!delete ?fact)
      :delete (?fact)
      :cost 0.0)

     (:- (ship-cost-heuristic ?p ?h)
         ((setof ?o (and (includes ?o ?p) (not (started ?o))) ?os)
          (order-costs ?os ?h 0))
         )

     (:- (ship-cost-heuristic ?p 0)
         ((forall ?o (order ?o) (imply (includes ?o ?p) (started ?o)))))

     (:- (order-costs ?os ?h ?hin)
         ((= ?os (?o . ?os1))
          (order-cost ?o ?h1)
          (assign ?h2 (+ ?h1 ?hin))
          (order-costs ?os1 ?h ?h2))
         ((= ?os nil)
          (= ?h ?hin)))

     (:- (order-cost ?o ?h)
         ((started ?o)
          (product-cost ?o ?pc)
          (assign ?h (1+ ?pc)))
         ((not (started ?o))
          (product-cost ?o ?h)))

     (:- (product-cost ?o ?c)
         ((setof ?p
                 (and (includes ?o ?p)
                      (not (made ?p)))
                 ?ps)
          (assign ?c (length '?ps))))

     (:- (ready-to-ship ?o) ((forall ?p (includes ?o ?p) (made ?p))))

     (:- (:original-goal ?g) ((:goal ?g)))
     ))


;;pure-pddl-domain version with replanning functionality
(defdomain (test-openstacks :type pure-pddl-domain :source-pddl-domain #.(merge-pathnames "domain-nocosts.pddl" *load-truename*))
    ((:include  openstacks-sequencedstrips-ADL-nocosts "domain-nocosts.pddl")
     (:static includes next-count)
     
     (:pddl-method (plan)
       (exists (?o - order)
               (not (shipped ?o)))
       (:ordered
        (open-all-stacks)
	;;Added for the replanning case - should map to epsilon otherwise
	(reset-order-status)
        (plan-for-goals)))

     (:pddl-method (open-all-stacks)
       open-a-stack-and-recurse
       (exists (?n ?n1 - count)
               (and (stacks-avail ?n)
                    (next-count ?n ?n1)))
       (:ordered (open-new-stack ?n ?n1)
                 (open-all-stacks)))

     (:pddl-method (open-all-stacks)
       done-opening-stacks
       (and (stacks-avail ?n)
            ;; FIXME: should implement rewriting of negated existentials...
            #+ignore
            (not (exists (?n1 - count)
                         (next-count ?n ?n1)))
            (forall (?n1 - count)
              (not (next-count ?n ?n1))))
       ())

     (:pddl-method (open-new-stack ?n ?n1)
        (and (stacks-avail ?n)
             (next-count ?n ?n1))
        (!open-new-stack ?n ?n1))       

     ;;Only used during replanning to reset the internal stack setup
     ;;  via resetting the package preparation status (started->waiting)
     (:pddl-method (reset-order-status)
       reset-an-order-and-recurse
       (and (started ?o)
	    (not (shipped ?o)))
       (:ordered (!reset ?o) (reset-order-status)))

     (:pddl-method (reset-order-status)
       done-resetting
       (forall (?o - order)
	       (not (started ?o)))
       ())

     (:pddl-method (plan-for-goals)
       ship-one-order
       (exists (?o - order)
               (not (shipped ?o)))
       (:ordered (one-step) (plan-for-goals)))

     (:pddl-method (plan-for-goals)
       done-shipping-orders
       (forall (?order - order) 
               (shipped ?order))
       ())

     (:method (one-step)
       ship-order
       ;; prefer to ship an order, if possible...
       (and (order ?o)
            (not (shipped ?o))
            ;; this is a kind of cheaty way to avoid the FORALL dependency check.
            (ready-to-ship ?o)
            (started ?o)
            #+ignore (forall ?p (includes ?o ?p) (made ?p)))
       (ship-an-order ?o))

     (:method (one-step)
       repair-an-order
       (and (order ?o)
            (not (shipped ?o))
            (not (started ?o))
            (ready-to-ship ?o)
            (next-count ?avail-prime ?avail))
       (!start-order ?o ?avail ?avail-prime))

     (:pddl-method (one-step)
       (forall (?order - order)
         (imply (not (shipped ?order))
                (exists (?p - product)
                   (and (includes ?order ?p)
                        (not (made ?p))))))
       (make-a-product))

     ;; here's a problem -- we want the product to be made only if
     ;; there are no products we can ship.  But we can't make that a
     ;; precondition for this method.
     (:method (make-a-product)
       (:sort-by ?h
                 (and
                  (includes ?o ?p)
                  (not (shipped ?o))
                  (not (made ?p))
                  (ship-cost-heuristic ?p ?h)))
       ((make-product ?p)))

     (:method (ship-an-order ?order)
       (and (order ?order)
            (not (shipped ?order))
            (forall ?p (includes ?order ?p) (made ?p))
            (stacks-avail ?avail)
            (next-count ?avail ?new))
       (!ship-order ?order ?avail ?new))

     (:method (make-product ?p)
       ()
       (:ordered (start-orders ?p)
                 (!make-product ?p)))

     (:pddl-method (start-orders ?p)
       start-an-order-and-recurse
       (exists (?o - order)
         (and (includes ?o ?p)
              (not (started ?o))))
       (:ordered (start-an-order-for ?p ?order)
        (start-orders ?p)))

     (:method (start-an-order-for ?p ?o)
       ((includes ?o ?p)
        (not (started ?o))
        (stacks-avail ?avail)
        (next-count ?avail-prime ?avail))
       (!start-order ?o ?avail ?avail-prime))

     (:pddl-method (start-orders ?p)
       done-starting-orders
       (forall (?o - order)
               (imply (includes ?o ?p)
                      (started ?o)))
       ())

#|
     (:method (verify-orders)
       orders-incorrect
       ((exists (?order - order) 
          (not (shipped ?order))))
       (:eval (error "complete plan does not satisfy goals.  State is:" shop2:*current-state*)))

     (:method (verify-orders)
       orders-correct
       ((forall (?order - order) 
          (shipped ?order)))
       ())
|#

     (:method (start-an-order ?order)
       ((stacks-avail ?next)
        (next-count ?count ?next))
       ((!start-order ?order ?next ?count)))

     (:method (ship-products ?order)
       ((stacks-avail ?count)
        (next-count ?count ?next))
       ((!ship-order ?order ?count ?next))
       )

     (:op (!!assert ?fact)
      :add (?fact)
      :cost 0.0)

     (:op (!!delete ?fact)
      :delete (?fact)
      :cost 0.0)

     (:- (ship-cost-heuristic ?p ?h)
         ((setof ?o (and (includes ?o ?p) (not (started ?o))) ?os)
          (order-costs ?os ?h 0))
         )

     (:- (ship-cost-heuristic ?p 0)
         ((forall ?o (order ?o) (imply (includes ?o ?p) (started ?o)))))

     (:- (order-costs ?os ?h ?hin)
         ((= ?os (?o . ?os1))
          (order-cost ?o ?h1)
          (assign ?h2 (+ ?h1 ?hin))
          (order-costs ?os1 ?h ?h2))
         ((= ?os nil)
          (= ?h ?hin)))

     (:- (order-cost ?o ?h)
         ((started ?o)
          (product-cost ?o ?pc)
          (assign ?h (1+ ?pc)))
         ((not (started ?o))
          (product-cost ?o ?h)))

     (:- (product-cost ?o ?c)
         ((setof ?p
                 (and (includes ?o ?p)
                      (not (made ?p)))
                 ?ps)
          (assign ?c (length '?ps))))

     (:- (ready-to-ship ?o) ((forall ?p (includes ?o ?p) (made ?p))))
     ))
 

  
