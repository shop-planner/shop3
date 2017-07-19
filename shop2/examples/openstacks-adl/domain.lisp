;; openstacks, strips version
(in-package :shop2-user)
(in-package :shop2-user)

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
     
     (:action !make-product
      :parameters (?p - product)
      :precondition (and (not (made ?p))
                         (forall (?o - order)
                                 (imply (includes ?o ?p)
                                        (started ?o))))
      :effect (made ?p))

     (:action !start-order
      :parameters (?o - order ?avail ?new-avail - count)
      :precondition (and (waiting ?o)
                         (stacks-avail ?avail)
                         (next-count ?new-avail ?avail))
      :effect (and (not (waiting ?o))
                   (started ?o)
                   (not (stacks-avail ?avail))
                   (stacks-avail ?new-avail))
              )

     (:action !ship-order
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

     (:action !open-new-stack
      :parameters (?open ?new-open - count)
      :precondition (and (stacks-avail ?open)
                         (next-count ?open ?new-open))
      :effect (and (not (stacks-avail ?open))
                   (stacks-avail ?new-open)
                   ;; (increase (total-cost) 1)
                   )
              )

     (:method (do-orders)
       ((:goal (and . ?goals)))
       ((:ordered (assert-goals ?goals)
                  (do-goals))))

     (:method (assert-goals nil)
       ()
       ())

     (:method (assert-goals (?goal . ?goals))
       ()
       (:ordered (!!assert (goal ?goal))
                 (assert-goals ?goals))
      )

     ;; Stub to check and make sure planning works at all...
     (:method (do-goals)
       ((goal (shipped ?order))
        (not (shipped ?order)))
       (:ordered (do-order ?order) (do-goals))
       ()
       ((verify-orders)))

     (:method (verify-orders)
       ((goal (shipped ?order))
        (not (shipped ?order)))
       (:eval (error "complete plan does not satisfy goals.  State is:" shop2:*current-state*))
       ()
       ())

     (:method (do-order ?order)
       ()
       (:ordered (ensure-stack-available) (start-an-order ?order) (make-products ?order) (ship-products ?order)))

     (:method (ensure-stack-available)
       ((stacks-avail ?count)
        (not (= ?count n0)))
       ()
       ((stacks-avail ?count)
        (next-count ?count ?next))
       (!open-new-stack ?count ?next))

     (:method (start-an-order ?order)
       ((stacks-avail ?next)
        (next-count ?count ?next))
       ((!start-order ?order ?next ?count)))

     (:method (make-products ?order)
       ((includes ?order ?product)
        (not (made ?product)))
       (!make-product ?product)
       ;; done
       ()
       ())

     (:method (ship-products ?order)
       ((stacks-avail ?count)
        (next-count ?count ?next))
       ((!ship-order ?order ?count ?next))
       )

     (:op (!!assert ?fact)
      :add (?fact))

     (:op (!!delete ?fact)
      :delete (?fact))


     )
  )
