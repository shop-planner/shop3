;; openstacks, strips version

(define (domain openstacks-sequencedstrips-ADL-nocosts)
  (:requirements :typing :adl)
  (:types order product count)
  (:predicates (includes ?o - order ?p - product)
               (waiting ?o - order)
               (started ?o - order)
               (shipped ?o - order)
               (made ?p - product)
               (stacks-avail ?s - count)
               (next-count ?s ?ns - count))

  ;; modified so that the precondition is that none of the orders may
  ;; be waiting, which means it could be "shipped", which makes plan
  ;; repair (and the construction of plan repair problems), easier.
  (:action make-product
    :parameters (?p - product)
    :precondition (and (not (made ?p))
                       (forall (?o - order)
                               (imply (includes ?o ?p)
                                      (not (waiting ?o)))))
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
                 (stacks-avail ?new-open))
    )

  )
