(in-package #:shop-user)

(defpackage :blocks-test-names)

(fiveam:def-suite blocks-plan-validation
  :description "Check to make sure that SHOP generates valid plans for the blocks domain.
One user discovered that the domain could yield invalid plans for unsolvable problems,
instead of failing.")

(defmacro new-blocks-test (name &body body)
  `(fiveam:test (,name :suite blocks-plan-validation)
    (define-blocks-domain)
     ,@body))

(new-blocks-test fail-on-invalid
  (make-problem 'invalid-blocks-problem 'blocks-normal
                '((arm-empty)
                  (block b1)
                  (block b2)
                  (block b3)
                  (on-table b1) (on b2 b1)
                  (on-table b3)
                  )
                '((achieve-goals ((on-table b2)
                                  (on b1 b3))
                   ))
                )
  (fiveam:is-false (find-plans 'invalid-blocks-problem))
  (fiveam:is-false (find-plans-stack 'invalid-blocks-problem)))

(new-blocks-test fixed-blocks-problem
    (make-problem 'problem3-revised 'blocks-normal
                  '((arm-empty)
                    (block b1)
                    (block b2)
                    (block b3)
                    (on-table b1) (on b2 b1)
                    (on-table b3)
                    (clear b3)
                    (clear b2)
                    )
                  '((achieve-goals ((on-table b2)
                                    (on b1 b3))
                     ))
                  )
  (fiveam:is-true (find-plans 'problem3-revised))
  (fiveam:is-true (find-plans-stack 'problem3-revised)))

(new-blocks-test contradictory
  (make-problem 'contradictory 'blocks-normal
                '((arm-empty)
                  (block b1)
                  (block b2)
                  (block b3)
                  (on-table b1) (on b2 b1)
                  (clear b2)
                  (clear b3)
                  (on-table b3)
                  )
                '((achieve-goals ((on-table b2)
                                  (on b1 b3)
                                  (on b3 b1)
                                  )
                   ))
                )
  (fiveam:is-false (find-plans 'contradictory))
  (fiveam:is-false (find-plans-stack 'contradictory)))
