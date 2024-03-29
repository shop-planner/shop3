(in-package :shop3-user)

(fiveam:def-suite misc-tests :in arity-test:all-shop3-internal-tests)

(fiveam:in-suite misc-tests)

(fiveam:test check-stack-overflow-fix
  (flet ((find-plans (problem)
           (find-plans  problem :verbose 0)))
    (let ((prob-name (gentemp "PROB")))
      (fiveam:is
       (equal
        (catch 'bad-problem-arg
          (handler-bind
              ((error
                 #'(lambda (c)
                     (throw 'bad-problem-arg
                       (with-output-to-string (x)
                         (format x "~a" c))))))
            (find-plans prob-name)))
        (format nil "No such problem: ~a" prob-name))))))

(fiveam:test ess-check-stack-overflow-fix
  (flet ((find-plans (problem)
           (find-plans-stack problem :verbose 0)))
    (let ((prob-name (gentemp "PROB")))
      (fiveam:is
       (equal
        (catch 'bad-problem-arg
          (handler-bind
              ((error
                 #'(lambda (c)
                     (throw 'bad-problem-arg
                       (with-output-to-string (x)
                         (format x "~a" c))))))
            (find-plans prob-name)))
        (format nil "No such problem: ~a" prob-name))))))

(define-condition %bad-backtrack-cond (error)
  ()
  (:report (lambda (c s)
             (declare (ignorable c))
             (format s "Encountered bad backtrack condition in SHOP3."))))

(5am:def-fixture test-backtrack-domain ()
  (progn
    (let ((shop3:*define-silently* t))
      (defdomain (test-backtrack :redefine-ok t)
          ((:method (example-with-backtrack ?x)
             ((foo ?x)
              (bar ?_y))
             ())
           (:- (= ?x ?x)
               ())
           (:- (foo ?x)
               ((= ?x 2)
                (eval (eql ?x 2)))
               ((eval (error '%bad-backtrack-cond)))))))
    (&body)))

(5am:def-fixture test-backtrack-problem ()
  (progn
    (let ((shop3:*define-silently* t))
        (make-problem '(check-bad-backtrack-domain :redefine-ok t) 'test-backtrack
                      '((bar 22))
                      '(example-with-backtrack 2)))
    (&body)))

(5am:def-fixture test-bad-backtrack-problem ()
  (let ((shop3:*define-silently* t)
        #+allegro(excl:*redefinition-warnings* nil))
    (make-problem 'bad-backtrack-problem 'test-backtrack
                nil
                '(example-with-backtrack 2))
    (&body)))

;;; this just checks to make sure that the domain works: it doesn't actually
;;; check the bug.
(fiveam:test check-bad-backtrack-domain
  (5am:with-fixture test-backtrack-domain ()
    (5am:with-fixture test-backtrack-problem ()
      (flet ((find-plans (problem)
               (find-plans  problem :verbose 0)))
        (unwind-protect
             (fiveam:is-true (find-plans 'check-bad-backtrack-domain))
          (shop3:delete-problem 'check-bad-backtrack-domain)
          (shop3:delete-domain 'test-backtrack))))))

(fiveam:test ess-check-bad-backtrack-domain
  (5am:with-fixture test-backtrack-domain ()
    (5am:with-fixture test-backtrack-problem ()
      (flet ((find-plans (problem)
               (find-plans-stack problem)))
        (unwind-protect
             (fiveam:is-true (find-plans 'check-bad-backtrack-domain))

          (shop3:delete-problem 'check-bad-backtrack-domain)
          (shop3:delete-domain 'test-backtrack))))))

;;; To verify SHOP2 ticket:261 (https://svn.sift.info:3333/trac/shop2/ticket/261) do the following:
(fiveam:test bad-backtrack-case
  (5am:with-fixture test-backtrack-domain ()
    (5am:with-fixture test-bad-backtrack-problem ()
      (unwind-protect
       (flet ((find-plans (problem &key verbose)
                (find-plans  problem :verbose verbose)))
         (fiveam:is-false
          (find-plans 'bad-backtrack-problem :verbose 0))
         (let* ((domain (find-domain 'test-backtrack))
                (state (shop3::make-initial-state domain :mixed '((bar 22)))))
           (fiveam:is
            (equalp
             `((,(shop3.unifier::make-binding '?x 2)
                ,(shop3.unifier::make-binding '?_y 22)))
             (find-satisfiers '((foo ?x)(bar ?_y)) state :just-one nil :level 0 :domain domain))))
         (let* ((domain (find-domain 'test-backtrack))
                (state (shop3::make-initial-state domain :mixed nil)))
           (fiveam:is-false
            (shop3.theorem-prover:query '((foo ?x)(bar ?_y)) state :domain domain))))

        (shop3:delete-problem 'bad-backtrack-problem)
        (shop3:delete-domain 'test-backtrack)))))


(fiveam:test ess-bad-backtrack-case
  (5am:with-fixture test-backtrack-domain ()
    (5am:with-fixture test-bad-backtrack-problem ()
      (unwind-protect
           (flet ((find-plans (problem &key verbose)
                    (find-plans-stack problem :verbose verbose)))
             (fiveam:is-false
              (find-plans 'bad-backtrack-problem :verbose 0))
             (let* ((domain (find-domain 'test-backtrack))
                    (state (shop3::make-initial-state domain :mixed '((bar 22)))))
               (fiveam:is
                (equalp
                 `((,(shop3.unifier::make-binding '?x 2)
                    ,(shop3.unifier::make-binding '?_y 22)))
                 (find-satisfiers '((foo ?x)(bar ?_y)) state :just-one nil :level 0 :domain domain))))
             (let* ((domain (find-domain 'test-backtrack))
                    (state (shop3::make-initial-state domain :mixed nil)))
               (fiveam:is-false
                (shop3.theorem-prover:query '((foo ?x)(bar ?_y)) state :domain domain))))

        (shop3:delete-problem 'bad-backtrack-problem)
        (shop3:delete-domain 'test-backtrack)))))
