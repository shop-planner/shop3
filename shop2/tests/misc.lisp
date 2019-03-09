(in-package :shop2-user)

(fiveam:def-suite misc-tests)

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
             (format s "Encountered bad backtrack condition in SHOP2."))))

(defun test-backtrack-domain ()
  (let ((shop2:*define-silently* t))
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
             ((eval (error '%bad-backtrack-cond))))))))

(defun bad-backtrack-problem ()
  (let ((shop2:*define-silently* t)
        #+allegro(excl:*redefinition-warnings* nil))
    (make-problem 'bad-backtrack-problem 'test-backtrack
                nil
                '(example-with-backtrack 2))))

;;; this just checks to make sure that the domain works: it doesn't actually
;;; check the bug.
(fiveam:test check-bad-backtrack-domain
  (flet ((find-plans (problem)
           (find-plans  problem :verbose 0)))
    (test-backtrack-domain)
    (let (#+allegro(excl:*redefinition-warnings* nil)
          (shop2:*define-silently* t))
     (make-problem 'check-bad-backtrack-domain 'test-backtrack
                   '((bar 22))
                   '(example-with-backtrack 2)))
    (unwind-protect
         (fiveam:is-true (find-plans 'check-bad-backtrack-domain))
      (shop2:delete-problem 'check-bad-backtrack-domain)
      (shop2:delete-domain 'test-backtrack))))

(fiveam:test ess-check-bad-backtrack-domain
  (flet ((find-plans (problem)
           (find-plans-stack problem)))
    (test-backtrack-domain)
    (let (#+allegro(excl:*redefinition-warnings* nil)
          (shop2:*define-silently* t))
      (make-problem 'check-bad-backtrack-domain 'test-backtrack
                    '((bar 22))
                    '(example-with-backtrack 2)))
    (unwind-protect
         (fiveam:is-true (find-plans 'check-bad-backtrack-domain))
      (shop2:delete-problem 'check-bad-backtrack-domain)
      (shop2:delete-domain 'test-backtrack))))
  
;;; To verify SHOP2 ticket:261 (https://svn.sift.info:3333/trac/shop2/ticket/261) do the following:
(fiveam:test bad-backtrack-case
  (flet ((find-plans (problem &key verbose)
           (find-plans  problem :verbose verbose)))
    (test-backtrack-domain)
    (bad-backtrack-problem)
    (fiveam:is-false
     (find-plans 'bad-backtrack-problem :verbose 0))
    (let* ((domain (find-domain 'test-backtrack))
           (state (shop2::make-initial-state domain :mixed '((bar 22)))))
      (fiveam:is
       (equalp
        `((,(shop2.unifier::make-binding '?x 2)
           ,(shop2.unifier::make-binding '?_y 22)))
        (find-satisfiers '((foo ?x)(bar ?_y)) state nil 0 :domain domain))))
    (let* ((domain (find-domain 'test-backtrack))
           (state (shop2::make-initial-state domain :mixed nil)))
      (fiveam:is-false
       (shop2.theorem-prover:query '((foo ?x)(bar ?_y)) state :domain domain)))))


(fiveam:test ess-bad-backtrack-case
  (flet ((find-plans (problem &key verbose)
           (find-plans-stack problem :verbose verbose)))
    (test-backtrack-domain)
    (bad-backtrack-problem)
    (fiveam:is-false
     (find-plans 'bad-backtrack-problem :verbose 0))
    (let* ((domain (find-domain 'test-backtrack))
           (state (shop2::make-initial-state domain :mixed '((bar 22)))))
      (fiveam:is
       (equalp
        `((,(shop2.unifier::make-binding '?x 2)
           ,(shop2.unifier::make-binding '?_y 22)))
        (find-satisfiers '((foo ?x)(bar ?_y)) state nil 0 :domain domain))))
    (let* ((domain (find-domain 'test-backtrack))
           (state (shop2::make-initial-state domain :mixed nil)))
      (fiveam:is-false
       (shop2.theorem-prover:query '((foo ?x)(bar ?_y)) state :domain domain)))))
