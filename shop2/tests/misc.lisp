(in-package :shop2-user)

(fiveam:def-suite misc-tests)

(fiveam:in-suite misc-tests)

(fiveam:test check-stack-overflow-fix
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
      (format nil "No such problem: ~a" prob-name)))))

(define-condition %bad-backtrack-cond (error)
  ()
  (:report (lambda (c s)
             (format s "Encountered bad backtrack condition in SHOP2."))))

(defun test-backtrack-domain ()
  (defdomain test-backtrack
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

(defun bad-backtrack-problem ()
  (make-problem 'bad-backtrack-problem 'test-backtrack
                nil
                '(example-with-backtrack 2)))

(fiveam:test check-bad-backtrack-domain
  (test-backtrack-domain)
  (make-problem 'check-bad-backtrack-domain 'test-backtrack
                '((bar 22))
                '(example-with-backtrack 2))
  (unwind-protect
       (fiveam:is-true (find-plans 'check-bad-backtrack-domain))
    (shop2:delete-problem 'check-bad-backtrack-domain)
    ;; FIXME: there should be a DELETE-DOMAIN, but there isn't...
    ))
  
;;; To verify SHOP2 ticket:261 (https://svn.sift.info:3333/trac/shop2/ticket/261) do the following:
#+nil
(progn
  (test-backtrack-domain)
  (bad-backtrack-problem)
  (find-plans 'bad-backtrack-problem))
;;; this will raise an error as we somehow backtrack into the error call, which
;;; should be unreachable.

      
