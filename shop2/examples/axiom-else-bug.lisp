(in-package :shop-user)

;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Example domain and problems created to illustrate a problem
;;;    with backtracking over if-then-else axioms in shop2.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2004/01/23:rpg] Created.
;;;
;;;---------------------------------------------------------------------------


(defdomain test-axiom-else-branch
    (
     (:method (simple-test)
	      ((ok-precondition 12))
	      (!operator))
     (:method (test)
	      ((labeled-precondition 12))
	      (!operator))

     (:method (second-test)
	      ((check-constraint (<= :unbound :unbound))
	       (:unbound 12))
	      (!operator))

     (:method (test-patch)
	      ()
	      (:ordered
	       (!establish-bad-axiom)
	       (!check-bad-axiom)))

     (:operator (!operator)
		()
		()
		())
     (:- (ok-precondition ?x)
	 ( (call = ?x 12) )
	 ( (eval error "should not have reached this branch!") ))

     (:- (bad-axiom ?x)
	 ( (call = ?x 12) ))

     (:operator (!establish-bad-axiom)
		()
		;; delete
		()
		;; add
		(bad-axiom 22))

     (:operator (!check-bad-axiom)
		((bad-axiom 22))
		;; delete
		()
		;; add
		())

     (:- (labeled-precondition ?x)
	 branch1 ( (call = ?x 12) )
	 branch2 ( (eval error "should not have reached this branch!") ))

     (:- (check-constraint (<= ?x ?y))
	 arg1-unbound ( (:unbound ?x) )
	 arg2-unbound ( (:unbound ?y) )
	 tested ((call <= ?x ?y)))

     ;; I'm not entirely sure how to specify a simple ground clause
     (:- (:unbound :unbound) nil)
		     
     ))

;;; this will work fine --- just intended to show that my domain was
;;; well-formed.
(defproblem is-domain-ok
    NIL
  (simple-test))

;;; actually labeling the axiom branches was a red herring.  This will
;;; plan correctly, as it should.
(defproblem testing-labeled-axioms
    NIL
  (test))

;;; If the bug isn't fixed, this problem will cause shop2 to crash,
;;; because it will backtrack from checking the arg1-unbound branch of
;;; the check-constraint axiom, and will then try to call
;;; (<= :unbound :unbound).  This problem should NOT be plannable.
;;; [2004/02/02:rpg]
(defproblem testing-check-constraint
    NIL
  (second-test))

(defproblem this-should-work-if-patch-is-ok
    NIL
  (test-patch))

(defun test-axiom-else-bug ()
  "Tests the CUT behavior of axioms with if-then-else branches.
Should return T if behavior is correct."
  (and 
   (let ((retval (find-plans 'is-domain-ok)))
     (equalp retval '(((!OPERATOR) 1.0))))
   (let ((retval (find-plans 'testing-labeled-axioms)))
     (equalp retval '(((!OPERATOR) 1.0))))
   (let ((retval (find-plans 'testing-check-constraint)))
     (null retval))))


    