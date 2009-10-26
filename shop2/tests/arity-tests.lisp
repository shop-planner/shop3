;;; -*- mode: common-lisp; coding: unix; -*-
;;;---------------------------------------------------------------------------
;;; Copyright 2008 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;;
;;;  This file made available together with the SHOP2 system, according to the
;;;  SHOP2 system's license
;;;
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This file is intended to supply a number of unit tests to
;;;    determine whether or not SHOP2's arity-checking is working.
;;;
;;;--------------------------------------------------------------------------

(in-package :arity-test)

(defun init-domain ()
  (let (( *defdomain-verbose* nil ))

    (defdomain (arity-test-domain :noset t)
        (
         (:method (method ?x ?y)
                  ()
                  ((!positive-op)))
         (:method (metamethod . ?args)
            ()
            (method . ?args))
         (:method (metamethodop . ?args)
            ()
            (!has-an-arg . ?args))
         (:operator (!positive-op)
                    ()                  ;no preconditions
                    ()
                    ((prop)))
         (:operator (!negative-op)
                    ()                  ;no preconditions
                    ((prop))
                    ())
         (:operator (!has-an-arg ?x)
                    ()
                    ()
                    ((prop)))
         (:operator (!add-protect)
                    ()
                    ()
                    ((:protection (prop))))
         (:operator (!remove-protect)
                    ()
                    ((:protection (prop)))
                    ())
         (:operator (!add-neg-protect)
                    ()
                    ()
                    ((:protection (not (prop)))))
         (:operator (!remove-neg-protect)
                    ()
                    ((:protection (not (prop))))
                    ())))))

(def-fixtures arity-domain ()
   (dom
    (init-domain)))

(def-fixtures good-problem (:uses arity-domain)
  (foo
   (let (( shop2::*make-problem-silently* t ))
     (make-problem 'arity-match
                   '() '(method foo bar)))))

(def-fixtures bad-problem (:uses arity-domain)
  (bar
   (let (( shop2::*make-problem-silently* t ))
     (make-problem 'arity-mismatch
                   '() '(method bar)))))

(def-fixtures good-problem-op (:uses arity-domain)
  (foo
   (let (( shop2::*make-problem-silently* t ))
     (make-problem 'arity-match-op
                   '() '(!has-an-arg foo)))))

(def-fixtures bad-problem-op (:uses arity-domain)
  (bar
   (let (( shop2::*make-problem-silently* t ))
     (make-problem 'arity-mismatch-op
                   '() '(!has-an-arg)))))

(def-fixtures good-rest-problem-1 ()
  (baz
   (let (( shop2::*make-problem-silently* t ))
     (make-problem 'meta
                   '() '(metamethod foo bar)))))

(def-fixtures good-rest-problem-2 ()
  (baz
   (let (( shop2::*make-problem-silently* t ))
     (make-problem 'meta-op
                   '() '(metamethodop foo)))))

(nst:def-values-criterion (failed () (retval &rest args))
    `(if (eq retval 'fail)
         (sift.nst::make-check-result)
         (sift.nst:emit-failure)))

(nst:def-values-criterion (unfailed () (retval &rest args) :declare ((ignore args)))
    `(if (eq retval 'fail)
         (sift.nst:emit-failure)
         (sift.nst::make-check-result)))

(def-test-group arity-test (arity-domain)
  (def-test  (method-arity-match :fixtures (good-problem))
      (unfailed)
    (locally (declare (special dom))
      (nth-value 0 (find-plans 'arity-match :verbose 0 :domain dom))))
  (def-test  (method-arity-mismatch :fixtures (bad-problem))
      (:err :type shop2:task-arity-mismatch)
        (locally (declare (special dom))
          (nth-value 0 (find-plans 'arity-mismatch :domain dom :verbose 0))))
  (def-test  (op-arity-match :fixtures (good-problem-op))
      (unfailed)
    (locally (declare (special dom))
      (nth-value 0 (find-plans 'arity-match-op :verbose 0 :domain dom))))
  (def-test  (op-arity-mismatch :fixtures (bad-problem-op))
      (:err :type shop2:task-arity-mismatch)
    (locally (declare (special dom))
      (nth-value 0 (find-plans 'arity-mismatch-op :domain dom :verbose 0))))
  (def-test  (method-rest-match :fixtures (good-rest-problem-1))
      (unfailed)
    (locally (declare (special dom))
      (nth-value 0 (find-plans 'meta :verbose 0 :domain dom))))
  (def-test  (op-rest-match :fixtures (good-rest-problem-2))
      (unfailed)
    (locally (declare (special dom))
      (nth-value 0 (find-plans 'meta-op :verbose 0 :domain dom))))
  )



