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

    (defdomain (arity-test-domain :noset t :redefine-ok t)
        (
         (:method (method ?_x ?_y)
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
         (:operator (!has-an-arg ?_x)
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

(def-fixture arity-domain ()
  (let ((dom
         (init-domain)))
    (&body)))

(def-fixture good-problem ()
  (progn
    (let (( shop2::*make-problem-silently* t ))
      (make-problem 'arity-match
                    '() '(method foo bar)))
    (&body)))

(def-fixture bad-problem ()
  (progn
    (let (( shop2::*make-problem-silently* t ))
      (make-problem 'arity-mismatch
                    '() '(method bar)))
    (&body)))

(def-fixture good-problem-op ()
  (progn
    (let (( shop2::*make-problem-silently* t ))
      (make-problem 'arity-match-op
                    '() '(!has-an-arg foo)))
    (&body)))

(def-fixture bad-problem-op ()
  (progn
    (let (( shop2::*make-problem-silently* t ))
      (make-problem 'arity-mismatch-op
                    '() '(!has-an-arg)))
    (&body)))

(def-fixture good-rest-problem-1 ()
  (progn
    (let (( shop2::*make-problem-silently* t ))
      (make-problem 'meta
                         '() '(metamethod foo bar)))
    (&body)))

(def-fixture good-rest-problem-2 ()
  (progn
    (let (( shop2::*make-problem-silently* t ))
      (make-problem 'meta-op
                    '() '(metamethodop foo)))
    (&body)))

(defmacro failed (body)
  `(fiveam:is (eq ,body 'fail)))

(defmacro unfailed (body)
  `(fiveam:is (not (eq ,body 'fail))))


(test arity-test
  (flet ((find-plans (problem &key verbose domain)
           (find-plans  problem :verbose verbose :domain domain)))
  (with-fixture arity-domain ()
    (with-fixture good-problem ()
      (unfailed 
         (find-plans 'arity-match :verbose 0 :domain dom)))
    (with-fixture bad-problem ()
      (signals shop2:task-arity-mismatch
          (find-plans 'arity-mismatch :domain dom :verbose 0)))
    (with-fixture good-problem-op ()
      (unfailed (find-plans 'arity-match-op :verbose 0 :domain dom)))
    (with-fixture bad-problem-op ()
      (signals shop2:task-arity-mismatch
               (find-plans 'arity-mismatch-op :domain dom :verbose 0)))
    (with-fixture good-rest-problem-1 ()
      (unfailed
       (find-plans 'meta :verbose 0 :domain dom)))
    (with-fixture good-rest-problem-2 ()
      (unfailed
       (find-plans 'meta-op :verbose 0 :domain dom))))))

(test ess-arity-test
  (flet ((find-plans (problem &key verbose domain)
           (find-plans-stack problem :verbose verbose :domain domain)))
  (with-fixture arity-domain ()
    (with-fixture good-problem ()
      (unfailed 
         (find-plans 'arity-match :verbose 0 :domain dom)))
    (with-fixture bad-problem ()
      (signals shop2:task-arity-mismatch
          (find-plans 'arity-mismatch :domain dom :verbose 0)))
    (with-fixture good-problem-op ()
      (unfailed (find-plans 'arity-match-op :verbose 0 :domain dom)))
    (with-fixture bad-problem-op ()
      (signals shop2:task-arity-mismatch
               (find-plans 'arity-mismatch-op :domain dom :verbose 0)))
    (with-fixture good-rest-problem-1 ()
      (unfailed
       (find-plans 'meta :verbose 0 :domain dom)))
    (with-fixture good-rest-problem-2 ()
      (unfailed
       (find-plans 'meta-op :verbose 0 :domain dom))))))



