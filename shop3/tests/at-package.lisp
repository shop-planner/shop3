;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Package declaration for misnamed arity-tests.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2010/05/19:rpg] modified; block comment added.
;;;
;;;---------------------------------------------------------------------------
(in-package :common-lisp-user)

(defpackage arity-test
  (:nicknames #:at #:arity-tests)
  (:use #:common-lisp #:shop3)
  (:export #:all-shop3-internal-tests)
  (:import-from #:fiveam
                #:def-fixture
                #:with-fixture
                #:test
                #:signals
                #:is
                #:def-suite
                #:in-suite))
