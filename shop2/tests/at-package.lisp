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


(defpackage arity-test
  (:nicknames #:at #:arity-tests)
  (:use #:common-lisp #:shop2)
  (:import-from #:nst
                #:def-fixtures
                #:def-values-criterion
                #:def-test
                #:def-test-group))