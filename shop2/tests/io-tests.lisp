;;; -*- mode: common-lisp; coding: unix; -*-
;;;---------------------------------------------------------------------------
;;; Copyright 2010 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;;
;;;  This file made available together with the SHOP2 system, according to the
;;;  SHOP2 system's license
;;;
;;;---------------------------------------------------------------------------
;;;
;;; Created [2010/05/19:rpg]
;;; File Description:
;;;
;;;    This file is intended to supply a number of unit tests to
;;;    determine whether or not SHOP2's processing of definitions is working.
;;;
;;;--------------------------------------------------------------------------

(in-package :arity-test)


(def-fixtures empty-domain ()
   (*domain* (make-instance 'domain)))

(def-fixtures method-def ()
  (meth '(:method (achieve-goals ?goals)
          ()
          ((assert-goals ?goals nil)
           (find-nomove) (add-new-goals) (find-movable) (move-block)))))

(def-test-group method-tests (empty-domain)
  (def-test (simple-method-parse :fixtures (method-def))
      (:equal '(:method (achieve-goals ?goals)
                placeholder
                ()
                '(:ordered (:task assert-goals ?goals nil)
                 (:task find-nomove) (:task add-new-goals) (:task find-movable) (:task move-block))))
    (let ((meth-def (shop2::process-method *domain* meth)))
      ;; there will be a gensym in the third position -- the name that is
      ;; automatically supplied
      (setf (nth 2 meth-def) 'placeholder)
      meth-def))
  )



