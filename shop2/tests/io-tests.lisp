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

(def-fixtures complex-method-def ()
  (meth '(:method (find-movable)
         (:first (clear ?x) (not (dont-move ?x))
                 (goal (on-table ?x)) (not (put-on-table ?x)))
                                        ; Decomposition
         ((!assert ((put-on-table ?x))) (find-movable))

         (:first (clear ?x) (not (dont-move ?x)) (goal (on ?x ?y))
                 (not (stack-on-block ?x ?y)) (dont-move ?y) (clear ?y))
                                        ;Decomposition
         ((!assert ((stack-on-block ?x ?y))) (find-movable))

         nil
         nil)))

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
  (def-test (compound-method-parse :fixtures (complex-method-def))
      (:equal '(:METHOD (FIND-MOVABLE)
                placeholder
                (:FIRST (CLEAR ?X) (NOT (DONT-MOVE ?X)) (GOAL (ON-TABLE ?X)) (NOT (PUT-ON-TABLE ?X)))
                '(:ORDERED (:TASK !ASSERT ((PUT-ON-TABLE ?X))) (:TASK FIND-MOVABLE))
                placeholder
                (:FIRST (CLEAR ?X) (NOT (DONT-MOVE ?X)) (GOAL (ON ?X ?Y)) (NOT (STACK-ON-BLOCK ?X ?Y)) (DONT-MOVE ?Y) (CLEAR ?Y))
                '(:ORDERED (:TASK !ASSERT ((STACK-ON-BLOCK ?X ?Y))) (:TASK FIND-MOVABLE))
                placeholder NIL '(:ORDERED (:TASK SHOP2::!!INOP))))
    (let ((meth-def (shop2::process-method *domain* meth)))
      ;; replace all the gensyms
      (subst-if 'placeholder
                #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                meth-def)))
  )



