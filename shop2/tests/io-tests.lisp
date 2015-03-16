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


(def-fixture empty-domain ()
  (let ((*domain* (make-instance 'domain)))
    (&body)))

(def-fixture method-def ()
  (let ((meth '(:method (achieve-goals ?goals)
          ()
          ((assert-goals ?goals nil)
           (find-nomove) (add-new-goals) (find-movable) (move-block)))))
    (&body)))

(def-fixture complex-method-def ()
  (let ((meth '(:method (find-movable)
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
    (&body)))

(test method-tests
  (with-fixture empty-domain ()
    (with-fixture method-def ()
      (is (equal (let ((meth-def (shop2::process-method *domain* meth)))
                   ;; there will be a gensym in the third position -- the name that is
                   ;; automatically supplied
                   (setf (nth 2 meth-def) 'placeholder)
                   meth-def)
                 '(:method (achieve-goals ?goals)
                   placeholder
                   ()
                   '(:ordered (:task assert-goals ?goals nil)
                     (:task find-nomove) (:task add-new-goals) (:task find-movable) (:task move-block))))))
    (with-fixture complex-method-def ()
      (is
       (equal (let ((meth-def (shop2::process-method *domain* meth)))
                ;; replace all the gensyms
                (subst-if 'placeholder
                          #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                          meth-def))
              '(:method (find-movable)
                placeholder
                (:first (clear ?x) (not (dont-move ?x)) (goal (on-table ?x)) (not (put-on-table ?x)))
                '(:ordered (:task !assert ((put-on-table ?x))) (:task find-movable))
                placeholder
                (:first (clear ?x) (not (dont-move ?x)) (goal (on ?x ?y)) (not (stack-on-block ?x ?y)) (dont-move ?y) (clear ?y))
                '(:ordered (:task !assert ((stack-on-block ?x ?y))) (:task find-movable))
                placeholder nil '(:ordered (:task shop2::!!inop))))))))

(test check-problem-deletion
  (make-problem 'problem-for-deletion-test
                '((foo x) (bar y))
                '(achieve (bar x)))
  (fiveam:is-true (find-problem 'problem-for-deletion-test))
  (delete-problem 'problem-for-deletion-test)
  (fiveam:is-false (find-problem 'problem-for-deletion-test nil)))




