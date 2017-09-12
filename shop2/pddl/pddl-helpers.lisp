;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This file provides utilities for translating PDDL domains,
;;;    problems, etc. into SHOP2 equivalents.  By putting this outside
;;;    SHOP2, we avoid having SHOP2 have to load the PDDL-UTILS.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2017/09/12:rpg] Created.
;;;
;;;---------------------------------------------------------------------------
(defpackage shop2-pddl-helpers
  (:use #:common-lisp #:iterate #:pddl-utils #:shop2)
  (:shadowing-import-from #:shop2
                          #:domain-name #:make-problem #:domain)
  (:export #:typed-object-list->facts
           #:translate-openstacks-problem))

(in-package #:shop2-pddl-helpers)

(defun typed-object-list->facts (list)
  "List *must be* canonicalized.  Returns a list of (<type> <instance>) facts."
  (loop :for (var dash type . nil) :on list :by #'cdddr
        :do (assert (eq dash '-))
        :collecting `(,type ,var)))

(defun translate-openstacks-problem (problem-file &key (package :shop2-openstacks) )
  (let ((pddl-utils:*pddl-package* package))
    (let ((problem 
            (pddl-utils:read-pddl-file problem-file)))
      (make-problem (pddl-utils:problem-name problem)
                    (intern '#:openstacks-sequencedstrips-ADL-included
                            package)
                    ;; state
                    (append
                     (pddl-utils:problem-state problem)
                     (typed-object-list->facts
                      (pddl-utils:canonicalize-types
                       (pddl-utils:problem-objects problem)))
                     `((:goal ,(pddl-utils:problem-goal problem))))
                    ;; tasks
                    '(shop2-openstacks::plan)))))


