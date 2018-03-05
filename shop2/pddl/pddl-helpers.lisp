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
  (:shadow #:problem-name)
  (:export #:typed-object-list->facts
           #:translate-openstacks-problem
           #:check-repair
           #:validate-replan
           #:make-divergence-operator))

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
                    (uiop:Intern* '#:openstacks-sequencedstrips-ADL-included
                            package)
                    ;; state
                    (append
                     (pddl-utils:problem-state problem)
                     (typed-object-list->facts
                      (pddl-utils:canonicalize-types
                       (pddl-utils:problem-objects problem)))
                     `((:goal ,(pddl-utils:problem-goal problem))))
                    ;; tasks
                    `(,(uiop:intern* '#:plan :shop2-openstacks))))))

(defun do-all-substitutions (alist tree)
  (let ((new-tree (copy-tree tree)))         ;now we can operate destructively
    (iter (for (old . new) in alist)
      (setf new-tree (nsubst new old new-tree)))
    new-tree))

(defun make-divergence-operator (divergence &key (package pddl-utils:*pddl-package*))
  (assert (eq  (first divergence) :divergence))
  (let* ((effects 
           (iter (for (op literal) in (rest divergence))
             (collecting
              (ecase op
                (:add literal)
                (:delete `(not ,literal))))))
         (constants (remove-duplicates
                     (alexandria:mappend
                      #'(lambda (x) (destructuring-bind (op literal) x
                                      (assert (or (eq op :add) (eq op :delete)))
                                      (copy-list (rest literal))))
                      (rest divergence))))
         (variables (mapcar #'(lambda (c) (gentemp (concatenate 'string "?" (symbol-name c)) package)) constants))
         (constant-map (pairlis constants
                                variables)))
    (format t "~&~S" constant-map)
    (values
     (make-action (uiop:Intern* '#:divergence package)
                  variables             ;parameters
                  :effect (nsublis
                           constant-map`
                           (and
                            ,@effects)))
     constants)))

;;; this function is now obsolete, because of easier-to-use validate-replan.
(defun check-repair (orig-domain orig-problem repaired-plan &key (package *package*) keep
                                                              (shop2-domain *domain*))
  (declare (ignore keep))
  (validate-replan repaired-plan :shop-domain shop2-domain :package package :pddl-domain orig-domain
                   :pddl-problem orig-problem))