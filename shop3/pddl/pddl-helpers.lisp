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
(defpackage shop3-pddl-helpers
  (:use #:common-lisp #:iterate #:pddl-utils #:shop3)
  (:nicknames #:shop3.pddl.helpers #:shop2-pddl-helpers)
  (:shadowing-import-from #:shop3
                          #:domain-name #:make-problem #:domain
                          #:*validator-progname*)
  (:shadow #:problem-name)
  (:export #:typed-object-list->facts
           #:translate-openstacks-problem
           #:check-repair
           #:validate-replan
           #:make-divergence-operator))

(in-package #:shop3-pddl-helpers)

(defun typed-object-list->facts (list)
  "List *must be* canonicalized.  Returns a list of (<type> <instance>) facts."
  (loop :for (var dash type . nil) :on list :by #'cdddr
        :do (assert (eq dash '-))
        :collecting `(,type ,var)))


(defun do-all-substitutions (alist tree)
  (let ((new-tree (copy-tree tree)))         ;now we can operate destructively
    (iter (for (old . new) in alist)
      (setf new-tree (nsubst new old new-tree)))
    new-tree))

;;; Expects DIVERGENCE to be a list of this form: (:DIVERGENCE (:add|:delete <atom>)*)
(defun make-divergence-operator (divergence &key (package pddl-utils:*pddl-package*))
  (assert (and (eq  (first divergence) :divergence)
               (every #'(lambda (x) (or (eq (first x) :add)
                                        (eq (first x) :delete)))
                      (rest divergence))))
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
                                                              (shop3-domain *domain*))
  (declare (ignore keep))
  (validate-replan repaired-plan :shop-domain shop3-domain :package package :pddl-domain orig-domain
                   :pddl-problem orig-problem))
