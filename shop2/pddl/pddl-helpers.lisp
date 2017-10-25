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
                    '(shop2-openstacks::plan)))))

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

;;; FIXME: later make KEEP default to NIL
(defun check-repair (orig-domain orig-problem repaired-plan &key (package *package*) (keep t)
                                                              (shop2-domain *domain*))
  (let ((pddl-utils:*pddl-package* package))
    (let* ((domain (if (or (stringp orig-domain) (pathnamep orig-domain))
                       (read-pddl-file orig-domain)
                       ;; don't need to copy here, because INSERT-DOMAIN-ACTIONS takes care of that
                       orig-domain))
           (divergence-pos (position :divergence repaired-plan
                                     :key #'(lambda (x) (and (listp x) (first x)))))
           (divergence (nth divergence-pos repaired-plan)))
      (multiple-value-bind (divergence-op-def divergence-args)
          (make-divergence-operator divergence)
        (let ((repaired-plan (let ((new (copy-list repaired-plan)))
                               (setf (nth divergence-pos new)
                                     (cons (uiop:Intern* '#:divergence package)
                                           divergence-args))
                               new))
              (problem-file
                (if (or (stringp orig-problem) (pathnamep orig-problem))
                    orig-problem
                    (uiop:with-temporary-file (:stream str :pathname path :keep t)
                      (pprint-pddl orig-problem str)
                      path)))
              (domain-file
                (uiop:with-temporary-file (:stream str :pathname path :keep t)
                  (let ((expanded 
                          (insert-domain-actions domain (list divergence-op-def))))
                    (pprint-pddl expanded str))
                  path)))
          (unwind-protect
               (values
                (validate-plan repaired-plan domain-file problem-file :shop2-domain shop2-domain)
                problem-file
                domain-file)
            (unless keep
              (uiop:delete-file-if-exists domain-file)
              (unless (or (stringp orig-problem) (pathnamep orig-problem))
                (uiop:delete-file-if-exists problem-file)))))))))