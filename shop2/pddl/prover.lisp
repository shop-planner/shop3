;;; -*- Mode: common-lisp; package: shop2; -*-
;;;
;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;; 
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;; 
;;; The Original Code is SHOP2.  
;;; 
;;; The Initial Developer of the Original Code is the University of
;;; Maryland. Portions created by the Initial Developer are Copyright (C)
;;; 2002,2003 the Initial Developer. All Rights Reserved.
;;;
;;; Additional developments made by Robert P. Goldman, and Ugur Kuter.
;;; Portions created by Drs. Goldman and Kuter are Copyright (C)
;;; 2017 SIFT, LLC.  These additions and modifications are also
;;; available under the MPL/GPL/LGPL licensing terms.
;;; 
;;; 
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.
;;; ----------------------------------------------------------------------
;;; FIXME: this GPR statement is wrong.
;;; Smart Information Flow Technologies Copyright 2006-2007 Unpublished work
;;; 
;;; GOVERNMENT PURPOSE RIGHTS
;;; 
;;; Contract No.         FA8650-06-C-7606, 
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;; 
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.

(in-package :shop2.theorem-prover)

(def-logical-keyword (forall (domain shop2::universal-preconditions-mixin))
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (pddl-satisfiers-for-forall domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (exists (domain shop2::existential-preconditions-mixin))
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (pddl-satisfiers-for-exists domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))


;;; FIXME: Actually, the attachment of this inference rule is
;;; incorrect.  negative-preconditions-mixin only enables negated
;;; *literals*, not negated *expressions*, which require
;;; disjunctive-preconditions
(def-logical-keyword (not (domain shop2::negative-preconditions-mixin))
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (pddl-satisfiers-for-not domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (imply (domain shop2::disjunctive-preconditions-mixin))
    (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
        (pddl-satisfiers-for-imply domain (cdr goal) other-goals
                                   state bindings (1+ level) just1 dependencies-in)))



(defun pddl-satisfiers-for-forall (domain arguments other-goals
                                   state bindings newlevel just1 dependencies-in)
  (assert (= (length arguments) 3))
  (let* ((bounds (second arguments))
         (conditions (third arguments))
         ;; in our PDDL FORALLs, the bounds are all going to be static type
         ;; predicates, which don't need their dependencies recorded.
         (mgu2 (let ((*record-dependencies-p* nil))
                 (find-satisfiers bounds state nil (1+ newlevel) :domain domain)))
         depends)
    ;; now we find one satisfied disjunct...
    (dolist (m2 mgu2)
      ;; but we find ALL the answers for that disjunct...
      (multiple-value-bind (new-answers new-depends)
          (find-satisfiers (apply-substitution conditions m2)
                           state t (1+ newlevel) :domain domain)
        ;; all of the conditions must pass
        (unless new-answers
          (return-from pddl-satisfiers-for-forall nil))
        ;; there should be no free variables bound here -- actually, because of the syntax of PDDL methods, this may not be true....
        ;; (assert (equal new-answers '(nil)))
        (when *record-dependencies-p*
          (setf depends 
                (rd-union (first new-depends) depends)))))
    (seek-satisfiers other-goals state bindings newlevel just1 :domain domain
                                                               :dependencies (when *record-dependencies-p* (rd-union depends dependencies-in)))))

;; FIXME: we should be stripping out bindings for the existential variable(s)
(defun pddl-satisfiers-for-exists (domain arguments other-goals
                                   state bindings newlevel just1 dependencies-in)
  (let* ((bounds (second arguments))
         (conditions (third arguments))
         ;; in our PDDL EXISTS, the bounds are all going to be static type
         ;; predicates, which don't need their dependencies recorded.
         (mgu2 (let ((*record-dependencies-p* nil))
                 (find-satisfiers bounds state nil 0 :domain domain)))
         dependencies)
    (iter (for m2 in mgu2)
      (multiple-value-bind (answers new-depends)
          (seek-satisfiers (apply-substitution conditions m2)
                           state bindings 0 t :domain domain)
        (when answers
          (when *record-dependencies-p*
            (setf dependencies
                  ;; (first new-depends) because there should be only one answer to the exists query --
                  ;; we passed t to JUST1 in SEEK-SATISFIERS
                  (union (first new-depends) dependencies-in :key #'(lambda (x) (rd-prop x)) :test 'equalp)))
          (return t))
        (finally (return-from pddl-satisfiers-for-exists nil))))

    ;; FIXME: the binding list in here should have the binding for the
    ;; variables (first arguments) ripped out, since they should not
    ;; be visible outside the bounds.  This isn't really important,
    ;; though.  Also, note that the binding for the existential variable(s) will always be vacuous (i.e., another variable).

    ;; Satisfy other goals
    (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies)))

(defun vars-boundp (tree &optional acc)
  (cond ((variablep tree) (member tree acc))
        ((atom tree)                    ; atom, but not a variable
         t)
        ((listp tree)
         (cond ((or (eq (first tree) 'forall)
                    (eq (first tree) 'exists))
                (destructuring-bind (keyword var expr) tree
                  (declare (ignore keyword))
                  (let ((new-acc (cons var acc)))
                    (vars-boundp expr new-acc))))
               (t
                (and (vars-boundp (car tree) acc)
                     (every #'(lambda (sexp) (vars-boundp sexp acc))
                            (rest tree))))))
        ;; FIXME: SBCL knows the following is impossible. I don't know
        ;; why -- maybe all values in CL are either atoms or lists?  I
        ;; think actually that's true here, because TREE is always a
        ;; list.  But heck if I know how SBCL knows.  [2018/04/12:rpg]
         (t (error "Unexpected value in tree: ~S" tree))))


(defun pddl-satisfiers-for-not (domain arguments other-goals
                                           state bindings newlevel just1 dependencies-in)
  (let ((argument (first arguments)))
    (when *record-dependencies-p*
      (unless (or (groundp argument) (vars-boundp argument))
        (error "Negation expression in PDDL should not contain any unbound variables: ~A." `(not ,argument))))
    ;; we just want to see if (CDR GOAL1) is satisfiable, so last arg is T
    (cond
      ((let ((*record-dependencies-p* nil))
         (seek-satisfiers arguments state nil newlevel t :domain domain))
       ;; the negation is falsified
       nil)
      (t
       (let (newdep)
         (when (and *record-dependencies-p* (groundp argument))
           (setf newdep
                 (make-raw-depend
                  :est
                  (dependency-for-negation argument state)
                  :prop `(not ,argument))))
         (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies (if newdep (cons newdep dependencies-in) dependencies-in)))))))


(defun pddl-satisfiers-for-imply (domain arguments other-goals
                                   state bindings newlevel just1 dependencies-in)
  (assert (= (length arguments) 2))
  (seek-satisfiers `((or (not ,(first arguments)) ,(second arguments)) ,@other-goals) state bindings newlevel just1
                   :domain domain :dependencies dependencies-in))
