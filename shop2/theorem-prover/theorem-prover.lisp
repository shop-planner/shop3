;;; -*- Mode: common-lisp; package: shop2.theorem-prover; -*-
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
;;; Additional developments made by Robert P. Goldman, John Maraist.
;;; Portions created by Drs. Goldman and Maraist are Copyright (C)
;;; 2004-2007 SIFT, LLC.  These additions and modifications are also
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

;;; ------------------------------------------------------------------------
;;; Theorem prover
;;; ------------------------------------------------------------------------

;;; This macro is defined before find-satisfiers because it is invoked by
;;;  find-satisfiers and some compilers want all macros to be defined before
;;;  any code which invokes them.
(defmacro seek-satisfiers (goals state bindings level just1
                           &key (domain nil domain-supp-p)
                             dependencies)
  "Find satisfying assignments to variables in GOALS, using STATE and
BINDINGS.  Find just one assignment if JUST1 is non-NIL.
   BINDINGS is *NOT* a list of binding structures! It is a list made
up of either variables or the values assigned to those variables.  This
list will be harvested into a list of binding lists by FIND-SATISFIERS.
   Returns a list of values for the variables \(or the variables
themselves, if unbound\).  Will also return a set of dependencies, which
will be computed if *RECORD-DEPENDENCIES-P* is non-NIL."
  (let ((d (gensym)))
    `(let ((,d (if ,domain-supp-p ,domain *domain*)))
       (if (null ,goals)
           (values (list ,bindings) (list ,dependencies))
           (real-seek-satisfiers ,d ,goals ,state
                                 ,bindings ,level ,just1 ,dependencies)))))

(defgeneric query (goals state &key just-one domain)
  (:documentation 
   "More convenient top-level alternative to FIND-SATISFIERS.
Manages optional arguments and ensures that the variable property
is properly set in GOALS.")
  (:method (goals state &key just-one (domain *domain*))
    (set-variable-property domain goals)
    (find-satisfiers goals state just-one 0 :domain domain)))

;;; FIND-SATISFIERS returns a list of all satisfiers of GOALS in AXIOMS
(locally (declare #+sbcl (sb-ext:muffle-conditions sb-int:simple-style-warning))
  ;; this is necessary because SBCL (correctly) hates the use of OPTIONAL and
  ;; KEY together.

  (defun find-satisfiers (goals state &optional just-one (level 0)
                                       &key (domain *domain*))
           "Find and return a list of binding lists that represents the answer to
goals \(a list representation of a deductive query\), where
state provides the database.  level is a non-negative integer indicating the
current depth of inference, used in debugging prints, and
just-one is a boolean indicating whether you want just one answer (non-nil)
or all answers (nil)."
           (setf *current-state* state)
           (let ((*state* state))
             (declare (special *state*))
             (let* ((sought-goals
                     (cond
                      ((eq (first goals) :first) (rest goals))
                      ((eq (first goals) :sort-by)
                       (if (= (length goals) 3)
                           (third goals)
                         (fourth goals)))
                      ((eq (first goals) :random)
                       (second goals))
                      (t goals)))
                    (variables (extract-variables sought-goals)))
               (multiple-value-bind (answers depends)
                   ;; if the preconditions use :SORT-BY, we assume that the preconditions are
                   ;; about heuristic choice, not correctness, and don't store causal links
                   ;; for the :SORT-BY expression. [2017/08/01:rpg]
                   (let ((*record-dependencies-p* (if (eq (first goals) :sort-by) nil
                                                      *record-dependencies-p*)))
                    (seek-satisfiers sought-goals state variables level
                                     (or (eq (first goals) :first) just-one)
                                     :domain domain))
                 (let* ((satisfiers (mapcar #'(lambda (bindings) (make-binding-list variables bindings))
                                           answers))
                       (num-satisfiers (length satisfiers)))
                                        ;(format t "~%sat: ~s~%" satisfiers) ;***

                   (cond ((eq (first goals) :sort-by)
                          (values 
                           (sort satisfiers
                                 (if (= (length goals) 3) #'<
                                     (eval (third goals)))
                                 :key #'(lambda (sat)
                                          (eval (apply-substitution (second goals) sat))))
                           (when *record-dependencies-p* (make-list num-satisfiers :initial-element nil)))
                          ;; see earlier comment about SORT-BY.
                          #+ignore(if *record-dependencies-p*
                              (let* ((double-list (pairlis satisfiers
                                                          depends))
                                     (sorted
                                      (sort double-list
                                            (if (= (length goals) 3) #'<
                                              (eval (third goals)))
                                            :key #'(lambda (double)
                                                     (eval (apply-substitution (second goals) (car double)))))))
                                (values (mapcar #'car sorted) (mapcar #'cdr sorted)))
                              ;; else
                              (sort satisfiers
                                    (if (= (length goals) 3) #'<
                                      (eval (third goals)))
                                    :key #'(lambda (sat)
                                             (eval (apply-substitution (second goals) sat))))))
                         ((eq (first goals) :random)
                          (let* ((n (length satisfiers))
                                 (r (random n)))
                            (if (> n 0)
                                (values
                                 (list
                                  (nth r satisfiers))
                                 (nth r depends))
                              (values satisfiers depends))))
                         (t
                          (values satisfiers depends)))))))))

;;; EXTRACT-VARIABLES returns a list of all of the variables in EXPR
(defun extract-variables (expr)
  (cond
   ((variablep expr) (list expr))
   ((and (consp expr) (not (eql (car expr) 'forall)))
    (shop-union (extract-variables (car expr))
                (extract-variables (cdr expr))))))

;;; ; REAL-SEEK-SATISFIERS is the workhorse for FIND-SATISFIERS.  For each
;;; ; proof of GOALS from AXIOMS and states, it returns the values of GOAL's variables
;;; ; that make the proof true.  Here are the other parameters:
;;; ;  - STATE: the state of the world in which the query is to be evaluated.
;;; ;  - BINDINGS is the variable bindings at the current node of the proof tree
;;; ;  - LEVEL is the current search depth, for use in debugging printout
;;; ;  - JUST1 is a flag saying whether to return after finding the 1st satisfier

;;; Function real-seek-satisfiers pulls out the
(defun real-seek-satisfiers (domain goals state bindings level just1 dependencies-in)
  (setq *inferences* (1+ *inferences*))
  (let ((goal1 goals) (remaining nil))

    ;; If the provided list goals has a list as the head element,
    ;; then we interpret goals as a list of items to satisfy,
    ;; rather than as one single item.
    (when (listp (car goals))
      (setq goal1 (car goals))
      (setq remaining (cdr goals))

      ;; But if it's lists three down and not just two, then we
      ;; have an implicit "and" as the first item in the original
      ;; list goals.  We make this explicit, so that "and" can be
      ;; redefined in the methods.
      (when (listp (car goal1))
        (setf goal1 (cons 'and goal1))))

    (real-seek-satisfiers-for domain (car goal1) goal1 remaining
                              state bindings level just1 dependencies-in)))

(defmacro def-logical-keyword ((name domain-specializer) &body forms)
  "(def-logical-keyword name domain-specializer options &body forms) where forms
should always include:

  (:satisfier-method (goal other-goals state bindings level just1)
     forms)

The FORMS in the satisfier method will be wrapped in a block named
:satisfier-method, in case one wishes to use a non-local exit.
\(return-from :satisfier-method ...\) will work.  The method should either
fail \(return NIL\), or it should compute some list of binding lists
from GOAL, combine them with the input BINDINGS, and then recursively
call SEEK-SATISFIERS on OTHER-GOALS, incorporating the new bindings.
The satisfier method may reference a formal named DOMAIN.

Defines a logical connective in the SHOP theorem prover.

Defines methods for REAL-SEEK-SATISFIERS-FOR and LOGICAL-KEYWORDP."

  (let ((satisfier-method-supp-p)
        (satisfier-method-params) (satisfier-method-body-forms)
        (domain-parameter-name) (domain) (domain-supp-p)
        documentation
        (name-param (gensym)))

    (unless (symbolp name)
      (error "~@<First argument to def-logical-keyword must be a symbol~
              ,~_ got ~s~:>" name))

    (cond
      ((symbolp domain-specializer)
       (setf domain-parameter-name domain-specializer
             domain-supp-p nil))
      ((listp domain-specializer)
       (setf domain-parameter-name (car domain-specializer)
             domain (cadr domain-specializer)
             domain-supp-p t))
      (t
       (error "~@<Malformed domain-specializer in def-logical-keyword: ~
                  ~_~s~:>" domain-specializer)))

    (loop for form in forms do
      (let ((form-type (pop form)))
        (case form-type
          (:satisfier-method
           (when satisfier-method-supp-p
             (error "Multiple :satisfier-method forms ~
                     in def-logical-keyword"))
           (setf satisfier-method-supp-p t
                 satisfier-method-params (pop form)
                 satisfier-method-body-forms form))
          (:documentation
           (setf documentation (pop form)))
          (otherwise
           (error "Unrecognized def-logical-keyword form ~s"
                  form-type)))))

    (unless satisfier-method-supp-p
      (error "~@<Use of def-logical-keyword without :satisfier-method: ~
                 ~_Keyword ~s, ~:_in ~
                 ~:[all domains~*~;domain class ~s~]~:>"
             name domain-supp-p domain))

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       #-sbcl
       (let ((old-doc (documentation ',name :shop-connective)))
         (when (and old-doc
                    (not (string-equal old-doc ,documentation)))
           (cerror "Continue and redefine ~a"
                   "The documentation string for connective ~a is changing.  This may indicate multiple (inconsistent) definitions for it as a logical connective"
                   ',name))
         #-sbcl
         (setf (documentation ',name :shop-connective)
               ,documentation))
       (defmethod real-seek-satisfiers-for (,domain-specializer
                                            (,name-param (eql ',name))
                                            ,@satisfier-method-params)
         (declare (ignorable ,domain-parameter-name ,name-param))
         (block :satisfier-method
         ,@satisfier-method-body-forms))
       (defmethod logical-keywordp ((,name-param (eql ',name))
                                    ,domain-specializer)
         (declare (ignorable ,domain-parameter-name ,name-param))
         t)
       )))



;;;(defun logical-keywordp (sym)
;;;  (member sym '(and or not eval call assign assign*
;;;             imply forall sort-by)))

(defmethod real-seek-satisfiers-for (domain goal-head goal other-goals
                                     state bindings level just1
                                     dependencies-in)
     "The default method catches the non-keyword case: goal is an
atomic predicate, so try to satisfy it.  The goal-head is intact
in the goal, so we ignore the extra reference."
     (declare (ignorable goal-head))
     (do-conjunct domain goal other-goals state bindings level just1 dependencies-in))

(def-logical-keyword (not domain)
  (:satisfier-method (goal other-goals state bindings level just1  dependencies-in)
      (standard-satisfiers-for-not domain (cdr goal) other-goals
                                   state bindings (1+ level) just1 dependencies-in)))

(defmethod real-seek-satisfiers-for :before (domain (op (eql 'not)) goal other-goals state bindings level just1 dependencies-in)
  (declare (ignorable domain op goal other-goals state bindings level just1 dependencies-in))
  (unless (eql (length goal) 2)
    (error 'incorrect-arity-error :expression goal :correct-arity 1 :op 'not)))
  

(def-logical-keyword (eval domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-eval domain (cdr goal) other-goals
                                  state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (call domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-call domain (cdr goal) other-goals
                                  state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (assign* domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-assign* domain (cdr goal) other-goals
                                     state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (assign domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-assign domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (imply domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-imply domain (cdr goal) other-goals
                                   state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (or domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-or domain (cdr goal) other-goals
                                state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (forall domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-forall domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (exists domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-exists domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (setof domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-setof domain (cdr goal) other-goals
                                   state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (bagof domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-bagof domain (cdr goal) other-goals
                                   state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (:external domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-external domain (cdr goal) other-goals
                                      state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (enforce domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-enforce domain (cdr goal) other-goals
                                 state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (and domain)
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (standard-satisfiers-for-and domain (cdr goal) other-goals
                                 state bindings (1+ level) just1 dependencies-in)))

;;; John's DEF-LOGICAL-KEYWORD is too rigid for this...
#+nil
(def-logical-keyword (%cut% domain)
  (:satisfier-method (goal other-goals state bindings level just1)
    (standard-satisfiers-for-%cut% domain other-goals
                                 state bindings (1+ level) just1)))
(DEFMETHOD REAL-SEEK-SATISFIERS-FOR (DOMAIN
                                       (keyword (EQL '%CUT%))
                                       GOAL
                                       OTHER-GOALS
                                       STATE
                                       BINDINGS
                                       LEVEL
                                       JUST1 dependencies-in)
    (DECLARE (IGNORABLE DOMAIN keyword goal))
    (restart-case
     (signal 'cut-commit)
     (continue ()
      (seek-satisfiers other-goals state bindings (1+ level) just1 :domain domain :dependencies dependencies-in))))
(DEFMETHOD LOGICAL-KEYWORDP ((keyword (EQL '%CUT%)) DOMAIN)
    (DECLARE (IGNORABLE DOMAIN keyword))
    T)

;; return new unifiers and dependencies (if they are being recorded), and then invokes REMAINING...
;;; FIXME: if REMAINING is NIL, then (APPLY-SUBSTITUTION REMAINING
;;; UNIFIER) will crash.  It's not clear whether this was intended.
(defun incorporate-unifiers (unifiers remaining just-one
                                      state bindings dependencies-in added-dependencies domain newlevel)
  (let (answers depends)
    (if *record-dependencies-p*
        (iter (for unifier in unifiers)
              (as dependency-set in added-dependencies)
              (multiple-value-bind (new-answers new-dependencies)
                  (seek-satisfiers (apply-substitution remaining unifier)
                                   state (apply-substitution bindings unifier)
                                   newlevel just-one :domain domain
                                   :dependencies (append dependencies-in dependency-set))
                (multiple-value-setq (answers depends)
                  (answer-set-union new-answers answers new-dependencies depends))))
      (dolist (unifier unifiers)
        (let ((new-answers
               (seek-satisfiers (apply-substitution remaining unifier)
                                state (apply-substitution bindings unifier)
                                newlevel just-one :domain domain)))
          (when new-answers
            (when just-one (return-from incorporate-unifiers new-answers))
            (setf answers (shop-union new-answers answers :test #'equal))))))
    (values answers depends)))

(defvar *negation-deps-ok* nil)

(defun standard-satisfiers-for-not (domain arguments other-goals
                                           state bindings newlevel just1 dependencies-in)

  (when *record-dependencies-p*
    (unless (or *negation-deps-ok* (groundp (first arguments)))
      (cerror "Simply return no new dependencies."
              "We do not have correct logic for computing arbitrary dependencies for negations.")
      (setf *negation-deps-ok* t)))
  ;; we just want to see if (CDR GOAL1) is satisfiable, so last arg is T
  (cond
    ((let ((*record-dependencies-p* nil))
       (seek-satisfiers arguments state nil newlevel t :domain domain))
     ;; the negation is falsified
     nil)
   (t
    (let (newdep)
      ;; FIXME: this really only works if we convert the preconditions to negation normal form.
      (when (and *record-dependencies-p* (groundp (first arguments)))
        (setf newdep
              (make-raw-depend
               :est
               (dependency-for-negation (first arguments) state)
               :prop `(not ,(first arguments)))))
      (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies (if newdep (cons newdep dependencies-in) dependencies-in))))))

(defun dependency-for-negation (positive-literal state)
  ;; FIXME: we should check that positive-literal is, in fact, a
  ;; positive literal but, oh dear -- SHOP2 doesn't really support
  ;; this.
  (last-establisher state `(not ,positive-literal)))

(defun standard-satisfiers-for-eval (domain arguments other-goals
                                     state bindings newlevel just1 dependencies-in)
  (assert (= (length arguments) 1))
  (cond
    ((eval (car arguments))
     (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies-in))
    (t nil)))

(defun standard-satisfiers-for-call (domain arguments other-goals
                                     state bindings newlevel just1 dependencies-in)
  (cond
    ((eval arguments)
     (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies-in))
    (t nil)))

(defun standard-satisfiers-for-assign* (domain arguments other-goals
                                        state bindings newlevel just1 dependencies-in)
  ;; it's possible that the VAR part of (ASSIGN VAR ANS) will already
  ;; be bound by the time the ASSIGN form is examined.  We need to
  ;; check for this important special case. [2003/06/20:rpg]
  (let ((var (car arguments))
        (answers (eval (cadr arguments))))
    (iter (for ans in answers)
          (with resulting-answers)
          (with new-answers)
          (with resulting-depends)
          (with new-depends)
        (when (or (variablep var)
               ;; trivial unification --- probably wrong, should be true unification
               (equalp var ans))
          (multiple-value-setq (new-answers new-depends)
             (seek-satisfiers
              (apply-substitution other-goals
                                  (list (make-binding var ans)))
              state
              (apply-substitution bindings
                                  (list (make-binding var ans)))
              newlevel just1 :domain domain :dependencies dependencies-in)))
        (when new-answers
          (if just1
              (return-from standard-satisfiers-for-assign* (values new-answers new-depends))
            ;; else
            (multiple-value-setq (resulting-answers resulting-depends)
              (answer-set-union new-answers resulting-answers new-depends resulting-depends))))
        (finally (return-from standard-satisfiers-for-assign* (values resulting-answers resulting-depends))))))

(defun standard-satisfiers-for-assign (domain arguments other-goals
                                       state bindings newlevel just1 dependencies-in)
  ;; it's possible that the VAR part of (ASSIGN VAR ANS) will already
  ;; be bound by the time the ASSIGN form is examined.  We need to
  ;; check for this important special case. [2003/06/20:rpg]
  (let ((var (first arguments))
        (ans (eval (second arguments))))
    (cond
      ((variablep var)
       (seek-satisfiers
        (apply-substitution other-goals (list (make-binding var ans)))
        state (apply-substitution bindings (list (make-binding var ans)))
        newlevel just1 :domain domain :dependencies dependencies-in))

      ;; trivial unification --- probably wrong --- should be true unification
      ;; actually, I don't think that this is wrong -- ASSIGN is variable assignment
      ;; and if you want unification, you should use =. [2016/04/06:rpg]
      ((equalp var ans)
       (seek-satisfiers
        other-goals
        state bindings
        newlevel just1 :domain domain :dependencies dependencies-in))

      ;; otherwise, a constant value for VAR didn't match ANS
      (t nil))))

(defun standard-satisfiers-for-enforce (domain arguments other-goals
                                               state bindings newlevel just1 dependencies-in)
  (destructuring-bind (clause &rest error-args) arguments
    (multiple-value-bind (new-answers new-dependencies)
        (find-satisfiers (list clause) state just1 newlevel
                         :domain domain)
      (unless new-answers (apply #'error error-args))
      (incorporate-unifiers new-answers other-goals
                            just1 state bindings dependencies-in new-dependencies domain newlevel))))

(defun find-variable (tree)
  (subst-if nil #'(lambda (x) (when (variablep x) (return-from find-variable x))) tree)
  nil)

(defun standard-satisfiers-for-imply (domain arguments other-goals
                                      state bindings newlevel just1 dependencies-in)
  (unless (= (length arguments) 2)
    (error "Ill-formed IMPLY expression: ~S" (cons 'IMPLY arguments)))
  (destructuring-bind (conditionA conditionB) arguments
    (unless (groundp conditionA)
      (error 'non-ground-error :var (find-variable conditionA) :expression conditionA))
    (multiple-value-bind (answers depends)
        (find-satisfiers `(or (not ,conditionA) ,conditionB) state nil newlevel :domain domain)
      (cond (answers
             (incorporate-unifiers answers other-goals just1 state bindings dependencies-in depends domain newlevel))
            (t nil)))))

(defun standard-satisfiers-for-or (domain arguments other-goals
                                   state bindings newlevel just1 dependencies-in)
  (let ((answers nil)
        (depends nil))
    (dolist (arg arguments)
      ;; First, look for ways to satisfy arg from the atoms in STATE
      (let ((mgu1 (find-satisfiers arg state nil newlevel :domain domain)))
        (when mgu1
          (dolist (tempmgu mgu1)
            (multiple-value-bind
                (new-answers new-depends)
                (seek-satisfiers
                                (apply-substitution other-goals tempmgu)
                                state (apply-substitution bindings tempmgu)
                                newlevel just1 :domain domain :dependencies dependencies-in)
              (when new-answers
                (if just1
                  (return-from standard-satisfiers-for-or (values new-answers new-depends))
                  (multiple-value-setq (answers depends)
                    (answer-set-union new-answers answers new-depends depends)))))))))
    (values answers depends)))

(defun standard-satisfiers-for-forall (domain arguments other-goals
                                       state bindings newlevel just1 dependencies-in)
  (when *record-dependencies-p*
    (cerror "Simply return no new dependencies."
            "We do not have correct logic for computing dependencies for FORALL."))
  (let* ((bounds (second arguments))
         (conditions (third arguments))
         (mgu2 (find-satisfiers bounds state nil 0 :domain domain)))
    (dolist (m2 mgu2)
      (unless (let ((*record-dependencies-p* nil))
                (seek-satisfiers (apply-substitution conditions m2)
                               state bindings 0 t :domain domain))
        (return-from standard-satisfiers-for-forall nil))))
  (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies-in))

(defun standard-satisfiers-for-exists (domain arguments other-goals
                                       state bindings newlevel just1 dependencies-in)
  (when *record-dependencies-p*
    (cerror "Simply return no new dependencies."
            "We do not have correct logic for computing dependencies for EXISTS."))
  (let* ((bounds (second arguments))
         (conditions (third arguments))
         (mgu2 (find-satisfiers bounds state nil 0 :domain domain)))
    (loop for m2 in mgu2
        when (let ((*record-dependencies-p* nil))
               (seek-satisfiers (apply-substitution conditions m2)
                              state bindings 0 t :domain domain))
          return t                      ; short-circuit, and move on
                                        ; to remaining
                                        ; goals... [2007/07/15:rpg]
        finally ;; didn't find any value that worked
          (return-from standard-satisfiers-for-exists nil)))

  ;; Satisfy other goals
  (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies-in))

(defun standard-satisfiers-for-setof (domain arguments other-goals
                                             state bindings newlevel just1 dependencies-in)
  ;; (setof ?var expr ?outvar)
  (when *record-dependencies-p*
    (cerror "Simply return no new dependencies."
            "We do not have correct logic for computing dependencies for SETOF."))
  (destructuring-bind (var bounds outvar) arguments
    (let ((raw-results (find-satisfiers (list bounds) state
                                        ;; no bindings should be
                                        ;; necessary because of the
                                        ;; substitution into the
                                        ;; goal...
                                        nil 0 :domain domain)))
      (unless raw-results (return-from standard-satisfiers-for-setof nil))
      (let ((new-binding
             (make-binding outvar
                           (remove-duplicates
                            (loop for binding-list in raw-results
                                collect (binding-list-value var binding-list))
                            :test 'equal))))
        (seek-satisfiers (apply-substitution other-goals (list new-binding))
                         state (apply-substitution bindings (list new-binding))
                         newlevel just1 :domain domain :dependencies dependencies-in)))))

(defun standard-satisfiers-for-bagof (domain arguments other-goals
                                             state bindings newlevel just1 dependencies-in)
  ;; (bagof ?var expr ?outvar)
  (when *record-dependencies-p*
    (cerror "Simply return no new dependencies."
            "We do not have correct logic for computing dependencies for BAGOF."))
  (destructuring-bind (var bounds outvar) arguments
    (let ((raw-results (find-satisfiers (list bounds) state
                                        ;; no bindings should be
                                        ;; necessary because of the
                                        ;; substitution into the
                                        ;; goal...
                                        nil 0 :domain domain)))
      (unless raw-results (return-from standard-satisfiers-for-bagof nil))
      (let ((new-binding
             (make-binding outvar
                            (loop for binding-list in raw-results
                                  collect (binding-list-value var binding-list)))))
        (seek-satisfiers (apply-substitution other-goals (list new-binding))
                         state (apply-substitution bindings (list new-binding))
                         newlevel just1 :domain domain :dependencies dependencies-in)))))

(defun standard-satisfiers-for-external (domain arguments other-goals
                                         state bindings newlevel just1 dependencies-in)
  (let (new-unifiers new-depends)
    (multiple-value-setq (new-unifiers new-depends)
      (external-find-satisfiers domain arguments state))
    (unless new-unifiers
      (multiple-value-setq (new-unifiers new-depends)
        (find-satisfiers arguments state nil newlevel :domain domain)))
    (when new-unifiers
      (incorporate-unifiers new-unifiers other-goals just1 state bindings dependencies-in new-depends domain newlevel))))
   
(defun standard-satisfiers-for-and (domain arguments other-goals
                                    state bindings newlevel just1 dependencies-in)
  (multiple-value-bind (new-unifiers new-depends)
      (find-satisfiers arguments state nil newlevel :domain domain)
  (incorporate-unifiers new-unifiers
   other-goals just1 state bindings dependencies-in new-depends domain newlevel)))

; An alternate version that wasn't used:
;(defun external-find-satisfiers (goal state)
;  (format t "EFS: ~s~%" goal)
;  (or (when *external-access*
;       (let ((sats (external-query domain goal state)))
;         (nconc *attribution-list*
;                (mapcar #'(lambda (sat)
;                            (list (apply-substitution goal sat)
;                                  ATTRIB))
;                        sats))
;         sats))
;      (find-satisfiers goal state nil 0 :domain domain)))

(defun external-find-satisfiers (domain goal state)
;  (format t "EFS: ~s~%" goal)
  (if *external-access*
      (external-query domain goal state)
    nil))

;;; Goal1 is guaranteed to be an atomic predicate
;;; BINDINGS is a list of either variables or the values assigned to those bindings.
;;; Note that this is a SINGLE (partial) solution -- but below here we recursively
;;; call SEEK-SATISFIERS multiple times, so that this is a single node in the proof
;;; tree, but the return value represents a SET of leaf nodes in the proof tree.
(defun do-conjunct (domain goal1 remaining state bindings level just1 dependencies-in)
  "Where goal1 is an atomic predicate (not an atom per se, but a clause not
headed by an operator or other symbol), try to find a binding that satisfies
goal1 along with all of the other formulas in remaining."
  (trace-print :goals (car goal1) state
               "~2%Level ~s, trying to satisfy goal ~s" level goal1)

  ;; Then, look for ways to satisfy GOAL1 from the atoms in state
  (multiple-value-bind (answers found-match dependencies)
      (do-conjunct-from-atoms domain goal1 remaining state bindings level just1 dependencies-in)
    (when *record-dependencies-p* (assert (= (length answers) (length dependencies))))
    (when (and answers just1)
      (return-from do-conjunct (values answers dependencies)))

    ;; Next, look for ways to prove GOAL1 from the *axioms*
    (multiple-value-bind (axiom-answers axiom-found-match axiom-dependencies)
        (do-conjunct-from-axioms domain goal1 remaining state bindings level just1 dependencies-in)
      (when *record-dependencies-p* (assert (= (length axiom-answers) (length axiom-dependencies))))
      (when (and axiom-answers just1)
        (return-from do-conjunct (values axiom-answers axiom-dependencies)))

      ;; found-match is a little deceiving -- it means we found a
      ;; MATCH for GOAL1: it doesn't mean we have proved REMAINAING,
      ;; as well.
      (if (or found-match axiom-found-match)
          (progn
            (trace-print :goals (car goal1) state
                         "~2%Level ~s, matched goal ~s" level goal1)
            ;; DO-CONJUNCT returns a list of lists of variable bindings,
            ;; each for a different answer.
            (answer-set-union answers axiom-answers dependencies axiom-dependencies))
          (progn 
            (trace-print :goals (car goal1) state
                         "~2%Level ~s, couldn't match goal ~s" level goal1)
            nil)))))

;;; BINDINGS is a list of either variables or the values assigned to those bindings.
;;; Note that this is a SINGLE (partial) solution -- but below here we recursively
;;; call SEEK-SATISFIERS multiple times, so that this is a single node in the proof
;;; tree, but the return value represents a SET of leaf nodes in the proof tree.
(defun do-conjunct-from-atoms (domain goal1 remaining state bindings level just1 dependencies-in)
  (let (answers mgu1 found-match depends)
    (dolist (r (state-candidate-atoms-for-goal state goal1))
      (unless (eql (setq mgu1 (unify goal1 r)) (shop-fail))
        (let ((updated-dependencies
               (when *record-dependencies-p*
                 (let ((e (last-establisher state r)))
                   (when e              ; fact might come from initial state
                     (cons (make-raw-depend :prop r :est e) dependencies-in))))))
          (setq found-match t) ; for debugging printout
          (multiple-value-bind (new-answers new-depends)
              (seek-satisfiers (apply-substitution remaining mgu1)
                               state (apply-substitution bindings mgu1)
                               (1+ level) just1 :domain domain :dependencies updated-dependencies)
            (when *record-dependencies-p*
              (assert (= (length new-answers) (length new-depends))))
            (when new-answers
              (trace-print :goals (car goal1) state
                           "~2%Level ~s, state satisfies goal ~s~%satisfiers ~s"
                           level goal1 new-answers)
              (when just1
                (return-from do-conjunct-from-atoms (values new-answers found-match new-depends)))
              ;; Union of list-of-binding-lists (ANSWERS) with list of binding-lists
              ;; NEW-ANSWERS.  So, e.g., eliminates duplicate copies of ((?X . 1) (?Y . 2))
              (multiple-value-setq (answers depends)
                (answer-set-union new-answers answers new-depends depends))
              ;; (format t "~&Answers: ~s~%" answers)
              )))))
    (unless answers
      (trace-print :goals (car goal1) state
                   "~2%Level ~s, state fails goal ~s~%"
                   level goal1))
    (values answers found-match depends)))

;;; BINDINGS is a list of either variables or the values assigned to those bindings.
;;; Note that this is a SINGLE (partial) solution -- but below here we recursively
;;; call SEEK-SATISFIERS multiple times, so that this is a single node in the proof
;;; tree, but the return value represents a SET of leaf nodes in the proof tree.
(defun do-conjunct-from-axioms (domain goal1 remaining state bindings level just1 dependencies-in)
  (let (found-match answers dependencies)
    (dolist (r (axioms domain (car goal1)))
      (multiple-value-bind (axiom-answers axiom-found-match axiom-dependencies)
          (do-conjunct-from-axiom r domain goal1 remaining state bindings
            level just1 dependencies-in)
        (when *record-dependencies-p*
          (assert (= (length axiom-answers) (length axiom-dependencies))))
        (when (and just1 axiom-answers)
          (return-from do-conjunct-from-axioms
            (values axiom-answers axiom-found-match axiom-dependencies)))
        (setf found-match (or found-match axiom-found-match))
        (multiple-value-setq (answers dependencies)
          (answer-set-union axiom-answers answers axiom-dependencies dependencies))))
    (when *record-dependencies-p* (assert (= (length answers) (length dependencies))))
    (unless answers
      (trace-print :goals (car goal1) state
                    "~2%Level ~s, state axioms fail goal ~s"
                         level goal1))
    (values answers found-match dependencies)))

(defun answer-set-union (answer-list new-answer-list dependency-list new-dependency-list)
  "Remove redundant answers from ANSWER-LIST.  If we remove an entry from ANSWER-LIST,
also remove the corresponding entry from DEPENDENCY-LIST.
   ANSWER-LIST is a list of lists of (variable | value).
   Returns two values: the filtered answer-list and the filtered dependency-list."
  (cond (*record-dependencies-p*
         (assert (and (= (length answer-list) (length dependency-list))
                      (= (length new-answer-list) (length new-dependency-list))))
         (iter (for answer in new-answer-list)
               (as dependencies in new-dependency-list)
               (unless (member answer answer-list :test 'equal)
                 (collect answer into filtered-answers)
                 (collect dependencies into filtered-dependencies))
               (finally (return-from answer-set-union
                          (values (append answer-list filtered-answers)
                                  (append dependency-list filtered-dependencies))))))
        (t
         (iter (for answer in new-answer-list)
               (unless (member answer answer-list :test 'equal)
                 (collect answer into filtered-answers))
               (finally (return-from answer-set-union (append answer-list filtered-answers)))))))
  

;;; BINDINGS is a list of either variables or the values assigned to those bindings.
;;; Note that this is a SINGLE (partial) solution -- but below here we recursively
;;; call SEEK-SATISFIERS multiple times, so that this is a single node in the proof
;;; tree, but the return value represents a SET of leaf nodes in the proof tree.
(defun do-conjunct-from-axiom (axiom domain goal1 remaining state bindings level just1 dependencies-in)
  (let* ((standardized-axiom (standardize axiom))
         (mgu1 (unify goal1 (second standardized-axiom)))
         committed
         found-match new-just1)
    (unless (eql mgu1 (shop-fail))
      ;; found an axiom which unifies, now look at branches of the tail
      (setf found-match t)
      (let ((tail (cddr standardized-axiom)))
        (block break
          (do ((ax-branch-name (car tail) (car tail))
               (ax-branch (cadr tail) (cadr tail)))
              ((null tail)  nil)
            (trace-print :goals (car goal1) state
                         "~2%Level ~s, axiom matches goal ~s~
                    ~%     axiom ~s~%satisfiers ~s"
                         level goal1 ax-branch-name mgu1)
            (trace-print :axioms ax-branch-name state
                         "~2%Level ~s, trying axiom ~s~%      goal ~s~
                    ~%      tail ~s"
                         level ax-branch-name goal1 (apply-substitution ax-branch mgu1))
            (if (eq (car ax-branch) :first)
                (setq new-just1 t ax-branch (cdr ax-branch))
              (setq new-just1 just1))
            (multiple-value-bind (new-answers new-dependencies)
                (handler-bind
                    ((cut-commit
                      #'(lambda (c)
                          (setf committed t)
                          (continue c))))
                    (seek-satisfiers
                     (apply-substitution (append (list ax-branch) `((%cut%)) remaining)
                                         mgu1)
                     state (apply-substitution bindings mgu1) (1+ level)
                     new-just1 :domain domain :dependencies dependencies-in))
              (if new-answers
                  (progn
                    (trace-print :axioms ax-branch-name state
                                 "~2%Level ~s, applying axiom ~s~%      goal ~s~
                        ~%      tail ~s"
                                 level ax-branch-name goal1
                                 (apply-substitution ax-branch mgu1))
                    (when *record-dependencies-p*
                      (assert (= (length new-answers) (length new-dependencies))))
                    (return-from do-conjunct-from-axiom
                      (values new-answers found-match new-dependencies)))
                (progn
                  (trace-print :axioms ax-branch-name state
                               "~2%Level ~s, exiting axiom ~s~%      goal ~s~
                        ~%      tail ~s"
                               level ax-branch-name goal1
                               (apply-substitution ax-branch mgu1)))))
              (if committed
                  ;; don't look at any more axiom tails.
                  (return-from break)
                (setf tail (cddr tail))))))))
    ;; if you get here, you have failed in the proof.
    nil)


;;; ------------------------------------------------------------------------
;;; Explanation of satisifiers
;;; ------------------------------------------------------------------------

; The following code is invoked only if the :explanation keyword for
;  the planner has been given a true value.

; Given a goal unified by a valid satisfier, construct an assertion
;  that explains how the unifier satisfied that goal and an
;  attribution for the explanation (if it exists).  For example, if
;  the original goal were:
;    '(and (or (and (on ?a ?b) (on ?a ?c)) (on ?d ?a)) (on ?b ?c))
;  and a unifier ((?a.x1) (?b.x2) (?c.x3)) then the explanation would be:
;    '(and (and (on x1 x2) (on x1 x3)) (on x2 x3))
;
; If any term or conjunct in the explanation comes from an external
;  query (see the external-query routine), a list containing the term
;  :source followed by attribution information is added to the
;  beginning of the term or conjunct.  In the above example, if the
;  attribution for (and (on x1 x2) (on x1 x3)) were (PoliceReport
;  UID1234), and the attribution for (on x2 x3) were (PoliceReport
;  UID4321) then the explanation would be
;    '(and ((:source PoliceReport UID1234) and (on x1 x2) (on x1 x3))
;          ((:source PoliceReport UID4321) on x2 x3))
(locally (declare #+sbcl (sb-ext:muffle-conditions sb-int:simple-style-warning))
  ;; this is necessary because SBCL (correctly) hates the use of OPTIONAL and
  ;; KEY together.

  (defun explain-satisfier (unified-goal state &optional external
                                       &key (domain nil domain-supp-p))
  (unless domain-supp-p (setf domain *domain*))
  (let ((*external-access* nil)) ; otherwise we'd query twice
    (cond
     ((member (first unified-goal)
              '(sort-by not eval call assign imply forall))
     ; The above constructs are not handled by the explanation code yet.
      nil)
     ((eq (first unified-goal) :external)
      (explain-satisfier (rest unified-goal) state t :domain domain))
     ((eq (first unified-goal) 'or)
      (cond
       ((null (rest unified-goal)) nil)
       ((not (find-satisfiers (second unified-goal) state nil 0 :domain domain))
        (explain-satisfier (cons 'or (rest (rest unified-goal)))
                           state external :domain domain))
       (t
        (explain-satisfier (second unified-goal) state external :domain domain))))

     ((eq (first unified-goal) 'and)
      (let* ((explanation-list
              (mapcar #'(lambda (g) (explain-satisfier g state external :domain domain))
                      (rest unified-goal)))
             (simplified-explanation-list (remove nil explanation-list))
             (explanation
              (when (find-satisfiers unified-goal state nil 0 :domain domain)
                (cons 'and simplified-explanation-list))))
        (when explanation
          (add-source unified-goal external explanation))))

     ((listp (first unified-goal)) ; implicit and
      (explain-satisfier (cons 'and unified-goal) state external :domain domain))
     (t ; logical-atom
      (add-source unified-goal external unified-goal))))))

(defun add-source (unified-goal external explanation)
  (if external
      (let ((source (get-attribution unified-goal)))
        (if source
            (cons (list :source source) explanation)
          explanation))
    explanation))

(defun get-attribution (unified-query)
;(format t "~%Source access: ~a from~%   ~a" unified-query *attribution-list*)
  (second (assoc unified-query *attribution-list*
                 :test #'(lambda (q att)
                           (or (equal q att)
                               (equal (list 'and q) att))))))

(defun fully-instantiated-goal (goal)
  (if (atom goal)
      (not (variablep goal))
    (and (fully-instantiated-goal (first goal))
         (fully-instantiated-goal (rest goal)))))

;;; ------------------------------------------------------------------------
;;; External access to state information
;;; ------------------------------------------------------------------------

; The following code is invoked only if an external-access-hook routine
;   has been defined.

; If external-query receives a query of the form (<pred> <val> <val>)
;   or (and (<pred> <val> <val>)+), it sends that query to
;   external-access-hook.  If the query succeeds, the resulting
;   information is added to the state and the attribution information
;   is stored in *attribution-list*.
;  If external-query receives a complex query involving and's and
;   or's, it decomposes that into queries it can send to
;   external-access-hook.  If external-query encounters other logical
;   constructs (e.g., not, imply), external-query returns nil (but
;   any responses it had already received are still kept in the
;   state and *attribution-list*.
(defun external-query (domain query state)
;  (format t "~%potential query: ~s" query)
  (cond
   ((null query) nil)
   ((member (first query) '(not eval call assign assign* imply forall sort-by))
    nil)
   ((listp (first query)) ; implicit and
    (external-query domain (cons 'and query) state))
   ((eq (first query) 'or)
    (or (external-query domain (second query) state)
        (when (rest (rest query))
          (external-query domain `(or ,(rest (rest query))) state))))
   ((and (eq (first query) 'and) (rest query))
    (if (find-if-not #'(lambda (subquery)
                         (and (listp subquery)
                              (= (length subquery) 3)
                              (not (logical-keywordp (first subquery) domain))
                              (not (find-if-not #'atom subquery))))
                     (rest query))
        ; complex query - try to decompose
        (let ((first-response (external-query domain (second query) state)))
          (when first-response
            (let ((rest-response (external-query domain `(and ,@(rest (rest query)))
                                                 state)))
              (when rest-response
                (merge-binding-set-lists first-response rest-response)))))
      ; simple query - invoke external access
      (invoke-external-query query state)))
   (t ; query of a single logical atom
    (invoke-external-query (list 'and query) state))))

; Takes two lists of sets of bindings and returns a list of sets of
;  bindings consisting of all consistent combinations of one set from
;  each list.  E.g.:
;
; (merge-binding-set-lists '(((?x 1) (?y 2)) ((?x 3) (?z 4)))
;                          '(((?x 1) (?a 5)) ((?b 6) (?c 7))))
; =>
; '(((?x 1) (?y 2) (?a 5)) ((?x 1) (?y 2) (?b 6) (?c 7))
;   ((?x 3) (?z 4) (?b 6) (?c 7)))
;
; Note that the second set in the first list and the first set in the
;  second list are not merged in the result because they have
;  incompatible bindings for ?x.
(defun merge-binding-set-lists (binding-sets1 binding-sets2
                                              &optional original-binding-sets2)
  (cond
   ((null binding-sets1) nil)
   ((null binding-sets2)
    (when original-binding-sets2
      (merge-binding-set-lists (rest binding-sets1)
                               original-binding-sets2 original-binding-sets2)))
   (t
    (let* ((original-sets2 (or original-binding-sets2 binding-sets2))
           (first-merge
            (merge-binding-sets (first binding-sets1) (first binding-sets2)))
           (rest-merge
            (merge-binding-set-lists binding-sets1 (rest binding-sets2)
                                     original-sets2)))
      (if first-merge
          (cons first-merge rest-merge)
        rest-merge)))))

(defun merge-binding-sets (binding-set1 binding-set2)
  (append
   binding-set1
   (remove-if
    #'(lambda (binding2)
        (find-if #'(lambda (binding1)
                     (cond
                      ((equal binding1 binding2) t)
                      ((eq (first binding1) (first binding2))
                       (return-from merge-binding-sets nil))
                      (t nil)))
                 binding-set1))
    binding-set2)))

; Directly invokes external-access-hook on a query of the form
;  (and (<pred> <val> <val>)+) and returns the resulting bindings,
;  stores the attribution information in *attribution-list*, and
;  stores the resulting facts in the state [NOTE: adding an atom to
;  the state here has wierd implications for backtracking, etc.; may
;  want to reconsider.]
(defun invoke-external-query (query state)
  (mapcar
   #'(lambda (attributed-binding-set)
       (let* ((attribution (first attributed-binding-set))
              (binding-set (fix-uninterned-bindings
                            (mapcar #'(lambda (l)
                                        (cons (first l) (second l)))
                                    (second attributed-binding-set))
                            (extract-variables query)))
              (unified-query (apply-substitution query binding-set)))
         (setf *attribution-list* (cons (list unified-query attribution)
                                        *attribution-list*))
         (dolist (fact (rest unified-query))
           (shop2.common::add-atom-to-state fact state nil nil))
         binding-set))
     (funcall (fdefinition 'external-access-hook) query)))

; Sample behavior of an external-access-hook:
;  Takes a goal as input, returns bindings and attribution.
(defun DUMMY-external-access-hook (goal)
  (format t "~%external query: ~s" goal)
  (let ((dummy-goal '(and
                     (employees ?business UID101)
                     (hasMembers UID101 ?business)))
        (dummy-bindings `((,(second (second goal)) UID102)))
        (dummy-attribution '(UID103 BusinessNews)))
    (when (goal-equalp goal dummy-goal)
      (list (list dummy-attribution dummy-bindings)))))

(defun goal-equalp (g1 g2)
  (cond
   ((eq g1 g2) t)
   ((and (variablep g1) (variablep g2)) t)
   ((and (listp g1) (listp g2)
         (= (length g1) (length g2))
         (goal-equalp (first g1) (first g2))
         (goal-equalp (rest g1) (rest g2)))
    t)
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; debug output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-axioms (&optional name (domain *domain*))
  (if name
    (progn (format t "Axioms for name ~S are:" name)
           (mapcar #'(lambda (x) (format t "~2%  ~s" x))
                   (axioms domain name)))
    (maphash #'(lambda (k defs)
                 (format t "~2%Axioms for goal ~S are:" k)
                 (dolist (ad defs)
                   (format t "~2%  ~S" ad)))
             (domain-axioms domain))))

(defun print-belief-state-readably (belief-state &optional (stream t))

  (let ((props-as-strings (sort
                           (mapcar #'(lambda (prop) (format nil "~s" prop))
                                   (shop2.common::state-atoms belief-state))
                           #'string<)))
;;;    (macroexpand '(iter (for prop-string in props-as-strings)
;;;                (for prev previous prop-string)
;;;                (unless (and prev
;;;                         (first-word-equal prev prop-string))
;;;                  (format stream "~%"))
;;;                (format stream "~a~%" prop-string))
;;; then query-replace #: symbols
    (let* ((list1 nil) (prop-string nil) (prev nil) (POST-SAVE-prop-string-2 nil))
      (block nil
        (tagbody
          (progn (setq list1 props-as-strings) (setq prev nil) (setq POST-SAVE-prop-string-2 nil))
         loop-top-nil
          (progn (progn (setq prev POST-SAVE-prop-string-2)) (if (endp list1) (go loop-end-nil))
                 (setq prop-string (car list1)) (setq list1 (cdr list1))
                 (progn (setq POST-SAVE-prop-string-2 prop-string))
                 (if (not (and prev (first-word-equal prev prop-string))) (progn nil (format stream "~%")))
                 (format stream "~a~%" prop-string))
          (progn)
          (go loop-top-nil)
         loop-end-nil
          (progn))
        nil))))

;; easy, but poss inefficient
(defun first-word-equal (string1 string2)
  (string-equal
   (subseq string1 0 (position #\Space string1))
   (subseq string2 0 (position #\Space string2))))
