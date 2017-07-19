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
(in-package :shop2)


(defgeneric validator-export (domain plan stream)
  (:documentation "Print a plan in a way that it can be consumed by
the VAL validator."))

(defun write-pddl-plan (plan &key (domain *domain*) (stream t stream-supplied-p) (filename nil))
  "Write the plan argument \(in a way sensitive to the domain type\)
to the FILENAME or STREAM in a format that it can be checked by the validator.
This is an easier to use interface to the validator-export function, qv."
  (when (and filename stream-supplied-p)
    (error "Cannot supply both a filename and stream destination for write-pddl-plan."))
  (when filename
    (setf stream (open filename :direction :output)))
  (unwind-protect
       (validator-export domain plan stream)
    (when filename (close stream))))


;;;---------------------------------------------------------------------------
;;; Class definitions for domains, used to control processing of PDDL
;;; syntax.[2006/08/01:rpg]
;;;---------------------------------------------------------------------------


(defclass simple-pddl-domain ( domain )
  ()
  (:documentation "A new class of SHOP2 domain that permits inclusion of
PDDL operator definitions.")
  )

(defclass universal-precondition-mixin ()
     ()
  )

(defclass existential-precondition-mixin ()
     ()
  )


(defclass conditional-effects-mixin ()
     ()
  )

(defclass equality-mixin ()
     ()
  )

(defclass pddl-domain ( conditional-effects-mixin existential-precondition-mixin universal-precondition-mixin equality-mixin simple-pddl-domain )
  ()
  (:documentation "A new class of SHOP2 domain that permits inclusion of
PDDL operator definitions.  Right now, this approximates what happens if you have
the :adl flag in a PDDL domain file.")
  )

(defgeneric process-action (domain action-def)
  (:documentation "PDDL has actions \(vanilla SHOP2 has operators\), so we 
add code for processing the actions that is parallel to process-operator."))

;;;---------------------------------------------------------------------------
;;; Structure for PDDL actions -- plays a role akin to the role played
;;; by the OPERATOR defstruct in vanilla shop2.
;;;---------------------------------------------------------------------------
(defstruct (pddl-action :named (:type list))
  (head nil :type list)         
  precondition
  effect
  (cost-fun nil))

(defmethod parse-domain-items :around ((domain equality-mixin) items)
  "Add the axiom that treats equality as a built-in predicate.  This should 
later be compiled into find-satisfiers or something."
  (let ((equality-axiom '(:- (= ?x ?x) nil)))
    (set-variable-property domain equality-axiom)
    (call-next-method domain (cons equality-axiom items))))

;;; parsing PDDL action items --- for now we treat them just as
;;; operators, but this may get us in trouble! [2006/07/28:rpg]
(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':action)) item)
  (let ((op-name (second item)))
    ;; do some nasty voodoo to give PDDL actions names like SHOP2
    ;; operators [2006/07/31:rpg]
    (unless (eql (elt (symbol-name op-name) 0) #\!)
      (setf op-name (intern (concatenate 'string
                                         "!" (symbol-name op-name))
                            (symbol-package op-name)
                            ))
;;;      (format t "~&Making new SHOP2 operator name ~S from old name ~S.~%"
;;;           op-name (second item))
      (setf (second item) op-name))
    (with-slots (operators) domain
      (when (gethash op-name operators)
        (error "There is more than one operator named ~s" op-name))
      (setf (gethash op-name operators) (process-action domain item)))))

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':predicates)) item)
  "For now, since SHOP doesn't typecheck, we simply ignore these declarations."
  (declare (ignore item))
  (values))

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':types)) item)
  "For now, since SHOP doesn't typecheck, we simply ignore these declarations."
  (declare (ignore item))
  (values))

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':requirements)) item)
  "For now, simply ignore requirements.  Later we should check that they match the domain class."
  (declare (ignorable domain item-key item))
  (values))
  

(defmethod process-action ((domain simple-pddl-domain) action-def)
  "Processes a PDDL action.  Handles only the simplest class of actions, 
with unconditional actions."
  (destructuring-bind (keyword action-symbol &key parameters precondition effect
                                                  (cost 1.0))
      action-def
    (unless (eq keyword :action)
      (error "Unexpected action expression: ~A" action-def))
    ;; this takes care of processing variables and marking primitive
    ;; actions [2006/08/01:rpg]
    (set-variable-property domain action-def)
    (multiple-value-bind (param-vars param-types)
        (typed-list-vars parameters)
      (declare (ignore param-types))
      (let ((precond
             (translate-precondition domain precondition))
            (eff
             (translate-effect domain effect))
            (head (cons action-symbol param-vars)))
        (make-pddl-action :head head :precondition precond
                          :effect eff :cost-fun cost)))))

;;;---------------------------------------------------------------------------
;;; Additional generic functions, used to tailor the translation of
;;; PDDL actions into SHOP2 syntax.  The different PDDL constructs are
;;; gated by PDDL requirements flags, which are represented as mixins
;;; in PDDL domains.
;;;---------------------------------------------------------------------------
(defgeneric translate-precondition (domain expression)
  (:documentation
   "This generic function is used to translate PDDL-style preconditions
into SHOP2-style syntax.  The rewriting is done based on the domain so 
that different syntax features can be turned on and off."))

(defgeneric translate-effect (domain expression)
  (:documentation
   "This generic function is used to translate PDDL-style effects
into SHOP2-style syntax.  The rewriting is done based on the domain so 
that different syntax features can be turned on and off."))

;;;---------------------------------------------------------------------------
;;; Methods for translate-effect
;;;---------------------------------------------------------------------------


(defmethod translate-effect ((domain simple-pddl-domain) effect)
  "Basis method for translating a PDDL effect into SHOP2 syntax is
to just leave it alone."
  effect)

(defmethod translate-effect ((domain conditional-effects-mixin) effect)
  "This method translates any forall expressions in a PDDL precondition into the
slightly-different SHOP2 FORALL syntax.
It then invokes the next method, to insure that all PDDL - SHOP2 constructs are
translated."
  (let ((new-effect (translate-pddl-quantifier effect 'forall)))
    (call-next-method domain new-effect)))

;;;---------------------------------------------------------------------------
;;; Methods for translate-precondition
;;;---------------------------------------------------------------------------

(defmethod translate-precondition ((domain simple-pddl-domain) expression)
  "Basis method for translating a PDDL precondition into SHOP2 syntax is
to just leave it alone."
  expression)

(defmethod translate-precondition ((domain universal-precondition-mixin) expression)
  "This method translates any forall expressions in a PDDL precondition into the
slightly-different SHOP2 FORALL syntax.
It then invokes the next method, to insure that all PDDL - SHOP2 constructs are
translated."
  (let ((new-expr (translate-pddl-quantifier expression 'forall)))
    (call-next-method domain new-expr)))

(defmethod translate-precondition ((domain existential-precondition-mixin) expression)
  "This method translates any exists expressions in a PDDL precondition into the
slightly-different SHOP2 EXISTS syntax.
It then invokes the next method, to insure that all PDDL - SHOP2 constructs are
translated."
  (let ((new-expr (translate-pddl-quantifier expression 'exists)))
    (call-next-method domain new-expr)))


;;;
;;;---------------------------------------------------------------------------
;;; Helper functions
;;;---------------------------------------------------------------------------

(defun translate-pddl-quantifier (expression quantifier)
  "Translate EXPRESSION from PDDL quantifier \(forall and exists\) notation 
into SHOP2 quantifier notation \(and adds some
\(:of-type <type> <var>\) conditions\)."
  (labels ((iter (expr)
             (cond ((and (listp expr) (eq (first expr) quantifier))
                    (rewrite-quant expr))
                   ((listp expr)
                    (mapcar #'iter expr))
                   ((atom expr) expr)))
           (rewrite-quant (quant-expr)
             (destructuring-bind (quant typed-list sub-expr)
                 quant-expr
               (declare (ignore quant))
               (multiple-value-bind (vars types)
                   (typed-list-vars typed-list)
                 `(,quantifier ,vars
                          ,(let ((exprs (of-type-exprs vars types)))
                             (if (= (length exprs) 1)
                                 (first exprs)
                                 (cons 'and exprs)))
                          ,(iter sub-expr))))))
    (iter expression)))

(defun of-type-exprs (vars types)
  "Because SHOP2 doesn't have the same typing, we take variables
and their types and generate a list of distinguished propositions 
we can use in preconditions."
  (loop for v in vars
        as type in types
        collect `(:of-type ,type ,v)))
  

;;; this should be fixed to check to make sure everything in the
;;; return value list is really a variable.... [2006/07/28:rpg]
;;; FIXME: If the variables are not all typed (some are implicitly OBJECT), this
;;; will fail. [2012/10/09:rpg]
(defun typed-list-vars (typed-list)
  "Takes a typed-list (this is a PDDL construct) as argument and 
pulls out the variables and types and returns two parallel
lists of variable names and type names."
  (loop with lst = typed-list
        with counter = 0
        until (null lst)
        if (eq (first lst) '-)
          append (make-list counter :initial-element (second lst)) into types
          and do (setf lst (cddr lst)
                    counter 0)
        else
          collect (first lst) into vars
          and do (setf lst (cdr lst))
                 (incf counter)
        ;; FIXME: check to make sure counter == 0 here. Otherwise there are
        ;; untyped variables 
        finally (return (values vars types))))
                            
;;;---------------------------------------------------------------------------
;;; Apply-action, which plays a role akin to apply-operator in vanilla
;;; SHOP2.
;;;---------------------------------------------------------------------------
(defun apply-action (state task-body action protections depth
                           in-unifier)
  "If ACTION, a PDDL ACTION, is applicable to TASK in STATE, then 
APPLY-ACTION returns five values:
1.  the operator as applied, with all bindings;
2.  a state tag, for backtracking purposes;
3.  a new set of protections;
4.  the cost of the new operator;
5.  unifier.
This function also DESTRUCTIVELY MODIFIES its STATE argument.
Otherwise it returns FAIL."
  (let* ((standardized-action (standardize action))
         (head (pddl-action-head standardized-action))
         (preconditions (pddl-action-precondition standardized-action))
         (effect (pddl-action-effect standardized-action))
         unifier depends)

    ;; added rudimentary arity-checking...
    (unless (= (length task-body) (length head))
      (cerror "Continue (action application will fail)"
              "Arity of action in the plan library, ~D~%~T~S~%does not match task, ~D~%~T~S"
              (length head)
              head
              (length task-body)
              task-body))

    ;; new tracing facility for debugging
    (when *traced-tasks*
      (when (member (first head) *traced-tasks*)
        (break "Applying action for ~A~%~S" (first head) task-body)))

    (let ((action-unifier (unify head (apply-substitution task-body in-unifier))))

      (when (eql action-unifier 'fail)
        (values 'fail protections 0))

      ;; everything below is "if action-unifier != fail"
      
      (setf action-unifier
            (compose-substitutions action-unifier in-unifier))
      ;; first check the preconditions, if any
      (if preconditions
        (let ((pre (apply-substitution preconditions action-unifier)))
          
          ;; need to specially handle the preconditions, since the
          ;; syntax of PDDL preconditions are different from
          ;; SHOP2. [2006/07/31:rpg]
          (multiple-value-bind (pu pd)
              (shopthpr:find-satisfiers pre state t)
            (unless pu
              (trace-print :operators (first head) state
                           "~2%Depth ~s, inapplicable action ~s~%     task ~s.~%     Precondition failed: ~s.~%"
                           depth
                           (first head)
                           (apply-substitution task-body unifier)
                           pre
                           )
              (return-from apply-action (values 'fail preconditions 0)))
            (setf unifier (compose-substitutions action-unifier (first pu))
                  depends (first pd))))
        (setq unifier action-unifier)))
    ;; end of scope for action-unifier...

    ;; all this stuff below here must be revised since we have an EFFECT,
    ;; instead of add and delete lists... [2006/07/30:rpg]
    (let* ((effect-subbed (apply-substitution effect unifier))
           (head-subbed (apply-substitution head unifier))
           (cost-value
            (eval (apply-substitution
                   (pddl-action-cost-fun standardized-action) unifier)))
           (cost-number (if (numberp cost-value) cost-value 1.0)))


      ;; at this point UNIFIER is bound to the results of unifying
      ;; TASK-BODY with the operator's head and then retrieving
      ;; the first set of bindings that satisfy the preconditions.
      ;; we will return this unifier later on, assuming that
      ;; bindings from add and delete lists should not be plugged
      ;; in. [2004/01/20:rpg]
      (when *explanation*
        (setq head-subbed `(,@(cons (first head-subbed)
                                    (mapcar #'list
                                            (rest (second action))
                                            (rest head-subbed)))
                              :explanation
                              ,(shopthpr:explain-satisfier (apply-substitution preconditions unifier)
                                state)))
        )
      (trace-print :operators (first head) state
                   "~2%Depth ~s, applying PDDL action ~s~%      task ~s~%       effect ~s"
                   depth
                   (first head)
                   (apply-substitution task-body unifier)
                   effect-subbed)

      (multiple-value-bind (final-adds final-dels)
          (extract-adds-and-deletes effect-subbed state)


        (let ((protections1 protections)               
              (statetag (tag-state state)))
          ;; process PROTECTIONS generated by this operator
          (dolist (d final-dels)
            (if (eq (car d) :protection)
                (setq protections1 
                      (delete-protection 
                       protections1 (second d) depth (first head) state))
                (delete-atom-from-state d state depth (first head))))

          (dolist (a final-adds)
            (unless (eq (car a) :protection)
              ;; added this error-checking.  I can't think of a case where
              ;; it's ok to add a non-ground literal to the
              ;; state. [2004/02/17:rpg]
              (unless (groundp a)
                (error "Attempting to add non-ground literal ~S to state."
                       a))
              (add-atom-to-state a state depth (first head))))

          (cond
            ((protection-ok state protections1 head) 
             (setq protections protections1))
            (t
             (retract-state-changes state statetag)
             ;; I don't understand why we need to return more than one
             ;; value here. [2006/07/31:rpg]
             (return-from apply-action (values 'fail 'fail protections 0))))

          ;; protections just added are not checked immediately...
          (dolist (a final-adds)
            (when (eql (car a) :protection)
              (setq protections 
                    (add-protection protections (second a) 
                                    depth (first head) state))))

          (trace-print :operators action state "~&PDDL action ~A successfully applied." head-subbed)

          (values head-subbed statetag 
                  protections cost-number
                  unifier depends))))))

;;;---------------------------------------------------------------------------
;;; Helpers for apply-action
;;;---------------------------------------------------------------------------
(defun extract-adds-and-deletes (effect-expr state)
  "This function is intended for use on a PDDL :effect expression.
It partitions the effects into adds and deletes and returns these
two values."
  (case (first effect-expr)
    (and
     (loop for effect in (rest effect-expr)
           with add and delete
           do (multiple-value-setq (add delete)
                  (extract-adds-and-deletes effect state))
           when add
             append add into recursive-adds
           when delete
             append delete into recursive-deletes
           finally (return (values recursive-adds recursive-deletes))))
    (not
     ;; add something to deletes
     (values nil (list (second effect-expr))))
    (forall
     (destructuring-bind (forall vars restr consequent)
         effect-expr
       (declare (ignore forall vars))
       (let ((unifiers
              (shopthpr:find-satisfiers restr state)))
         (when unifiers
           (loop for unifier in unifiers
                 with new-adds and new-deletes
                 do (multiple-value-setq (new-adds new-deletes)
                        (extract-adds-and-deletes
                         (apply-substitution consequent unifier)
                         state))
                 append new-adds into adds
                 append new-deletes into dels
                 finally (return (values adds dels)))))))
    (when
        (when *record-dependencies-p*
          (cerror "Simply continue without recording secondary preconditions"
                  "Do not correctly compute dependencies for PDDL conditional effects."))
        (destructuring-bind (when antecedent consequent)
            effect-expr
          (declare (ignore when))
          (let ((result (shopthpr:find-satisfiers antecedent state t)))
            (when result
              (let ((unifier (first result)))
                (multiple-value-bind (new-adds new-deletes)
                    (extract-adds-and-deletes
                     (apply-substitution consequent unifier)
                     state)
                  (values new-adds new-deletes)))))))
    (otherwise                          ;includes :protection
     ;; normal expression
     (values (list effect-expr) nil))))
        

(defmethod validator-export ((domain simple-pddl-domain) (plan list) stream)
  ;; first check to see if there are costs in the plan...
  (when (numberp (second plan))
    (setf plan (remove-costs plan)))
  (flet ((de-shopify (list)
           (cons
            ;; now the first element will be a list, which isn't
            ;; entirely desirable, but helps us avoid package
            ;; issues. [2007/07/17:rpg]
            (subseq (symbol-name (first list)) 1)
            (rest list))))
    (let ((massaged-plan
           (mapcar #'de-shopify
            (remove-if #'internal-operator-p plan :key 'first))))
      (loop for x in massaged-plan
            as i from 0
            do (format stream "~d: ~a~%" i x)))))
