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

;;; ----------------------------------------------------------------------
;;; Further changes
;;; Copyright(c) 2017  Smart Information Flow Technologies
;;; Air Force Research Lab Contract # FA8750-16-C-0182
;;; Unlimited Government Rights
;;; ----------------------------------------------------------------------
(in-package :shop3)


(defparameter +fluent-updates+
  '(assign increase decrease scale-up scale-down))

(defvar *validator-progname*
  (let ((local-validator (asdf:system-relative-pathname "shop3" "../jenkins/VAL/validate")))
    (if (probe-file local-validator)
        (truename local-validator)
        "validate")))

(defgeneric validator-export (domain plan stream)
  (:documentation "Print a plan in a way that it can be consumed by
the VAL validator."))

(defun write-pddl-plan (shop-plan &key (domain *domain*) (stream t stream-supplied-p) (filename nil))
  "Write the plan argument \(in a way sensitive to the domain type\)
to the FILENAME or STREAM in a format that it can be checked by the validator.
This is an easier to use interface to the validator-export function, qv."
  (let ((shop3-domain
          (etypecase domain
            (domain domain)
            (symbol (find-domain domain)))))
    (when (and filename stream-supplied-p)
      (error "Cannot supply both a filename and stream destination for write-pddl-plan."))
    (when filename
      (setf stream (open filename :direction :output :if-exists :supersede)))
    (unwind-protect
         (validator-export shop3-domain shop-plan stream)
      (when filename (close stream)))))

(defun validate-plan (plan domain-file problem-file &key (validator-progname *validator-progname*) shop3-domain (verbose *verbose*))
  "Check the plan for validity. PLAN can be either lisp list
or can be a filename.  DOMAIN-FILE and PROBLEM-FILE should be PDDL domain and problem
file designators.  VALIDATOR-PROGNAME should be a string that names the
validator in a way that enables it to be found (either an absolute namestring,
absolute pathname, or a path-relative namestring)."
  (let ((domain-file (etypecase domain-file
                       (string domain-file)
                       (pathname (namestring domain-file))))
        (problem-file (etypecase problem-file
                       (string problem-file)
                       (pathname (namestring problem-file)))))
    (flet ((validate-plan (plan-filename)
             (multiple-value-bind (output error-output exit-code)
                 ;; -x option needed to exit with non-zero exit code on bad plan.
                 (uiop:run-program (format nil "~a -x ~a ~a ~a" validator-progname domain-file problem-file plan-filename)
                                   :ignore-error-status t
                                   :error-output :string :output :string)
               (setf output (string-left-trim (list #\return) output))
               (if (zerop exit-code)
                   (progn (when (> verbose 1)
                            (format t "~&Validator output was: ~a~% Exit code was: ~d" output exit-code))
                          t)
                   (progn
                     (format t "~&Validation of plan-file ~a failed with messages:~%~T~A~%~T~A" plan-filename output error-output)
                     nil)))))
      (if (stringp plan)
          (validate-plan plan)
          (let (validated)
            (unless shop3-domain
              (setf shop3-domain (make-instance 'domain)))
            (uiop:with-temporary-file (:stream str :pathname path :keep (not validated))
              (write-pddl-plan plan :domain shop3-domain :stream str)
              :close-stream
              (setf validated (validate-plan path))
              validated))))))


;;;---------------------------------------------------------------------------
;;; Class definitions for domains, used to control processing of PDDL
;;; syntax.[2006/08/01:rpg]
;;;---------------------------------------------------------------------------


(defclass simple-pddl-domain ( domain )
  ((source-pddl-domain
    :initarg :source-pddl-domain
    :reader source-pddl-domain
    ))
  (:documentation "A new class of SHOP3 domain that permits inclusion of
PDDL operator definitions.")
  )

(defgeneric pddl-method-p (domain sexpr)
  (:method ((domain domain) obj)
    (declare (ignorable domain obj))
    nil)
  (:method ((domain simple-pddl-domain) (sexpr list))
    (declare (ignorable domain))
    (or (eq (first sexpr) :pddl-method)
        (call-next-method))))


(defmethod method-p ((domain simple-pddl-domain) (sexpr list))
  (or (pddl-method-p domain sexpr)
      (call-next-method)))

(defmethod method-name ((domain simple-pddl-domain) (sexpr list))
  (or
   (and (pddl-method-p domain sexpr)
        (third sexpr))
   (call-next-method)))


(defclass negative-preconditions-mixin ()
  ()
  )

(defclass disjunctive-preconditions-mixin ()
  ()
  )

(defclass universal-preconditions-mixin ()
     ()
  )

(defclass existential-preconditions-mixin ()
     ()
  )

(defclass quantified-preconditions-mixin (universal-preconditions-mixin existential-preconditions-mixin)
  ()
  )

(defclass conditional-effects-mixin ()
     ()
  )

(defclass equality-mixin ()
     ()
  )

;; FIXME: any domain with PDDL types has the types as static predicates...
(defclass pddl-typing-mixin ()
  ((pddl-types
    :accessor pddl-types
    :initarg :%pddl-types              ; don't use the initform -- it's for testing.
   )))

(defclass costs-mixin ()
  ())

(defclass adl-mixin (pddl-typing-mixin negative-preconditions-mixin disjunctive-preconditions-mixin equality-mixin quantified-preconditions-mixin conditional-effects-mixin)
  ())

(defclass pddl-domain ( conditional-effects-mixin quantified-preconditions-mixin equality-mixin
                       static-predicates-mixin
                       pddl-typing-mixin
                       simple-pddl-domain )
  ()
  (:documentation "A new class of SHOP3 domain that permits inclusion of
PDDL operator definitions.  Right now, this approximates what happens if you have
the :adl flag in a PDDL domain file.")
  )

(defclass adl-domain (simple-pddl-domain adl-mixin)
  ())


(defgeneric process-action (domain action-def)
  (:documentation "PDDL has actions \(vanilla SHOP3 has operators\), so we
add code for processing the actions that is parallel to process-operator."))

;;;---------------------------------------------------------------------------
;;; Structure for PDDL actions -- plays a role akin to the role played
;;; by the OPERATOR defstruct in vanilla shop3.
;;;---------------------------------------------------------------------------
(defstruct (pddl-action :named (:type list))
  (head nil :type list)
  precondition
  effect
  (cost-fun nil))


(defun pddl-action-name (pddl-action)
  (first (pddl-action-head pddl-action)))


(defmethod parse-domain-items :around ((domain fluents-mixin) items)
  (let ((fluents-axioms `((:- (is-fluent-function (?name . ?_))
                              ((call fluent-function-p ,domain '?name)))
                          (:- (fluent-check ?op ?x ?y)
                              ((call fluent-expr-p ,domain '?x)
                               (call fluent-expr-p ,domain '?y)
                               (f-exp-value ?x ?x-val)
                               (f-exp-value ?y ?y-val)
                               (call ?op ?x-val ?y-val))))))
    (set-variable-property domain fluents-axioms)
    ;; the definition of functions must come before the other items we parse
    ;; for reasons I don't understand, the items are parsed in reverse
    ;; order, so we move the types definition to the
    ;; end. [2017/08/02:rpg]
    (let* ((functions-def (find :functions items :key 'first))
           (types-def (find :types items :key 'first))
           (innocuous-items
             (remove-if #'(lambda (x) (or (eq x :types) (eq x :functions)))
                        items :key 'first)))
      (unless functions-def (warn "Unable to find definitions of functions in domain."))
      (setf (slot-value domain 'shop.theorem-prover::fluent-functions) (mapcar #'first (rest functions-def)))
      (call-next-method domain `(,@fluents-axioms
                                 ,@innocuous-items
                                 ,@(when functions-def (list functions-def))
                                 ,@(when types-def (list types-def)))))))


;;; FIXME: if the preconditions are rewritten properly, we won't need
;;; the metric equality axiom.
(defmethod parse-domain-items :around ((domain equality-mixin) items)
  "Add the axiom that treats equality as a built-in predicate.  This should
later be compiled into find-satisfiers or something."
  (let* ((basic-equality-axiom '(:- (= ?x ?x) nil))
         (equality-axioms (list basic-equality-axiom))
         (metric-equality-axiom '(:- (= ?x ?y)
                                  ((fluent-check = ?x ?y)))))
    (when (typep domain 'fluents-mixin)
      (setf equality-axioms (cons metric-equality-axiom equality-axioms)))
    (set-variable-property domain equality-axioms)
    (call-next-method domain (append equality-axioms items))))

(defmethod parse-domain-items :around ((domain pddl-typing-mixin) items)
  ;; the definition of types must come before the other items we parse
  ;; for reasons I don't understand, the items are parsed in reverse
  ;; order, so we move the types definition to the
  ;; end. [2017/08/02:rpg]
  (let ((types-def (find :types items :key 'first)))
    ;; if there's no type definition, add a trivial one.
    (unless types-def (setf (pddl-types domain) '(object)))
    (call-next-method domain `(,@(if types-def (remove types-def items) items)
                               ,@(when types-def (list types-def))))))

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':action)) item)
   (let ((op-name (second item)))
     ;; do some nasty voodoo to give PDDL actions names like SHOP3
     ;; operators [2006/07/31:rpg]
     (unless (eql (elt (symbol-name op-name) 0) #\!)
       (setf op-name (intern (concatenate 'string
                                          "!" (symbol-name op-name))
                             (symbol-package op-name)
                             ))
;;;      (format t "~&Making new SHOP3 operator name ~S from old name ~S.~%"
;;;           op-name (second item))
       (setf (second item) op-name))
     (with-slots (operators) domain
       (when (gethash op-name operators)
         (error "There is more than one operator named ~s" op-name))
       (setf (gethash op-name operators) (process-action domain item)))))

;;; default method so we don't try to parse constructs inappropriately
(defmethod parse-domain-item ((domain domain) (item-key (eql :pddl-method)) item)
   (declare (ignorable domain item-key item))
   (error "This domain is not a subclass of SIMPLE-PDDL-DOMAIN, so PDDL-methods cannot be used."))

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':predicates)) item)
   "For now, since SHOP doesn't typecheck, we simply ignore these declarations."
   (declare (ignore item))
   (values))

(defmethod parse-domain-item ((domain pddl-typing-mixin) (item-key (eql ':types)) item)
  (assert (every 'symbolp (rest item)))
  (multiple-value-bind (types super-types)
      (parse-typed-list (rest item))
    (setf (pddl-types domain) (union '(object) types))
    (loop :for type :in types
          :as super-type :in super-types
          :do (parse-domain-item domain ':- `(:- (,super-type ?x) (,type ?x)))))
  (values))

;;;---------------------------------------------------------------------------
;;; Metric function declarations.
;;;---------------------------------------------------------------------------

(defmethod parse-domain-item ((domain fluents-mixin) (item-key (eql ':functions)) item)
  (when (member '- item)
    (error "Don't support the typing of fluents (which can only be number anyway."))
  (when (typep domain 'pddl-typing-mixin)
    (warn "Haven't yet implemented function type checking."))
  (values))

;;;---------------------------------------------------------------------------
;;; Requirements
;;;---------------------------------------------------------------------------

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql ':requirements)) item)
  "For now, simply ignore requirements.  Later we should check that they match the domain class."
  (declare (ignorable domain item-key item))
  (values))

(defmethod process-action ((domain simple-pddl-domain) action-def)
  (destructuring-bind (keyword action-symbol &key parameters precondition effect
                                                  (cost 1.0))
      action-def
    (unless (eq keyword :action)
      (error "Unexpected action expression: ~A" action-def))
    ;; this takes care of processing variables and marking primitive
    ;; actions [2006/08/01:rpg]
    (set-variable-property domain action-def)
    (multiple-value-bind (param-vars param-types)
        (typed-list-vars parameters domain)
      (let ((precond
              (translate-precondition domain precondition))
            ;; add a precondition that checks for the action to be well-typed.
            (type-precond
              (cons 'and
                (iter (for pair in (of-type-exprs param-vars param-types))
                  (destructuring-bind (type obj) pair
                    (collecting `(enforce ,pair
                                          "Parameter ~a unbound or ill-typed. Should be ~a"
                                          (quote ,obj) (quote ,type)))))))
            (eff
              (translate-effect domain effect))
            (head (cons action-symbol param-vars)))
        (make-pddl-action :head head
                          :precondition
                          (if (typep domain 'pddl-typing-mixin)
                              `(and ,type-precond ,precond)
                              precond)
                          :effect eff
                          :cost-fun cost)))))

;;;---------------------------------------------------------------------------
;;; Additional generic functions, used to tailor the translation of
;;; PDDL actions into SHOP3 syntax.  The different PDDL constructs are
;;; gated by PDDL requirements flags, which are represented as mixins
;;; in PDDL domains.
;;;---------------------------------------------------------------------------
(defgeneric translate-precondition (domain expression)
  (:documentation
   "This generic function is used to translate PDDL-style preconditions
into SHOP3-style syntax.  The rewriting is done based on the domain so
that different syntax features can be turned on and off."))

(defgeneric translate-effect (domain expression)
  (:documentation
   "This generic function is used to translate PDDL-style effects
into SHOP3-style syntax.  The rewriting is done based on the domain so
that different syntax features can be turned on and off."))

;;;---------------------------------------------------------------------------
;;; Methods for translate-effect
;;;---------------------------------------------------------------------------


(defmethod translate-effect ((domain simple-pddl-domain) effect)
  "Basis method for translating a PDDL effect into SHOP3 syntax is
to just leave it alone."
  effect)

(defmethod translate-effect ((domain conditional-effects-mixin) effect)
  "This method translates any forall expressions in a PDDL effect into the
slightly-different SHOP3 FORALL syntax.
It then invokes the next method, to insure that all PDDL - SHOP3 constructs are
translated."
  ;;; FIXME: don't we need to translate existential variables, as well?
  (let ((new-effect (translate-pddl-quantifier effect 'forall domain)))
    (call-next-method domain new-effect)))

;;; this is done as an :around method because it must be done before any
;;; other translations change processing of the action...
(defmethod translate-effect :around ((domain fluents-mixin) effect)
  "This method translates any metric effects into updates using ASSIGN-FLUENT.
It then invokes the next method, to insure that all the other
PDDL - SHOP3 constructs are translated."
  (let ((new-effect (translate-metric-updates domain effect)))
    (call-next-method domain new-effect)))

(defun translate-metric-updates (domain expression)
  (labels ((iter (expr)
             ;; is this a fluent update?
             (cond ((and (listp expr)
                         (member (first expr) +fluent-updates+)
                         (or
                          ;; first check for SHOP ASSIGN vs. PDDL ASSIGN
                          (and (eq (first expr) 'assign)
                               (fluent-function-p domain (first (second expr))))
                          ;; otherwise this is an increase or decrease...
                          (and (not (eq (first expr) 'assign))
                               (prog1 t
                                 (assert (fluent-function-p domain (first (second expr)))
                                         ()
                                         "Ill-formed fluent update expression:~s~
                                           first argument ~s should be a fluent function."
                                         (second expr) expr)
                                 (assert (fluent-expr-p domain (third expr))
                                         ()
                                         "Ill-formed fluent update expression: ~s~
                                           second argument ~s should be a fluent expression (value to assign)"
                                         expr (third expr))))))
                    `(fluent-update ,@expr))
                   ((and (listp expr) (symbolp (first expr)))
                    (case (first expr)
                      ((and not) `(,(first expr) ,@(mapcar #'iter (rest expr))))
                      (when `(when ,(translate-fluent-precond domain (second expr))
                               ,(iter (third expr))))
                      ((exists forall)
                       (destructuring-bind (quant vars body) expr
                         `(,quant ,vars ,(iter body))))
                      (otherwise
                       expr)))
                   ((atom expr) expr))))
    (iter expression)))

;;;---------------------------------------------------------------------------
;;; Methods for translate-precondition
;;;---------------------------------------------------------------------------

(defmethod translate-precondition ((domain simple-pddl-domain) expression)
  "Basis method for translating a PDDL precondition into SHOP3 syntax is
to just leave it alone."
  expression)

(defmethod translate-precondition ((domain universal-preconditions-mixin) expression)
  "This method translates any forall expressions in a PDDL precondition into the
slightly-different SHOP3 FORALL syntax.
It then invokes the next method, to insure that all PDDL - SHOP3 constructs are
translated."
  (let ((new-expr (translate-pddl-quantifier expression 'forall domain)))
    (call-next-method domain new-expr)))

(defmethod translate-precondition ((domain existential-preconditions-mixin) expression)
  "This method translates any exists expressions in a PDDL precondition into the
slightly-different SHOP3 EXISTS syntax.
It then invokes the next method, to insure that all PDDL - SHOP3 constructs are
translated."
  (let ((new-expr (translate-pddl-quantifier expression 'exists domain)))
    (call-next-method domain new-expr)))

(defmethod translate-precondition :around ((domain fluents-mixin) expression)
  (let ((new-expr (translate-fluent-precond domain expression)))
    (call-next-method domain new-expr)))

(defun translate-fluent-precond (domain expression)
  "Find the fluent comparison expressions in EXPRESSION and rewrite them
into expressions of the form (FLUENT-CHECK <COMP-OP> <F-EXP> <F-EXP>) for
the theorem-prover."
  (labels ((iter (expr)
             ;; is this a fluent expression?
             (cond ((and (listp expr)
                         (= (length expr) 3)
                         (member (first expr) +numerical-comparisons+ :test 'eq)
                         (or (not (eq (first expr) '=)) ; this must be a fluent comparision
                             ;; for equality predications, we need to be heuristic.
                             (fluent-expr-p domain (second expr)))
                         `(fluent-check ,@expr)))
                   ((and (listp expr) (symbolp (first expr)))
                    (case (first expr)
                      ((and or not imply) `(,(first expr) ,@(mapcar #'iter (rest expr))))
                      ((exists forall)
                       (destructuring-bind (quant vars body) expr
                         `(,quant ,vars ,(iter body))))
                      (otherwise
                       expr)))
                   (t expr))))
    (iter expression)))

;;;---------------------------------------------------------------------------
;;; Including PDDL domains in our domains...
;;;---------------------------------------------------------------------------

(defmethod domain-include-parse ((parent-domain simple-pddl-domain) domain-name path)
  (declare (ignorable parent-domain))
  (flet ((string-match-symbol (sym match-me)
           (equalp (symbol-name sym) (symbol-name match-me))))
    (let ((domain-form
            (with-open-file (str path :direction :input)
              (let ((*package* *package*))
                (iter
                  (for x = (let ((*read-eval* nil))
                             (read str nil nil)))
                  (while x)
                  (cond ((eq (car x) 'in-package)
                         (set '*package* (find-package (second x))))
                        ((and (string-match-symbol (car x) 'define)
                              (string-match-symbol (first (second x)) 'domain)
                              (string-match-symbol (second (second x)) domain-name))
                         (return x)))
                  (finally (return nil)))))))
      (if domain-form
          ;; return the items
          (cddr domain-form)
          ;; could be a SHOP3 domain you're including
          (call-next-method)))))


;;;
;;;---------------------------------------------------------------------------
;;; Helper functions
;;;---------------------------------------------------------------------------

(defun translate-pddl-quantifier (expression quantifier &optional (domain *domain*))
  "Translate EXPRESSION from PDDL quantifier \(forall and exists\) notation
into SHOP3 quantifier notation \(and adds some
\(<type> <var>\) conditions\)."
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
                   (typed-list-vars typed-list domain)
                 `(,quantifier ,vars
                          ,(let ((exprs (of-type-exprs vars types)))
                             (if (= (length exprs) 1)
                                 (first exprs)
                                 (cons 'and exprs)))
                          ,(iter sub-expr))))))
    (iter expression)))

(defun of-type-exprs (vars types)
  "Because SHOP3 doesn't have the same typing, we take variables
and their types and generate a list of distinguished propositions
we can use in preconditions."
  (loop for v in vars
        as type in types
        collect `(,type ,v)))


(defun typed-list-vars (typed-list &optional (domain *domain*))
  "Takes a typed-list (this is a PDDL construct) as argument and
pulls out the variables and types and returns two parallel
lists of variable names and type names."
  (set-variable-property domain typed-list)
  (multiple-value-bind (vars types)
      (parse-typed-list typed-list)
    (assert (every #'(lambda (x) (variable-p x)) vars))
    (let ((undefined (remove-duplicates (remove-if #'(lambda (type) (member type (pddl-types domain))) types))))
      (when undefined
        (error "Reference to undefined PDDL type~P: ~S"
               (length undefined) undefined)))
    (values vars types)))

(defun parse-typed-list (typed-list)
  "Takes a typed-list (this is a PDDL construct) as argument and
pulls out the declared items and types and returns two parallel
lists of declared names and type names."
  (loop with lst = typed-list
        with counter = 0
        until (null lst)
        if (eq (first lst) '-)
          append (make-list counter :initial-element (second lst)) into types
          and do (setf lst (cddr lst)
                       counter 0)
        else
          collect (first lst) into items
          and do (setf lst (cdr lst))
                 (incf counter)
        finally (return (values items
                                (if (= counter 0) types
                                    ;; this handles the case where the
                                    ;; end of the type list doesn't
                                    ;; have a type declaration:
                                    ;; defaults to OBJECT.
                                    (append types (make-list counter :initial-element 'shop3:object)))))))

;;;---------------------------------------------------------------------------
;;; Apply-action, which plays a role akin to apply-operator in vanilla
;;; SHOP3.
;;;---------------------------------------------------------------------------
(defun apply-action (domain state task-body action protections depth
                     in-unifier)
  "If ACTION, a PDDL ACTION, is applicable to TASK in STATE, then
APPLY-ACTION returns six values:
1.  the operator as applied, with all bindings;
2.  a state tag, for backtracking purposes;
3.  a new set of protections;
4.  the cost of the new operator;
5.  unifier;
6.  dependencies for primary and secondary effects, if *record-dependencies-p*
    is true.
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

    ;; FIXME: somehow SBCL knows that the IN-UNIFIER here can't be null, but I'm not sure how.
    (let ((action-unifier (unify head (apply-substitution task-body in-unifier))))

      (when (eql action-unifier 'fail)
        (return-from apply-action 'fail))

      ;; everything below is "if action-unifier != fail"

      (setf action-unifier
            (compose-substitutions action-unifier in-unifier))
      ;; first check the preconditions, if any
      (if preconditions
          (let ((pre (apply-substitution preconditions action-unifier)))

            ;; need to specially handle the preconditions, since the
            ;; syntax of PDDL preconditions are different from
            ;; SHOP3. [2006/07/31:rpg]
            (multiple-value-bind (pu pd)
                ;; finding only one satisfier for the preconditions is
                ;; justified by the syntactic constraint that PDDL
                ;; preconditions cannot introduce any new variables
                ;; scoped over the effects, and the (not enforced)
                ;; constraint that all actions should be ground when
                ;; introduced into the plan by SHOP3.
                (find-satisfiers pre state :just-one t :level (1+ depth) :domain domain)
              (unless pu
                (trace-print :operators (first head) state
                             "~2%Depth ~s, inapplicable action ~s~%     task ~s.~%     Precondition failed: ~s.~%"
                             depth
                             (first head)
                             (apply-substitution task-body unifier)
                             pre
                             )
                (return-from apply-action 'fail))
              (setf unifier (compose-substitutions action-unifier (first pu))
                    ;; first of pd and pu because we find only one satisfier of the preconditions.
                    depends (first pd))))
          (setq unifier action-unifier)))
    ;; end of scope for action-unifier...

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

      (multiple-value-bind (final-adds final-dels
                            secondary-depends) ; dependencies for secondary preconditions
          (extract-adds-and-deletes domain effect-subbed state (1+ depth))


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
            ((protection-ok domain state protections1 head)
             (setq protections protections1))
            (t
             (retract-state-changes state statetag)
             ;; I don't understand why we need to return more than one
             ;; value here. [2006/07/31:rpg]
             (return-from apply-action 'fail)))

          ;; protections just added are not checked immediately...
          (dolist (a final-adds)
            (when (eql (car a) :protection)
              (setq protections
                    (add-protection protections (second a)
                                    depth (first head) state))))

          (trace-print :operators action state "~&PDDL action ~A successfully applied." head-subbed)

          (values head-subbed statetag
                  protections cost-number
                  unifier (shop.theorem-prover::rd-union depends secondary-depends)))))))

;;;---------------------------------------------------------------------------
;;; Helpers for apply-action
;;;---------------------------------------------------------------------------
(defun extract-adds-and-deletes (domain effect-expr state depth)
  "This function is intended for use on a PDDL :effect expression.
It partitions the effects into adds and deletes and returns these
two values, together with a set of dependencies as a third value,
if dependency tracking is turned on."
  (case (first effect-expr)
    (and
     (iter (for effect in (rest effect-expr))
       (with recursive-deps)
       (multiple-value-bind (add delete deps)
           (extract-adds-and-deletes domain effect state (1+ depth))
         (appending add into recursive-adds)
         (appending delete into recursive-deletes)
         (when (and deps *record-dependencies-p*)
           (setf recursive-deps
                 (shop3.theorem-prover::rd-union recursive-deps deps))))
       (finally
        (return
          (values recursive-adds recursive-deletes recursive-deps)))))
    (fluent-update
     (compute-fluent-updates effect-expr state domain depth))
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
           (iter (for unifier in unifiers)
             (with depends)
             (multiple-value-bind (new-adds new-deletes new-depends)
                 (extract-adds-and-deletes
                  domain
                  (apply-substitution consequent unifier)
                  state (1+ depth))
               (appending new-adds into adds)
               (appending new-deletes into dels)
               (when (and new-depends *record-dependencies-p*)
                 (setf depends (shop3.theorem-prover::rd-union depends new-depends))))
             (finally (return (values adds dels depends))))))))
    (when
        (destructuring-bind (when antecedent consequent)
            effect-expr
          (declare (ignore when))
          (multiple-value-bind (result sp-dependencies) ;secondary precondition dependencies
              (shopthpr:find-satisfiers antecedent state :just-one t :level (1+ depth) :domain domain)
            (when result
              (let ((unifier (first result)))
                (multiple-value-bind (new-adds new-deletes new-depends)
                    (extract-adds-and-deletes domain
                                              (apply-substitution consequent unifier)
                                              state (1+ depth))
                  (values new-adds new-deletes (when *record-dependencies-p*
                                                 (shop3.theorem-prover::rd-union (first sp-dependencies) new-depends)))))))))
    (otherwise                          ;includes :protection
     ;; normal expression
     (values (list effect-expr) nil))))

(defun compute-fluent-updates (effect-expr state domain depth)
  "Translate a FLUENT-UPDATE EFFECT-EXPR into a set of ADD and DELETES
\(these will have as predicate FLUENT-VALUE\), and optionally return a
set of dependencies."
  (let (new-deps)
    (destructuring-bind (word op fluent-function new-value-expr)
        effect-expr
      (declare (ignore word))
      ;; recover the old fluent value
      (let* ((fluent-goal `(f-exp-value ,fluent-function ?value))
             (old-val (multiple-value-bind (unifiers deps)
                          (shopthpr:find-satisfiers (list fluent-goal) state
                                                    :level (1+ depth)
                                                    :just-one t :domain domain)
                        (assert unifiers ()
                                "Unable to find current value for ~s in fluent update"
                                fluent-function)
                        (setf new-deps (first deps))
                        (apply-substitution '?value (first unifiers)))))
        (let* ((update-val (cond ((fluent-expr-p domain new-value-expr)
                                  (multiple-value-bind (unifiers deps)
                                      (shopthpr:find-satisfiers `(f-exp-value ,new-value-expr ?val)
                                                                state :domain domain
                                                                      :level (1+ depth)
                                                                      :just-one t)
                                    (assert unifiers ()
                                            "Unable to find update value ~s in ~s"
                                            new-value-expr effect-expr)
                                    (assert (= (length unifiers) 1) ()
                                            "Ill-defined update value -- multiple bindings for ~s in ~s"
                                            new-value-expr effect-expr)
                                    (when *record-dependencies-p*
                                      (setf new-deps
                                            (shopthpr::rd-union (first deps) new-deps)))
                                    (apply-substitution '?val (first unifiers))))
                                 ((numberp new-value-expr)
                                  new-value-expr)
                                 (t (error "Could not evaluate fluent update expression ~s in ~s"
                                           new-value-expr effect-expr))))
               ;; apply the op
               (new-val (ecase op
                          (assign update-val)
                          (scale-up (* old-val update-val))
                          (scale-down (* old-val update-val))
                          (increase (+ old-val update-val))
                          (decrease (- old-val update-val)))))
          (values
           ;; add the new value to the adds
           (list `(fluent-value ,fluent-function ,new-val))
           ;; delete old value
           (list `(fluent-value ,fluent-function ,old-val))
           new-deps))))))


(defmethod validator-export ((domain domain) (plan list) stream)
  (loop :for x :in (pddl-plan domain plan)
        :as i :from 0
        :do (format stream "~d: " i)
            (princ x stream)
            (terpri stream)))

(defmethod pddl-plan ((domain domain) (plan cons) &key (package *package*))
    ;; first check to see if there are costs in the plan...
  (when (numberp (second plan))
    (setf plan (remove-costs plan)))
  (flet ((de-shopify (list)
           (cons
            ;; now the first element will be a string, which isn't
            ;; entirely desirable, but helps us avoid package
            ;; issues. [2007/07/17:rpg]
            (intern (string-left-trim (list #\!) (symbol-name (first list))) package)
            (rest list))))
    (mapcar #'de-shopify
            (remove-if #'internal-operator-p plan :key 'first))))

(defmethod parse-domain-item ((domain simple-pddl-domain) (item-key (eql :pddl-method)) method)
  (multiple-value-bind (method-obj method-id)
      (parse-pddl-method domain method)
    (index-method-on-domain domain method-id method-obj)))

(defun parse-pddl-method (domain method)
  (let* ((method (uniquify-anonymous-variables method))
         (method-head (second method))
         (method-task-name (car method-head))
         #+ignore(task-variables (harvest-variables method-head))
         (pre (if (symbolp (third method)) (fourth method) (third method)))
         (method-name (if (symbolp (third method))
                          (third method)
                          (gensym (format nil "~A"
                                          method-task-name))))
         (task-net (if (symbolp (third method)) (fifth method) (fourth method)))
         ;; FIXME: later we should check singleton variables...
         #+ignore var-table)
    (when (typep domain 'fluents-mixin)
      (setf pre (translate-fluent-precond domain pre)))
    (when (gethash method-name *all-method-names* nil)
      (ecase *unique-method-names*
        (t
         (let ((new-name (gensym (symbol-name method-name))))
           (cerror (format nil "Rename to ~s" new-name)
                   'non-unique-method-name-error :old-name method-name)
           (setf method-name new-name)))
        (:warn (warn 'non-unique-method-name-warning :old-name method-name))
        ((nil))))
    (setf (gethash method-name *all-method-names*) t)
    (labels ((process-task-list (tasks)
               (cond
                 ((null tasks) (list :ordered (list :task '!!inop)))
                 ((member (first tasks) '(:ordered
                                          ;; unordered not yet supported
                                          #+ignore
                                          :unordered))
                  (cons (first tasks)
                        (mapcar #'process-task-list (rest tasks))))
                 ((eq (first tasks) :task)
                  tasks)
                 ((atom (first tasks))
                  (cons :task tasks))
                 (t
                  (error "Illegal task network: ~S" tasks)))))
      ;; FIXME: later we should check singleton variables...
      #+ignore (setf var-table (harvest-variables method-head))
      ;; now we need to harvest only FREE variables in the preconditions and task net...
      ;; answer will be (:pddl-method <head> ...)
      (values
       ;; here's the transformed METHOD
       `(,(first method)
         ,method-head
         ,method-name
         ,(process-pddl-method-pre domain pre)
         (quote ,(process-task-list task-net)))
       method-name)
      ;; FIXME: just before returning, check for singletons in the method's head
      #+ignore
      (check-for-singletons task-variables :context-tables body-var-tables
                                           :construct-type (first method)
                                           :construct-name method-task-name
                                           :construct method))))

(defun process-pddl-method-pre (domain pre)
  "This is the main function that does the pre-processing, it
looks through the preconditions finding the forall
 conditions and replacing the variables in that condition."
  (if (eq (first pre) :sort-by)
      (ecase (length pre)
        (3 `(:sort-by ,(second pre) ,(translate-precondition domain (third pre))))
        (4 `(:sort-by ,(second pre) ,(third pre) ,(translate-precondition domain (fourth pre)))))
      (translate-precondition domain pre)))

;;; Rewrite for the case where we have methods in SHOP that use SHOP's FORALL, in a domain
;;; where the operators use PDDL's FORALL.
(defmethod process-method-pre :around ((domain universal-preconditions-mixin) precond method-name &key strict)
  (declare (ignore strict))
  (call-next-method domain (subst 'shop-forall 'forall precond) method-name))

;;;---------------------------------------------------------------------------
;;; Process problem initial state for metric fluents
;;;---------------------------------------------------------------------------
(defmethod make-initial-state :around ((domain fluents-mixin) state-encoding atoms &key)
  "Rewrite initial state facts of the form (= <fluent-function> <value>) into
new facts of the form (fluent-value fluent-function value)."
  (call-next-method domain state-encoding
                    (iter (for atom in atoms)
                      (if (and (eq (first atom) '=)
                               (fluent-function-p domain (first (second atom))))
                          (collecting `(fluent-value ,(second atom) ,(third atom)))
                          (collecting atom)))))
