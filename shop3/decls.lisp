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

(in-package :shop3)

;;;------------------------------------------------------------------------------------------------------
;;; Global Variables
;;;------------------------------------------------------------------------------------------------------

(defvar *all-problems* nil)         ; all problems that have been defined
(defvar *problem* nil)              ;last problem defined
(defvar *shop-version* "VERSION STRING")

;;; Many of these should probably be absorbed into the definition of
;;; PLANNER... [2006/07/05:rpg]

(defparameter *internal-time-limit* nil)  ; maximum time (in units) for execution
(defparameter *internal-time-tag* nil)    ; catch tag to throw to when time expires
(defparameter *time-limit* nil)     ; maximum time (in seconds) for execution
(defparameter *expansions* 0)       ; number of task expansions so far
(defparameter *plans-found* nil)    ; list of plans found so far
(defparameter *plan-tree* nil)      ; whether to return the tree --
                                        ; note that this should NEVER be true if
                                        ; using FIND-PLANS-STACK instead of FIND-PLANS
(defparameter *collect-state* nil)  ; whether to return the final states
;; record of the parents in the tree
(defvar *subtask-parents*)
(declaim (type hash-table *subtask-parents*))

;; table recording a map from operators to the tasks they were
;; done for. This is necessary because the tasks are not identical to
;; the operators (I'm not exactly sure of the difference). [2023/05/25:rpg]
(defvar *operator-tasks*) ; record of the task atom for operators
(declaim (type hash-table *operator-tasks* *task-operator*))
(defvar *task-operator*) ; inverse of *operator-tasks*
(defparameter *optimize-cost* nil)  ; whether to optimize with branch and bound
(defparameter *optimal-plan* 'fail) ; optimal plan found so far
(defparameter *optimal-cost* 0)     ; cost of *optimal-plan*
(defparameter *depth-cutoff* nil)   ; maximum allowable depth for SEEK-PLANS
(defparameter *verbose* 1)          ; default value for VERBOSE in FIND-PLANS
(defparameter *which* :first)       ; default value for WHICH in FIND-PLANS
(defparameter *plan-num-limit* 1)    ; default value for PLAN-NUM-LIMIT in FIND-PLANS
(defparameter *gc* t)        ; whether to call GC each time we call SEEK-PLANS
(defparameter *pp* t)               ; what value to use for *PRINT-PRETTY*
(defparameter *tasklist* nil)       ; initial task list set to nil
(defparameter *protections* nil)    ; initial protection list set to nil
(defparameter *explanation* nil)    ; whether to return explanations

(defvar *print-plans*)              ; whether to print out the plans we find
(defvar *pshort*)  ; whether to skip ops that begin with !! when printing plans
(defvar *print-stats* nil)              ; whether to print statistics
(defparameter *current-plan* nil)  ; current plan
(defparameter *current-tasks* nil) ; current task

(defvar *unifiers-found* nil)   ;associated with *PLANS-FOUND*
(defvar *states-found* nil)             ;associated with *PLANS-FOUND* [2004/09/14:rpg]

(defvar *hand-steer* nil
  "This variable will be DYNAMICALLY bound and used to indicate whether the user
wishes to choose tasks for planning by hand.")
(defvar *leashed* nil
  "This variable will be DYNAMICALLY bound and it will further constrain the behavior
of SHOP when we hand-steer it (see *hand-steer*).  If *leashed* is NIL, SHOP will
simply proceed when choosing tasks, if there is no choice, i.e., when there's only
one task on the open list, or only one :immediate task.  If *leashed* is non-nil,
will consult the user even in these cases.")

(defvar *break-on-backtrack* nil
  "If this variable is bound to T, SHOP will enter a break loop upon backtracking.")

(defvar *more-tasks-p* nil
  "When NIL, it is safe to tail-call any plan-seeking function.")

(defvar *make-analogy-table* nil
  "If this variable is bound to T, SHOP will build a table for analogical replay
\(*analogical-replay-table*\) while planning.
  Currently supported only in FIND-PLANS-STACK.")

(defvar *analogical-replay* nil
  "If this variable is bound to T, SHOP will use analogical replay to provide
guidance in heuristic search while planning.
  Currently supported only in FIND-PLANS-STACK.")

(defvar *unique-method-names* nil
  "This variable will be dynamically bound by code for parsing domains
\(and should *not* be set by users\).  If T, the system will raise an error
if methods do not have unique names.  If :WARN, the system will issue a warning.
If NIL, the system will accept non-unique names quietly.")

(defvar *before-extract-trees-hook* nil
  "This function exists solely for instrumenting for testing purposes.
If this is bound to a function, that function will be called at the top
of EXTRACT-PLAN-TREES.  Generally should be NIL.")

(declaim (type (or null (function () (values &optional)))
               *before-extract-trees-hook*))

(defvar *old-depth* 0
  "For iterative-deepening search, what was the maximum depth of the last iteration,
so we can avoid duplicate plans.")


;;;------------------------------------------------------------------------------------------------------
;;; Compiler Directives and Macros
;;;------------------------------------------------------------------------------------------------------

#+allegro
(top-level:alias "bb" (&optional (val t))
  "Set *break-on-backtrack* variable"
  (setf *break-on-backtrack* val))


;;; It is relatively common practice in Lispworks to use ! as a macro
;;; for :redo.  This will really mess up interpretation of operators
;;; in SHOP, since all operators start with "!".  Thus, we will turn
;;; off macro interpretation of "!" if we are running Lispworks.  see:
;;; http://www.xanalys.com/software_tools/reference/lwl41/lwuser/LWUG_93.HTM

#+:lispworks (set-syntax-from-char #\! #\@)

;;;-----------------------------------------------------------------------
;;; Had to rewrite backquote macro for MCL so it leaves the structure
;;; alone, then write a function to create the structure later.  We're
;;; assuming that "," only appears by itself and not as ",." or ",@".
;;; This is a Allegro Common Lisp compatibility hack.  (swm)

#+:MCL (defparameter *blue-light-readtable* (copy-readtable nil))

#+:MCL (set-macro-character #\`
                           #'(lambda (stream char)
                               (declare (ignore char))
                               (list 'simple-backquote (read stream t nil t  )))
                           nil *blue-light-readtable*)

#+:MCL (set-macro-character #\,
                           #'(lambda (stream char)
                               (declare (ignore char))
                               (list 'bq-comma (read stream t nil t  )))
                           nil *blue-light-readtable*)

(defparameter *back-quote-name*
  #+:MCL 'simple-backquote
  ;; need ,this to make sure it doesn't turn into quote
  #-:MCL (car '`(test ,this)))

(defmacro simple-backquote (x)
  (quotify x))

(defun quotify (x) ; lifted from YAPS; added check for "call" and "eval"
  (cond ((not (consp x)) (list 'quote x))
        ((eql (car x) 'bq-comma) (cadr x))
        ((member (car x) '(call eval)) x)
        (t (let ((type 'list) args)
             (setq args
                   (cons (quotify (car x))
                         (loop for y on x
                               collect (cond ((atom (cdr y))
                                              ;; non-nil tail
                                              (setq type 'list*)
                                              (cdr y))
                                             (t (quotify (cadr y)))))))
             (cons type args)))))

;;;
(declaim (inline primitivep))
(defun primitivep (x)
  "PRIMITIVEP returns T if X is a symbol whose name begins with #\!"
  (locally (declare (optimize (speed 3)))
    (and (symbolp x)
         (let ((cached (get x +primitive-property-name+ :noval)))
           (if (eq cached :noval)
               (setf (get x +primitive-property-name+)
                     (primitive-symbol-p x))
               cached)))))


(defmacro catch-internal-time (&rest body)
  `(if *internal-time-limit*
       (catch *internal-time-tag*
         ,@body)
       (progn ,@body)))

;;; A simple macro to add an item on to the end of a list.
(defmacro push-last (item list)
  `(setf ,list
         (if (null ,list)
             (list ,item)
             (progn
               (setf (cdr (last ,list)) (list ,item))
               ,list))))

#+sbcl
(defmacro defconstant (name value &optional doc)
       `(common-lisp:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))

(defmacro call (fn &rest params)
  `(funcall (function ,fn) ,.params))

;;;---------------------------------------------------------------------------
;;; DOMAINS
;;;---------------------------------------------------------------------------
(defgeneric domain-methods (domain)
  (:documentation "Returns a hash-table mapping complex task names to
methods for expanding such tasks.")
  (:method ((domain-name symbol))
    (domain-methods (find-domain domain-name))))

(defgeneric domain-operators (domain)
  (:documentation "Returns a hash-table mapping primitive task names to
operator definitions.")
  (:method ((domain-name symbol))
    (domain-operators (find-domain domain-name))))

(defgeneric domain-items (domain-designator)
  (:documentation "Return the source code for the items used to define this DOMAIN."))

(defclass actions-domain-mixin ()
  ((methods
    :initarg :methods
    :reader domain-methods
    )
   (operators
    :initarg :operators
    :reader domain-operators
    )
   (methods-to-names
    :initform (make-hash-table :test 'eq)
    :reader domain-method-to-name-table
    :documentation "Map from methods (by object identity) to method IDs (symbols)."
    )
   (names-to-methods
    :initform (make-hash-table :test 'eq)
    :reader domain-name-to-method-table
    :documentation "Map from method IDs (symbols) to methods."
    )
   (operators-dont-fail
    :documentation "Boolean. If true, then it is an error if an operator
fails -- all operators are expected to be put in the plan in states
that satisfy their preconditions. Debugging util."
    :initarg :operators-dont-fail
    :reader operators-dont-fail
    )
   )
  (:default-initargs :operators-dont-fail nil)
  )

(defclass domain (actions-domain-mixin shop3.theorem-prover:thpr-domain)
     ((items
       :documentation "Cached copy of the ITEMS source."
       :reader domain-items))
  (:documentation "An object representing a SHOP domain.")
  )

(defparameter +method-definition-keywords+
  '(:method :pddl-method)
  "This constant holds a list of method definition keywords that are valid for default
SHOP domains.")

(defgeneric method-definition-keywords (domain-designator)
  (:documentation "Keywords that are acceptable for use in method definitions.
Used for checking domain definitions.")
  (:method :around ((domain-name symbol))
    (call-next-method (find-domain domain-name)))
  (:method ((domain domain))
    (declare (ignorable domain))
    +method-definition-keywords+))

(defgeneric domain-method-id-lookup (domain method-id)
  (:method ((domain actions-domain-mixin) (method-id symbol))
    (gethash method-id (domain-name-to-method-table domain))))

(defgeneric domain-id-for-method-lookup (domain method)
  (:method ((domain actions-domain-mixin) (method list))
    (gethash method (domain-method-to-name-table domain))))

(defgeneric assign-domain-method-id (domain method method-id)
  (:method ((domain actions-domain-mixin) method (method-id symbol))
    (with-slots (methods-to-names names-to-methods) domain
     (assert (not (gethash method-id names-to-methods)))
      (setf (gethash method-id names-to-methods) method
            (gethash method methods-to-names) method-id))))


(defmethod domain-items ((domain-name symbol))
  (domain-items (find-domain domain-name)))


(defclass pure-logic-domain-mixin ()
  ()
  (:documentation "A MIXIN indicating that a domain should not use
IF-THEN-ELSE semantics in methods."))

(defmethod print-object ((obj domain) str)
  (print-unreadable-object (obj str :type t)
    (handler-case
        (when (domain-name obj)
          (format str "~a" (domain-name obj)))
      ;; don't have the print-method cough an error if the domain has no name. [2009/03/26:rpg]
      (unbound-slot () nil))))

;;;---------------------------------------------------------------------------
;;; PROBLEMS
;;;---------------------------------------------------------------------------

(defclass problem ()
     ((state-atoms
       :initarg :state-atoms
       :reader state-atoms)
      (tasks
       :initarg :tasks
       :reader tasks)
      (name
       :initarg :name
       :reader problem-name
       :reader name)
      (domain-name
       :initarg :domain-name
       :reader domain-name
       :initform nil
       :documentation
       "The programmer MAY (but is not obligated to) specify that a problem
         is intended for use with a particular domain definition."))
  (:documentation "An object representing a SHOP problem."))

(defmethod make-load-form ((problem problem) &optional environment)
  (declare (ignore environment))
  (with-slots ((problem-name name) domain-name state-atoms tasks) problem
    `(make-problem '(,problem-name ,@ (when domain-name `(:domain ,domain-name)))
                   (quote ,state-atoms)
                   (quote ,tasks))))

(defmethod domain-name ((probspec symbol))
  (domain-name (find-problem probspec t)))

(defmethod print-object ((x problem) stream)
  (if *print-readably*
      (format stream "#.(make-problem '~s ~@['~s~] '~s '~s)"
              (name x) (domain-name x) (state-atoms x) (tasks x))
      (print-unreadable-object (x stream :type t)
        (princ (name x) stream))))

(defgeneric copy-shop-problem (name prob &key problem-class atoms tasks)
  (:method ((name symbol) (prob problem) &key (problem-class 'problem) atoms tasks)
    (apply 'make-problem
           `((,name :type ,(or problem-class (class-name (class-of prob))))
             ,@(when (domain-name prob)
                 (list (domain-name prob)))
             ,(or atoms (state-atoms prob))
             ,(or tasks (tasks prob))))))

;;;---------------------------------------------------------------------------
;;; OPERATORS
;;;---------------------------------------------------------------------------
(deftype operator () '(satisfies operator-p))
(deftype pddl-action () '(satisfies pddl-action-p))

(defstruct (operator :named (:type list))
  "This structure definition was added in order to make the
access to operator attributes self-documenting.  Later, the list
structure could be removed, and a true struct could be used instead."
  (head nil :type list)                 ;this is the operator expression itself...
  preconditions
  deletions
  additions
  (cost-fun nil))

(defun operator-name (operator)
  (first (operator-head operator)))

;;;---------------------------------------------------------------------------
;;; METHODS
;;;---------------------------------------------------------------------------


(defgeneric method-head (domain method)
  (:method ((domain domain) (method list))
    (declare (ignorable domain))
    (assert (member (first method) (method-definition-keywords domain)))
    (second method)))


(defgeneric method-p (domain sexpr)
  (:documentation "Is SEXPR a method object relative to DOMAIN?")
  (:method ((domain domain) obj)
    (declare (ignorable domain obj))
    nil)
  (:method ((domain domain) (sexpr list))
    (declare (ignorable domain))
    (eq (first sexpr) :method)))

(defgeneric method-task (domain sexpr)
  (:method ((domain domain) (sexpr list))
    (second sexpr)))

(defgeneric method-name (domain sexpr)
  (:method ((domain domain) (sexpr list))
    (third sexpr)))

(defgeneric method-preconditions (domain sexpr)
  (:method ((domain domain) (sexpr list))
    (fourth sexpr)))

(defgeneric method-task-net (domain sexpr)
  (:method ((domain domain) (sexpr list))
    (fifth sexpr)))


;;;------------------------------------------------------------------------------------------------------
;;; CLOS Generic Function Definitions
;;;------------------------------------------------------------------------------------------------------
(defgeneric methods (domain task-name)
  (:documentation
   "Return a list of all the SHOP methods for TASK-NAME in DOMAIN.")
  (:method ((domain domain) (task-name symbol))
    (gethash task-name (domain-methods domain)))
  (:method ((domain symbol) (task-name t))
    (methods (find-domain domain) task-name))
  (:method :around (domain task-name)
    (declare (ignorable domain task-name))
    (let ((methods (call-next-method)))
      (unless methods
        (cerror "Continue anyway." 'no-method-for-task :task-name task-name))
      methods)))

(defgeneric sort-methods (domain methods which-plans)
  (:documentation "Sort a list of METHODS in DOMAIN according to WHICH-PLANS.")
  (:method (domain (methods list) (which-plans symbol))
    (declare (ignorable domain))
    methods)
  (:method (domain (methods list) (which-plans (eql :random)))
    (declare (ignorable domain))
    (randomize-list methods)))

(defgeneric sort-results (domain results unifiers which-plans)
  (:documentation
   "Sort lists of RESULTS and UNIFIERS in DOMAIN according to WHICH-PLANS
and return the new RESULTS and UNIFIERS.")
  (:method (domain (results list) (unifiers list) (which-plans symbol))
    (declare (ignorable domain))
    (values results
            unifiers))
  (:method (domain (results list) (unifiers list) (which-plans (eql :random)))
    (declare (ignorable domain))
    (let ((results&unifiers (randomize-list (pairlis results unifiers))))
      (values (mapcar #'car results&unifiers)
              (mapcar #'cdr results&unifiers)))))

(defgeneric operator (domain task-name)
  (:documentation "Return the SHOP operator (if any)
defined for TASK-NAME in DOMAIN.")
  (:method :around ((domain-name symbol) op-name)
    (operator (find-domain domain-name) op-name))
  (:method ((domain domain) (name symbol))
    (gethash name (domain-operators domain))))

(defgeneric install-domain (domain &optional redefine-ok)
  (:documentation "Record DOMAIN for later retrieval.
Currently records the domain on the prop-list of the domain's name.
By default will warn if you redefine a domain.  If REDEFINE-OK is
non-nil, redefinition warnings will be quashed (handy for test suites)."))

(defgeneric handle-domain-options (domain &key type &allow-other-keys)
  (:documentation "Handle the options in option-list,
as presented to defdomain.  These are typically ways of
tailoring the domain object, and should be keyword arguments.
Returns no value; operates by side effects on DOMAIN."))

(defgeneric parse-domain-items (domain items)
  (:documentation "Process all the items in ITEMS.  These will be
methods, operators, axioms, and whatever special items domain
subclasses may require.
  Returns no value; operates by side effects on DOMAIN.
  For example, in one case we define a method on this for a
particular class of domain that adds to the set of items
for the domain a list of standard axioms."))

(defgeneric parse-domain-item (domain item-keyword item)
  (:documentation "The default method for parse-domain-items
will invoke this method to process a single item s-expression.
The addition of this generic function makes SHOP's syntax
more extensible.")
  )

(defgeneric process-operator (domain operator-def)
  (:documentation "This generic function allows for specialization
by domain type on how operator definitions are parsed.
Should return something suitable for installation in the
operators table of DOMAIN."))

(defgeneric process-op (domain operator-def)
  (:documentation "This generic function allows for specialization
by domain type on how operator definitions are parsed.
Should return something suitable for installation in the
operators table of DOMAIN.
  This generic function supports the new syntax for
operators that comes with the :OP keyword instead of
:OPERATOR."))

(defgeneric process-method (domain method-def)
  (:documentation "This generic function allows for
specialization by domain type on how method definitions
are parsed.  Should return something suitable for
installation in the methods table of DOMAIN."))

(defgeneric regularize-axiom (domain axiom-def)
  (:documentation "This generic function allows for
specialization by domain type on how axiom definitions
are parsed.  Should return something suitable for
installation in the axioms table of DOMAIN."))

(defgeneric task-sorter (domain tasks unifier)
  (:documentation
   "This function allows customization of choice of pending task
to expand.  SHOP search behavior can be changed by specializing this
generic function (most likely using the domain argument).  Returns a
sorted list of tasks in the order in which they should be expanded.
A failure can be triggered by returning NIL.")
  (:method ((domain domain) tasks unifier)
    (declare (ignorable unifier))
    tasks))

(defgeneric sort-tasks (domain tasks unifier which-plans)
  (:documentation "Sort the list of pending TASKS in DOMAIN.
This is a replacement for TASK-SORTER that takes an additional
WHICH-PLANS argument for EQL-specialization.")
  (:method (domain tasks unifier (which-plans symbol))
    "Call TASK-SORTER by default for backward compatibility."
    (task-sorter domain tasks unifier))
  (:method (domain (tasks list) unifier (which-plans (eql :random)))
    "Uniformly randomize the list of tasks (originally for Monroe)."
    (declare (ignorable domain unifier))
    (randomize-list tasks)))

(defgeneric process-pre (domain precondition &optional context-name)
  (:documentation "Preprocess the PRECONDITION in accordance with
rules established by the DOMAIN class.  Default method is to
standardize universal quantification, creating new variable
names for the bound variables in quantified expressions."))

(defgeneric process-add-or-delete (domain expression &optional context-name)
    (:documentation "Preprocess add and delete lists, finding the forall conditions and
replacing the variables in them.  Extendable for subtypes of the DOMAIN class."))

(defgeneric process-method-pre (domain precondition method-id &key strict)
  (:documentation "Wrapper around process-pre that takes responsibility for
handling method-specific processing.  If STRICT is non-NIL, warn about implicit
conjunctions.")
  (:method ((domain domain) precondition method-id &key strict)
    (declare (ignore strict))
    (process-pre domain precondition method-id)))

;;;;;; I believe that two changes should be made to this function (at least!):
;;;;;; 1.  It should be renamed to apply-primitive and
;;;;;; 2.  The CLOSage should be modified to make the operators be
;;;;;; objects so that we can dispatch on them.  Currently this isn't
;;;;;; really workable because the operators are lists (so that we can do
;;;;;; things like standardize them).
(defgeneric apply-operator (domain state task-body operator protections depth in-unifier)
  (:documentation
   "If OPERATOR is applicable to TASK in STATE, then APPLY-OPERATOR returns
five values:
1.  the operator as applied, with all bindings;
2.  a state tag, for backtracking purposes;
3.  a new set of protections;
4.  the cost of the new operator;
5.  unifier.
This function also DESTRUCTIVELY MODIFIES its STATE argument.
Otherwise it returns FAIL."))

(defgeneric seek-plans-primitive (domain task1 state tasks top-tasks
                                  partial-plan partial-plan-cost depth which-plans
                                  protections unifier)
  (:documentation "If there is an OPERATOR that is applicable to the primitive TASK1 in STATE,
then SEEK-PLANS-PRIMITIVE applies that OPERATOR, generates the successor state, updates the current
partial plan with the OPERATOR, and continues with planning by recursively calling SEEK-PLANS.
Otherwise, it returns FAIL."))

(defgeneric seek-plans-null (domain state which-plans partial-plan partial-plan-cost depth unifier)
  (:documentation " This SEEK-PLANS-NULL is used with WebServices to strip the add and del lists from
the actions in the partial plan before doing the actual SEEK-PLANS-NULL..."))

(defgeneric seek-plans (domain state tasks top-tasks
                        partial-plan partial-plan-cost depth
                        which-plans protections unifier)
  (:documentation "Top-level task-decomposition function."))

(defgeneric seek-plans-task (domain task1 state tasks top-tasks
                             partial-plan partial-plan-cost depth
                             which-plans protections unifier)
  (:documentation "This is a wrapper function that checks whether the current task to be decomposed is primitive or not,
                   and depending on the task's type, it invokes either SEEK-PLANS-PRIMITIVE or SEEK-PLANS-NONPRIMITIVE,
                   which are the real task-decomposition workhorses."))

(defgeneric seek-plans-nonprimitive (domain task1 state tasks top-tasks
                                     partial-plan partial-plan-cost depth
                                     which-plans protections unifier)
  (:documentation "Applies the HTN method to the current TASK1 and generates the subtasks of TASK.
                   Recursively calls the task-decomposition routine to decompose the subtasks. In the LTML context,
                   What should it do with the OUTPUTS and/or the RESULTS of that method? (TBD) "))

(defgeneric apply-method (domain state task method protections depth in-unifier)
  (:documentation "If METHOD is applicable to TASK in STATE, then APPLY-METHOD returns the
resulting list of reductions.  Otherwise it returns NIL.
   PROTECTIONS are to be passed down the stack and checked whenever we apply an operator
in the context of applying the METHOD.
   DEPTH is used for tracing.
   IN-UNIFIER will be applied to the TASK when applying the method."))

(defgeneric problem->state (domain problem)
  (:documentation "Translate PROBLEM into a list of arguments to make-initial-state.
Allows DOMAIN-specific rules for extracting special problem-components.
  Note that this must return a LIST of arguments for apply, so that, for example,
the default method will return a list whose only element is a list of atoms, not
a simple list of atoms."))

(defgeneric initialize-problem (problem &key)
  (:documentation "Function that does the work of populating a problem.  Allows the programmer
of SHOP extensions to extend or override the normal problem-building.")
  )


(defgeneric get-state (problem)
  (:method ((problem problem))
     (state-atoms problem))
  (:method ((problem-name symbol))
    (get-state (find-problem problem-name t))))

(defgeneric problem-state (problem)
  (:documentation "Alias for get-state.")
  (:method ((problem problem))
     (get-state problem))
  (:method ((name symbol))
    (problem-state (find-problem name t)))
  )

(defgeneric get-tasks (problem)
  (:method ((name symbol))
    (get-tasks (find-problem name t)))
  (:method ((problem problem))
           (tasks problem)))

(defgeneric problem-tasks (problem)
  (:documentation "Alias for get-tasks.")
  (:method ((problem problem))
     (get-tasks problem))
  (:method ((name symbol))
    (problem-tasks (find-problem name t))))

(defgeneric delete-problem (problem)
  (:method ((problem-name symbol))
     (setf *all-problems*
           (delete problem-name *all-problems* :key #'name)))
  (:method ((problem problem))
     (setf *all-problems*
           (delete problem *all-problems*))))

(defgeneric pddl-plan (domain plan &key package)
  (:documentation "Return a PDDL plan representation of PLAN for
DOMAIN (a SHOP ddomain).  When PACKAGE is supplied, put the
symbols into that package (instead of into the value of *PACKAGE*,
which should be the default)."))

(defgeneric plan-cost (plan)
  (:documentation "Return a float that is the cost of the plan argument."))



;;; ERRORP defaults to NIL only for backwards compatibility.  It might be better
;;; to make T be the default. [2015/01/01:rpg]
(defun find-problem (name-or-problem &optional (errorp NIL))
  (if (typep name-or-problem 'problem)
      ;; make FIND-PROBLEM idempotent...
      name-or-problem
      (let* ((name name-or-problem)
             (found
               (find name *all-problems* :key #'name)))
        (cond (found
               found)
              (errorp
               (error "No such problem: ~a" name))
              (t nil)))))




;;;---------------------------------------------------------------------------
;;; Conditions
;;;---------------------------------------------------------------------------
(define-condition shop-condition ()
  ()
  (:documentation "A condition that should be added to all conditions defined in SHOP.")
  )

(define-condition shop-error (shop-condition error)
  ()
  (:documentation "A convenient superclass for SHOP error conditions."))

(define-condition task-arity-mismatch (shop-error)
  (
   (task
    :initarg :task
    :reader task-arity-mismatch-task
    )
   (library-task
    :initarg :library-task
    :reader task-arity-mismatch-library-task
    )
   (library-entry
    :initarg :library-entry
    :reader task-arity-mismatch-library-entry
    )
   )
  (:documentation "An error representing the case where the LIBRARY-ENTRY has a way of performing
LIBRARY-TASK whose arity does not match that of TASK, although the
task keyword of TASK and LIBRARY-TASK are the same.")
  (:report report-task-arity-mismatch))

(defun report-task-arity-mismatch (condition stream)
  (with-slots (task library-task) condition
    (format stream "Arity mismatch between task to plan and task in library:~%Task: ~S~%Task in library: ~S"
            task library-task)))

(define-condition no-method-for-task (shop-error)
  ((task-name
    :initarg :task-name
    ))
  (:report report-no-method))

(defun report-no-method (x str)
  (format str "No method definition for task ~A" (slot-value x 'task-name)))

(define-condition domain-parse-warning (shop-condition warning)
  ()
  )

(define-condition domain-parse-error (shop-condition error)
  ()
  )

(define-condition domain-style-warning (shop-condition
                                        simple-condition style-warning)
  ())

(define-condition implicit-conjunction-warning (domain-style-warning)
  ((bad-conjunction
    :initarg :bad-conjunction)
   (context-type
    :type (member :method :operator)
    :initarg :context-type)
   (context-name
    :initarg :context-name)
   (replacement
    :initarg :replacement))
  (:report (lambda (c stream)
             (with-slots (bad-conjunction context-type context-name replacement) c
               (format stream "~a ~a has implicit conjunction in preconditions:~%~T~s.~%This is deprecated. Rewriting to:~%~t~S~%"
                       (string-upcase (symbol-name context-type))
                       context-name
                       bad-conjunction
                       replacement)))))

(define-condition non-unique-method-name-mixin ()
  ((old-name
    :initarg :old-name
    :reader old-name)
   (task
    :initarg :task
    :reader task))
  (:report (lambda (c stream)
             (format stream "Non-unique method name ~a for task ~a" (old-name c) (task c)))))

(define-condition non-unique-method-name-warning  (non-unique-method-name-mixin domain-style-warning) ())
(define-condition non-unique-method-name-error  (non-unique-method-name-mixin domain-parse-error) ())

(define-condition domain-item-parse-warning (domain-parse-warning)
  ()
  (:documentation "We have a separate class for warnings generated while parsing
  a shop domain.  This is done because the items in the SHOP domain parsing are
  processed in reverse order, so we would like to grab up all of these warnings
  and then re-issue them in lexical order."))


(define-condition singleton-variable (domain-item-parse-warning)
  ((variable-names
    :initarg :variable-names
    :reader variable-names
    )
   (construct-type
    :initarg :construct-type
    :reader construct-type
    )
   (construct-name
    :initarg :construct-name
    :reader construct-name
    )
   (construct
    :initarg :construct
    :reader construct
    :initform nil
    )
   (branch-number
    :initarg :branch-number
    :reader branch-number
    :initform nil
    ))
  (:report (lambda (cond str)
             (format str "Singleton variable~p ~{~a~^, ~} in ~a ~a ~@[branch ~d~]~@[~%~s~]"
                     (length (variable-names cond))
                     (variable-names cond)
                     ;; the messages read oddly when we said "Singleton variable ... in :-..."
                     ;; change to something more meaningful
                     (if (eq (construct-type cond) :-)
                         "AXIOM"
                         (construct-type cond))
                     (construct-name cond)
                     (branch-number cond)
                     (construct cond))))
)

;;; for shop-pprint -- a dispatch table.
(defparameter *shop-pprint-table*
              (copy-pprint-dispatch))

;;;---------------------------------------------------------------------------
;;; Miscellaneous utilities from Monroe
;;;---------------------------------------------------------------------------
(defun randomize-list (list)
  "Return a copy of LIST with all its members in a random order."
  (if (cdr list)
      (nshuffle-list (copy-list list))
      list))

(defun nshuffle-list (list)
  "Shuffle the list using an intermediate vector."
  (let ((array (nshuffle-array (coerce list 'vector))))
    (declare (dynamic-extent array))
    (map-into list 'identity array)))

(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
        do (rotatef (aref array (random i))
                    (aref array (1- i)))
        finally (return array)))

;;;---------------------------------------------------------------------------
;;; Type macro from Phoebe
;;;---------------------------------------------------------------------------
(deftype values! (&rest types)
  "Like `cl:values', but implicit additional values are forbidden."
  (flet ((rest-or-optional-p (thing)
           (member thing '(&optional &rest) :test #'eq)))
    (if (find-if #'rest-or-optional-p types) `(values ,@types)
        `(values ,@types &optional))))
