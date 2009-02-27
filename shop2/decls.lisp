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

;;;------------------------------------------------------------------------------------------------------
;;; Global Variables
;;;------------------------------------------------------------------------------------------------------

(defvar *all-problems* nil)         ; all problems that have been defined

;;; Many of these should probably be absorbed into the definition of
;;; PLANNER... [2006/07/05:rpg]

(defparameter *internal-time-limit* nil)  ; maximum time (in units) for execution
(defparameter *internal-time-tag* nil)    ; catch tag to throw to when time expires
(defparameter *time-limit* nil)     ; maximum time (in seconds) for execution
(defparameter *expansions* 0)       ; number of task expansions so far
(defparameter *plans-found* nil)    ; list of plans found so far
(defparameter *plan-tree* nil)      ; whether to return the tree
(defparameter *subtask-parents* nil) ; record of the parents in the tree
(defparameter *operator-tasks* nil) ; record of the task atom for operators
(defparameter *optimize-cost* nil)  ; whether to optimize with branch and bound
(defparameter *optimal-plan* 'fail) ; optimal plan found so far
(defparameter *optimal-cost* 0)     ; cost of *optimal-plan*
(defparameter *depth-cutoff* nil)   ; maximum allowable depth for SEEK-PLANS
(defparameter *verbose* 1)          ; default value for VERBOSE in FIND-PLANS
(defparameter *which* :first)       ; default value for WHICH in FIND-PLANS
(defparameter *gc* t)        ; whether to call GC each time we call SEEK-PLANS
(defparameter *pp* t)               ; what value to use for *PRINT-PRETTY*
(defparameter *tasklist* nil)       ; initial task list set to nil
(defparameter *protections* nil)    ; initial protection list set to nil
(defparameter *explanation* nil)    ; whether to return explanations

(defvar *print-plans*)              ; whether to print out the plans we find
(defvar *pshort*)  ; whether to skip ops that begin with !! when printing plans
(defvar *print-stats*)              ; whether to print statistics
(defparameter *current-plan* nil)  ; current plan
(defparameter *current-tasks* nil) ; current task

(defvar *unifiers-found* nil)   ;associated with *PLANS-FOUND*
(defvar *states-found* nil)             ;associated with *PLANS-FOUND* [2004/09/14:rpg]

(defvar *hand-steer* nil
  "This variable will be DYNAMICALLY bound and used to indicate whether the user
wishes to choose tasks for planning by hand.")
(defvar *leashed* nil
  "This variable will be DYNAMICALLY bound and it will further constrain the behavior
of SHOP2 when we hand-steer it (see *hand-steer*).  If *leashed* is NIL, SHOP2 will
simply proceed when choosing tasks, if there is no choice, i.e., when there's only
one task on the open list, or only one :immediate task.  If *leashed* is non-nil,
will consult the user even in these cases.")

(defvar *break-on-backtrack* nil
  "If this variable is bound to T, SHOP will enter a break loop upon backtracking.")


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

;;; PRIMITIVEP returns T if X is a symbol whose name begins with "!"
(defmacro primitivep (x) `(and (symbolp ,x) (get ,x 'primitive)))


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
(defclass actions-domain-mixin ()
     ((methods
       :initarg :methods
       :reader domain-methods
       )
      (operators
       :initarg :operators
       :reader domain-operators
       ))
  )

(defclass domain (actions-domain-mixin shop2.theorem-prover:thpr-domain)
     ()
  (:documentation "An object representing a SHOP2 domain.")
  )

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
       :reader name)
      (domain-name
       :initarg :domain-name
       :reader domain-name
       :initform nil
       :documentation
       "The programmer MAY (but is not obligated to) specify that a problem
         is intended for use with a particular domain definition."))
  (:documentation "An object representing a SHOP2 problem."))

(defmethod print-object ((x problem) stream)
  (if *print-readably*
      (format stream "#.(make-problem :name ~s ~@[:domain-name ~s~] :tasks ~s :state-atoms ~s)"
              (name x) (domain-name x) (tasks x) (state-atoms x))
      (print-unreadable-object (x stream :type t)
        (princ (name x) stream))))

;;;------------------------------------------------------------------------------------------------------
;;; CLOS Generic Method Definitions
;;;------------------------------------------------------------------------------------------------------
(defgeneric methods (domain task-name)
  (:documentation "Return a list of all the SHOP2
methods for TASK-NAME in DOMAIN."))

(defgeneric operator (domain task-name)
  (:documentation "Return the SHOP2 operator (if any)
defined for TASK-NAME in DOMAIN."))

(defgeneric set-variable-property (domain x)
  (:documentation
   "Record facts about X being a variable, operator, or
other special symbol.  Done for side-effects."))

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
Returns no value; operates by side effects on DOMAIN."))

(defgeneric parse-domain-item (domain item-keyword item)
  (:documentation "The default method for parse-domain-items
will invoke this method to process a single item s-expression.
The addition of this generic function makes SHOP2's syntax
more extensible.")
  )

(defgeneric process-operator (domain operator-def)
  (:documentation "This generic function allows for specialization
by domain type on how operator definitions are parsed.
Should return something suitable for installation in the
operators table of DOMAIN."))

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
to expand.  SHOP2 search behavior can be changed by specializing this
generic function (most likely using the domain argument).  Returns a
sorted list of tasks in the order in which they should be expanded.
A failure can be triggered by returning NIL."))

(defmethod methods ((domain domain) (name symbol))
  (gethash name (domain-methods domain)))

(defmethod operator ((domain domain) (name symbol))
  (gethash name (domain-operators domain)))

(defmethod axioms ((domain domain) (name symbol))
  "Return a list of axioms for for NAME as defined in DOMAIN."
  (gethash name (domain-axioms domain))
  )

(defgeneric process-pre (domain precondition)
  (:documentation "Preprocess the PRECONDITION in accordance with
rules established by the DOMAIN class.  Default method is to
standardize universal quantification, creating new variable
names for the bound variables in quantified expressions.

Note that this name is a bad misnomer, since it is invoked not simply
on preconditions, but also on add and delete lists.  Possibly it should
be renamed \"process quantifier\"."))



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

(defgeneric seek-plans-primitive (domain task1 task-name task-body state tasks top-tasks
                                  partial-plan partial-plan-cost depth which-plans protections
                                  unifier)
  (:documentation "If there is an OPERATOR that is applicable to the primitive TASK in STATE,
then SEEK-PLANS-PRIMITIVE applies that OPERATOR, generates the successor state, updates the current
partial plan with the OPERATOR, and continues with planning by recursively calling SEEK-PLANS.
Otherwise, it returns FAIL."))

(defgeneric seek-plans-null (domain state which-plans partial-plan partial-plan-cost depth unifier)
  (:documentation " This SEEK-PLANS-NULL is used with WebServices to strip the add and del lists from
the actions in the partial plan before doing the actual SEEK-PLANS-NULL..."))

;;; Below, SEEK-PLANS-XXX method definitions take as input a DOMAIN object and a LTML-PLANNER-STATE object.
;;; Here, I would like to implement the ideas we discussed about CLOSifying SHOP2's internal planner state
;;; and passing planner-state objects around in the SEEK-PLANS-XXX functions, instead of the current crazy
;;; list of arguments. I am going to assume that we define a top-level PLANNER-STATE object that would
;;; probably include the current state, the partial plan, and perhaps the current task. Then, SHOP2 would
;;; use its inherited version SHOP2-PLANNER-STATE and here we would use our inherited version LTML-PLANNER-STATE.
;;; This would also help plugging ND-SHOP2, and perhaps Yoyo, in the system, since they use their slightly different
;;; planner-state versions. [2006/12/28:uk]

(defgeneric seek-plans (domain state tasks top-tasks partial-plan partial-plan-cost depth which-plans
                          protections unifier)
  (:documentation "Top-level task-decomposition function."))

(defgeneric seek-plans-task (domain task1 state tasks top-tasks partial-plan
                             partial-plan-cost depth which-plans protections unifier)
  (:documentation "This is a wrapper function that checks whether the current task to be decomposed is primitive or not,
                   and depending on the task's type, it invokes either SEEK-PLANS-PRIMITIVE or SEEK-PLANS-NONPRIMITIVE,
                   which are the real task-decomposition workhorses."))

(defgeneric seek-plans-nonprimitive (domain task1 task-name task-body state tasks top-tasks
                                     partial-plan partial-plan-cost depth which-plans protections
                                     unifier)
  (:documentation "Applies the HTN method to the current TASK1 and generates the subtasks of TASK.
                   Recursively calls the task-decomposition routine to decompose the subtasks. In the LTML context,
                   What should it do with the OUTPUTS and/or the RESULTS of that method? (TBD) "))

(defgeneric apply-method (domain state task-body method protections depth in-unifier)
  (:documentation "Applies the LTML method and returns a decomposition of the TASK. In LTML context,
                   what should it do with the OUTPUTS and/or the RESULTS of that method? (TBD) "))

(defgeneric problem->state (domain problem)
  (:documentation "Translate PROBLEM into a list of arguments to make-initial-state.
Allows DOMAIN-specific rules for extracting special problem-components.
  Note that this must return a LIST of arguments for apply, so that, for example,
the default method will return a list whose only element is a list of atoms, not
a simple list of atoms."))

(defgeneric initialize-problem (problem &key)
  (:documentation "Function that does the work of populating a problem.  Allows the programmer
of SHOP2 extensions to extend or override the normal problem-building.")
  )


(defgeneric get-state (problem)
  (:method ((problem problem))
     (state-atoms problem))
  (:method ((problem-name symbol))
     (let ((answer (find-problem problem-name)))
       (unless answer (error "No initial state for the name ~s" problem-name))
       (get-state answer))))

(defgeneric problem-state (problem)
  (:documentation "Alias for get-state.")
  (:method ((problem problem))
     (get-state problem)))

(defgeneric get-tasks (problem)
  (:method ((name symbol))
           (let ((answer (find-problem name)))
             (unless answer (error "No task list for the name ~s" name))
             (get-tasks answer)))
  (:method ((problem problem))
           (tasks problem)))

(defgeneric problem-tasks (problem)
  (:documentation "Alias for get-tasks.")
  (:method ((problem problem))
     (get-tasks problem)))

(defgeneric delete-problem (problem)
  (:method ((problem-name symbol))
     (setf *all-problems*
           (delete problem-name *all-problems* :key #'name)))
  (:method ((problem problem))
     (setf *all-problems*
           (delete problem *all-problems*))))


(defun find-problem (name)
  (find name *all-problems* :key #'name))






