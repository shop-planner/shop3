(defparameter *version* "SHOP2 version 1.1+ (April 22, 2003)
Copyright (C) 2002  University of Maryland.
This software is distributed on an \"AS IS\" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied.  This software is distributed under an
MPL/GPL/LGPL triple license.  For details, see the software source file.")

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
;;; Contributor(s):
;;;    Dana S. Nau
;;;    Yue Cao
;;;    Tsz-Au Chiu
;;;    Okhtay Ilghami
;;;    Ugur Kuter
;;;    Steve Mitchell
;;;    J. William Murdock
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

;;; -----------------------------------------------------------------------
;;; REVISION LOG

; ****************** Revisions since the SHOP2 1.1 Release

;;   2003.8.1 (jwm) Added reporting of method preconditions for
;;   shop-trace of methods.

;;   2003.7.17 (jwm) Fixed a minor bug with the shop-trace code
;;   that was causing it to incorrectly report that it was
;;   backtracking from immediate tasks when it actually had
;;   completed planning.

;;   2003.7.9 (jwm) Revised the external-access-hook mechanism (see
;;   the 2003.4.21 revision, below).  There is a new keyword for the
;;   theorem prover: :external.  Any expression that begins with
;;   :external has the following semantics: If there is an
;;   external-access-hook defined, any attempt to prove (:external A B
;;   C ...)  will return the results of running external-access-hook
;;   on the list '(A B C ...).  If there is no external-access-hook
;;   defined, any attempt to prove (:external A B C ...) will return
;;   nil.  In addition, assertions obtained from calling the
;;   external-access-hook are added to the internal state.

;;   2003.6.16 (jwm) There is a new hook routine, trace-query-hook.
;;   If a routine with that name is defined, whenever the shop-trace
;;   module is invoked, that hook routine is invoked on whatever trace
;;   information is available in that point (including the logical atoms
;;   in the current state).
;;   This functionality was originally created for John Shin's SHOP2
;;   graphical user interface.  In general, we expect it would be
;;   generally useful for an external system that needs access to lots
;;   of internal SHOP2 information.

;;   2003.4.21 (jwm) Added code to the theorem proving mechanism that
;;   invokes a routine called external-access-hook iff one is defined.
;;   This hook is intended to allow SHOP2 users to supply their own
;;   mechanisms for accessing external data structures.  If no
;;   external-access hook routine is supplied, this change has no
;;   effect.

;;   2003.4.1 (jwm) Added new keyword argument to SHOP2, :explanation.
;;   Defaults to nil; when true SHOP2 adds extra information at the
;;   end of each operator explaining how the preconditions for that
;;   operator were satisfied.  Currently supports only logical atoms,
;;   and, & or (no forall, not, eval, etc.).

;;   2003.3.4 (jwm) Enforced consistent capitalization of variables
;;   and function names (important for using SHOP2 with case-sensitive
;;   Lisp).  SHOP2 has not been extensively tested using
;;   case-sensitive Lisp, however.

; ****************** Revisions since the SHOP2 1.0 Release

;;   2002.2.8 (jwm) Fixed some minor issues that were requiring that
;;   you load SHOP2 before compiling it.  You don't have to do that
;;   any more.

;;   2002.12.20 (jwm) Tiny bug fix involving debugging code that
;;   didn't work with one of the state encodings (:bit).

;;;  2002.12.15 (jwm) Fixed the previous bug fix (see 2002.12.12)

;;;  2002.12.12 (jwm) Fixed subtle bug that was caused by unexpected
;;;  interaction between the new plan tree code (see 2002.9.27 below)
;;;  and existing (bad) delete-task-top-list code.

;;;  2002.11.1 (jwm) Fixed bug in retract-state-changes that was
;;;  causing errors when an operator attempted to delete and add the
;;;  same atom to the state (the error only arises during
;;;  backtracking).

;;;  2002.9.27 (jwm) Added new keyword parameter :plan-tree to
;;;  find-plans.  It defaults to nil; if true, the first return value
;;;  is a _list_ containing (1) the complete task decomposition tree
;;;  for the plan and (2) the plan itself.

;;;  2002.8.16 (jwm) Added code to seek-plans-null which invokes a
;;;  function called plan-found-hook if such a function is defined.
;;;  Useful for larger systems that invoke SHOP2 and want to perform
;;;  some action whenever a plan is found.

;;;  2002.7.12 (jwm) Added new global variable *all-problems* which stores
;;;  all of the problems that have been defined by defproblem or make-problem.

; ****************** Revisions prior to SHOP2 1.0 Release

;;;  2002.5.24 (jwm) Fixed bug involving operators that try to add
;;;  atoms to the state that are already true or delete operators from
;;;  a state that were already false; those operators caused strange
;;;  problems when backtracking.

;;;  2002.5.10 (jwm) Added new :state keyword to find-plans which
;;;  takes the same arguments that encode-states used to;
;;;  encode-states is no longer user.  I also fixed strip-nops so that
;;;  iNOP's are no longer included in the final plan.  I also fixed
;;;  eval terms (eval in the task list of a method).

;;;  2002.5.10 (dsn)  Removed the old verbosity code (since it is superseded by
;;;  SHOP-TRACE), and replaced it with code for :STATS, :PLANS, :LONG-PLANS,
;;;  etc.  I moved a small amount of the old verbosity code into SHOP-TRACE,
;;;  e.g., we now print a message if a traced operator violates a protection. 
;;;  Also, I fixed the *VERSION* variable and put the right copyright/licensing
;;;  comments at the start of the file, to prepare for our software release.

;;;  2002.5.3 (jwm) Task lists in methods and in problem definitions
;;;  are now processed using the same mechanism.  The :ordered and
;;;  :task keywords are optional in both mechanisms and are implied
;;;  where omitted.  For example, the task list:
;;;;    (((a 1) (b 2))
;;;;     (:unordered (:immediate c 3) (d 4)))
;;;  is interpretted as:
;;;;   (:ordered (:ordered (:task a 1) (:task b 2))
;;;;             (:unordered (:task :immediate c 3) (:task d 4)))
;;;  Note that this approach is backward compatible with SHOP1,
;;;  which leaves out the :ordered and :task keywords, since those
;;;  keywords are implied.  It is _not_ backward compatible with
;;;  MSHOP which has the top-level task list always be unordered.

;;;  2002.5.3 (jwm) Deleted the check-for-loops feature because it was
;;;  never used much and stopped working many revisions ago.

;;;  2002.4.26 (jwm) Fixed bug in sort-by which reversed sort orders.
;;;  Incidentally, the change also makes the :which option return plans
;;;  in the order that they were found (rather than the reverse order).

;;;  2002.4.19 (jwm) Did more fine tuning of state code.  Improves speed,
;;;  especially with :bit and :hash encodings.

;;;  2002.4.7 (jwm) Added new option for encode-state, :bit, which
;;;  uses bit strings to represent states; casual testing suggests
;;;  that :bit is much slower than the other options and adds little
;;;  or no additional coverage.  It may improve with further tweaking,
;;;  however, so its staying in for now.

;;;  2002.4.7 (jwm) Fixed an idiosyncracy in axiom handling code which
;;;  led to strange behavior when an axiom has a single expression for
;;;  a tail rather than a list of expressions.  Axioms now work
;;;  correctly with either form.

;;;  2002.3.30 (jwm) Fixed a bug in the interaction between the new
;;;  state handling code and the existing code for protections.
;;;  Protections should now work like they did before the new state
;;;  code was introduced.

;;;  2002.3.29 (jwm) The :pshort argument now omits operators that
;;;  begin with !! rather than operators that have 0 cost.

;;;  2002.3.29 (jwm) It is now possible to use unbound variables in
;;;  methods when invoking an operator.  If those variables are bound
;;;  in the precondition of the operator then the associated step in
;;;  the final plan will use those bindings.

;;;  2002.3.26 (jwm) Updated print-current-state and
;;;  query-current-state so that they work with the modified state
;;;  representations.

;;;  2002.3.22 (jwm) Modified the :sort-by keyword for method
;;;  preconditions again; the new syntax is:
;;;    (:sort-by <value-expr> [ <comparison-expr> ] <precondition>)
;;;  For example, the following precondition would require that there
;;;  be values for the predicates foo and bar and would sort the
;;;  satisfiers in decreasing order of the product of the values of
;;;  those predicates:
;;;    (:sort-by (* ?x ?y) #'> (and (foo ?x) (bar ?y)))

;;;  2002.3.19 (uk) Added two new keywords that is to be used by the
;;;  keyword :sort-by. These are namely :asc and :dsc, which enable 
;;;  sorting in either ascending or descending order.

;;;  2002.3.18 (jwm) Added new keyword for logical expressions,
;;;  :sort-by.  When seek-satisfiers encounters the expression
;;;  (:sort-by ?foo expr1), it seeks staisfiers for the expression
;;;  expr1 and then sorts the results by the number that they bind
;;;  ?foo too.  If a satisfier does not bind ?foo or binds ?foo to a
;;;  non-number, it is treated as 0 for sorting purposes.

;;;  2002.3.15 (jwm) Added new keyword argument, :time-limit.  When
;;;  :time-limit is a number, SHOP2 terminates when that number of
;;;  seconds of CPU time has elapsed (or when it gets done, whichever
;;;  comes first).  The limit is checked at each call to seek-plans,
;;;  so if there is a long gap between calls, the total CPU time of
;;;  planning may be greater than the time limit.

;;;  2002.3.15 (jwm) Added a new user function encode-state, which
;;;  changes the way that states are encoded.  Currently three values
;;;  are supported:
;;;
;;;    :hash encodes states as hash tables; memory efficient but slow
;;;
;;;    :list encodes states as lists; fastest but memory inefficient
;;;
;;;    :mixed encodes states as lists within hash tables; a compromise
;;;      between speed and memory effiency
;;;
;;;  The default behavior is :mixed.

;;;  2002.3.15 (jwm) Changed find-plans to return both the plans and
;;;  total CPU time used (in seconds) rather than just the plans.

;;;  2002.3.13 (jwm) Fixed bug in delete-task-top-list (list was being
;;;  destructively modified).  

;;;  2002.3.10 (jwm) Isolated the code for manipulating states.
;;;  States are now destructively modified, and need to be explicitly
;;;  undone whenever backtracking occurs.  Hash tables (keyed by
;;;  predicate) are used to store state information.

;;;  2002.2.28 (jwm) Fixed minor bug in apply-method (some values
;;;  returned by force-immediate-reduction were being dropped).

;;;  2002.2.28 (jwm) Added some rounding code to print-stats because
;;;  it printed out very badly with some floating point numbers (this
;;;  has only become significant recently with the addition of various
;;;  cost related features see below).

;;;  2002.2.27 (dsn) get-top-tasks was running in quadratic time; I
;;;  replaced it with a linear-time algorithm.

;;;  2002.2.26 (dsn) in several places, SHOP2 was computing NEWDEPTH,
;;;  passing it as a parameter, and then taking (1- NEWDEPTH).  I
;;;  changed this so that SHOP2 just passes DEPTH as a parameter
;;;  instead.  Oddly enough, this minor change improved memory usage a
;;;  bit in my tests.

;;;  2002.2.26 (dsn) declared the local COST variables.  Removed
;;;  'CONTINUE from SEEK-PLANS and its subroutines, and fixed a case
;;;  where SEEK-PLANS was being called unnecessarily -- this seems to
;;;  improve both speed and memory usage

;;;  2002.2.26 (dsn) fixed problems with unused variables.

;;;  2002.2.26 (dsn) removed got-plan, and fixed some problems with
;;;  variable declarations.

;;;  2002.2.24 (dsn) replaced list-methods, list-operators, and
;;;  list-axioms with print-methods, print-operators, and print-axioms
;;;  -- functions that I had mentioned in an email but forgot to put
;;;  into this file!

;;;  2002.2.24 (dsn) mtrace, otrace, and atrace are gone; I replaced
;;;  them with functions named shop-trace and shop-untrace.

;;;  2002.2.23 (dsn) New debugging functions: mtrace, otrace, and
;;;  atrace, for tracing individual methods, operators, and axioms.
;;;  Also, modifications so that the domain name is irrelevant.

;;;  2002.2.22 (jwm) New keyword for find-plans, :optimize-cost,
;;;  defaults to nil, and if true uses branch-and-bound to find
;;;  optimal (minimum cost) results.  If the argument to
;;;  :optimize-cost is a number, results which are less than or equal
;;;  to that number are found.

;;;  2002.2.22 (dsn) Fixed MCL compatibility bug with seek-satisfiers.

;;;  2002.2.22 (dsn) Added debug printing code to apply-method

;;;  2002.2.15 (jwm) Costs for SHOP2 operators can now be arbitrary
;;;  eval expresssions, rather than just numbers.  Note that this
;;;  feature doesn't work with backward-compatible SHOP1 operators
;;;  (i.e., ones with no preconditions).

;;;  2002.2.8 (chiu) New debugging functions: query-current-state,
;;;  print-current-state, print-current-tasks, and print-current-plan

;;;  2002.2.8 (uk) Fix the the missing part in the if-then-else
;;;  structure in handling the axioms in the function do-conjunct

;;;  2002.1.2 (jwm) Added a check-for-loops keyword parameter to
;;;  find-plans (for symmetry with the other parameters which used to
;;;  be global variables, e.g., pshort.

;;;  2001.12.27 (jwm) Added the force-immediate function to resolve
;;;  some issues with combining ordered and unordered methods, q.v.

;;;  2001.12.27 (jwm) Some miscellaneous compatibility tweaks, e.g.,
;;;  the #+:lispworks code below.

;;;  2001.12.17 (jwm) Changed printing so that if find-plans is
;;;  invoked with :verbose 0, it prints absolutely nothing.  Before,
;;;  it always printed stats no matter what.  Now to get stats you
;;;  must explicitly include :stats in the verbosity list or give
;;;  :verbose a number greater than 0.

;;;  2001.12.17 (jwm) Changed !iNOP operator to automatically be
;;;  treated as if it has a :immediate before it.  This makes !iNOP's
;;;  more efficient in unordered lists and has no functional effects
;;;  because iNOP's never do anything.

;;;  2001.12.17 (jwm) Changed :pshort argument to find-plans such that
;;;  short printing is enabled when the argument is non-nil, rather
;;;  than when it is equal to 0.  This is more consistent with LISP
;;;  and with the other arguments to find-plans.

;;;  2001.12.17 (jwm) Bug fix for the :pshort argument; previously
;;;  when printing short, only the first plan was printed.  Now all
;;;  plans are printed.

;;;  2001.12.13 (ois) Changed the apply-operator function under the
;;;  assumption that at most one operator's precondition will be
;;;  satisfied every time.  Changed find-satisfiers to have an
;;;  optional parameter which is sent as just1 to seek-satisfiers.
;;;  Set this parameter to "t" when find-satisfiers is called to find
;;;  the satisfiers of an operator's preconditions, since we assume
;;;  that there will be at most one such satisfier.

;;;  2001.12.12 (jwm) Divided seek-plans into smaller functions such a
;;;  seek-plans-nonprimitive.  This is not a functional change, but it
;;;  should make it easier to edit seek-plans in the future.

;;;  2001.12.12 (ois) Added the capability to handle ANDs in
;;;  preconditions. Also changed return value of find-satisfiers from
;;;  'fail to nil in case of failure.

;;;  2001.12.05 (jwm) Added calls to standardize for methods and
;;;  operators.  Also rewrote unify1 to no longer standardize, because
;;;  all calls to unify now follow calls to standardize.

;;;  2001.11.28 (jwm) Added call to standardize when using axioms in
;;;  seek-satisfiers (primarily because recursive axioms were broken).
;;;  Also rewrote standardize to be more efficient (since it is called
;;;  more).

;;;  2001.11.22 (chiu) Added the assignment-term "assign".

;;;  2001.11.14 (chiu) Added *current-plan* and *current-tasks*
;;;  similar to *current-state* below.

;;;  2001.11.07 (chiu) Created global variable *current-state*,
;;;  updated in find-satisfiers, which records the current state;
;;;  handy for debugging.

;;;  2001.11.07  (jwm) Fixed the debug-print of the plans to use
;;;  the :plans tag rather than :stats

;;;  20001.11.07 (jwm) Created a new function variable-gensym which
;;;  generates a new variable _and_ sets the variable property for
;;;  that variable.  Replaced calls of the form (gensym "?... with
;;;  this new function

;;;  2001.07.05 (swm) Hacked on call some more to get process-method
;;;  to expand out method tails to something which actually gets
;;;  evaluated at run time.

;;;  2001.07.02 (swm) Removed !do-nothing operator from logistics
;;;  domain and re-ran regession tests on logistics and blocks-world
;;;  domains.  This finishes the previous change, and eliminates
;;;  Jason's hacked-in requirement to avoid nil tails in methods.

;;;  2001.07.01 (swm) Added default !NOP operator to make-domain and
;;;  defdomain.  Fixed handling of nil method tails in process-method.

;;;  2001.06.27 (swm) Additional syntactic changes to synchronize SHOP
;;;  2.0 syntax with the recently released JSHOP 1.0 Added macro call
;;;  to be used in place of eval.  Replaced make-problem with
;;;  defproblem.  Replaced make-problem-set with defproblem-set.
;;;  Replaced make-domain with defdomain.  NOTE BENE that these
;;;  changes were also propogated to the sample domain problem
;;;  definition files.

;;;  2001.06.06 (swm) Added preconditions to operators.  This required
;;;  modifying apply-operator to handle the new SHOP2 operator syntax,
;;;  and to test the preconditions before executing the delete and add
;;;  lists.  The precondition substitutions are interated over when
;;;  actually evaluating the delete and add lists.  Changed
;;;  operator-cost to access the correct element of the new SHOP 2
;;;  operator.  Modified process-operator to handle SHOP 2 operators,
;;;  and to re-write SHOP 1 operators into SHOP 2 syntax.

;;;  2001.06.05 (swm) Re-designed the control of debug output.  You
;;;  can still use a number which gives the same behavior as before,
;;;  or give a list of keywords (listed after the (defvar *verbosity*
;;;  declaration near the bottom of the file, which provides finer
;;;  control over the debug output.  A new macro is provided called
;;;  debug-print which replaces the old use of if statements with
;;;  tests on various numeric and other arguments.

;;;  2001.06.02 (swm) Replaced all occurances of :serial with
;;;  :ordered, and :parallel with :unordered to better represent the
;;;  actual meaning of that syntax.

;;;  2001.05.28 (swm) Modified make-domain to convert SHOP 1.x axioms
;;;  into SHOP 2 axioms with named branches if necessary.  Modified
;;;  seek-satisfiers to accomodate new axiom syntax.

;;;  2001.02.26 (swm) Modified process-method and apply-method to
;;;  accomodate the new branch labels in the method data structure:
;;;  (:method head label_1 precondition_1 task_list_1
;;;           ...  label_n precondition_n task_list_n)
;;;  NOTE: if the method does not have labels for the branches,
;;;  process-method will generate a label for each 
;;;  branch of the form method-name-branch-number.

;;;  2001.01.02 (swm) Added strip-NOPs to remove the !NOP operator
;;;  that Jason hacked in to fix another problem.  This is only used
;;;  when pushing a partial plan onto the list *plans-found*. I'll go
;;;  back and fix the problem right at some later date.  The real
;;;  ugliness here is that to accomodate Jason's !NOP hack all of the
;;;  domains had to be modified so that methods which used to have ()
;;;  as the action now have to have (!NOP) or (!do-nothing).

;;;  2000.12.19 (swm) ACL and MCL expand backquotes in very different
;;;  ways.  The code that Jason wrote for incorporating :serial,
;;;  :parallel, :task, and :immediate into legacy SHOP :method tails
;;;  works for ACL, but cannot handle the MCL backquote expansions.
;;;  To get around that, added special backquote and comma reader
;;;  macros to a special readtable called *blue-light-readtable*, and
;;;  modified all regression SHOP domain files to bind to that
;;;  readtable during input under MCL.

;;;  2000.12.13  (swm) selectively replaced equal with = or eql to 
;;;  speed up code.

;;;  2000.12.8   (swm) redefined the IMPLY case in seek-satisfiers per 
;;;  the embedded comment.

;;;  2000.10.20 (swm) redefined variablep and primitivep as macros and
;;;  added preprocessing code to make-domain with the helper function
;;;  set-variable-property to set variable and primitive as properties
;;;  on the atom's plist to speed up code.

;;; -----------------------------------------------------------------------
;;; Global, dynamically scoped variables.  Most of these could probably have
;;; been passed as parameters instead, but it would have been awkward
;;; -----------------------------------------------------------------------

(defvar *methods*)                  ; methods in the current planning domain
(defvar *operators*)                ; operators in the current planning domain
(defvar *axioms*)                   ; axioms in the current planning domain
(defvar *start-run-time*)
(defvar *start-real-time*)
(defparameter *internal-time-limit* nil)  ; maximum time (in units) for execution
(defparameter *internal-time-tag* nil)    ; catch tag to throw to when time expires
(defparameter *state-encoding* :mixed) ; current encoding of states
(defparameter *time-limit* nil)     ; maximum time (in seconds) for execution
(defparameter *inferences* 0)       ; number of logical inferences so far
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
(defparameter *external-access* nil)  ; whether to access external data
(defparameter *trace-query* nil)    ; whether to query at each trace step
(defparameter *attribution-list* nil) ; sources of facts from external access

(defvar *print-plans*)              ; whether to print out the plans we find
(defvar *pshort*)  ; whether to skip ops that begin with !! when printing plans
(defvar *print-stats*)              ; whether to print statistics
(defvar *shop-trace* nil)           ; all items currently being traced
(defvar *all-problems* nil)         ; all problems that have been defined

(defparameter *current-state* nil) ; current state (for find-satisfiers)
(defparameter *current-plan* nil)  ; current plan
(defparameter *current-tasks* nil) ; current task

;;; It is relatively common practice in Lispworks to use ! as a macro
;;;  for :redo.  This will really mess up interpretation of operators in
;;;  SHOP, since all operators start with "!".  Thus, we will turn off
;;;  macro interpretation of "!" if we are running Lispworks.
;;; see:
;;;  http://www.xanalys.com/software_tools/reference/lwl41/lwuser/LWUG_93.HTM
#+:lispworks (set-syntax-from-char #\! #\@)

;;; Setting the compiler parameters
(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug 0)))

; (defun trace-query-hook (&rest args) (query-java args))

(defmacro trace-print (type item &rest formats)
  `(when *shop-trace*
     (when (or (member ,type *shop-trace*) (member ',item *shop-trace*))
       (when *trace-query*
	 (funcall (fdefinition 'trace-query-hook) ,type ,item (list ,@formats)
		  (state-atoms state)))
       (format t ,.formats)
       (when (member :states *shop-trace*)
	 (format t "~%     state ~s" (state-atoms state))))))

;;; (SHOP-TRACE) will print a list of what's currently being traced
;;;
;;; (SHOP-TRACE 'ITEM) will turn on tracing for ITEM, which may be
;;;   any of the following:
;;;    - the name of a method, axiom, operator, task, or predicate;
;;;    - one of the keywords :METHODS, :AXIOMS, :OPERATORS, :TASKS,
;;;      :GOALS, :EFFECTS, or :PROTECTIONS, in which case SHOP will
;;;      trace all items of that type (:GOALS, :EFFECTS, and :PROTECTIONS
;;;      refer to three different ways predicates can occur: as goals to
;;;      be satisfied, and as effects or protections in operators);
;;;    - the keyword :STATES, in which case SHOP will include the current
;;;      state whenever it prints out a tracing message
;;;
;;; (SHOP-TRACE '(ITEM1 ITEM2 ...)) will do the same for a list of items
(defun shop-trace (&optional items)
  (cond ((null items) *shop-trace*)
        ((atom items) (pushnew items *shop-trace*))
        (t (dolist (item items)
             (pushnew item *shop-trace*)))))

;;; (SHOP-UNTRACE ...) is the inverse of (SHOP-TRACE ...)
(defun shop-untrace (&optional items)
  ;; it's OK to use destructive deletion here
  (cond ((null items) (setq *shop-trace* nil))
        ((atom items)
         (setq *shop-trace* (delete items *shop-trace*)))
        (t (dolist (item items)
             (setq *shop-trace* (delete item *shop-trace*))))))

;;; VARIABLEP returns T if X is a symbol whose name begins with "?"
; The code below is faster than checking for the ? each time, but
;  assumes that all variables, have been preprocessed.
(defmacro variablep (x) `(and (symbolp ,x) (get ,x 'variable)))
; If for some reason (e.g., debugging) you want to work with
;  non-preprocessed variables, use the following instead:
;(defun variablep (sym) (and (symbolp sym) (equal (elt (symbol-name sym) 0) #\?)))

;;; PRIMITIVEP returns T if X is a symbol whose name begins with "!"
(defmacro primitivep (x) `(and (symbolp ,x) (get ,x 'primitive)))

(defmacro call (fn &rest params)
  `(funcall (function ,fn) ,.params))

(defmacro defproblem (problem-name state tasks &optional extra)
  ;; if extra is given, then the args are problem-name, domain-name, state,
  ;; and tasks respectively. in that case, we want to ignore domain-name
  (when extra
    (setq state tasks)
    (setq tasks extra)) 
  `(progn
     (format t "~%Defining problem ~s ..." ',problem-name)
     (setf *all-problems* (cons ',problem-name *all-problems*))
     (setf (get ',problem-name :state) ',state)
     (setf (get ',problem-name :tasks) ',(process-task-list tasks))
     ',problem-name))

(defmacro def-problem-set (list-name problem-list)
  `(progn
    (format t "~%Defining problem set ~s ..." ',list-name)
    (setf (get ',list-name :problems) ',problem-list)))

(defmacro defdomain (name &optional items)
  (if (null items) (setq items name))
  ;; name is ignored -- it's there just for compatibility with SHOP 1.x
  (setq items (append '((:operator (!!inop) () () 0)) items))
  `(progn
    (format t "~%Defining domain ...")
    (let ((axioms (make-hash-table))
          (operators (make-hash-table))
          (methods (make-hash-table)))
      (set-variable-property ',items) ; set primitive and variable properties
      (dolist (x (reverse ',items))
        (ecase (car x)
          ((:method) 
            (push (process-method x) (gethash (caadr x) methods)))
          ((:operator)
            (if (gethash (caadr x) operators)
              (error "There is more than one operator named ~s" (caadr x))
              (push (process-operator x) (gethash (caadr x) operators))))
          ((:-)
            ;; convert SHOP 1.x axioms into SHOP 2 axioms if necessary
            (setf x (regularize-axiom x))
            (push x (gethash (caadr x) axioms)))))
      (setq *axioms* axioms)
      (setq *operators* operators)
      (setq *methods* methods))))

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

;;; A simple macro to add an item on to the end of a list.
(defmacro push-last (item list)
  `(setf ,list 
   (if (null ,list)
       (list ,item)
     (progn
       (setf (cdr (last ,list)) (list ,item))
       ,list))))

;;; This is a hack to pull out the !NOP operations Jason stuck in to get around
;;; another problem.
(defun strip-NOPs (plan)
  (cond ((null plan) nil)
  ((and
    (listp (first plan))
    (eq '!!inop (first (first plan))))
   (strip-NOPs (rest (rest plan))))
  (t
   (cons (first plan)
         (cons (second plan)
         (strip-NOPs (rest (rest plan))))))))

(defun print-axioms (&optional name)
  (if name
    (progn (format t "Axioms for name ~S are:" name)
           (mapcar #'(lambda (x) (format t "~2%  ~s" x))
                   (gethash name *axioms*)))
    (maphash #'(lambda (k defs) 
                 (format t "~2%Axioms for goal ~S are:" k)
                 (dolist (ad defs)
                   (format t "~2%  ~S" ad)))
             *axioms*)))

(defun print-methods (&optional name)
  (if name
    (progn (format t "Methods for name ~S are:" name)
           (mapcar #'(lambda (x) (format t "~2%  ~s" x))
                   (gethash name *methods*)))
    (maphash #'(lambda (k defs)
                 (format t "~2%Methods for task ~S are:" k)
                 (dolist (ad  defs)
                   (format t "~2%   ~S" ad)))
             *methods*)))

(defun print-operators (&optional name)
  (if name
    (progn (format t "Operators for name ~S are:" name)
           (mapcar #'(lambda (x) (format t "~2%  ~s" x))
                   (gethash name *operators*)))
    (maphash #'(lambda (k defs) 
                 (format t "~2%Operators for task ~S are:" k)
                 (dolist (ad defs)
                   (format t "~2%   ~S" ad)))
             *operators*)))

;;; determine-verbosity uses the current value of *verbose* to set
;;; global flags that tell SHOP2 what information to print out
(defun determine-verbosity (verbose)
  (ecase verbose
      ((0 nil)         (setq *print-stats* nil *print-plans* nil))
      ((1 :stats)      (setq *print-stats* t *print-plans* nil))
      ((2 :plans)      (setq *print-stats* t *print-plans* t *pshort* t))
      ((3 :long-plans) (setq *print-stats* t *print-plans* t *pshort* nil))))

;;; get the list of top-tasks in the main task list
;;; i.e.  the tasks with no predecessor
(defun get-top-tasks (L)
  (if (null L)
    nil
    (ecase (car L)
      (:unordered 
       ;; a parallel task list, so recursively call the function on 
       ;; each element and append the answers
       (mapcan #'get-top-tasks (cdr L)))
      (:ordered
       ;; a serial task list, so recursively call the function on the 
       ;; first element of the list
       (get-top-tasks (cadr L)))    
      (:task
       ;; a simple task, so return a list containing the task
       (list L)))))

;;; look inside the L, replace first t1 with the top-level tasks of t2
(defun replace-task-top-list (L t1 t2)
  (append (remove t1 L :count 1 :test #'equal) (get-top-tasks t2)))

;;; look inside the ML, replace first t1 with the top-level 
;;; tasks of t2, also, this function will replace t1 in the 
;;; main task-list with t2
(defun replace-task-main-list (ML t1 t2)
  (let (answer temp temp-found found)
    (if (or (null t1) (null ML))
      (return-from replace-task-main-list (values ML nil))
      (if (equal ML t1)
        (return-from replace-task-main-list (values t2 t))))
    (ecase (car ML)
      (:unordered 
       ;; a parallel task list, recursively calling the function on 
       ;; each element
       (setq answer '(:unordered))
       (dolist (sub-task (cdr ML))
         (if found
           ;; already found t1
           (setq answer (cons sub-task answer))
           (progn
             ;; recursively calling the function on sub-task
             (multiple-value-setq (temp temp-found) 
               (replace-task-main-list sub-task t1 t2))
             (if temp-found 
               (setq t1 nil
                     t2 nil 
                     found t))
             (setq answer (cons temp answer)))))
       (return-from replace-task-main-list (values (reverse answer) found)))
      
      (:ordered
       ;; a serial task list. recursively calling the function to the
       ;; first element of the list, then append the rest of the list
       (setq answer '(:ordered))
       (multiple-value-setq (temp found)
         (replace-task-main-list (second ML) t1 t2))
       (setq answer (cons temp answer))
       (setq answer (append (reverse answer) (cddr ML)))
       (return-from replace-task-main-list (values answer found)))
      
      (:task
       ;; a simple task, if it equals to t1, it wouldn't have
       ;; reached here.  so simply return ML and nil
       (return-from replace-task-main-list (values ML nil))))))

;;; this function will delete TASK from both the main task-list ML,  
;;; and L, the top-level task list.  Also, it will update
;;; L by appending the children of TASK in the main task-list
(defun delete-task-top-list (L ML TASK)
  (let ((tempML (copy-task-tree ML)) L1)
    (setq L1 (append (remove TASK L :count 1 :test #'equal) 
                     (find-next-main-list TASK tempML)))
    (delete-task-main-list tempML TASK nil)

    (unless (car L1)
      (setq L1 (cdr L1)))
    (return-from delete-task-top-list (values L1 tempML))))

; copy-task-tree is like copy-tree except that anything starting with
; a :task is simply inserted rather than copied.  The motivation here
; is to allow functions that destructively modify the task tree
; structures to not disrupt other copies but to still allow references
; to individual tasks to be compared using eq (as is done in some
; helpers to extract-tree).
(defun copy-task-tree (tt)
  (cond
   ((atom tt) tt)
   ((eq (first tt) :task) tt)
   (t
    (cons (copy-task-tree (first tt))
          (copy-task-tree (rest tt))))))

;;; this function will find the list of children that
;;; decend from task directly
(defun find-next-main-list (task L)
  (if (null L)
    (return-from find-next-main-list (values nil nil t))
    (let (found answer (goNext t))
      (ecase (car L)
        (:unordered 
         ;; a parallel task list, recursively calling the function on 
         ;; each element, stop when it has found task
         (dolist (sub-task (cdr L))
           (if (equal sub-task task)
             ;; found the task, since it's in a :unordered list, it has 
             ;; no child directly
             (if (<= (list-length L) 2)
               ;; a :unordered list with at most one task
               (return-from find-next-main-list (values nil t t))
               (return-from find-next-main-list (values nil t nil)))
             (progn
               (multiple-value-setq (answer found goNext) (find-next-main-list task sub-task))
               (if found
                 (if (and goNext (<= (list-length L) 2))
                   ;; need to getNext child 
                   (return-from find-next-main-list (values answer found t))
                   (return-from find-next-main-list (values answer found nil)))))))
         (return-from find-next-main-list (values nil nil t)))
        
        (:ordered
         ;; a serial task list, check if the first element is equal to task, 
         ;; if not, recursively calling the function on the 
         ;; first element of the list
         (if (equal (second L) task)
           (if (<= (list-length L) 2)
             (return-from find-next-main-list (values nil t t))
             (return-from find-next-main-list 
               (values (get-top-tasks (third L)) t nil)))
           (progn
             (multiple-value-setq (answer found goNext) (find-next-main-list task (second L)))
             (if found
               (if goNext 
                 (if (<= (list-length L) 2)
                   ;; need to getNext child 
                   (return-from find-next-main-list (values answer found t))
                   (return-from find-next-main-list 
                     (values (get-top-tasks (third L)) found nil)))
                 (return-from find-next-main-list (values answer found nil)))))))
        
        (:task
         ;; a simple task, L can't equal task, if it did, it would have been 
         ;; caught on the previous two cases
         (return-from find-next-main-list (values nil nil t)))))))

;;; This function removes TASK from the main-list.
;;; TODO: this function should not only delete the first occurance of TASK, but
;;; also should flatten the list if possible
;;; i.e. (:ordered ... (:ordered t1 t2) ... ) should become
;;; (:ordered ... t1 t2 ...)
(defun delete-task-main-list (L TASK deleted)
  (let (current-deleted
	(templist (cdr L))
	sub-task)
    (if (null L)
      (return-from delete-task-main-list nil)
      (ecase (car L)
        (:unordered 
         ;; a parallel task list, recursively calling the function on 
         ;; each element.
         (loop while (and (not (null templist)) (not (null (car templist)))) do
               (progn
                 (setf sub-task (car templist))
                 (setq current-deleted (delete-task-main-list sub-task TASK deleted))
                 (setq deleted (if (or deleted current-deleted) t nil))
                 (if (or (<= (list-length sub-task) 1)
			 (eq current-deleted :task))
                   ;; remove sub-task from L
                   (setf (car templist) (second templist)
                         (cdr templist) (cddr templist))
                   (setf templist (cdr templist)))))
         ;; clean up code to remove the last nil in the list
         (if (and (> (list-length L) 1)
                  (null (car (last L))))
           (rplacd (nthcdr (- (list-length L) 2) L) nil))
         (return-from delete-task-main-list deleted))

        (:ordered
         ;; a serial task list, recursively calling the function on the 
         ;; first element of the list
         (setf sub-task (car templist))
         (setq current-deleted (delete-task-main-list sub-task TASK deleted))
         (setq deleted (if (or deleted current-deleted) t nil))
         (if (or (<= (list-length sub-task) 1)
			 (eq current-deleted :task))
           ;; remove sub-task from L
           (setf (car templist) (second templist)
                 (cdr templist) (cddr templist)))
         (if (and (> (list-length L) 1)
                  (null (car (last L))))
           (rplacd (nthcdr (- (list-length L) 2) L) nil))
         (return-from delete-task-main-list deleted))
        
        (:task
         ;; a simple task, check if it's equal to t1, if so, set it to (nil)
         ;; and at the up-level it will be deleted
         ;; Don't set -> instead just non-destructively return :task since
	 ;;  individual task elements are reused now - jwm 12/12/02
         (if (and (not deleted) (equal TASK L))
             (return-from delete-task-main-list :task)
           (return-from delete-task-main-list nil)))))))

(defun process-task-list (tasks)
  (cond
   ((null tasks) (list :ordered (list :task '!!inop)))
   ((member (first tasks) '(:ordered :unordered))
    (cons (first tasks)
    (mapcar #'process-task-list (rest tasks))))
   ((eq (first tasks) :task)
    tasks)
   ((atom (first tasks))
    (cons :task tasks))
   (t
    (cons :ordered
    (mapcar #'process-task-list tasks)))))

;;; EXTRACT-VARIABLES returns a list of all of the variables in EXPR
(defun extract-variables (expr)
  (cond ((variablep expr) (list expr))
  ((and (consp expr) (not (eql (car expr) 'forall)))
   (my-union (extract-variables (car expr))
       (extract-variables (cdr expr))))))
  
;;; Lisp has a built-in UNION function, but we need something that returns
;;; the same results regardless of what platform SHOP is running on.
(defun my-union (s1 s2 &key (test #'eql))
   (append s1
     (remove-if #'(lambda (e2) (member e2 s1 :test test)) s2)))

;;; APPLY-SUBSTITUTION searches through TARGET, replacing each variable
;;; symbol with the corresponding value (if any) in A-LIST  (Dana Nau)
(defmacro apply-substitution (target a-list)
  `(if (null ,a-list) ,target
    (real-apply-substitution ,target ,a-list)))
(defun real-apply-substitution (target a-list)
  (cond ((atom target)
         (let ((result (assoc target a-list)))
           (if result (cdr result) target)))
        ((null (cdr target)) (list (real-apply-substitution (car target) a-list)))
        (t (cons (real-apply-substitution (car target) a-list)
                 (real-apply-substitution (cdr target) a-list)))))

;;; COMPOSE-SUBSTITUTIONS applies SUB2 to the right-hand-side of each item
;;; in SUB1, and appends all items in SUB2 whose left-hand-sides aren't in SUB1.
;;; *** Warning:  COMPOSE-SUBSTITUTIONS destroys the old value of SUB1 ***
;;; I normally would avoid destructive operations, but here it speeds up the
;;; planner by about 5%, and none of the calling functions need SUB1 afterwards
;;; (Dana Nau)
(defun compose-substitutions (sub1 sub2)
  (dolist (pair sub1)
    (setf (cdr pair) (apply-substitution (cdr pair) sub2)))
  (dolist (pair sub2 sub1)
    (pushnew pair sub1 :test #'(lambda (x y) (equal (car x) (car y))))))

;;; UNIFY is based on the procedure in Nilsson's 1980 book, but modified
;;; to produce an mgu that simultaneously unifies and standardizes the two
;;; expressions.  This improves efficiency by avoiding repeated calls to
;;; STANDARDIZER in FIND-SATISFIERS and APPLY-METHOD.  (Dana Nau)
(defun unify (e1 e2)
  (cond ((atom e1) (unify1 e1 e2))
        ((atom e2) (unify1 e2 e1))
        (t (let ((hsub (unify (car e1) (car e2))))
             (if (eql hsub 'fail)
               'fail
               (let* ((tail1 (apply-substitution (cdr e1) hsub))
                      (tail2 (apply-substitution (cdr e2) hsub))
                      (tsub (unify tail1 tail2)))
                 (if (eql tsub 'fail)
                   'fail
                   (compose-substitutions hsub tsub))))))))

(defun unify1 (e1 e2)
  (cond ((equal e1 e2) nil)
    ((variablep e1)
      (if (and (occurs e1 e2))
        'fail
        (list (cons e1 e2))))
    ((variablep e2)
      (list (cons e2 e1)))
    (t 'fail)))

;;; OCCURS is the infamous "occurs check" - it returns T if the variable
;;; symbol V occurs anywhere in the expression EXPR (Dana Nau)
(defun occurs (v expr)
  (if (atom expr)
    (eq v expr)
    (or (occurs v (car expr)) (occurs v (cdr expr)))))

(defun variable-gensym (&optional base-name)
  (let ((sym (if base-name (gensym (string base-name)) (gensym "?"))))
    (setf (get sym 'variable) t)
    sym))

;;; STANDARDIZER returns a substitution that replaces every varible symbol
;;; in EXPRESSION with a new variable symbol not used elsewhere (Dana Nau)
(defun standardizer (expression)
  (mapcar #'(lambda (x) (cons x (variable-gensym))) (extract-variables expression)))

(defun standardize (expr &optional subs)
  (cond
   ((null expr) (values nil subs))
   ((consp expr)
    (multiple-value-bind 
     (car-expr car-subs)
     (standardize (car expr) subs)
     (multiple-value-bind
      (cdr-expr cdr-subs)
      (standardize (cdr expr) car-subs)
      (values (cons car-expr cdr-expr) cdr-subs))))
   ((variablep expr)
    (let ((bin (assoc expr subs)))
      (if bin
        (values (cdr bin) subs)
        (let ((new-var (variable-gensym expr)))
          (values new-var (cons (cons expr new-var) subs))))))
   (t
    (values expr subs))))

;;; ------------------------------------------------------------------------
;;; Theorem prover
;;; ------------------------------------------------------------------------

;;; This macro is defined before find-satisfiers because it is invoked by
;;;  find-satisfiers and some compilers want all macros to be defined before
;;;  any code which invokes them.
(defmacro seek-satisfiers (goals state bindings level just1)
         `(if (null ,goals)
            (list ,bindings)
            (real-seek-satisfiers ,goals ,state ,bindings ,level ,just1)))

;;; FIND-SATISFIERS returns a list of all satisfiers of GOALS in AXIOMS
(defun find-satisfiers (goals state &optional just-one)
  (setf *current-state* state)
  (let* ((sought-goals
          (cond
            ((eq (first goals) :first) (rest goals))
            ((eq (first goals) :sort-by)
              (if (= (length goals) 3)
                (third goals)
                (fourth goals)))
            (t goals)))
         (variables (extract-variables sought-goals))
         (answer (seek-satisfiers sought-goals state variables 0
                                  (or (eq (first goals) :first) just-one)))
         (satisfiers (mapcar #'(lambda (bindings) (pairlis variables bindings))
                             answer)))
    ;(format t "~%sat: ~s~%" satisfiers) ;***

    (if (eq (first goals) :sort-by)
      (sort satisfiers 
            (if (= (length goals) 3) #'<
              (eval (third goals)))
            :key #'(lambda (sat)
                 (eval (apply-substitution (second goals) sat))))
      satisfiers)))

;;; REAL-SEEK-SATISFIERS is the workhorse for FIND-SATISFIERS.  For each
;;; proof of GOALS from AXIOMS, it returns the values of GOAL's variables
;;; that make the proof true.  Here are the other parameters:
;;;  - VARIABLES is the list of variables that need to appear in the answer
;;;  - BINDINGS is the variable bindings at the current node of the proof tree
;;;  - LEVEL is the current search depth, for use in debugging printout
;;;  - JUST1 is a flag saying whether to return after finding the 1st satisfier
(defun real-seek-satisfiers (goals state bindings level just1)
  (setq *inferences* (1+ *inferences*))
  (let ((goal1 goals) (newlevel (1+ level))
        remaining mgu1 answers new-answers)
    (when (listp (car goals))
      (setq goal1 (car goals))
      (setq remaining (cdr goals)))
    (case (car goal1)
      (not
        ;; we just want to see if (CDR GOAL1) is satisfiable, so last arg is T
        (if (seek-satisfiers (cdr goal1) state nil newlevel t)
          (return-from real-seek-satisfiers nil)
          (seek-satisfiers remaining state bindings newlevel just1)))
      
      (eval
        (if (eval (cadr goal1))
          (seek-satisfiers remaining state bindings newlevel just1)
          (return-from real-seek-satisfiers nil)))

      (call
        (if (eval (cdr goal1))
          (seek-satisfiers remaining state bindings newlevel just1)
          (return-from real-seek-satisfiers nil)))

      (assign
        (let ((ans (eval (caddr goal1))))
          (seek-satisfiers 
            (apply-substitution remaining (list (cons (cadr goal1) ans)))
            state (apply-substitution bindings (list (cons (cadr goal1) ans)))
            newlevel just1)))

      (imply
        (let ((conditionA (second goal1)) (conditionB (third goal1)))
          (if (or (not (find-satisfiers conditionA state))
                  (find-satisfiers conditionB state t))
            (seek-satisfiers remaining state bindings newlevel just1)
            (return-from real-seek-satisfiers nil))))

      (or
        (dolist (goal1-sub (cdr goal1))
          ;; First, look for ways to satisfy GOAL1-sub from the atoms in STATE
          (when (setq mgu1 (find-satisfiers goal1-sub state))
            (dolist (tempmgu mgu1)
              (setq new-answers (seek-satisfiers
                                 (apply-substitution remaining tempmgu)
                                 state (apply-substitution bindings tempmgu)
                                 newlevel just1))
              (when new-answers
                (if just1
                  (return-from real-seek-satisfiers new-answers))
        (setq answers (my-union new-answers answers :test #'equal))))))
        (return-from real-seek-satisfiers answers))
      
      (forall
        (let ((bounds (third goal1)) (conditions (fourth goal1)) mgu2)
          (setq mgu2 (find-satisfiers bounds state))
          (dolist (m2 mgu2)
            (unless (seek-satisfiers (apply-substitution conditions m2)
                     state bindings 0 t)
              (return-from real-seek-satisfiers nil))))
        (seek-satisfiers remaining state bindings newlevel just1))

      (t
        ;; Handling and
        (if (eq (car goal1) 'and)
          (setq goal1 (cdr goal1)))
        ;; Handling a list of goals as a conjunction of 
        ;; (not necessarily atomic) goals
        (if (or (listp (car goal1)) (eq (car goal1) :external))
          (progn
            (when (setq mgu1 (if (eq (car goal1) :external)
				 (external-find-satisfiers (cdr goal1) state)
			       (find-satisfiers goal1 state)))
              (dolist (tempmgu mgu1)
		      (setq new-answers 
			    (seek-satisfiers
			     (apply-substitution remaining tempmgu)
			     state (apply-substitution bindings tempmgu)
			     newlevel just1))
                (when new-answers
                 (if just1
                  (return-from real-seek-satisfiers new-answers))
                  (setq answers (my-union new-answers answers 
					  :test #'equal)))))
            (return-from real-seek-satisfiers answers))
          ;; If goal1 is an atomic predicate, try to satisfy it.
          (do-conjunct goal1 remaining state bindings level just1))))))

; An alternate version that wasn't used:
;(defun external-find-satisfiers (goal state)
;  (format t "EFS: ~s~%" goal)
;  (or (when *external-access* 
;	(let ((sats (external-query goal state)))
;	  (nconc *attribution-list* 
;		 (mapcar #'(lambda (sat)
;			     (list (apply-substitution goal sat)
;				   ATTRIB))
;			 sats))
;	  sats))
;      (find-satisfiers goal state)))

(defun external-find-satisfiers (goal state)
;  (format t "EFS: ~s~%" goal)
  (if *external-access*
      (external-query goal state)
    nil))

;;; Goal1 is guaranteed to be an atomic predicate
(defun do-conjunct (goal1 remaining state bindings level just1)
  (let (mgu1 answers new-answers found-match new-just1)
    (trace-print :goals (car goal1)
                 "~2%Level ~s, trying to satisfy goal ~s"
                 level
                 goal1)

    ;; Then, look for ways to satisfy GOAL1 from the atoms in STATE
    (dolist (r (state-candidate-atoms-for-goal state goal1))
      (unless (eql (setq mgu1 (unify goal1 r)) 'fail)
        (setq found-match t) ; for debugging printout
        (setq new-answers (seek-satisfiers
                           (apply-substitution remaining mgu1)
                           state (apply-substitution bindings mgu1) 
                           (1+ level) just1))
        (if new-answers
          (progn
            (trace-print :goals (car goal1)
                         "~2%Level ~s, state satisfies goal ~s~%satisfiers ~s"
                         level
                         goal1
                         new-answers)
            (if just1
              (return-from do-conjunct new-answers))
            (setq answers (my-union new-answers answers :test #'equal))))))
    ;; Next, look for ways to prove GOAL1 from the *axioms*
    (dolist (r (gethash (car goal1) *axioms*))
      (let ((standardized-r (standardize r)))
        (unless (eql (setq mgu1 (unify goal1 (second standardized-r))) 'fail)
          (setq found-match t) ; for debugging printout
          ;; found an axiom which unifies, now look at branches of the tail
          (let ((tail (cddr standardized-r)))
            (do ((ax-branch-name (car tail) (car tail))
                 (ax-branch (cadr tail) (cadr tail)))
              ((null tail)  nil)
              (trace-print :goals (car goal1)
                           "~2%Level ~s, axiom matches goal ~s~%     axiom ~s~%satisfiers ~s"
                           level
                           goal1
                           ax-branch-name
                           mgu1)
              (trace-print :axioms ax-branch-name
                   "~2%Level ~s, trying axiom ~s~%      goal ~s~%      tail ~s"
                     level
                     ax-branch-name
                     goal1
                     (apply-substitution ax-branch mgu1))
              (if (eq (car ax-branch) :first)
                (setq new-just1 t ax-branch (cdr ax-branch))
                (setq new-just1 just1))
              (setq new-answers (seek-satisfiers
                                  (apply-substitution 
                                    (append (list ax-branch) remaining) mgu1)
                                 state (apply-substitution bindings mgu1)
                                 (1+ level) new-just1))
              (if new-answers
                (progn
                  (trace-print :axioms ax-branch-name
                               "~2%Level ~s, applying axiom ~s~%      goal ~s~%      tail ~s"
                               level
                               ax-branch-name
                               goal1
                               (apply-substitution ax-branch mgu1))
                  (if new-just1 
                    (return-from do-conjunct new-answers))
                  (setq answers (my-union new-answers answers :test #'equal))
                  (return nil))
                (progn
                  (trace-print :axioms ax-branch-name
                               "~2%Level ~s, exiting axiom ~s~%      goal ~s~%      tail ~s"
                               level
                               ax-branch-name
                               goal1
                               (apply-substitution ax-branch mgu1))))
              (setf tail (cddr tail)))))))
    (unless found-match
      (trace-print :goals (car goal1)
                   "~2%Level ~s, couldn't match goal ~s"
                   level
                   goal1))
    (return-from do-conjunct answers)))



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
(defun explain-satisfier (unified-goal state &optional external)
  (let ((*external-access* nil)) ; otherwise we'd query twice
    (cond
     ((member (first unified-goal)
	      '(sort-by not eval call assign imply forall))
     ; The above constructs are not handled by the explanation code yet.
      nil)
     ((eq (first unified-goal) :external)
      (explain-satisfier (rest unified-goal) state t))
     ((eq (first unified-goal) 'or)
      (cond
       ((null (rest unified-goal)) nil)
       ((not (find-satisfiers (second unified-goal) state))
	(explain-satisfier (cons 'or (rest (rest unified-goal)))
			   state external))
       (t
	(explain-satisfier (second unified-goal) state external))))

     ((eq (first unified-goal) 'and)
      (let* ((explanation-list
	      (mapcar #'(lambda (g) (explain-satisfier g state external)) 
		      (rest unified-goal)))
	     (simplified-explanation-list (remove nil explanation-list))
	     (explanation
	      (when (find-satisfiers unified-goal state)
		(cons 'and simplified-explanation-list))))
	(when explanation
	  (add-source unified-goal external explanation))))

     ((listp (first unified-goal)) ; implicit and
      (explain-satisfier (cons 'and unified-goal) state external))
     (t ; logical-atom
      (add-source unified-goal external unified-goal)))))

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
(defun  external-query (query state)
;  (format t "~%potential query: ~s" query)
  (cond
   ((null query) nil)
   ((member (first query) '(not eval call assign imply forall sort-by))
    nil)
   ((listp (first query)) ; implicit and
    (external-query (cons 'and query) state))
   ((eq (first query) 'or)
    (or (external-query (second query) state)
	(when (rest (rest query))
	  (external-query `(or ,(rest (rest query))) state))))
   ((and (eq (first query) 'and) (rest query))
    (if (find-if-not #'(lambda (subquery) 
			 (and (listp subquery)
			      (= (length subquery) 3)
			      (not (logical-keywordp (first subquery)))
			      (not (find-if-not #'atom subquery))))
		     (rest query))
	; complex query - try to decompose
	(let ((first-response (external-query (second query) state)))
	  (when first-response
	    (let ((rest-response (external-query `(and ,@(rest (rest query)))
						 state)))
	      (when rest-response
		(merge-binding-set-lists first-response rest-response)))))
      ; simple query - invoke external access
      (invoke-external-query query state)))
   (t ; query of a single logical atom
    (invoke-external-query (list 'and query) state))))

(defun logical-keywordp (sym)
  (member sym '(and or not eval call assign imply forall sort-by)))

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
                 (add-atom-to-state fact state nil nil))
         binding-set))
     (funcall (fdefinition 'external-access-hook) query)))

; Some bindings returned in a query may involve uninterned symbols.  This
;  routine substitutes those symbols with the original values.
(defun fix-uninterned-bindings (bindings query-vars)
;(format t "~%bindings: ~s - vars: ~s" bindings query-vars)
  (mapcar #'(lambda (binding)
	      (let* ((name (symbol-name (car binding)))
		     (matching-var
		      (find-if 
		       #'(lambda (v) (string-equal (symbol-name v) name))
		       query-vars)))
		(if matching-var
		    (cons matching-var (cdr binding))
		  binding)))
	  bindings))

; Sample behavior of an external-access-hook:
;  Takes a goal as input, returns bindings and attribution.
(defun DUMMY-external-access-hook (goal)
;(format t "~%external query: ~s" goal)
  (let ((dummy-goal '(and
		     (employees ?business UID206)
		     (hasMembers UID201 ?business)
		     (isa UID202 MafiyaGroup-Russian)))
	(dummy-bindings `((,(second (second goal)) UID203)))
	(dummy-attribution '(UID6147 PoliceOrganization)))
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

;;; ------------------------------------------------------------------------
;;; Creating and manipulating states
;;; ------------------------------------------------------------------------

;;; SHOP maintains a state as a list of the form
;;; (tags-info (name1 . (all atoms that begin with name1))
;;;            (name2 . (all atoms that begin with name2))
;;;            ...)
;;;  where name1, name2, ..., are in lexicographical order.

;;; Tags-info is a list of tag-info entries.  Each tag-info is a list whose
;;; first element is a tag (represented by an integer) and whose remaining
;;; elements are a list of changes made to the state while that tag was active.
;;; The command tag-state activates a new tag and returns it.  The command
;;; retract-state-changes retracts all changes which were made while the given
;;; tag was active.  It is expected that retractions will typically involve the
;;; most recently added tag, but the system does allow older tags to be
;;; retracted instead.

(defun make-state (atoms)
  (list
   (list (list 0))
   (make-statebody atoms)
   *state-encoding*))

(defun state-all-atoms-for-predicate (state pred)
  (statebody-atoms-for-predicate (second state) pred))

(defun state-candidate-atoms-for-goal (state goal)
  (statebody-candidate-atoms-for-goal (second state) goal))

(defun state-atoms (state)
  (statebody-atoms (second state)))

(defun tag-state (state)
  (let ((new-tag (1+ (first (first (first state))))))
    (setf (first state) (cons (list new-tag) (first state)))
    new-tag))

(defun retract-state-changes (state tag)
  (multiple-value-bind
   (new-tags-info changes)
   (pull-tag-info (first state) tag)
   (setf (first state) new-tags-info)
   (dolist
    (change changes)
    (if (eq (first change) 'add)
      (setf (second state)
            (remove-atom-from-statebody (rest change) (second state)))
      (setf (second state)
            (insert-atom-into-statebody (rest change) (second state)))))))

(defun add-atom-to-state (atom state depth operator)
  (trace-print :effects (car atom)
               "~2%Depth ~s, adding atom to current state~%      atom ~s~%  operator ~s"
               depth atom operator)
  (unless (atom-in-state-p atom state)
;    (unless (member (cons 'delete atom) (first (first state)) :test #'equal)
    (include-in-tag 'add atom state)
    (setf (second state) (insert-atom-into-statebody atom (second state)))))

(defun delete-atom-from-state (atom state depth operator)
  (trace-print :effects (car atom)
               "~2%Depth ~s, deleting atom from current state~%      atom ~s~%  operator ~s"
               depth atom operator)
  (when (atom-in-state-p atom state)
    (include-in-tag 'delete atom state)
    (setf (second state) (remove-atom-from-statebody atom (second state)))))

(defun atom-in-state-p (atom state)
  (atom-in-statebody-p atom (second state)))

;;; The functions below are internal helpers for the above state
;;;  manipulation routines.

(defun include-in-tag (action atom state)
  (let ((first-info (first (first state))))
    (setf (rest first-info) (cons (cons action atom) (rest first-info)))))

(defun pull-tag-info (tags-info tag)
  (if (null tags-info)
      (error "Attempt to retract to non-existant state")
    (let ((first-info (first tags-info)))
      (if (= tag (first first-info))
        (values (rest tags-info) (rest first-info))
      (multiple-value-bind
        (rest-info rest-changes)
        (pull-tag-info (rest tags-info) (rest first-info))
        (values (cons first-info rest-info) rest-changes))))))

;;; Creates a new statebody and returns it.
(defun make-statebody (atoms)
  (ecase *state-encoding*
   (:mixed (MIXED-make-statebody atoms))
   (:bit (BIT-make-statebody atoms))
   (:list (LIST-make-statebody atoms))
   (:hash (HASH-make-statebody atoms))))

;;; Returns a statebody which includes atom and all of the atoms in the
;;;  given statebody.  May be destructive.
(defun insert-atom-into-statebody (atom statebody)
  (ecase *state-encoding*
   (:mixed (MIXED-insert-atom-into-statebody atom statebody))
   (:bit (BIT-insert-atom-into-statebody atom statebody))
   (:list (LIST-insert-atom-into-statebody atom statebody))
   (:hash (HASH-insert-atom-into-statebody atom statebody))))

;;; Returns a statebody which does not include atom but does include all
;;;  other atoms in the given statebody.  May be destructive.
(defun remove-atom-from-statebody (atom statebody)
  (ecase *state-encoding*
   (:mixed (MIXED-remove-atom-from-statebody atom statebody))
   (:bit (BIT-remove-atom-from-statebody atom statebody))
   (:list (LIST-remove-atom-from-statebody atom statebody))
   (:hash (HASH-remove-atom-from-statebody atom statebody))))

;;; Returns a list of all atoms in the state.  This operation can be
;;;  expensive; it is primarily intended for debugging output, not for
;;;  normal operation.
(defun statebody-atoms (statebody)
  (ecase *state-encoding*
   (:mixed (MIXED-statebody-atoms statebody))
   (:bit (BIT-statebody-atoms statebody))
   (:list (LIST-statebody-atoms statebody))
   (:hash (HASH-statebody-atoms statebody))))

;;; Returns a list of all atoms in the state which match a given predicate.
(defun statebody-atoms-for-predicate (statebody pred)
  (ecase *state-encoding*
   (:mixed (MIXED-statebody-atoms-for-predicate statebody pred))
   (:bit (BIT-statebody-atoms-for-predicate statebody pred))
   (:list (LIST-statebody-atoms-for-predicate statebody pred))
   (:hash (HASH-statebody-atoms-for-predicate statebody pred))))

;;; Returns a list of candidate atoms in the state which may match the
;;;  current goal.  For some implementations, this is just a list of
;;;  atoms which match the goal's predicates.  However, if some
;;;  implementation is able to narrow the list down further (e.g., by
;;;  excluding results which don't match some concrete value) then
;;;  this can save time in the matching process.
(defun statebody-candidate-atoms-for-goal (statebody goal)
  (ecase *state-encoding*
   (:mixed (MIXED-statebody-candidate-atoms-for-goal statebody goal))
   (:bit (BIT-statebody-candidate-atoms-for-goal statebody goal))
   (:list (LIST-statebody-candidate-atoms-for-goal statebody goal))
   (:hash (HASH-statebody-candidate-atoms-for-goal statebody goal))))

;;; Returns true iff the given atom is in the given statebody
(defun atom-in-statebody-p (atom statebody)
  (ecase *state-encoding*
   (:mixed (MIXED-atom-in-statebody-p atom statebody))
   (:bit (BIT-atom-in-statebody-p atom statebody))
   (:list (LIST-atom-in-statebody-p atom statebody))
   (:hash (HASH-atom-in-statebody-p atom statebody))))

(defun MIXED-make-statebody (atoms)
  (let ((res (make-hash-table :test #'eq)))
    (dolist (atom atoms) (MIXED-insert-atom-into-statebody atom res))
    res))

;;; Insert an atom to a statebody
(defun MIXED-insert-atom-into-statebody (atom statebody)
  (setf (gethash (first atom) statebody)
        (cons (rest atom) (gethash (first atom) statebody)))
  statebody)

;;; Remove an atom from a statebody
(defun MIXED-remove-atom-from-statebody (atom statebody)
  (setf
   (gethash (first atom) statebody)
   (delete
    (rest atom)
    (gethash (first atom) statebody)
    :test #'equal))
  statebody)

(defun MIXED-statebody-atoms (statebody)
  (let ((acc nil)) 
    (maphash #'(lambda (pred lis)
                (setf acc
                      (append (mapcar #'(lambda (entry) (cons pred entry)) lis)
                              acc)))
             statebody)
    acc))

(defun MIXED-statebody-atoms-for-predicate (statebody pred)
  (let* ((lis (gethash pred statebody)))
    (mapcar #'(lambda (entry) (cons pred entry)) lis)))

(defun MIXED-statebody-candidate-atoms-for-goal (statebody goal)
  (cond
   ((find-if-not #'(lambda (term)
                    (and (atom term) (not (variablep term))))
                 (rest goal))
    (MIXED-statebody-atoms-for-predicate statebody (first goal)))
   ((MIXED-atom-in-statebody-p goal statebody) (list goal))
   (t nil)))

(defun MIXED-atom-in-statebody-p (atom statebody)
  (member (rest atom) (gethash (first atom) statebody) :test #'equal))

(defun LIST-make-statebody (atoms)
  (if (null atoms) nil
    (LIST-insert-atom-into-statebody
     (first atoms)
     (LIST-make-statebody (rest atoms)))))

;;; Insert an atom to a statebody
(defun LIST-insert-atom-into-statebody (atom statebody)
  (cond 
   ((null statebody)
    (list (list (car atom) atom)))
   ((string< (car atom) (caar statebody))
    (cons (list (car atom) atom) statebody))
   ((eq (car atom) (caar statebody))
    (cons
     (cons (caar statebody)
      (if (member atom (cdar statebody) :test #'equal)
        (cdar statebody)
      (cons atom (cdar statebody))))
     (cdr statebody)))
   (t (cons (car statebody)
    (LIST-insert-atom-into-statebody atom (cdr statebody))))))

;;; Remove an atom from a statebody
(defun LIST-remove-atom-from-statebody (atom statebody)
  (cond ((null statebody) nil)
  ((string< (car atom) (caar statebody)) statebody)
  ((eq (car atom) (caar statebody))
   (let ((newval (remove atom (cdar statebody) :test #'equal)))
     (if newval
       (cons (cons (car atom) newval) (cdr statebody))
       (cdr statebody))))
  (t (cons (car statebody)
     (LIST-remove-atom-from-statebody atom (cdr statebody))))))

(defun LIST-statebody-atoms-for-predicate (statebody pred)
  (rest (assoc pred statebody)))

(defun LIST-statebody-candidate-atoms-for-goal (statebody goal)
  (LIST-statebody-atoms-for-predicate statebody (first goal)))

(defun LIST-statebody-atoms (statebody)
  (mapcan #'(lambda (entry) (copy-list (cdr entry))) statebody))

(defun LIST-atom-in-statebody-p (atom statebody)
  (member atom (rest (assoc (first atom) statebody)) :test #'equal))

(defun HASH-make-statebody (atoms)
  (let ((res (make-hash-table :test #'equal)))
    (dolist (atom atoms) (HASH-insert-atom-into-statebody atom res))
    res))

(defun HASH-insert-atom-into-statebody (atom statebody)
  (setf (gethash atom statebody) t)
  statebody)

(defun HASH-remove-atom-from-statebody (atom statebody)
  (remhash atom statebody)
  statebody)

(defun HASH-statebody-atoms-for-predicate (statebody pred)
  (remove-if-not
   #'(lambda (atom)
       (eq (first atom) pred))
   (HASH-statebody-atoms statebody)))

(defun HASH-statebody-candidate-atoms-for-goal (statebody goal)
  (cond
   ((find-if-not #'(lambda (term) (and (atom term) (not (variablep term))))
                 (rest goal))
    (HASH-statebody-atoms-for-predicate statebody (first goal)))
   ((HASH-atom-in-statebody-p goal statebody)
    (list goal))
   (t nil)))

(defun HASH-statebody-atoms (statebody)
  (let ((acc nil)) 
    (maphash #'(lambda (key val)
                (declare (ignore val)) (setf acc (cons key acc)))
             statebody)
    acc))

(defun HASH-atom-in-statebody-p (atom statebody)
  (gethash atom statebody))

(defun BIT-make-statebody (atoms)
  (let* ((pred-table (make-hash-table :test #'eq))
         (entity-table (make-hash-table :test #'equal))
         (type-table (make-hash-table :test #'eq))
         (statebody (list pred-table entity-table type-table
                          (LIST-make-statebody nil))))
    (dolist (atom atoms)
      (if (= (length atom) 2)
        (let ((type-name (first atom))
              (entity (second atom)))
          (unless (gethash entity entity-table)
            (let ((type-info (gethash type-name type-table)))
              (if type-info (incf (first type-info))
                (setf (gethash type-name type-table)
                      (setf type-info (list 1 (make-hash-table :test #'eql)))))
              (setf (gethash entity entity-table)
                    (list type-name (1- (first type-info))))
              (setf (gethash (1- (first type-info)) (second type-info))
                    entity))))))
    (dolist (atom atoms)
      (unless (gethash (first atom) pred-table)
       (let* ((types (mapcar #'(lambda (entity)
                                (first (gethash entity entity-table)))
                             (rest atom)))
              (type-counts (mapcar #'(lambda (type)
                                      (when type
                                        (first (gethash type type-table))))
                                   types)))

         (unless (or (null type-counts) (member nil type-counts))
           (setf (gethash (first atom) pred-table)
                 (list types type-counts 
                   (make-array type-counts :element-type 'bit))))))
      (BIT-insert-atom-into-statebody atom statebody))
    statebody))

(defun BIT-insert-atom-into-statebody (atom statebody)
  (let* ((pred-table (first statebody))
         (entity-table (second statebody))
         (extras (fourth statebody))
         (entities (rest atom))
         (types (mapcar #'(lambda (entity)
                           (first (gethash entity entity-table)))
                        entities))
         (entity-numbers (mapcar #'(lambda (entity)
                                    (second (gethash entity entity-table)))
                                 entities))
         (pred-entry (gethash (first atom) pred-table))
         (pred-types (first pred-entry))
         (pred-array (third pred-entry)))

    (if (and entities (equal types pred-types))
        (setf (apply #'aref pred-array entity-numbers) 1)
      (setf (fourth statebody)
            (LIST-insert-atom-into-statebody atom extras))))
  statebody)

(defun BIT-remove-atom-from-statebody (atom statebody)
  (let* ((pred-table (first statebody))
         (entity-table (second statebody))
         (extras (fourth statebody))
         (entities (rest atom))
         (types (mapcar #'(lambda (entity)
                           (first (gethash entity entity-table)))
                        entities))
         (entity-numbers (mapcar #'(lambda (entity)
                                    (second (gethash entity entity-table)))
                                 entities))
         (pred-entry (gethash (first atom) pred-table))
         (pred-types (first pred-entry))
         (pred-array (third pred-entry)))

    (if (and entities (equal types pred-types))
        (setf (apply #'aref pred-array entity-numbers) 0)
      (setf (fourth statebody)
            (LIST-remove-atom-from-statebody atom extras))))
  statebody)

(defun BIT-statebody-atoms-for-predicate (statebody pred)
  (let* ((pred-table (first statebody))
         (type-table (third statebody))
         (extras (fourth statebody))
         (pred-entry (gethash pred pred-table))
         (pred-types (first pred-entry))
         (pred-type-counts (second pred-entry))
         (pred-array (third pred-entry)))

    (append
     (when pred-entry
       (mapcar #'(lambda (entities)
                  (cons pred entities))
               (BIT-statebody-search-array
                pred-array pred-type-counts
                (mapcar #'(lambda (type-name)
                           (second (gethash type-name type-table)))
                        pred-types)
                (mapcar #'(lambda (x) (declare (ignore x)) (list :variable 0))
                        pred-types))))
     (LIST-statebody-atoms-for-predicate extras pred))))

(defun BIT-statebody-candidate-atoms-for-goal (statebody goal)
  (let* ((pred-table (first statebody))
         (entity-table (second statebody))
         (type-table (third statebody))
         (extras (fourth statebody))
         (pred (first goal))
         (goal-terms (rest goal))
         (pred-entry (gethash pred pred-table))
         (pred-types (first pred-entry))
         (pred-type-counts (second pred-entry))
         (pred-array (third pred-entry)))

    (append
     (when (and pred-entry
                (= (length goal-terms) (length pred-types)))
       (let ((initial-counter
              (mapcar #'(lambda (entity pred-type)
                         (if (variablep entity)
                             (list :variable 0)
                           (let ((entry (gethash entity entity-table)))
                             (if (eq (first entry) pred-type)
                                 (list :fixed (second entry))
                               nil))))
                      goal-terms pred-types)))

         (unless (member nil initial-counter)
           (mapcar #'(lambda (entities)
                      (cons pred entities))
                   (BIT-statebody-search-array
                    pred-array pred-type-counts
                    (mapcar #'(lambda (type-name)
                               (second (gethash type-name type-table)))
                            pred-types)
                    initial-counter)))))
     (LIST-statebody-atoms-for-predicate extras pred))))

(defun BIT-statebody-search-array
  (pred-array pred-type-counts entity-number-tables complex-position)
  (let ((position (mapcar #'second complex-position)))
    (cond
     ((null position)
      nil)
     ((= (apply #'aref pred-array position) 1)
      (cons
       (mapcar #'(lambda (num entity-number-table)
                  (gethash num entity-number-table))
               position entity-number-tables)
       (BIT-statebody-search-array
        pred-array pred-type-counts entity-number-tables
        (BIT-statebody-increment-position
         complex-position pred-type-counts))))
     (t
      (BIT-statebody-search-array
       pred-array pred-type-counts entity-number-tables
       (BIT-statebody-increment-position
        complex-position pred-type-counts))))))

(defun BIT-statebody-increment-position
  (position pred-type-counts)
  (cond
   ((null position) nil)
   ((eq :fixed (first (first position)))
    (if (BIT-statebody-increment-position
         (rest position) (rest pred-type-counts))
        position
      nil))
   (t
    (incf (second (first position)))
    (cond 
     ((< (second (first position)) (first pred-type-counts))
      position)
     ((null (rest position))
      nil)
     ((BIT-statebody-increment-position
       (rest position) (rest pred-type-counts))
      (setf (second (first position)) 0)
      position)
     (t nil)))))

(defun BIT-statebody-atoms (statebody)
  (let ((acc nil)) 
    (maphash #'(lambda (pred lis)
                (declare (ignore lis))
                (setf acc
                      (append
                       (BIT-statebody-atoms-for-predicate statebody pred)
                       acc)))
             (first statebody))
    (remove-duplicates (append acc (LIST-statebody-atoms (fourth statebody))))))

(defun BIT-atom-in-statebody-p (atom statebody)
  (let* ((pred-table (first statebody))
         (entity-table (second statebody))
         (extras (fourth statebody))
         (entities (rest atom))
         (types (mapcar #'(lambda (entity)
                           (first (gethash entity entity-table)))
                        entities))
         (entity-numbers (mapcar #'(lambda (entity)
                                    (second (gethash entity entity-table)))
                                 entities))
         (pred-entry (gethash (first atom) pred-table))
         (pred-types (first pred-entry))
         (pred-array (third pred-entry)))

    (if (and entities (equal types pred-types))
        (= (apply #'aref pred-array entity-numbers) 1)
      (LIST-atom-in-statebody-p atom extras))))

;;; ------------------------------------------------------------------------
;;; Method and operator application
;;; modified to use the new SHOP2 operator syntax, which includes preconditions (swm)
;;; ------------------------------------------------------------------------

;;; If OPERATOR is applicable to TASK in STATE, then APPLY-OPERATOR returns
;;; the resulting state.  Otherwise it returns FAIL.
(defun apply-operator (state task-body operator protections depth)
  (let* ((standardized-operator (standardize operator))
         (head (second standardized-operator))
         (preconditions (third standardized-operator))
         (deletions (fourth standardized-operator))
         (additions (fifth standardized-operator))
         operator-unifier pre protections1 tempdel tempadd statetag
         head-subbed dels-subbed adds-subbed pu unifier
         cost-value cost-number)

    (setq operator-unifier (unify head task-body))
    (cond
      ((eql operator-unifier 'fail) (values 'fail protections 0))
      (t 
        (progn
          ;; first check the preconditions, if any
          (when preconditions
            (setq pre (apply-substitution preconditions operator-unifier))
            (setq pu (find-satisfiers pre state t))
            (unless pu
              (trace-print :operators (first head)
                           "~2%Depth ~s, inapplicable operator ~s~%     task ~s"
                           depth
                           (first head)
                           task-body)
              (return-from apply-operator (values 'fail preconditions 0))))
          (setq unifier (compose-substitutions operator-unifier (car pu)))
          (setq dels-subbed (apply-substitution deletions unifier))
          (setq adds-subbed (apply-substitution additions unifier))
          (setq head-subbed (apply-substitution head unifier))

;(format t "~%Sat to explain: ~s" (apply-substitution preconditions unifier))

	  (if *explanation*
	      (setq head-subbed `(,@(cons (first head-subbed)
				      (mapcar #'list
					      (rest (second operator))
					      (rest head-subbed)))
				  :explanation
				  ,(explain-satisfier
				   (apply-substitution preconditions unifier)
				   state))))

          (trace-print :operators (first head)
                       "~2%Depth ~s, applying operator ~s~%      task ~s~%       del ~s~%       add ~s"
                       depth
                       (first head)
                       task-body
                       dels-subbed
                       adds-subbed)
          (setq cost-value
                (eval (apply-substitution
                       (sixth standardized-operator) unifier)))
          (setq cost-number (if (numberp cost-value) cost-value 1))
          ;; process DELETE list
          (dolist (d dels-subbed)
            (unless (eql 'fail d)
              (if (eql (car d) 'forall)
                (let ((bounds (third d)) (dels (fourth d)) mgu2 tempd)
                  (setq mgu2 (find-satisfiers bounds state))
                  (dolist (m2 mgu2)
                    (setq tempd (apply-substitution dels m2))
                    (dolist (d1 tempd)
                      (setq tempdel
                            (my-union (list d1) tempdel :test #'equal)))))
                (setq tempdel (my-union (list d) tempdel :test #'equal)))))
          ;; process ADD list
          (dolist (a adds-subbed)
            (unless (eql 'fail a)
              (if (eql (car a) 'forall)
                (let ((bounds (third a)) (adds (fourth a)) mgu2 tempa)
                  (setq mgu2 (find-satisfiers bounds state))
                  (dolist (m2 mgu2)
                    (setq tempa (apply-substitution adds m2))
                    (dolist (a1 tempa)
                      (setq tempadd
                            (my-union (list a1) tempadd :test #'equal)))))
                (setq tempadd (my-union (list a) tempadd :test #'equal)))))

          (setq protections1 protections)               
          (setq statetag (tag-state state))
          ;; process PROTECTIONS generated by this operator
          (dolist (d tempdel)
            (if (eql (car d) :protection)
                (setq protections1 
                      (delete-protection 
                        protections1 (second d) depth (first head) state))
              (delete-atom-from-state d state depth (first head))))
          (if (protection-ok state protections1 head) 
              (setq protections protections1)
            (progn
              (retract-state-changes state statetag)
              (return-from apply-operator (values 'fail 'fail protections 0))))

          (dolist (a tempadd)
            (if (eql (car a) :protection)
                (setq protections (add-protection protections (second a) depth (first head) state))
              (add-atom-to-state a state depth (first head))))
          (return-from apply-operator 
                       (values head-subbed statetag 
                               protections cost-number)))))))

;;; if the state has everything that the protections list has, then 
;;; return true, else return nil
(defun protection-ok (state protections head)
  (dolist (p protections)
    (unless (atom-in-state-p (car p) state)
      (trace-print :operators (first head)
                   "~%Backtracking because operator ~s deleted the protected atom ~s"
                   (first head) (car p))
      (return-from protection-ok nil)))
  t)

;;; increase the count for PROTECT in the protection list
(defun add-protection (protections protect depth operator state)
  (let (p)
    (dolist (d protections)
      (if (equal protect (car d))
        (return (setq p d))))
    (cond ((null p)
           (trace-print :protections (car protect)
                        "~2%Depth ~s, incrementing protection count to ~s~%      atom ~s~%  operator ~s"
                        depth 1 protect operator)
           (setq protections (cons (list protect 1) protections)))
          (t 
           (setq protections (remove p protections))
           (setq p (cons (car p) (list (+ 1 (car (cdr p))))))
           (trace-print :protections (car protect)
                        "~2%Depth ~s, incrementing protection count to ~s~%      atom ~s~%  operator ~s"
                        depth (cadr p) protect operator)
           (setq protections (cons p protections))))))

;;; decrease the count for PROTECT in the protection list
(defun delete-protection (protections protect depth operator state)
  (let (p)
    (dolist (d protections)
      (if (equal protect (car d))
        (return (setq p d))))
    (if (null p)
      (trace-print :protections (car protect)
                   "~2%Depth ~s, protection count not decremented since it is already 0~%      atom ~s~%  operator ~s"
                   depth protect operator)
    (progn 
      (setq protections (remove p protections))
      (if (eql 1 (car (cdr p)))
        protections
        (progn 
          (setq p (cons (car p) (list (- (car (cdr p)) 1))))
          (trace-print :protections (car protect)
                        "~2%Depth ~s, decrementing protection count to ~s~%      atom ~s~%  operator ~s"
                        depth (cadr p) protect operator)
          (setq protections (cons p protections))))))))

;;; If METHOD is applicable to TASK in STATE, then APPLY-METHOD returns the
;;; resulting list of reductions.  Otherwise it returns NIL.
(defun apply-method (state task-body method protections depth)
  (declare (ignore protections))  ; do we really want to ignore protections?
  (let ((standardized-method (standardize method))  
        task-unifier state-unifiers pre tail)

    ;; see if the standardized-method's head unifies with TASK-BODY
    (setq task-unifier (unify (second standardized-method) task-body))
    (unless (eql task-unifier 'fail)
      ;; STANDARDIZED-METHOD's CDDR is a list 
      ;; (label_1 pre_1 d_1 label_2 pre_2 d_2 ...) which acts like an
      ;; if-then-else: we look for the first true pre_i, and then evaluate d_i
      (do* ((body (cddr standardized-method) (cdddr body)))
           ((null body) nil)
        
        ;; apply v to PRE and TAIL
        (setq pre (apply-substitution (second body) task-unifier))
        (setq tail (apply-substitution (third body) task-unifier))
        
        ;; find all matches to the current state
        (setq state-unifiers (find-satisfiers pre state))

        (if state-unifiers
            (let* ((answers-with-duplicates
                    (mapcan
                     #'(lambda (unifier)
                        (mapcar
                         #'(lambda (reduction)
                            (cons (first body) reduction))
                         (force-immediate-reduction 
                          (eval (apply-substitution tail unifier)))))
                     state-unifiers))
                   (answers
                    (remove-duplicates answers-with-duplicates
                     :test #'equal :from-end t)))
              (trace-print :methods (first body)
                           "~2%Depth ~s, applicable method ~s~%      task ~s~%reductions ~s"
                           depth
                           (first body)
                           task-body
                           answers)
              (return-from apply-method answers))
          (trace-print :methods (first body)
                     "~2%Depth ~s, inapplicable method ~s~%      task ~s"
                     depth
                     (first body)
                     task-body))))))

;;; This function forces there to be at least one immediate task in any
;;;  reduction so that when a method is reduced, it is immediately
;;;  reduced all the way to the bottom level.  For ordered reductions,
;;;  the function makes the first task immediate.  For unordered
;;;  reductions which have no immediate element, it splits the reduction
;;;  into seperate reductions each of which have exactly one immediate
;;;  element UNLESS the reduction already has an immediate element.  For
;;;  nested reductions, it applies the appropriate fix at each level.
;;;
;;; Examples:
;;;  (force-immediate ; non-nested
;;;    '((:ordered (:task a) (:task b))
;;;      (:unordered (:task x) (:task y))))
;;;  =>
;;;   ((:ordered (:task :immediate a) (:task b))
;;;    (:unordered (:task :immediate x) (:task y))
;;;    (:unordered (:task x) (:task :immediate y)))
;;;----
;;;  (force-immediate ; nested
;;;    '((:unordered (:ordered (:task a) (:task b))
;;;                  (:ordered (:task x) (:task y))))
;;;  =>
;;;   ((:unordered (:ordered (:task :immediate a) (:task b))
;;;                (:ordered (:task x) (:task y)))
;;;    (:unordered (:ordered (:task a) (:task b))
;;;                (:ordered (:task :immediate x) (:task y))))
;;;----
;;;  (force-immediate ; nested, but already immediate
;;;    '((:unordered (:ordered (:task a) (:task b))
;;;                  (:ordered (:task :immediate x) (:task y))))
;;;  =>
;;;   ((:unordered (:ordered (:task a) (:task b))
;;;                (:ordered (:task :immediate x) (:task y))))
(defun force-immediate (reductions)
  (mapcan #'force-immediate-reduction reductions))

(defun force-immediate-reduction (reduction)
  (cond
   ((null reduction) nil)
   ((already-immediate-p reduction)
    (list reduction))
   ((eq (first reduction) :task)
    (list `(:task :immediate ,@(get-task-body reduction))))
   ((eq (first reduction) :ordered)
    (mapcar #'(lambda (ordered-term)
    (append (list :ordered ordered-term)
      (rest (rest reduction))))
      (force-immediate-reduction (second reduction))))
   ((eq (first reduction) :unordered)
    (mapcar #'(lambda (unordered-result)
    (cons :unordered unordered-result))
      (force-immediate-unordered (rest reduction))))))
  
(defun already-immediate-p (reduction)
  (cond
   ((null reduction) nil)
   ((eq (first reduction) :task)
    (eq (second reduction) :immediate))
   ((eq (first reduction) :ordered)
    (already-immediate-p (second reduction)))
   ((eq (first reduction) :unordered)
    (find-if #'already-immediate-p (rest reduction)))))

(defun force-immediate-unordered (unordered-list &optional previous)
  (if (null unordered-list)
      nil
    (append
     (mapcar #'(lambda (unordered-term)
     `(,@previous
       ,unordered-term
       ,@(rest unordered-list)))
       (force-immediate-reduction (first unordered-list)))
     (force-immediate-unordered
      (rest unordered-list)
      (append previous (list (first unordered-list)))))))

;;; ------------------------------------------------------------------------
;;; Functions for creating and manipulating planning domains and problems
;;; ------------------------------------------------------------------------

;;; MAKE-DOMAIN gives the name NAME to the planning domain whose axioms,
;;; operators, and methods are those in in ITEMS.  More specifically, it
;;; puts the axioms, operators, and methods onto NAME's property list under
;;; the indicators :AXIOMS, :OPERATORS, and :METHODS, respectively
(defun make-domain (name &optional items)
  (if (null items) (setq items name))
  ;; name is ignored -- it's there only for compatibility with SHOP 1
  (format t "~%Defining domain ...")
  (let ((axioms (make-hash-table))
        (operators (make-hash-table))
        (methods (make-hash-table)))
    (setq items (append '((:operator (!!inop) () () 0)) items))
    (set-variable-property items)       ; set the primitive and variable properties
    (dolist (x (reverse items))
      (ecase (car x)
        ((:method) 
         (push (process-method x) (gethash (caadr x) methods)))
        ((:operator)
         (if (gethash (caadr x) operators)
           (error "There is more than one operator named ~s" (caadr x))
           (push (process-operator x) (gethash (caadr x) operators))))
        ((:-) 
         (setf x (regularize-axiom x))          ; convert SHOP 1.x axioms into SHOP 2 axioms if necessary
         (push x (gethash (caadr x) axioms))))) 
    (setq *axioms* axioms)
    (setq *operators* operators)
    (setq *methods* methods)))

;;; If the axiom has SHOP 1.x or mixed syntax, regularize-axiom will return it in SHOP 2 syntax.
(defun regularize-axiom (axiom)
  (if (shop2-axiom-p axiom) axiom       ; SHOP2 syntax
    (let ((ax-name (caadr axiom))       ; SHOP1 or mixed syntax
          (branch-counter 0)
          (labelified-axiom (list (car axiom) (cadr axiom)))
          (atail (cddr axiom)))
          (append labelified-axiom 
              (loop until (null atail) do
                    (incf branch-counter)
                    if (listp (car atail))       ; no label on this branch
                    append (list (gensym (format nil "~A~D--"
                                                 ax-name branch-counter))
                                 (pop atail))
                    else                ; this branch has a label
                    append (list (pop atail) (pop atail)))))))

(defun shop2-axiom-p (ax)
  (if (not (listp ax)) nil
      (let ((lax (length ax)))
        (if (<= lax 3) nil (rest-shop2-axiom-p (cddr ax))))))

(defun rest-shop2-axiom-p (ax)
  (let ((lax (length ax)))
    (cond ((< lax 2) nil)
          ((= lax 2) (if (and (atom (car ax)) (listp (cadr ax))) t nil))
          ((= lax 3) nil)
          (t (and (atom (car ax)) (listp (cadr ax)) (rest-shop2-axiom-p (cddr ax)))))))
  
(defun set-variable-property (x)
  (cond ((atom x) (and (symbolp x) 
                       (or (and (equal (elt (symbol-name x) 0) #\?)
                                (setf (get x 'variable) t))
                           (and (equal (elt (symbol-name x) 0) #\!)
                                (setf (get x 'primitive) t)))))
        ((consp x) (set-variable-property (car x))
         (set-variable-property (cdr x)))
        (t (error "the domain is malformed"))))


;;; MAKE-PROBLEM creates a planning problem named PROBLEM-NAME
;;; by putting STATE and TASK onto PROBLEM-NAME's
;;; property list under the indicators :STATE and :TASKS.
(defun make-problem (problem-name state tasks &optional extra)
  ;; if extra is given, then the args are problem-name, domain-name, state, tasks
  ;; in that case, we want to ignore domain-name
  (when extra
    (setq state tasks)
    (setq tasks extra)) 
  (format t "~%Defining problem ~s ..." problem-name)
  (setf *all-problems* (cons problem-name *all-problems*))
  (setf (get problem-name :state) state)
  (setf (get problem-name :tasks) (process-task-list tasks)))

;;; MAKE-PROBLEM-SET gives the name SET-NAME to the problems in PROBLEM-SET.
;;; More specifically, it puts PROBLEM-SET onto PROBLEM-NAME's
;;; property list under the indicators :STATE, :TASKS, and :DOMAIN
(defun make-problem-set (list-name problem-list)
  (format t "~%Defining problem set ~s ..." list-name)
  (setf (get list-name :problems) problem-list))

;;; Get the initial state for the problem named NAME
(defun get-state (name)
  (let ((answer (get name :state 'fail)))
    (if (eq answer 'fail) (error "No initial state for the name ~s" name))
    answer))

;;; Get the task list for the problem named NAME
(defun get-tasks (name)
  (let ((answer (get name :tasks 'fail)))
    (if (eq answer 'fail) (error "No task list for the name ~s" name))
    answer))

;;; Get the list of problems for the problem set named NAME
(defun get-problems (name &key print)
  (let ((answer (get name :problems 'fail)))
    (cond ((eq answer 'fail) (error "No problem list for the name ~s" name))
          (print (format t "~%~s" answer)))
    answer))

;;; DO-PROBLEMS runs FIND-PLANS on each problem in PROBLEMS, which may be
;;; either a problem-set name or a list of problems
(defun do-problems (problems &rest keywords)
  (if (not (listp problems))    ; treat NIL as an empty list, not a problem name
    (setq problems (get-problems problems)))
  (dolist (problem problems)
    (apply #'find-plans (cons problem keywords))))

;;; get-immediate-list returns the first member in tasklist tl that has the
;;; first task with the keyword :immediate
;;; Furthermore, all iNOP's are considered immediate, because they don't do
;;; anything so you might as well run them right away (this is just a minor
;;; efficiency tweak if you have a domain with lots of unordered tasks which
;;; reduce to iNOP's).  - Murdock 12/01
(defun get-immediate-list (tl)
  (dolist (obj tl)
    (if (or (eq :immediate (second obj)) (eq '!!inop (second obj))) 
      (return-from get-immediate-list (list obj))))
  (return-from get-immediate-list nil))

;;; SEEK-PLANS is the basic planning engine.  Here are its arguments:
;;;  - STATE is the current state;
;;;  - TASKS is the list of remaining tasks to accomplish;
;;;  - TOP-TASKS is the agenda of tasks to be worked first, 
;;;    based on the :immediate keyword
;;;  - PARTIAL-PLAN is the partial plan accumulated so far, with the
;;;    steps listed in reverse order;
;;;  - PARTIAL-PLAN-COST is the total cost of the partial plan
;;;  - DEPTH is the current level of recursive calls to SEEK-PLANS;
;;;  - WHICH-PLANS has the same meaning as the WHICH argument to FIND-PLANS
;;; Rather than returning the plans, SEEK-PLANS puts them into the dynamically
;;; scoped variable *PLANS-FOUND*.  Ordinarily I'd consider this to be poor
;;; coding style, but in this case it makes the coding significantly cleaner.
;;;
;;; Seek-plans has been broken down into the following lower level
;;; functions, below: seek-plans-task, seek-plans-primitive,
;;;                   seek-plans-nonprimitive, seek-plans-null

(defun seek-plans (state tasks top-tasks partial-plan partial-plan-cost
                         depth which-plans protections)
  (when (time-expired-p)
    (if *print-stats* (format t "~%Terminating because the time limit has expired."))
    (throw *internal-time-tag* nil))

  (setq *current-plan* partial-plan)
  (setq *current-tasks* tasks)
  (setq *expansions* (1+ *expansions*))

  (cond
    ;; if no top-tasks to accomplish, then we have an answer
    ((or (null top-tasks) (equal top-tasks '(NIL)))
      (seek-plans-null state which-plans partial-plan partial-plan-cost depth))
   
    ;; else if we've hit the depth bound, then fail
    ((and *depth-cutoff* (>= depth *depth-cutoff*)))

    ;; else look at the agenda
    (t
     (let* ((immediate-tasks (get-immediate-list top-tasks))
            task1)
       (if immediate-tasks
         (progn
           (setq task1 (car immediate-tasks))
           (seek-plans-task
            task1 state tasks top-tasks partial-plan partial-plan-cost
            depth which-plans protections)
	   (and *plans-found* (eq which-plans :first) 
		(not (optimize-continue-p))
		(return-from seek-plans nil))
           (trace-print :tasks (get-task-name task1)
                        "~2%Depth ~s, backtracking from task~%      task ~s"
                        depth
                        task1))
         ;; no :IMMEDIATE tasks, so do the rest of the tasks on the agenda
         (progn
           (dolist (task1 top-tasks)
             (seek-plans-task task1 state tasks top-tasks partial-plan
                              partial-plan-cost depth which-plans
                              protections)
             (and *plans-found* (eq which-plans :first) 
		  (not (optimize-continue-p))
                  (return-from seek-plans nil))
             (trace-print :tasks (get-task-name task1)
                          "~2%Depth ~s, backtracking from task ~s"
                          depth
                          task1))))))))

(defun seek-plans-task (task1 state tasks top-tasks partial-plan
                              partial-plan-cost depth which-plans
                              protections)
  (let ((task-name (get-task-name task1))
        (task-body (get-task-body task1)))
    (trace-print :tasks task-name
                 "~2%Depth ~s, trying task ~s"
                 depth
                 task1)
    (if (primitivep task-name)
      (seek-plans-primitive task1 task-name task-body state tasks top-tasks
                            partial-plan partial-plan-cost depth which-plans
                            protections)
      (seek-plans-nonprimitive task1 task-name task-body state tasks top-tasks
                               partial-plan partial-plan-cost depth
                               which-plans protections))))

(defun seek-plans-primitive (task1 task-name task-body state tasks top-tasks
                                   partial-plan partial-plan-cost depth 
                                   which-plans protections
                                   &aux m tasks1 top-tasks1)
  (setq m (car (gethash task-name *operators*)))
  (unless m
    (error "No operator for task ~s" task1))
  (multiple-value-bind
   (result1 tag protections cost)
   (apply-operator state task-body m protections depth)
   (when (eql result1 'fail)
     (return-from seek-plans-primitive nil))

    ;; The task was satisfied by a primitive operator.  Prune it
    ;; from tasks, update top-tasks1, and call seek-plans recursively.

    (when *plan-tree* (record-operator task1 result1))

    (multiple-value-setq (top-tasks1 tasks1) (delete-task-top-list top-tasks tasks task1))
    
    (let ((new-cost (+ cost partial-plan-cost)))
     (when (and *optimize-cost*
		(not (acceptable-cost-p new-cost)))
       (trace-print :operators task-name
		    "~2%Depth ~s, backtracking from operator ~s because the plan costs too much~%     task ~s~%     cost ~s"
		    depth task-name (cdr task1) new-cost)
       (retract-state-changes state tag)
       (return-from seek-plans-primitive nil))
     
     (seek-plans state tasks1 top-tasks1 
		 (cons cost (cons result1 partial-plan))
		 new-cost (1+ depth) which-plans protections)
     (retract-state-changes state tag)
     nil)))

(defun seek-plans-nonprimitive (task1 task-name task-body state tasks 
                                      top-tasks partial-plan partial-plan-cost
                                      depth which-plans protections)
  (let (result1 tasks1 top-tasks1 label r)
    (dolist (m (gethash task-name *methods*))
      (setq result1 (apply-method state task-body m protections depth))
      (when result1
        (dolist (lr result1)
          (setq label (car lr))
          (setq r (cdr lr))
	  (when *plan-tree* (record-reduction task1 r))
          (trace-print :methods label
                       "~2%Depth ~s, applying method ~s~%      task ~s~%   precond ~s~% reduction ~s"
                       depth label task1 (fourth m) r)
          (trace-print :tasks task-name
                       "~2%Depth ~s, reduced task ~s~% reduction ~s"
                       depth task1 r)
          (setq top-tasks1 (replace-task-top-list top-tasks task1 r))
          (setq tasks1 (replace-task-main-list tasks task1 r))
          (seek-plans state tasks1 top-tasks1 partial-plan
                      partial-plan-cost (1+ depth) which-plans
                      protections)
          (and *plans-found* (eq which-plans :first)
               (not (optimize-continue-p))
               (return-from seek-plans-nonprimitive nil)))))))

;;; Called when there are no top level tasks to run
(defun seek-plans-null (state which-plans partial-plan partial-plan-cost depth)
  (when (fboundp 'plan-found-hook)
    ; This hook is useful for external routines that invoke SHOP2 
    ;  and want to do something whenever a plan is found.  For example,
    ;  the Java / SHOP2 interface uses this to store the final state
    ;  of the plan.
    (funcall (fdefinition 'plan-found-hook)
	     state which-plans partial-plan partial-plan-cost depth))
  (let ((acceptable-cost (acceptable-cost-p partial-plan-cost)))
    (unless (and *optimize-cost* acceptable-cost
                 (eq which-plans :first)
                 (equal *optimal-cost* partial-plan-cost))
      (when (and *optimize-cost* acceptable-cost)
        (when (or (null *optimal-cost*)
                  (< partial-plan-cost *optimal-cost*))
          (setq *optimal-plan* partial-plan)
          (setq *optimal-cost* partial-plan-cost))
        (if (member which-plans '(:first :shallowest))
            (setq *plans-found* nil)
          (setq *plans-found*
                (delete-if
                 #'(lambda (plan) (not (acceptable-cost-p (plan-cost plan))))
                 *plans-found*))))
      (cond
       ((or (eq which-plans :all-shallowest)
            (and (eq which-plans :shallowest) *optimize-cost*))
        (when (not (equal *depth-cutoff* depth))
          (setq *plans-found* nil)
          (setq *depth-cutoff* depth)))
       ((eq which-plans :shallowest)
        (setq *depth-cutoff* (1- depth)
              *plans-found* nil)))
      (when acceptable-cost
        (push-last (strip-NOPs (reverse partial-plan)) *plans-found*))))
  nil)

(defun get-task-name (task1)
  (if (eq (second task1) :immediate)
    (third task1)
    (second task1)))

(defun get-task-body (task1)
  (if (eq (second task1) :immediate)
    (cddr task1)
    (cdr task1)))

;;; This function returns true iff additional optimization needs to be done.
(defun optimize-continue-p ()
  (cond
   ((not *optimize-cost*) nil)
   ((and (numberp *optimize-cost*)
         (numberp *optimal-cost*))
    (>= *optimal-cost* *optimize-cost*))
   (t t)))

;;; This function returns true iff a given cost is acceptible under the
;;;  current optimization requirements.
(defun acceptable-cost-p (cost)
  (cond
   ((numberp *optimize-cost*)
    (<= cost *optimize-cost*))
   ((and *optimize-cost*
         (not (eq *optimal-plan* 'fail)))
    (<= cost *optimal-cost*))
   (t t)))

;;; This function returns true iff there is a time limit and it has expired.
(defun time-expired-p ()
  (if *internal-time-limit*
      (>= (- (get-internal-run-time) *start-run-time*)
          *internal-time-limit*)
    nil))

(defmacro catch-internal-time (&rest body)
  `(if *internal-time-limit*
       (catch *internal-time-tag*
	 ,@body)
     (progn ,@body)))
  
;;; ------------------------------------------------------------------------
;;; :plan-tree option functions
;;; ------------------------------------------------------------------------

; This function records the parents of each subtask in a reduction.
(defun record-reduction (task1 reduction)
  (let ((all-subtasks (extract-subtasks reduction)))
    (setf *subtask-parents*
	  (nconc
	   *subtask-parents*
	   (mapcar #'(lambda (subtask) (list subtask task1))
		   all-subtasks)))))

(defun extract-subtasks (reduction)
  (cond
   ((atom reduction) nil)
   ((eq (first reduction) :task) (list reduction))
   (t (append (extract-subtasks (first reduction))
	      (extract-subtasks (rest reduction))))))

; This function records the task atom that produced a given operator
; instance.
(defun record-operator (task1 operator)
  (setf *operator-tasks*
	(cons
	 (list operator task1)
	 *operator-tasks*)))

; This function is executed at the end of the planning process to produce
;  the final tree.
(defun extract-tree (plan)
  (strip-tree-tags
   (let* ((operator-nodes (plan-operator-nodes plan))
	  (all-nodes (plan-tree-nodes operator-nodes))
	  (root-nodes (node-children nil all-nodes)))
     (mapcar #'(lambda (root-node) (extract-subtree root-node all-nodes))
	     root-nodes))))

(defun strip-tree-tags (tree)
  (cond
   ((atom tree) tree)
   ((and (eq (first tree) :task)
	 (eq (second tree) :immediate))
    (rest (rest tree)))
   ((eq (first tree) :task)
    (rest tree))
   (t
    (cons
     (strip-tree-tags (first tree))
     (strip-tree-tags (rest tree))))))

(defun extract-subtree (root-node nodes)
  (let ((children (node-children root-node nodes)))
    (if children
	(cons root-node
	      (mapcar #'(lambda (child) (extract-subtree child nodes))
		      children))
      root-node)))

(defun node-children (node nodes)
  (remove-if-not
   #'(lambda (other-node) 
       (eq (second (assoc (or (operator-task other-node) other-node)
			  *subtask-parents*))
	   node))
   nodes))

(defun plan-tree-nodes (base-nodes)
  (let* ((extended-base-nodes
	  (remove-duplicates
	   (extend-plan-tree-nodes base-nodes)
	   :from-end t))
	 (new-base-nodes
	  (set-difference extended-base-nodes base-nodes)))
    (if new-base-nodes
	(plan-tree-nodes extended-base-nodes)
      base-nodes)))

(defun extend-plan-tree-nodes (base-nodes)
  (if (null base-nodes) nil
    (let* ((operator-task (operator-task (first base-nodes)))
	   (task (or operator-task (first base-nodes)))
	   (parent (second (assoc task *subtask-parents*)))
	   (rest-nodes (cons (first base-nodes)
			     (extend-plan-tree-nodes (rest base-nodes)))))
      (if parent
	  (cons parent rest-nodes)
	rest-nodes))))

(defun operator-task (operator)
  (second (assoc (second operator) *operator-tasks*)))

(defun plan-operator-nodes (plan &optional (n 0))
  (if (null plan) nil
    (cons
     (list (second plan) (first plan) n)
     (plan-operator-nodes (rest (rest plan)) (1+ n)))))

;;; ------------------------------------------------------------------------
;;; Functions to create some of the output 
;;; ------------------------------------------------------------------------

;;; debugging output, indented by INDENTATION number of spaces 
(defun indented-format (indentation &rest body)
  (format nil "~%~A" (make-string indentation :initial-element #\space))
  (apply #'format (cons nil body)))

(defun print-stats-header (label)
  (if *print-stats*
    (format t
            "~%~7@a Plans Mincost Maxcost Expansions Inferences  CPU time  Real time"
            label)))

(defun print-stats (depth plans tasks inferences runtime realtime)
  (if *print-stats*
    (format t "~%~6@a~6@a ~7@a ~7@a~11@s~11@s~10,3f~11,3f"
            depth (length plans)
            (if plans (to-string (apply #'min (mapcar #'plan-cost plans)) 2) "-")
            (if plans (to-string (apply #'max (mapcar #'plan-cost plans)) 2) "-")
            tasks inferences
            (/ runtime internal-time-units-per-second)
            (/ realtime internal-time-units-per-second))))

; Converts a number to a string.
; Examples:
;   (to-string 3 2) => "3"
;   (to-string 3.0 2) => "3.0"
;   (to-string 3.8 2) => "3.8"
;   (to-string 3.775 2) => "3.78"
; This is sort of like the ~F format string, but it doesn't add extra 0's
;  at the end.
(defun to-string (num &optional (max-decimal 1))
  (if (integerp num) (format nil "~a" num)
    (let* ((base-string (format nil (format nil "~~,~aF" max-decimal) num))
	   (trimmed-string (string-right-trim "0" base-string)))
      (if (eq #\. (char trimmed-string (1- (length trimmed-string))))
	  (concatenate 'string trimmed-string "0")
	trimmed-string))))

(defun plan-cost (plan)
  (if (null plan)
    0
    (+ (cadr plan) (plan-cost (cddr plan)))))

(defun shorter-plan (plan)
  (cond 
   ((null plan) nil)
   ((internal-operator-p (first (first plan)))
    (shorter-plan (rest (rest plan))))
   (t
    (cons (first plan) (shorter-plan (rest (rest plan)))))))

(defun internal-operator-p (operator-name)
  (if (symbolp operator-name) 
      (let ((name (symbol-name operator-name)))
        (and 
         (>= (length name) 2)
         (equal (elt name 0) #\!)
         (equal (elt name 1) #\!)))
    nil))

(defun print-output-file (plan)
  (dolist (obj plan)
    (format t ", ")
    (print1 obj)))

(defun print1 (obj)
  (if (atom obj) (format t " ~A" obj)
      (progn
        (format t "(~S" (car obj))
        (dolist (obj1 (cdr obj))
          (print1 obj1))
        (format t ")"))))

(defun get-alist (variables)
  (let (alist vlist)
    (setq vlist (mapcar #'(lambda (x) (declare (ignore x))
                            (variable-gensym))
                            variables))
    (setq alist (pairlis variables vlist))
    (values alist vlist)))

;;; Plan accessible debugging information.

;;; The following 4 routines, which were written by Chiu, are not
;;; invoked by any other SHOP routines.  Instead, they are intended to
;;; be invoked in call or eval forms within a SHOP domain; they are
;;; intended to assist with debugging a domain.

(defun query-current-state (first-symbol)
  (state-all-atoms-for-predicate *current-state* first-symbol))

(defun print-current-state ()
  (format t "~%~A~%" (state-atoms *current-state*))
  t)

(defun print-current-tasks ()
  (format t "~%~A~%" *current-tasks*)
  t)

(defun print-current-plan ()
  (let ((tplan *current-plan*)
        (fplan nil)
        (i 1))
    (do* ((cc (pop tplan) (pop tplan))
          (tt (pop tplan) (pop tplan)))
         ((not cc))
      (when (not (internal-operator-p (car tt)))
        (push tt fplan)))
    (dolist (s fplan)
      (format t "~A: ~A~%" i s)
      (setf i (1+ i)))
    t))


;;; this is the main function that does the pre-processing, it
;;; looks through the preconditions finding the forall 
;;; conditions and replace the variables in that condition
(defun process-pre (pre)
  (let (answer pre1 alist vlist)
    (if (atom pre)
      (return-from process-pre pre)
      (progn
  (setq pre1 (car pre))
  (cond  
   ((listp pre1)
    (setq answer (cons (process-pre pre1) (process-pre (cdr pre)))))
   ((eql pre1 'or)
    (setq answer (cons 'or (process-pre (cdr pre)))))
   ((eql pre1 'imply)
    (setq answer (cons 'imply (process-pre (cdr pre)))))
   ((eql pre1 ':first)
    (setq answer (cons ':first (process-pre (cdr pre)))))
   ((eql pre1 'forall)
    (progn
      (multiple-value-setq (alist vlist) (get-alist (second pre)))
      (setq answer (list 'forall vlist 
             (process-pre (apply-substitution (third pre) alist))
             (process-pre (apply-substitution (fourth pre) alist))))))
   (t
    (setq answer pre)))
  (return-from process-pre answer)))))

;;; this function pre-processes the methods, replace every 
;;; variable defined by the forall condition to a previously
;;; unused variable. It also regularizes the methdods in old SHOP format.
(defun process-method (method)
  (let ((method-head (cadr method))
  (answer nil)
  (tail nil)
  (branch-counter 0)
  (method-name nil)
  clause first-of-clause)
    (setq method-name (car method-head))
    (setq answer (list (car method) method-head))
    (setq tail (cddr method))
    (append answer 
            (loop until (null tail) do  
                  (incf branch-counter)
                  when (and (not (null (car tail))) (symbolp (car tail)))
                  append (list (pop tail))       ; skip over a label
                  else append (list (gensym (format nil "~A~D--" 
                                              method-name branch-counter)))
                  append (list  (process-pre (pop tail))
                                ;; check to see if there is a quote or 
                                ;; backquote in the front of this list (SHOP1
                                ;; or SHOP2 syntax) and process accordingly
                                (progn 
                                  (setq clause (pop tail)
                                        first-of-clause (car clause))
                                  (cond ((or (eq first-of-clause 'quote)
                                             (eq first-of-clause *back-quote-name*))
                                         ;; this next bit of strangeness is to take the quote 
                                         ;; off the front of the task list,
                                         ;; decompose the task list, then slap the quote back on
                                        (list first-of-clause (process-task-list (cadr clause))))
                                       ((search-tree 'call clause)
                                        (list 'simple-backquote (process-task-list clause)))
                                       (t (list 'quote (process-task-list clause))))))))))

;;; returns t if item is found anywhere in the tree; doubly recursive,
;;; but only runs once per method definition.
(defun search-tree (item tree)
  (cond ((eq item tree) t)
        ((not (consp tree)) nil)
        (t (or (search-tree item (car tree)) (search-tree item (cdr tree))))))

;;; The above puts QUOTE on the front, but inside the clause there could be a "call" which is
;;; ignored.  Previously, instead of "call" there would have been a backquote around the form and
;;; commas in front of the "call".  After substituting the variables, the form is eval'd and the
;;; backquote is done or the quote is just taken off.  Now, something like backquote needs to be done.
;;; perhaps the form should be searched for an instance of "call"; if found, put "backquote" on the
;;; front and modify the backquote function to handle "call" like comma.

;;; this function pre-process the operator, replace every 
;;; variable defined by the forall condition to a previously
;;; unused variable. It also addresses the issue of different
;;; syntaxes of operators in different versions of SHOP.
(defun process-operator (operator)
  (let ((lopt (length operator)))
    (append (list (first operator) (second operator))
        (cond ((= lopt 4)             ; a SHOP 1 operator, no cost specified
               (list '() 
                     (process-pre (third operator))
                     (process-pre (fourth operator))
                     1.0))
              ((and (= lopt 5) (numberp (fifth operator)))    
               (list '()              ; a SHOP 1 operator, with cost specified
                     (process-pre (third operator))
                     (process-pre (fourth operator))
                     (process-pre (fifth operator))))
              ((= lopt 5)             ; a SHOP 2 operator, no cost specified
               (list (process-pre (third operator))
                     (process-pre (fourth operator))
                     (process-pre (fifth operator))
                     1.0))
              ((= lopt 6)             ; a SHOP 2 operator, with cost specified
               (list (process-pre (third operator))
                     (process-pre (fourth operator))
                     (process-pre (fifth operator))
                     (process-pre (sixth operator))))
              (t (error (format nil "mal-formed operator ~A in process-operator" operator)))))))


;;; ------------------------------------------------------------------------
;;; Top-level calls to the planner
;;; ------------------------------------------------------------------------


;;; FIND-PLANS looks for solutions to the planning problem named PROBLEM.
;;; The keyword arguments are as follows:
;;;   :WHICH tells what kind of search to do.  Its possible values are:
;;;      :FIRST      - depth-first search, returning the first plan found.
;;;      :ALL        - depth-first search for *all* plans.
;;;      :SHALLOWEST - depth-first search for the shallowest plan in the
;;;                    search space (this usually is also the shortest plan).
;;;                    If there's more than one such plan, return the first.
;;;      :ALL-SHALLOWEST - depth-first search for all shallowest plans.
;;;      :ID-FIRST   - iterative deepening search, returning the first plan.
;;;      :ID-ALL     - iterative deepening search for all shallowest plans.
;;;   :VERBOSE says how much information to print about the plans SHOP2
;;;            finds.  Its values can be any of the following:
;;;      0 or NIL    - print nothing
;;;      1 or :STATS - print some statistics on SHOP2's operation
;;;      2 or :PLANS - print the stats and print all plans found, but omit
;;;           operator costs and omit all operators whose names start with "!!"
;;;      3 or :LONG-PLANS - print the stats and plans, including all operator
;;;           costs and all operators (even those whose names start with "!!")
;;;   :GC says whether to do a garbage collection before calling SEEK-PLANS

(defun find-plans (problem
                   &key (which *which*) (verbose *verbose*)
                   (gc *gc*) (pp *pp*) (state *state-encoding*) 
                   (plan-tree *plan-tree*) (optimize-cost *optimize-cost*)
                   (time-limit *time-limit*) (explanation *explanation*))
  (declare (ignorable gc))
  #+(or :MCL :allegro)(if gc (gc))
  (let* ((*start-run-time* (get-internal-run-time))
         (*start-real-time* (get-internal-real-time))
         (*internal-time-limit* 
          (if time-limit (* time-limit
                            internal-time-units-per-second)
            nil))
         (*internal-time-tag* (gensym))
         (*print-pretty* pp)
         (*state-encoding* state)
         (*plan-tree* plan-tree) (*subtask-parents* nil) (*operator-tasks* nil)
         (*optimize-cost* optimize-cost)
         (*expansions* 0) (*inferences* 0)
         (total-expansions 0) (total-inferences 0)
         (old-expansions 0) (old-inferences 0)
         (total-run-time 0) (total-real-time 0)
         (*depth-cutoff* nil) (*plans-found* nil)
         (*optimal-plan* 'fail)
	 (*explanation* explanation) (*attribution-list* nil)
	 (*external-access* (fboundp 'external-access-hook))
	 (*trace-query* (fboundp 'trace-query-hook))
         (state (make-state (get-state problem)))
         (tasks (get-tasks problem))
         *optimal-cost* *verbose*
         new-run-time new-real-time top-tasks)
    
    (setq top-tasks (get-top-tasks tasks))
    (determine-verbosity verbose)

    (when *print-stats*
      (format t "~%---------------------------------------------------------------------------")
      (format t "~%Problem ~s with :WHICH = ~s, :VERBOSE = ~s" problem which verbose)    
      (if optimize-cost
        (format t ", OPTIMIZE-COST = ~s" optimize-cost)))
        
    (ecase which
      ((:id-first :id-all)
       (print-stats-header "Depth")
       (do ((*depth-cutoff* 0 (1+ *depth-cutoff*)))
         (*plans-found* nil)
         (setq new-run-time (get-internal-run-time)
               new-real-time (get-internal-real-time))
         (catch-internal-time
          (seek-plans state tasks top-tasks nil 0 0 
                      (if (eq which :id-first) :first :all) nil))
         (setq new-run-time (- (get-internal-run-time) new-run-time)
               new-real-time (- (get-internal-real-time) new-real-time)
               total-run-time (+ total-run-time new-run-time)
               total-real-time (+ total-real-time new-real-time)
               total-expansions (+ total-expansions *expansions*)
               total-inferences (+ total-inferences *inferences*))
         (print-stats *depth-cutoff* *plans-found* *expansions*
                      *inferences* new-run-time new-real-time)
         (and (equal *expansions* old-expansions)
              (equal *inferences* old-inferences)
              (return nil))  ; abort if nothing new happened on this iteration
         (setq old-expansions *expansions*
               old-inferences *inferences*)
         (setq *expansions* 0
               *inferences* 0)))
      
      ((:first :all :shallowest :all-shallowest)
       (catch-internal-time
        (seek-plans state tasks top-tasks nil 0 0 which nil))
       (setq total-expansions *expansions*
             total-inferences *inferences*)))
    
    (if (numberp *optimize-cost*)
        (setq *plans-found*
              (delete-if
               #'(lambda (plan)
                   (> (plan-cost plan) *optimize-cost*))
               *plans-found*)))

    (setq total-run-time (- (get-internal-run-time) *start-run-time*)
          total-real-time (- (get-internal-real-time) *start-real-time*))

    (print-stats-header "Totals:")
    (print-stats "" *plans-found* total-expansions total-inferences
     total-run-time total-real-time)
    (let ((plan-trees
           (when *plan-tree* (mapcar #'extract-tree *plans-found*))))
      (when *print-plans*
        (cond
         (*plan-tree*
          (format t "~%Plans trees:~%~s~%~%" plan-trees))
         (*pshort*
          (format t "~%Plans:~%~s~%~%" (mapcar #'shorter-plan *plans-found*)))
         (t
          (format t "~%Plans:~%~s~%~%" *plans-found*))))
      (values (if *plan-tree* (list plan-trees *plans-found*) *plans-found*)
              (+ 0.0
                 (/ total-run-time
                    internal-time-units-per-second))))))

(format t "~2%~a~%" *version*)
