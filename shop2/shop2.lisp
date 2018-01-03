;;; -*- Mode: common-lisp; package: shop2; -*-

(in-package :shop2)

;; replaced with a constant that is defined in the ASDF system definition. [2007/10/24:rpg]
;;(defparameter *version* "SHOP2 version 2.0 alpha")

(defconstant +shopyright+
"Copyright (C) 2002  University of Maryland.
Modifications by SIFT, LLC personnel Copyright (C) 2004-2016 SIFT, LLC.
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
;;; Additions and modifications made by Robert P. Goldman, John
;;; Maraist.  Portions created by Drs. Goldman and Maraist are
;;; Copyright (C) 2004-2007 SIFT, LLC.  These additions and
;;; modifications are also available under the MPL/GPL/LGPL licensing
;;; terms.
;;;
;;; Contributor(s):
;;;    Dana S. Nau (UMD)
;;;    Yue Cao (???)
;;;    Tsz-Au Chiu (UMD)
;;;    Okhtay Ilghami (UMD)
;;;    Ugur Kuter (UMD)
;;;    Steve Mitchell (???)
;;;    J. William Murdock (IBM Research)
;;;    Robert P. Goldman (SIFT, LLC)
;;;    John Maraist (SIFT, LLC)
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

;;; For modifications made to SHOP2 under the Integrated Learning Contract:
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


;;; -----------------------------------------------------------------------
;;; REVISION LOG

; ****************** Revisions since SHOP2 1.2 Release

;;;  [2007/04/30:jm] Dividing the main SHOP2 Lisp file to separate the
;;;  theorem prover, and providing the prover as a separate package.

;;;  [2005/05/26:rpg] Substantial changes that can be found in the CVS
;;;  changelog.
;;   [2004/12/30:rpg] An enormous modification for check-in to
;;   sourceforge.  This set of changes contains modifications to
;;   improve the return of plan trees from SHOP2; the introduction of
;;   LIST type DEFSTRUCTS (STATE, OPERATOR, and OPERATOR-NODE) to
;;   provide named accessors to data structures (makes a little easier
;;   to understand for now, could be an intermediate step towards more
;;   efficient implementation using true DEFSTRUCTs later); added
;;   problem-state and problem-tasks "accessor" functions to make
;;   PROBLEM implementation more abstract; added an ASSIGN*
;;   pseudo-logical operator that provides a multiple-binding
;;   equivalent to ASSIGN; modified the SHOP-TRACE mechanism (see
;;   change to SHOP-TRACE argument pattern); added a (rudimentary)
;;   manually-directed planning option; fixed a problem with
;;   retrieving preconditions with already-bound variables; made it
;;   possible to extract a state-trajectory, as well as an operator
;;   sequence and plan tree from FIND-PLANS; also folded in mods from
;;   unreleased UMD 1.3 beta version.

;;   2004.04.05 (rpg) Made functions that encapsulated the choice of
;;   next task to plan for, so that they could be replaced or wrapped
;;   to customize the behavior of SHOP2.  See TASK-SORTER and
;;   CHOOSE-IMMEDIATE-TASK.
;;   Changed the binding of *CURRENT-STATE* to happen earlier, in
;;   SEEK-PLANS, instead of later (in FIND-SATISFIERS).

;;   2004.02.07 (uk) Fixed the variable binding problem in function
;;   MAKE-PROBLEM, that was reported by Robert Goldman as a bug in
;;   DEFPROBLEM macro. The function MAKE-PROBLEM is helpful for backward-
;;   compatability with SHOP -- the earlier version of SHOP2.

;;   2004.02.06 (rpg) Fixed problems in variable-binding interacting
;;   with returning trees.  Added new TREE-STORE object argument to the
;;   SEEK-PLANS-<foo> functions.  Also added code to FIND-PLANS to
;;   recover those trees.  This is only an interim modification: the
;;   original functionaly (although buggy) has not been removed yet.

;;   2003.11.17 (jwm) Provisional modification to the way protections
;;   are handled: protections can now be arbitrary logical expressions
;;   (including references to axioms).  Since this capability creates
;;   the possibility that an addition to a state could violate a
;;   protection, an additional change was required so that protections
;;   are now checked after deletes AND adds (before they were checked
;;   after deletes but before adds).  This has the side-effect that an
;;   operator that deletes and then adds back a protected atom does
;;   not constitute a protection violation (it's not clear that the
;;   former behavior was desirable anyway).

;;   2003.11.04 (rpg) Moved SHOP-TRACE comments into the function as a document
;;   string.

;;   2003.11.03 (rpg) Added an ASSIGN* intended to make it possible to have a
;;   lisp function compute multiple possible bindings for a variable.


; ****************** Revisions since the SHOP2 1.1 Release

;;   2003.9.17 (jwm) Revised the :external keyword (see below); now if
;;   external access fails, an attempt is made to prove the reference
;;   internally.

;;   2003.9.2 (jwm) Modified the call to plan-found-hook (see below)
;;   to send the final plan rather than the internal partial plan.

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


;;; ------------------------------------------------------------------------
;;; Top-level calls to the planner
;;; ------------------------------------------------------------------------
(defun find-plans (problem
                   &key
                     domain (which *which*) (verbose *verbose*)
                     (gc *gc*) (pp *pp*)
                     (plan-tree *plan-tree*) (optimize-cost *optimize-cost*)
                     (collect-state *collect-state*)
                     (time-limit *time-limit*) (explanation *explanation*)
                     (depth-cutoff *depth-cutoff*)
                     ;; [mpelican:20090226.1824CST] state is obsolete, find-plans will error if it is supplied
                     (state *state-encoding* state-supplied-p)
                     (state-type nil state-type-supplied-p)
                     hand-steer leashed
                     (out-stream t))
  "FIND-PLANS looks for solutions to the planning problem named PROBLEM.
   The keyword arguments are as follows:
     :WHICH tells what kind of search to do.  Its possible values are:
        :FIRST      - depth-first search, returning the first plan found.
        :ALL        - depth-first search for *all* plans.
        :SHALLOWEST - depth-first search for the shallowest plan in the
                      search space (this usually is also the shortest plan).
                      If there's more than one such plan, return the first.
        :ALL-SHALLOWEST - depth-first search for all shallowest plans.
        :ID-FIRST   - iterative deepening search, returning the first plan.
        :ID-ALL     - iterative deepening search for all shallowest plans.
        :RANDOM     - Randomized search.  Used by Monroe. Not for normal
                      SHOP2 domains, since normal SHOP2 domains have order-
                      dependent semantics.
     :VERBOSE says how much information to print about the plans SHOP2
              finds.  Its values can be any of the following:
        0 or NIL    - print nothing
        1 or :STATS - print some statistics on SHOP2's operation
        2 or :PLANS - print the stats and print all plans found, but omit
             operator costs and omit all operators whose names start with \"!!\"
        3 or :LONG-PLANS - print the stats and plans, including all operator
             costs and all operators (even those whose names start with \"!!\")
     :GC says whether to do a garbage collection before calling SEEK-PLANS
     :PLAN-TREE indicates whether or not to return plan tree(s).
     :COLLECT-STATE indicates whether or not to return final state(s).  For backward-
             compatibility, states are also returned whenever :PLAN-TREE is true.
             This should probably eventually change.
  RETURN VALUES:
     PLANS FOUND --- a list of plans.  Each plan is a list that alternates a
                     between instantiated operators and costs
     RUN TIME --- floating point value in seconds
       /if/ the PLAN-TREE keyword argument is supplied, there will be two
       additional return values:
     PLAN-TREES --- a list of plan trees, whose form is specified elsewhere.
     FINAL-STATES --- a list of final state structures, one per plan."
  (declare (ignore state))

  (when state-supplied-p
    (error "State argument to find-plans is obsolete.~%Please use state-type or default-state-type slot in domain class."))
  ;;; should add a dependency on TRIVIAL-GARBAGE to get rid of this... [2011/09/28:rpg]
  #+(or ccl allegro sbcl clisp abcl ecl)
  (when gc #+allegro (excl:gc t)
        #+sbcl (sb-ext:gc :full t)
        #+ccl (ccl:gc)
        #+clisp (ext:gc)
        #+cmucl (extensions:gc :full t)
        #+abcl (extensions:gc)
        #+lispworks (hcl:gc-generation t) ; add case for lispworks 5/1/13 BWM
        #+ecl (ext:gc t)
        )
  #-(or :cCL :allegro :sbcl clisp cmucl abcl lispworks ecl)
  (when gc (cerror "Just continue, skip GC."
                   "Requested GC before planning, but do not know how to request GC for this lisp implementation (see source code)."))
  (let* ((*start-run-time* (get-internal-run-time))
         (*start-real-time* (get-internal-real-time))
         (*internal-time-limit*
          (if time-limit (* time-limit
                            internal-time-units-per-second)
            nil))
         (*internal-time-tag* (gensym))
         (*print-pretty* pp)
         ;; [mpelican:20090226.1825CST] obsolete, please use state-type arg or default-state-type slot in domain class
         (*state-encoding* :obsolete-state-encoding-variable)
         (*plan-tree* plan-tree)
         (*collect-state* (or collect-state plan-tree))
         (*subtask-parents* nil) (*operator-tasks* nil)
         (*optimize-cost* optimize-cost)
         (*expansions* 0) (*inferences* 0)
         ;; make this controllable [2004/08/06:rpg]
         (*depth-cutoff* depth-cutoff)
         (*plans-found* nil)
         (*optimal-plan* 'fail)
         (*explanation* explanation) (*attribution-list* nil)
         (*external-access* (fboundp 'external-access-hook))
         (*trace-query* (fboundp 'trace-query-hook))
         (problem (find-problem problem t))
         (domain (cond (domain
                        (etypecase domain
                          (symbol
                           (find-domain domain :error))
                          (domain domain)))
                       ((domain-name problem)
                        (find-domain (domain-name problem) :error))
                       (*domain* *domain*)
                       (t
                        (error "Domain not supplied and problem does not specify domain."))))
         (state (apply 'make-initial-state domain
                       (if state-type-supplied-p
                           state-type
                         (default-state-type domain))
                       (problem->state domain problem)))
         (tasks (get-tasks problem))
         *optimal-cost*
         (*verbose* verbose)
         ;; used for tree-building code.
         (*unifiers-found* nil)
         ;; if you want the state trajectory [2004/09/14:rpg]
         (*states-found* nil)
         ;; used dynamically to allow the user to choose tasks for
         ;; expansion, rather than allowing SHOP2 to search
         ;; autonomously. [2004/03/30:rpg]
         (*hand-steer* hand-steer)
         (*leashed* leashed)
         (*domain* domain)
         )
    (find-plans-1 domain state tasks which problem out-stream)))

(defun find-plans-1 (domain state tasks which problem &optional (out-stream t))
  (let ((total-expansions 0) (total-inferences 0)
         (old-expansions 0) (old-inferences 0)
         (total-run-time 0) (total-real-time 0)
        new-run-time new-real-time top-tasks)


    ;; we need to be sure that the pieces of the input tasks are
    ;; properly recognized as being/not being variables, etc. This
    ;; used to be done in make-problem, but now that
    ;; set-variable-property has a domain argument... [2007/03/14:rpg]
    (set-variable-property domain tasks)

    (setq top-tasks (get-top-tasks tasks))
    ;;    (format t "~%Top-Tasks: ~a" top-tasks)
    (determine-verbosity *verbose*)

    ;; FIXME: the format calls in here should probably be PRINT-STATS or PRINT-STATS-HEADER.
    (when *print-stats*
      (format out-stream "~&---------------------------------------------------------------------------")
      (format out-stream "~%~@[Problem ~s with ~]:WHICH = ~s, :VERBOSE = ~s" problem which *verbose*)
      (when *optimize-cost*
          (format out-stream ", OPTIMIZE-COST = ~s" *optimize-cost*))
      (terpri))
    ;; if *hand-steer* allows the user to abort planning.
    (catch 'user-done
      (ecase which
        ((:id-first :id-all)
         (print-stats-header "Depth" out-stream)
         (do ((*depth-cutoff* 0 (1+ *depth-cutoff*)))
             ((or (time-expired-p)      ;need to check here for expired time....
                  (and *plans-found*
                       ;; I think the second disjunct here is probably
                       ;; unnecessary now [2005/01/10:rpg]
                       (not (or (optimize-continue-p (if (eq which :id-first) :first :all))
                                (eq which :id-all)))))
              nil)
           (setq new-run-time (get-internal-run-time)
                 new-real-time (get-internal-real-time))
           (catch-internal-time
            (seek-plans domain state tasks top-tasks nil 0 0
                        (if (eq which :id-first) :first :all) nil
                        ;; unifier
                        nil
                        ))
           (setq new-run-time (- (get-internal-run-time) new-run-time)
                 new-real-time (- (get-internal-real-time) new-real-time)
                 total-run-time (+ total-run-time new-run-time)
                 total-real-time (+ total-real-time new-real-time)
                 total-expansions (+ total-expansions *expansions*)
                 total-inferences (+ total-inferences *inferences*))
           (print-stats *depth-cutoff* *plans-found* *expansions*
                        *inferences* new-run-time new-real-time out-stream)
           (and (equal *expansions* old-expansions)
                (equal *inferences* old-inferences)
                (progn (format t "~&Ending at depth ~D: no new expansions or inferences.~%"
                               *depth-cutoff*)
                       (return nil)))           ; abort if nothing new happened on this iteration
           (setq old-expansions *expansions*
                 old-inferences *inferences*)
           (setq *expansions* 0
                 *inferences* 0)))

        ((:first :all :shallowest :all-shallowest :random)
         (catch-internal-time
          (seek-plans domain state tasks top-tasks nil 0 0 which nil
                      ;; unifier
                      nil))
         (setq total-expansions *expansions*
               total-inferences *inferences*))))

    ;; I'm pretty sure that this needs to be modified to update the
    ;; unifiers and states found as well.  [2005/01/06:rpg]
    (if (numberp *optimize-cost*)
        (setq *plans-found*
              (delete-if
               #'(lambda (plan)
                   (> (plan-cost plan) *optimize-cost*))
               *plans-found*)))


    (setq total-run-time (- (get-internal-run-time) *start-run-time*)
          total-real-time (- (get-internal-real-time) *start-real-time*))

    (print-stats-header "Totals:" out-stream)
    (print-stats "" *plans-found* total-expansions total-inferences
                 total-run-time total-real-time out-stream)
    (let ((plan-trees
           (when *plan-tree*
             (extract-trees *plans-found* *unifiers-found*))))
      (when *print-plans*
        (cond
         (*plan-tree*
          (format t "~%Plan trees:~%~s~%~%" plan-trees))
         (*pshort*
          (format t "~%Plans:~%~s~%~%" (mapcar #'shorter-plan *plans-found*)))
         (t
          (format t "~%Plans:~%~s~%~%" *plans-found*))))
      (cond ((and *plan-tree* *collect-state*)
             (values *plans-found*
                  (+ 0.0
                     (/ total-run-time
                        internal-time-units-per-second))
                  plan-trees
                  *states-found*))
            (*collect-state*
             (values *plans-found*
                  (+ 0.0
                     (/ total-run-time
                        internal-time-units-per-second))
                  nil
                  *states-found*))
            (t
             (values *plans-found*
                     (+ 0.0
                        (/ total-run-time
                           internal-time-units-per-second))))))))

(defun extract-trees (plans-found unifiers-found)
  (assert (= (length plans-found) (length unifiers-found)))
  (loop for plan in plans-found
        for unifier in unifiers-found
        for tree = (extract-tree plan)
        collect (apply-substitution tree unifier)))



(format t "~2%SHOP2 version ~a~%~a~%" *shop-version* +shopyright+)

;; (eval-when (:compile-toplevel)
;; (warn "Bogus warning."))


