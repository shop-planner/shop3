
(in-package :shop3)

;; replaced with a constant that is defined in the ASDF system definition. [2007/10/24:rpg]
;;(defparameter *version* "SHOP2 version 2.0 alpha")

(defconstant +shopyright+
"Copyright (C) 2004-2021 SIFT, LLC.

Original SHOP2 code Copyright (C) 2002  University of Maryland.

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
;;; Additions and modifications made by Robert P. Goldman, and other SIFT employees.
;;; Portions created by SIFT employees
;;; Copyright (C) 2002-2019 SIFT, LLC.  These additions and
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
;;;    Ugur Kuter (SIFT, LLC)
;;;    Alex Plotnick (SIFT, LLC)
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


;;; ------------------------------------------------------------------------
;;; Top-level calls to the planner
;;; ------------------------------------------------------------------------
(defun find-plans (problem
                   &rest options
                   &key domain (which *which*) (verbose *verbose*)
                        (gc *gc*) (pp *pp*)
                        (plan-tree *plan-tree*) (optimize-cost *optimize-cost*)
                        (collect-state *collect-state*)
                        (time-limit *time-limit*) (explanation *explanation*)
                        (depth-cutoff *depth-cutoff*)
                        ;; [mpelican:20090226.1824CST] state is obsolete, find-plans will error if it is supplied
                        (state *state-encoding* state-supplied-p)
                        (tasks nil tasks-supplied-p)
                        (state-type nil state-type-supplied-p)
                        hand-steer leashed
                        (out-stream t)
                     &allow-other-keys)
  "FIND-PLANS looks for solutions to the planning PROBLEM.
   PROBLEM should be a problem-designator (a PROBLEM or a symbol naming one).
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
                      SHOP3 domains, since normal SHOP3 domains have order-
                      dependent semantics.

     :VERBOSE says how much information to print about the plans SHOP3
              finds.  Its values can be any of the following:
        0 or NIL    - print nothing
        1 or :STATS - print some statistics on SHOP3's operation
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
  (declare (ignorable state))

  (when (and state-supplied-p (not (eq which :mcts)))
    (error "State argument to find-plans is obsolete.~%Please use state-type or default-state-type slot in domain class."))
  (when gc
    (trivial-garbage:gc :full t))

  (let* ((*start-run-time* (get-internal-run-time))
         (*start-real-time* (get-internal-real-time))
         (*internal-time-limit*
          (if time-limit (* time-limit
                            internal-time-units-per-second)
            nil))
         (*which* which)
         (*internal-time-tag* (gensym))
         (*print-pretty* pp)
         ;; [mpelican:20090226.1825CST] obsolete, please use state-type arg or default-state-type slot in domain class
         (*state-encoding* :obsolete-state-encoding-variable)
         (*plan-tree* plan-tree)
         (*collect-state* (or collect-state plan-tree))
         (*subtask-parents* (make-subtask-parents-table))
         (*operator-tasks* (make-operator-task-table))
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
         ;; FIXME: Embarrassed to admit that I don't recall why
         ;; state must always be supplied for Monte Carlo Tree Search [2022/11/10:rpg]
         (state (if (and (eq which :mcts)
                         state-supplied-p)
                    state
                     (apply 'make-initial-state domain
                            (if state-type-supplied-p
                                state-type
                                (default-state-type domain))
                            (problem->state domain problem))))
         (tasks (if tasks-supplied-p tasks (get-tasks problem)))
         *optimal-cost*
         (*verbose* verbose)
         ;; used for tree-building code.
         (*unifiers-found* nil)
         ;; if you want the state trajectory [2004/09/14:rpg]
         (*states-found* nil)
         ;; used dynamically to allow the user to choose tasks for
         ;; expansion, rather than allowing SHOP3 to search
         ;; autonomously. [2004/03/30:rpg]
         (*hand-steer* hand-steer)
         (*leashed* leashed)
         (*domain* domain)
         )
    (apply 'find-plans-1 domain state tasks which problem :out-stream out-stream
           (alexandria:remove-from-plist options
                                         :which :domain :out-stream :verbose :gc :pp
                                         :plan-tree :optimize-cost :collect-state
                                         :time-limit :explanation :depth-cutoff
                                         :state-type :tasks :hand-steer :leashed))))

(defun find-plans-1 (domain state tasks which problem &key (out-stream t) &allow-other-keys)
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
                        (if (eq which :id-first) :first :all)
                        nil nil))
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
          (seek-plans domain state tasks top-tasks nil 0 0 which nil nil))
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
  (when *before-extract-trees-hook*
    (funcall *before-extract-trees-hook*))
  (assert (= (length plans-found) (length unifiers-found)))
  (loop for plan in plans-found
        for unifier in unifiers-found
        for tree = (extract-tree plan)
        collect (apply-substitution tree unifier)))



(format t "~2%SHOP3 version ~a~%~a~%" *shop-version* +shopyright+)

;; (eval-when (:compile-toplevel)
;; (warn "Bogus warning."))


