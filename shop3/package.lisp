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
(in-package :common-lisp-user)

(defpackage :shop3
    (:nicknames :shop :shop2)
    (:documentation "The SHOP3 package is the package that exposes SHOP3's API.")
    (:use :common-lisp :shop3.unifier :shop3.common :shop3.theorem-prover
          :iterate)
    (:import-from #:shop3.common #:domain-core)
    (:import-from #:shop3.theorem-prover #:+numerical-comparisons+
                  #:fluent-value
                  #:f-exp-value
                  #:fluents-mixin
                  #:fluent-function-p
                  #:fluent-expr-p
                  #:fluent-comparison-p
                  #:*random-generator*)
    ;; so can be imported into prepare-return-values
    (:intern #:make-plan-copy)
    (:import-from #:shop3.unifier #:+primitive-property-name+ #:primitive-symbol-p)
    (:shadowing-import-from #:shop3.theorem-prover #:random #:shop-random)

    #+sbcl
    (:shadow #:defconstant)
    (:export #:shop-fail

             #:domain-axioms #:domain-name
             #:domain-operators #:domain-methods
             #:axioms #:operators #:methods #:name

             #:*domain* #:*current-state* #:*inferences* #:*external-access*
             #:*attribution-list* #:*state-encoding*

             #:*random-generator*
             #:shop-random

             ;; quash some default definition messages
             #:*define-silently*

             #:state-candidate-atoms-for-goal #:state-atoms
             #:state-all-atoms-for-predicate
             #:copy-state
             #:retract-state-changes
             #:add-atom-to-state
             #:delete-atom-from-state
             #:tag-state
             #:make-state

             ;; The following set of symbols is defined in
             ;; shop-unifier, but are replayed for one export by
             ;; shop2.  Packages :use'ing shop2 should *not* also :use
             ;; shop-unifier.
             #:trace-print
             #:shop-trace-info
             #:*shop-trace* #:*shop-trace-stream* #:*trace-query*

             #:unify #:standardize #:fail
             #:apply-substitution #:compose-substitutions
             #:fix-uninterned-bindings #:binding-list-value
             #:make-binding-list #:make-binding
             #:variablep #:groundp
             #:extract-variables #:shop-union
             #:get-alist

             ;; defined lisp types
             #:shop-variable
             #:binding-list
             ;; end of shop-unifier stuff


             ;; The following set of symbols is defined in
             ;; shop-theorem-prover, but are replayed for one export
             ;; by shop2.  Packages :use'ing shop2 should *not* also
             ;; :use shop-theorem-prover
             #:explain-satisfier #:find-satisfiers
             #:extract-variables
             #:query

             #:call
             #:imply
             #:forall
             #:exists
             #:assign
             #:enforce
             #:assign*
             #:setof
             #:bagof
             #:protect

             ;; metric fluents
             #:increase
             #:decrease
             #:scale-up
             #:scale-down

             ;; top type for PDDL type hierarchy
             #:object

             #:defproblem
             #:make-problem
             #:delete-problem
             #:def-problem-set
             #:make-problem-set
             #:*make-problem-silently*
             #:find-problem
             #:problem-name
             #:copy-shop-problem

             #:find-plans
             #:find-plans-stack         ; explicit stack version
             #:repair-plan
             #:do-problems
             #:shop-trace
             #:shop-untrace
             #:plan-states
             #:plan-final-state

             ;; domain objects and their functions...
             #:defdomain
             #:set-domain
             #:find-domain
             #:delete-domain
             #:*defdomain-verbose*

             ;; default planner search setting --- should be replaced
             ;; by subclassing a planner object later.
             #:*which*

             ;; hooks
             #:plan-found-hook
             #:trace-query-hook
             #:external-access-hook

             ;; This function prints a list of the axioms for the
             ;; domain whose name is name defaults to the most
             ;; recently defined domain.
             #:print-axioms
             ;; This function prints a list of the operators for the domain whose
             ;; name is name; defaults to the most recently defined domain.
             #:print-operators

             ;; This function prints a list of the methods for the
             ;; domain whose name is name; defaults to the most
             ;; recently defined domain.
             #:print-methods

             ;; this function is designed for use in debugging SHOP
             ;; domains and problems.
             #:print-current-state
             #:state-trajectory
             #:prop-sorter

             ;; accessors to problems and domains...
             #:get-state
             #:get-tasks
             #:get-problems

             ;; get rid of extraneous out of a plan tree...
             #:remove-costs
             #:shorter-plan
             #:internal-operator-p

             ;; classes
             #:domain
             #:problem

             ;; generic functions to be specialized
             #:task-sorter
             #:sort-tasks
             #:sort-methods
             #:sort-results
             #:process-operator
             #:process-op
             #:process-axiom
             #:process-method
             #:handle-domain-options
             #:parse-domain-items

             ;; subclasses of domain for PDDL support
             #:pddl-domain    ; deprecated: this was implicitly an ADL domain
             #:simple-pddl-domain
             #:negative-preconditions-mixin
             #:disjunctive-preconditions-mixin
             #:universal-preconditions-mixin
             #:existential-preconditions-mixin
             #:quantified-preconditions-mixin ;; inherits from the above 2 mixins
             #:conditional-effects-mixin
             #:equality-mixin
             #:pddl-typing-mixin
             #:costs-mixin
             #:adl-mixin
             #:adl-domain
             #:fluents-mixin


             ;; MIXIN
             #:pure-logic-domain-mixin
             #:static-predicates-mixin

             ;; ENQ-related methods
             #:apply-operator
             #:seek-plans-primitive
             #:seek-plans-null

             ;; tree-accessors
             #:copy-plan-tree
             #:complex-node-p
             #:complex-node-task
             #:complex-node-children
             #:complex-node-reduction-label
             #:complex-node
             #:primitive-node-p
             #:primitive-node-task
             #:primitive-node-cost
             #:primitive-node-position
             #:primitive-node
             #:remove-internal-operators
             #:tree-node-task
             #:tree-node-task-name
             #:task-name
             #:find-complex-node-if
             #:find-primitive-node-if
             #:all-primitive-nodes
             #:find-all-primitive-nodes-if
             #:find-primitive-node-for-task
             #:find-all-complex-node-if
             #:find-complex-node-for-task
             #:find-all-complex-node-for-task
             #:make-complex-node
             #:node-parent

             ;; plan return accessors (for ESS)
             #:plan
             #:tree

             ;; conditions
             #:no-method-for-task
             #:task-arity-mismatch
             #:singleton-variable
             #:incorrect-arity-error
             #:incomplete-dependency-error

             ;; things you might want to use in your domain definitions
             #:variablep

             ;; other utilities
             #:shop-union

             ;; print a plan cleanly, for human readability
             #:pprint-plan

             ;; check a plan from a PDDL domain/problem for correctness
             #:validate-plan
             #:source-pddl-domain       ; domain accessor -- helper for validation...

             ;; exporting for val
             #:validator-export
             #:write-pddl-plan

             ;; exporting so that it can be overridden
             #:plan-value

             #:shop-random))

(defpackage plan-tree
  (:nicknames shop-extended-plan-tree)
  (:use common-lisp iterate)
  (:intern #:top-node-lookup-table
           #:copy-plan-tree-node)
  (:import-from :alexandria #:when-let)
  (:export #:dependency
           #:establisher
           #:consumer
           #:prop
           #:tree-node
           #:tree-node-p
           #:tree-node-task
           #:tree-node-expanded-task
           #:tree-node-dependencies
           #:tree-node-parent
           #:primitive-tree-node
           #:make-primitive-tree-node
           #:complex-tree-node
           #:make-complex-tree-node
           #:complex-tree-node-children
           #:complex-tree-node-method-name
           #:top-node
           #:make-top-node
           #:pseudo-node
           #:unordered-tree-node
           #:make-unordered-tree-node
           #:make-ordered-tree-node
           #:ordered-tree-node
           #:make-dependency
           ;; finders
           #:find-plan-step
           #:find-task-in-tree
           #:find-tree-node-if

           #:copy-plan-tree
           #:plan-tree->sexp
           ))

(defpackage prepare-return-values
  (:nicknames #:prv)
  (:use :common-lisp :iterate)
  (:import-from #:shop3 #:apply-substitution #:make-plan-copy)
  (:import-from #:alexandria #:when-let)
  (:import-from #:plan-tree
                #:tree-node
                #:tree-node-task
                #:tree-node-parent
                #:tree-node-dependencies
                #:primitive-tree-node
                #:tree-node-expanded-task
                #:top-node
                #:top-node-lookup-table
                #:dependency
                #:make-dependency
                #:establisher
                #:consumer
                #:prop
                #:copy-plan-tree-node
                #:complex-tree-node-children)
  (:export #:prepare-return-values))

(defpackage :shop3-user
    (:nicknames :shop-user :shop2-user)
    (:documentation "SHOP3-USER is a \"scratch\" package for
experimenting with the SHOP3 API. Typically any real work
should be moved to a dedicated package you create for yourself.")
    (:use :shop3 :common-lisp))
