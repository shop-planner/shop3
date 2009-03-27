;;; -*- Mode: common-lisp; package: common-lisp-user; -*-
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

(defpackage :shop2
    (:nicknames :shop)
    (:use :common-lisp :shop2.unifier :shop2.common :shop2.theorem-prover)
    #+sbcl
    (:shadow #:defconstant)
    (:shadow #:domain)
    (:export #:shop-fail

             ;; The following set of symbols is defined in
             ;; shop-common, but are replayed for one export by shop2.
             ;; Packages :use'ing shop2 should *not* also :use
             ;; shop-common.

             #:domain #:domain-axioms #:domain-name
             #:domain-operators #:domain-methods
             #:axioms #:operators #:methods #:name

             #:*domain* #:*current-state* #:*inferences* #:*external-access*
             #:*attribution-list* #:*state-encoding*

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
             #:*shop-trace* #:*shop-trace-stream* #:*trace-query*

             #:unify #:standardize #:fail
             #:apply-substitution #:compose-substitutions
             #:fix-uninterned-bindings #:binding-list-value
             #:make-binding-list #:make-binding
             #:variablep #:groundp
             #:extract-variables #:shop-union
             #:get-alist
             ;; end of shop-unifier stuff


             ;; The following set of symbols is defined in
             ;; shop-theorem-prover, but are replayed for one export
             ;; by shop2.  Packages :use'ing shop2 should *not* also
             ;; :use shop-theorem-prover
             #:explain-satisfier #:find-satisfiers
             #:extract-variables

             #:call
             #:imply
             #:forall
             #:exists
             #:assign
             #:enforce
             #:assign*
             #:setof
             #:protect

             #:defproblem
             #:make-problem
             #:delete-problem
             #:def-problem-set
             #:make-problem-set
             #:*make-problem-silently*

             #:find-plans
             #:do-problems
             #:shop-trace
             #:shop-untrace

             ;; domain objects and their functions...
             #:defdomain
             #:set-domain
             #:find-domain
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

             ;; accessors to problems and domains...
             #:get-state
             #:get-tasks
             #:get-problems

             ;; get rid of cost information out of a plan tree...
             #:remove-costs

             ;; classes and methods to be specialized
             #:domain
             #:task-sorter
             #:process-operator
             #:process-axiom
             #:process-method
             #:handle-domain-options
             #:parse-domain-items

             #:pddl-domain

             ;; ENQ-related methods
             #:apply-operator
             #:seek-plans-primitive
             #:seek-plans-null

             ;; tree-accessors
             #:complex-node-p
             #:complex-node-task
             #:complex-node-children
             #:primitive-node-p
             #:remove-internal-operators
             #:tree-node-task
             #:tree-node-task-name
             #:task-name
             #:find-complex-node-if
             #:find-all-complex-node-if
             #:find-complex-node-for-task
             #:find-all-complex-node-for-task

             ;; conditions
             #:no-method-for-task
             #:task-arity-mismatch

             ;; things you might want to use in your domain definitions
             #:variablep

             ;; other utilities
             #:shop-union

             ;; exporting for val
             #:validator-export
             #:write-pddl-plan
             ))

(defpackage :shop2-user
    (:nicknames :shop-user)
    (:use :shop2 :common-lisp)
    (:export #:explain-satisfier
             #:find-satisfiers))


