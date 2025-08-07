;;;
;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;;
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS
;;; IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
;;; implied. See the License for the specific language governing
;;; rights and limitations under the License.
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
;;; Alternatively, the contents of this file may be used under the
;;; terms of either of the GNU General Public License Version 2 or
;;; later (the "GPL"), or the GNU Lesser General Public License
;;; Version 2.1 or later (the "LGPL"), in which case the provisions of
;;; the GPL or the LGPL are applicable instead of those above. If you
;;; wish to allow use of your version of this file only under the
;;; terms of either the GPL or the LGPL, and not to allow others to
;;; use your version of this file under the terms of the MPL, indicate
;;; your decision by deleting the provisions above and replace them
;;; with the notice and other provisions required by the GPL or the
;;; LGPL. If you do not delete the provisions above, a recipient may
;;; use your version of this file under the terms of any one of the
;;; MPL, the GPL or the LGPL.
;;; ---------------------------------------------------------------------

;;; Smart Information Flow Technologies Copyright 2006-2025
;;; Unpublished work
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

(fiveam:def-suite* pddl-tests)

(fiveam:def-suite* short-pddl-tests :in pddl-tests)

(fiveam:def-fixture simple-pddl-actions ()
  (let ((action-def '(:action drive
                      :parameters (?v - vehicle
                                   ?from ?to - location
                                   ?fbefore ?fafter - fuel-level)
                      :precondition (and (at ?v ?from)
                                     (accessible ?v ?from ?to)
                                     (fuel ?v ?fbefore)
                                     (next ?fbefore ?fafter))
                      :effect (and (not (at ?v ?from))
                               (at ?v ?to)
                               (not (fuel ?v ?fbefore))
                               (fuel ?v ?fafter))))
        (untyped-action-def '(:action drive
                              :parameters (?v - vehicle
                                           ?from ?to - location
                                           ?fbefore ?fafter)
                              :precondition (and (at ?v ?from)
                                             (accessible ?v ?from ?to)
                                             (fuel ?v ?fbefore)
                                             (next ?fbefore ?fafter))
                              :effect (and (not (at ?v ?from))
                                       (at ?v ?to)
                                       (not (fuel ?v ?fbefore))
                                       (fuel ?v ?fafter))))
        (typelist '(?v - vehicle
                    ?from ?to - location
                    ?fbefore ?fafter)))
    (declare (ignorable typelist action-def untyped-action-def))
    (&body)))

(fiveam:test type-list-translation
  (fiveam:with-fixture simple-pddl-actions ()
    (let ((domain (make-instance 'adl-domain
                                 :name (gentemp "DOMAIN")
                                 :%pddl-types '(vehicle location object))))
      (fiveam:is
       (equalp (list '(?v ?from ?to ?fbefore ?fafter)
                     '(vehicle location location object object))
               (multiple-value-list
                (typed-list-vars typelist domain)))))))

(fiveam:def-fixture action-test-fixtures ()
  (let* ((*define-silently* t)
         (domain (make-instance 'adl-domain
                                :name (gentemp (symbol-name '#:domain))
                                :%pddl-types '(location vehicle fuel-level object)))
         (action (process-action domain action-def))
         (untyped-action (process-action domain untyped-action-def)))
    (declare (ignorable action untyped-action))
    (&body)))

(fiveam:test pddl-actions
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:with-fixture action-test-fixtures ()
      (fiveam:is (equal '(drive ?v ?from ?to ?fbefore ?fafter)
                        (pddl-action-head action)))))
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:with-fixture action-test-fixtures ()
      #+nil(break "Domain is ~S" domain)
      (fiveam:is (equal '(and
                          (and
                           (enforce (vehicle ?v) "Parameter ~a unbound or ill-typed. Should be ~a"  '?v 'vehicle)
                           (enforce (location ?from) "Parameter ~a unbound or ill-typed. Should be ~a"  '?from 'location)
                           (enforce (location ?to) "Parameter ~a unbound or ill-typed. Should be ~a"  '?to 'location)
                           (enforce (fuel-level ?fbefore) "Parameter ~a unbound or ill-typed. Should be ~a"  '?fbefore 'fuel-level)
                           (enforce (fuel-level ?fafter) "Parameter ~a unbound or ill-typed. Should be ~a"  '?fafter 'fuel-level))
                          (and (at ?v ?from) (accessible ?v ?from ?to)
                           (fuel ?v ?fbefore) (next ?fbefore ?fafter)))
                        (pddl-action-precondition action)))
      (fiveam:is (equal '(and
                          (and
                           (enforce (vehicle ?v) "Parameter ~a unbound or ill-typed. Should be ~a"  '?v 'vehicle)
                           (enforce (location ?from) "Parameter ~a unbound or ill-typed. Should be ~a"  '?from 'location)
                           (enforce (location ?to) "Parameter ~a unbound or ill-typed. Should be ~a"  '?to 'location)
                           (enforce (object ?fbefore) "Parameter ~a unbound or ill-typed. Should be ~a"  '?fbefore 'object)
                           (enforce (object ?fafter) "Parameter ~a unbound or ill-typed. Should be ~a"  '?fafter 'object))
                          (and (at ?v ?from) (accessible ?v ?from ?to)
                           (fuel ?v ?fbefore) (next ?fbefore ?fafter)))
                        (pddl-action-precondition untyped-action)))))
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:with-fixture action-test-fixtures ()
      (fiveam:is (equal '(and (not (at ?v ?from)) (at ?v ?to)
                          (not (fuel ?v ?fbefore)) (fuel ?v ?fafter))
                        (pddl-action-effect action)))))
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:with-fixture action-test-fixtures ()
      (fiveam:is (eql (pddl-action-cost-fun action) 1.0))
      (fiveam:is
       (equal
        '(PDDL-ACTION (!WALK ?FROM ?TO)
          (and
           (and
            (enforce (loc ?from) "Parameter ~a unbound or ill-typed. Should be ~a"  '?from 'loc)
            (enforce (loc ?to) "Parameter ~a unbound or ill-typed. Should be ~a"  '?to 'loc))
           (AT ROBOT ?FROM))
          (AND (NOT (AT ROBOT ?FROM)) (AT ROBOT ?TO))
          1.0)
        (progn
          (defdomain (#.(gentemp (symbol-name '#:domain))
                        :type adl-domain
                        :redefine-ok t)
              ((:types loc)
               (:action walk
                :parameters (?from ?to - loc)
                :precondition (at robot ?from)
                :effect (and (not (at robot ?from))
                             (at robot ?to)))))
          (operator *domain* '!walk)))))))



(fiveam:def-fixture add-del-fixtures ()
  (progn
    (let ((shop3:*define-silently* t))
      (defdomain (test-add-del-domain
                  :type pddl-domain
                  :redefine-ok t)
          ((:types loc)
           (:action walk
            :parameters (?from ?to - loc)
            :precondition (at robot ?from)
            :effect (and (not (at robot ?from))
                         (at robot ?to)))))
      (&body))))

(fiveam:test add-del-tests

  ;;; NB the two forms differ in the initial state.

  (fiveam:with-fixture add-del-fixtures ()
    (fiveam:is
     (equal
      (sort (copy-list'((AT ROBOT NEW-YORK) (loc new-york) (loc new-jersey))) 'prop-sorter)
      (sort
       (let ((*state-encoding* :list))
         (declare (special *state-encoding*))
         (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey) (loc new-jersey) (loc new-york)))))
           (apply-action *domain* state
                         '(!walk new-jersey new-york)
                         (operator *domain* '!walk)
                         nil 0 nil)
           (state-atoms state)))
       'prop-sorter)))
    (fiveam:signals
        error
      (let ((*state-encoding* :list))
        (declare (special *state-encoding*))
        (let ((state (make-initial-state *domain* *state-encoding*  nil)))
          (apply-action *domain* state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil))))))


(fiveam:def-fixture quantified-preconditions-fixtures ()
  (let* ((*define-silently* t)
         (domain (defdomain (quantified-preconditions-domain
                             :type pddl-domain
                             :redefine-ok t)
                     ((:types airplane airplanetype direction segment))))
         (act (process-action domain
                              '(:action move
                                :parameters
                                (?a - airplane ?t - airplanetype
                                 ?d1 - direction ?s1 ?s2  - segment
                                 ?d2 - direction)
                                :precondition
                                (forall (?s - segment)
                                 (imply
                                  (and
                                   (is-blocked ?s ?t ?s2 ?d2)
                                   (not (= ?s ?s1)))
                                  (not (occupied ?s))))))))
    (&body)))

(fiveam:test quantified-preconditions
  (fiveam:with-fixture quantified-preconditions-fixtures ()
    (fiveam:is
     (equal
      '(and
        (and
         (enforce (airplane ?a) "Parameter ~a unbound or ill-typed. Should be ~a"  '?a 'airplane)
         (enforce (airplanetype ?t) "Parameter ~a unbound or ill-typed. Should be ~a"  '?t 'airplanetype)
         (enforce (direction ?d1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d1 'direction)
         (enforce (segment ?s1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s1 'segment)
         (enforce (segment ?s2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s2 'segment)
         (enforce (direction ?d2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d2 'direction))
        (forall (?s)
         (segment ?s)
         (imply
          (and
           (is-blocked ?s ?t ?s2 ?d2)
           (not (= ?s ?s1)))
          (not (occupied ?s)))))
      (pddl-action-precondition act)))

    (fiveam:is
     (equal '(and
              (and (enforce (airplane ?a) "Parameter ~a unbound or ill-typed. Should be ~a"  '?a 'airplane)
               (enforce (airplanetype ?t) "Parameter ~a unbound or ill-typed. Should be ~a"  '?t 'airplanetype)
               (enforce (direction ?d1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d1 'direction)
               (enforce (segment ?s1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s1 'segment)
               (enforce (segment ?s2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s2 'segment)
               (enforce (direction ?d2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d2 'direction))
              (and
               (has-type ?a ?t)
               (is-moving ?a)
               (not (= ?s1 ?s2))
               (facing ?a ?d1)
               (can-move ?s1 ?s2 ?d1)
               (move-dir ?s1 ?s2 ?d2)
               (at-segment ?a ?s1)
               (forall (?s)
                (segment ?s)
                (imply
                 (and
                  (is-blocked ?s ?t ?s2 ?d2)
                  (not (= ?s ?s1)))
                 (not (occupied ?s))))))
            (pddl-action-precondition
             (process-action domain
                             '(:action move
                               :parameters
                               (?a - airplane ?t - airplanetype ?d1 - direction ?s1 ?s2  - segment ?d2 - direction)
                               :precondition
                               (and
                                (has-type ?a ?t)
                                (is-moving ?a)
                                (not (= ?s1 ?s2))
                                (facing ?a ?d1)
                                (can-move ?s1 ?s2 ?d1)
                                (move-dir ?s1 ?s2 ?d2)
                                (at-segment ?a ?s1)
                                (forall (?s - segment)
                                 (imply
                                  (and
                                   (is-blocked ?s ?t ?s2 ?d2)
                                   (not (= ?s ?s1)))
                                  (not (occupied ?s))))))))))

    (fiveam:is
     (equal '(and
              (and
               (enforce (airplane ?a) "Parameter ~a unbound or ill-typed. Should be ~a"  '?a 'airplane)
               (enforce (airplanetype ?t) "Parameter ~a unbound or ill-typed. Should be ~a"  '?t 'airplanetype)
               (enforce (direction ?d1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d1 'direction)
               (enforce (segment ?s1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s1 'segment)
               (enforce (segment ?s2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s2 'segment)
               (enforce (direction ?d2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d2 'direction))
              (forall (?s ?a2)
               (and
                (segment ?s)
                ( airplane ?a2))
               (imply
                (and
                 (is-blocked ?s ?t ?s2 ?d2)
                 (not (= ?s ?s1)))
                (not (at ?a2 ?s)))))
            (pddl-action-precondition
             (process-action domain
                             '(:action move
                               :parameters
                               (?a - airplane ?t - airplanetype ?d1 - direction ?s1 ?s2  - segment ?d2 - direction)
                               :precondition
                               (forall (?s - segment ?a2 - airplane)
                                (imply
                                 (and
                                  (is-blocked ?s ?t ?s2 ?d2)
                                  (not (= ?s ?s1)))
                                 (not (at ?a2 ?s)))))))))

    (fiveam:is
     (equal '(and
              (and
               (enforce (airplane ?a) "Parameter ~a unbound or ill-typed. Should be ~a"  '?a 'airplane)
               (enforce (airplanetype ?t) "Parameter ~a unbound or ill-typed. Should be ~a"  '?t 'airplanetype)
               (enforce (direction ?d1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d1 'direction)
               (enforce (segment ?s1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s1 'segment)
               (enforce (segment ?s2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?s2 'segment)
               (enforce (direction ?d2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?d2 'direction))
              (and
               (has-type ?a ?t)
               (is-moving ?a)
               (not (= ?s1 ?s2))
               (facing ?a ?d1)
               (can-move ?s1 ?s2 ?d1)
               (move-dir ?s1 ?s2 ?d2)
               (at-segment ?a ?s1)
               (not
                (exists (?a1)
                 (airplane ?a1)
                 (and  (not (= ?a1 ?a))
                  (blocked ?s2 ?a1))))
               (forall (?s)
                (segment ?s)
                (imply (and (is-blocked ?s ?t ?s2 ?d2)
                        (not (= ?s ?s1)))
                 (not (occupied ?s))))))
            (pddl-action-precondition
             (process-action domain
                             '(:action move
                               :parameters
                               (?a - airplane ?t - airplanetype ?d1 - direction ?s1 ?s2  - segment ?d2 - direction)
                               :precondition
                               (and
                                (has-type ?a ?t)
                                (is-moving ?a)
                                (not (= ?s1 ?s2))
                                (facing ?a ?d1)
                                (can-move ?s1 ?s2 ?d1)
                                (move-dir ?s1 ?s2 ?d2)
                                (at-segment ?a ?s1)
                                (not    (exists (?a1 - airplane)        (and    (not (= ?a1 ?a))
                                                                         (blocked ?s2 ?a1))))
                                (forall (?s - segment)  (imply  (and    (is-blocked ?s ?t ?s2 ?d2)
                                                                 (not (= ?s ?s1)))
                                                         (not (occupied ?s))
                                                         ))
                                )
                               :effect
                               (and
                                (occupied ?s2)
                                (blocked ?s2 ?a)
                                (not (occupied ?s1))
                                (when   (not (is-blocked ?s1 ?t ?s2 ?d2))
                                  (not (blocked ?s1 ?a)))
                                (when   (not (= ?d1 ?d2))
                                  (not (facing ?a ?d1)))
                                (not (at-segment ?a ?s1))
                                (forall (?s - segment)  (when   (is-blocked ?s ?t ?s2 ?d2)
                                                          (blocked ?s ?a)
                                                          ))
                                (forall (?s - segment)  (when   (and    (is-blocked ?s ?t ?s1 ?d1)
                                                                        (not (= ?s ?s2))
                                                                        (not (is-blocked ?s ?t ?s2 ?d2))
                                                                        )
                                                          (not (blocked ?s ?a))
                                                          ))
                                (at-segment ?a ?s2)
                                (when   (not (= ?d1 ?d2))
                                  (facing ?a ?d2))
                                )
                               )))))))

(fiveam:def-fixture simple-when-fixtures ()
  (progn
    (let ((*define-silently* t))
      (defdomain (simple-when-domain
                  :type pddl-domain
                  :redefine-ok t)
          ((:types loc)
           (:action walk
            :parameters (?from ?to - loc)
            :precondition (at robot ?from)
            :effect (and (not (at robot ?from))
                         (at robot ?to)
                         (when (carrying cargo)
                           (and (not (at cargo ?from))
                                (at cargo ?to))))))))
    (&body)))

(fiveam:test simple-when
  (fiveam:with-fixture simple-when-fixtures ()
    (fiveam:is
     (equal
      (sort
       (copy-list '((AT ROBOT NEW-YORK) (loc new-jersey) (loc new-york)))
       'prop-sorter)
      (sort
       (let ((*state-encoding* :list))
         (declare (special *state-encoding*))
         (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey) (loc new-jersey) (loc new-york)))))
           (apply-action *domain* state
                         '(!walk new-jersey new-york)
                         (operator *domain* '!walk)
                         nil 0 nil)
           (state-atoms state)))
       'prop-sorter)))

    (fiveam:is
     (equal
      '((AT CARGO NEW-YORK) (AT ROBOT NEW-YORK)
        (CARRYING CARGO) (loc new-jersey) (loc new-york))
      (let ((*state-encoding* :list))
        (declare (special *state-encoding*))
        (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                     (loc new-jersey) (loc new-york)
                                                                     (carrying cargo)))))

          (apply-action *domain* state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)
          (sort (state-atoms state)
                'prop-sorter)))))))

(fiveam:test simple-when-with-deps
  (fiveam:with-fixture simple-when-fixtures ()
    (let ((*state-encoding* :list) (*record-dependencies-p* t))
      (declare (special *state-encoding*))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey) (loc new-jersey) (loc new-york)))))
        (multiple-value-bind (op tag protections cost unifier deps)
            (apply-action *domain* state
                          '(!walk new-jersey new-york)
                          (operator *domain* '!walk)
                          nil 0 nil)
          (declare (ignore op tag protections cost unifier))
          (fiveam:is
           (equal
            (sort
             (copy-list '((AT ROBOT NEW-YORK) (loc new-jersey) (loc new-york)))
             'prop-sorter)
            (sort
             (state-atoms state)
             'prop-sorter)))
          (fiveam:is
           (equal
            (sort
             (copy-list '((at robot new-jersey) (loc new-jersey) (loc new-york)))
             'prop-sorter)
            (sort
             (mapcar #'shop3.theorem-prover::rd-prop deps)
             'prop-sorter))))))

    (let ((*state-encoding* :list)(*record-dependencies-p* t))
      (declare (special *state-encoding*))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                   (loc new-jersey) (loc new-york)
                                                                   (carrying cargo))))
            )
        (multiple-value-bind (op tag protections cost unifier deps)
            (apply-action *domain* state
                          '(!walk new-jersey new-york)
                          (operator *domain* '!walk)
                          nil 0 nil)
          (declare (ignore op tag protections cost unifier))
          (fiveam:is
           (equal
            '((AT CARGO NEW-YORK) (AT ROBOT NEW-YORK)
              (CARRYING CARGO) (loc new-jersey) (loc new-york))
            (sort (state-atoms state)
                  'prop-sorter)))
          (fiveam:is
           (equal
            (sort
             (copy-list '((at robot new-jersey) (carrying cargo) (loc new-jersey) (loc new-york)))
             'prop-sorter)
            (sort
             (mapcar #'shop3.theorem-prover::rd-prop deps)
             'prop-sorter))))))))

(fiveam:def-fixture quantified-when-fixtures ()
  (progn
    (let ((*define-silently* t))
      (defdomain (quantified-when-domain
                  :type pddl-domain
                  :redefine-ok t)
          ((:types loc luggage)
           (:action walk
            :parameters (?from ?to - loc)
            :precondition (at robot ?from)
            :effect (and (not (at robot ?from))
                         (at robot ?to)
                         (forall (?x - luggage)
                                 (when (carrying robot ?x)
                                   (and (not (at ?x ?from))
                                        (at ?x ?to)))))))))
    (&body)))

(fiveam:test quantified-when
  (fiveam:with-fixture quantified-when-fixtures ()
    (fiveam:is
     (equal
      '((at bag1 new-jersey)
        (at bag2 new-jersey)
        (at bag3 new-jersey)
        (at robot new-jersey)
        (luggage bag1)
        (luggage bag2)
        (luggage bag3))
      (let ((*state-encoding* :list))
        (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                     (at bag1 new-jersey)
                                                                     (at bag2 new-jersey)
                                                                     (at bag3 new-jersey)
                                                                     (luggage bag1)
                                                                     (luggage bag2)
                                                                     (luggage bag3)))))
          (sort (state-atoms state) 'prop-sorter)))))

    (fiveam:is
     (equal
      '((at bag1 new-jersey)
        (at bag2 new-jersey)
        (at bag3 new-jersey)
        (at robot new-york)
        (loc new-jersey)
        (loc new-york)
        (luggage bag1)
        (luggage bag2)
        (luggage bag3))
      (let ((*state-encoding* :list))
        (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                     (at bag1 new-jersey)
                                                                     (at bag2 new-jersey)
                                                                     (at bag3 new-jersey)
                                                                     (loc new-jersey) (loc new-york)
                                                                     (luggage bag1)
                                                                     (luggage bag2)
                                                                     (luggage bag3)))))
          (apply-action *domain* state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)
          (sort (state-atoms state) 'prop-sorter)))))


    (fiveam:is
     (equal
      '((at bag1 new-york)
        (at bag2 new-jersey)
        (at bag3 new-jersey)
        (at robot new-york)
        (carrying robot bag1)
        (loc new-jersey) (loc new-york)
        (luggage bag1)
        (luggage bag2)
        (luggage bag3))
      (let ((*state-encoding* :list))
        (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                     (at bag1 new-jersey)
                                                                     (at bag2 new-jersey)
                                                                     (at bag3 new-jersey)
                                                                     (carrying robot bag1)
                                                                     (loc new-jersey) (loc new-york)
                                                                     (luggage bag1)
                                                                     (luggage bag2)
                                                                     (luggage bag3)))))
          (apply-action *domain* state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)
          (sort (state-atoms state) 'prop-sorter)))))

    (fiveam:is
     (equal
      '((at bag1 new-york)
        (at bag2 new-jersey)
        (at bag3 new-york)
        (at robot new-york)
        (carrying robot bag1)
        (carrying robot bag3)
        (loc new-jersey) (loc new-york)
        (luggage bag1)
        (luggage bag2)
        (luggage bag3))
      (let ((*state-encoding* :list))
        (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                     (at bag1 new-jersey)
                                                                     (at bag2 new-jersey)
                                                                     (at bag3 new-jersey)
                                                                     (carrying robot bag1)
                                                                     (carrying robot bag3)
                                                                     (loc new-jersey) (loc new-york)
                                                                     (luggage bag1)
                                                                     (luggage bag2)
                                                                     (luggage bag3)))))
          (apply-action *domain* state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)
          (sort (state-atoms state) 'prop-sorter)))))))

(fiveam:test quantified-when-with-deps
  (fiveam:with-fixture quantified-when-fixtures ()
    (let ((*state-encoding* :list)
          (*record-dependencies-p* t))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                   (at bag1 new-jersey)
                                                                   (at bag2 new-jersey)
                                                                   (at bag3 new-jersey)
                                                                   (loc new-jersey) (loc new-york)
                                                                   (luggage bag1)
                                                                   (luggage bag2)
                                                                   (luggage bag3)))))
        (multiple-value-bind (op state-tag protections cost unifier deps)
            (apply-action *domain* state
                          '(!walk new-jersey new-york)
                          (operator *domain* '!walk)
                          nil 0 nil)
          (declare (ignore unifier state-tag op))
          (fiveam:is
           (equal
            '((at bag1 new-jersey)
              (at bag2 new-jersey)
              (at bag3 new-jersey)
              (at robot new-york)
              (loc new-jersey)
              (loc new-york)
              (luggage bag1)
              (luggage bag2)
              (luggage bag3))
            (sort (state-atoms state) 'prop-sorter)))
          (fiveam:is-false protections)
          (fiveam:is
           (eql 1.0 cost))
          (fiveam:is
           (equalp
            '((at robot new-jersey) (loc new-jersey) (loc new-york))
            (sort (mapcar #'shop3.theorem-prover::rd-prop deps) 'prop-sorter)))))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                                                   (at bag1 new-jersey)
                                                                   (at bag2 new-jersey)
                                                                   (at bag3 new-jersey)
                                                                   (loc new-jersey) (loc new-york)
                                                                   (carrying robot bag1)
                                                                   (luggage bag1)
                                                                   (luggage bag2)
                                                                   (luggage bag3)))))
        (multiple-value-bind (op state-tag protections cost unifier deps)
            (apply-action *domain* state
                          '(!walk new-jersey new-york)
                          (operator *domain* '!walk)
                          nil 0 nil)
          (declare (ignore unifier state-tag op))
          (fiveam:is
           (equal
            '((at bag1 new-york)
              (at bag2 new-jersey)
              (at bag3 new-jersey)
              (at robot new-york)
              (carrying robot bag1)
              (loc new-jersey)
              (loc new-york)
              (luggage bag1)
              (luggage bag2)
              (luggage bag3))
            (sort (state-atoms state) 'prop-sorter)))
          (fiveam:is-false protections)
          (fiveam:is
           (eql 1.0 cost))
          (fiveam:is
           (equalp
            '((at robot new-jersey) (carrying robot bag1) (loc new-jersey) (loc new-york))
            (sort (mapcar #'shop3.theorem-prover::rd-prop deps) 'prop-sorter))))))))

(in-package :shop3-openstacks)
(fiveam:def-suite* plan-openstacks :in shop3::pddl-tests)

(fiveam:test pddl-planning
  (let ((shop3:*define-silently* t))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.lisp"))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/p01-manual.lisp")))
  (fiveam:is (equalp '((!OPEN-NEW-STACK N0 N1) (!OPEN-NEW-STACK N1 N2)
                       (!OPEN-NEW-STACK N2 N3) (!OPEN-NEW-STACK N3 N4)
                       (!OPEN-NEW-STACK N4 N5) (!START-ORDER O5 N5 N4) (!MAKE-PRODUCT P5)
                       (!SHIP-ORDER O5 N4 N5) (!START-ORDER O4 N5 N4) (!MAKE-PRODUCT P4)
                       (!START-ORDER O3 N4 N3) (!MAKE-PRODUCT P3) (!SHIP-ORDER O3 N3 N4)
                       (!SHIP-ORDER O4 N4 N5) (!START-ORDER O2 N5 N4) (!MAKE-PRODUCT P1)
                       (!START-ORDER O1 N4 N3) (!MAKE-PRODUCT P2) (!SHIP-ORDER O1 N3 N4)
                       (!SHIP-ORDER O2 N4 N5))
                     (shorter-plan (first (find-plans
                                           'os-sequencedstrips-p5_1 :verbose 0)))))
  (fiveam:is (equalp '((!OPEN-NEW-STACK N0 N1) (!OPEN-NEW-STACK N1 N2)
                       (!OPEN-NEW-STACK N2 N3) (!OPEN-NEW-STACK N3 N4)
                       (!OPEN-NEW-STACK N4 N5) (!START-ORDER O5 N5 N4) (!MAKE-PRODUCT P5)
                       (!SHIP-ORDER O5 N4 N5) (!START-ORDER O4 N5 N4) (!MAKE-PRODUCT P4)
                       (!START-ORDER O3 N4 N3) (!MAKE-PRODUCT P3) (!SHIP-ORDER O4 N3 N4)
                       (!SHIP-ORDER O3 N4 N5) (!START-ORDER O2 N5 N4) (!MAKE-PRODUCT P1)
                       (!START-ORDER O1 N4 N3) (!MAKE-PRODUCT P2) (!SHIP-ORDER O2 N3 N4)
                       (!SHIP-ORDER O1 N4 N5))
                     (shorter-plan (first (find-plans
                                           'os-sequencedstrips-p5_1i :verbose 0))))))

(fiveam:def-suite ess-pddl-planning :in plan-openstacks)

(fiveam:test (ess-pddl-planning-normal :suite ess-pddl-planning)
  (let ((shop3:*define-silently* t)
        (plan '((!OPEN-NEW-STACK N0 N1) (!OPEN-NEW-STACK N1 N2)
                (!OPEN-NEW-STACK N2 N3) (!OPEN-NEW-STACK N3 N4)
                (!OPEN-NEW-STACK N4 N5) (!START-ORDER O5 N5 N4) (!MAKE-PRODUCT P5)
                (!SHIP-ORDER O5 N4 N5) (!START-ORDER O4 N5 N4) (!MAKE-PRODUCT P4)
                (!START-ORDER O3 N4 N3) (!MAKE-PRODUCT P3) (!SHIP-ORDER O3 N3 N4)
                (!SHIP-ORDER O4 N4 N5) (!START-ORDER O2 N5 N4) (!MAKE-PRODUCT P1)
                (!START-ORDER O1 N4 N3) (!MAKE-PRODUCT P2) (!SHIP-ORDER O1 N3 N4)
                (!SHIP-ORDER O2 N4 N5))))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.lisp"))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/p01-manual.lisp"))
    (let ((new-plan (shorter-plan (first (find-plans-stack
                                          'os-sequencedstrips-p5_1 :verbose 0)))))
      (fiveam:is-true (shop:validate-plan new-plan
                                          (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.pddl")
                                          (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/p01.pddl")))
      (fiveam:is (equalp plan new-plan)))))

(fiveam:test (ess-pddl-planning-included :suite ess-pddl-planning)
  (let ((shop3:*define-silently* t)
        (shuffled-plan '((!OPEN-NEW-STACK N0 N1) (!OPEN-NEW-STACK N1 N2)
                         (!OPEN-NEW-STACK N2 N3) (!OPEN-NEW-STACK N3 N4)
                         (!OPEN-NEW-STACK N4 N5) (!START-ORDER O5 N5 N4) (!MAKE-PRODUCT P5)
                         (!SHIP-ORDER O5 N4 N5) (!START-ORDER O4 N5 N4) (!MAKE-PRODUCT P4)
                         (!START-ORDER O3 N4 N3) (!MAKE-PRODUCT P3) (!SHIP-ORDER O4 N3 N4)
                         (!SHIP-ORDER O3 N4 N5) (!START-ORDER O2 N5 N4) (!MAKE-PRODUCT P1)
                         (!START-ORDER O1 N4 N3) (!MAKE-PRODUCT P2) (!SHIP-ORDER O2 N3 N4)
                         (!SHIP-ORDER O1 N4 N5))))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.lisp"))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/p01-manual.lisp"))
    (fiveam:is (equalp shuffled-plan
                       (shorter-plan (first (find-plans-stack
                                             'os-sequencedstrips-p5_1i :verbose 0)))))))



(defmacro openstacks-test-loop (plan-form)
  `(let ((shop3::*define-silently* t))
     (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.lisp"))
     (let ((domain-file (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.pddl")))
       (loop :for i from 1 :to 30
             :as probfilename = (format nil "p~2,'0d.pddl" i)
             :as problem-file = (asdf:system-relative-pathname "shop3" (format nil "examples/openstacks-adl/~a" probfilename))
             :as shop-problem-file =(merge-pathnames (make-pathname :type "lisp") problem-file)
             :as shop-problem = (progn (load shop-problem-file) shop3::*problem*)
             :as standard-plan = ,plan-form
             :do (fiveam:is-true (and (or standard-plan
                                          (warn "Failed to SHOP3 plan for problem ~a" (shop3:name shop-problem)))
                                      (validate-plan standard-plan domain-file problem-file)))))))

(fiveam:in-suite shop3::pddl-tests)

(fiveam:test test-openstacks-adl
  (openstacks-test-loop (first (find-plans shop-problem :verbose 0))))

(fiveam:test test-openstacks-adl-explicit-stack-search
  (openstacks-test-loop (first (find-plans-stack shop-problem :verbose 0))))

(fiveam:in-suite plan-openstacks)

(fiveam:test test-forall-dependencies
  (let ((shop3::*define-silently* t))
    (load (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/domain.lisp"))
    (make-problem 'test-quantified-precondition-dependencies 'openstacks-sequencedstrips-ADL-included
                  ;; from problem 1 and modified
                  '((NEXT-COUNT N0 N1) (NEXT-COUNT N1 N2) (NEXT-COUNT N2 N3)
                    (NEXT-COUNT N3 N4) (NEXT-COUNT N4 N5)
                    (STACKS-AVAIL N5)   ; (STACKS-AVAIL N0)
                    (WAITING O1) (INCLUDES O1 P2)
                    (WAITING O2) (INCLUDES O2 P1) (INCLUDES O2 P2)
                    (WAITING O3) (INCLUDES O3 P3) (WAITING O4) (INCLUDES O4 P3)
                    (INCLUDES O4 P4) (WAITING O5) (INCLUDES O5 P5) (= (TOTAL-COST) 0)
                    (COUNT N0) (COUNT N1) (COUNT N2) (COUNT N3) (COUNT N4) (COUNT N5)
                    (ORDER O1) (ORDER O2) (ORDER O3) (ORDER O4) (ORDER O5) (PRODUCT P1)
                    (PRODUCT P2) (PRODUCT P3) (PRODUCT P4) (PRODUCT P5)
                    (:GOAL
                     (AND (SHIPPED O1) (SHIPPED O2) (SHIPPED O3) (SHIPPED O4)
                      (SHIPPED O5))))
                  '(:ordered (!start-order o1 ?avail ?avail1) (!start-order o2 ?avail1 ?avail2) (!make-product p2)))
    (multiple-value-bind (plans plan-trees plan-tree-hashes)
        (shop:find-plans-stack 'test-quantified-precondition-dependencies :plan-tree t)
      (fiveam:is-true plans)
      (when plans
        (let ((plan (shop:remove-costs (first plans))))
          (fiveam:is
           (eql 7
                (length (plan-tree:tree-node-dependencies
                         (plan-tree:find-plan-step (third plan) (first plan-trees) (first plan-tree-hashes)))))))))))

(in-package :shop3)
(fiveam:in-suite short-pddl-tests)

(fiveam:test test-forall-bounds
  ;; try to make this side-effect free
  (let ((*domain* nil))
    (let ((*define-silently* t))
      (defdomain (rover-for-test :type pddl-domain)
          (
           (:requirements :typing)
           (:static
            can_traverse
            equipped_for_soil_analysis
            equipped_for_rock_analysis
            equipped_for_imaging
            supports
            visible
            visible_from
            store_of
            on_board)
           (:types rover waypoint store camera mode lander objective)
           (:predicates (at ?x - rover ?y - waypoint)
                        (at_lander ?x - lander ?y - waypoint)
                        (can_traverse ?r - rover ?x - waypoint ?y - waypoint)
                        (equipped_for_soil_analysis ?r - rover)
                        (equipped_for_rock_analysis ?r - rover)
                        (equipped_for_imaging ?r - rover)
                        (empty ?s - store)
                        (have_rock_analysis ?r - rover ?w - waypoint)
                        (have_soil_analysis ?r - rover ?w - waypoint)
                        (full ?s - store)
                        (calibrated ?c - camera ?r - rover)
                        (supports ?c - camera ?m - mode)
                        (available ?r - rover)
                        (visible ?w - waypoint ?p - waypoint)
                        (have_image ?r - rover ?o - objective ?m - mode)
                        (communicated_soil_data ?w - waypoint)
                        (communicated_rock_data ?w - waypoint)
                        (communicated_image_data ?o - objective ?m - mode)
                        (at_soil_sample ?w - waypoint)
                        (at_rock_sample ?w - waypoint)
                        (visible_from ?o - objective ?w - waypoint)
                        (store_of ?s - store ?r - rover)
                        (calibration_target ?i - camera ?o - objective)
                        (on_board ?i - camera ?r - rover)
                        (channel_free ?l - lander)
                        )
           ))
      (DEFPROBLEM test-rover-problem ROVER-for-test
        ((OBJECTIVE OBJECTIVE1) (OBJECTIVE OBJECTIVE0)
         (CAMERA CAMERA0) (WAYPOINT WAYPOINT3) (WAYPOINT WAYPOINT2)
         (WAYPOINT WAYPOINT1) (WAYPOINT WAYPOINT0)
         (STORE ROVER0STORE) (ROVER ROVER0) (MODE LOW_RES)
         (MODE HIGH_RES) (MODE COLOUR) (LANDER GENERAL)
         (VISIBLE WAYPOINT1 WAYPOINT0)
         (VISIBLE WAYPOINT0 WAYPOINT1)
         (VISIBLE WAYPOINT2 WAYPOINT0)
         (VISIBLE WAYPOINT0 WAYPOINT2)
         (VISIBLE WAYPOINT2 WAYPOINT1)
         (VISIBLE WAYPOINT1 WAYPOINT2)
         (VISIBLE WAYPOINT3 WAYPOINT0)
         (VISIBLE WAYPOINT0 WAYPOINT3)
         (VISIBLE WAYPOINT3 WAYPOINT1)
         (VISIBLE WAYPOINT1 WAYPOINT3)
         (VISIBLE WAYPOINT3 WAYPOINT2)
         (VISIBLE WAYPOINT2 WAYPOINT3) (AT_SOIL_SAMPLE WAYPOINT0)
         (AT_ROCK_SAMPLE WAYPOINT1) (AT_SOIL_SAMPLE WAYPOINT2)
         (AT_ROCK_SAMPLE WAYPOINT2) (AT_SOIL_SAMPLE WAYPOINT3)
         (AT_ROCK_SAMPLE WAYPOINT3) (AT_LANDER GENERAL WAYPOINT0)
         (CHANNEL_FREE GENERAL) (AT ROVER0 WAYPOINT3)
         (AVAILABLE ROVER0) (STORE_OF ROVER0STORE ROVER0)
         (EMPTY ROVER0STORE) (EQUIPPED_FOR_SOIL_ANALYSIS ROVER0)
         (EQUIPPED_FOR_ROCK_ANALYSIS ROVER0)
         (EQUIPPED_FOR_IMAGING ROVER0)
         (CAN_TRAVERSE ROVER0 WAYPOINT3 WAYPOINT0)
         (CAN_TRAVERSE ROVER0 WAYPOINT0 WAYPOINT3)
         (CAN_TRAVERSE ROVER0 WAYPOINT3 WAYPOINT1)
         (CAN_TRAVERSE ROVER0 WAYPOINT1 WAYPOINT3)
         (CAN_TRAVERSE ROVER0 WAYPOINT1 WAYPOINT2)
         (CAN_TRAVERSE ROVER0 WAYPOINT2 WAYPOINT1)
         (ON_BOARD CAMERA0 ROVER0)
         (CALIBRATION_TARGET CAMERA0 OBJECTIVE1)
         (SUPPORTS CAMERA0 COLOUR) (SUPPORTS CAMERA0 HIGH_RES)
         (VISIBLE_FROM OBJECTIVE0 WAYPOINT0)
         (VISIBLE_FROM OBJECTIVE0 WAYPOINT1)
         (VISIBLE_FROM OBJECTIVE0 WAYPOINT2)
         (VISIBLE_FROM OBJECTIVE0 WAYPOINT3)
         (VISIBLE_FROM OBJECTIVE1 WAYPOINT0)
         (VISIBLE_FROM OBJECTIVE1 WAYPOINT1)
         (VISIBLE_FROM OBJECTIVE1 WAYPOINT2)
         (VISIBLE_FROM OBJECTIVE1 WAYPOINT3)
         (COMMUNICATE_SOIL_DATA WAYPOINT2)
         (COMMUNICATE_ROCK_DATA WAYPOINT3)
         (COMMUNICATE_IMAGE_DATA OBJECTIVE1 HIGH_RES)
         )
        (:TASK ACHIEVE-GOALS))
      )
    (fiveam:is-false
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (make-initial-state *domain* :list (problem-state (find-problem 'test-rover-problem)))
            :return-dependencies nil :domain *domain*))
    (fiveam:is-false
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (problem-state (find-problem 'test-rover-problem))
            :return-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (make-initial-state *domain* :list (remove '(communicate_image_data objective1 high_res)
                                                       (problem-state (find-problem 'test-rover-problem))
                                                       :test 'equalp))
            :return-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (remove '(communicate_image_data objective1 high_res)
                                                      (problem-state (find-problem 'test-rover-problem))
                                                      :test 'equalp)
            :return-dependencies nil :domain *domain*))
    (fiveam:is-false
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (make-initial-state *domain* :list (problem-state (find-problem 'test-rover-problem)))
            :return-dependencies nil :domain *domain*))
    (fiveam:is-false
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (problem-state (find-problem 'test-rover-problem))
            :return-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (make-initial-state *domain* :list (remove '(communicate_image_data objective1 high_res)
                                                       (problem-state (find-problem 'test-rover-problem))
                                                       :test 'equalp))
            :return-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (remove '(communicate_image_data objective1 high_res)
                                                      (problem-state (find-problem 'test-rover-problem))
                                                      :test 'equalp)
            :return-dependencies nil :domain *domain*))))

(fiveam:test (pddl-domain-constant-defs :suite pddl-tests)
  (let* ((pddl-domain-file (asdf-utilities:system-relative-pathname "shop3" "examples/UMT2/from-archive/UMT.pddl"))
         (shop-domain (eval `(defdomain (test-umt-domain :type metric-pddl-domain)
                              ((:include um-translog-2 ,pddl-domain-file)))))
         (new-state (shop.common:make-initial-state shop-domain :mixed nil)))
    (fiveam:is (alexandria:set-equal
                '((vtype regularv) (vtype flatbed) (vtype tanker) (vtype hopper) (vtype auto) (vtype air)
                  (vptype truck) (vptype airplane) (vptype train)
                  (rtype road-route) (rtype rail-route) (rtype air-route)
                  (ptype regularp) (ptype bulky) (ptype liquid) (ptype granular) (ptype cars) (ptype mail)
                  (ltype airport) (ltype train-station))
                (state-atoms new-state)
                :test 'equalp))))


(fiveam:def-suite rovers-tests :in pddl-tests)
(fiveam:in-suite rovers-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun def-rover-test (n)
    `(fiveam:test ,(intern (format nil "ROVER-TEST~2,'0d" n) :shop3)
       (let ((*define-silently* t))
         (load (asdf:system-relative-pathname "shop3" "examples/rovers/strips/domain.lisp")))
       (let ((probfile (asdf:system-relative-pathname "shop3" ,(format nil "examples/rovers/strips/p~2,'0d.lisp" n))))
         (load probfile)
         (let ((plans (shop:find-plans shop::*problem* :verbose 0)))
           (fiveam:is-true plans)
           (let ((valid
                   (when plans
                     (shop3:validate-plan (first plans)
                                          (asdf:system-relative-pathname "shop3" "examples/rovers/strips/domain.pddl")
                                          (asdf:system-relative-pathname "shop3" (format nil "examples/rovers/strips/~a.pddl" (pathname-name probfile)))))))
             (fiveam:is-true valid)))))))


(defmacro pddl-problem-tests ()
  `(progn
     ,@(iter (for probnum from 1 to 20)
         (collecting
          (def-rover-test probnum)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pddl-problem-tests))

(fiveam:def-suite pddl-prover-tests :in pddl-tests)

(defclass test-metric-fluent-domain (pddl-domain fluents-mixin)
  ())

(in-package :shop-user)

(defparameter shop::mfd-functions
  '(amount capacity))

(defparameter shop::init-fluent-values
  '((shop::fluent-value (amount jug0) 6)
    (shop::fluent-value (capacity jug0) 12)))

(in-package :shop)

(fiveam:def-fixture mock-fluent-domain ()
  (let ((domain (make-instance 'test-metric-fluent-domain
                               :fluent-functions mfd-functions)))
    (&body)))

(fiveam:def-test test-fluent-value-retrieval (:suite pddl-prover-tests :fixture mock-fluent-domain)
  (let ((state (make-initial-state domain :mixed init-fluent-values)))
    (let ((answers
            (query '((fluent-value (shop-user::amount shop-user::jug0) ?amt)
                     (fluent-value (shop-user::capacity shop-user::jug0) ?cap))
                   state :domain domain)))
      (fiveam:is-true answers)
      (fiveam:is (= 1 (length answers)))
      (let ((bindings (first answers)))
        (fiveam:is (eql 6 (shop.unifier:binding-list-value '?amt bindings)))
        (fiveam:is (eql 12 (shop.unifier:binding-list-value '?cap bindings)))))))

(fiveam:def-test test-f-exp-retrieval (:suite pddl-prover-tests :fixture mock-fluent-domain)
  (let ((state (make-initial-state domain :mixed init-fluent-values)))
    (let ((answers
            (query '((f-exp-value (shop-user::amount shop-user::jug0) ?amt)
                     (f-exp-value (shop-user::capacity shop-user::jug0) ?cap))
                   state :domain domain)))
      (fiveam:is-true answers)
      (fiveam:is (= 1 (length answers)))
      (let ((bindings (first answers)))
        (fiveam:is (eql 6 (shop.unifier:binding-list-value '?amt bindings)))
        (fiveam:is (eql 12 (shop.unifier:binding-list-value '?cap bindings)))))
    (let ((answers
            (query '((f-exp-value (- (shop-user::amount shop-user::jug0)) ?amt)
                     (f-exp-value (- 12) ?cap))
                   state :domain domain)))
      (fiveam:is-true answers)
      (fiveam:is (= 1 (length answers)))
      (let ((bindings (first answers)))
        (fiveam:is (eql -6 (shop.unifier:binding-list-value '?amt bindings)))
        (fiveam:is (eql -12 (shop.unifier:binding-list-value '?cap bindings)))))

    (let ((answers
            (query '((f-exp-value (* (shop-user::amount shop-user::jug0) 2) ?amt1)
                     (f-exp-value (/ (shop-user::amount shop-user::jug0) 2) ?amt2)
                     (f-exp-value (+ (* (shop-user::amount shop-user::jug0) 2) 12) ?amt3)
                     (f-exp-value (+ (shop-user::amount shop-user::jug0) (shop-user::capacity shop-user::jug0)) ?amt4)
                     (f-exp-value (/ (shop-user::amount shop-user::jug0) 5) ?amt5))
                   state :domain domain)))
      (fiveam:is-true answers)
      (fiveam:is (= 1 (length answers)))
      (let ((bindings (first answers)))
        (fiveam:is (eql 12 (shop.unifier:binding-list-value '?amt1 bindings)))
        (fiveam:is (eql 3 (shop.unifier:binding-list-value '?amt2 bindings)))
        (fiveam:is (eql 24 (shop.unifier:binding-list-value '?amt3 bindings)))
        (fiveam:is (eql 18 (shop.unifier:binding-list-value '?amt4 bindings)))
        (fiveam:is (eql 6/5 (shop.unifier:binding-list-value '?amt5 bindings)))))))

(fiveam:def-suite pddl-fluents-tests :in pddl-tests)

(fiveam:def-test test-precond-rewriting (:suite pddl-fluents-tests :fixture mock-fluent-domain)
  (iter (for comp-op in +numerical-comparisons+)
    (as sample-expr-1 = `(,comp-op (shop-user::amount shop-user::jug0) 12))
    (as sample-expr-2 = `(,comp-op 12 (shop-user::capacity shop-user::jug0)))
    (as sample-expr-3 = `(,comp-op (shop-user::amount shop-user::jug0) (shop-user::capacity shop-user::jug0)))

    (as result-1 = `(fluent-check ,comp-op (shop-user::amount shop-user::jug0) 12))
    (as result-2 = `(fluent-check ,comp-op 12 (shop-user::capacity shop-user::jug0)))
    (as result-3 = `(fluent-check ,comp-op (shop-user::amount shop-user::jug0) (shop-user::capacity shop-user::jug0)))
    (fiveam:is (equalp result-1 (translate-fluent-precond domain sample-expr-1)))
    (fiveam:is (equalp result-2 (translate-fluent-precond domain sample-expr-2)))
    (fiveam:is (equalp result-3 (translate-fluent-precond domain sample-expr-3))))
  (let ((jug-precond-1 '(>= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
        (jug-precond-2 '(and (jug-in-hand) (>= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1))))
        (jug-precond-3 '(or (and (jug-in-hand) (>= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                         (and (jug-in-robot) (>= (shop-user::capacity ?jug2) (shop-user::amount ?jug2)))))
        (jug-precond-4 '(forall (?jug1 ?jug2)
                         (or (and (jug-in-hand) (>= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                          (and (jug-in-robot) (>= (shop-user::capacity ?jug2) (shop-user::amount ?jug2))))))
        (jug-precond-5 '(exists (?jug1 ?jug2)
                         (or (and (jug-in-hand) (>= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                          (and (jug-in-robot) (>= (shop-user::capacity ?jug2) (shop-user::amount ?jug2))))))
        )
    (fiveam:is (equalp
                '(shop::fluent-check >= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1))
                (translate-fluent-precond domain jug-precond-1)))
    (fiveam:is (equalp
                '(and (jug-in-hand) (shop::fluent-check >= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                (translate-fluent-precond domain jug-precond-2)))
    (fiveam:is (equalp
                '(or (and (jug-in-hand) (shop::fluent-check >= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                  (and (jug-in-robot) (shop::fluent-check >= (shop-user::capacity ?jug2) (shop-user::amount ?jug2))))
                (translate-fluent-precond domain jug-precond-3)))
    (fiveam:is (equalp
                '(forall (?jug1 ?jug2)
                  (or (and (jug-in-hand) (shop::fluent-check >= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                   (and (jug-in-robot) (shop::fluent-check >= (shop-user::capacity ?jug2) (shop-user::amount ?jug2)))))
                (translate-fluent-precond domain jug-precond-4)))
    (fiveam:is (equalp
                '(exists (?jug1 ?jug2)
                  (or (and (jug-in-hand) (shop::fluent-check >= (- (shop-user::capacity ?jug2) (shop-user::amount ?jug2)) (shop-user::amount ?jug1)))
                   (and (jug-in-robot) (shop::fluent-check >= (shop-user::capacity ?jug2) (shop-user::amount ?jug2)))))
                (translate-fluent-precond domain jug-precond-5)))))

(fiveam:def-test test-effect-rewriting (:suite pddl-fluents-tests :fixture mock-fluent-domain)
  (iter (for update-op in +fluent-updates+)
    (as sample-expr-1 = `(,update-op (shop-user::amount shop-user::jug0) 12))
    (as sample-expr-2 = `(,update-op (shop-user::amount shop-user::jug0) (shop-user::capacity shop-user::jug0)))

    (as result-1 = `(fluent-update ,update-op (shop-user::amount shop-user::jug0) 12))
    (as result-2 = `(fluent-update ,update-op (shop-user::amount shop-user::jug0) (shop-user::capacity shop-user::jug0)))
    (fiveam:is (equalp result-1 (translate-metric-updates domain sample-expr-1)))
    (fiveam:is (equalp result-2 (translate-metric-updates domain sample-expr-2))))
  ;; jug pouring example:
  (fiveam:is (equalp '(and (shop::fluent-update assign (shop-user::amount ?jug1) 0)
                       (shop::fluent-update assign (shop-user::amount ?jug2)
                        (+ (shop-user::amount ?jug1) (shop-user::amount ?jug2))))
                     (translate-metric-updates domain
                                               '(and (assign (shop-user::amount ?jug1) 0)
                                                 (assign (shop-user::amount ?jug2)
                                                  (+ (shop-user::amount ?jug1) (shop-user::amount ?jug2)))))))
  (fiveam:is (equalp '(forall (?jug1 ?jug2) (and (shop::fluent-update assign (shop-user::amount ?jug1) 0)
                                             (shop::fluent-update assign (shop-user::amount ?jug2)
                                              (+ (shop-user::amount ?jug1) (shop-user::amount ?jug2)))))
                     (translate-metric-updates domain
                                               '(forall (?jug1 ?jug2)
                                                 (and (assign (shop-user::amount ?jug1) 0)
                                                  (assign (shop-user::amount ?jug2)
                                                   (+ (shop-user::amount ?jug1) (shop-user::amount ?jug2))))))))
  (fiveam:is (equalp '(forall (?jug1 ?jug2)
                       (when (and (shop::fluent-check > (shop-user::amount ?jug2) 0)
                                  (robotic-jug ?jug2))
                         (and (shop::fluent-update assign (shop-user::amount ?jug1) 0)
                              (shop::fluent-update assign (shop-user::amount ?jug2)
                                                   (+ (shop-user::amount ?jug1) (shop-user::amount ?jug2))))))
                     (translate-metric-updates domain
                                               '(forall (?jug1 ?jug2)
                                                 (when (and (> (shop-user::amount ?jug2) 0)
                                                            (robotic-jug ?jug2))
                                                   (and (assign (shop-user::amount ?jug1) 0)
                                                        (assign (shop-user::amount ?jug2)
                                                                (+ (shop-user::amount ?jug1) (shop-user::amount ?jug2))))))))))

(defclass metric-fluents-domain (pddl-domain fluents-mixin)
  ())

(fiveam:def-fixture jug-pouring-domain ()
  (progn
    (let ((*define-silently* t)
          #+allegro (excl:*redefinition-warnings* nil))
      (defdomain (shop-jug-pouring :type metric-fluents-domain)
          ((:include jug-pouring #. (asdf:system-relative-pathname "shop3" "tests/jug-pouring.pddl"))) ))
    (let ((domain (find-domain 'shop-jug-pouring)))
      (&body))))

(fiveam:def-fixture successful-jug-pouring-problem ()
  (let ((problem
          (let ((*define-silently* t)
                #+allegro (excl:*redefinition-warnings* nil))
            (make-problem 'successful-jug-pouring
                          '((jug jug0)
                            (jug jug1)
                            (= (capacity jug0) 12)
                            (= (capacity jug1) 12)
                            (= (amount jug0) 6)
                            (= (amount jug1) 6))
                          '(!empty jug0 jug1)
                          ))))
    (&body)))

(fiveam:test test-pouring-jug
  (fiveam:with-fixture jug-pouring-domain ()
    (fiveam:with-fixture successful-jug-pouring-problem ()
      (let ((action (operator domain '!empty))
            (state (make-initial-state domain :mixed (problem-state problem))))
        (fiveam:is (equalp '(pddl-action (!empty ?jug1 ?jug2)
                             (and
                              (and (enforce (jug ?jug1) "Parameter ~a unbound or ill-typed. Should be ~a"  '?jug1 'jug)
                               (enforce (jug ?jug2) "Parameter ~a unbound or ill-typed. Should be ~a"  '?jug2 'jug))
                              (fluent-check >= (- (capacity ?jug2) (amount ?jug2))
                               (amount ?jug1)))
                             (and (fluent-update assign (amount ?jug1) 0)
                              (fluent-update assign (amount ?jug2)
                               (+ (amount ?jug1) (amount ?jug2))))
                             1.0)
                           action))
        (fiveam:is (alexandria:set-equal
                    '((JUG JUG0) (JUG JUG1) (FLUENT-VALUE (AMOUNT JUG1) 6) (FLUENT-VALUE (AMOUNT JUG0) 6)
                      (FLUENT-VALUE (CAPACITY JUG1) 12) (FLUENT-VALUE (CAPACITY JUG0) 12))
                    (state-atoms state) :test 'equalp))
        (let ((bound-op
                (apply-action domain state '(!empty jug0 jug1) action nil 0 nil)))
          (fiveam:is (equalp '(!empty jug0 jug1) bound-op))
          (fiveam:is (alexandria:set-equal '((JUG JUG0) (JUG JUG1) (FLUENT-VALUE (AMOUNT JUG1) 12) (FLUENT-VALUE (AMOUNT JUG0) 0)
                                             (FLUENT-VALUE (CAPACITY JUG1) 12) (FLUENT-VALUE (CAPACITY JUG0) 12))
                                           (state-atoms state) :test 'equalp)))
        (let ((trivial-plans (find-plans problem :domain domain :verbose 0)))
          (fiveam:is-true trivial-plans)
          (fiveam:is (equalp '((!empty jug0 jug1) 1.0)
                             (first trivial-plans))))
        (let ((trivial-plans (find-plans-stack problem :domain domain :verbose 0)))
          (fiveam:is-true trivial-plans)
          (fiveam:is (equalp '((!empty jug0 jug1) 1.0)
                             (first trivial-plans))))))))

(fiveam:def-suite* pddl-method-name-checks :in pddl-tests)

(defpackage :shop3-rovers-test
  (:use common-lisp shop3)
  (:import-from #:fiveam #:test #:def-fixture #:is-true #:is-false #:with-fixture #:signals #:warns)
  (:import-from #:shop3 #:pddl-method-name-checks #:non-unique-method-name-error #:non-unique-method-name-warning)
  (:intern
   #:communicated_image_data
   #:communicated_rock_data
   #:communicated_soil_data

   ;; rewrite for goals
   #:communicate_image_data
   #:communicate_rock_data
   #:communicate_soil_data))
(in-package :shop3-rovers-test)

(defclass pure-pddl-domain (pure-logic-domain-mixin pddl-domain)
  ())

;;; this domain has been modified so that all the method names are unique
(def-fixture new-rovers-domain (unique-method-names)
  (progn
    (let ((shop3::*define-silently* t))
     (eval
      `(defdomain (new-rovers-domain :type pure-pddl-domain :unique-method-names ,unique-method-names)
           (
            (:requirements :typing)
            (:static
             can_traverse
             equipped_for_soil_analysis
             equipped_for_rock_analysis
             equipped_for_imaging
             supports
             visible
             visible_from
             store_of
             on_board)
            (:types rover waypoint store camera mode lander objective)
            (:predicates (at ?x - rover ?y - waypoint)
                         (at_lander ?x - lander ?y - waypoint)
                         (can_traverse ?r - rover ?x - waypoint ?y - waypoint)
                         (equipped_for_soil_analysis ?r - rover)
                         (equipped_for_rock_analysis ?r - rover)
                         (equipped_for_imaging ?r - rover)
                         (empty ?s - store)
                         (have_rock_analysis ?r - rover ?w - waypoint)
                         (have_soil_analysis ?r - rover ?w - waypoint)
                         (full ?s - store)
                         (calibrated ?c - camera ?r - rover)
                         (supports ?c - camera ?m - mode)
                         (available ?r - rover)
                         (visible ?w - waypoint ?p - waypoint)
                         (have_image ?r - rover ?o - objective ?m - mode)
                         (communicated_soil_data ?w - waypoint)
                         (communicated_rock_data ?w - waypoint)
                         (communicated_image_data ?o - objective ?m - mode)
                         (at_soil_sample ?w - waypoint)
                         (at_rock_sample ?w - waypoint)
                         (visible_from ?o - objective ?w - waypoint)
                         (store_of ?s - store ?r - rover)
                         (calibration_target ?i - camera ?o - objective)
                         (on_board ?i - camera ?r - rover)
                         (channel_free ?l - lander)
                         )

            (:action navigate
             :parameters (?x - rover ?y - waypoint ?z - waypoint)
             :precondition (and (can_traverse ?x ?y ?z) (available ?x) (at ?x ?y)
                                (visible ?y ?z)
                                )
             :effect (and (not (at ?x ?y)) (at ?x ?z)
                          )
             )

            (:action sample_soil
             :parameters (?x - rover ?s - store ?p - waypoint)
             :precondition (and (at ?x ?p) (at_soil_sample ?p)
                                (equipped_for_soil_analysis ?x) (store_of ?s ?x) (empty ?s)
                                )
             :effect (and (not (empty ?s)) (full ?s) (have_soil_analysis ?x ?p) (not (at_soil_sample ?p))
                          )
             )

            (:action sample_rock
             :parameters (?x - rover ?s - store ?p - waypoint)
             :precondition (and (at ?x ?p) (at_rock_sample ?p) (equipped_for_rock_analysis ?x) (store_of ?s ?x)(empty ?s)
                                )
             :effect (and (not (empty ?s)) (full ?s) (have_rock_analysis ?x ?p) (not (at_rock_sample ?p))
                          )
             )

            (:action drop
             :parameters (?x - rover ?y - store)
             :precondition (and (store_of ?y ?x) (full ?y)
                                )
             :effect (and (not (full ?y)) (empty ?y)
                          )
             )

            (:action calibrate
             :parameters (?r - rover ?i - camera ?t - objective ?w - waypoint)
             :precondition (and (equipped_for_imaging ?r) (calibration_target ?i ?t) (at ?r ?w) (visible_from ?t ?w)(on_board ?i ?r)
                                )
             :effect (calibrated ?i ?r)
             )

            (:action take_image
             :parameters (?r - rover ?p - waypoint ?o - objective ?i - camera ?m - mode)
             :precondition (and (calibrated ?i ?r)
                                (on_board ?i ?r)
                                (equipped_for_imaging ?r)
                                (supports ?i ?m)
                                (visible_from ?o ?p)
                                (at ?r ?p)
                                )
             :effect (and (have_image ?r ?o ?m)(not (calibrated ?i ?r))
                          )
             )

            (:action communicate_soil_data
             :parameters (?r - rover ?l - lander
                             ;; the location from which ?r took the soil data
                             ?p - waypoint
                             ;; the location of the rover
                             ?x - waypoint
                             ;; the location of the lander
                             ?y - waypoint)
             :precondition (and (at ?r ?x)
                                (at_lander ?l ?y)
                                (have_soil_analysis ?r ?p)
                                (visible ?x ?y)
                                (available ?r)
                                (channel_free ?l)
                                )
             :effect (and (not (available ?r))
                          (not (channel_free ?l))
                          (channel_free ?l)
                          (communicated_soil_data ?p)
                          (available ?r)
                          )
             )

            (:action communicate_rock_data
             :parameters (?r - rover ?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
             :precondition (and (at ?r ?x)(at_lander ?l ?y)(have_rock_analysis ?r ?p)
                                (visible ?x ?y)(available ?r)(channel_free ?l)
                                )
             :effect (and (not (available ?r))(not (channel_free ?l))(channel_free ?l)(communicated_rock_data ?p)(available ?r)
                          )
             )

            (:action communicate_image_data
             :parameters (?r - rover ?l - lander ?o - objective ?m - mode
                             ;; rover position
                             ?x - waypoint
                             ;; lander location
                             ?y - waypoint)
             :precondition (and (at ?r ?x)(at_lander ?l ?y)(have_image ?r ?o ?m)(visible ?x ?y)(available ?r)(channel_free ?l))
             :effect (and (not (available ?r))
                          (not (channel_free ?l))
                          (channel_free ?l)
                          (communicated_image_data ?o ?m)
                          (available ?r)
                          )
             )

            ;; three imperatives that are used as in-memory representation of goals:
            ;; (COMMUNICATE_SOIL_DATA ?GOAL-LOC)
            ;; (COMMUNICATE_ROCK_DATA ?GOAL-LOC)
            ;; (COMMUNICATE_IMAGE_DATA ?OBJ ?MODE)

            ;; TOP LEVEL TASK:
            (:pddl-method (achieve-goals)
                          communicate-one-soil-data
                          (communicate_soil_data ?goal-loc)
                          (:ordered
                           (communicated_soil_data ?goal-loc ?_rover)
                           (achieve-goals)))

            (:pddl-method (achieve-goals)
                          communicate-one-rock-data
                          (communicate_rock_data ?goal-loc)
                          (:ordered
                           (communicated_rock_data ?goal-loc ?_rover)
                           (achieve-goals)))

            (:method (achieve-goals)
              communicate-one-image-data
              (communicate_image_data ?obj ?mode)
              (:ordered
               (communicated_image_data ?obj ?mode ?_rover)
               (achieve-goals)))

            (:pddl-method (achieve-goals)
                          check-for-all-goals-done
                          (and (forall (?goal-loc - waypoint) (not (communicate_soil_data ?goal-loc)))
                               (forall (?goal-loc - waypoint)(not (communicate_rock_data ?goal-loc)))
                               (forall (?obj - objective)
                                       (forall (?m - mode)
                                               (not (communicate_image_data ?obj ?m)))))
                          ())

            (:method (empty-store ?s ?_rover)
              already-empty
              ((empty ?s))
              ())

            (:method (empty-store ?s ?rover)
              drop-to-empty
              ((not (empty ?s)))
              ((!drop ?rover ?s)))

            (:method (navigate ?rover ?to)
              already-there
              ((at ?rover ?to))
              ())

            (:method (navigate ?rover ?to)
              go-there
              ((not (at ?rover ?to))
               (at ?rover ?from)
               (assign ?visited nil)
               (path ?rover ?from ?to ?path ?visited))
              ((move ?rover ?from ?path)))

            ;; this just traverses over the computed PATH
            (:method (move ?_rover ?_from nil)
              end-of-path
              ()
              ())

            (:method (move ?rover ?from (?first . ?rest))
              recursive-move
              ()
              ((!navigate ?rover ?from ?first)
               (move ?rover ?first ?rest)))

            (:method (communicated_soil_data ?goal-loc ?rover)
              achieve-communicated-soil-data
              ((store_of ?s ?rover))
              ((navigate ?rover ?goal-loc)
               (:immediate empty-store ?s ?rover)
               (:immediate !sample_soil ?rover ?s ?goal-loc)
               ;; FIXME: shouldn't there be a protection of the store until the communication is done?
               (:immediate communicate soil ?goal-loc ?_rover-loc ?rover)
               (:immediate !!retract ((COMMUNICATE_SOIL_DATA ?goal-loc)))))

            (:method (communicated_rock_data ?goal-loc ?rover)
              achieve-communicated-rock-data
              ((store_of ?s ?rover))
              ((navigate ?rover ?goal-loc)
               (:immediate empty-store ?s ?rover)
               (:immediate !sample_rock ?rover ?s ?goal-loc)
               (:immediate communicate ROCK ?goal-loc ?_rover-loc ?rover)
               (:immediate !!retract ((COMMUNICATE_ROCK_DATA ?goal-loc)))))

            (:method (communicated_image_data ?obj ?mode ?rover)
              achieve-communicated-image-data
              ((on_board ?camera ?rover)
               (supports ?camera ?mode)
               (at_lander ?_lander ?lander-loc))
              ((calibrate-camera ?rover ?camera)
               (get-line-of-sight ?rover ?obj ?photo-loc)
               (!take_image ?rover ?photo-loc ?obj ?camera ?mode)
               ;; navigate to a transmission location and transmit
               (communicate-image ?photo-loc ?lander-loc ?rover ?obj ?mode)
               (:immediate !!retract ((COMMUNICATE_IMAGE_DATA ?obj ?mode)))))

            (:method (calibrate-camera ?rover ?camera)
              camera-already-calibrated
              ((calibrated ?camera ?rover))
              ())

            (:method (calibrate-camera ?rover ?camera)
              calibrate-the-camera
              ((not (calibrated ?camera ?rover))
               (calibration_target ?camera ?calibration-obj)
               (visible_from ?calibration-obj ?calibration-loc))
              (:ordered (navigate ?rover ?calibration-loc)
                        (!calibrate ?rover ?camera ?calibration-obj ?calibration-loc)))

            (:method (get-line-of-sight ?rover ?obj ?photo-loc)
              have-line-of-sight-for-photo
              ((at ?rover ?photo-loc)
               (visible_from ?obj ?photo-loc))
              ())

            (:method (get-line-of-sight ?rover ?obj ?photo-loc)
              need-line-of-sight
              ((at ?rover ?rover-loc)
               (not (visible_from ?obj ?rover-loc))
               (visible_from ?obj ?photo-loc))
              (:ordered (navigate ?rover ?photo-loc)))


            ;; HELPERS
            ;; the following shows a need for some higher-order method constructs

            (:method (communicate soil ?analysis-loc ?rover-loc ?rover)
              have-line-of-sight-for-soil
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (visible ?rover-loc ?lander-loc))
              ((!communicate_soil_data ?rover ?l ?analysis-loc ?rover-loc
                                       ?lander-loc)))


            (:method (communicate soil ?analysis-loc ?rover-loc ?rover)
              go-to-line-of-sight-for-soil
              ;; Otherwise, go somewhere where the lander is visible
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (not (visible ?rover-loc ?lander-loc))
               ;; FIXME: should pick a *good* location, instead of any location that has vi
               (visible ?new-loc ?lander-loc))
              ((navigate ?rover ?new-loc)
               (!communicate_soil_data ?rover ?l ?analysis-loc ?new-loc
                                       ?lander-loc)))

            (:method (communicate rock ?analysis-loc ?rover-loc ?rover)
              have-line-of-sight-for-rock
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (visible ?rover-loc ?lander-loc))
              ((!communicate_rock_data ?rover ?l ?analysis-loc ?rover-loc
                                       ?lander-loc)))

            (:method (communicate rock ?analysis-loc ?rover-loc ?rover)
              go-to-line-of-sight-for-rock
              ;; Otherwise, go somewhere where the lander is visible
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (not (visible ?rover-loc ?lander-loc))
               ;; FIXME: should pick a *good* location, instead of any location that has vi
               (visible ?new-loc ?lander-loc))
              ((navigate ?rover ?new-loc)
               (!communicate_rock_data ?rover ?l ?analysis-loc ?new-loc
                                       ?lander-loc)))

            (:method (communicate image ?analysis-loc ?rover-loc ?rover)
              have-line-of-sight-for-image
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (visible ?rover-loc ?lander-loc))
              ((!communicate_image_data ?rover ?l ?analysis-loc ?rover-loc
                                        ?lander-loc)))

            (:method (communicate image ?analysis-loc ?rover-loc ?rover)
              go-to-line-of-sight-for-image
              ;; Otherwise, go somewhere where the lander is visible
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (not (visible ?rover-loc ?lander-loc))
               ;; FIXME: should pick a *good* location, instead of any location that has vi
               (visible ?new-loc ?lander-loc))
              ((navigate ?rover ?new-loc)
               (!communicate_image_data ?rover ?l ?analysis-loc ?new-loc
                                        ?lander-loc)))

            ;; end of helpers



            (:method (communicate-IMAGE ?rover-loc ?lander-loc
                      ?rover ?obj ?mode)
              communicate-image
              ((at ?rover ?rover-loc)
               (at_lander ?l ?lander-loc)
               (visible ?rover-loc ?lander-loc))
              ((!communicate_image_data ?rover ?l ?obj ?mode ?rover-loc
                                        ?lander-loc)))

            (:method (communicate-IMAGE ?rover-loc ?lander-loc
                      ?rover ?obj ?mode)
              relocate-then-communicate-image
              ((at ?rover ?loc)
               (at_lander ?l ?lander-loc)
               (not (visible ?rover-loc ?lander-loc))
               (visible ?new-loc ?lander-loc)
               (different ?loc ?new-loc))

              ((navigate ?rover ?new-loc)
               (!communicate_image_data ?rover ?l ?obj ?mode ?new-loc
                                        ?lander-loc)))

            (:op (!!retract ?g)
             :delete ?g)

            ;; State axioms
            (:- (same ?x ?x) nil)
            (:- (different ?x ?y) ((not (same ?x ?y))))


            ;; This is a simple implementation that looks for an existence of a
            ;; path, not necessarily a shortest or best path.
            (:- (path ?_rover ?from ?from nil ?_visited)
                nil)

            (:- (path ?rover ?from ?to (?to . nil) ?_visited)
                ((not (same ?from ?to))
                 (can_traverse ?rover ?from ?to)))

            (:- (path ?rover ?from ?to (?to1 . ?path1) ?visited)
                ((not (same ?from ?to))
                 (not (can_traverse ?rover ?from ?to))
                 (can_traverse ?rover ?from ?to1)
                 (not (eval (member '?to1 '?visited)))
                 (path ?rover ?to1 ?to ?path1 (?from . ?visited))))
            ))))
    (&body)))

;;; this domain has duplicated method names
(def-fixture openstacks-domain (unique-method-names)
  (progn
    (let ((shop3::*define-silently* t))
      (eval
       `(defdomain (openstacks-sequencedstrips-ADL
                    :unique-method-names ,unique-method-names
                    :type pddl-domain
                    :source-pddl-domain
                    #.(merge-pathnames "domain-nocosts.pddl" (or *compile-file-truename* *load-truename*
                                                                 (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/"))))
            (
             (:requirements :typing :adl :action-costs)
             (:types order product count)
             (:predicates (includes ?o - order ?p - product)
                          (waiting ?o - order)
                          (started ?o - order)
                          (shipped ?o - order)
                          (made ?p - product)
                          (stacks-avail ?s - count)
                          (next-count ?s ?ns - count))

             ;;     (:functions (total-cost) - number)

             (:action make-product
              :parameters (?p - product)
              :precondition (and (not (made ?p))
                                 (forall (?o - order)
                                         (imply (includes ?o ?p)
                                                (started ?o))))
              :effect (made ?p))

             (:action start-order
              :parameters (?o - order ?avail ?new-avail - count)
              :precondition (and (waiting ?o)
                                 (stacks-avail ?avail)
                                 (next-count ?new-avail ?avail))
              :effect (and (not (waiting ?o))
                           (started ?o)
                           (not (stacks-avail ?avail))
                           (stacks-avail ?new-avail))
              )

             (:action ship-order
              :parameters (?o - order ?avail ?new-avail - count)
              :precondition (and (started ?o)
                                 (forall (?p - product)
                                         (imply (includes ?o ?p) (made ?p)))
                                 (stacks-avail ?avail)
                                 (next-count ?avail ?new-avail))
              :effect (and (not (started ?o))
                           (shipped ?o)
                           (not (stacks-avail ?avail))
                           (stacks-avail ?new-avail))
              )

             (:action open-new-stack
              :parameters (?open ?new-open - count)
              :precondition (and (stacks-avail ?open)
                                 (next-count ?open ?new-open))
              :effect (and (not (stacks-avail ?open))
                           (stacks-avail ?new-open)
                           ;; (increase (total-cost) 1)
                           )
              )

             ;;This action should only be used during replanning to reset order status
             ;;  (otherwise, the stack system will be offset and break the state)
             (:action reset
              :parameters (?o - order)
              :precondition (and (started ?o) (not (shipped ?o)) (not (waiting ?o)))
              :effect (and (waiting ?o) (not (started ?o)))
              )

             (:method (assert-goals nil)
               ()
               ())

             (:method (assert-goals (?goal . ?goals))
               ()
               (:ordered (!!assert (goal ?goal))
                         (assert-goals ?goals))
               )

             (:method (plan)
               ((:goal (and . ?goals)))
               ((:ordered (assert-goals ?goals)
                          (open-all-stacks)
                          (plan-for-goals))))

             (:method (open-all-stacks)
               open-one-stack
               ((stacks-avail ?n)
                (next-count ?n ?n1))
               (:ordered (!open-new-stack ?n ?n1)
                         (open-all-stacks))
               done
               ()
               ()
               )

             (:method (plan-for-goals)
               ((goal (shipped ?order))
                (not (shipped ?order)))
               (:ordered (one-step) (plan-for-goals))
               ()
               ((verify-orders)))

             (:method (one-step)
               ;; prefer to ship an order, if possible...
               ((goal (shipped ?o))
                (not (shipped ?o))
                (forall (?p) (includes ?o ?p) (made ?p)))
               ((ship-products ?o))
               (:sort-by ?h
                         (and (goal (shipped ?o))
                              (not (shipped ?o))
                              (includes ?o ?p)
                              (not (made ?p))
                              (ship-cost-heuristic ?p ?h)))
               ((make-product ?p))
               done
               ()
               ()
               )

             (:method (make-product ?p)
               ()
               (:ordered (start-orders ?p)
                         (!make-product ?p)))

             (:method (start-orders ?p)
               ((includes ?o ?p)
                (not (started ?o)))
               ((start-an-order ?o)
                (start-orders ?p))
               done
               ()
               ())

             (:method (verify-orders)
               ((goal (shipped ?order))
                (not (shipped ?order)))
               (:eval (error "complete plan does not satisfy goals.  State is:" shop2:*current-state*))
               ()
               ())

             (:method (start-an-order ?order)
               ((stacks-avail ?next)
                (next-count ?count ?next))
               ((!start-order ?order ?next ?count)))

             (:method (ship-products ?order)
               ((stacks-avail ?count)
                (next-count ?count ?next))
               ((!ship-order ?order ?count ?next))
               )

             (:op (!!assert ?fact)
              :add (?fact))

             (:op (!!delete ?fact)
              :delete (?fact))

             (:- (ship-cost-heuristic ?p ?h)
                 ((setof ?o (and (includes ?o ?p) (not (started ?o))) ?os)
                  (order-costs ?os ?h 0))
                 )

             (:- (order-costs ?os ?h ?hin)
                 ((= ?os (?o . ?os1))
                  (order-cost ?o ?h1)
                  (assign ?h2 (+ ?h1 ?hin))
                  (order-costs ?os1 ?h ?h2))
                 ((= ?os nil)
                  (= ?h ?hin)))

             (:- (order-cost ?o ?h)
                 ((started ?o)
                  (product-cost ?o ?pc)
                  (assign ?h (1+ ?pc)))
                 ((not (started ?o))
                  (product-cost ?o ?h)))

             (:- (product-cost ?o ?c)
                 ((setof ?p
                         (and (includes ?o ?p)
                              (not (made ?p)))
                         ?ps)
                  (assign ?c (length '?ps))))
             )
          )))
    (unwind-protect
         (&body)
      (ignore-errors
       (shop::delete-domain 'openstacks-sequencedstrips-ADL)))))


(test (ok-method-names :suite pddl-method-name-checks)
  (with-fixture new-rovers-domain (t)
    (is-true
     (find-domain 'new-rovers-domain))
    (shop::delete-domain 'new-rovers-domain))
  (with-fixture new-rovers-domain (:warn)
    (is-true
     (find-domain 'new-rovers-domain))
    (shop::delete-domain 'new-rovers-domain))
  (with-fixture new-rovers-domain (nil)
    (is-true
     (find-domain 'new-rovers-domain))
    (shop::delete-domain 'new-rovers-domain)))


(test (bad-method-names :suite pddl-method-name-checks)
  (signals non-unique-method-name-error
    (with-fixture openstacks-domain (t)
      nil))
  (is-false (find-domain 'openstacks-sequencedstrips-ADL nil))
  (warns non-unique-method-name-warning
   (with-fixture openstacks-domain (:warn)
     (is-true
      (find-domain 'openstacks-sequencedstrips-ADL))))
  (handler-bind ((non-unique-method-name-warning
                   #'(lambda (e)
                       (declare (ignorable e))
                       (fiveam:fail "Inappropriately raised a non-unique-method name warning."))))
   (with-fixture openstacks-domain (nil)
     (is-true
      (find-domain 'openstacks-sequencedstrips-ADL)))))
