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

;;; Smart Information Flow Technologies Copyright 2006-2007
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


(fiveam:test ess-pddl-planning
  (let ((shop3:*define-silently* t)
        (plan '((!OPEN-NEW-STACK N0 N1) (!OPEN-NEW-STACK N1 N2)
                (!OPEN-NEW-STACK N2 N3) (!OPEN-NEW-STACK N3 N4)
                (!OPEN-NEW-STACK N4 N5) (!START-ORDER O5 N5 N4) (!MAKE-PRODUCT P5)
                (!SHIP-ORDER O5 N4 N5) (!START-ORDER O4 N5 N4) (!MAKE-PRODUCT P4)
                (!START-ORDER O3 N4 N3) (!MAKE-PRODUCT P3) (!SHIP-ORDER O3 N3 N4)
                (!SHIP-ORDER O4 N4 N5) (!START-ORDER O2 N5 N4) (!MAKE-PRODUCT P1)
                (!START-ORDER O1 N4 N3) (!MAKE-PRODUCT P2) (!SHIP-ORDER O1 N3 N4)
                (!SHIP-ORDER O2 N4 N5)))
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
    (fiveam:is (equalp plan (shorter-plan (first (find-plans-stack 
                                                  'os-sequencedstrips-p5_1 :verbose 0)))))
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
            :record-dependencies nil :domain *domain*))
    (fiveam:is-false 
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (problem-state (find-problem 'test-rover-problem))
            :record-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (make-initial-state *domain* :list (remove '(communicate_image_data objective1 high_res)
                                                       (problem-state (find-problem 'test-rover-problem))
                                                       :test 'equalp))
            :record-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective)
                                       (forall (?m - mode)
                                        (not (communicate_image_data ?obj ?m)))))
            (remove '(communicate_image_data objective1 high_res)
                                                      (problem-state (find-problem 'test-rover-problem))
                                                      :test 'equalp)
            :record-dependencies nil :domain *domain*))
    (fiveam:is-false 
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (make-initial-state *domain* :list (problem-state (find-problem 'test-rover-problem)))
            :record-dependencies nil :domain *domain*))
    (fiveam:is-false 
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (problem-state (find-problem 'test-rover-problem))
            :record-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (make-initial-state *domain* :list (remove '(communicate_image_data objective1 high_res)
                                                       (problem-state (find-problem 'test-rover-problem))
                                                       :test 'equalp))
            :record-dependencies nil :domain *domain*))
    (fiveam:is-true
     (query (process-pddl-method-pre *domain*
                                     '(forall (?obj - objective ?m - mode)
                                       (not (communicate_image_data ?obj ?m))))
            (remove '(communicate_image_data objective1 high_res)
                                                      (problem-state (find-problem 'test-rover-problem))
                                                      :test 'equalp)
            :record-dependencies nil :domain *domain*))))


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
        (multiple-value-bind (bound-op state-tag protections cost unifier)
            (apply-action domain state '(!empty jug0 jug1) action nil 0 nil)
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
