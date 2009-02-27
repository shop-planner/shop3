;;; -*- Mode: common-lisp; package: shop2; -*-
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
(in-package :shop2)

(nst:def-fixtures simple-pddl-actions ()
  (action-def '(:action drive
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
                         (fuel ?v ?fafter)))))

(nst:def-fixtures action-test-fixtures (:uses (simple-pddl-actions))
  (domain (make-instance 'simple-pddl-domain
            :name (gentemp (symbol-name '#:domain))))
  (action (process-action domain action-def)))

(nst:def-test-group pddl-tests ()

  (nst:def-check (check-head :fixtures (simple-pddl-actions
                                        action-test-fixtures))
      (:equal '(drive ?v ?from ?to ?fbefore ?fafter))
    (pddl-action-head action))

  (nst:def-check (check-precondition :fixtures (simple-pddl-actions
                                                action-test-fixtures))
      (:equal '(and (at ?v ?from) (accessible ?v ?from ?to)
                    (fuel ?v ?fbefore) (next ?fbefore ?fafter)))
    (pddl-action-precondition action))

  (nst:def-check (check-effect :fixtures (simple-pddl-actions
                                          action-test-fixtures))
      (:equal '(and (not (at ?v ?from)) (at ?v ?to)
                    (not (fuel ?v ?fbefore)) (fuel ?v ?fafter)))
    (pddl-action-effect action))

  (nst:def-check (check-cost-fun :fixtures (simple-pddl-actions
                                            action-test-fixtures))
      (:eql 1.0)
    (pddl-action-cost-fun action))

  (nst:def-check defdomain-single-action
      (:progn (defdomain (#.(gentemp (symbol-name '#:domain))
                          :type simple-pddl-domain
                          :redefine-ok t)
                  ((:action walk
                            :parameters (?from ?to - loc)
                            :precondition (at robot ?from)
                            :effect (and (not (at robot ?from))
                                         (at robot ?to)))))
              (:equal '(PDDL-ACTION (!WALK ?FROM ?TO) (AT ROBOT ?FROM)
                        (AND (NOT (AT ROBOT ?FROM)) (AT ROBOT ?TO))
                        1.0)))
    (first (multiple-value-list (operator *domain* '!walk))))

  )


(nst:def-fixtures add-del-fixtures ()
  (domain-def
   (defdomain (#.(gentemp (symbol-name '#:domain))
               :type simple-pddl-domain
               :redefine-ok t)
       ((:action walk
                 :parameters (?from ?to - loc)
                 :precondition (at robot ?from)
                 :effect (and (not (at robot ?from))
                              (at robot ?to))))))

  )

(nst:def-test-group add-del-tests ()

  ;;; NB the two forms differ in the initial state.

  (nst:def-check (pddl-action-test :fixtures (add-del-fixtures))
      (:equal '((AT ROBOT NEW-YORK)))
    (let ((*state-encoding* :list))
      (declare (special *state-encoding*))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)))))
        (apply-action state
                      '(!walk new-jersey new-york)
                      (operator *domain* '!walk)
                      nil 0 nil)
        (state-atoms state))))

  (nst:def-check (pddl-action-failed-precond-test
                  :fixtures (add-del-fixtures))
      (:symbol fail)
    (let ((*state-encoding* :list))
      (declare (special *state-encoding*))
      (let ((state (make-initial-state *domain* *state-encoding*  nil)))
        (first
         (multiple-value-list
          (apply-action state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)))))))

(nst:def-fixtures quantified-preconditions-fixtures ()
  (domain (defdomain (#.(gentemp (symbol-name '#:domain))
                      :type pddl-domain
                      :redefine-ok t)
             nil))
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

(nst:def-test-group quantified-preconditions
    (quantified-preconditions-fixtures)
  (nst:def-check simplest
      (:equal '(forall (?s)
                (:of-type segment ?s)
                (imply
                 (and
                  (is-blocked ?s ?t ?s2 ?d2)
                  (not (= ?s ?s1)))
                 (not (occupied ?s)))))
    (pddl-action-precondition act))

  (nst:def-check with-others
      (:equal '(and
                (has-type ?a ?t)
                (is-moving ?a)
                (not (= ?s1 ?s2))
                (facing ?a ?d1)
                (can-move ?s1 ?s2 ?d1)
                (move-dir ?s1 ?s2 ?d2)
                (at-segment ?a ?s1)
                (forall (?s)
                 (:of-type segment ?s)
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
                          (not (occupied ?s)))))))))

  (nst:def-check multiple-variables
      (:equal '(forall (?s ?a2)
                (and
                 (:of-type segment ?s)
                 (:of-type airplane ?a2))
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
                                          (not (at ?a2 ?s))))))))

  (nst:def-check existential
      (:equal '(and
                (has-type ?a ?t)
                (is-moving ?a)
                (not (= ?s1 ?s2))
                (facing ?a ?d1)
                (can-move ?s1 ?s2 ?d1)
                (move-dir ?s1 ?s2 ?d2)
                (at-segment ?a ?s1)
                (not
                 (exists (?a1)
                  (:of-type airplane ?a1)
                  (and  (not (= ?a1 ?a))
                   (blocked ?s2 ?a1))))
                (forall (?s)
                 (:of-type segment ?s)
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
                       )))))

(nst:def-fixtures simple-when-fixtures ()
  (domain (defdomain (#.(gentemp (symbol-name '#:domain))
                      :type pddl-domain
                      :redefine-ok t)
              ((:action walk
                        :parameters (?from ?to - loc)
                        :precondition (at robot ?from)
                        :effect (and (not (at robot ?from))
                                     (at robot ?to)
                                     (when (carrying cargo)
                                       (and (not (at cargo ?from))
                                            (at cargo ?to)))))))))

(defun prop-sorter (p1 p2)
  (cond ((and p1 p2)
         (or (string-lessp (first p1) (first p2))
             (and (not (string-lessp (first p2) (first p1)))
                  (prop-sorter (cdr p1) (cdr p2)))))
        ;; if p1 is longer, it's greater --- this should never happen!
        (p1 nil)))

(nst:def-test-group simple-when ()
  (nst:def-check (simple-when-no :fixtures (simple-when-fixtures))
      (:equal '((AT ROBOT NEW-YORK)))
    (let ((*state-encoding* :list))
      (declare (special *state-encoding*))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)))))
        (apply-action state
                      '(!walk new-jersey new-york)
                      (operator *domain* '!walk)
                      nil 0 nil)
        (state-atoms state))))

  (nst:def-check (simple-when-yes :fixtures (simple-when-fixtures))
      (:equal '((AT CARGO NEW-YORK) (AT ROBOT NEW-YORK)
                (CARRYING CARGO)))
    (let ((*state-encoding* :list))
      (declare (special *state-encoding*))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                 (carrying cargo)))))

        (apply-action state
                      '(!walk new-jersey new-york)
                      (operator *domain* '!walk)
                      nil 0 nil)
        (sort (state-atoms state)
              'prop-sorter)))))

(nst:def-fixtures quantified-when-fixtures ()
  (domain (defdomain (#.(gentemp (symbol-name '#:domain))
                      :type pddl-domain
                      :redefine-ok t)
             ((:action walk
                       :parameters (?from ?to - loc)
                       :precondition (at robot ?from)
                       :effect (and (not (at robot ?from))
                                    (at robot ?to)
                                    (forall (?x - luggage)
                                            (when (carrying robot ?x)
                                              (and (not (at ?x ?from))
                                                   (at ?x ?to))))))))))

(nst:def-test-group quantified-when (quantified-when-fixtures)

  (nst:def-check check-state-making
      (:equal '((at bag1 new-jersey)
                (at bag2 new-jersey)
                (at bag3 new-jersey)
                (at robot new-jersey)
                (:of-type luggage bag1)
                (:of-type luggage bag2)
                (:of-type luggage bag3)))
    (let ((*state-encoding* :list))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                 (at bag1 new-jersey)
                                 (at bag2 new-jersey)
                                 (at bag3 new-jersey)
                                 (:of-type luggage bag1)
                                 (:of-type luggage bag2)
                                 (:of-type luggage bag3)))))
        (sort (state-atoms state) 'prop-sorter))))

  (nst:def-check check-antecedent-unsatisfied
      (:equal '((at bag1 new-jersey)
                (at bag2 new-jersey)
                (at bag3 new-jersey)
                (at robot new-york)
                (:of-type luggage bag1)
                (:of-type luggage bag2)
                (:of-type luggage bag3)))
    (let ((*state-encoding* :list))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                 (at bag1 new-jersey)
                                 (at bag2 new-jersey)
                                 (at bag3 new-jersey)
                                 (:of-type luggage bag1)
                                 (:of-type luggage bag2)
                                 (:of-type luggage bag3)))))
        (apply-action state
                      '(!walk new-jersey new-york)
                      (operator *domain* '!walk)
                      nil 0 nil)
        (sort (state-atoms state) 'prop-sorter))))

  (nst:def-check check-antecedent-satisfied
      (:equal '((at bag1 new-york)
                (at bag2 new-jersey)
                (at bag3 new-jersey)
                (at robot new-york)
                (carrying robot bag1)
                (:of-type luggage bag1)
                (:of-type luggage bag2)
                (:of-type luggage bag3)))
    (let ((*state-encoding* :list))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                 (at bag1 new-jersey)
                                 (at bag2 new-jersey)
                                 (at bag3 new-jersey)
                                 (carrying robot bag1)
                                 (:of-type luggage bag1)
                                 (:of-type luggage bag2)
                                 (:of-type luggage bag3)))))
        (apply-action state
                      '(!walk new-jersey new-york)
                      (operator *domain* '!walk)
                      nil 0 nil)
        (sort (state-atoms state) 'prop-sorter))))

  (nst:def-check check-antecedent-multiply-satisfied
      (:equal '((at bag1 new-york)
                (at bag2 new-jersey)
                (at bag3 new-york)
                (at robot new-york)
                (carrying robot bag1)
                (carrying robot bag3)
                (:of-type luggage bag1)
                (:of-type luggage bag2)
                (:of-type luggage bag3)))
    (let ((*state-encoding* :list))
      (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey)
                                 (at bag1 new-jersey)
                                 (at bag2 new-jersey)
                                 (at bag3 new-jersey)
                                 (carrying robot bag1)
                                 (carrying robot bag3)
                                 (:of-type luggage bag1)
                                 (:of-type luggage bag2)
                                 (:of-type luggage bag3)))))
        (apply-action state
                      '(!walk new-jersey new-york)
                      (operator *domain* '!walk)
                      nil 0 nil)
        (sort (state-atoms state) 'prop-sorter)))))
