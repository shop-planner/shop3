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

(fiveam:def-suite pddl-tests)

(fiveam:in-suite pddl-tests)

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
    (locally (declare (ignorable typelist action-def untyped-action-def))
      (&body))))

(defmacro nst-def-fixtures (name options &rest defs)
  (unless (null options)
    (warn "Don't know how to translate ~s" options))
  `(fiveam:def-fixture ,name ()
       (let* ,defs
         (&body))))

(fiveam:test type-list-translation
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:is
     (equalp (list '(?v ?from ?to ?fbefore ?fafter)
                   '(vehicle location location object object))
             (multiple-value-list
              (typed-list-vars typelist))))))

(fiveam:def-fixture action-test-fixtures ()
  (let* ((*define-silently* t)
         (domain (make-instance 'pddl-domain
                                :name (gentemp (symbol-name '#:domain))))
         (action (process-action domain action-def))
         (untyped-action (process-action domain untyped-action-def)))
    (locally (declare (ignorable action untyped-action))
      (&body))))

(fiveam:test pddl-actions
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:with-fixture action-test-fixtures ()
      (fiveam:is (equal '(drive ?v ?from ?to ?fbefore ?fafter)
                        (pddl-action-head action)))))
  (fiveam:with-fixture simple-pddl-actions ()
    (fiveam:with-fixture action-test-fixtures ()
      (fiveam:is (equal '(and
                          (%enforce-type-constraints (vehicle ?v)
                           (location ?from)
                           (location ?to)
                           (fuel-level ?fbefore)
                           (fuel-level ?fafter))
                          (and (at ?v ?from) (accessible ?v ?from ?to)
                           (fuel ?v ?fbefore) (next ?fbefore ?fafter)))
                        (pddl-action-precondition action)))
      (fiveam:is (equal '(and
                          (%enforce-type-constraints (vehicle ?v)
                           (location ?from)
                           (location ?to)
                           (object ?fbefore)
                           (object ?fafter))
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
          (and (%enforce-type-constraints (loc ?from) (loc ?to)) (AT ROBOT ?FROM))
          (AND (NOT (AT ROBOT ?FROM)) (AT ROBOT ?TO))
          1.0)
        (progn
          (defdomain (#.(gentemp (symbol-name '#:domain))
                        :type pddl-domain
                        :redefine-ok t)
              ((:action walk
                :parameters (?from ?to - loc)
                :precondition (at robot ?from)
                :effect (and (not (at robot ?from))
                             (at robot ?to)))))
          (operator *domain* '!walk)))))))



(fiveam:def-fixture add-del-fixtures ()
  (progn
    (let ((shop2:*define-silently* t))
      (defdomain (test-add-del-domain
                  :type pddl-domain
                  :redefine-ok t)
          ((:action walk
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
      (sort '((AT ROBOT NEW-YORK) (loc new-york) (loc new-jersey)) 'prop-sorter)
      (sort
       (let ((*state-encoding* :list))
         (declare (special *state-encoding*))
         (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey) (loc new-jersey) (loc new-york)))))
           (apply-action state
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
            (apply-action state
                          '(!walk new-jersey new-york)
                          (operator *domain* '!walk)
                          nil 0 nil))))))


(nst-def-fixtures quantified-preconditions-fixtures ()
  (domain (defdomain (quantified-preconditions-domain
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

(fiveam:test quantified-preconditions
  (fiveam:with-fixture quantified-preconditions-fixtures ()
    (fiveam:is
     (equal
      '(and
        (%enforce-type-constraints (airplane ?a) (airplanetype ?t) (direction ?d1) (segment ?s1) (segment ?s2) (direction ?d2))
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
              (%enforce-type-constraints (airplane ?a) (airplanetype ?t) (direction ?d1) (segment ?s1) (segment ?s2) (direction ?d2))
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
              (%enforce-type-constraints
               (airplane ?a) (airplanetype ?t) (direction ?d1)
               (segment ?s1) (segment ?s2) (direction ?d2))
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
              (%enforce-type-constraints
                (airplane ?a) (airplanetype ?t) (direction ?d1) (segment ?s1) (segment ?s2) (direction ?d2))
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
    (defdomain (simple-when-domain
                      :type pddl-domain
                      :redefine-ok t)
              ((:action walk
                        :parameters (?from ?to - loc)
                        :precondition (at robot ?from)
                        :effect (and (not (at robot ?from))
                                     (at robot ?to)
                                     (when (carrying cargo)
                                       (and (not (at cargo ?from))
                                            (at cargo ?to)))))))
    (&body)))

(defun prop-sorter (p1 p2)
  (cond ((and p1 p2)
         (or (string-lessp (first p1) (first p2))
             (and (not (string-lessp (first p2) (first p1)))
                  (prop-sorter (cdr p1) (cdr p2)))))
        ;; if p1 is longer, it's greater --- this should never happen!
        (p1 nil)))

(fiveam:test simple-when
  (fiveam:with-fixture simple-when-fixtures ()
    (fiveam:is 
     (equal
      (sort 
       '((AT ROBOT NEW-YORK) (loc new-jersey) (loc new-york))
       'prop-sorter)
      (sort
       (let ((*state-encoding* :list))
         (declare (special *state-encoding*))
         (let ((state (make-initial-state *domain* *state-encoding* '((at robot new-jersey) (loc new-jersey) (loc new-york)))))
           (apply-action state
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

          (apply-action state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)
          (sort (state-atoms state)
                'prop-sorter)))))))

(fiveam:def-fixture quantified-when-fixtures ()
  (progn (defdomain (quantified-when-domain
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
                                                   (at ?x ?to))))))))
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
          (apply-action state
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
          (apply-action state
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
          (apply-action state
                        '(!walk new-jersey new-york)
                        (operator *domain* '!walk)
                        nil 0 nil)
          (sort (state-atoms state) 'prop-sorter)))))))

(in-package :shop2-user)

;;; this doesn't work yet because my methods are too stupid. [2017/07/19:rpg]
#+ignore
(fiveam:test pddl-planning
  (let ((shop2:*define-silently* t))
    (load (asdf:system-relative-pathname "shop2" "examples/openstacks-adl/domain.lisp"))
    (load (asdf:system-relative-pathname "shop2" "examples/openstacks-adl/p01.lisp")))
  (fiveam:is-true (funcall (if shop2::*test-explicit-state-search* 'find-plans-stack 'find-plans)
                           'os-sequencedstrips-p5_1 :verbose 0)))



