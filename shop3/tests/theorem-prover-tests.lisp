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
;;; Additional developments made by Robert P. Goldman, and Ugur Kuter.
;;; Portions created by Drs. Goldman and Kuter are Copyright (C)
;;; 2017 SIFT, LLC.  These additions and modifications are also
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
;;; ----------------------------------------------------------------------
;;; Copyright(c) 2017  Smart Information Flow Technologies
;;; Air Force Research Lab Contract # FA8750-16-C-0182
;;; Unlimited Government Rights
;;; ----------------------------------------------------------------------

(defpackage shop-theorem-prover-tests
  (:shadowing-import-from #:shop3.theorem-prover #:fail #:random)
  (:import-from #:shop3.theorem-prover #:seek-satisfiers #:*domain*)
  (:use common-lisp shop3.theorem-prover fiveam))

(in-package #:shop-theorem-prover-tests)

(def-suite* theorem-prover-tests)

(def-fixture tp-domain ()
  (let ((*domain* (make-instance 'thpr-domain)))
    (&body)))

(def-fixture pddl-tp-domain ()
  (let ((*domain* (make-instance 'shop3:adl-domain)))
    (&body)))

(defun sorted-bindings (variable binding-lists)
  (sort (mapcar #'(lambda (x) (binding-list-value variable x))
                               binding-lists)
                       'string-lessp))

;;; does OR properly bind variables?
(test check-disjunction-bindings
  (with-fixture tp-domain ()
    (let ((bindings
            (query '(or (foo ?x) (bar ?x))
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (bar b))))))
      (is (equal '(a b) (sorted-bindings '?x bindings))))
    (let ((bindings
            (query '(or (foo ?x) (bar ?x))
                   '((foo a) (bar b)))))
      (is (equal '(a b) (sorted-bindings '?x bindings))))))

;;; what about IMPLY?
(test check-implication-bindings
  (with-fixture tp-domain ()
    (let ((bindings
            (query '(and (foo ?x) (imply (bar ?x) (baz ?x)))
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar b) (baz b)
                                                      (foo c)
                                                      (foo d) (bar d))))))
      (is (equal '(a b c) (sorted-bindings '?x bindings))))
    (let ((bindings
            (query '(and (foo ?x) (imply (bar ?x) (baz ?x)))
                   '((foo a) (foo b) (bar b) (baz b)
                     (foo c)
                     (foo d) (bar d)))))
      (is (equal '(a b c) (sorted-bindings '?x bindings))))
    ;; in this context, (NOT (BAR ?X)) has no sensible semantics.  Raise error.
    (signals non-ground-error
     (query '(and (imply (bar ?x) (baz ?x)))
            (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                             '((foo a) (foo b) (bar b) (baz b)))))
    (signals non-ground-error
     (query '(and (imply (bar ?x) (baz ?x)))
            '((foo a) (foo b) (bar b) (baz b)))))
  (with-fixture pddl-tp-domain ()
    (let ((bindings
            (query '(and (foo ?x) (imply (bar ?x) (baz ?x)))
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar b) (baz b)
                                                      (foo c)
                                                      (foo d) (bar d))))))
      (is (equal '(a b c) (sorted-bindings '?x bindings))))
    (let ((bindings
            (query '(and (foo ?x) (imply (bar ?x) (baz ?x)))
                   '((foo a) (foo b) (bar b) (baz b)
                     (foo c)
                     (foo d) (bar d)))))
      (is (equal '(a b c) (sorted-bindings '?x bindings))))))


(test check-setof-bindings
  (with-fixture tp-domain ()
    ;; simple setof and bagof
    (let ((bindings
            (query '(setof ?x (foo ?x) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b))))))
      (is (equal '((a b)) (sorted-bindings '?xes bindings))))
    (let ((bindings
            (query '(setof ?x (or (foo ?x) (bar ?x)) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar a))))))
      (is (equal '((a b)) (sorted-bindings '?xes bindings))))
    (let ((bindings
            (query '(setof (foo ?x ?y) (and (foo ?x) (bar ?y)) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar a))))))
      (is (alexandria:set-equal '((foo a a) (foo b a)) (first (sorted-bindings '?xes bindings)) :test 'equal)))
    (let ((bindings
            (query '(setof ?x (foo ?x) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((bar a) (bar b))))))
      (is-false bindings))
    ;; multiple variables
    (let ((bindings
            (query '(setof (?x ?y) (and (foo ?x) (bar ?y)) ?bs)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar a))))))
      (is (= (length bindings) 1))
      (is-true (alexandria:set-equal '((a a) (b a))
                                (first (sorted-bindings '?bs bindings))
                                :test 'equalp)))))

(test check-bagof-bindings
  (with-fixture tp-domain ()
    ;; simple bagof and bagof
    (let ((bindings
            (query '(bagof ?x (foo ?x) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b))))))
      (is (equal '((a b)) (sorted-bindings '?xes bindings))))
    ;; duplicate bindings here.  Does not work because our implementation of OR is "wrong."
    #|
    (let ((bindings
            (query '(bagof ?x (or (foo ?x) (bar ?x)) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar a))))))
      (princ bindings)
      (is (equal '((a a b)) (sorted-bindings '?xes bindings))))
    |#
    (let ((bindings
            (query '(bagof ?x (foo ?x) ?xes)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((bar a) (bar b))))))
      (is-false bindings))
    ;; multiple variables
    (let ((bindings
            (query '(bagof (?x ?y) (and (foo ?x) (bar ?y)) ?bs)
                   (shop3.common:make-initial-state *domain* (default-state-type *domain*)
                                                    '((foo a) (foo b) (bar a))))))
      (is (= (length bindings) 1))
      (is-true (alexandria:set-equal '((a a) (b a))
                                (first (sorted-bindings '?bs bindings))
                                :test 'equalp)))))

(test check-seek-satisfiers-expansion
  ;; make sure that we don't have vacuous IF form in
  ;; expansion...
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1
       '(seek-satisfiers ((foo)) state nil 1 nil :domain domain))
    (is-true expanded-p)
    (when expanded-p
      (is (eq (first expansion) 'let))
      ;; binding list
      (is (eql (length (second expansion)) 1))
      (is (eq (second                      ; the value
               (first (second expansion))) ;the binding form
              'domain))))
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1
       '(seek-satisfiers ((foo)) state nil 1 nil))
    (is-true expanded-p)
    (when expanded-p
      (is (eq (first expansion) 'let))
      ;; binding list
      (is (eql (length (second expansion)) 1))
      (is (eq (second                      ; the value
               (first (second expansion))) ;the binding form
              '*domain*)))))

(def-suite* test-new-random :in theorem-prover-tests)

(test random-repeatable
  (fiveam:is (eq 'random 'shopthpr::random))
  (fiveam:is-false (eq 'random 'cl:random))
  (let ((new-generator (random-state:make-generator :squirrel (random-state:hopefully-sufficiently-random-seed)))
        sequence1 sequence2)
    (let ((shopthpr:*random-generator* (random-state:copy new-generator)))
      (fiveam:is (equalp shopthpr:*random-generator* new-generator))
      (setf sequence1 (let (sequence)
                        (dotimes (x 10)
                          (push
                           (random 10)
                           sequence))
                        sequence)))
    (let ((shopthpr:*random-generator* (random-state:copy new-generator)))
      (setf sequence2 (let (sequence)
                        (dotimes (x 10)
                          (push
                           (random 10)
                           sequence))
                        sequence)))
    (fiveam:is (equalp sequence1 sequence2))))
