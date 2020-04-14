;;;
;;; Version: MPL 1.1/LGPL 2.1
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
;;; Alternatively, the contents of this file may be used under the terms of
;;; the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the LGPL are
;;; applicable instead of those above. 
;;; 
;;; Copyright (C) 2020, Robert P. Goldman and SIFT, LLC.

;;;---------------------------------------------------------------------------
;;; Because the metric-fluents mixin is so deeply tied into the functioning
;;; of the SHOP theorem-prover, we must define it, and key methods here, and
;;; import them into the SHOP package proper.
;;;---------------------------------------------------------------------------


(in-package :shop3.theorem-prover)

(defclass fluents-mixin ()
  ((fluent-functions
    :initarg :fluent-functions
    :reader fluent-functions
    :documentation "List of symbols naming metric fluent functions."
    ))
  (:documentation "MIXIN that adds support for PDDL metric fluents."))

(defgeneric fluent-function-p (domain name)
  (:method ((domain fluents-mixin) (name symbol))
    (member name (fluent-functions domain)))
  (:method ((domain fluents-mixin) name)
    (declare (ignorable name domain))
    nil))

(defgeneric fluent-expr-p (domain expr)
  (:method ((domain fluents-mixin) (value number))
    (declare (ignorable value))
    t)
  (:method ((domain fluents-mixin) (expr list))
    (or
     (fluent-function-p domain (first expr))
     (and (eq (first expr) '-)
          (= (length expr) 2)
          (fluent-expr-p domain (second expr)))
     (and (member (first expr) +binary-numerical-operators+)
          (= (length expr) 3)
          (fluent-expr-p domain (second expr))
          (fluent-expr-p domain (third expr)))))
  (:method ((domain fluents-mixin) expr)
    (declare (ignorable expr domain))
    nil))

(defgeneric fluent-comparison-p (domain op)
  (:method ((domain fluents-mixin) (op symbol))
    (member op +numerical-comparisons+))
  (:method ((domain fluents-mixin) op)
    (declare (ignorable op domain))
    nil))


(defparameter +numerical-comparisons+
  '(< > <= >= =)
  "Numerical comparisons permitted by the :fluents requirement.")

(defparameter +binary-numerical-operators+
  '(+ - * /)
  "Binary numerical operations permitted by the :fluents requirement.")
