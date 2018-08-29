;;; -*- Mode: common-lisp; -*-
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

;;; Smart Information Flow Technologies Copyright 2009 Unpublished work
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

(in-package :shop2.theorem-prover)

(defvar *domain* nil)

(defvar *record-dependencies-p* NIL
  "Do we record dependencies when we find literals in the theorem
prover.  If so, see *LITERALS* and *ESTABLISHERS*.")

(defvar *optimize-first-retrieval* NIL
  "In some cases we can optimize solutions to :FIRST to find only
the first match.")

(defvar *literals*)
(defvar *establishers*)

(defvar *state*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*state* 'variable)
        "This special variable should be bound to the current
state inside FIND-SATISFIERS, qv., giving axioms access to the
state data structure."))

(defgeneric axioms (thpr-domain predicate)
  (:documentation "Return a list of all the SHOP2
axioms for PREDICATE in THPR-DOMAIN.")
  (:method (no-axioms-domain predicate)
    (declare (ignorable no-axioms-domain predicate))
    nil))

(defgeneric static-preds (domain)
  (:documentation "Return a list of predicate names for static predicates."))


;;;---------------------------------------------------------------------------
;;; Defgenerics for functions used to tailor the theorem-prover
;;;---------------------------------------------------------------------------
(defgeneric real-seek-satisfiers-for
    (domain goal-head goal other-goals state bindings level just1 dependencies-in)
  (:documentation "Define how a semantically meaningful logical
connective should be interpreted in the given domain.

IT IS STRONGLY RECOMMENDED that defmethod should NOT be used with this
function!  Instead, please use the def-logical-keyword macro."))

(defgeneric logical-keywordp (symbol domain)
  (:documentation "Return true if the planning domain should interpret
symbol as a semantically meaningful logical connective, as opposed to
an atomic predicate name.

IT IS STRONGLY RECOMMENDED that defmethod should NOT be used with this
function!  Instead, please use the def-logical-keyword macro.")

  (:method (symbol domain)
     (declare (ignorable domain symbol))
     nil))

;;;---------------------------------------------------------------------------
;;; end of defgenerics
;;;---------------------------------------------------------------------------


(defclass has-axioms-mixin ()
     ((axioms
       ;; axioms are hashed by predicate name...
       :initform (make-hash-table :test 'eq)
       :reader domain-axioms
       ))
  )

(defmethod axioms ((domain has-axioms-mixin) (name symbol))
  "Return a list of axioms for for NAME as defined in DOMAIN."
  (gethash name (domain-axioms domain)))


;;;(defclass domain (shop2.common:domain has-axioms-mixin)
(defclass thpr-domain (has-axioms-mixin)
  (
   (domain-name
    :initarg :domain-name
    :initarg :name
    :reader domain-name
    )
   (default-state-type
    :initarg :default-state-type
    :reader default-state-type
    ;; default default!
    :initform :mixed)
   )
  (:documentation "An object representing a SHOP2 theorem prover domain.")
  )

(defclass static-predicates-mixin ()
  ((static-preds
    :initarg :static-preds
    :reader static-preds
    :initform nil
    ))
  (:documentation "Add this to domain classes that should have 
static predicates defined."))


(define-condition theorem-prover-condition ()
     ()
  (:documentation "This mixin class should be added to any condition raised by
the theorem-prover, in order to allow handlers to distinguish such conditions.
It provides no additional behavior or slots, so that it can be mixed into
warnings, errors, etc.")
  )

(define-condition instantiation-error (error theorem-prover-condition)
  ((predicate
    :initarg :predicate
    :reader predicate
    )
   (argno
    :initarg :argno
    :reader argno
    )
   )
  (:report (lambda (condition stream)
             (format stream "Predicate ~a used with " (predicate condition))
             (if (slot-boundp condition 'argno)
                 (format stream "~dth argument " (argno condition))
                 (format stream "some argument(s) "))
             (format stream "incorrectly instantiated."))))

(define-condition bad-argument (error theorem-prover-condition)
  ((predicate
    :initarg :predicate
    :reader predicate
    )
   (argno
    :initarg :argno
    :reader argno
    )
   (comment
    :initarg :comment
    :reader comment
    ))
  (:report (lambda (condition stream)
             (format stream "Predicate ~a used with " (predicate condition))
             (if (slot-boundp condition 'argno)
                 (format stream "~dth argument " (argno condition))
               (format stream "some argument(s) "))
             (format stream "incorrect.")
             (when (slot-boundp condition 'comment)
               (format stream "  ~a" (comment condition))))))

(define-condition non-ground-error (error theorem-prover-condition)
  ((var
    :initarg :var
    :reader var
    )
   (expression
    :initarg :expression
    :reader expression
    ))
  (:report (lambda (condition stream)
             (format stream "Variable ~a is not ground in ~a, but must be."
                     (var condition)
                     (expression condition)))))

(define-condition incorrect-arity-error (error theorem-prover-condition)
  ((op
    :initarg :op
    :reader op
    )
   (correct-arity
    :initarg :correct-arity
    :reader correct-arity
    :type fixnum
    )
   (expression
    :initarg :expression
    :reader expression
    ))
  (:report (lambda (condition stream)
             (format stream "Operator ~a takes ~d arguments. Used incorrectly in ~a."
                     (op condition)
                     (correct-arity condition)
                     (expression condition)))))

(define-condition incomplete-dependency-error (theorem-prover-condition)
  ((logical-op
    :initarg :logical-op
    :reader logical-op
    )
   (expression
    :initarg :expression
    :reader expression))
  (:report (lambda (condition stream)
	     (format stream "We do not have correct logic for computing dependencies for expression ~a. Simply return no new dependencies."
		     (expression condition)))))
    

;;; used for the internals of IF-THEN-ELSE in the theorem-prover
(define-condition cut-commit (condition)
  ())

(deftype raw-depend-list ()
  `(and (satisfies listp) (satisfies all-raw-depends-p)))

(deftype list-raw-depend-lists ()
  `(and (satisfies listp) (satisfies all-raw-depends-lists-p)))

(defstruct (raw-depend (:conc-name rd-))
  "Raw dependency record, computed by the theorem-prover before
using it to build dependency records in the enhanced plan trees."
  prop
  est)

(defun all-raw-depends-p (list)
  (every #'(lambda (x) (typep x 'raw-depend)) list))

(defun all-raw-depends-lists-p (list)
  (every #'(lambda (x) (typep x 'raw-depend-list)) list))

;;;---------------------------------------------------------------------------
;;; Starting work on static predicates
;;;---------------------------------------------------------------------------
;; FIXME: Replace with CLASS slot?
(defgeneric has-static-preds-p (domain)
  (:method (domain)
    (declare (ignorable domain))
    nil)
  (:method ((domain static-predicates-mixin))
    t))


