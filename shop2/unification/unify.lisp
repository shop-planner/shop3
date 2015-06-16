;;; -*- Mode: common-lisp; package: shop2.unifier; -*-
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

(in-package :shop2.unifier)

(defgeneric set-variable-property (domain x)
  (:documentation
   "Record facts about X being a variable, operator, or
other special symbol.  Done for side-effects.  Returns nothing
of interest."))

;;;(defstruct constraint
;;;  "Variables can be bound to constraints, which will be handled specially in
;;;unification and substitution."
;;;  op
;;;  rhs)

;;;(defstruct constraint-set
;;;  constraint-list)

;;;(defstruct attrib-variable
;;;  name                                       ; should always be a variable symbol, at least for now... [2005/11/06:rpg]
;;;  constraints)

;;;(defmethod print-object ((av attrib-variable) str)
;;;  (format str "<~A" (attrib-variable-name av))
;;;  (when (attrib-variable-constraints av)
;;;    (format str "~{ ~A~}" (attrib-variable-constraints av)))
;;;  (format str ">"))


;;;(defun prim-variable-p (x)
;;;  (and (symbolp x) (get x 'variable)))

;;; VARIABLEP returns T if X is a symbol whose name begins with "?"
;;; The code below is faster than checking for the ? each time, but
;;;  assumes that all variables, have been preprocessed.
(defun variablep (x)
  (and (symbolp x) (get x 'variable)))
(defmacro variable-p (x)
  "I can't remember the no-hyphen syntax for this call."
  `(variablep ,x))

(defun anonymous-var-p (x)
  "Does X name an anonymous variable?"
  (and (variablep x) (get x 'anonymous)))

(deftype shop-variable ()
    `(satisfies variablep))

;;; OLD def...
; If for some reason (e.g., debugging) you want to work with
;  non-preprocessed variables, use the following instead:
;(defun variablep (sym) (and (symbolp sym) (equal (elt (symbol-name sym) 0) #\?)))

(defun groundp (literal)
  "Is LITERAL a ground literal?  Traverse the tree looking for a
variable."
  (if (atom literal)
      (not (variablep literal))
    (every #'groundp literal)))

(defun variable-name (x) x)

;;; Lisp has a built-in UNION function, but we need something that returns
;;; the same results regardless of what platform SHOP is running on.
(defun shop-union (s1 s2 &key (test #'eql))
   (append s1
     (remove-if #'(lambda (e2) (member e2 s1 :test test)) s2)))

;;;(defun variable-name (x)
;;;  (cond ((prim-variable-p x) x)
;;;     ((typep x 'attrib-variable) (attrib-variable-name x))))

(defstruct (binding (:constructor make-binding (var val
                                                    ; &optional constraints
                                                    )))
  var
  val
  ;; constraints
  )

(defun find-binding (target binding-list)
  (find target binding-list :key #'binding-var))

(defun binding-list-value (var binding-list &optional (if-not-found :error))
  (let ((binding (find-binding var binding-list)))
    (cond (binding (binding-val binding))
          ((eq if-not-found :error)
           (error "Unable to find binding for ~A in ~S" var binding-list))
          (t if-not-found))))


(defun make-binding-list (variables bindings)
  (loop for var in variables
        as val in bindings
        collect (make-binding var val)))

;;; APPLY-SUBSTITUTION searches through TARGET, replacing each variable
;;; symbol with the corresponding value (if any) in A-LIST  (Dana Nau)
(defmacro apply-substitution (target a-list)
  `(if (null ,a-list) ,target
    (real-apply-substitution ,target ,a-list)))

;;; notes:  called by
;;;   COMPOSE-SUBSTITUTIONS, :OPERATOR
;;;   UNIFY, :OPERATOR
;;;   DO-CONJUNCT, :OPERATOR
;;;   REAL-SEEK-SATISFIERS, :OPERATOR
;;;   INVOKE-EXTERNAL-QUERY, :OPERATOR
;;;   APPLY-SUBSTITUTION, :OPERATOR
;;;   (:INTERNAL FIND-SATISFIERS 0), :OPERATOR
;;;   PROCESS-PRE, :OPERATOR
;;;   FIND-PLANS, :OPERATOR
;;;   SEEK-PLANS-TASK, :OPERATOR
;;;   APPLY-OPERATOR, :OPERATOR
;;;   USER-CHOOSE-TASK, :OPERATOR
;;;   APPLY-METHOD, :OPERATOR
;;;   MAKE-TASK-ITERATOR-ALIST, :OPERATOR --- in temporal shop2
;;;   (:INTERNAL APPLY-METHOD 0), :OPERATOR
;;;   UNSCHEDULABLE, :OPERATOR
;;;   (:INTERNAL FIND-SATISFIERS 0), :OPERATOR
;;;   (:INTERNAL APPLY-METHOD 0), :OPERATOR
;;;   REAL-APPLY-SUBSTITUTION, :OPERATOR

(defun real-apply-substitution (target binding-list)
  (cond ((atom target)
         (let ((result (find-binding target binding-list)))
           (if result (binding-val result) target)))
        ((null (cdr target)) (list (real-apply-substitution (car target) binding-list)))
        (t (cons (real-apply-substitution (car target) binding-list)
                 (real-apply-substitution (cdr target) binding-list)))))

#|
(define-condition constraint-failure (error)
  ((constraint
    :initarg :constraint
    :reader constraint
    )))
|#

;;; COMPOSE-SUBSTITUTIONS applies SUB2 to the right-hand-side of each item
;;; in SUB1, and appends all items in SUB2 whose left-hand-sides aren't in SUB1.
;;; *** Warning:  COMPOSE-SUBSTITUTIONS destroys the old value of SUB1 ***
;;; I normally would avoid destructive operations, but here it speeds up the
;;; planner by about 5%, and none of the calling functions need SUB1 afterwards
;;; (Dana Nau)
(defun compose-substitutions (sub1 sub2)
  (dolist (pair sub1)
    (setf (binding-val pair) (apply-substitution (binding-val pair) sub2))
;;;    (when (binding-constraints pair)
;;;      (setf (binding-constraints pair)
;;;         (apply-substitution (binding-constraints pair) sub2)))
;;;    (when (variable-p (binding-val pair))
;;;      ;; we may need to compose constraints...
;;;      (let ((prev-bind
;;;          (find (binding-val pair)
;;;                sub2 :key #'binding-var)))
;;;     (when (and prev-bind (binding-constraints prev-bind))
;;;       (setf (binding-constraints pair)
;;;             (nconc (binding-constraints pair) (binding-constraints prev-bind))))))
;;;    (when (groundp (binding-val pair))
;;;      (when (binding-constraints pair)
;;;     (multiple-value-bind (success failed-constraint)
;;;         (check-constraints pair)
;;;       (unless success
;;;         (error (make-condition 'constraint-failure
;;;                                :constraint failed-constraint))))))
    )
  (dolist (pair sub2)
    (push pair sub1))
;;;    (let ((prev-bind (find pair sub1 :key #'(lambda (x) (binding-var x)))))
;;;      (cond (prev-bind
;;;          ;; if there are constraints on sub2, we need to compoose them together with those in sub1
;;;          (setf (binding-constraints prev-bind)
;;;             (nconc (binding-constraints prev-bind) (binding-constraints pair))))
;;;         (t
;;;          (push pair sub1)))))
  sub1)

(defun check-constraints (binding)
  "Check to make sure that all the constraints on the
binding-val are satisfied.
Returns a boolean, and a second value (for debugging)
listing a failed constraint."
  (declare (ignore binding))
  ;; for now, stub this out, and make sure that things still
  ;; work... [2005/11/06:rpg]
;;;  (dolist (bind binding-list)
;;;    (let ((var (binding-var bind)
;;;    (when (typep var 'attrib-variable)
;;;      (when (attrib-variable-constraints var)
;;;     ;; at this point, I believe that the binding-list should be
;;;     ;; standardized... [2005/11/06:rpg]
;;;     (let ((val (apply-substitution e
  t)

(defmacro unify-fail (e1 e2)
  "It's painful \(and bug-inducing\) to have to remember to compare the
result of unify with 'fail."
  `(eq (unify ,e1 ,e2) 'shop2.unifier::fail))

(defmacro unify-p (e1 e2)
  "It's painful \(and bug-inducing\) to have to remember to compare the
result of unify with 'fail.  This checks to see whether E1 and E2
unify, and returns T or NIL, accordingly."
  `(not (unify-fail ,e1 ,e2)))

;;; UNIFY is based on the procedure in Nilsson's 1980 book, but modified
;;; (Dana Nau)
;;; Added handler to trap cases where we unify variables, but fail to
;;; successfully satisfy their constraints. [2005/11/07:rpg]
(defun unify (e1 e2)
  "Checks to see whether or not E1 and E2 unify, returning a substitution
if they do, or FAIL \(*NOT* nil\) if they don't."
  (cond ((atom e1) (unify1 e1 e2))
        ((atom e2) (unify1 e2 e1))
        (t (let ((hsub (unify (car e1) (car e2))))
             (if (eql hsub 'fail)
                 'fail
                 (let* ((tail1 (apply-substitution (cdr e1) hsub))
                        (tail2 (apply-substitution (cdr e2) hsub))
                        (tsub (unify tail1 tail2)))
                   (if (eql tsub 'fail)
                       'fail
                       (compose-substitutions hsub tsub)
;;;                    (handler-bind (( constraint-failure #'(lambda (c)
;;;                                                            (declare (ignore c))
;;;                                                            (return-from unify
;;;                                                              'fail))))
;;;                      (compose-substitutions hsub tsub))
                       )))))))

(defun unify1 (e1 e2)
  (cond ((equal e1 e2) nil)
        ((variablep e1)
         (if (and (occurs (variable-name e1) e2))
             'fail
             (list (make-binding e1 e2))))
        ((variablep e2)
         (list (make-binding e2 e1)))
        (t 'fail)))

;;; OCCURS is the infamous "occurs check" - it returns T if the variable
;;; symbol V occurs anywhere in the expression EXPR (Dana Nau)
(defun occurs (variable-name expr)
  (cond ((variablep expr)
         (eq variable-name (variable-name expr)))
        ((atom expr)
         ;; if it's an atom, and it's not also a variable then no...
         nil)
        (t
         ;; should be a cons with car and cdr...
         (or (occurs variable-name (car expr)) (occurs variable-name (cdr expr))))))

(defun variable-gensym (&optional base-name)
  "Return a new variable, made from BASE-NAME"
  (let ((sym (if base-name (gensym (string base-name)) (gensym "?"))))
    (setf (get sym 'variable) t)
    (when (eq (aref (symbol-name sym) 1) #\_)
      (setf (get sym 'anonymous) t))
    sym))

;;;---------------------------------------------------------------------------
;;; The following appears to be dead code. [2008/04/06:rpg]
;;;---------------------------------------------------------------------------

#|
;;; STANDARDIZER returns a substitution that replaces every variable symbol
;;; in EXPRESSION with a new variable symbol not used elsewhere (Dana Nau)
;;; modified this to reuse the (badly-named) GET-ALIST, to avoid
;;; repeated code. [2005/11/06:rpg]
(defun standardizer (expression)
  (get-alist (extract-variables expression)))
|#

(defun standardize (expr &optional subs)
  "Replace all variables in EXPR with newly-generated
variables, with new names."
  (cond
    ((null expr) (values nil subs))
    ((consp expr)
     (multiple-value-bind
         (car-expr car-subs)
         (standardize (car expr) subs)
       (multiple-value-bind
           (cdr-expr cdr-subs)
           (standardize (cdr expr) car-subs)
         (values (cons car-expr cdr-expr) cdr-subs))))
    ((variablep expr)
     (let ((bin (assoc expr subs)))
       (if bin
           (values (cdr bin) subs)
           (let ((new-var (variable-gensym expr)))
             (values new-var (cons (cons expr new-var) subs))))))
    (t
     (values expr subs))))

(defun uniquify-anonymous-variables (sexp)
  "Replace all anonymous variables in SEXP by standardization,
so that all anonymous variables are distinct."
  (labels ((iter (sexp)
             (cond ((null sexp) nil)
                   ((anonymous-var-p sexp)
                    (shop2.unifier::variable-gensym sexp))
                   ((consp sexp)
                    (cons
                     (iter (car sexp))
                     (iter (rest sexp))))
                   (t
                    ;; can't check for ATOM, because there might be arrays, or
                    ;; any old stuff in the SHOP2 code.
                    sexp))))
      (iter sexp)))

;;;---------------------------------------------------------------------------
;;; Moved this here because it has to do with binding lists... [2005/11/06:rpg]
;;;---------------------------------------------------------------------------

; Some bindings returned in a query may involve uninterned symbols.  This
;  routine substitutes those symbols with the original values.
(defun fix-uninterned-bindings (bindings query-vars)
;(format t "~%bindings: ~s - vars: ~s" bindings query-vars)
  (mapcar #'(lambda (binding)
              (let* ((name (symbol-name (binding-var binding)))
                     (matching-var
                      (find-if
                       #'(lambda (v) (string-equal (symbol-name v) name))
                       query-vars)))
                (if matching-var
                    (make-binding matching-var (binding-val binding))
                  binding)))
          bindings))

(defmethod set-variable-property ((domain t) x)
  (cond ((symbolp x)
         (cond ((eql (elt (symbol-name x) 0) #\?)
                (setf (get x 'variable) t)
                (when (eql (elt (symbol-name x) 1) #\_)
                  (setf (get x 'anonymous) t)))
               ((eql (elt (symbol-name x) 0) #\!)
                (setf (get x 'primitive) t)))
         (values))
        ((atom x) (values))
        ((consp x)
         (set-variable-property domain (car x))
         (set-variable-property domain (cdr x)))))


;;;---------------------------------------------------------------------------
;;; This function is called only inside process-pre, where it is
;;; called to make a new set of variables to replace those in
;;; VARIABLES.  [2005/11/06:rpg]
;;;---------------------------------------------------------------------------

(defun get-alist (variables)
  "Takes a list of variable names as input (VARIABLES), and
returns two values:  a binding-list mapping the VARIABLES to
newly created variables (made using VARIABLE-GENSYM), and
a list of the new variables, made with VARIABLE-GENSYM."
  (let ((vlist (mapcar #'(lambda (x)
                           (declare (ignore x))
                           (variable-gensym))
                       variables)))
    (values
     (make-binding-list variables vlist)
     vlist)))
