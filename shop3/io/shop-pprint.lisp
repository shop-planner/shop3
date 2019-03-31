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

(defpackage shop3-pprint
    (:use :shop3 :common-lisp :iterate)
    (:nicknames #:shop2-pprint)
    (:import-from :shop3
                  #:variablep
                  #:*shop-pprint-table*))

(in-package :shop3-pprint)

(defun method-sexp-p (lst)
  (and (listp lst)
       (eq (first lst) :method)))

(defun operator-sexp-p (lst)
  (and (listp lst)
       (eq (first lst) :operator)))

(defun op-sexp-p (lst)
  (and (listp lst)
       (eq (first lst) :op)))

(defun axiom-sexp-p (lst)
  (and (listp lst)
       (eq (first lst) :axiom)))

(defun method-printer (str method)
  (pprint-logical-block (str method :prefix "(" :suffix ")")
    (write (first method) :stream str)  ; keyword
    (write-char #\Space str)
    (pprint-newline :miser str)         ; break before task, if necessary
    ;; (pprint-indent :current 0)
    (write (second method) :stream str) ; header/task
    (pprint-newline :mandatory str)     ; mandatory break before body
    (pprint-logical-block (str (cddr method))
      (pprint-indent :block 2)
      (loop :with tails = (cddr method)
            :with first = t
            :while tails
            :unless first
              :do (pprint-newline :mandatory str)
            :do (setf first nil)
            :do (let* ((name-or-preconds (pop tails))
                       (name (when (and (symbolp name-or-preconds)
                                        (not (variablep name-or-preconds)))
                               name-or-preconds))
                       (preconds (if name (pop tails)
                                     name-or-preconds))
                       (task-net (pop tails)))
                  (pprint-logical-block (str (if name (list name preconds task-net)
                                                 (list preconds task-net)))
                    (when name (write name :stream str))
                    (pprint-newline :mandatory str)
                    ;; preconditions
                    (pprint-preconds str preconds)
                    (pprint-newline :mandatory str)
                    ;; task-net
                    (pprint-task-net str task-net)))))))

(defun pprint-task-net (str task-net)
  ;; FIXME: handle the :IMMEDIATE, :ORDERED, and :UNORDERED keywords
  (pprint-logical-block (str task-net :prefix "(" :suffix ")")
    ;; (pprint-indent :block 1 str)
    (format str "~{~a~^~:@_~}" task-net)
    (pprint-newline :miser str)))

(defun pprint-preconds (str preconds)
  ;; FIXME: handle the :FIRST, :JUST-ONE, etc. keywords
  (case (first preconds)
    (:first
     (pprint-logical-block (str preconds :prefix "(" :suffix ")")
       (write :first :stream str)
       (pprint-indent :block 3 str)
       (pprint-newline :mandatory str)
       (print-list-suffix-lines str (rest preconds))))
    (otherwise
     (print-list-on-lines str preconds))))

(defun operator-printer (str operator)
  (pprint-logical-block (str operator :prefix "(" :suffix ")")
    (write (first operator) :stream str) ; keyword
    (write-char #\Space str)
    (pprint-newline :miser str)         ; break before task, if necessary
    ;; (pprint-indent :current 0)
    (write (second operator) :stream str) ; task
    (pprint-newline :mandatory str)     ; mandatory break before body
    (let ((lopt (length operator))
          preconditions deletions additions cost)
      (cond ((= lopt 4)
             (setf deletions (third operator)
                   additions (fourth operator)))
            ((and (= lopt 5) (numberp (fifth operator)))
             (setf deletions (third operator)
                   additions (fourth operator)
                   cost (fifth operator)))
            ((= lopt 5)
             (setf
              preconditions (third operator)
              deletions (fourth operator)
              additions (fifth operator)))
            ((= lopt 6)
             preconditions (third operator)
             deletions (fourth operator)
             additions (fifth operator)
             cost (sixth operator))
            (t (error "Ill-formed operator.")))
      (pprint-indent :block 2)
      (when preconditions
        (format str ";;; preconditions~:@_")
        (pprint-preconds str preconditions)
        (pprint-newline :mandatory str))
      (format str ";;; deletes~:@_")
      (print-list-on-lines str deletions)
      (pprint-newline :mandatory str)
      (format str ";;; additions~:@_")
      (print-list-on-lines str additions)
      (when cost
        (pprint-newline :mandatory str)
        (write cost :stream str)))))

(defun print-list-on-lines (str list)
  (pprint-logical-block (str list :prefix "(" :suffix ")")
    (pprint-indent :block 1)
    (mapc #'(lambda (x)
              (write x :stream str)
              (pprint-newline :linear str))
          list)))

(defun print-list-suffix-lines (str list)
  (format str "~{~a~^~_~}" list)
  (pprint-newline :miser str))




(set-pprint-dispatch '(satisfies method-sexp-p)
                     'method-printer
                     0
                     *shop-pprint-table*)

(set-pprint-dispatch '(satisfies operator-sexp-p)
                     'operator-printer
                     0
                     *shop-pprint-table*)
