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

(in-package :shop2)

(defvar *start-run-time*)
(defvar *start-real-time*)

;; (defvar *traced-operators* nil)      ; break when attempting to
                                        ; apply one of these.
(defvar *traced-methods* nil)           ; break when attempting to
                                        ; apply one of these.
(defvar *traced-tasks* nil)             ; break when attempting to
                                        ; expand one of these.
(defvar *traced-axioms*
  nil)
(defvar *traced-goals* nil)


(defconstant SHOP-TRACE-ITEMS
  (list :methods :axioms :operators :tasks :goals :effects :protections
       :states :plans :item)
  "Acceptable arguments for SHOP-TRACE (and SHOP-UNTRACE).")

(defmacro shop-trace (&rest items)
  "- (SHOP-TRACE) with no arguments will return a list of what's
currently being traced.

 - (SHOP-TRACE ITEM) will turn on tracing for ITEM.

 ITEM may be any of the following:
 
    - the name of a method, axiom, operator, task, or predicate;
    - one of the keywords :METHODS, :AXIOMS, :OPERATORS, :TASKS,
      :GOALS, :EFFECTS, or :PROTECTIONS, in which case SHOP will
      trace all items of that type (:GOALS, :EFFECTS, and :PROTECTIONS
      refer to three different ways predicates can occur: as goals to
      be satisfied, and as effects or protections in operators);
    - a pair of the form (:TASK <taskname>), (:METHOD <methodname>).  SHOP will
      break when attempting to expand the task, or apply the method, respectively.
    - the keyword :STATES, in which case SHOP will include the current
      state whenever it prints out a tracing message
    - the keyword :ALL in which case SHOP will print out all the tracing
      information it knows how to.

 - (SHOP-TRACE ITEM1 ITEM2 ...) will do the same for a list of items"
  (let* ((items `,items)
        (new-items
         (if (null items) nil items)
         ))
    (when (member :all new-items)
      (setf new-items (delete :all new-items))
      (setf new-items (union SHOP-TRACE-ITEMS new-items)))

    `(shop-trace-1 ',new-items)))

(defun shop-trace-1 (items)
  ;; make sure the argument is coerced to a list
  (unless (null items)
    (dolist (item items)
      (cond ((member item SHOP-TRACE-ITEMS)
             (pushnew item *shop-trace*))
            ((listp item)
             (macrolet ((trace-item (variable)
                          `(progn (pushnew (second item) ,variable)
                                  (pushnew :item *shop-trace*))))
               (case (car item)
                 (:task (trace-item *traced-tasks*))
                 (:method (trace-item *traced-methods*))
                 (:goal (trace-item *traced-goals*))
                 (:axiom (trace-item *traced-axioms*))
                 (otherwise
                  (warn "Ignoring invalid shop-trace argument ~S" item)))))
            (t
             (warn "Ignoring invalid shop-trace argument ~S" item)))))
  (shop-trace-info))

(defmethod trigger-trace ((keyword (eql :methods)) (item symbol))
  (member item *traced-methods* :test 'eq))

(defmethod trigger-trace ((keyword (eql :axioms)) (item symbol))
  (member item *traced-axioms* :test 'eq))

(defmethod trigger-trace ((keyword (eql :goals)) (item symbol))
  (member item *traced-goals* :test 'eq))

(defmethod trigger-trace ((keyword (eql :tasks)) (item symbol))
  (member item *traced-tasks* :test 'eq))


(defun shop-trace-info ()
  "Information about the traced aspects of shop2."
  (append
   *shop-trace*
   (mapcar #'(lambda (taskname)
               `(:task ,taskname))
           *traced-tasks*)
   (mapcar #'(lambda (methname)
               `(:method ,methname))
           *traced-methods*)
   (mapcar #'(lambda (goalname)
               `(:goal ,goalname))
           *traced-goals*)
   (mapcar #'(lambda (axiomname)
               `(:axiom ,axiomname))
           *traced-axioms*)))


(defmacro shop-untrace (&rest items)
  "(SHOP-UNTRACE ...) is the inverse of (SHOP-TRACE ...)"
  (if (or (member :all items) (null items))
      '(shop-untrace-all)
    `(shop-untrace-1 ',items)))

(defun shop-untrace-all ()
  (setf *shop-trace* nil
       *traced-tasks* nil
       ;;*traced-operators* nil
       *traced-methods* nil
       *traced-goals* nil
       *traced-axioms* nil))


(defun shop-untrace-1 (items)
  ;; it's OK to use destructive deletion here
  (dolist (item items)
    (cond ((symbolp item)
          (setq *shop-trace* (delete item *shop-trace*)))
         ((eq (car item) :task)
          (setf *traced-tasks* (delete (second item) *traced-tasks*)))
         ((eq (car item) :method)
          (setf *traced-methods* (delete (second item) *traced-methods*)))
         ((eq (car item) :all)
          (shop-untrace-all))
         (t
          (warn "don't know how to delete ~S from shop-trace items: ignoring."
                item)))))

(defun print-methods (&optional name (domain *domain*))
  (if name
    (progn (format t "Methods for name ~S are:" name)
           (mapcar #'(lambda (x) (format t "~2%  ~s" x))
                   (methods domain name)))
    (maphash #'(lambda (k defs)
                 (format t "~2%Methods for task ~S are:" k)
                 (dolist (ad  defs)
                   (format t "~2%   ~S" ad)))
             (domain-methods domain))))

;;; this I don't understand: AFAICT from the rest of the code there
;;; can be only ONE operator defined with a particular
;;; name... [2006/07/05:rpg]
(defun print-operators (&optional name (domain *domain*))
  (if name
    (progn (format t "Operator for name ~S is:" name)
           (format t "~2%  ~s" (operator domain name)))
    (maphash #'(lambda (k defs)
                 (format t "~2%Operators for task ~S are:" k)
                 (dolist (ad defs)
                   (format t "~2%   ~S" ad)))
             (domain-operators domain))))

(defun print-operator (operator &optional (stream t))
  (format stream "~THead: ~S~%~@[~TPreconditions: ~S~%~]~@[~TDelete-list: ~S~%~]
                  ~@[~TAdd-list: ~S~%~]~@[~TCost-fun: ~S~%~]"
          (operator-head operator)
          (operator-preconditions operator)
          (operator-deletions operator)
          (operator-additions operator)
          (operator-cost-fun operator)))
