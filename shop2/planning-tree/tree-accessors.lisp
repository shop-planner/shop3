;;; -*- Mode: common-lisp; package: shop; -*-
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
;;; This code developed by Robert P. Goldman.
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
(in-package :shop)

(defun complex-node-p (tree-node)
  "Is TREE-NODE a representation of a complex node (i.e., not an operator) in 
the SHOP2 tree format as described in SHOP2?"
  (listp (first tree-node)))

(defun complex-node-task (tree-node)
  "TREE-NODE must be a COMPLEX-NODE (cf. COMPLEX-NODE-P).
Returns the corresponding TASK s-expression."
  (first tree-node))

(defun complex-node-children (tree-node)
  "TREE-NODE must be a COMPLEX-NODE (cf. COMPLEX-NODE-P).
Returns its children."
  (rest tree-node))

(defun (setf complex-node-children) (value tree-node)
  (assert (complex-node-p tree-node))
  (setf (cdr tree-node) value))

(defun make-complex-node (task children)
  (cons task children))

(defun remove-internal-operators (complex-node)
  "Returns a new complex-node like the original, but with any 
children that are internal operators (primitive nodes) removed."
  (assert (complex-node-p complex-node))
  (make-complex-node (tree-node-task complex-node)
		     (loop for child in (complex-node-children complex-node)
			 if (complex-node-p child)
			 collect child
			 else unless (internal-operator-p
				      (tree-node-task-name child))
			 collect child)))

(defun primitive-node-p (tree-node)
    "Is TREE-NODE a representation of a primitive node (i.e., an operator) in 
the SHOP2 tree format as described in SHOP2?"
  (and (= (length tree-node) 3)
       (numberp (first tree-node))))

(defun primitive-node-task (tree-node)
  "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
Returns the corresponding TASK s-expression."
  (second tree-node))

(defun tree-node-task (tree-node)
  (cond ((primitive-node-p tree-node) (primitive-node-task tree-node))
	((complex-node-p tree-node)   (complex-node-task tree-node))
	(t (error "Not a valid SHOP2 tree node."))))

(defun tree-node-task-name (tree-node)
  (task-name (tree-node-task tree-node)))

(defun task-name (task)
  (case (first task)
    (:task (task-name (rest task)))
    (:immediate (second task))
    (otherwise (first task))))

(defun task-args (task)
  (let ((task-pos (position (task-name task) task)))
    (rest (nthcdr task-pos task))))

(defun find-complex-node-if (fun tree)
  "Return a complex node whose TASK (first element)
satisfies FUN."
  (labels ((list-iter (lst)
	     (unless (null lst)
	       (or (node-iter (first lst))
		   (list-iter (cdr lst)))))
	   (node-iter (node)
	     (when (complex-node-p node)
;;	       (format t "Complex node head: ~S~%" (first node))
	       (if (funcall fun (first node)) node
		   (list-iter (cdr node))))))
    ;; top level i
    (list-iter tree)))

(defun find-all-complex-node-if (fun tree)
  "Return a complex node whose TASK (first element)
satisfies FUN."
  (labels ((list-iter (lst acc)
	     (if (null lst)
		 acc
		 (let ((new (node-iter (first lst))))
		   (reverse (list-iter (cdr lst) (append new acc))))))
	   (node-iter (node)
	     (when (complex-node-p node)
;;	       (format t "Complex node head: ~S~%" (first node))
	       (let ((new (when (funcall fun (first node)) (list node))))
		 (list-iter (cdr node) new)))))
    ;; top level i
    (list-iter tree nil)))

(defun find-complex-node-for-task (task-name tree)
  "Find a complex-node in the TREE whose task name is TASK."
  (find-complex-node-if
   #'(lambda (task)
       (eq (first task) task-name))
   tree))

(defun find-all-complex-node-for-task (task-name tree)
  "Find all complex-nodes in the TREE whose task name is TASK."
  (find-all-complex-node-if
   #'(lambda (task)
       (eq (first task) task-name))
   tree))



