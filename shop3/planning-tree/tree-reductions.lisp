;;; -*- Mode: common-lisp; package: shop2; -*-
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

;;; ------------------------------------------------------------------------
;;; :plan-tree option functions
;;; ------------------------------------------------------------------------

; This function records the parents of each subtask in a reduction.
(defun record-reduction (task1 reduction unifier)
  (declare (ignore unifier))
  (let ((all-subtasks (extract-subtasks reduction)))
    (setf *subtask-parents*
	   (nconc
	    *subtask-parents*
	    (mapcar #'(lambda (subtask) (list subtask task1))
		    all-subtasks)))))

(defun extract-subtasks (reduction)
  (cond
   ((atom reduction) nil)
   ((eq (first reduction) :task) (list reduction))
   (t (append (extract-subtasks (first reduction))
	      (extract-subtasks (rest reduction))))))

; This function records the task atom that produced a given operator
; instance.
(defun record-operator (task1 operator unifier)
  (declare (ignore unifier))
  (setf *operator-tasks*
    (cons
     (list operator task1)
     *operator-tasks*)))
	

; This function is executed at the end of the planning process to produce
;  the final tree.
(defun extract-tree (plan)
  (strip-tree-tags
   (let* ((operator-nodes (plan-operator-nodes plan))
	  (all-nodes (plan-tree-nodes operator-nodes))
	  (root-nodes (node-children nil all-nodes)))
     (mapcar #'(lambda (root-node) (extract-subtree root-node all-nodes))
	     root-nodes))))

(defun strip-tree-tags (tree)
  (cond
   ((atom tree) tree)
   ((and (eq (first tree) :task)
	 (eq (second tree) :immediate))
    (rest (rest tree)))
   ((eq (first tree) :task)
    (rest tree))
   (t
    (cons
     (strip-tree-tags (first tree))
     (strip-tree-tags (rest tree))))))

(defun extract-subtree (root-node nodes)
  (let ((children (node-children root-node nodes)))
    (if children
	(cons root-node
	      (mapcar #'(lambda (child) (extract-subtree child nodes))
		      children))
      root-node)))

(defun node-children (node nodes)
  (remove-if-not
   #'(lambda (other-node) 
       (eq (second (assoc (or (operator-task other-node) other-node)
			  *subtask-parents*))
	   node))
   nodes))

(defun plan-tree-nodes (base-nodes)
  (let* ((extended-base-nodes
	  (remove-duplicates
	   (extend-plan-tree-nodes base-nodes)
	   :from-end t))
	 (new-base-nodes
	  (set-difference extended-base-nodes base-nodes)))
    (if new-base-nodes
	(plan-tree-nodes extended-base-nodes)
      base-nodes)))

(defun extend-plan-tree-nodes (base-nodes)
  (if (null base-nodes) nil
    (let* ((operator-task (operator-task (first base-nodes)))
	   (task (or operator-task (first base-nodes)))
	   (parent (second (assoc task *subtask-parents*)))
	   (rest-nodes (cons (first base-nodes)
			     (extend-plan-tree-nodes (rest base-nodes)))))
      (if parent
	  (cons parent rest-nodes)
	rest-nodes))))

;;; Introduced an OPERATOR-NODE structure as a way of better
;;; understanding the TREE extraction code. [2004/02/05:rpg]
(defstruct (operator-node (:type list))
  cost 
  operator
  position)

;;; I think OPERATOR-TASK here actually applies to an operator NODE...
(defun operator-task (operator-node)
  (second (assoc (operator-node-operator operator-node) *operator-tasks*)))

(defun plan-operator-nodes (plan &optional (n 0))
  "This function translates operator expressions into operator nodes,
assembling together the operator, its the position in the plan and cost."
  (if (null plan) nil
    (cons
     (make-operator-node
      :cost (second plan)
      :operator (first plan)
      :position n)
     (plan-operator-nodes (rest (rest plan)) (1+ n)))))

