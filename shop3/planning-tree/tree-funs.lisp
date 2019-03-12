;;; -*- Mode: common-lisp; package: common-lisp-user; -*-
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
;;; The developer of this SHOP2 augmentation is SIFT, LLC.
;;; Contributors:  Robert P. Goldman
;;;
;;; Portions created by Dr. Goldman are Copyright (C)
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
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;; Contains code for building and extracting trees in SHOP2.    
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2004/02/06:rpg] Created.
;;;
;;;---------------------------------------------------------------------------

(in-package :common-lisp-user)

(defclass tree-store ()
     ((subtask-parents
       :initarg :subtask-parents
       :accessor subtask-parents
       :initform nil
       )
      (operator-tasks
       :initarg :operator-tasks
       :accessor operator-tasks
       :initform nil
       )
      (unifier
       :initarg :unifier
       :accessor unifier
       :initform nil
       )
      ;; new attempt to make an OK plan tree
      (task-indices
       :initarg :task-indices
       :accessor task-indices
       :type list
       :initform nil
       )
      (task-children
       :initarg :task-children
       :accessor task-children
       :type list
       :initform nil
       )
      (root-list
       :initarg :root-list
       :accessor root-list
       :initform nil
       )
      )
  (:documentation "This object can be used to store information
we will later use to create a tree representation of a SHOP2
plan."))

(defmethod shop-tree ((ts tree-store) plan)
  "Extract an instantiated plan tree from the TREE-STORE, using PLAN."
  (apply-substitution (extract-shop-tree ts plan) (unifier ts)))

(defmethod extract-shop-tree ((ts tree-store) plan)
  "Extract a skeletal plan tree from the TREE-STORE, using PLAN.
The returned plan tree still needs to undergo substitution from the
unifier in TS."
  ;; this code lifted from original SHOP2, with some modifications
  ;; [2004/02/06:rpg]
  (strip-tree-tags
   (let* ((operator-nodes (plan-operator-nodes plan))
	  (all-nodes (shop-tree-plan-tree-nodes ts operator-nodes))
	  (root-nodes (shop-tree-node-children ts nil all-nodes)))
     (mapcar #'(lambda (root-node) (shop-tree-extract-subtree ts root-node all-nodes))
	     root-nodes))))

(defmethod shop-tree-plan-tree-nodes ((ts tree-store) base-nodes)
  "Pull plan tree nodes out of TREE-STORE TS searching up the tree from
BASE-NODES."
  (let* ((extended-base-nodes
	  (remove-duplicates
	   (shop-tree-extend-plan-tree-nodes ts base-nodes)
	   :from-end t))
	 (new-base-nodes
	  (set-difference extended-base-nodes base-nodes)))
    (if new-base-nodes
	(shop-tree-plan-tree-nodes ts extended-base-nodes)
      base-nodes)))


(defmethod shop-tree-extend-plan-tree-nodes ((ts tree-store) base-nodes)
  "Service function for SHOP-TREE-PLAN-TREE-NODES, qv."
  (if (null base-nodes) nil
    (let* ((operator-task (shop-tree-operator-task ts (first base-nodes)))
	   (task (or operator-task (first base-nodes)))
	   (parent (second (assoc task (subtask-parents ts))))
	   (rest-nodes (cons (first base-nodes)
			     (shop-tree-extend-plan-tree-nodes ts (rest base-nodes)))))
      (if parent
	  (cons parent rest-nodes)
	  rest-nodes))))

(defmethod shop-tree-node-children ((ts tree-store) node nodes)
  (remove-if-not
   #'(lambda (other-node) 
       (eq (second (assoc (or (shop-tree-operator-task ts other-node) other-node)
			  (subtask-parents ts)))
	   node))
   nodes))

(defmethod shop-tree-extract-subtree ((ts tree-store)root-node nodes)
  (let ((children (shop-tree-node-children ts root-node nodes)))
    (if children
	(cons root-node
	      (mapcar #'(lambda (child) (shop-tree-extract-subtree ts child nodes))
		      children))
	root-node)))


(defmethod shop-tree-operator-task ((ts tree-store) operator-node)
  "Extract a operator corresponding to OPERATOR-NODE from TS."
  (second (assoc (operator-node-operator operator-node)
		 (operator-tasks ts))))

(defmethod add-operator-task ((ts tree-store) task operator
			      unifier)
  "Return a NEW TREE-STORE object derived from TS by adding the
results of applying OPERATOR to satisfy TASK."
  (make-instance 'tree-store
    :operator-tasks (cons
		     (list operator task)
		     (operator-tasks ts))
    :subtask-parents (subtask-parents ts)
    :unifier (append (unifier ts) unifier)
    ;; for new attempt
    :root-list (root-list ts)
    :task-indices (task-indices ts)
    :task-children (task-children ts)))


(defun iota (start end)
  "Returns a list of the numbers from START (inclusive) to
END (exclusive).  So (iota 1 3) => (1 2)."
  (assert (and (integerp start)
	       (integerp end)
	       (< start end)))
  (loop for i from start below end
      collect i))

(defmethod add-reduction ((ts tree-store) task reduction
			  unifier)
  "Return a NEW TREE-STORE object derived from TS by adding the
results of reducing TASK to REDUCTION."
  (let* ((all-subtasks (extract-subtasks reduction))
	 (new-start (length (task-children ts)))
	 (task-index (cdr (assoc task (task-indices ts))))
	 (new-task-children (copy-list (task-children ts)))
	 (new-root-list (root-list ts))
	 new-index-entries)
    (when (null task-index)
      ;; TASK should be a top-level task
      (setf task-index new-start)
      (setf new-root-list (cons task-index new-root-list))
      (push (cons task task-index) new-index-entries)
      (setf new-task-children (nconc new-task-children (list nil)))
      (incf new-start))
    (let ((new-children-list (iota new-start (+ new-start (length all-subtasks)))))
      (setf new-index-entries
	    (nconc new-index-entries
		   (loop for i from new-start
			 for subtask in all-subtasks
			 collect (cons subtask i))))
      (setf new-task-children (nconc new-task-children
				     (make-list (length all-subtasks) :initial-element nil)))
      ;; can't ASSERT this because TASK might be a top-level task
      ;; (assert (integerp task-index))
      (setf (nth task-index new-task-children) (cons task new-children-list))
      (make-instance 'tree-store
	:operator-tasks (operator-tasks ts)
	:subtask-parents
	(append (subtask-parents ts)
		(mapcar #'(lambda (subtask) (list subtask task))
			all-subtasks))
	:unifier (append (unifier ts) unifier)
	;; for new attempt
	:root-list new-root-list
	:task-indices (append (task-indices ts) new-index-entries)
	:task-children new-task-children))))

      

(defun empty-tree ()
  "Generate an empty TREE-STORE object to start planner search with."
  (make-instance 'tree-store))

;;;---------------------------------------------------------------------------
;;; New code from third attempt... [2004/02/10:rpg]
;;;---------------------------------------------------------------------------


(defclass new-tree-store ()
     ((subtask-parents
       :initarg :subtask-parents
       :accessor subtask-parents
       )
      (task-indices
       :initarg :task-indices
       :accessor task-indices
       )
      (operator-tasks
       :initarg :operator-tasks
       :accessor operator-tasks
       )
      (unifier
       :initarg :unifier
       :accessor unifier
       )
      )
  )

(defmethod copy-instance ((nts new-tree-store)
			  &key (subtask-parents nil subtask-parents-supplied)
			       (task-indices nil task-indices-supplied)
			       (operator-tasks nil operator-tasks-supplied)
			       (unifier nil unifier-supplied))
  (make-instance 'new-tree-store
    :subtask-parents (if subtask-parents-supplied subtask-parents
		       (subtask-parents nts))
    :unifier (if unifier-supplied unifier
	       (unifier nts))
    :operator-tasks (if operator-tasks-supplied operator-tasks
		      (operator-tasks nts))
    :task-indices (if task-indices-supplied task-indices
		    (task-indices nts))))

(defun tasks-from-task-net (L)
  (if (null L)
      nil
    (ecase (car L)
      ((:unordered :ordered)
       ;; an embedded task list, so recursively call the function
       ;; on each element and append the answers
       (mapcan #'tasks-from-task-net (cdr L)))
      (:task
       ;; a simple task, so return a list containing the task
       (list L)))))

(defun record-initial-task-net (task-list)
  (loop for task in (tasks-from-task-net task-list)
      for i from 0
      collect (cons task i) into alist
      collect (cons task nil) into subtask-parents
      finally (return (make-instance 'new-tree-store
			:unifier nil
			:operator-tasks nil
			:task-indices alist
			:subtask-parents subtask-parents))))

(defmethod task-index ((ts new-tree-store) task)
  (let ((entry (cdr (assoc task (task-indices ts)))))
    (assert (integerp entry))
    entry))
				  
(defmethod add-operator-task ((ts new-tree-store) task operator
			      unifier)
  "Return a NEW TREE-STORE object derived from TS by adding the
results of applying OPERATOR to satisfy TASK."
  (copy-instance ts 
    :operator-tasks (cons
		     (list operator task)
		     (operator-tasks ts))
    :unifier (append (unifier ts) unifier)))

(defmethod add-assocs ((ts new-tree-store) new-index-assocs)
  (copy-instance ts
		 :task-indices (append (task-indices ts) new-index-assocs)))
  
(defmethod new-assocs ((tree new-tree-store) old-tasks new-tasks)
  "OLD-TASKS and NEW-TASKS should be parallel lists of tasks,
the OLD-TASKS already reflected in TS and the NEW-TASKS possibly not."
  (loop for ot in old-tasks
      for nt in new-tasks
      with old-index
      unless (eq ot nt)
      do (setf old-index (task-index tree ot))
      and collect (cons nt old-index) into new-assocs
	finally (return (add-assocs tree new-assocs))))

(defmethod add-reduction ((ts new-tree-store) task reduction
			  unifier)
  "Return a NEW TREE-STORE object derived from TS by adding the
results of reducing TASK to REDUCTION."
  (let* ((all-subtasks (extract-subtasks reduction))
	 (new-start (length (subtask-parents ts)))
	 (task-index (cdr (assoc task (task-indices ts))))
	 (new-index-entries
	  (loop for i from new-start
	      for subtask in all-subtasks
	      collect (cons subtask i)))
	 (new-parent-entries
	  (loop for subtask in all-subtasks
	      collect (cons subtask task-index))))
    ;; this case should not arise, because I should have all the root
    ;; tasks initialized by record-initial-task-net

;;;    (when (null task-index)
;;;      ;; TASK should be a top-level task
;;;      (setf task-index new-start)
;;;      (setf new-root-list (cons task-index new-root-list))
;;;      (push (cons task task-index) new-index-entries)
;;;      (setf new-task-children (nconc new-task-children (list nil)))
;;;      (incf new-start))

    (assert (integerp task-index))
    (copy-instance ts
		   :operator-tasks (operator-tasks ts)
		   :subtask-parents
		   (append (subtask-parents ts)
			   new-parent-entries)
		   :unifier (append (unifier ts) unifier)
		   :task-indices (append (task-indices ts) new-index-entries))))

