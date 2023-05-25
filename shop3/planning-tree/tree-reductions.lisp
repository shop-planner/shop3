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
    (iter (for subtask in all-subtasks)
      (setf (gethash subtask *subtask-parents*)
            task1))))

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
  (setf (gethash operator *operator-tasks*) task1))


; This function is executed at the end of the planning process to produce
;  the final tree.
(defun extract-tree (plan)
  (strip-tree-tags
   (let* ((operator-nodes (plan-operator-nodes plan))
          ;; all-nodes are either operator-nodes or complex tasks
          (all-nodes (plan-tree-nodes operator-nodes))
          (root-tasks (node-children nil all-nodes)))
     (mapcar #'(lambda (root-node) (extract-subtree root-node all-nodes))
             root-tasks))))

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
  "Recursively build the subtree below ROOT-NODE from the
set of possible nodes in NODES.  At the top level, it returns
a COMPLEX-NODE if ROOT-NODE is a complex task or ROOT-NODE if
ROOT-NODE is a PRIMITIVE-NODE."
  (let ((children (node-children root-node nodes)))
    (cond
      (children
       (make-complex-node root-node
                          (mapcar #'(lambda (child) (extract-subtree child nodes))
                                  children)))
      ((primitive-node-p root-node)
       root-node)
      (t
       (make-complex-node root-node nil)))))

;;; this is done bottom-up because the children of NODE will change
;;; depending on which PLAN we are considering (i.e., which call to
;;; EXTRACT-TREE we are in." [2023/05/25:rpg]
(defun node-children (node nodes)
  "Find all the nodes in NODES whose parent is NODE."
  (remove-if-not
   #'(lambda (other-node)
       (eq (gethash
            ;; other-node willj either be a primitive-node or a
            ;; task (and not a node at all). [2023/05/25:rpg]
            (if (primitive-node-p other-node)
                (operator-task other-node)
                other-node)
            *subtask-parents*)
           node))
   nodes))

;;; the name of this function is misleading, because it does *not*
;;; return a list of nodes. Instead it returns a list that contains
;;; primitive-nodes and tasks (corresponding to complex-nodes).
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
  ;; this returns a list of nodes which are either operator-nodes
  (if (null base-nodes) nil
      (let* ((node-task (tree-node-task (first base-nodes)))
             (task (or node-task (first base-nodes)))
             ;; parent will be a task expression here, and not a node.
             (parent (second (gethash task *subtask-parents*)))
             (rest-nodes (cons (first base-nodes)
                               (extend-plan-tree-nodes (rest base-nodes)))))
        (if parent
            (cons parent rest-nodes)
            rest-nodes))))

;;; this function is necessary because the operators are not EQ
;;; to their tasks, which must be looked up in *operator-tasks*
(defun operator-task (operator-node)
  ;; (declare (type primitive-node operator-node))
  (gethash (primitive-node-operator operator-node) *operator-tasks*))

(defun plan-operator-nodes (plan &optional (n 0) acc)
  "This function translates operator expressions into operator nodes,
assembling together the operator, its the position in the plan and cost."
  (declare (optimize space speed))
  (if (null plan) (nreverse acc)
      (plan-operator-nodes (rest (rest plan)) (1+ n)
                           (cons
                            (make-primitive-node
                             :cost (second plan)
                             :operator (first plan)
                             :position n)
                            acc))))

(declaim (ftype (function () (values hash-table &optional))
                make-subtask-parents-table
                make-operator-task-table))
(defun make-subtask-parents-table ()
  (make-hash-table :test 'eq))

(defun make-operator-task-table ()
  (make-hash-table :test 'eq))
