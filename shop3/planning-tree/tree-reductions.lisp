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


(in-package :shop)

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
  (declare (ignore unifier))            ; TASK1 and OPERATOR should be ground.
  (setf (gethash operator *operator-tasks*) task1
        (gethash task1 *task-operator*) operator))

; This function is executed at the end of the planning process to produce
;  the final tree.

(defvar *node-children-table*)
(declaim (type hash-table *node-children-table*))

(defun extract-tree (plan)
  (strip-tree-tags
   (let* ((operator-nodes (plan-operator-nodes plan))
          ;; all-nodes are either operator-nodes or complex tasks
          (all-nodes (plan-tree-nodes operator-nodes))
          (*node-children-table* (create-node-children-table *subtask-parents* all-nodes operator-nodes))
          (root-tasks (node-children nil *node-children-table*)))
     (mapcar #'(lambda (root-node) (extract-subtree root-node all-nodes))
             root-tasks))))

;;; FIXME: Rewrite to use tree-node accessors
;;; Also rewrite to destructively modify -- this will
;;; do a lot of copying.
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
  (let ((children (node-children root-node *node-children-table*)))
    (cond
      (children
       (make-complex-node root-node
                          (mapcar #'(lambda (child) (extract-subtree child nodes))
                                  children)))
      ((primitive-node-p root-node)
       root-node)
      (t
       (make-complex-node root-node nil)))))

(defun node-children (node &optional (children-table *node-children-table*))
  "Find all the nodes in NODES whose parent is NODE."
  (if (primitive-node-p node) nil
   (gethash node children-table)))

(declaim (ftype (function (hash-table list list) (values hash-table &optional))))
(defun create-node-children-table (subtask-parents-table all-nodes all-primitive-nodes)
  "Build and return a hash-table of nodes' children using the SUBTASK-PARENTS-TABLE
and the lists of ALL-NODES and ALL-PRIMITIVE-NODES. Elements of ALL-NODES
are *either* PRIMITIVE-NODEs or TASKS (lists) for complex tasks."
  (let ((new-table (make-hash-table :test 'eq
                                    :size (hash-table-size subtask-parents-table))))
    (iter (for (task parent) in-hashtable subtask-parents-table)
      ;; FIXME remove this if it pans out
      (assert (not (primitive-node-p task)))
      ;; need to translate the task back to operator...
      (as decoded-task = (decode-task task all-primitive-nodes))
      (declare (type (or null primitive-node cons) decoded-task))
      (when decoded-task
        ;; preserve order...
        (alexandria:nconcf
         (gethash parent new-table)
         (list decoded-task))))
    (setf (gethash nil new-table)
          (all-roots all-nodes subtask-parents-table))
    new-table))

(defun all-roots (all-nodes subtask-parents-table)
  "Return the list of nodes in ALL-NODES that don't have parents.
Parents are determined by the contents of the SUBTASK-PARENTS-TABLE.
As in many other places, the elements of ALL-NODES are either PRIMITIVE-NODEs
or tasks (s-expressions)."
  (iter (for task in all-nodes)
    ;; add all the root nodes
    (unless (or (primitive-node-p task) (gethash task subtask-parents-table))
      (collect task))))

(defun decode-task (task all-primitive-nodes)
  "Take a TASK s-expression and return either: (a) NIL if it corresponds to a
no-op ('SHOP::!!INOP), (b) the corresponding primitive-node, if the task is a
primitive one, or (c) TASK itself."
  (cond ((eq (task-name task) 'shop::!!INOP) ; unpleasant special case
         nil)
        ((shop::primitivep (task-name task))
         (let* ((entry (or (gethash task *task-operator*)
                           (error "Could not find operator for ~A in *TASK-OPERATOR* table"
                                  task)))
                (prim-node
                  (find entry all-primitive-nodes
                        :key #'(lambda (x) (primitive-node-task x))
                        :test 'eq)))
           (declare (type (or null primitive-node) prim-node))
           (or
            prim-node)
           (error "Unable to find primitive node for task ~A" task)))
        (t
         task)))

;;; the name of this function is misleading, because it does *not*
;;; return a list of nodes. Instead it returns a list that contains
;;; primitive-nodes and tasks (corresponding to complex-nodes).
;;; If the base-nodes are ordered, then the result should be ordered.
(defun plan-tree-nodes (base-nodes)
  (let* ((extended-base-nodes
           (remove-duplicates
            (extend-plan-tree-nodes base-nodes)
            :from-end t))
         (new-base-nodes
           (iter (for n in extended-base-nodes)
             (unless (member n base-nodes)
               (return t))
             (finally (return nil)))
           ;; wasteful way to get a boolean
           ;;(set-difference extended-base-nodes base-nodes)
           ))
    (if new-base-nodes
        (plan-tree-nodes extended-base-nodes)
        base-nodes)))

;;; returns a list of elements which are either PRIMITIVE-NODEs
;;; or non-primitive tasks (which later will be transformed
;;; into COMPLEX-NODEs)
(defun extend-plan-tree-nodes (base-nodes &optional acc)
  (if (null base-nodes) (nreverse acc)  ; preserve order
      (let* ((node  (first base-nodes))
             (task (or (when (primitive-node-p  node)
                         (operator-task node))
                       node))
             ;; parent will be a task expression here, and not a node.
             (parent (gethash task *subtask-parents*)))
        ;; (format t "~&Node is ~A.~%Task is ~A.~%Parent is ~A." node task parent)
        ;; (unless parent (break "Couldn't find parent node"))
        (if parent
            (extend-plan-tree-nodes (rest base-nodes) (cons parent (cons node acc)))
            (extend-plan-tree-nodes (rest base-nodes) (cons node acc))))))

;;; this function is necessary because the operators are not EQ
;;; to their tasks, which must be looked up in *operator-tasks*
(declaim (ftype (function (primitive-node) (values list &optional))))
(defun operator-task (operator-node)
  ;; (declare (type primitive-node operator-node))
  (or (gethash (primitive-node-task operator-node) *operator-tasks*)
      (error "Unable to find the task for primitive tree node ~a"
             operator-node)))

(defun plan-operator-nodes (plan &optional (n 0) acc)
  "This function translates operator expressions into operator nodes,
assembling together the operator, its the position in the plan and cost."
  (declare (optimize space speed))
  (if (null plan) (nreverse acc)
      (progn
        ;; make sure we have a plan instead of a SHORTER-PLAN
        (assert (numberp (first (rest plan))))
        (plan-operator-nodes
         ;; skip the costs
         (rest (rest plan))
         (1+ n)
         (cons
          (make-primitive-node
           (second plan)
           (first plan)
           n)
          acc)))))

(declaim (ftype (function () (values hash-table &optional))
                make-subtask-parents-table
                make-operator-task-table))
(defun make-subtask-parents-table ()
  (make-hash-table :test 'eq))

(defun make-operator-task-table ()
  (make-hash-table :test 'eq))
