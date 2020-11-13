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

;;; Smart Information Flow Technologies Copyright 2006-2007, 2016
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
  (and (listp (first tree-node))
       (symbolp (first (first tree-node)))))

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

(defun make-primitive-node (task cost position)
  (list cost task position))

(defun primitive-node-p (tree-node)
    "Is TREE-NODE a representation of a primitive node (i.e., an operator) in
the SHOP2 tree format as described in SHOP2?"
  (and (= (length tree-node) 3)
       (numberp (first tree-node))
       (numberp (third tree-node))
       (listp (second tree-node))))

(defun primitive-node-task (tree-node)
  "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
Returns the corresponding TASK s-expression."
  (second tree-node))

(defun primitive-node-cost (tree-node)
  "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
Returns the corresponding cost (number)."
  (first tree-node))

(defun primitive-node-position (tree-node)
  "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
Returns the corresponding plan sequence position (integer)."
  (third tree-node))


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

(defun find-complex-node-if (fun tree &key (node-fun nil))
  "If `NODE-FUN` is nil return a complex node whose *task* (first element)
satisfies `FUN`.  If `NODE-FUN` is non-`NIL`, then return a complex node
that satisfies `FUN`."
  (labels ((list-iter (lst)
             (unless (null lst)
               (or (node-iter (first lst))
                   (list-iter (cdr lst)))))
           (node-iter (node)
             (when (complex-node-p node)
;;             (format t "Complex node head: ~S~%" (first node))
               (if node-fun
                (if (funcall fun node) node
                    (list-iter (cdr node)))   
                (if (funcall fun (first node)) node
                    (list-iter (cdr node)))))))
    ;; top level i
    (list-iter tree)))

(defun find-primitive-node-if (fun tree)
  "Return a primitive node whose TASK satisfies FUN.  When
the node is found, returns two values: the node itself, and its
parent."
  (labels ((list-iter (lst parent)
             (unless (null lst)
               (multiple-value-bind (found-node found-parent)
                   (node-iter (first lst) parent)
                 (if found-node
                     (values found-node found-parent)
                     (list-iter (rest lst) parent)))))
           (node-iter (node parent)
             ;; MUST check for primitive node first, because a primitive node may
             ;; satisfy complex-node-p, which is not a good check
             ;; and can't be, because of the format.
             (cond ((primitive-node-p node)
                    (when (funcall fun (primitive-node-task node)) (values node parent)))
                   ((complex-node-p node)
                    (list-iter (complex-node-children node) node))
                   (t
                    (error 'type-error :expected-type 'tree-node :datum node)))))
    ;; Ugh: SHOP plan trees are really forests. Most of the time.
    (if (complex-node-p tree)
        (node-iter tree nil)
        (list-iter tree nil))))

(defun find-primitive-node-for-task (task tree)
  "Return a primitive node whose task matches TASK. When
the node is found, returns two values: the node itself, and its
parent."
  (find-primitive-node-if #'(lambda (node-task) (equalp node-task task)) tree))

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
;;             (format t "Complex node head: ~S~%" (first node))
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

(defun copy-plan-tree (tree)
  "Return a functional copy of TREE. By \"functional\" we mean satisfies the
key tree property -- the primitive tasks are EQ to the primitive tasks in the 
plan sequence."
  (labels ((list-iter (lst)
             (mapcar #'node-iter lst))
           (node-iter (node)
             (cond ((primitive-node-p node)
                    (make-primitive-node (primitive-node-task node)
                                         (primitive-node-cost node)
                                         (primitive-node-position node)))
                   ((complex-node-p node)
                    (make-complex-node (complex-node-task node)
                                       (list-iter (complex-node-children node))))
                   (t
                    (error 'type-error :expected-type 'tree-node :datum node)))))
    ;; Ugh: SHOP plan trees are really forests. Most of the time.
    (cond ((primitive-node-p tree)
           (node-iter tree))
          ((complex-node-p tree)
           (node-iter tree))
          (t (list-iter tree)))))

(defun node-parent (node tree)
  "Find `NODE`'s parent in `TREE`. Returns `NIL` if nothing found."
  (flet ((match-if-child (n)
           (member node (complex-node-children n) :test 'eq)))
    (find-complex-node-if #'match-if-child tree :node-fun t)))
