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

(defstruct (complex-node (:type list)
                         (:constructor make-complex-node (task children)))
  task
  children)

(defun complex-node-p (tree-node)
  "Is TREE-NODE a representation of a complex node (i.e., not an operator) in
the SHOP2 tree format as described in SHOP2?"
  (and (listp (first tree-node))
       (symbolp (first (first tree-node)))))

(deftype complex-node ()
  '(satisfies complex-node-p))

(defun remove-internal-operators (complex-node)
  "Returns a new complex-node like the original, but with any
children that are internal operators (primitive nodes) removed."
  (assert (complex-node-p complex-node))
  (make-complex-node (tree-node-task complex-node)
                     (iter (for child in (complex-node-children complex-node))
                       (etypecase child
                         (complex-node (collecting child))
                         (primitive-node (unless (internal-operator-p
                                                  (task-name
                                                   (primitive-node-task child)))
                                           (collecting child)))))))

;;; Introduced an OPERATOR-NODE structure as a way of better
;;; understanding the TREE extraction code. [2004/02/05:rpg]
(defstruct (primitive-node (:type list)
                           (:constructor make-primitive-node (cost task position)))
  cost
  task
  position)

;; (defun make-primitive-node (task cost position)
;;   (list cost task position))

(defun primitive-node-p (tree-node)
    "Is TREE-NODE a representation of a primitive node (i.e., an operator) in
the SHOP2 tree format as described in SHOP2?"
  (and (= (length tree-node) 3)
       (numberp (primitive-node-cost tree-node))
       (integerp (primitive-node-position tree-node))
       (listp (primitive-node-task tree-node))))

(deftype primitive-node ()
  '(satisfies primitive-node-p))

;; (defun primitive-node-task (tree-node)
;;   "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
;; Returns the corresponding TASK s-expression."
;;   (second tree-node))

;; (defun primitive-node-cost (tree-node)
;;   "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
;; Returns the corresponding cost (number)."
;;   (first tree-node))

;; (defun primitive-node-position (tree-node)
;;   "TREE-NODE must be a PRIMITIVE-NODE (cf. PRIMITIVE-NODE-P).
;; Returns the corresponding plan sequence position (integer)."
;;   (third tree-node))


(deftype tree-node ()
  '(or complex-node primitive-node))

(defun tree-node-task (tree-node)
  (cond ((primitive-node-p tree-node) (primitive-node-task tree-node))
        ((complex-node-p tree-node)   (complex-node-task tree-node))
        (t (error "Not a valid SHOP tree node."))))

(defsetf tree-node-task (tree-node) (value)
  `(etypecase ,tree-node
    (primitive-node
     (setf (primitive-node-task ,tree-node) ,value))
    (complex-node
     (setf (complex-node-task ,tree-node) ,value))))


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

(defun find-complex-node-if (fun tree-list &key (node-fun nil))
  "If `NODE-FUN` is nil return a complex node whose *task* (first element)
satisfies `FUN`.  If `NODE-FUN` is non-`NIL`, then return a complex node
that satisfies `FUN`.  Takes a list of trees (list of nodes) as input.
Won't work properly on a true tree."
  (labels ((list-iter (lst)
             (unless (null lst)
               (or (node-iter (first lst))
                   (list-iter (cdr lst)))))
           (node-iter (node)
             (when (complex-node-p node)
;;             (format t "Complex node head: ~S~%" (first node))
               (if node-fun
                (if (funcall fun node) node
                    (list-iter (complex-node-children node)))
                (if (funcall fun (complex-node-task node)) node
                    (list-iter (complex-node-children node)))))))
    ;; top level i
    (assert (every #'(lambda (x) (or (primitive-node-p x) (complex-node-p x))) tree-list))
    (list-iter tree-list)))

(defun all-primitive-nodes (tree)
  (find-all-primitive-nodes-if #'identity tree))

(defun find-primitive-node-if (fun tree)
  "Return A primitive node whose TASK satisfies FUN.  When
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


(defun find-all-primitive-nodes-if (fun tree)
  "Return a list of all primitive nodes whose TASK satisfies FUN."
  (labels ((list-iter (lst acc)
             (if (null lst)
                 acc
                 (let ((all-found
                         (node-iter (first lst))))
                   (if all-found
                       ;; probably this could be NREVERSE...
                       (reverse (list-iter (rest lst)
                                           (append all-found acc)))
                       (list-iter (rest lst) acc)))))
           (node-iter (node)
             ;; MUST check for primitive node first, because a primitive node may
             ;; satisfy complex-node-p, which is not a good check
             ;; and can't be, because of the format.
             (cond ((primitive-node-p node)
                    (when (funcall fun (primitive-node-task node))
                      (list node)))
                   ((complex-node-p node)
                    (list-iter (complex-node-children node) nil))
                   (t
                    (error 'type-error :expected-type 'tree-node :datum node)))))
    ;; Ugh: SHOP plan trees are really forests. Most of the time.
    (sort
     (if (complex-node-p tree)
         (node-iter tree)
         (list-iter tree nil))
     #'<
     :key #'(lambda (x) (primitive-node-position x)))))

(defun find-primitive-node-for-task (task tree)
  "Return a primitive node whose task matches TASK. When
the node is found, returns two values: the node itself, and its
parent."
  (find-primitive-node-if #'(lambda (node-task) (equalp node-task task)) tree))

(defun find-all-complex-node-if (fun tree)
  "Return a complex node whose TASK satisfies FUN."
  (labels ((list-iter (lst acc)
             (if (null lst)
                 acc
                 (let ((new (node-iter (first lst))))
                   (reverse (list-iter (cdr lst) (append new acc))))))
           (node-iter (node)
             (when (complex-node-p node)
;;             (format t "Complex node head: ~S~%" (first node))
               (let ((new (when (funcall fun (complex-node-task node)) (list node))))
                 (list-iter (complex-node-children node) new)))))
    ;; top level i
    (list-iter tree nil)))

(defun tree-compare (tree1 tree2)
  "Strictly for debugging use -- this very inefficient function pinpoints differences
between trees."
  (labels ((list-iter (lst1 lst2)
             (cond ((null lst1)
                    (if (null lst2)
                        t
                        (format t "~&NIL does not match ~s~%" lst2)))
                   ((null lst2)
                    (format t "~&~s does not match nil~%" lst1))
                   ((equalp lst1 lst2)
                    t)
                   (t
                    (let ((n1 (first lst1))
                          (n2 (first lst2)))
                      (and (node-iter n1 n2)
                           (list-iter (rest lst1) (rest lst2)))))))
           (node-iter (n1 n2)
             (cond ((primitive-node-p n1)
                    (cond ((primitive-node-p n2)
                           (assert (not (equalp n1 n2)))
                           (format t "~&Primitive nodes do not match:~%~T~S~%~T~S~%" n1 n2))))
                   ((primitive-node-p n2)
                    (format t "~&Node:~%~T~S~%~TDoes not match primitive node:~%~T~S~%"  n1 n2))
                   (t (assert (and (complex-node-p n1)
                                   (complex-node-p n2)))
                      (unless (equalp (complex-node-task n1)
                                      (complex-node-task n2))
                        (format t "Mismatching complex-node tasks:~%~T~S~%~T~S~%"
                                (complex-node-task n1)
                                (complex-node-task n2))
                        (return-from node-iter nil))
                      (if (equalp (complex-node-children n1)
                                  (complex-node-children n2))
                          t
                          (list-iter (complex-node-children n1)
                                     (complex-node-children n2)))))))
    (cond ((complex-node-p tree1)
           (if (complex-node-p tree2)
               (node-iter tree1 tree2)
               (format t "~&Tree1 is a single complex node, but tree2 is not.~%")))
          ((complex-node-p tree2)
           (format t "~&Tree1 is a single complex node, but tree1 is not.~%"))
          (t
           (list-iter tree1 tree2)))))


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
plan sequence -- but the nodes will not be EQ to the nodes in the original
tree (although they will be EQUALP."
  (labels ((list-iter (lst)
             (mapcar #'node-iter lst))
           (node-iter (node)
             (cond ((primitive-node-p node)
                    (make-primitive-node (primitive-node-cost node)
                                         (primitive-node-task node)
                                         (primitive-node-position node)))
                   ((complex-node-p node)
                    (make-complex-node (complex-node-task node)
                                       (list-iter (complex-node-children node))))
                   (t
                    (error 'type-error :expected-type 'tree-node :datum node)))))
    ;; Ugh: SHOP plan "trees" are really forests. Most of the time.
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

(defun canonically-order (tree &optional (keep-empty nil))
  (labels ((list-iter (lst)
             (let ((elts
                     (remove nil (mapcar #'node-iter lst))))
               (sort elts #'<= :key #'min-start)))
           (node-iter (node)
             (etypecase node
               (primitive-node node
                (make-primitive-node (primitive-node-cost node)
                                     (primitive-node-task node)
                                     (primitive-node-position node)))
              (complex-node
               (unless (or keep-empty (complex-node-children node))
                (make-complex-node (complex-node-task node)
                                   (list-iter (complex-node-children node))))))))
    ;; Ugh: SHOP plan "trees" are really forests. Most of the time.
    (if (or (primitive-node-p tree) (complex-node-p tree))
        (node-iter tree)
        (list-iter tree))))

(declaim (ftype (function (tree-node) (values fixnum &optional))
                min-start))
(defun min-start (node)
  (etypecase node
   (primitive-node
    (primitive-node-position node))
   (complex-node
    (or
     (iter (for c in (complex-node-children node))
       (minimizing (min-start c)))
     most-positive-fixnum))))
