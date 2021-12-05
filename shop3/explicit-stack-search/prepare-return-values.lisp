(in-package :prepare-return-values)

(declaim (ftype (function
                 (list &key (:plan-tree (or null top-node)) (:bindings t))
                 (values list (or top-node null)))
                prepare-return-values)
         (ftype
          (function (tree-node hash-table t)
                    (values tree-node hash-table &optional))
          phase-one)
         (ftype (function (tree-node hash-table hash-table)
                          (values &optional))
                translate-lookup-table)
         (ftype (function (tree-node hash-table)
                          (values &optional))
                translate-child-links
                translate-dependency-links))


(defun prepare-return-values (plan &key plan-tree bindings)
  "Take the PLAN and PLAN-TREE and create new PLAN and PLAN-TREE objects,
copies of the originals, with the BINDINGS applied."
  (multiple-value-bind (new-plan translation-table)
      (make-plan-copy plan)
    (if plan-tree
        (multiple-value-bind (new-root tree-translation-table)
            (phase-one plan-tree translation-table bindings)
          (declare (type tree-node new-root))
          (translate-lookup-table new-root translation-table tree-translation-table)
          (translate-child-links new-root tree-translation-table)
          ;; translate dependency links, if present
          (translate-dependency-links new-root tree-translation-table)
          (values new-plan new-root))
        ;; much simpler if there is no plan tree...
        (values new-plan nil))))


;;; Make a copy of the original plan-tree, using the translation-table
;;; to rewrite the tasks of the primitive nodes, and using the bindings
;;; and the original tasks to rewrite the other tasks.
(defun phase-one (plan-tree translation-table bindings)
  (let* ((size (hash-table-size (top-node-lookup-table plan-tree)))
         (tree-translation-table (make-hash-table
                                  :test 'eq
                                  :size size))
         (new-lookup-table (make-hash-table
                            :test 'eq
                            :size size))
         new-root)
    (iter (with open = (list plan-tree))
      (while open)
      (for node = (pop open))
      (as new-node = (copy-plan-tree-node node))
      ;; make a new node and index the copy against the original node
      (setf (gethash node tree-translation-table) new-node)
      (when (typep new-node 'top-node)
        (unless (null new-root) (error "Apparently two root nodes in tree."))
        (setf new-root
              new-node
              (top-node-lookup-table new-node)
              new-lookup-table))
      (if (typep node 'primitive-tree-node)
          (progn
            (assert (null (tree-node-expanded-task node)))
           (setf (tree-node-task new-node)
                 (when-let (tsk (tree-node-task node))
                   (gethash tsk translation-table))))
          (setf (tree-node-task new-node)
                (apply-substitution (copy-tree (tree-node-task node)) bindings)
                (tree-node-expanded-task new-node)
                (copy-tree (tree-node-expanded-task node))))
      (unless (typep node 'primitive-tree-node)
        (setf open (append (complex-tree-node-children node) open))))
    (values new-root tree-translation-table)))


;;; the NEW-ROOT is the copy of the plan tree.
;;; the PLAN-TRANSLATION-TABLE is an EQ table that translates tasks in the
;;;   original plan to tasks in the copied plan
;;; the TREE-PLAN-TRANSLATION-TABLE is a hash-table mapping from nodes in
;;;   the original tree to nodes under NEW-ROOT.
;;; The complexity here is necessary because the plan tree is indexed by
;;; *object equality* on the tasks, instead of expression equality, which
;;; is necessary to accommodate the possibility of multiple instances of the
;;; same ground task in one plan.
(defun translate-lookup-table (new-root plan-translation-table tree-plan-translation-table)
  "Update the tree node lookup table in NEW-ROOT using the two translation
hash tables. Done for side-effects: returns nothing."
  (flet ((translate-node (node)
           (declare (type tree-node node))
           (or (gethash node tree-plan-translation-table)
               (error "Tree translation table has no translation for node ~s" node))))
    (declare (ftype (function (tree-node) (values tree-node &optional))
                    translate-node))
    ;; now rewrite the lookup-table
    (iter (with new-table = (top-node-lookup-table new-root))
      (for (old-task old-node) in-hashtable (top-node-lookup-table new-root))
      (as new-node = (translate-node old-node))
      (as new-task = (if (typep old-node 'primitive-tree-node)
                         (or (gethash old-task plan-translation-table)
                             (error "No translation for primitive node ~s" old-task))
                         (tree-node-task new-node)))
      (as new-expanded-task =
          (unless (typep old-node 'primitive-tree-node)
            (tree-node-expanded-task new-node)))
      (declare (type hash-table new-table)
               (type (or null tree-node) old-node new-node)
               (type list old-task new-task))
      (setf (gethash new-task new-table) new-node)
      (when new-expanded-task (setf (gethash new-expanded-task new-table) new-node)))
    (values)))

(defun translate-child-links (new-root tree-translation-table)
  (flet ((translate-node (node)
           (declare (type tree-node node))
           (or (gethash node tree-translation-table)
               (error "Tree translation table has no translation for node ~s" node))))
    (declare (ftype (function (tree-node) (values tree-node &optional))
                    translate-node))
  (labels ((iter (node)
             (unless (typep node 'primitive-tree-node)
               (let ((new-children
                       (mapcar #'translate-node (complex-tree-node-children node))))
                 (setf (complex-tree-node-children node) new-children)
                 (mapc #'(lambda (x) (setf (tree-node-parent x) node))
                       new-children)
                 (mapc #'iter new-children)))))
    (iter new-root)))
  (values))

(defun translate-dependency-links (new-root tree-translation-table)
  (flet ((translate-node (node)
           (declare (type tree-node node))
           (or (gethash node tree-translation-table)
               (error "Tree translation table has no translation for node ~s" node))))
    (declare (ftype (function (tree-node) (values tree-node &optional))
                    translate-node))
    (flet ((translate-dependency (d)
             (make-dependency
              :establisher (translate-node (establisher d))
              :consumer (translate-node (consumer d))
              :prop (prop d))))
    (declare (ftype (function (dependency) (values dependency &optional))
                    translate-dependency))
      (labels ((iter (node)
                 (setf (tree-node-dependencies node)
                       (mapcar #'translate-dependency (tree-node-dependencies node)))
                 (unless (typep node 'primitive-tree-node)
                   (dolist (c (complex-tree-node-children node))
                     (iter c)))))
        (iter new-root))))
  (values))
