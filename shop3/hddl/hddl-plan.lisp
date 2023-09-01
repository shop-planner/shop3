(in-package :hddl-translator)

(defun hddl-plan (plan tree)
  "Take a SHOP PLAN and TREE (really a forest) as input and produce an
HDDL plan encoded as an s-expression."
  (multiple-value-bind (indexed-plan task-indices)
      (index-shop-plan (shop:remove-costs plan))
    (let ((next-index (1+ (caar (last indexed-plan))))
          (root-tasks (forest-roots tree))
          roots decompositions)
      (setf roots
            (iter (for root in root-tasks)
              (setf (gethash root task-indices) next-index)
              (collecting next-index)
              (incf next-index)))

      `(:hddl-plan
        :actions ,indexed-plan
        :roots ,roots
        :decompositions ,decompositions
        ))))

(declaim (ftype (function (list) (values list hash-table &optional))
                index-shop-plan))
(defun index-shop-plan (action-list)
  (let ((hash-table (make-hash-table :test 'eq))
        (assoc-table (pairlis
                      (alexandria:iota (length action-list))
                      action-list)))
    (iter (for (i . act) in assoc-table)
      (setf (gethash act hash-table) i))
    (values assoc-table hash-table)))

(defun forest-roots (plan-tree)
  (mapcar #'shop:tree-node-task plan-tree))
