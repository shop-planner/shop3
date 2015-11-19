(in-package :spg)

(defun graph-plan-tree (plan-tree &key (tree-processor 'identity))
  (let ((modified-tree (funcall tree-processor plan-tree)))
    (cl-dot:generate-graph modified-tree)))

(defmethod cl-dot:object-node ((obj cons))
  ;; SHOP2 plan trees are lists...
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" (tree-node-task obj))
                         :shape ,(if (primitive-node-p obj) :box
                                    :ellipse))))
                         

(defmethod cl-dot:object-points-to ((obj cons))
  (when (complex-node-p obj)
    (reverse
     (complex-node-children obj))))

