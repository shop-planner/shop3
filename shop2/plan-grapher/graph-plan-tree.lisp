(in-package :spg)

(defun graph-plan-tree (plan-tree &key (tree-processor 'identity) graph-class)
  "Takes a SHOP2 PLAN-TREE as output, and returns a CL-DOT graph object."
  (declare (ignore graph-class))        ; this is an IOU for better using the CL-DOT protocol.
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

