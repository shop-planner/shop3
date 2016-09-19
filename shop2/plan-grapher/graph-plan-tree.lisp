(in-package :spg)

(defun graph-plan-tree (plan-forest &key (tree-processor 'identity)
                                         (graph-object (make-instance 'plan-tree-graph)))
  "Takes a SHOP2 plan forest (PLAN-TREE) as input, and returns a CL-DOT graph object.
Note that the PLAN-TREE name is a misnomer that reflects SHOP2 FIND-PLANS misnomer.
If you ask for plan trees from SHOP2, you really get plan *forests*."
  (let ((modified-forest (mapcar #'(lambda (tree) (funcall tree-processor tree))
                               plan-forest)))
    (cl-dot:generate-graph-from-roots graph-object modified-forest)))

(defmethod cl-dot:graph-object-node ((g plan-tree-graph) (obj cons))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" (tree-node-task obj))
                         :shape ,(if (primitive-node-p obj) :box
                                    :ellipse))))
                         

(defmethod cl-dot:graph-object-points-to ((g plan-tree-graph)(obj cons))
  (declare (ignorable g))
  (when (complex-node-p obj)
    (reverse
     (complex-node-children obj))))

