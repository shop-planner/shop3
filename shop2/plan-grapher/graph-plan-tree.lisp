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

(defun graph-enhanced-plan-tree (plan-tree &key 
                                         (graph-object (make-instance 'enhanced-plan-tree-graph)))
  "Takes a SHOP2 plan forest (PLAN-TREE) as input, and returns a CL-DOT graph object.
Note that the PLAN-TREE name is a misnomer that reflects SHOP2 FIND-PLANS misnomer.
If you ask for plan trees from SHOP2, you really get plan *forests*."
    (cl-dot:generate-graph-from-roots graph-object (list plan-tree)))


(defmethod cl-dot:graph-object-points-to ((g enhanced-plan-tree-graph)(obj plan-tree:complex-tree-node))
  ;; ugh, cl-dot emits these in reverse order, reversing the left-to-right order of the tree.
  (reverse (plan-tree:complex-tree-node-children obj)))

(defmethod cl-dot:graph-object-points-to ((g enhanced-plan-tree-graph)(obj plan-tree:primitive-tree-node))
  nil)

(defmethod cl-dot:graph-object-pointed-to-by ((g enhanced-plan-tree-graph)(obj plan-tree:tree-node))
  (mapcar #'(lambda (x)
              (let ((est (plan-tree:establisher x)))
                (assert (typep est 'plan-tree:primitive-tree-node))
                (make-instance 'cl-dot:attributed
                               :object est
                               :attributes `(:constraint nil :color :blue))))
          (plan-tree:tree-node-dependencies obj)))


(defmethod cl-dot:graph-object-node ((g enhanced-plan-tree-graph) (obj plan-tree:complex-tree-node))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" (plan-tree:tree-node-task obj))
                         :shape :ellipse)))

(defmethod cl-dot:graph-object-node ((g enhanced-plan-tree-graph) (obj plan-tree:primitive-tree-node))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" (plan-tree:tree-node-task obj))
                         :id ,(string (gensym "NODE"))
                         :shape :box)))

(defmethod cl-dot:graph-object-node ((g enhanced-plan-tree-graph) (obj plan-tree:ordered-tree-node))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label "ord" ;;  ,(format nil "~A" (tree-node-task obj))
                         :id ,(string (gensym "NODE"))
                         :shape :rarrow)))

(defmethod cl-dot:graph-object-node ((g enhanced-plan-tree-graph) (obj plan-tree:unordered-tree-node))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label "unord" ;,(format nil "~A" (tree-node-task obj))
                         :shape :diamond)))

