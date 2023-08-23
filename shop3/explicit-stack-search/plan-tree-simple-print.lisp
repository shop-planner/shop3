(in-package :plan-tree)

(defparameter *output-form* nil)

(defmethod print-node ((plan-tree-node top-node))
  (let ((*output-form* nil))
    (iter
     (for node in (complex-tree-node-children plan-tree-node))
     (format t "~%Node: ~s" node)
     (print-node node))
    (format t "~%Plan tree: ~s" *output-form*)))
  
(defmethod print-node ((plan-tree-node complex-tree-node))
  (let ((dependencies (tree-node-dependencies plan-tree-node))
        output-dependency-form
        output-children-form)
    (iter
     (for d in dependencies)
     (push
      `(:establisher
        ((:task
          ,(if (eq (establisher d) :init)
               :init
               (tree-node-task (establisher d))))
         (:type ,(type-keyw (type-of (establisher d)))))
        :label ,(prop d))
      output-dependency-form))

    (iter
     (with children = (complex-tree-node-children plan-tree-node))
     (while children)
     (as node = (pop children))
     (format t "~% Child node: ~s" node)
     ;; What about :UNORDERED???
     (if (eql (type-of node) 'plan-tree::ordered-tree-node)
       (setf children (append
                       (complex-tree-node-children node)
                       children))
       ;; else
       (push
        `(:task ,(tree-node-task node)
          :type ,(type-keyw (type-of node)))
        output-children-form)
       ))
     
    (when (tree-node-task plan-tree-node)
      (push
       `(:task ,(tree-node-task plan-tree-node)
         :type ,(type-keyw (type-of plan-tree-node))
         :children ,output-children-form
         :depends-on ,output-dependency-form)
       *output-form*))

    (iter
     (for node in (complex-tree-node-children plan-tree-node))
     (print-node node))))


(defmethod print-node ((plan-tree-node primitive-tree-node))
  (let ((dependencies (tree-node-dependencies plan-tree-node))
        output-dependency-form)
    (iter
     (for d in dependencies)
     (push
      `(:establisher
        ((:task
          ,(if (eq (establisher d) :init)
               :init
               (tree-node-task (establisher d))))
         (:type ,(type-keyw (type-of (establisher d)))))
        :label ,(prop d))
      output-dependency-form))

    ;; there is no children since this node is primitive.

    (when (tree-node-task plan-tree-node)
      (push
       `(:task ,(tree-node-task plan-tree-node)
         :type :primitive
         :depends-on ,output-dependency-form)
       *output-form*))))

(defmethod print-node ((plan-tree-node ordered-tree-node))
  (iter
   (for node in (complex-tree-node-children plan-tree-node))
   (format t "~%Node in ordered: ~s" node)
   (print-node node)))

(defmethod print-node ((plan-tree-node unordered-tree-node))
  (format t "~%Unordered CHILDREN: ~A"
          (complex-tree-node-children plan-tree-node)))

(defun type-keyw (node-type)
  (ecase node-type
    (primitive-tree-node
     :primitive)
    (complex-tree-node
     :complex)
    (t (format t "~%Add this type to the list: ~s" node-type))))
#|
    (plan-tree:primitive-tree-node
     (unless (ignored-primitive-node-p tree)
       (incf *tree-leaf-index*)
       (dolist (partial-constraint partial-constraints)
         (declare (type list partial-constraint))
         (let ((vars (find-primitive-node-variables tree)))

           (assert (eq '<= (tree-partial-constraint-op (car partial-constraint))))
           (assert (eq '<= (tree-partial-constraint-op (cdr partial-constraint))))

           (new-constraint-with-properties *constraint-db*
                                           (lte-constraint (tree-partial-constraint-lhs (car partial-constraint))
                                                           (tachyon/internal/csp-problem-def/structures:node-variables-start-min vars))
                                           :bound :min
                                           :purpose 'tree-constraint)
           (new-constraint-with-properties *constraint-db*
                                           (lte-constraint (tree-partial-constraint-lhs (cdr partial-constraint))
                                                           (tachyon/internal/csp-problem-def/structures:node-variables-start-max vars))
                                           :bound :max
                                           :purpose 'tree-constraint)))))

    (t (error 'unhandled-tree-node-error :node tree))))

|#
