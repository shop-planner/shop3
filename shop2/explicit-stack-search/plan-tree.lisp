(defpackage plan-tree
  (:use common-lisp)
  (:export #:dependency
           #:establisher
           #:consumer
           #:prop
           #:tree-node
           #:tree-node-task
           #:tree-node-dependencies
           #:primitive-tree-node
           #:make-primitive-tree-node
           #:complex-tree-node
           #:make-complex-tree-node
           #:complex-tree-node-children
           #:unordered-tree-node
           #:make-unordered-tree-node
           #:make-ordered-tree-node
           #:ordered-tree-node
           ))

(in-package :plan-tree)

(defstruct (dependency (:conc-name nil))
  establisher
  consumer
  prop
  )

;;; this is an "abstract" class and should never be directly instantiated --
;;; only primitive-tree-node and complext-tree-node should be instantiated.
(defstruct tree-node
  task
  dependencies
  )

(defstruct (primitive-tree-node (:include tree-node))
  )

(defstruct (complex-tree-node (:include tree-node))
  children
  )

;;; maybe should revise this and have complex-tree-node as mixin, since
;;; ordered-tree-node and unordered-tree-node have neither TASK nor
;;; DEPENDENCIES.
(defstruct (ordered-tree-node (:include complex-tree-node)))

(defstruct (unordered-tree-node (:include complex-tree-node)))

           
