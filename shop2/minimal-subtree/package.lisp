(in-package :common-lisp-user)

(defpackage shop2-minimal-subtree
  (:nicknames #:subtree)
  (:shadowing-import-from #:plan-tree #:tree-node-task #:tree-node)
  (:use common-lisp iterate shop2 plan-tree))
