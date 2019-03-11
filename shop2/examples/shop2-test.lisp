(in-package :shop-user)

(defun do-plan-tree-tests ()
  (load "site-lisp:shop2;examples;test-plan-tree.lisp")
  (or (test-plan-tree-test)
      (format t "~&TEST-PLAN-TREE-TEST failed.~%")))

(defun do-tree-binding-tests ()
  (load "site-lisp:shop2;examples;tree-binding-problems.lisp")
  (or (tree-binding-problems-test)
      (format t "~&TREE-BINDING-PROBLEMS-TEST failed.~%")))

(defun do-apply-operator-tests ()
  (load "site-lisp:shop2;examples;apply-operator-bug.lisp")
  (or (apply-operator-bug-test)
      (format t "~&APPLY-OPERATOR-BUG-TEST failed.~%")))

(if (and
     (do-plan-tree-tests)
     (do-tree-binding-tests)
     (do-apply-operator-tests))
    (format t "~&All SHOP2 tests succeeded.~%")
    (format t "~&Some SHOP2 test(s) failed.~%"))
  