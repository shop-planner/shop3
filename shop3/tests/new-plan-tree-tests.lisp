;;;---------------------------------------------------------------------------
;;; Tests for the new SHOP plan tree -- the one created by explicit stack
;;; search and used for plan repair. [2024/08/09:rpg]
;;;---------------------------------------------------------------------------
(defpackage :new-plan-tree-tests
  (:use :common-lisp :fiveam)
  (:import-from #:shop
                #:find-plans-stack)
  (:import-from #:plan-tree
                #:make-tree-and-plan
                #:tree-node-task
                #:tree-and-plan-tree
                #:tree-and-plan-plan
                #:map-tree
                #:compare-trees
                ))


(in-package :new-plan-tree-tests)

(def-suite* new-plan-tree-tests)

(defun all-tree-tasks (tree)
  (let (tasks)
    (map-tree #'(lambda (x)
                  (let ((task (tree-node-task x)))
                    (if (symbolp task)
                        (unless (or (null task)
                                    (equalp (symbol-name task)
                                            (symbol-name '#:top)))
                          (error "Unexpected node task: ~s" task))
                        (push (tree-node-task x) tasks))))
              tree)
    (sort tasks #'shop:prop-sorter)))

(defvar *good-tasks* nil)
(defvar *bad-tasks* nil)

(test test-hddl-tree
  (let* ((pr (first (find-plans-stack 'shop3-rovers:roverprob01 :unpack-returns nil :plan-tree t)))
         (pt (make-tree-and-plan :tree (shop:tree pr) :plan (shop:plan pr)))
         (pt2 (eval (make-load-form pt)))
         (tree1 (shop-hddl:hddl-plan (shop:plan pr) (shop:tree pr)))
         (tree2 (shop-hddl:hddl-plan (tree-and-plan-plan pt) (tree-and-plan-tree pt))))
    (is
     (equalp tree1 tree2))
    (is (equalp (all-tree-tasks (shop:tree pr)) (all-tree-tasks (tree-and-plan-tree pt))))
    (is (equalp (all-tree-tasks (tree-and-plan-tree pt))
                (all-tree-tasks (tree-and-plan-tree pt2))))
    (let ((*trace-output* *standard-output*)
          (hddl-translator::*trace-indexer* t))
      (declare (special hddl-translator::*trace-indexer*))
      (trace hddl-translator::task-index)
      (format t "~&Good tasks:~%")
      (pprint (setf *good-tasks* (all-tree-tasks (tree-and-plan-tree pt))))
      (format t "~&Bad tasks:~%")
      (pprint (setf *bad-tasks* (all-tree-tasks (tree-and-plan-tree pt2)))))
    (untrace hddl-translator::task-index)
    (let ((tree3 (shop-hddl:hddl-plan (tree-and-plan-plan pt2) (tree-and-plan-tree pt2)
                                      :if-not-ground :ignore)))
      (is (equalp tree1 tree3)))))

(defmacro compare-tree-helper (expr1 expr2)
  `(multiple-value-bind (matchp mismatch)
                   (compare-trees ,expr1 ,expr2)
               (or matchp
                   (progn
                     (let ((*print-length* 80))
                      (format t "~&Tree mismatch b/w ~s and ~s at nodes:~%~t~a~%~t~a~%"
                              ',expr1 ',expr2
                              (first mismatch) (second mismatch)))
                     (format t "~s:~%" ',expr1)
                     (describe (first mismatch))
                     (format t "~s:~%" ',expr2)
                     (describe (second mismatch))
                     (error
                      ;; format t
                      "~&Tree mismatch b/w pr and pt at nodes: ~a ~a~%"
                      (first mismatch) (second mismatch))))))


(test test-make-load-form
  (let* ((pr (first (find-plans-stack 'shop3-rovers:roverprob01 :unpack-returns nil :plan-tree t)))
         (pt (make-tree-and-plan :tree (shop:tree pr) :plan (shop:plan pr)))
         (pt2 (eval (make-load-form pt))))
    (is
     (equalp (shop:plan pr) (tree-and-plan-plan pt)))
    (is
     (equalp (tree-and-plan-plan pt) (tree-and-plan-plan pt2)))
    (is (equalp (all-tree-tasks (shop:tree pr))
                (all-tree-tasks (tree-and-plan-tree pt))))
    (is (equalp (all-tree-tasks (tree-and-plan-tree pt))
                (all-tree-tasks (tree-and-plan-tree pt2))))
    (is (eq (shop:tree pr) (tree-and-plan-tree pt)))
    (is-true (compare-tree-helper (shop:tree pr) (tree-and-plan-tree pt)))
    (is-true (compare-tree-helper (shop:tree pr) (tree-and-plan-tree pt2)))))
