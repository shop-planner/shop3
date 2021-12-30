(in-package :shop-user)

;;;---------------------------------------------------------------------------
;;; This first test domain is written in SHOP2 notation, which I
;;; honestly have a hard time figuring out how to work with soundly.
;;; FIXME: I will add a version with PDDL operators, instead.
;;;---------------------------------------------------------------------------

(defdomain test-flaw-detection
  (
   (:method (task0)
     m0
     ()
     (:ordered (task1) (task2)))

   (:method (task1)
     m1
     ()
     (:ordered (!op1) (task3)))

   (:method (task2)
     m2
     ((a))
     (:ordered (task4) (task5)))

   (:method (task3)
     m3
     ((a))
     ((!op2)))

   (:method (task4)
     m4
     ()
     (:ordered (!op3) (!op4)))

   (:method (task5)
     m5
     ((not (d)))
     (!op5))

   (:op (!op1)
        :precond ((c))
        :add ((a) (b)))

   (:op (!op2)
        :precond ((b))
        :delete ((c) (d)))

   (:op (!op3)
        :precond ((b)))

   (:op (!op4)
        :precond ((not (c))))

   (:op (!op5)
         :precond ((not (c)) (b)))))

(defproblem find-flaw-test-problem test-flaw-detection
  ((c)(d)) ; initial state
  ((task0))                             ; top level task
   )

#|
(defparameter *res*
  (multiple-value-list
   (find-plans-stack 'find-flaw-test-problem :plan-tree t)))
(defparameter *plan* (shorter-plan (caar *res*)))

(shop3-minimal-subtree:find-failed-task 'test-flaw-detection (caar *res*) (first (second *res*)) (list (first *plan*)) '((:delete (b))))
;;; should return the node for (!op2)
|#

(fiveam:def-suite* minimal-subtree-tests)

(fiveam:test find-flaw-shop2-tests
  (let* ((res (multiple-value-list
               (find-plans-stack 'find-flaw-test-problem :plan-tree t :verbose 0)))
         (plan-tree (first (second res)))
         (plan (remove-costs (first (first res))))
         (hash-table (first (third res))))
    (flet ((find-failed-task (prefix divergence)
             (shop3-minimal-subtree:find-failed-task 'test-flaw-detection
                                                     plan plan-tree
                                                     prefix divergence
                                                     :plan-tree-hash hash-table))
           (complex-parent (tree-node)
             (loop :while (or (typep tree-node 'plan-tree:primitive-tree-node)
                              (typep tree-node 'plan-tree:pseudo-node))
                   :do (setf tree-node (plan-tree:tree-node-parent tree-node))
                   :finally (return tree-node))))
      (macrolet ((flaw-finding-check (expected prefix divergence)
                   `(let ((expected-parent-task
                            (complex-parent
                             (plan-tree:find-task-in-tree ,expected
                                                          hash-table plan-tree))))
                      (multiple-value-bind (replan-parent failed-action)
                           (find-failed-task ,prefix ',divergence)
                        (fiveam:is
                         (eq ,expected failed-action))
                        (fiveam:is
                         (eq expected-parent-task replan-parent))))))
        (flaw-finding-check (second plan) (subseq plan 0 1) ((:delete (b))))
        (flaw-finding-check (third plan) (subseq plan 0 2) ((:delete (b))))
        (flaw-finding-check (first plan) nil ((:delete (c))))
        (flaw-finding-check (fourth plan) (subseq plan 0 3) ((:add (c))))
        (flaw-finding-check (fifth plan) (subseq plan 0 4) ((:add (c))))
#|       (fiveam:is
        (eq
         (plan-tree:find-task-in-tree (second plan) ; OP2
                                      hash-table plan-tree)
         (find-failed-task (subseq plan 0 1) '((:delete (b)))))))
      (fiveam:is
       (eq
        (plan-tree:find-task-in-tree (third plan) ; OP3
                                     hash-table plan-tree)
        (find-failed-task (subseq plan 0 2) '((:delete (b))))))
      (fiveam:is
       (eq
        (plan-tree:find-task-in-tree (first plan) ; OP1
                                     hash-table plan-tree)
        (find-failed-task nil '((:delete (c))))))
      (fiveam:is
       (eq
        (plan-tree:find-task-in-tree (fourth plan) ; OP4
                                     hash-table plan-tree)
        (find-failed-task (subseq plan 0 3) '((:add (c))))))
      (fiveam:is
       (eq
        (plan-tree:find-task-in-tree (fifth plan) ; OP5
                                     hash-table plan-tree)
        (find-failed-task (subseq plan 0 4) '((:add (c))))))
      (fiveam:is
       (eq
        (plan-tree:find-tree-node-if
         #'(lambda (node)
             (equalp (plan-tree:tree-node-task node) '(task3)))
         plan-tree)
        (find-failed-task (subseq plan 0 1) '((:delete (a))))))|#
      (fiveam:is
       (eq
        (plan-tree:find-tree-node-if
         #'(lambda (node)
             (equalp (plan-tree:tree-node-task node) '(task2)))
         plan-tree)
        (find-failed-task (subseq plan 0 2) '((:delete (a))))))
      (fiveam:is
       (eq
        (plan-tree:find-tree-node-if
         #'(lambda (node)
             (equalp (plan-tree:tree-node-task node) '(task5)))
         plan-tree)
        (find-failed-task (subseq plan 0 2) '((:add (d))))))))))


;;; check to make sure that variable bindings will be reflected in the
;;; enhanced plan tree.
(fiveam:def-fixture tree-construction-fixture ()
  (progn
   (defdomain check-tree-construction
       (
        (:method (foo ?x ?y)
          ((parent ?x ?y))
          ((!give-allowance ?x ?y)))

        (:op (!give-allowance ?x ?y)
         :add ((has ?y (dollars 10)))
         :delete ((has ?x (dollars 10))))))
   (defproblem tree-construction-problem
     check-tree-construction
     ((has robert (dollars 10))
      (parent robert phoebe))
     (foo ?x ?y))
   (&body)))


(fiveam:def-suite* enhanced-plan-tree)

(fiveam:test test-tree-unify-upwards
  (fiveam:with-fixture tree-construction-fixture ()
   (multiple-value-bind (plans trees)
       (find-plans-stack 'tree-construction-problem :plan-tree t)
     (fiveam:is-true (first plans))
     (let ((tree (first trees)))
       (fiveam:is
        (equalp '(foo robert phoebe)
                (plan-tree:tree-node-expanded-task
                 (first (plan-tree:complex-tree-node-children tree)))))))))
