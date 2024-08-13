;;;---------------------------------------------------------------------------
;;; Tests for the new SHOP plan tree -- the one created by explicit stack
;;; search and used for plan repair. [2024/08/09:rpg]
;;;---------------------------------------------------------------------------
(defpackage :new-plan-tree-tests
  (:use :common-lisp :fiveam)
  (:import-from #:shop
                #:find-plans-stack
                #:shorter-plan
                #:task-name
                #:internal-operator-p)
  (:import-from #:plan-tree
                #:make-tree-and-plan
                #:tree-node-task
                #:tree-and-plan-tree
                #:tree-and-plan-plan
                #:map-tree
                #:compare-trees
                #:tree-node-expanded-task
                #:primitive-tree-node-p
                )
  (:import-from #:hddl-translator
                #:*task-indices*
                #:*next-index*
   ))


(in-package :new-plan-tree-tests)

(def-suite* new-plan-tree-tests)

(defun all-tree-tasks (tree)
  (let (tasks)
    (map-tree #'(lambda (x)
                  (let ((task (tree-node-expanded-task x)))
                    (cond
                      ((symbolp task)
                       (unless (or (null task)
                                   (equalp (symbol-name task)
                                           (symbol-name '#:top)))
                         (error "Unexpected node task: ~s" task)))
                      ((primitive-tree-node-p x)
                       (push (tree-node-expanded-task x) tasks)))))
              tree)
    (sort tasks #'shop:prop-sorter)))

(defvar *good-tasks* nil)
(defvar *bad-tasks* nil)

(test test-hddl-plan-indexing           ; 5 checks
  (let* ((pr (first (find-plans-stack 'shop3-rovers:roverprob01 :unpack-returns nil :plan-tree t)))
         (plan (shop:plan pr))
         (shorter (shorter-plan plan))
         (tree (shop:tree pr))
         (*next-index* 1)
         (*task-indices* (make-hash-table :test 'eq))
         (indexed-plan (hddl-translator::index-shop-plan shorter))
         (root-tasks (hddl-translator::forest-roots tree))
         (all-tasks (all-tree-tasks tree)))
    (flet ((is-indexed (x)
             (or (shop:internal-operator-p (task-name x))
                 (gethash x *task-indices*)))
           (primitive-task-p (x)
             (shop::primitivep
              (task-name x)))
           (internal-task-p (x)
             (internal-operator-p
              (task-name x))))
    (is (equalp (alexandria:iota (length shorter) :start 1)
                (mapcar #'car indexed-plan)))
    (is (equalp shorter
                (mapcar #'cdr indexed-plan)))
    (is (= (length shorter)
           (length
            (remove-if #'internal-task-p (remove-if-not #'primitive-task-p all-tasks))))
        ;; "Number of primitive tasks in tree (~d) not equal to number of actions in plan (~d).~%Plan:~%~{~t~s~%~}~%Tree tasks:~%~{~t~s~%~}"
        ;; (length
        ;;     (remove-if-not #'primitive-task-p all-tasks))
        ;; (length shorter)
        ;; (sort (copy-list shorter) #'shop3cmn::prop-sorter)
        ;; (sort (remove-if-not #'primitive-task-p all-tasks) #'shop3cmn::prop-sorter)
        )
    (is (equalp '((shop-rovers::achieve-goals))
                root-tasks))
    (let ((root-indices (mapcar #'hddl-translator::task-index root-tasks)))
      (is (= 1 (length root-indices))))
    (hddl-translator::index-plan-tree tree :error)
      (let* ((unindexed (remove-if #'is-indexed
                                  (all-tree-tasks tree)))
             (unindexed-primitives (remove-if-not #'(lambda (x)
                                                      (and
                                                       (primitive-task-p x)
                                                       (not (internal-task-p x))))
                                                  unindexed)))
        ;; (format t "~&UN-indexed tasks:~%")
        ;; (pprint unindexed)
        ;; (format t "~&~d unindexed primitive tasks.~%"
        ;;         (length unindexed-primitives))
        ;; (format t "Not in plan:~%~{~t~s~%~}"
        ;;         (remove-if #'(lambda (x) (member x shorter))
        ;;                    unindexed-primitives))
        ;; (format t "Not in plan by equality:~%~{~t~s~%~}"
        ;;         (remove-if #'(lambda (x) (member x shorter :test #'equalp))
        ;;                    unindexed-primitives))
        )
      (is-true (every #'is-indexed
                      (all-tree-tasks tree))))
    ;; (hddl-translator::generate-decompositions tree :error nil 0)
    ))



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

(test test-make-load-form ; 7 checks
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

(test test-expanded-tasks ; 5 checks
  ;; Verify that the expanded tasks are the correct tasks to use.
  (let* ((pr (first (find-plans-stack 'shop3-rovers:roverprob01 :unpack-returns nil :plan-tree t)))
         (tree (shop:tree pr))
         (plan (shop:plan pr))
         (primitive-nodes (plan-tree:find-all-tree-nodes-if #'plan-tree::primitive-tree-node-p tree))
         (real-complex-nodes (plan-tree:find-all-tree-nodes-if #'(lambda (x)
                                                                   (and (plan-tree::complex-tree-node-p x)
                                                                        (not (plan-tree::pseudo-node-p x))
                                                                        (not (plan-tree::top-node-p x))))
                                                               tree)))
    (is-true (every #'(lambda (x) (tree-node-expanded-task x)) primitive-nodes))
    (is-false (some #'(lambda (x) (eq (plan-tree:tree-node-task x) (tree-node-expanded-task x))) primitive-nodes))
    (is-true (every #'(lambda (x) (tree-node-expanded-task x)) real-complex-nodes))
    (is-false (some #'(lambda (x) (eq (plan-tree:tree-node-task x) (tree-node-expanded-task x))) real-complex-nodes))
    (is-true (alexandria:set-equal (shorter-plan plan)
                                   (mapcar #'(lambda (x) (tree-node-expanded-task x))
                                           (remove-if #'(lambda (n) (internal-operator-p (task-name (tree-node-expanded-task n))))
                                                      primitive-nodes))))))

(test test-hddl-tree ; 4 checks
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
    ;; (let ((*trace-output* *standard-output*)
    ;;       (hddl-translator::*trace-indexer* t))
    ;;   (declare (special hddl-translator::*trace-indexer*))
    ;;   (trace hddl-translator::task-index)
    ;;   (format t "~&Good tasks:~%")
    ;;   (pprint (setf *good-tasks* (all-tree-tasks (tree-and-plan-tree pt))))
    ;;   (format t "~&Bad tasks:~%")
    ;;   (pprint (setf *bad-tasks* (all-tree-tasks (tree-and-plan-tree pt2)))))
    ;; (untrace hddl-translator::task-index)
    (let ((tree3 (shop-hddl:hddl-plan (tree-and-plan-plan pt2) (tree-and-plan-tree pt2)
                                      :if-not-ground :ignore)))
      (is (equalp tree1 tree3)))))
