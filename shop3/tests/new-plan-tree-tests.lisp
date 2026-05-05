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
                #:defdomain
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
                #:all-primitive-nodes)
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
            (remove-if #'internal-task-p (remove-if-not #'primitive-task-p all-tasks)))))
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
        (is-true (every #'is-indexed
                        (all-tree-tasks tree)))
        (is-false unindexed-primitives)))))



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
    (let ((tree3 (shop-hddl:hddl-plan (tree-and-plan-plan pt2) (tree-and-plan-tree pt2)
                                      :if-not-ground :ignore)))
      (is (equalp tree1 tree3)))))


(in-package :shop3-user)

(defmacro new-plan-tree-tests::test-hopping-domain (&body body)
  `(progn
     (let ((*package* (find-package :shop3-user)))
       (defdomain (test-hopping :type pddl-domain :silently t)
           ((:include ste-test-domain ,(asdf:system-relative-pathname "shop3" "tests/st-test-domain.pddl"))

            (:method (win)
              ((:goal (attacker-owns-core-dump ?proc)))
              (:ordered
               ;;(make-mem-block-contents mem-block-core-pattern-buffer-plus-2 corrupt-path)
               (write-corrupt-path-to-cpb corrupt-path)
               (!kill-process ?proc)))

            (:method (write-corrupt-path-to-cpb corrupt-path)
              ()
              (:ordered
               (find-mem-block mem-block-core-pattern-buffer)
               (!compute-mem-block-addr-core-pat-plus-2 mem-block-core-pattern-buffer
                                                        mem-block-core-pattern-buffer-plus-2)
               (make-mem-block-contents mem-block-core-pattern-buffer-plus-2 corrupt-path)))

            (:method (make-mem-block-contents ?mb ?contents)
              mbc-already-done
              (mem-block-contents ?mb ?contents)
              ()
              mb-needs-doing
              ()
              ((find-mem-block ?mb)
               (write-mem-block-contents ?mb ?contents)))

            (:method (write-mem-block-contents ?mb ?contents)
              use-ace2
              ;; if the ?contents is pointer to some memory buffer
              ;; whose address we know, then we can use ACE2
              (and (not (ace2-used))
                   ;; the following will be established by the
                   ;; first task in the task net.
                   ;; (know-addr-of ?mb)
                   ;; the following is failing for the target that is the
                   ;; offset into the
                   (or (ste-data-addr ?mb ?mbptr)
                       (addr ?mb ?mbptr))
                   (pointer ?mbptr)
                   (addr ?omb ?contents)
                   (know-addr-of ?omb))
              (                           ;ordered
               (find-mem-block ?mb)
               (!ace2-set-pointer-to-buffer ?mbptr ?omb ?contents)))

            (:method (write-mem-block-contents ?mb ?contents)
              use-two-sysctl-writes
              (and (sysctl-table-entry ?ste1)
                   (not (used-in-chain ?ste1))
                   (sysctl-table-entry ?ste2)
                   (not (used-in-chain ?ste2))
                   (not (= ?ste1 ?ste2))
                   (ste-data-addr ?ste1 ?ste-addr1)
                   (ste-procname ?ste1 ?procname)
                   (ste-data-addr ?ste2 ?_ste-addr2)
                   (ste-procname ?ste2 ?_procname2)
                   (addr ?mb ?mb-addr)
                   )
              (:ordered (!!assert (used-in-chain ?ste1))
                        (find-mem-block ?ste1)
                        (find-mem-block ?ste2)
                        (write-mem-block-contents ?ste1 ?mb-addr)
                        (!sysctl-write ?procname ?contents ?ste1
                                       ?ste-addr1 ?mb ?mb-addr)))

            (:method (find-mem-block ?mb)
              mb-addr-already-known
              (or (know-addr-of ?mb)
                  (ste-know-addr-of ?mb))
              ()
              mb-needs-finding
              ((mem-block ?mb))
              (!compute-memory-block-addr-using-dmesg ?mb)
              ste-entry-needs-finding
              ((sysctl-table-entry ?mb))
              (!compute-sysctl-table-entry-addr ?mb))

            (:op (!!assert ?x)
             :add (?x))

            (:op (!!query (?x))
             :precond (?x))

            (:op (!!format . ?rest)
             :precond (eval (progn
                              (apply #'format t '?rest)
                              t))))))
    (let ((new-plan-tree-tests::prob (MAKE-PROBLEM '(GP-PROBLEM :domain test-hopping :redefine-ok t)
                             '((ADDR MEM-BLOCK-CORE-PATTERN-BUFFER ADDR4780926)
                               (ADDR MEM-BLOCK-CORE-PATTERN-BUFFER-PLUS-2
                                ADDR-CORE-PAT-PLUS-2)
                               (ADDR MEM-BLOCK-CORE_PIPE_LIMIT_BUFFER ADDR4780930)
                               (ADDR MEM-BLOCK-CORE_USES_PID_BUFFER ADDR4780928)
                               (ADDR MEM-BLOCK-IO_URING_GROUP_BUFFER ADDR4780932)
                               (ADDR MEM-BLOCK-MAX_LOCK_DEPTH_BUFFER ADDR4780934)
                               (ADDR MEM-BLOCK-OOPS_LIMIT_BUFFER ADDR4780936)
                               (ADDR MEM-BLOCK-PANIC_BUFFER ADDR4780940)
                               (ADDR MEM-BLOCK-PANIC_ON_OOPS_BUFFER ADDR4780938)
                               (ADDR MEM-BLOCK-PERF_EVENT_MLOCK_KB_BUFFER ADDR4780942)
                               (ADDR MEM-BLOCK-PERF_EVENT_PARANOID_BUFFER ADDR4780944)
                               (ADDR MEM-BLOCK-PRINTK_BUFFER ADDR4780946)
                               (ADDR MEM-BLOCK-PRINTK_RATELIMIT_BURST_BUFFER
                                ADDR4780948)
                               (ADDR MEM-BLOCK-RANDOMIZE_VA_SPACE_BUFFER ADDR4780950)
                               (ADDR MEM-BLOCK-WARN_LIMIT_BUFFER ADDR4780952)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN
                                ADDR4780925)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT
                                ADDR4780929)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID
                                ADDR4780927)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP
                                ADDR4780931)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH
                                ADDR4780933)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT ADDR4780935)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_PANIC ADDR4780939)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS
                                ADDR4780937)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB
                                ADDR4780941)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID
                                ADDR4780943)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_PRINTK ADDR4780945)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST
                                ADDR4780947)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE
                                ADDR4780949)
                               (ADDR PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT ADDR4780951)
                               (ADDRESS ADDR-CORE-PAT-PLUS-2) (ADDRESS ADDR4780925)
                               (ADDRESS ADDR4780926) (ADDRESS ADDR4780927)
                               (ADDRESS ADDR4780928) (ADDRESS ADDR4780929)
                               (ADDRESS ADDR4780930) (ADDRESS ADDR4780931)
                               (ADDRESS ADDR4780932) (ADDRESS ADDR4780933)
                               (ADDRESS ADDR4780934) (ADDRESS ADDR4780935)
                               (ADDRESS ADDR4780936) (ADDRESS ADDR4780937)
                               (ADDRESS ADDR4780938) (ADDRESS ADDR4780939)
                               (ADDRESS ADDR4780940) (ADDRESS ADDR4780941)
                               (ADDRESS ADDR4780942) (ADDRESS ADDR4780943)
                               (ADDRESS ADDR4780944) (ADDRESS ADDR4780945)
                               (ADDRESS ADDR4780946) (ADDRESS ADDR4780947)
                               (ADDRESS ADDR4780948) (ADDRESS ADDR4780949)
                               (ADDRESS ADDR4780950) (ADDRESS ADDR4780951)
                               (ADDRESS ADDR4780952)
                               (ADDRESSABLE MEM-BLOCK-BUFF-TARGET)
                               (ADDRESSABLE MEM-BLOCK-CORE-PATTERN-BUFFER)
                               (ADDRESSABLE MEM-BLOCK-CORE-PATTERN-BUFFER-PLUS-2)
                               (ADDRESSABLE MEM-BLOCK-CORE_PIPE_LIMIT_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-CORE_USES_PID_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-IO_URING_GROUP_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-MAX_LOCK_DEPTH_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-OOPS_LIMIT_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-PANIC_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-PANIC_ON_OOPS_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-PERF_EVENT_MLOCK_KB_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-PERF_EVENT_PARANOID_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-PRINTK_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-PRINTK_RATELIMIT_BURST_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-RANDOMIZE_VA_SPACE_BUFFER)
                               (ADDRESSABLE MEM-BLOCK-WARN_LIMIT_BUFFER)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_PANIC)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (ADDRESSABLE
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (ADDRESSABLE
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_PRINTK)
                               (ADDRESSABLE
                                PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (ADDRESSABLE
                                PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (ADDRESSABLE PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_PANIC)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_PRINTK)
                               (ADDRESSABLE
                                STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (ADDRESSABLE STE-STRING_KERNEL_DOT_WARN_LIMIT)
                               (DMESG-OBSERVABLE MEM-BLOCK-CORE-PATTERN-BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-CORE_PIPE_LIMIT_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-CORE_USES_PID_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-IO_URING_GROUP_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-MAX_LOCK_DEPTH_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-OOPS_LIMIT_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-PANIC_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-PANIC_ON_OOPS_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-PERF_EVENT_MLOCK_KB_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-PERF_EVENT_PARANOID_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-PRINTK_BUFFER)
                               (DMESG-OBSERVABLE
                                MEM-BLOCK-PRINTK_RATELIMIT_BURST_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-RANDOMIZE_VA_SPACE_BUFFER)
                               (DMESG-OBSERVABLE MEM-BLOCK-WARN_LIMIT_BUFFER)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (DMESG-OBSERVABLE PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (DMESG-OBSERVABLE PTR-STE-STRING_KERNEL_DOT_PANIC)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (DMESG-OBSERVABLE PTR-STE-STRING_KERNEL_DOT_PRINTK)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (DMESG-OBSERVABLE
                                PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (DMESG-OBSERVABLE PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT)
                               (DMESG-RUNNING)
                               (:GOAL (ATTACKER-OWNS-CORE-DUMP PROC-PROC-TARGET))
                               (HAVE-ROOT) (MEM-BLOCK MEM-BLOCK-BUFF-TARGET)
                               (MEM-BLOCK MEM-BLOCK-BUFF-TARGET)
                               (MEM-BLOCK MEM-BLOCK-CORE-PATTERN-BUFFER)
                               (MEM-BLOCK MEM-BLOCK-CORE-PATTERN-BUFFER-PLUS-2)
                               (MEM-BLOCK MEM-BLOCK-CORE_PIPE_LIMIT_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-CORE_PIPE_LIMIT_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-CORE_USES_PID_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-CORE_USES_PID_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-IO_URING_GROUP_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-IO_URING_GROUP_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-MAX_LOCK_DEPTH_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-MAX_LOCK_DEPTH_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-OOPS_LIMIT_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-OOPS_LIMIT_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PANIC_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PANIC_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PANIC_ON_OOPS_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PANIC_ON_OOPS_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PERF_EVENT_MLOCK_KB_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PERF_EVENT_MLOCK_KB_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PERF_EVENT_PARANOID_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PERF_EVENT_PARANOID_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PRINTK_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PRINTK_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PRINTK_RATELIMIT_BURST_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-PRINTK_RATELIMIT_BURST_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-RANDOMIZE_VA_SPACE_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-RANDOMIZE_VA_SPACE_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-WARN_LIMIT_BUFFER)
                               (MEM-BLOCK MEM-BLOCK-WARN_LIMIT_BUFFER)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_PANIC)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (MEM-BLOCK
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (MEM-BLOCK
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_PRINTK)
                               (MEM-BLOCK
                                PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (MEM-BLOCK
                                PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (MEM-BLOCK PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-BUFF-TARGET TARGET-MEM)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-CORE-PATTERN-BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS
                                MEM-BLOCK-CORE-PATTERN-BUFFER-PLUS-2 EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-CORE_PIPE_LIMIT_BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-CORE_USES_PID_BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-IO_URING_GROUP_BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-MAX_LOCK_DEPTH_BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-OOPS_LIMIT_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-PANIC_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-PANIC_ON_OOPS_BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS
                                MEM-BLOCK-PERF_EVENT_MLOCK_KB_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS
                                MEM-BLOCK-PERF_EVENT_PARANOID_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-PRINTK_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS
                                MEM-BLOCK-PRINTK_RATELIMIT_BURST_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-RANDOMIZE_VA_SPACE_BUFFER
                                EMPTY)
                               (MEM-BLOCK-CONTENTS MEM-BLOCK-WARN_LIMIT_BUFFER EMPTY)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN ADDR4780926)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT ADDR4780930)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID ADDR4780928)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP ADDR4780932)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH ADDR4780934)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT ADDR4780936)
                               (MEM-BLOCK-CONTENTS PTR-STE-STRING_KERNEL_DOT_PANIC
                                ADDR4780940)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS ADDR4780938)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB
                                ADDR4780942)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID
                                ADDR4780944)
                               (MEM-BLOCK-CONTENTS PTR-STE-STRING_KERNEL_DOT_PRINTK
                                ADDR4780946)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST
                                ADDR4780948)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE
                                ADDR4780950)
                               (MEM-BLOCK-CONTENTS
                                PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT ADDR4780952)
                               (OBJECT EMPTY) (OBJECT TARGET-MEM)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_PANIC)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_PRINTK)
                               (POINTER
                                PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (POINTER PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT)
                               (PROC-CONTAINS PROC-PROC-TARGET MEM-BLOCK-BUFF-TARGET)
                               (PROC-STATE PROC-PROC-TARGET PS-RUNNING)
                               (PROCESS PROC-PROC-TARGET)
                               (PROCNAME STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (PROCNAME STRING_KERNEL_DOT_CORE_USES_PID)
                               (PROCNAME STRING_KERNEL_DOT_IO_URING_GROUP)
                               (PROCNAME STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (PROCNAME STRING_KERNEL_DOT_OOPS_LIMIT)
                               (PROCNAME STRING_KERNEL_DOT_PANIC)
                               (PROCNAME STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (PROCNAME STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (PROCNAME STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (PROCNAME STRING_KERNEL_DOT_PRINTK)
                               (PROCNAME STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (PROCNAME STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (PROCNAME STRING_KERNEL_DOT_WARN_LIMIT)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_CORE_PATTERN
                                PTR-STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT
                                PTR-STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_CORE_USES_PID
                                PTR-STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_IO_URING_GROUP
                                PTR-STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH
                                PTR-STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_OOPS_LIMIT
                                PTR-STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_PANIC
                                PTR-STE-STRING_KERNEL_DOT_PANIC)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_PANIC_ON_OOPS
                                PTR-STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (STE-DATA-ADDR
                                STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (STE-DATA-ADDR
                                STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID
                                PTR-STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_PRINTK
                                PTR-STE-STRING_KERNEL_DOT_PRINTK)
                               (STE-DATA-ADDR
                                STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST
                                PTR-STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE
                                PTR-STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (STE-DATA-ADDR STE-STRING_KERNEL_DOT_WARN_LIMIT
                                PTR-STE-STRING_KERNEL_DOT_WARN_LIMIT)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_CORE_PATTERN
                                STRING_KERNEL_DOT_CORE_PATTERN)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT
                                STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_CORE_USES_PID
                                STRING_KERNEL_DOT_CORE_USES_PID)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_IO_URING_GROUP
                                STRING_KERNEL_DOT_IO_URING_GROUP)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH
                                STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_OOPS_LIMIT
                                STRING_KERNEL_DOT_OOPS_LIMIT)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_PANIC
                                STRING_KERNEL_DOT_PANIC)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_PANIC_ON_OOPS
                                STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB
                                STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID
                                STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_PRINTK
                                STRING_KERNEL_DOT_PRINTK)
                               (STE-PROCNAME
                                STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST
                                STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE
                                STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (STE-PROCNAME STE-STRING_KERNEL_DOT_WARN_LIMIT
                                STRING_KERNEL_DOT_WARN_LIMIT)
                               (SYSCTL-TABLE-ENTRY STE-STRING_KERNEL_DOT_CORE_PATTERN)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_CORE_PIPE_LIMIT)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_CORE_USES_PID)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_IO_URING_GROUP)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_MAX_LOCK_DEPTH)
                               (SYSCTL-TABLE-ENTRY STE-STRING_KERNEL_DOT_OOPS_LIMIT)
                               (SYSCTL-TABLE-ENTRY STE-STRING_KERNEL_DOT_PANIC)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_PANIC_ON_OOPS)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_PERF_EVENT_MLOCK_KB)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_PERF_EVENT_PARANOID)
                               (SYSCTL-TABLE-ENTRY STE-STRING_KERNEL_DOT_PRINTK)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_PRINTK_RATELIMIT_BURST)
                               (SYSCTL-TABLE-ENTRY
                                STE-STRING_KERNEL_DOT_RANDOMIZE_VA_SPACE)
                               (SYSCTL-TABLE-ENTRY STE-STRING_KERNEL_DOT_WARN_LIMIT))
                             '(:TASK WIN))))
    ,@body)))

(in-package :new-plan-tree-tests)

(defun check-single-plan-tree-internal (tree plan)
  (let* ((plan-no-costs (shop:remove-costs plan))
         (primitive-tasks
           (remove-if #'(lambda (x) (eq (task-name x) 'shop::!!inop))
                      (mapcar #'(lambda (n) (or (plan-tree:tree-node-task n)
                                                (plan-tree:tree-node-expanded-task n)))
                              (all-primitive-nodes tree)))))
    (is (typep tree 'plan-tree:top-node))
    (is (= 1 (length (plan-tree:complex-tree-node-children tree))))
    (is (equalp '(shop3-user::win)
                (plan-tree:tree-node-task (first (plan-tree:complex-tree-node-children tree)))))
    (is (alexandria:set-equal plan-no-costs
                              primitive-tasks
                              :test 'equalp)
        "Plan is not set-equal to the set of primitive nodes of the tree:~%
 Actions not in the tree:~%~s~%
 Tree nodes not in the plan:~%~s~%"
        (set-difference plan-no-costs primitive-tasks :test #'equalp)
        (set-difference primitive-tasks plan-no-costs :test #'equalp))))

(test check-single-plan-tree ;; 8 checks
  (test-hopping-domain
    ;; finding just one plan
    (multiple-value-bind (plans trees states)
        (find-plans-stack prob :plan-tree t :which :first)
      (5am:is-true plans)
      (5am:is-true trees)
      (5am:is-true states)
      (is (= (length plans) (length trees) (length states) 1))
      (check-single-plan-tree-internal (first trees) (first plans)))))

(test check-all-plan-trees ; 680 checks
  (test-hopping-domain
    ;; finding all the plans
    (multiple-value-bind (plans trees states)
        (find-plans-stack prob :plan-tree t :which :all)
      (5am:is-true plans)
      (5am:is-true trees)
      (5am:is-true states)
      (is (= 169 (length plans) (length trees) (length states))
          "There are ~d plans ~d trees and ~d states"
           (length plans) (length trees) (length states))
      (mapcar #'check-single-plan-tree-internal trees plans))))

(test check-multiple-plan-trees ; 12 checks
  (test-hopping-domain
    ;; finding more than one, but less than all plans
    (multiple-value-bind (plans trees states)
        (find-plans-stack prob :plan-tree t :which :first :plan-num-limit 2)
      (5am:is-true plans)
      (5am:is-true trees)
      (5am:is-true states)
      (is (= 2 (length plans) (length trees) (length states))
          "There are ~d plans ~d trees and ~d states"
           (length plans) (length trees) (length states))
      (mapcar #'check-single-plan-tree-internal trees plans))))
