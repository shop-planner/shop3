;;; -*- Mode: common-lisp; package: shop2; -*-

(in-package :shop2)

(defvar *enhanced-plan-tree*
  nil
  "Do we build a dependency-enhanced plan tree?")
(defvar *no-dependencies*
  NIL
  "When building an ENHANCED-PLAN-TREE, do not record  causal links.  Defaults to NIL.")

(defgeneric unfold-loop-task (domain state)
  (:documentation "Driver for the looping tasks."))

(defun find-plans-stack (problem &key domain (verbose 0) plan-tree (gc *gc*)
                                 (no-dependencies nil)
                                 repairable
                                 (out-stream t))
  "Top level search function for explicit-state search in SHOP2.
Does not support the full range of options supported by SHOP2: only
supports finding the first solution to PROBLEM.  To comply with SHOP2,
though, always returns a list of plans.
  If the PLAN-TREE keyword argument is non-NIL, will return an enhanced plan
tree, with causal links, unless NO-DEPENDENCIES is non-NIL."
  #+(or ccl allegro sbcl clisp abcl ecl)
  (when gc #+allegro (excl:gc t)
        #+sbcl (sb-ext:gc :full t)
        #+ccl (ccl:gc)
        #+clisp (ext:gc)
        #+cmucl (extensions:gc :full t)
        #+abcl (extensions:gc)
        #+lispworks (hcl:gc-generation t) ; add case for lispworks 5/1/13 BWM
        #+ecl (ext:gc t)
        )
  #-(or :cCL :allegro :sbcl clisp cmucl abcl lispworks ecl)
  (when gc (cerror "Just continue, skip GC."
                   "Requested GC before planning, but do not know how to request GC for this lisp implementation (see source code)."))
  (let* ((start-run-time (get-internal-run-time))
         (start-real-time (get-internal-real-time))
         (*plan-tree* nil)
         (*plans-found* nil)
         (*enhanced-plan-tree* plan-tree)
         (*no-dependencies* no-dependencies)
         (*record-dependencies-p* (and *enhanced-plan-tree* (not *no-dependencies*)))
         (*verbose* verbose)
         (problem (find-problem problem t))
         (domain (cond (domain
                        (etypecase domain
                          (symbol
                           (find-domain domain :error))
                          (domain domain)))
                       ((domain-name problem)
                        (find-domain (domain-name problem) :error))
                       (*domain* *domain*)
                       (t
                        (error "Domain not supplied and problem does not specify domain."))))
         (*domain* domain)
         (world-state (apply 'make-initial-state domain
                             (default-state-type domain)
                             (problem->state domain problem)))
         (tasks (get-tasks problem))
         (search-state (make-instance 'search-state
                                      :world-state world-state
                                      :tasks tasks
                                      ;; tree will be NIL if we aren't returning a plan tree.
                                      :top-tasks (get-top-tasks tasks)))
         (tree  (when plan-tree
                  (let ((tree (plan-tree:make-top-node
                               :task 'TOP
                               :lookup-table (plan-tree-lookup search-state))))
                    (make-plan-tree-for-task-net tasks tree (plan-tree-lookup search-state))
                    tree)))
         (*expansions* 0)
         (*inferences* 0)
         total-run-time total-real-time
         total-expansions total-inferences)

    #+ignore(when repairable (clrhash *analogical-replay-table*))
    
    (when plan-tree
      (setf (slot-value search-state 'plan-tree) tree)
      (unless no-dependencies
        (prepare-state-tag-decoder)))
    (set-variable-property domain tasks)

    (unwind-protect
        (seek-plans-stack search-state domain :which :first
                          :repairable repairable)
      (setq total-run-time (- (get-internal-run-time) start-run-time)
            total-real-time (- (get-internal-real-time)
                               start-real-time))
      
      (setq total-expansions *expansions*
            total-inferences *inferences*)
      
      (when (> verbose 0)
        (print-stats-header "Totals:" out-stream)
        (print-stats "" *plans-found* total-expansions total-inferences
                     total-run-time total-real-time out-stream))
      
      (unless repairable
        (delete-state-tag-decoder)))))
      
(defun seek-plans-stack (state domain &key (which :first) repairable)
  "Workhorse function for FIND-PLANS-STACK.  Executes the SHOP2 search
virtual machine, cycling through different virtual instructions depending
on the value of the MODE slot of STATE.
   Returns three values:
List of PLANS -- currently there is always only one, but this complies
   with the return from conventional SHOP2.
List of PLAN-TREES -- optional
List of indices into PLAN-TREES -- optional, will be supplied if PLAN-TREES
    supplied."
  (declare (ignorable which))
  ;; kick off the stack VM
  (setf (mode state) 'test-for-done)
  (catch 'search-failed
    (iter
      (when *enhanced-plan-tree* (unless (plan-tree state)
                                   (error "Search state object should have a PLAN-TREE.")))
      ;; bumped the verbose for this to be printed, because it's really not useful
      (when (>= *verbose* 2)
        (format t "~&State is: ~a. Mode is: ~a.~%" state (mode state)))
      (ecase (mode state)
        (test-for-done
         (if (empty-p state)
             ;; because we are doing HTN Planning, if there are no tasks, we have a plan.
             (setf (mode state) 'extract-plan)
             ;; (cond ((test-for-done state which-plans)
             ;;        (setf (mode state) 'extract-plan))
             ;;       (t (stack-backtrack state))))
             (setf (mode state) 'look-for-immediate-task)))
        (look-for-immediate-task
         (cond ((immediate-tasks state)
                (let ((state (prepare-choose-immediate-task-state state)))
                  (setf (mode state) 'pop-immediate-task)))
               (t
                (setf (mode state) 'prepare-to-choose-toplevel-task))))
        (pop-immediate-task
         (if (choose-immediate-task-state state)
             (setf (mode state) 'expand-task)
             (stack-backtrack state)))
        (prepare-to-choose-toplevel-task
         (let ((tasks (task-sorter domain (top-tasks state) (unifier state))))
           (unless tasks (error "Should never get to ~A with no top-tasks." (mode state)))
           (setf (alternatives state) tasks
                 (mode state) 'pop-toplevel-task)))
        (pop-toplevel-task
         (if (choose-toplevel-task state)
             (setf (mode state) 'expand-task)
             (stack-backtrack state)))
        (expand-task
         (let ((task (current-task state)))
           (trace-print :tasks (get-task-name task) (world-state state)
                        "~2%Depth ~s, trying task ~s"
                        (depth state)
                        (apply-substitution task (unifier state)))
           (incf *expansions*)
           (cond
             ((primitivep (get-task-name task))
              (setf (mode state) 'expand-primitive-task))
             ((eql (get-task-name task) :loop)
              (setf (mode state) 'unfold-looping-task))
             (t ; original nonprimitive:
              (setf (mode state) 'prepare-to-choose-method)))))

        (unfold-looping-task
         (if (unfold-loop-task domain state)
             (progn
               (setf (mode state) 'test-for-done)
               (incf (depth state)))
             (with-slots (current-task depth world-state) state
               (trace-print :tasks (get-task-name current-task) world-state
                            "~2%Depth ~s, backtracking from task~%      task ~s"
                            depth
                            current-task)
               (stack-backtrack state))))

        (expand-primitive-task
         (if (expand-primitive-state state domain)
             (progn
               (setf (mode state) 'test-for-done)
               (incf (depth state)))
             (with-slots (current-task depth world-state) state
               (trace-print :tasks (get-task-name current-task) world-state
                            "~2%Depth ~s, backtracking from task~%      task ~s"
                            depth
                            current-task)
               (stack-backtrack state))))
        (prepare-to-choose-method
         (let* ((task-name (get-task-name (current-task state)))
                (methods (methods domain task-name)))
           ;; possibly the following should be a continuable error.
           (unless methods (error "No methods for complex task ~a" task-name))
           (setf (alternatives state) methods)
           (setf (mode state) 'choose-method)))
        (choose-method
         (if (choose-method-state state domain)
             (setf (mode state) 'choose-method-bindings)
             (progn
               (let ((task1 (current-task state))
                     (depth (depth state))
                     (state (world-state state)))
                 (trace-print :tasks (get-task-name task1) state
                              "~2%Depth ~s, backtracking from task~%      task ~s"
                              depth
                              task1))
               (stack-backtrack state))))
        ;; the alternatives here are triples of (expansions unifiers dependencies)
        (choose-method-bindings
         (if (choose-method-bindings-state state)
             (progn
               (setf (mode state) 'test-for-done)
               (incf (depth state)))
             (stack-backtrack state)))
        (extract-plan
         (let ((plans (test-plans-found state :repairable repairable)))
           (when *enhanced-plan-tree*
             (apply-substitution-to-tree (unifier state) (plan-tree state)))
           (setf *plans-found* (append plans *plans-found*))
           (return
             (values plans
                     (when *enhanced-plan-tree*
                       (list
                            (plan-tree state)))
                     (when *enhanced-plan-tree*
                       (list
                        (plan-tree-lookup state)))
                     state))))))))

;;; Traverse the plan tree, applying the bindings to the
;;; EXPANDED-TASKs everywhere in the tree.
(defun apply-substitution-to-tree (bindings plan-tree)
  (labels ((apply-bindings-and-recurse (node)
             (etypecase node
               ((or plan-tree:top-node plan-tree:ordered-tree-node plan-tree:unordered-tree-node)
                (recurse node))
               (plan-tree:primitive-tree-node
                (apply-bindings node))
               (plan-tree:complex-tree-node
                (apply-bindings node)
                (recurse node))))
           (apply-bindings (node)
             (setf (plan-tree:tree-node-expanded-task node)
                   (apply-substitution (plan-tree:tree-node-expanded-task node) bindings)))
           (recurse (node)
             (dolist (c (plan-tree:complex-tree-node-children node))
               (apply-bindings-and-recurse c))))
    
    (apply-bindings-and-recurse plan-tree)))

(defmethod unfold-loop-task ((domain domain) search-state)
  (declare (ignore search-state))
  (error "SHOP2 proper should not have used this instruction yet at all."))


(defun CHOOSE-METHOD-BINDINGS-STATE (state)
  (with-slots (alternatives backtrack-stack
               current-task depth
               top-tasks tasks)
      state
    (when alternatives            ; method alternatives remain
      (let ((method-body-unifier (pop alternatives)))
        (destructuring-bind ((label . reduction) unifier depends)
            method-body-unifier
          (push (make-cs-state :mode (mode state)
                               :current-task current-task
                               :alternatives alternatives)
                backtrack-stack)
          (push (make-method-instantiation
                 :unifier (unifier state)
                 :top-tasks top-tasks
                 :tasks tasks)
                backtrack-stack)
          (when *enhanced-plan-tree*
            (let* ((parent (plan-tree:find-task-in-tree current-task (plan-tree-lookup state)))
                   (child (make-plan-tree-for-task-net reduction parent (plan-tree-lookup state))))
              ;; MAKE-PLAN-TREE-FOR-TASK-NET as a side-effect, links PARENT and CHILD.
              (push (make-add-child-to-tree :parent parent :child child)
                    backtrack-stack)
              (when *record-dependencies-p*
                (let ((depends (make-dependencies parent depends (plan-tree-lookup state))))
                  (when depends
                    (setf (plan-tree:tree-node-dependencies parent) depends)
                    (make-add-dependencies depends))))))
          (multiple-value-setq (top-tasks tasks)
            (apply-method-bindings current-task top-tasks tasks
                                   reduction unifier))
          (trace-print :methods label (world-state state)
                       "~2%Depth ~s, applying method ~s~%      task ~s~% reduction ~s"
                       depth label current-task reduction)
          (setf (unifier state) unifier)))
      t)))
(defun CHOOSE-METHOD-STATE (state domain)
  (with-slots (alternatives backtrack-stack
               plan-tree-lookup current-task)
      state
    (when alternatives            ; method alternatives remain
      (let ((method (pop alternatives)))
        (push (make-cs-state :mode (mode state)
                             :current-task current-task
                             :alternatives alternatives)
              backtrack-stack)
        (multiple-value-bind (expansions unifiers dependencies task-expansion)
            (apply-method domain (world-state state)
                          (get-task-body current-task)
                          method (protections state)
                          (depth state) (unifier state))
          (when expansions
            (when *enhanced-plan-tree*
              (let ((task-node (plan-tree:find-task-in-tree current-task plan-tree-lookup)))
                (push (record-node-expansion task-node task-expansion plan-tree-lookup)
                      backtrack-stack)))
            (setf alternatives
                  (if *record-dependencies-p*
                      (mapcar #'list expansions unifiers dependencies)
                      (mapcar #'(lambda (x y) (list x y nil)) expansions unifiers)))
            t))))))

(defgeneric expand-primitive-state (state domain))

(defmethod EXPAND-PRIMITIVE-STATE (state (domain domain))
  ;; first we need to record what we will need to pop...
  (with-slots (top-tasks tasks protections partial-plan
               current-task depth
               unifier cost backtrack-stack
               world-state)
      state
    (push (make-prim-state-expand :top-tasks top-tasks
                                  :tasks tasks
                                  :protections protections
                                  :partial-plan partial-plan
                                  :unifier unifier
                                  :partial-plan-cost cost)
          backtrack-stack)
    (multiple-value-bind (success top-tasks1 tasks1 protections1 planned-action unifier1 tag prim-cost
                          depends)      ;one set of dependencies...
        (seek-plans-primitive-1 domain current-task world-state tasks top-tasks depth protections unifier)
      (when success
        (setf top-tasks top-tasks1
              tasks tasks1
              protections protections1
              partial-plan (cons prim-cost (cons planned-action (partial-plan state)))
              unifier unifier1)
        (incf cost prim-cost)
        (when (and *enhanced-plan-tree* *record-dependencies-p*)
          (let ((tree-node
                  (plan-tree:find-task-in-tree current-task (plan-tree-lookup state))))
            (push (record-node-expansion tree-node planned-action (plan-tree-lookup state))
                  (backtrack-stack state))
            (let ((depends (make-dependencies tree-node depends (plan-tree-lookup state))))
              (when depends
                (setf (plan-tree:tree-node-dependencies tree-node) depends)
                (make-add-dependencies depends))))
          (make-tag-map tag current-task planned-action))
        (push (make-world-state-tag :tag tag) (backtrack-stack state))
        t))))

;;; record the expansion of a tree node by rewriting its task.  Return
;;; the backtrack stack entry needed to undo the transformation.
(defun record-node-expansion (tree-node expanded-task hash-table)
  (assert expanded-task)
  (setf (plan-tree:tree-node-expanded-task tree-node)
        expanded-task)
  (setf (gethash expanded-task hash-table) tree-node)
  (make-record-expansion tree-node))

(defun make-dependencies (tree-node depend-lists hash-table)
  (iter (for depend in depend-lists)
    ;; PROP is the proposition consumed and establisher is a task name.
    (as prop = (shop2.theorem-prover::rd-prop depend))
    (as establisher = (shop2.theorem-prover::rd-est depend))
    (unless (and prop establisher)
      (error "Ill-formed dependency-list: ~S"  depend))
    (collecting
     (plan-tree:make-dependency
      :establisher (if (eq establisher :init)
                       :init
                       (plan-tree:find-task-in-tree (strip-task-sexp establisher) hash-table))
      :prop prop
      :consumer tree-node))))

(defun task-sexp-task-name (task)
  (let* ((task (if (eq (first task) :task) (rest task)
                 task))
         (task (if (eq (first task) :immediate) (rest task) task)))
    (first task)))

(defun strip-task-sexp (task)
  "Remove qualifiers like :TASK and :IMMEDIATE from TASK and return it."
  (let* ((task (if (eq (first task) :task) (rest task)
                   task))
         (task (if (eq (first task) :immediate) (rest task) task)))
    task))
  


(defun make-plan-tree-for-task-net (task-net parent hash-table)
  (ecase (first task-net)
    (:ordered (let ((node (plan-tree:make-ordered-tree-node :parent parent)))
                (appendf (plan-tree:complex-tree-node-children parent) (list node))
                (mapcar #'(lambda (x) (make-plan-tree-for-task-net x node hash-table))
                        (rest task-net))
                node))
    (:unordered (let ((node (plan-tree:make-unordered-tree-node :parent parent)))
                  (appendf (plan-tree:complex-tree-node-children parent) (list node))
                  (mapcar #'(lambda (x) (make-plan-tree-for-task-net x node hash-table))
                          (rest task-net))
                  node))
    (:task (let* ((task (strip-task-sexp task-net))
                  (node (if (primitivep (first task))
                            (plan-tree:make-primitive-tree-node :task task :parent parent)
                            (plan-tree:make-complex-tree-node :task task :parent parent))))
             (appendf (plan-tree:complex-tree-node-children parent)  (list node))
             (setf (gethash task hash-table) node)
             node))))
                           


;;; end stubs

;;; PRECONDITION: The alternatives for the state must be populated with the set of candidate tasks.
(defun CHOOSE-TOPLEVEL-TASK (state)
  (when (alternatives state)
    (with-slots (current-task alternatives) state
      ;; heuristic already applied here.
      (setf current-task (pop alternatives))
      (push (make-cs-state :alternatives alternatives
                           :current-task current-task
                           :mode (mode state))
            (backtrack-stack state))
      state)))
(defun IMMEDIATE-TASKS (state)
  (get-immediate-list (top-tasks state)))
#+ignore (defun TEST-FOR-DONE (state which-plans)
  (let ((*plans-found* (plans-found state)))
    (when-done t)))
;;; I wish I knew why TOP-TASKS = '(NIL) instead of just NIL when there
;;; are no top-tasks.  I just copied this over from mainstream SHOP2.
(defun EMPTY-P (state)
  (with-slots (top-tasks) state
    (or (null top-tasks) (equal top-tasks '(NIL)))))
;;; FIXME: for now we just extract the plan -- as if we only are
;;; finding the first plan.  Simplification to get things done
;;; more quickly.
(defun test-plans-found (state &key repairable)
  (with-slots (partial-plan) state
    (when partial-plan
      (list ; comply with FIND-PLANS return type by returning a list of plans
       ;; in this case always a singleton or nil.
       (if repairable
           (reverse partial-plan)
           (strip-NOPs (reverse partial-plan)))))))
(defun prepare-choose-immediate-task-state (state)
  (let ((immediates (immediate-tasks state)))
    (setf (alternatives state) immediates)
    state))
(defun choose-immediate-task-state (state)
  (when (alternatives state)
    (with-slots (current-task alternatives) state
      (setf current-task
            (choose-immediate-task alternatives (unifier state)))
      (setf alternatives (remove current-task alternatives :test 'eq))
      (push (make-cs-state :alternatives alternatives
                           :current-task current-task
                           :mode (mode state))
            (backtrack-stack state))
      state)))
(defun stack-backtrack (state)
  "Chronological backtracking only, for now.
Return the CHOICE-ENTRY where you stopped."
  (verbose-format 2 "~&Backtracking:~%")
  (iter (for entry = (pop (backtrack-stack state)))
    (verbose-format 2 "~T~a~%" entry)
    (when (typep entry 'bottom-of-stack)
      (throw 'search-failed nil))
    (do-backtrack entry state)
    (when (typep entry 'choice-entry)
      (return entry))))

(defun stack-backjump (state target)
  (assert (typep target 'choice-entry))
  (iter (for entry = (stack-backtrack state))
    (when (eq entry target)
      (return target))))

(defun remove-subtree-from-table (hash-table subtree)
  (assert (typep subtree 'plan-tree:tree-node))
  (labels ((remove-forest (forest)
               (if (null forest) nil
                   (or (remove-subtree (first forest))
                       (remove-forest (rest forest)))))
             (remove-subtree (tree)
               (cond ((or (typep tree 'plan-tree:ordered-tree-node)
                          (typep tree 'plan-tree:unordered-tree-node))
                      (remove-forest (plan-tree:complex-tree-node-children tree)))
                     ((typep tree 'plan-tree:primitive-tree-node)
                      (assert (gethash (plan-tree:tree-node-task tree) hash-table))
                      (remhash (plan-tree:tree-node-task tree) hash-table))
                     ((typep tree 'plan-tree:complex-tree-node)
                      (assert (gethash (plan-tree:tree-node-task tree) hash-table))
                      (remhash (plan-tree:tree-node-task tree) hash-table)
                      (remove-forest (plan-tree:complex-tree-node-children tree)))
                     (t (error "Unexpected argument:  ~s"  tree)))))
    (remove-subtree subtree)))
