
(in-package :shop3)

(defvar *enhanced-plan-tree*
  nil
  "Do we build a dependency-enhanced plan tree?")
(defvar *no-dependencies*
  NIL
  "When building an ENHANCED-PLAN-TREE, do not record  causal links.  Defaults to NIL.")

(defgeneric unfold-loop-task (domain state)
  (:documentation "Driver for the looping tasks."))

(defgeneric expand-primitive-state (state domain))

(declaim
 (ftype
  (function ((or problem symbol) &key
                                 (:domain (or domain symbol))
                                 (:verbose (member 0 1 2))
                                 (:plan-tree t)
                                 (:gc t)
                                 (:no-dependencies t)
                                 (:repairable t)
                                 (:state-type symbol)
                                 (:out-stream (or t stream))
                                 (:which (member :first :all))
                                 (:plan-num-limit (and (integer 1) fixnum))
                                 (:analogical-replay t)
                                 (:unpack-returns t)
                                 (:make-analogy-table t))
            t)
  find-plans-stack))
(defun find-plans-stack (problem &key domain (verbose 0) plan-tree (gc *gc*)
                                   (no-dependencies nil)
                                   repairable
                                   (state-type :mixed state-type-supplied-p)
                                   (out-stream t)
                                   (which :first)
                                   (plan-num-limit 1)
                                   analogical-replay
                                   (unpack-returns t)
                                   make-analogy-table)
  "Top level search function for explicit-state search in SHOP3.
Does not support the full range of options supported by SHOP3's
`find-plans-stack`.

Keyword arguments:
* domain : either a domain name (symbol) or a `shop:domain` object.
* verbose : 0, 1, 2, 3; default 0
* plan-tree : build and return a plan tree? (`plan-tree:top-node`),
        defaults to `nil`.
* gc : If possible, perform a full GC before starting to plan.  Default:
        current value of `shop:*gc*`.
* no-dependencies : if building a plan tree, build it *without* causal
        dependencies.  Default: `nil`.
* repairable : return plans that can be repaired.  Default: `nil`.
* state-type : what state type should be used for representing world states?
        (Note: world-state/SHOP state, *not* search-state). Default: `:mixed`.
* out-stream : where should output be printed.  Default: `t` (standard output).
* which : What/how many plans should be returned?  Supports only `:first` (the
        default) and `:all`.
* plan-num-limit: an int greater than or equal to 1 (its default value)
             specifying a limit on the number of plans to generate.
* analogical-replay : Do search informed by the contents of the
        `*analogical-replay-table*`. Default: `nil`.
* make-analogy-table : Populate `*analogical-replay-table*` while planning.
        Only works with `:which` = `:first`.  Default: `nil`.
* unpack-returns : If true, return values in a way compatible with `find-plans`.
        If false, return a list of `plan-return` objects instead.  See discussion
        of return values, below.  Default: `t`.

Return values:
    There are two possible return types, selected by the keyword argument
`unpack-returns`:

1. Default/compatible with `find-plans`:
    * List of plans
    * List of plan trees (if computed)
    * List of plan tree lookup tables
    * List of final world states
    * List of analogical replay tables (if computed)
To comply with SHOP3, always returns a list of plans.
  If the PLAN-TREE keyword argument is non-NIL, will return an enhanced plan
tree, with causal links, unless NO-DEPENDENCIES is true.
  Returns the values returned by SEEK-PLANS-STACK, qv.

2. If UNPACK-RETURNS is NIL, then return one or more appropriate PLAN-RETURN
objects."
  (when gc
    (trivial-garbage:gc :full t))

  (when (and (not plan-tree) repairable)
    (warn "Does not make sense to plan repairably without the plan tree.~%Setting PLAN-TREE to true.")
    (setf plan-tree t))

  (let* ((start-run-time (get-internal-run-time))
         (start-real-time (get-internal-real-time))
         (*plan-tree* nil)
         (*plans-found* nil)
         (*enhanced-plan-tree* plan-tree)
         (*no-dependencies* no-dependencies)
         (*record-dependencies-p* (and *enhanced-plan-tree* (not *no-dependencies*)))
         (*verbose* verbose)
         (*which* which)
         (*make-analogy-table* (progn
                                 (or (and make-analogy-table (eq which :first))
                                     (not make-analogy-table)
                                     (error "Make analogy table only supported for :which == :first"))
                                 make-analogy-table))
         (*analogical-replay* (progn
                                 (or (and analogical-replay (eq which :first))
                                     (not analogical-replay)
                                     (error "Analogical replay only supported for :which == :first"))
                                 analogical-replay))
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
                             (if state-type-supplied-p
                                 state-type
                                 (default-state-type domain))
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

    (unless (member which '(:all :first))
      (error "Unable to plan with :WHICH = ~s in FIND-PLANS-STACK" which))

    (when (and analogical-replay make-analogy-table)
      (error "Cannot build the analogy table while using it: :analogical-replay and ~
              :make-analogy-table are mutually exclusive options."))
    (when make-analogy-table
      (clear-replay-table domain *analogical-replay-table*))

    (when plan-tree
      (setf (slot-value search-state 'plan-tree) tree)
      (unless no-dependencies
        (prepare-state-tag-decoder)))
    (set-variable-property domain tasks)

    (unwind-protect
        (seek-plans-stack search-state domain
                          :unpack-returns unpack-returns
                          :which which
                          :plan-num-limit plan-num-limit
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

(declaim
 (ftype
  (function (search-state &key (:repairable t))
            (values (or t nil) &optional list))
  test-plan-found))


(defun seek-plans-stack (state domain &key (which :first) repairable
                                        (unpack-returns t)
                                        plan-num-limit)
  "Workhorse function for FIND-PLANS-STACK.  Executes the SHOP3 search
virtual machine, cycling through different virtual instructions depending
on the value of the MODE slot of STATE.
   If UNPACK-RETURNS is non-NIL (the default):
   Returns five values:
List of PLANS -- currently there is always only one, but this complies
   with the return from conventional SHOP3.
List of PLAN-TREES -- optional
List of indices into PLAN-TREES -- optional, will be supplied if PLAN-TREES
    supplied.
List of world states (SHOP states) -- optional
List of analogical-replay tables -- optional
   If UNPACK-RETURNS is NIL, returns a list of PLAN-RETURN objects."
  ;; kick off the stack VM
  (setf (mode state) 'test-for-done)
  (handler-case
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
           (let ((tasks (sort-tasks domain (top-tasks state) (unifier state) which)))
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
           (when (> *verbose* 2) (format t "~%Starting to unfold the loop..."))
           (if (unfold-loop-task domain state)
               (progn
                 (setf (mode state) 'test-for-done)
                 (incf (depth state)))
               ;; Else,
               (with-slots (current-task depth world-state) state
                 (when (> *verbose* 0) (format t "~%Could not unfold the loop successfully..."))
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
             (setf (alternatives state) (sort-methods domain methods which))
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
           (let (plan-return)
             ;; did we find a new plan? If so, then store it
             (multiple-value-bind (success plan)
                 (test-plan-found state :repairable repairable)
               (when success
                 (setf plan-return
                       (make-plan-return domain which
                                         :plan plan
                                         :state state
                                         :repairable repairable
                                         :replay-table *analogical-replay-table*))
                 (setf *plans-found* (cons plan-return *plans-found*))
                 (when (> *verbose* 0)
                   (format t "~%~%Solution plan found successfully...:~%~a"
                           plan))))
             ;; handle *PLANS-FOUND* based on the value of WHICH
             (ecase which
               (:first
                (cond ((and plan-return (>= (length *plans-found*) plan-num-limit))
                       (return-from seek-plans-stack
                         (plan-returns (reverse *plans-found*)
                                       unpack-returns)))
                      ;; we've found one plan, but there are possibly more plans to find...
                      (plan-return (stack-backtrack state))
                      (t
                       (return-from seek-plans-stack nil))))
               ;; if we want all the plans, just keep searching until we fail,
               ;; and then return any plans we have found.
               (:all (stack-backtrack state)))))))
    (search-failed ()
      (case which
        (:first
         ;; no plans this time -- are there other plans to return?
         (when *plans-found*
             (plan-returns (reverse *plans-found*) unpack-returns)))
        (:all
         (when *plans-found*
           (plan-returns (reverse *plans-found*) unpack-returns)))
        (otherwise nil)))))


(declaim (ftype (function (list) (values list hash-table &optional))
                make-plan-copy))
(defun make-plan-copy (plan)
  "Copy the argument PLAN, and return the copy and a hash-table that maps
the tasks in the original PLAN into the copy.
  The hash table is essential because the plan tree is indexed by object
equality, so when the plan is rewritten, any plan tree must be rewritten,
as well."
  (iter (with lookup-table = (make-hash-table :test #'eq))
               (declare (ignorable rest))
               (for (task num . rest) on plan by 'cddr)
               (as copied-task = (copy-tree task))
               (collecting copied-task into plan-copy)
               (collecting num into plan-copy)
               (setf (gethash task lookup-table) copied-task)
               (finally (return (values plan-copy lookup-table)))))

;;; Internal function, just a helper for `make-plan-return`.
(declaim (inline populate-plan-return))
(defun populate-plan-return (&rest args)
  (apply #'make-instance 'plan-return args))

#-sbcl                                  ; SBCL doesn't like FTYPE declaration for a generic function.
(declaim
 (ftype
  (function (domain symbol &key (:state t) (:repairable t) (:plan list) (:replay-table (or null hash-table)) &allow-other-keys)
            (values plan-return &optional))
  make-plan-return))

(defgeneric make-plan-return (domain which &key state plan replay-table repairable &allow-other-keys)
  (:documentation "Make and return a PLAN-RETURN structure.  How return values are collected
is directed by DOMAIN and WHICH arguments.")
  (:method ((domain domain) (which (eql :all)) &key state plan replay-table repairable)
    (assert (not repairable))
    ;; if there are going to be multiple return values, we must make
    ;; sure that further search does not clobber them.
    (if (not (or *enhanced-plan-tree* *analogical-replay-table*))
        ;; no danger of clobbering
        (populate-plan-return :plan (copy-tree plan))
        (multiple-value-bind (new-plan new-tree)
            (prv:prepare-return-values plan :bindings (unifier state)
                                            :plan-tree (when (slot-boundp state 'plan-tree)
                                                    (plan-tree state)))
          (populate-plan-return
           :plan new-plan
           :tree new-tree
           ;; lookup-table is unnecessary...
           :lookup-table (when new-tree (plan-tree::top-node-lookup-table new-tree))
           :world-state (copy-state (world-state state))
           :replay-table (when replay-table
                           (alexandria:copy-hash-table replay-table))))))
  (:method ((domain domain) (which (eql :first)) &key plan state replay-table repairable)
    (populate-plan-return
     :plan plan
     :tree (when *enhanced-plan-tree*
             (apply-substitution-to-tree (unifier state) (plan-tree state))
             (plan-tree state))
     :lookup-table (when *enhanced-plan-tree*
                     (plan-tree-lookup state))
     :search-state (when repairable state)
     :world-state (world-state state)
     :replay-table (when replay-table
                     (alexandria:copy-hash-table replay-table)))))


(defun plan-returns (pr-list &optional (unpack-returns t))
  "Unpack the return values from PR-LIST, which should be a list
of PLAN-RETURN objects."
  (if unpack-returns
   (iter (for pr in pr-list)
     (check-type pr plan-return)
     (with-slots (plan tree lookup-table replay-table world-state) pr
       (collecting plan into plans)
       (collecting tree into trees)
       (collecting lookup-table into lookup-tables)
       (collecting world-state into world-states)
       (collecting replay-table into replay-tables)
       (finally (return (values plans trees lookup-tables world-states replay-tables)))))
   pr-list))

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
              ;; MAKE-PLAN-TREE-FOR-TASK-NET as a side-effect, links
              ;; PARENT and CHILD.
#|            (format t "~%Subtree1: ~s"  (make-add-child-to-tree :parent (apply-substitution
                                                (plan-tree::complex-tree-node-task parent) unifier)
                                       :child (apply-substitution
                                                (plan-tree::complex-tree-node-children
                                                 child)
                                                unifier)))
              (format t "~%Subtree2: ~s"  (make-add-child-to-tree :parent parent :child child))
              |#
              (push
               (make-add-child-to-tree :parent parent :child child)
               backtrack-stack)
              (when *record-dependencies-p*
                (let ((depends (make-dependencies parent depends (plan-tree-lookup state))))
                  (when depends
                    (setf (plan-tree:tree-node-dependencies parent) depends)
                    (make-add-dependencies :dependencies depends))))))
          (multiple-value-setq (top-tasks tasks)
            (apply-method-bindings current-task top-tasks tasks
                                   reduction unifier label))
          (trace-print :methods label (world-state state)
                       "~2%Depth ~s, applying method ~s~%      task ~s~% reduction ~s"
                       depth label current-task reduction)
          (setf (unifier state) unifier)))
      t)))

(defun CHOOSE-METHOD-STATE (state domain)
  "Try to apply the first of the methods in the current set of
alternatives to the search-state STATE, using DOMAIN.  Return is
boolean, true if the expansion is successful, otherwise NIL to
trigger backtracking."
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
              (let ((task-node (plan-tree:find-task-in-tree
                                current-task plan-tree-lookup)))

                (push (record-node-expansion task-node task-expansion plan-tree-lookup
                                             :chosen-method (method-name domain method))
                      backtrack-stack)))
            (when *make-analogy-table*
              (let ((method-id (domain-id-for-method-lookup domain method)))
                (record-decomposition domain current-task method-id backtrack-stack)))
            (setf alternatives
                  ;; FIXME: Why is the version for recording dependencies not
                  ;; sorting here?
                  (if *record-dependencies-p*
                      (mapcar #'list expansions unifiers dependencies)
                      (multiple-value-bind (expansions unifiers)
                          (sort-results domain expansions unifiers *which*)
                        (mapcar #'(lambda (x y) (list x y nil)) expansions unifiers))))
            t))))))



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
        (when *enhanced-plan-tree*
          (let ((tree-node
                  (plan-tree:find-task-in-tree current-task (plan-tree-lookup state))))
            (push (record-node-expansion tree-node planned-action (plan-tree-lookup state))
                  (backtrack-stack state))
            (when  *record-dependencies-p*
             (let ((depends (make-dependencies tree-node depends (plan-tree-lookup state))))
               (when depends
                 (setf (plan-tree:tree-node-dependencies tree-node) depends)
                 (make-add-dependencies :dependencies depends)))
             ;; tag map is only needed for dependency tracking. [2023/05/02:rpg]
             (make-tag-map tag current-task planned-action))))
        (push (make-world-state-tag :tag tag) (backtrack-stack state))
        t))))

(defmethod sort-methods :around ((domain domain) (methods list) (which-plans symbol))
  (unless (and *analogical-replay* (> (length methods) 1))
    (return-from sort-methods (call-next-method)))
  (let ((guidance (guidance domain
                            ;; FIXME: this is not ideal -- we should have a way of getting
                            ;; the actual task here.  But right now, we are only looking at
                            ;; the task name, anyway.
                            (method-head domain (first methods))
                            *analogical-replay-table*
                            methods)))
    (if guidance ;; we get back a preferred alternative
        (cons guidance (remove guidance methods))
        (call-next-method))))

;;; record the expansion of a tree node by rewriting its task.  Return
;;; the backtrack stack entry needed to undo the transformation.
(defun record-node-expansion (tree-node expanded-task hash-table &key chosen-method)
  (assert expanded-task)
  (setf (plan-tree:tree-node-expanded-task tree-node)
        expanded-task)
  (when chosen-method
    (setf (plan-tree:complex-tree-node-method-name tree-node)
          chosen-method))
  (setf (gethash expanded-task hash-table) tree-node)
  (make-record-expansion :tree-node tree-node))

(defun make-dependencies (tree-node depend-lists hash-table)
  (iter (for depend in depend-lists)
    ;; PROP is the proposition consumed and establisher is a task name.
    (as prop = (shop3.theorem-prover::rd-prop depend))
    (as establisher = (shop3.theorem-prover::rd-est depend))
    (unless (and prop establisher)
      (error "Ill-formed dependency-list: ~S"  depend))
    (collecting
     (plan-tree:make-dependency
      :establisher (if (eq establisher :init)
                       :init
                       (plan-tree:find-task-in-tree (strip-task-sexp establisher) hash-table))
      :prop prop
      :consumer tree-node))))

;; DUPLICATE -- the TASK-ID function does this, as well.
;; (defun task-sexp-task-name (task)
;;   (let* ((task (if (eq (first task) :task) (rest task)
;;                  task))
;;          (task (if (eq (first task) :immediate) (rest task) task)))
;;     (first task)))

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

(defun test-plan-found (state &key repairable)
  "If there is a plan in STATE (a SEARCH-STATE), then
return T and the plan, otherwise return NIL."
  (with-slots (partial-plan) state
    (when partial-plan
      (values t
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

(declaim (ftype (function (search-state) choice-entry) stack-backtrack))
(defun stack-backtrack (state)
  "Do a one-step, chronological backtrack, undoing all
changes until we reach the next CHOICE-ENTRY.

Return the CHOICE-ENTRY where backtracking stopped.

Note: Requires the dynamic variable *DOMAIN* to be properly
bound around calls."
  (verbose-format 2 "~&Backtracking:~%")
  (iter (for entry = (pop (backtrack-stack state)))
    (verbose-format 2 "~T~a~%" entry)
    (when (typep entry 'bottom-of-stack)
      (signal 'search-failed))
    (do-backtrack entry state)
    (when (typep entry 'choice-entry)
      (return entry))))

(declaim (ftype (function (search-state choice-entry) choice-entry) stack-backjump))
(defun stack-backjump (state target)
  (check-type target choice-entry)
  (iter (for entry = (stack-backtrack state))
    (when (eq entry target)
      (return target))))

(defun remove-subtree-from-table (hash-table subtree)
  (check-type subtree plan-tree:tree-node)
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
