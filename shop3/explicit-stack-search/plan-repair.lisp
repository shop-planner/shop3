(in-package :shop)

(defun divergence-op-p (op)
  (or (eq op :add)
      (eq op :delete)))

(defun divergence-list-p (list)
  (and (alexandria:proper-list-p list)
       (every #'(lambda (d) (divergence-op-p (first d))) list)))

(deftype divergence-list ()
  `(satisfies divergence-list-p))

(define-condition no-failed-task ()
  ()
  (:report (lambda (nft stream)
             (declare (ignorable nft))
             (format stream "~&Divergence does not cause plan failure. Returning initial plan with divergence.~%"))))

;;; This is the only exported function for now, and should be used when backjumping.
(defun repair-plan (domain plan plan-tree executed divergence search-state
                    &key (verbose 2) plan-tree-hash
                      no-failed-task)
  "Arguments:
DOMAIN: SHOP2 domain object
PLAN: SHOP2 plan sequence (should work with costs or without, but don't remove internal operators)
PLAN-TREE: *Enhanced* plan tree object (not classic SHOP2 plan tree)
EXECUTED: Prefix of the PLAN
DIVERGENCE: Divergence between the expected state after EXECUTED, specified as (([:ADD|:DELETE] <fact>)+)
SEARCH-STATE: Search state object
PLAN-TREE-HASH: Hash table indexing and optimizing access to PLAN-TREE.  This is optional -- we can
  manage access anyway, but it will be slower.
Returns:
  1. new plan WITHOUT COSTS or INTERNAL ACTIONS
  2. new plan tree (enhanced plan tree, not old-style SHOP plan tree)
  3. plan tree lookup table
  4. search-state object."
  (check-type divergence divergence-list)
  (let ((*verbose* verbose))
    (verbose-format "Divergence after step ~d (from 1) is:~%~s~%" (length executed) divergence)
    (handler-bind
        ((no-failed-task #'(lambda (nft)
                             (format *error-output* "Got no failed task error: ~a~%" nft)
                             (force-output *error-output*)
                             (case no-failed-task
                               (:error (cerror "Just continue and return unmodified plan"
                                               "Divergence should cause some task failure."))
                               (:retry (return-from repair-plan :no-failed-task))))))
      (multiple-value-bind (failed ; tree node -- this is the node that must be *replanned*, not the node whose preconditions are violated.
                                        ; It's the *parent* of the node that has actually failed.
                            failed-action ; The failed action is EITHER the first action whose dependencies are broken
                                        ; OR the leftmost leaf child of the leftmost broken complex task
                            )
          ;; we find the failed tree node by looking UP from the actions in the plan.
          (subtree:find-failed-task domain plan plan-tree executed
                                    divergence :plan-tree-hash plan-tree-hash)
        (verbose-format "~&Failing task is:~%~T~A~%Failing action is:~%~T~A~%" failed failed-action)
        (if failed
            (let ((new-search-state (freeze-state executed failed-action divergence search-state)))
              #+nil(break "Inspect NEW-SEARCH-STATE.")
              (multiple-value-bind (new-plans new-plan-trees lookup-tables final-search-state)
                  (let ((*plan-tree* nil) ; plan tree should never be true when using Explicit Stack Search.
                        (*enhanced-plan-tree* t))
                    (replan-from-failure domain failed new-search-state :verbose verbose))
                (when new-plans
                  (let ((new-plan (first new-plans))
                        (new-plan-tree (first new-plan-trees))
                        (new-lookup-table (first lookup-tables)))
                    (multiple-value-bind (prefix suffix)
                        (extract-suffix new-plan executed)
                      (values
                       ;; new plan sequence
                       (append (shorter-plan prefix)
                               (list (cons :divergence divergence))
                               (shorter-plan suffix))
                       new-plan-tree
                       new-lookup-table
                       final-search-state))))))
            ;; the old plan is good
            (multiple-value-bind (prefix suffix)
                (extract-suffix plan executed)
              (signal 'no-failed-task)
              (values
               (append prefix
                       (if (numberp (second plan)) ;plan with costs?
                           (list (cons :divergence divergence) 0.0)
                           (list (cons :divergence divergence)))
                       suffix)
               plan-tree
               plan-tree-hash
               search-state)))))))

(defgeneric find-failed-stack-entry (failed obj)
  (:documentation "Find and return the stack entry that corresponds
to adding FAILED to the plan tree.")
  (:method ((failed plan-tree::complex-tree-node)
            (obj search-state))
    (find-failed-stack-entry failed (backtrack-stack obj)))
  (:method ((failed plan-tree::complex-tree-node)
            (stack list))
    (or
     (find-if #'(lambda (s)
                  (and (typep s 'add-child-to-tree)
                       (let ((child (child s)))
                         (or (eq child failed)
                             (and (typep child 'plan-tree:pseudo-node)
                                  (member failed
                                          (plan-tree:complex-tree-node-children child)))))))
              stack)
     (error "Unable to find stack entry for adding ~a to plan tree." failed))))

(defgeneric find-failed-choice-entry (failed obj)
  (:documentation "Find and return the stack entry for the choice
BEFORE the insertion of FAILED into the plan tree.")
  (:method ((failed plan-tree::complex-tree-node)
            (obj search-state))
    (find-failed-choice-entry failed (backtrack-stack obj)))
  (:method ((failed plan-tree::complex-tree-node)
            (stack list))
    (let* ((tree-addition (find-failed-stack-entry failed stack))
           ;; stack elements below tree-addition
           (stack-suffix (member tree-addition stack)))
      (or
       (find-if #'(lambda (x) (and (typep x 'choice-entry)
                                   (eq (mode x) 'pop-toplevel-task)))
                stack-suffix)
       (error "Unable to find a stack entry for failed node ~A" failed)))))


(defun replan-from-failure (domain failed-tree-node search-state &key (verbose 0) (plan-num-limit 1))
  (let ((*verbose* verbose)
        (*domain* domain)
        *plans-found*)
    (when (>= *verbose* 2)
      (format t "~&World state before backjump is:~%")
      (pprint (state-atoms (world-state search-state))))
    (let ((failed-choice-node (find-failed-choice-entry failed-tree-node search-state)))
      (when (>= *verbose* 1)
        (format t "~&Backjumping to ~A~%" failed-choice-node))
      (stack-backjump search-state failed-choice-node)
      (cond ((>= *verbose* 2)
             (format t "~&Beginning plan repair~%World state after backjump is:~%")
             (pprint (state-atoms (world-state search-state))))
            ((= *verbose* 1)
             (format t "~&Beginning plan repair~%")))
      ;; at this point, we have a backtrack state where we have popped
      ;; a toplevel task, and if we restart here, we lose the possible
      ;; choices.  So what we need to do is to reset the alternatives
      ;; for solving this task, not find a different task.
      (setf (mode search-state) 'expand-task)
      ;; must be "repairable" so that we don't strip NOPs.
      (seek-plans-stack search-state domain :plan-num-limit plan-num-limit :repairable t))))

;;; This is a very messy function: it's supposed to grab fresh copies
;;; of actions (i.e., the actions in the PLAN, which is a replan) in
;;; the prefix, whenever possible.  But there can be divergences
;;; (i.e., actions that are not EQ), so we need to copy some actions
;;; from the prefix.  Yuck.
(defun extract-suffix (plan prefix)
  ;; hardened to handle costs... yuck
  (let ((plan-copy (copy-list plan)))
    (iter (for (x . rest) on prefix)
      (as y = (first plan-copy))
      (with unmatched-prefix = nil)
      (if (numberp x)
          (cond ((eql x y)              ; match a number
                 (pop plan-copy)
                 (collecting y into new-prefix))
                ((listp y) ; x is a cost but y is an action - do nothing
                 )
                (t (error "Unmatched cost in plan: ~D doesn't match ~D" x y)))
          ;; x is an operator/action
          (cond ((eq x y)
                 (pop plan-copy)
                 (collecting y into new-prefix)) ; match
                ((numberp y)
                 (pop plan-copy)
                 (let ((next (first plan-copy)))
                   (if (eq x next)
                       (progn (pop plan-copy)
                              (collecting next into new-prefix))
                       (progn (setf unmatched-prefix (cons x rest))
                              (finish)))))
                (t (setf unmatched-prefix (cons x rest))
                   (finish))))
      (finally
       (setf new-prefix (append new-prefix unmatched-prefix))
       (cond ((and (numberp (first plan-copy))
                   (some 'numberp (rest plan-copy))
                   (not unmatched-prefix))
              ;; move cost of last executed action
              (let ((last-cost (pop plan-copy)))
                (setf new-prefix (append new-prefix (list last-cost)))))
             ((numberp (first plan-copy))
              ;; there are no more numbers in the plan
              (pop plan-copy)))
       (return
         (values new-prefix plan-copy))))))

;;; SHOP assigns integers as world state tags according to a
;;; magic formula.  This tells us what will be the world state
;;; tag after the actions in EXECUTED are executed.
(defun find-world-state-tag (executed)
  ;; we have to find the index of the last executed action. The
  ;; NUMBERP check is to make sure that we can handle plans
  ;; with and without the cost numbers.
  (* (if (some #'numberp executed) ; if there are costs in the plan
         (/ (length executed) 2)
         (length executed))
     ;; magic constant for the tag increment per
     ;; operator.
     2))

(defun freeze-state (executed failed-action divergence search-state)
  "Arguments:
PLAN: sequence of primitive actions.
EXECUTED: Prefix of the plan that has been executed.
FAILED-ACTION: First action whose preconditions are clobbered,
          or the leftmost leaf child of the first non-primitive
          task whose preconditions are broken.
DIVERGENCE: Divergence between the expected state after
      EXECUTED, specified as (([:ADD|:DELETE] <fact>)+)
SEARCH-STATE: Search state object.

  Returns:
Modified search state object."
  (let* ((world-state (world-state search-state))
         ;; we have to find the index of the last executed action. The
         ;; NUMBERP check is to make sure that we can handle plans
         ;; with and without the cost numbers.
         (world-state-tag (find-world-state-tag executed))
         ;; can't correctly apply state updates beyond here
         (failed-action-tag
           (tag-for-action failed-action))
         (new-state-obj (shop2.common::copy-state world-state)))
    (assert (integerp world-state-tag))
    #+nil(break "Inside FREEZE-STATE")
    ;; this gives us the state right after the execution of the action immediately
    ;; before the divergence (i.e., after EXECUTED).
    (shop.common:retract-state-changes new-state-obj (1+ world-state-tag))
    ;; now put the divergences into effect, taking sleazy advantage of the fact that the
    ;; world state tag increments by two.
    (let ((new-tag
            (shop.common:tag-state new-state-obj 1)))
      (iter (for (op fact) in divergence)
        (ecase op
          (:add (shop.common:add-atom-to-state fact new-state-obj 0 :execution-divergence))
          (:delete (shop.common:delete-atom-from-state fact new-state-obj 0 :execution-divergence))))
      ;; now make it impossible to backtrack before this point...
      (setf (shop2.common::tagged-state-block-at new-state-obj) new-tag))
      #+ignore(break "After freezing, before rolling forward, state is: ~s" new-state-obj)
    ;; now roll forward again.  Note that the world state changes are a *stack*, so we need to
    ;; push them back on....  So this is *semantically* a "suffix" but syntactically a prefix.
    (let ((suffix
            (reverse
             (subseq (shop2.common::tagged-state-tags-info world-state)
                     0
                     (or (position world-state-tag (shop2.common::tagged-state-tags-info world-state)
                                   :key 'first)
                         (error "Couldn't find world state tag ~D in state" world-state-tag))))))
      #+ignore(break "Suffix of state tags is: ~S" suffix)
      ;; Redo the state updates...
      (replay-state-changes new-state-obj suffix failed-action-tag)

      ;; now put the new world state in place...
      (setf (world-state search-state) new-state-obj)
      (when (> *verbose* 1)
        (format t "~&At start of plan repair, state is: ~%")
        (print-current-state :state new-state-obj
                             :sorted t))

      search-state)))

#+ignore
(progn
  (defvar *saved-state* nil)
  (excl:def-fwrapper save-frozen-state (&rest args)
    (setf *saved-state* (excl:call-next-fwrapper)))
  (excl:fwrap 'freeze-state 'save-frozen-state 'save-frozen-state))
