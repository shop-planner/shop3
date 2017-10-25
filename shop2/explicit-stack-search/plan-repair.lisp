(in-package :shop2)

;;; This is the only exported function for now, and should be used when backjumping.
(defun repair-plan (domain plan plan-tree executed divergence search-state &key (verbose 1) plan-tree-hash)
  "Arguments:
DOMAIN: SHOP2 domain object
PLAN: SHOP2 plan sequence (should work with costs or without, but don't remove internal operators)
PLAN-TREE: *Enhanced* plan tree object (not classic SHOP2 plan tree)
EXECUTED: Prefix of the PLAN
DIVERGENCE: Divergence between the expected state after EXECUTED, specified as (([:ADD|:DELETE] <fact>)+)
SEARCH-STATE: Search state object
PLAN-TREE-HASH: Hash table indexing and optimizing access to PLAN-TREE.  This is optional -- we can
  manage access anyway, but it will be slower.
Returns: (1) new plan (2) new plan tree (enhanced plan tree, not old-style SHOP plan tree)
\(3\) plan tree lookup table (4) search-state object."
  #+ignore(break "starting to repair plan")
  (multiple-value-bind (failed ; tree node
                        failed-action)
      (subtree:find-failed-task domain plan plan-tree executed
                                divergence :plan-tree-hash plan-tree-hash)
    (let ((new-search-state (freeze-state executed failed-action divergence search-state)))
      #+nil(break "Inspect NEW-SEARCH-STATE.")
      (multiple-value-bind (new-plans new-plan-trees lookup-tables final-search-state)
          (let ((*plan-tree* t)
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
               (append prefix
                       (list (cons :divergence divergence) 0.0)
                       suffix)
               new-plan-tree
               new-lookup-table
               final-search-state))))))))

(defgeneric find-failed-stack-entry (failed obj)
  (:documentation "Find and return the stack entry that corresponds
to adding FAILED to the plan tree.")
  (:method ((failed plan-tree::complex-tree-node)
            (obj search-state))
    (find-failed-stack-entry failed (backtrack-stack obj)))
  (:method ((failed plan-tree::complex-tree-node)
            (stack list))
    (find-if #'(lambda (s)
                 (and (typep s 'add-child-to-tree)
                      (let ((child (child s)))
                        (member failed
                                (plan-tree:complex-tree-node-children child)))))
                    stack)))

(defgeneric find-failed-choice-entry (failed obj)
  (:documentation "Find and return the stack entry for the choice
before the insertion of FAILED into the plan tree.")
  (:method ((failed plan-tree::complex-tree-node)
            (obj search-state))
    (find-failed-choice-entry failed (backtrack-stack obj)))
  (:method ((failed plan-tree::complex-tree-node)
            (stack list))
    (let* ((tree-addition (find-failed-stack-entry failed stack))
           ;; stack elements below tree-addition
           (stack-suffix (member tree-addition stack)))
      (find-if #'(lambda (x) (and (typep x 'choice-entry)
                                  (eq (mode x) 'pop-toplevel-task)))
               stack-suffix))))


(defun replan-from-failure (domain failed-tree-node search-state &key (verbose 0))
  (let ((*verbose* verbose)
        (*domain* domain))
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
      (seek-plans-stack search-state domain :repairable t))))

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

(defun freeze-state (executed failed-action divergence search-state)
  "Arguments:
PLAN: sequence of primitive actions.
EXECUTED: Prefix of the plan that has been executed.
FAILED-ACTION: First action whose preconditions are clobbered,
          if any.
DIVERGENCE: Divergence between the expected state after
      EXECUTED, specified as (([:ADD|:DELETE] <fact>)+)
SEARCH-STATE: Search state object.

  Returns:
Modified search state object."
  (let* ((world-state (world-state search-state))
         ;; this is the tag or the "failed" action -- really the one
         ;; that gave an unexpected result.  So we want to undo all the
         ;; actions AFTER this one
         (world-state-tag (* (if (some #'numberp executed)
                               (/ (length executed) 2)
                               (length executed))
                             ;; magic constant for the tag increment per
                             ;; operator.
                             2))
         ;; can't correctly apply state updates beyond here
         (failed-action-tag (tag-for-action failed-action))
         (new-state-obj (shop2.common::copy-state world-state)))
    (assert (integerp world-state-tag))
    #+nil(break "Inside FREEZE-STATE")
    ;; this gives us the state right after the execution of the "failed" action.
    (shop2.common:retract-state-changes new-state-obj (1+ world-state-tag))
    #+ignore(break "Inside FREEZE-STATE, before adding divergences, world state is: ~S" new-state-obj)
    ;; now put the divergences into effect, taking sleazy advantage of the fact that the
    ;; world state tag increments by two.
    (let ((new-tag 
            (shop2.common:tag-state new-state-obj 1)))
      (iter (for (op fact) in divergence)
        (ecase op
          (:add (shop2.common:add-atom-to-state fact new-state-obj 0 :execution-divergence))
          (:delete (shop2.common:delete-atom-from-state fact new-state-obj 0 :execution-divergence))))
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
      (format t "At start of plan repair, state is: ~%")
      (print-current-state :state new-state-obj
                           :sorted t)
      #+nil (break "Check it out..." )

      search-state)))

#+ignore
(progn
  (defvar *saved-state* nil)
  (excl:def-fwrapper save-frozen-state (&rest args)
    (setf *saved-state* (excl:call-next-fwrapper)))
  (excl:fwrap 'freeze-state 'save-frozen-state 'save-frozen-state))
