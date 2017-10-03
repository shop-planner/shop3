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
  (let ((failed (subtree:find-failed-task domain plan plan-tree executed
                                                 divergence :plan-tree-hash plan-tree-hash))
        (new-search-state (freeze-state executed divergence search-state)))
    #+nil(break "Inspect NEW-SEARCH-STATE.")
    (multiple-value-bind (new-plans new-plan-trees lookup-tables final-search-state)
        (replan-from-failure domain failed new-search-state :verbose verbose)
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
            final-search-state)))))))

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
  (let ((*verbose* verbose))
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

(defun extract-suffix (plan prefix)
  ;; hardened to handle costs... yuck
  (let ((plan-copy (copy-list plan)))
   (iter (for x in prefix)
     (as y = (first plan-copy))
     (if (numberp x)
         (cond ((eql x y)               ; match a number
                (pop plan-copy)
                (collecting y into new-prefix))
               ((listp y) ; x is a cost but y is an action - do nothing
                )
               (t (error "Mismatching prefix ~s of ~s" x plan-copy)))
         ;; x is an operator/action
         (cond ((eq x y)
                (pop plan-copy)
                (collecting y into new-prefix))                     ; match
               ((numberp y)
                (pop plan-copy)
                (let ((y (first plan-copy)))
                  (if (eq x y)
                      (progn (pop plan-copy)
                             (collecting y into new-prefix))
                      (error "Mismatch in plan: ~s ~s" x plan-copy))))
               (t (error "Mismatching prefix ~s of ~s" x plan-copy))))
     (finally
      (cond ((and (numberp (first plan-copy))
                   (some 'numberp (rest plan-copy)))
             ;; move cost of last executed action
             (let ((last-cost (pop plan-copy)))
               (setf new-prefix (append new-prefix (list last-cost)))))
            ((numberp (first plan-copy))
             ;; there are no more numbers in the plan
             (pop plan-copy)))
      (return
        (values new-prefix plan-copy))))))

(defun freeze-state (executed divergence search-state)
  "Arguments:
PLAN: sequence of primitive actions.
EXECUTED: Prefix of the plan that has been executed.
DIVERGENCE: Divergence between the expected state after
      EXECUTED, specified as (([:ADD|:DELETE] <fact>)+)
SEARCH-STATE: Search state object.

  Returns:
Modified search state object."
  (let* ((world-state (world-state search-state))
         ;; this is the tag or the "failed" action -- really the one
         ;; that gave an unexpected result.  So we want to undo all the
         ;; actions AFTER this one
         (world-state-tag (if (some #'numberp executed)
                              (/ (length executed) 2)
                              (length executed)))
         (new-state-obj (shop2.common::copy-state world-state)))
    (assert (integerp world-state-tag))
    #+NIL(break "Inside FREEZE-STATE")
    ;; this gives us the state right after the execution of the "failed" action.
    (shop2.common:retract-state-changes new-state-obj world-state-tag)
    ;; now put the divergences into effect....
    (iter (for (op fact) in divergence)
      (ecase op
        (:add (shop2.common:add-atom-to-state fact new-state-obj 0 :execution-divergence))
        (:delete (shop2.common:delete-atom-from-state fact new-state-obj 0 :execution-divergence))))
    ;; now make it impossible to backtrack before this point...
    (setf (shop2.common::tagged-state-block-at new-state-obj) world-state-tag)
    ;; now roll forward again
    (let ((suffix 
            (subseq (shop2.common::tagged-state-tags-info world-state) 0
                    (or (position world-state-tag (shop2.common::tagged-state-tags-info world-state)
                                  :key 'first)
                        (error "Couldn't find world state tag ~D in state" world-state-tag)))))
      (setf (shop2.common::tagged-state-tags-info new-state-obj)
            ;; NCONC is safe because SUBSEQ creates fresh copy.
            (nconc suffix (shop2.common::tagged-state-tags-info new-state-obj))))
    (break "Inspect NEW-STATE-OBJ.")
    ;; now put the new world state in place...
    (setf (world-state search-state) new-state-obj)
    search-state)) 
