(in-package :shop2)

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
      (find-if #'(lambda (x) (typep x 'choice-entry)) stack-suffix))))


(defun replan-from-failure (domain failed-tree-node search-state &key (verbose 0))
  (let ((*verbose* verbose))
    (when (>= *verbose* 2)
        (format t "~&World state before backjump is:~%")
        (pprint (state-atoms (world-state search-state))))
    (let ((failed-choice-node (find-failed-choice-entry failed-tree-node search-state)))
      (when (>= *verbose* 1)
        (format t "~&Backjumping to ~A~%"))
      (stack-backjump search-state failed-choice-node)
      (when (>= *verbose* 2)
        (format t "~&World state after backjump is:~%")
        (pprint (state-atoms (world-state search-state))))
      (seek-plans-stack search-state domain))))

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
         (world-state-tag (length executed))
         (new-state-obj (shop2.common::copy-state world-state)))
    ;; this gives us the state right after the execution of the "failed" action.
    (shop2.common:retract-state-changes new-state-obj (1+ world-state-tag))
    ;; now put the divergences into effect....
    (iter (for (op fact) in divergence)
      (ecase op
        (:add (shop2.common:add-atom-to-state fact new-state-obj 0 :execution-divergence))
        (:delete (shop2.common:delete-atom-from-state fact new-state-obj 0 :execution-divergence))))
    ;; now make it impossible to backtrack before this point...
    (setf (shop2.common::tagged-state-block-at new-state-obj) (1+ world-state-tag))
    ;; now roll forward again
    (let ((suffix (copy-seq (rest (member world-state-tag (shop2.common::tagged-state-tags-info world-state)
                                     :key 'first)))))
      (alexandria:appendf (shop2.common::tagged-state-tags-info new-state-obj)
                          suffix))
    ;; now put the new world state in place...
    (setf (world-state search-state) new-state-obj)
    search-state))
