(in-package :shop3-minimal-subtree)

;;; if necessary, look up the domain from its name.
(defmethod find-failed-task :around ((domain symbol) plan plan-tree
                                     executed divergence &key plan-tree-hash)
  (find-failed-task (find-domain domain) plan plan-tree
                    executed divergence
                    :plan-tree-hash plan-tree-hash))

;;; this is needed for SBCL, which interprets the ANSI spec about
;;; standard method combination more strictly than others.
(defmethod find-failed-task ((domain symbol) plan plan-tree
                             executed divergence &key plan-tree-hash)
  (declare (ignorable domain plan plan-tree executed divergence plan-tree-hash))
  (error "Around method should avoid this primary method altogether."))

;;; Note: the current version of this file assumes that the primitive
;;; task s-expressions in PLAN and PLAN-TREE are EQ -- i.e., pointers
;;; to the same lists.  If you pull the plans out of SHOP2, that will
;;; be the case, but if you save and reload plans, it might not be.
;;; Caveat lisper!
(defmethod find-failed-task ((domain domain) plan
                             (plan-tree plan-tree:tree-node) executed divergence
                             &key plan-tree-hash)
  "Default method for FIND-FAILED-TASK."
  (when (find-if 'numberp plan)
    ;; we haven't removed the costs
    (setf plan (remove-costs plan)))
  (let ((plan-suffix (find-plan-suffix plan executed)))
    (iter outer (for plan-step in plan-suffix)
      (iter (for next in (find-checking-path (find-plan-step plan-step plan-tree plan-tree-hash)))
        (unless (typep next 'pseudo-node) ;ordered and unordered nodes
          (when (clobbered-p next divergence)
            ;; we can't replan from primitives, so if we find a failing
            ;; primitive, return its parent
            (iter (while (or (typep next 'primitive-tree-node)
                              (typep next 'pseudo-node)))
              (setf next (tree-node-parent next)))
            (return-from find-failed-task (values next plan-step)))))))
    nil)            ; no threatened step found

(defun find-checking-path (tree-node)
  "Find and return the series of tree nodes that should be
checked above TREE-NODE.  Returns a list in order of
checking (i.e., top-down)."
  (iter (with next = tree-node)
    (with retval = nil)
    (while next)
    (push next retval)
    (if (leftmost-p next)
      ;; reset next pointer and repeat the loop
      (setf next (tree-node-parent next)) ; will be NIL at the root
      ;; or we are done
      (finish))
    (finally (return retval))))

(defun leftmost-p (tree-node)
  (let ((parent (tree-node-parent tree-node)))
    (when parent
      (let ((sibling-list (complex-tree-node-children parent)))
        (zerop (position tree-node sibling-list))))))

;;; FIND-PLAN-SUFFIX will return a suffix that has costs in it.
(defun find-plan-suffix (orig-plan executed-prefix)
  (iter (for step in executed-prefix)
    (with plan = orig-plan)
    ;; remove costs, if necessary...
    (when (numberp step) (next-iteration))
    (when (numberp (first plan))
      (setf plan (rest plan)))
    ;; testing here must be EQ, because there can be multiple copies
    ;; of a single ground action in the plan sequence
    (if (eq step (first plan))
        (setf plan (rest plan))
        (error "Executed plan step ~S is not part of plan: ~S"
               step orig-plan))
    (finally (return plan))))



(defun clobbered-p (tree-node divergence)
  (let ((causal-links (tree-node-dependencies tree-node)))
    (iter (for link in causal-links)
      (as prop = (prop link))
      ;; is there a clobbered causal link
      (thereis (if (eq (first prop) 'not)
                   (find-adder (second prop) divergence)
                   (find-deleter prop divergence)))
      ;; if not return nil
      (finally (return nil)))))

(defun find-adder (prop divergence-list)
  (find-if #'(lambda (x) (and (eq (first x) :add)
                              (equalp (second x) prop)))
           divergence-list))

(defun find-deleter (prop divergence-list)
  (find-if #'(lambda (x) (and (eq (first x) :delete)
                              (equalp (second x) prop)))
           divergence-list))
    
