(in-package :shop2-minimal-subtree)

(defgeneric find-failed-task (domain plan plan-tree executed divergence)
  (:documentation
   "Return the leftmost failed task in the PLAN that has a
failed precondition.
  Arguments:
  PLAN: Sequence of primitive actions
  PLAN-TREE: *Enhanced* SHOP2 plan tree with causal links
  EXECUTED: Prefix of PLAN that has been executed
  DIVERGENCE: Divergence between the expected state of the
world after EXECUTED and the actual state of the world.
Specified as follows (([:ADD|:DELETE] <fact>)+).
That is, any number of lists headed by the :ADD or :DELETE
operators, and followed by a list of facts (ground literals).
  Returns:
  Leftmost, minimal task (plan subtree) that has one or more
broken causal links."))