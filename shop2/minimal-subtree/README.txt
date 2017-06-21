# -*- Mode:org; -*- 
#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \n:nil ^:t arch:headline
#+OPTIONS: author:t broken-links:nil c:nil creator:nil
#+OPTIONS: d:(not "LOGBOOK") date:t e:t email:nil f:t inline:t num:t
#+OPTIONS: p:nil pri:nil prop:nil stat:t tags:t tasks:t tex:t
#+OPTIONS: timestamp:t title:t toc:t todo:t |:t
#+TITLE: Specification for Failed Subtree Finder
#+DATE: <2017-06-20 Tue>
#+AUTHOR: Robert P. Goldman
#+EMAIL: rpgoldman@sift.net
#+LANGUAGE: en
#+SELECT_TAGS: export
#+EXCLUDE_TAGS: noexport
#+CREATOR: Emacs 25.1.1 (Org mode 8.3.5)

* Specification

The specification for =FIND-FAILED-TASK= can be found in the doc
string for that function (see [[minimal-subtree.lisp]]).  Currently:

#+BEGIN_QUOTE
   "Return the leftmost failed task in the PLAN that has a
failed precondition.
  Arguments:
  PLAN: Sequence of primitive actions
  PLAN-TREE: *Enhanced* SHOP2 plan tree with causal links
  EXECUTED: Prefix of PLAN that has been executed
  DIVERGENCE: Divergence between the expected state of the
world after EXECUTED and the actual state of the world.
Specified as follows (([:ADD|:DELETE] <facts>)*).
That is, any number of lists headed by the :ADD or :DELETE
operators, and followed by a list of facts (ground literals).
  Returns:
  Leftmost, minimal task (plan subtree) that has one or more
broken causal links."
#+END_QUOTE

** NOTE about DIVERGENCE

Might be better to make the format of =DIVERGENCE= more rigid:

=(([:ADD|:DELETE] <fact>)+)=

Then we can check the clobbering relationship in much cleaner code.  I
suppose we could explode the divergence into this format ourselves, if
it's submitted in the more terse format described above.

* Suggestion
Make PLANOBJ structure that will hold plan, plan sequence, and state
object.  Also domain and problem reference, perhaps.  If we make this
and a load form for it, it will simplify maintaining state of a plan
server, doing repeatable tests, etc.

* Algorithm
** (possibly)
Make a table of links from primitives into the plan structure (this
may already be computed and stored -- check).
** Record all executed tasks
- "fast forward" along the plan, to reach the first non-executed step
- similarly, fast forward through the state structure (we may need to
  construct a state structure).
- at the same time, track the tasks performed in the plan tree.
** Find threatened task
*** First draft
Iteration should work as follows:
#+BEGIN_SRC common-lisp
  (iter outer (for plan-step in plan-suffix)
    (iter (with next = plan-step)
      (while next)
      (if (clobbered-p next)
          (return-from outer next)
          (setf next
                (if (leftmost-p next)
                  (parent next)
                  nil))))
    (finally (return nil))) ; no threatened task found
#+END_SRC
where =plan-suffix= is the list of 
*** Problem   
For left-most tasks in a plan tree, the parent should be checked
/before/ the child, because the parent's precondition is tested first
and justifies the inclusion of the child.  So we revise:
*** Second draft
#+BEGIN_SRC common-lisp
  (iter outer (for plan-step in plan-suffix)
    (iter (with next = plan-step)
      (while next)
      (when (leftmost-p next)
        ;; reset next pointer and repeat the loop
        (setf next parent)
        (next-iteration))
      (if (clobbered-p next last-executed discrepancy)
          (return-from outer next)
          (setf next
                (if (has-children next)
                    (leftmost-child next)
                    ;; else we are done with tree above
                    ;; PLAN-STEP, and need to move to
                    ;; next primitive
                    nil))))
    (finally (return nil))) ; no threatened task found
#+END_SRC
*** =LEFTMOST-P=
=(LEFTMOST-P TASK)= is true if =TASK= is the leftmost child of its parent.
*** Definition of =CLOBBERED-P=
#+BEGIN_SRC common-lisp
  (defun clobbered-p (task-node last-executed discrepancy)
    (let* ((links-in (causal-links-to task-node))
           (filtered (remove-if-not #'(lambda (x) (eq (source x) last-executed))
                                    links-in)))
      (unless filtered (return-from clobbered-p nil))
      (some #'(lambda (link)
                (contradicts (prop link) discrepancy))
            filtered)))
                                    
#+END_SRC

* Replanning
Replanning loop will run as follows, given =FAILED-TASK=:
#+BEGIN_SRC common-lisp
  (iter (with target = (if (primitivep failed-task)
                           (parent failed-task)
                           failed-task))
    (for new-plan = (replan target))
    (if new-plan
        (return new-plan)
        (setf target (parent target)))
    (finally (return nil)))               ; comprehensive failure
#+END_SRC
The reason for the check in the initial check of target is that
primitive tasks are not replannable in SHOP2, but complex tasks might
have alternative methods that could be applied.

