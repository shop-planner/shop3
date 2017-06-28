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

* Test problems

We build a plan as shown in 
#+CAPTION: Test plan subtree
[[file:figures/test-shop2-plan.pdf]]

From this, given that the initial state establishes =C= and =D=, we
can run the following test-cases:
1. After =!OP1=, clobber =B=.  =!OP2= should be marked as first failed
   (and not =TASK2=).
2. After =!OP2=, clobber =B=.  =!OP3= should now be marked as the
   first failed.
3. Fencepost case: clobber =C= after /no/ actions.  Should cause
   =!OP1= to fail.
4. After =OP3=, add =C=, should cause =OP4= to fail.
5. After =OP4=, add =C=, should cause =OP5= to fail.
6. After =OP1=, delete =A=, should cause =TASK3= node to fail.
7. After =OP2=, delete =A=, should cause =TASK2= node to fail.
8. After =OP2=, add =D=, should cause =TASK5= node to fail.

All of these pass in hand-tests.  Need to automate these tests.


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

* Secondary preconditions
If we build causal links for secondary preconditions as well as
primary preconditions, then we get failure detection that is complete,
but not sound.

But if we /didn't/ build causal links for the secondary preconditions,
then we get failure detection that is sound, but incomplete, which
would be worse.

A possibility would be to use the causal links that are complete, but
unsound, as a /prefilter/, and as part of the plan repair routine,
check to see if there's a real plan failure, or a false positive.

* Sound and complete causal links

I propose the following limitation on the domain language in order to
achieve sound and complete causal links:

1. Typed STRIPS dialect of PDDL for the primitives;
2. Conjunctive preconditions for the methods, no SHOP2 special
   language features, just preconditions whose variables are either
   from the task parameters or that are in the subtask parameter
   list.
3. All tasks should be ground when added to the plan.  That is, all
   task parameters should be ground before the preconditions are
   checked.

** Design notes

What we want to achieve is that when a task fails, we can
unambiguously specify where to begin replanning.

So, consider a primitive (STRIPS PDDL task): since the task is ground
when it's instantiated, there will be no unbound variables in the
preconditions.  This means that the above techniques for computing
causal links will be sound and complete -- it is not possible to
replan the primitive operator without replanning the parent method
that introduced it into the plan.

The method preconditions are more problematic.  We must allow method
preconditions with unbound variables, or we cannot have plans that
feature a choice of variable bindings anywhere.  But this means that
if we start replanning from a method's parents, we could have missed
an opportunity to just replan the method (by re-binding variables for
its children).  That would make this incomplete (we fail to detect
that there's a replanning opportunity at the method).  Alternatively,
if we start replanning from the method, we could be unsound: it's
possible that the method itself hasn't really failed.

One possibility that is messy, but would be sound and complete, would
be to treat preconditions with bound variables different from
preconditions with unbound variables.  These would be two different
"colors" of preconditions.  The preconditions for bound variables
would work as for operator preconditions: if they fail, then the
parent task must be replanned.  But if preconditions for *unbound*
variables fail, then the task node itself should be replanned
(attempted, anyway).
