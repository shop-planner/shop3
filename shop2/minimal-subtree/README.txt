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

* Definition of replanning

Our definition of replanning must specify conditions for successful
execution of an HTN plan under disturbances.  A non-trivial definition
must explain when a repaired plan does and does not successfully
satisfy the top level task specification.

# FIXME: add formal definition of successful execution.

* Sound and complete causal links

This is a revision of the earlier version.  Here I introduce a very
restricted language, and later we extend it....

** "STRIPS SHOP2"

The STRIPS dialect of SHOP2 is as follows:

1.  Primitives are PDDL typed STRIPS operators (i.e., equivalent to
   PDDL requirements of =:typing :negative-preconditions=).

2.  The method grammar is as follows (done in the style of the PDDL
   2.1 grammar):

#+BEGIN_EXAMPLE
<method> :: (:method <method-task>
             :method-name <symbol>
             :precondition <GD>
             :task-net <task-net>)

<method-task> ::= (<task-symbol> <task-arg>*)
<task-arg> ::= <name> | [<variable> - <type>]
<task-net> ::= ([:ordered]? <task-net-component>+) |
               (:unordered <task-net-component>+)
<task-net-component> ::= (<task-symbol> <method-task-arg>*)
<method-task-arg> ::= <term>

#+END_EXAMPLE
=<GD>= and =<typed list (variable)>= are as defined in the PDDL 2.1
grammar.  =<task-symbol>= is the same as PDDL 2.1's =<action-symbol>=,
however additionally each =<task-symbol>= is classified as either
/primitive/ or /complex/.  =<method-task-args>= and arguments in
preconditions, if variables, must be elements of the parameter list.
=<method-name>= is also equivalent to =<action-symbol>=: it designates
a unique method for achieving the =<task-spec>=.

This is equivalent to the following limitations, which ensure sound
and complete causal links:

1. Typed STRIPS dialect of PDDL for the primitives;
2. Conjunctive preconditions for the methods, no SHOP2 special
   language features, just preconditions whose variables are 
   from the task parameters.
3. All tasks will be ground when added to the plan.  That is, all
   task parameters should be ground before the preconditions are
   checked.

*** Replanning

We find the first task in the unexecuted suffix of the plan that has a
causal link that is clobbered by one of the
/discrepancies/[fn:discrepancies].  We refer to this as the /failed
task/.  Replanning will be initiated at the parent of the failed task,
if the failed task is a primitive task.  Replanning will be initiated
at the task itself, if the failed task is a complex task.

The rationale for the above is that primitive tasks cannot be
replanned.  There is only one way that they can be executed, and if
their preconditions are clobbered, they are not executable.  Ergo, the
first opportunity to replan is at the parent of the failed primitive
task.

In contrast, it /is/ possible to replan a complex task since there can
be multiple different methods for the same task.  Ergo, replanning
begins at the failed task, when the failed task is complex.

*** Soundness and completeness

With respect to the above definition of replanning, our methods of
identifying failed tasks are sound and complete.

# FIXME: add proof sketch

*** Remark

One alternative way of specifying this dialect would be to permit
tasks in the task network to have unbound variables, but still dictate
that their values would have to be assigned before the tasks are added
to the plan.  In this case it would be possible to have /some/ choice
in how a method would be instantiated, but any such method /schema/
would be equivalent to some finite (but possibly large) set of ground
schemata.  In this case the above replanning approach would still be
correct, because it wouldn't be possible to change the variable
bindings for any subtask without replanning its parent task.  This
might be a more congenial way to specify "STRIPS SHOP2."

** "STRIPS+ SHOP2"

The above dialect is extremely limited.  Unless we allow methods to
introduce variable bindings for their task networks, we cannot have
planning problems that feature a choice of variable bindings anywhere.

So we enhance the method grammar as follows:

#+BEGIN_EXAMPLE
<method> :: (:method <method-task>
             :method-name <symbol>
             :variables <typed-list (variable)>
             :precondition <GD>
             :task-net <task-net>)
#+END_EXAMPLE

We also relax the constraint that variables appearing in the task-net
or the preconditions must be method task parameters.  This makes it
possible for a method to use its preconditions to bind the variables
that do not appear in its parameters, and adds a new branching factor
to the planning process.

*** Soundness and completeness

The above extension does not change the soundness or completeness of
the causal link and failed task computation.  Note that any
newly-introduced variables in a method expansion must be bound by the
preconditions of the method in which they are introduced.
Accordingly, the child tasks introduced by the method expansion will
still be ground.  If preconditions that do /not/ bind new variables
are clobbered, then the above discussion establishes the need to
replan, as in STRIPS PDDL.  The only difference is that there are now
preconditions that bind variables for child tasks.  If those
preconditions are violated, the task introducing them will be marked
as failing, and must be replanned.  But this is correct, because it is
possible that the task has alternative bindings for these variables,
or that there are alternative methods for the task.  Note that 
a method schema with the new, expanded syntax is still equivalent
to a finite collection of ground methods.

** ADL SHOP2

** Design notes

[*Note:* These are not up-to-date; the above discussion has been
significantly rewritten without a revision of the design notes.]

What we want to achieve is that when a task fails, we can
unambiguously specify where to begin replanning.  In this dialect, 

In the above dialect, every task is ground when it's instantiated,
there will be no unbound variables in the preconditions.  This means
that the above techniques for computing causal links will be sound and
complete: It is not possible to replan a primitive operator
without replanning the parent method that introduced it into the
plan.  Similarly, 

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

* Footnotes

[fn:discrepancies] The term /discrepancy/ must be defined.