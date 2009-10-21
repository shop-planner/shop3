;;; -*- Mode: common-lisp; package: shop2; -*-
;;;
;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;;
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;;
;;; The Original Code is SHOP2.
;;;
;;; The Initial Developer of the Original Code is the University of
;;; Maryland. Portions created by the Initial Developer are Copyright (C)
;;; 2002,2003 the Initial Developer. All Rights Reserved.
;;;
;;; Additional developments made by Robert P. Goldman, John Maraist.
;;; Portions created by Drs. Goldman and Maraist are Copyright (C)
;;; 2004-2007 SIFT, LLC.  These additions and modifications are also
;;; available under the MPL/GPL/LGPL licensing terms.
;;;
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.
;;; ----------------------------------------------------------------------

;;; Smart Information Flow Technologies Copyright 2006-2007 Unpublished work
;;;
;;; GOVERNMENT PURPOSE RIGHTS
;;;
;;; Contract No.         FA8650-06-C-7606,
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;;
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.


(in-package :shop2)

;;; ------------------------------------------------------------------------
;;; Method and operator application
;;; modified to use the new SHOP2 operator syntax, which includes preconditions (swm)
;;; ------------------------------------------------------------------------

(defstruct (operator :named (:type list))
  "This structure definition was added in order to make the
access to operator attributes self-documenting.  Later, the list
structure could be removed, and a true struct could be used instead."
  (head nil :type list)                 ;this is the operator expression itself...
  preconditions
  deletions
  additions
  (cost-fun nil))

(defmethod apply-operator ((domain domain) state task-body operator protections depth
                             in-unifier)
  "If OPERATOR is applicable to TASK in STATE, then APPLY-OPERATOR returns
five values:
1.  the operator as applied, with all bindings;
2.  a state tag, for backtracking purposes;
3.  a new set of protections;
4.  the cost of the new operator;
5.  unifier.
This function also DESTRUCTIVELY MODIFIES its STATE argument.
Otherwise it returns FAIL."
  (let* ((*domain* domain)
         (standardized-operator (standardize operator))
         (head (operator-head standardized-operator))
         (preconditions (operator-preconditions standardized-operator))
         (deletions (operator-deletions standardized-operator))
         (additions (operator-additions standardized-operator))
         operator-unifier pre protections1 tempdel tempadd statetag
         head-subbed dels-subbed adds-subbed pu unifier
         cost-value cost-number)

    ;; added rudimentary arity-checking...
    (when (and (well-formed-listp head)
               (well-formed-listp task-body))
      (unless
          (= (length task-body) (length head))
      (cerror "Continue (operator application will fail)"
              'task-arity-mismatch
              :task task-body :library-task head
              :library-entry operator)))

    ;; new tracing facility for debugging
    (when *traced-tasks*
      (when (member (first head) *traced-tasks*)
        (break "Applying operator for ~A~%~S" (first head) task-body)))

    (setq operator-unifier (unify head (apply-substitution task-body in-unifier)))

    (cond
     ((eql operator-unifier 'fail) (values 'fail protections 0))
     (t
      (setf operator-unifier
            (compose-substitutions operator-unifier in-unifier))
      ;; first check the preconditions, if any
      (when preconditions
        (setq pre (apply-substitution preconditions operator-unifier))
        (setq pu (shopthpr:find-satisfiers pre state t 0 :domain domain))
        (unless pu
          (trace-print :operators (first head) state
                       "~2%Depth ~s, inapplicable operator ~s~%     task ~s.~%     Precondition failed: ~s.~%"
                       depth
                       (first head)
                       (apply-substitution task-body unifier)
                       pre
                       )
          (return-from apply-operator (values 'fail preconditions 0))))
      (setq unifier (compose-substitutions operator-unifier (car pu)))
      (setq dels-subbed (apply-substitution deletions unifier))
      (setq adds-subbed (apply-substitution additions unifier))
      (setq head-subbed (apply-substitution head unifier))
      ;; added this to update the tree [2003/06/25:rpg]
      ;(setq *current-tree* (apply-substitution *current-tree* unifier))
                                        ;(format t "~%Sat to explain: ~s" (apply-substitution preconditions unifier))

      ;; at this point UNIFIER is bound to the results of unifying
      ;; TASK-BODY with the operator's head and then retrieving
      ;; the first set of bindings that satisfy the preconditions.
      ;; we will return this unifier later on, assuming that
      ;; bindings from add and delete lists should not be plugged
      ;; in. [2004/01/20:rpg]
      (when *explanation*
          (setq head-subbed `(,@(cons (first head-subbed)
                                      (mapcar #'list
                                              (rest (second operator))
                                              (rest head-subbed)))
                                :explanation
                                ,(shopthpr:explain-satisfier
                                  (apply-substitution preconditions unifier)
                                  state))))
      (trace-print :operators (first head) state
                   "~2%Depth ~s, applying operator ~s~%      task ~s~%       del ~s~%       add ~s"
                   depth
                   (first head)
                   (apply-substitution task-body unifier)
                   dels-subbed
                   adds-subbed)
      (setq cost-value
        (eval (apply-substitution
               (operator-cost-fun standardized-operator) unifier)))
      (setq cost-number (if (numberp cost-value) cost-value 1))
      ;; process DELETE list
      (dolist (d dels-subbed)
        (unless (eql 'fail d)
          (if (eql (car d) 'forall)
              (let ((bounds (third d)) (dels (fourth d)) mgu2 tempd)
                (setq mgu2 (shopthpr:find-satisfiers bounds state nil 0
                                                     :domain domain))
                (dolist (m2 mgu2)
                  (setq tempd (apply-substitution dels m2))
                  (dolist (d1 tempd)
                    (setq tempdel
                      (shop-union (list d1) tempdel :test #'equal)))))
            (setq tempdel (shop-union (list d) tempdel :test #'equal)))))
      ;; process ADD list
      (dolist (a adds-subbed)
        (unless (eql 'fail a)
          (if (eql (car a) 'forall)
              (let ((bounds (third a)) (adds (fourth a)) mgu2 tempa)
                (setq mgu2 (shopthpr:find-satisfiers bounds state nil 0
                                                     :domain domain))
                (dolist (m2 mgu2)
                  (setq tempa (apply-substitution adds m2))
                  (dolist (a1 tempa)
                    (setq tempadd
                      (shop-union (list a1) tempadd :test #'equal)))))
            (setq tempadd (shop-union (list a) tempadd :test #'equal)))))

      (setq protections1 protections)
      (setq statetag (tag-state state))
      ;; process PROTECTIONS generated by this operator
      (dolist (d tempdel)
        (if (eql (car d) :protection)
            (setq protections1
              (delete-protection
               protections1 (second d) depth (first head) state))
          (delete-atom-from-state d state depth (first head))))

      (dolist (a tempadd)
        (unless (eql (car a) :protection)
          ;; added this error-checking.  I can't think of a case where
          ;; it's ok to add a non-ground literal to the
          ;; state. [2004/02/17:rpg]
          ;; the above check should probably be moved to
          ;; add-atom-to-state... [2008/02/07:rpg
          (unless (groundp a)
                 (error "Attempting to add non-ground literal ~S to state."
                        a))
          (add-atom-to-state a state depth (first head))))

      (if (protection-ok state protections1 head)
          (setq protections protections1)
        (progn
          (retract-state-changes state statetag)
          (return-from apply-operator (values 'fail 'fail protections 0))))

      (dolist (a tempadd)
        (when (eql (first a) :protection)
          (unless (find-satisfiers (list (second a)) state t 0 :domain domain)
            (error "Adding a protection ~A that is violated in state." a))
          (setq protections
            (add-protection protections (second a)
                            depth (first head) state))))

      (trace-print :operators operator state "~&Operator successfully applied.")

      (values head-subbed statetag
              protections cost-number
              unifier)))))

(defun well-formed-listp (l)
  (null (cdr (last l))))


;;; If METHOD is applicable to TASK in STATE, then APPLY-METHOD returns the
;;; resulting list of reductions.  Otherwise it returns NIL.
(defmethod apply-method ((domain domain) state task-body method protections depth
                         in-unifier)
  (declare (ignore protections))        ; do we really want to ignore protections?
  (let ((standardized-method (standardize method))
        task-unifier state-unifiers pre tail)

    (when (and (well-formed-listp (second standardized-method))
               (well-formed-listp task-body))
      (unless (= (length (second standardized-method))
                 (length task-body))
      (error 'task-arity-mismatch
             :task task-body
             :library-task (second standardized-method)
             :library-entry method)))

    ;; see if the standardized-method's head unifies with TASK-BODY
    (setq task-unifier (unify (second standardized-method)
                              (apply-substitution task-body in-unifier)))
    (when *traced-tasks*
      (when (member (first task-body) *traced-tasks*)
        (break "Attempting to apply a method for ~A:~%~S"
               (first task-body) task-body)))

    (unless (eql task-unifier 'fail)
      (setq task-unifier (compose-substitutions in-unifier task-unifier))
      ;; STANDARDIZED-METHOD's CDDR is a list
      ;; (label_1 pre_1 d_1 label_2 pre_2 d_2 ...) which acts like an
      ;; if-then-else: we look for the first true pre_i, and then evaluate d_i
      (do* ((body (cddr standardized-method) (cdddr body)))
           ((null body) nil)

        ;; apply v to PRE and TAIL
        (setq pre (apply-substitution (second body) task-unifier))
        (setq tail (apply-substitution (third body) task-unifier))

        ;; check for tracing
        (when *traced-methods*
          (when (member (first body) *traced-methods*)
            (break "Attempting to apply method ~A" (first body))))

        (trace-print :methods (first body) state
                           "~2%Depth ~s, trying method ~s~%      for task ~s~%"
                           depth
                           (first body)
                           task-body)

        ;; find all matches to the current state
        (setq state-unifiers (shopthpr:find-satisfiers pre state nil 0
                                                       :domain domain))

        (if state-unifiers
            (let* ((answers-with-duplicates
                    (mapcan
                     #'(lambda (unifier)
                         (let ((unifier (compose-substitutions (copy-tree unifier) task-unifier)))
                           (mapcar
                            #'(lambda (reduction)
                                (cons
                                 (cons (first body) reduction)
                                 ;;keep the unifier around a bit longer...
                                 ;; [2003/06/25:rpg]
                                 unifier))
                            (force-immediate-reduction
                             (eval (apply-substitution tail unifier))))))
                     state-unifiers))
                   (answers-and-unifiers
                    (remove-duplicates answers-with-duplicates
                                       ;; added this to ignore the unifiers....
                                       :key #'car
                                       :test #'equal :from-end t))
                   (answers (mapcar #'car answers-and-unifiers))
                   (unifiers (mapcar #'cdr answers-and-unifiers)))
              (trace-print :methods (first body) state
                           "~2%Depth ~s, applicable method ~s~%      task ~s~%reductions ~s"
                           depth
                           (first body)
                           task-body
                           answers)
              (return-from apply-method (values answers unifiers)))
          (trace-print :methods (first body) state
                     "~2%Depth ~s, inapplicable method ~s~%      task ~s"
                     depth
                     (first body)
                     task-body))))))

;;; This function forces there to be at least one immediate task in any
;;;  reduction so that when a method is reduced, it is immediately
;;;  reduced all the way to the bottom level.  For ordered reductions,
;;;  the function makes the first task immediate.  For unordered
;;;  reductions which have no immediate element, it splits the reduction
;;;  into seperate reductions each of which have exactly one immediate
;;;  element UNLESS the reduction already has an immediate element.  For
;;;  nested reductions, it applies the appropriate fix at each level.
;;;
;;; Examples:
;;;  (force-immediate ; non-nested
;;;    '((:ordered (:task a) (:task b))
;;;      (:unordered (:task x) (:task y))))
;;;  =>
;;;   ((:ordered (:task :immediate a) (:task b))
;;;    (:unordered (:task :immediate x) (:task y))
;;;    (:unordered (:task x) (:task :immediate y)))
;;;----
;;;  (force-immediate ; nested
;;;    '((:unordered (:ordered (:task a) (:task b))
;;;                  (:ordered (:task x) (:task y))))
;;;  =>
;;;   ((:unordered (:ordered (:task :immediate a) (:task b))
;;;                (:ordered (:task x) (:task y)))
;;;    (:unordered (:ordered (:task a) (:task b))
;;;                (:ordered (:task :immediate x) (:task y))))
;;;----
;;;  (force-immediate ; nested, but already immediate
;;;    '((:unordered (:ordered (:task a) (:task b))
;;;                  (:ordered (:task :immediate x) (:task y))))
;;;  =>
;;;   ((:unordered (:ordered (:task a) (:task b))
;;;                (:ordered (:task :immediate x) (:task y))))
(defun force-immediate (reductions)
  (mapcan #'force-immediate-reduction reductions))

(defun force-immediate-reduction (reduction)
  (cond
   ((null reduction) nil)
   ((already-immediate-p reduction)
    (list reduction))
   ((eq (first reduction) :task)
    (list `(:task :immediate ,@(get-task-body reduction))))
   ((eq (first reduction) :ordered)
    (mapcar #'(lambda (ordered-term)
    (append (list :ordered ordered-term)
      (rest (rest reduction))))
      (force-immediate-reduction (second reduction))))
   ((eq (first reduction) :unordered)
    (mapcar #'(lambda (unordered-result)
    (cons :unordered unordered-result))
      (force-immediate-unordered (rest reduction))))))

(defun already-immediate-p (reduction)
  (cond
   ((null reduction) nil)
   ((eq (first reduction) :task)
    (eq (second reduction) :immediate))
   ((eq (first reduction) :ordered)
    (already-immediate-p (second reduction)))
   ((eq (first reduction) :unordered)
    (find-if #'already-immediate-p (rest reduction)))))

(defun force-immediate-unordered (unordered-list &optional previous)
  (if (null unordered-list)
      nil
    (append
     (mapcar #'(lambda (unordered-term)
     `(,@previous
       ,unordered-term
       ,@(rest unordered-list)))
       (force-immediate-reduction (first unordered-list)))
     (force-immediate-unordered
      (rest unordered-list)
      (append previous (list (first unordered-list)))))))

(defun get-task-name (task1)
  (if (eq (second task1) :immediate)
    (third task1)
    (second task1)))

(defun get-task-body (task1)
  (if (eq (second task1) :immediate)
    (cddr task1)
    (cdr task1)))


(defun internal-operator-p (operator-name)
  (if (symbolp operator-name)
      (let ((name (symbol-name operator-name)))
        (and
         (>= (length name) 2)
         (equal (elt name 0) #\!)
         (equal (elt name 1) #\!)))
    nil))


;;; This is a hack to pull out the !NOP operations Jason stuck in to get around
;;; another problem.
(defun strip-NOPs (plan)
  (cond ((null plan) nil)
  ((and
    (listp (first plan))
    (eq '!!inop (first (first plan))))
   (strip-NOPs (rest (rest plan))))
  (t
   (cons (first plan)
         (cons (second plan)
         (strip-NOPs (rest (rest plan))))))))



;;; get the list of top-tasks in the main task list
;;; i.e.  the tasks with no predecessor
(defun get-top-tasks (L)
  (if (null L)
    nil
    (ecase (car L)
      (:unordered
       ;; a parallel task list, so recursively call the function on
       ;; each element and append the answers
       (mapcan #'get-top-tasks (cdr L)))
      (:ordered
       ;; a serial task list, so recursively call the function on the
       ;; first element of the list
       (get-top-tasks (cadr L)))
      (:task
       ;; a simple task, so return a list containing the task
       (list L)))))

;;; look inside the L, replace first t1 with the top-level tasks of t2
(defun replace-task-top-list (L t1 t2) ;;t2 is reduction, L is top-tasks
  (append (remove t1 L :count 1 :test #'equal) (get-top-tasks t2)))

;;; look inside the ML, replace first t1 with the top-level
;;; tasks of t2, also, this function will replace t1 in the
;;; main task-list with t2  (t2 is the reduction)
(defun replace-task-main-list (ML t1 t2)
  (let (answer temp temp-found found)
    (if (or (null t1) (null ML))
      (return-from replace-task-main-list (values ML nil))
      (if (equal ML t1)
       ;; if t1 is last task, just return reduction as list
        (return-from replace-task-main-list (values t2 t))))
    (ecase (car ML)
      (:unordered
       ;; a parallel task list, recursively calling the function on
       ;; each element
       (setq answer '(:unordered))
       (dolist (sub-task (cdr ML))
         (if found
           ;; already found t1
           (setq answer (cons sub-task answer))
           (progn
             ;; recursively calling the function on sub-task
             (multiple-value-setq (temp temp-found)
               (replace-task-main-list sub-task t1 t2))
             (if temp-found
               (setq t1 nil
                     t2 nil
                     found t))
             (setq answer (cons temp answer)))))
       (return-from replace-task-main-list (values (reverse answer) found)))

      (:ordered
       ;; a serial task list. recursively calling the function to the
       ;; first element of the list, then append the rest of the list
       (setq answer '(:ordered))
       (multiple-value-setq (temp found)
         (replace-task-main-list (second ML) t1 t2))
       (setq answer (cons temp answer))
       (setq answer (append (reverse answer) (cddr ML)))
       (return-from replace-task-main-list (values answer found)))

      (:task
       ;; a simple task, if it equals to t1, it wouldn't have
       ;; reached here.  so simply return ML and nil
       (return-from replace-task-main-list (values ML nil))))))

;;; this function will delete TASK from both the main task-list ML,
;;; and L, the top-level task list.  Also, it will update
;;; L by appending the children of TASK in the main task-list
(defun delete-task-top-list (L ML TASK)
  (let ((tempML (copy-task-tree ML)) L1)
    (setq L1 (append (remove TASK L :count 1 :test #'equal)
                     (find-next-main-list TASK tempML)))
    (delete-task-main-list tempML TASK nil)

    (unless (car L1)
     ;;;If top-task list is empty just return results of find-next-main-list
      (setq L1 (cdr L1)))
    (return-from delete-task-top-list (values L1 tempML))))

; copy-task-tree is like copy-tree except that anything starting with
; a :task is simply inserted rather than copied.  The motivation here
; is to allow functions that destructively modify the task tree
; structures to not disrupt other copies but to still allow references
; to individual tasks to be compared using eq (as is done in some
; helpers to extract-tree).
(defun copy-task-tree (tt)
  (cond
   ((atom tt) tt)
   ((eq (first tt) :task) tt)
   (t
    (cons (copy-task-tree (first tt))
          (copy-task-tree (rest tt))))))

;;; this function will find the list of children that
;;; decend from task directly - by "children" it refers to tasks that must be
;;; completed next.  Only really applicable in an ordered list
(defun find-next-main-list (task L)
  (if (null L)
    (return-from find-next-main-list (values nil nil t))
    (let (found answer (goNext t))
      (ecase (car L)
        (:unordered
         ;; a parallel task list, recursively calling the function on
         ;; each element, stop when it has found task
         (dolist (sub-task (cdr L))
           (if (equal sub-task task)
             ;; found the task, since it's in a :unordered list, it has
             ;; no child directly
             (if (<= (list-length L) 2)
               ;; a :unordered list with at most one task
               (return-from find-next-main-list (values nil t t))
               (return-from find-next-main-list (values nil t nil)))
             (progn
               (multiple-value-setq (answer found goNext) (find-next-main-list task sub-task))
               (if found
                 (if (and goNext (<= (list-length L) 2))
                   ;; need to getNext child
                   (return-from find-next-main-list (values answer found t))
                   (return-from find-next-main-list (values answer found nil)))))))
         (return-from find-next-main-list (values nil nil t)))

        (:ordered
         ;; a serial task list, check if the first element is equal to task,
         ;; if not, recursively calling the function on the
         ;; first element of the list
         (if (equal (second L) task)
           (if (<= (list-length L) 2)
             (return-from find-next-main-list (values nil t t))
             (return-from find-next-main-list
               (values (get-top-tasks (third L)) t nil)))
           (progn
             (multiple-value-setq (answer found goNext) (find-next-main-list task (second L)))
             (if found
                 ;; if matching task is found in (second L)
               (if goNext
                   ;; if (second L) only contains that task
                 (if (<= (list-length L) 2)
                   ;; need to getNext child
                   (return-from find-next-main-list (values answer found t))
                   (return-from find-next-main-list
                     (values (get-top-tasks (third L)) found nil)))
                 (return-from find-next-main-list (values answer found nil)))))))

        (:task
         ;; a simple task, L can't equal task, if it did, it would have been
         ;; caught on the previous two cases
         (return-from find-next-main-list (values nil nil t)))))))

;;; This function removes TASK from the main-list.
;;; TODO: this function should not only delete the first occurance of TASK, but
;;; also should flatten the list if possible
;;; i.e. (:ordered ... (:ordered t1 t2) ... ) should become
;;; (:ordered ... t1 t2 ...)
(defun delete-task-main-list (L TASK deleted)
  (let (current-deleted
        (templist (cdr L))
        sub-task)
    (if (null L)
      (return-from delete-task-main-list nil)
      (ecase (car L)
        (:unordered
         ;; a parallel task list, recursively calling the function on
         ;; each element.
         (loop while (and (not (null templist)) (not (null (car templist)))) do
               (progn
                 (setf sub-task (car templist))
                 (setq current-deleted (delete-task-main-list sub-task TASK deleted))
                 (setq deleted (if (or deleted current-deleted) t nil))
                 ;;;If task was deleted or detected in recursive call, deleted = true
                 (if (or (<= (list-length sub-task) 1)
                         (eq current-deleted :task))
                   ;;If sub-task is not actually a task,
                   ;;OR if it was found a layer down, remove sub-task from L
                   (setf (car templist) (second templist)
                         (cdr templist) (cddr templist))
                   (setf templist (cdr templist)))))
         ;; clean up code to remove the last nil in the list
         (if (and (> (list-length L) 1)
                  (null (car (last L))))
           (rplacd (nthcdr (- (list-length L) 2) L) nil))
         (return-from delete-task-main-list deleted))

        (:ordered
         ;; a serial task list, recursively calling the function on the
         ;; first element of the list
         (setf sub-task (car templist))
         (setq current-deleted (delete-task-main-list sub-task TASK deleted))
         (setq deleted (if (or deleted current-deleted) t nil))
         ;;If task was deleted or detected in recursive call, deleted = true
         (if (or (<= (list-length sub-task) 1)
                         (eq current-deleted :task))
           ;; remove sub-task from L if sub-task is not a task OR
           ;; if task was found one layer down (recursive call returned :task)
           (setf (car templist) (second templist)
                 (cdr templist) (cddr templist)))
         (if (and (> (list-length L) 1)
                  (null (car (last L))))
           (rplacd (nthcdr (- (list-length L) 2) L) nil))
         (return-from delete-task-main-list deleted))

        (:task
         ;; a simple task, check if it's equal to t1, if so, set it to (nil)
         ;; and at the up-level it will be deleted
         ;; Don't set -> instead just non-destructively return :task since
         ;;  individual task elements are reused now - jwm 12/12/02
         (if (and (not deleted) (equal TASK L))
             (return-from delete-task-main-list :task)
           (return-from delete-task-main-list nil)))))))

