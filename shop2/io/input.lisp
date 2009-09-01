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

(defvar *define-silently* nil
  "Set to a non-nil value to suppress output messages printed when SHOP2 model components
\(domains, etc.\) are defined.  When this value is NIL, SHOP2 defaults to printing
messages when it is asked to define components.")

;;; ------------------------------------------------------------------------
;;; Functions for creating and manipulating planning domains and problems
;;; ------------------------------------------------------------------------

;;; MAKE-DOMAIN gives the name NAME to the planning domain whose axioms,
;;; operators, and methods are those in in ITEMS.  More specifically, it
;;; puts the axioms, operators, and methods onto NAME's property list under
;;; the indicators :AXIOMS, :OPERATORS, and :METHODS, respectively

;;; It scares me that we have defdomain and make-domain, they do the
;;; same thing, and there's duplicated code.  This seems like a recipe
;;; for pain in the future.  I believe that defdomain should be
;;; modified to expand to a call to make-domain... [2006/07/05:rpg]
(defun make-domain (name &optional items)
  (warn "MAKE-DOMAIN is an obsolete, deprecated interface.  Please use DEFDOMAIN instead.")
  (when (null items)
    (psetf items name
           name (gentemp (symbol-name '#:domain))))
  ;; name is ignored -- it's there only for compatibility with SHOP 1
  (unless *define-silently*
    (format t "~%Defining domain ..."))
  (let ((domain (make-instance 'domain
                  :name name)))
    (setq items (append '((:operator (!!inop) () () 0)) items))
    (parse-domain-items domain items)
    (install-domain domain)
    (setf *domain* domain)))

;;; If the axiom has SHOP 1.x or mixed syntax, regularize-axiom will
;;; return it in SHOP 2 syntax.
(defmethod regularize-axiom ((domain domain) axiom)
  (if (shop2-axiom-p axiom) axiom       ; SHOP2 syntax
    (let ((ax-name (caadr axiom))       ; SHOP1 or mixed syntax
          (branch-counter 0)
          (labelified-axiom (list (car axiom) (cadr axiom)))
          (atail (cddr axiom)))
          (append labelified-axiom
              (loop until (null atail) do
                    (incf branch-counter)
                    if (listp (car atail))       ; no label on this branch
                    append (list (gensym (format nil "~A~D--"
                                                 ax-name branch-counter))
                                 (pop atail))
                    else                ; this branch has a label
                    append (list (pop atail) (pop atail)))))))

(defun shop2-axiom-p (ax)
  (if (not (listp ax)) nil
      (let ((lax (length ax)))
        (if (<= lax 3) nil (rest-shop2-axiom-p (cddr ax))))))

(defun rest-shop2-axiom-p (ax)
  (let ((lax (length ax)))
    (cond ((< lax 2) nil)
          ((= lax 2) (if (and (atom (car ax)) (listp (cadr ax))) t nil))
          ((= lax 3) nil)
          (t (and (atom (car ax)) (listp (cadr ax)) (rest-shop2-axiom-p (cddr ax)))))))

;;; MAKE-PROBLEM creates a planning problem named PROBLEM-NAME
;;; by putting STATE and TASK onto PROBLEM-NAME's
;;; property list under the indicators :STATE and :TASKS.
#+allegro (excl::define-simple-parser make-problem second :shop2-problem)

;;; this must be a variable, rather than an optional argument, because
;;; of the unpleasant way make-problem has extra arguments for
;;; backward compatibility. [2005/01/07:rpg]
(defvar *make-problem-silently* nil
  "If this variable is bound to t, make-problem will NOT print a message.")

(defun make-problem (problem-name-etc state tasks &rest extras &aux domain-name)
  ;; ever nastier contortions to be backward-compatible with the
  ;; optional, ignored domain-name :-( [2008/01/28:rpg]
  (let ((extra (unless (keywordp (first extras)) (pop extras))))
    ;; if extra is given, then the args are problem-name, domain-name, state, tasks
    ;; in that case, we want to ignore domain-name
    (when extra
      (setf domain-name state
            state tasks
            tasks extra))
    (let (type problem-name options)
      (cond ((listp problem-name-etc)
             (setf problem-name (pop problem-name-etc)
                   type (getf problem-name-etc :type))
             (remf problem-name-etc :type)
             (setf options problem-name-etc)
             (when options
               (error "Do not yet have logic for handling problem options.")))
            (t (setf problem-name problem-name-etc
                     type 'problem)))
      (unless *make-problem-silently*
        (unless *define-silently*
          (format t "~%Defining problem ~s ...~%" problem-name)))
      (let ((problem-inst (make-instance type
                            :domain-name domain-name
                            :name problem-name
                            )))
        (apply 'initialize-problem problem-inst :state state
               :tasks tasks extras)
        #+allegro
        (excl:record-source-file problem-name :type :shop2-problem)
        problem-inst))))

(defmethod initialize-problem ((problem problem) &key state tasks)
  (setf *all-problems*
        (delete (name problem) *all-problems* :key 'name))
  (setf *all-problems* (cons problem *all-problems*))
  (setf (slot-value problem 'tasks) (process-task-list tasks)
        (slot-value problem 'state-atoms) state)
  ;; uck --- this won't work any more because we aren't guaranteed to
  ;; have a domain here.  Will move to find-plans... [2007/03/14:rpg]
  ;; (set-variable-property *domain* tasks)
  problem)

(defmethod problem->state ((domain domain) problem)
  (list
   (get-state problem)))

;;;(defun delete-problem (problem-name)
;;;  (setf (get problem-name :state) nil
;;;     (get problem-name :tasks) nil)
;;;  (setf *all-problems* (delete problem-name *all-problems*))
;;;  problem-name)

;;;---------------------------------------------------------------------------
;;; I have added these two accessors to make it easier to modify the
;;; implementation of SHOP2 problems, should we like to do it.  I
;;; suggest that we use these instead of (get <problem-name> :state)
;;; and (get <problem-name> :tasks) [2004/10/27:rpg]
;;;---------------------------------------------------------------------------

;;; MAKE-PROBLEM-SET gives the name SET-NAME to the problems in PROBLEM-SET.
;;; More specifically, it puts PROBLEM-SET onto PROBLEM-NAME's
;;; property list under the indicators :STATE, :TASKS, and :DOMAIN
(defun make-problem-set (list-name problem-list)
  (unless *define-silently*
    (format t "~%Defining problem set ~s ..." list-name))
  (setf (get list-name :problems) problem-list))

(defun get-problems (name &key print)
  "Get the list of problems for the problem set named NAME"
  (let ((answer (get name :problems 'fail)))
    (when (eq answer 'fail) (error "No problem list for the name ~s" name))
    (when print
      (format t "~%~s" answer))
    answer))

;;; DO-PROBLEMS runs FIND-PLANS on each problem in PROBLEMS, which may be
;;; either a problem-set name or a list of problems
(defun do-problems (problems &rest keywords)
  (if (not (listp problems))    ; treat NIL as an empty list, not a problem name
    (setq problems (get-problems problems)))
  (dolist (problem problems)
    (apply #'find-plans (cons problem keywords))))

(defmethod process-pre (domain pre)
  "This is the main function that does the pre-processing, it
looks through the preconditions finding the forall
 conditions and replacing the variables in that condition."
  (if (atom pre) pre
    (let ((pre1 (car pre)))
      (if (listp pre1)
          (cons (process-pre domain  pre1) (process-pre domain  (cdr pre))))
      (case pre1
        (or (cons 'or (process-pre domain  (cdr pre))))
        (imply
         (cons 'imply (process-pre domain  (cdr pre))))
        (:first (cons :first (process-pre domain  (cdr pre))))
        (forall
         (unless (and (listp (second pre)) (every #'(lambda (x) (variablep x)) (second pre)))
           (error "The first argument to a FORALL expression should be a LIST of variables in ~S"
                  pre))
         (unless (= (length pre) 4)
           (error "Ill-formed FORALL expression: ~s" pre))
         (multiple-value-bind (alist vlist) (get-alist (second pre))
           `(forall ,vlist
                 ,(process-pre domain  (apply-substitution (third pre) alist))
                 ,(process-pre domain  (apply-substitution (fourth pre) alist)))))
        (otherwise pre)))))

;;; this function pre-processes the methods, replace every
;;; variable defined by the forall condition to a previously
;;; unused variable. It also regularizes the methdods in old SHOP format.
(defmethod process-method ((domain domain) method)
  (let ((method-head (cadr method))
        (answer nil)
        (tail nil)
        (branch-counter 0)
        (method-name nil)
        clause first-of-clause)
    (setq method-name (car method-head))
    (setq answer (list (car method) method-head))
    (setq tail (cddr method))
    (append answer
            (loop until (null tail)
                  do (incf branch-counter)
                  when (and (not (null (car tail))) (symbolp (car tail)))
                    append (list (pop tail))       ; skip over a label
                  else append (list (gensym (format nil "~A~D--"
                                                    method-name branch-counter)))
                  append (list  (process-pre domain  (pop tail))
                                ;; check to see if there is a quote or
                                ;; backquote in the front of this list (SHOP1
                                ;; or SHOP2 syntax) and process accordingly
                                (progn
                                  (setq clause (pop tail)
                                        first-of-clause (car clause))
                                  (cond ((or (eq first-of-clause 'quote)
                                             (eq first-of-clause *back-quote-name*))
                                         ;; this next bit of strangeness is to take the quote
                                         ;; off the front of the task list,
                                         ;; decompose the task list, then slap the quote back on
                                         (list first-of-clause (process-task-list (cadr clause))))
                                        ((search-tree 'call clause)
                                         (list 'simple-backquote (process-task-list clause)))
                                        (t (list 'quote (process-task-list clause))))))))))

;;; returns t if item is found anywhere in the tree; doubly recursive,
;;; but only runs once per method definition.
(defun search-tree (item tree)
  (cond ((eq item tree) t)
        ((not (consp tree)) nil)
        (t (or (search-tree item (car tree)) (search-tree item (cdr tree))))))

;;; The above puts QUOTE on the front, but inside the clause there could be a "call" which is
;;; ignored.  Previously, instead of "call" there would have been a backquote around the form and
;;; commas in front of the "call".  After substituting the variables, the form is eval'd and the
;;; backquote is done or the quote is just taken off.  Now, something like backquote needs to be done.
;;; perhaps the form should be searched for an instance of "call"; if found, put "backquote" on the
;;; front and modify the backquote function to handle "call" like comma.

;;; this function pre-process the operator, replace every
;;; variable defined by the forall condition to a previously
;;; unused variable. It also addresses the issue of different
;;; syntaxes of operators in different versions of SHOP.
(defmethod process-operator ((domain domain) operator)
  (let ((lopt (length operator)))
    (cond ((= lopt 4)             ; a SHOP 1 operator, no cost specified
           (make-operator :head (second operator)
                          :preconditions nil
                          :deletions (process-pre domain  (third operator))
                          :additions (process-pre domain  (fourth operator))
                          :cost-fun 1.0))
          ;; a SHOP 1 operator, with cost specified
          ((and (= lopt 5) (numberp (fifth operator)))
           (make-operator :head (second operator)
                          :preconditions nil
                          :deletions (process-pre domain  (third operator))
                          :additions (process-pre domain  (fourth operator))
                          :cost-fun (process-pre domain  (fifth operator))))
          ((= lopt 5)             ; a SHOP 2 operator, no cost specified
           (make-operator :head (second operator)
                          :preconditions (process-pre domain  (third operator))
                          :deletions (process-pre domain  (fourth operator))
                          :additions (process-pre domain  (fifth operator))
                          :cost-fun 1.0))
          ((= lopt 6)             ; a SHOP 2 operator, with cost specified
           (make-operator :head (second operator)
                          :preconditions (process-pre domain  (third operator))
                          :deletions (process-pre domain  (fourth operator))
                          :additions (process-pre domain  (fifth operator))
                          :cost-fun (process-pre domain  (sixth operator))))
          (t (error (format nil "mal-formed operator ~A in process-operator" operator))))))

(defun process-task-list (tasks)
  (cond
   ((null tasks) (list :ordered (list :task '!!inop)))
   ((member (first tasks) '(:ordered :unordered))
    (cons (first tasks)
    (mapcar #'process-task-list (rest tasks))))
   ((eq (first tasks) :task)
    tasks)
   ((atom (first tasks))
    (cons :task tasks))
   (t
    (cons :ordered
          (mapcar #'process-task-list tasks)))))


;;;---------------------------------------------------------------------------
;;; Changed defproblem to simply add quotes and call MAKE-PROBLEM.
;;; This can help us avoid the danger of having defproblem and
;;; make-problem diverge. [2004/02/17:rpg]
;;;---------------------------------------------------------------------------
#+allegro (excl::define-simple-parser defproblem second :shop2-problem)
(defmacro defproblem (problem-name &rest args)
  ;; ARGS normally are state tasks
  ;; if extra arg is given, then the args are problem-name, domain-name, state,
  ;; and tasks respectively. in that case, we want to ignore domain-name
  (if (= (length args) 3)
    `(apply 'make-problem ',problem-name ',(cdr args))
    `(apply 'make-problem ',problem-name ',args)))

(defmacro def-problem-set (list-name problem-list)
  `(progn
     (unless *define-silently*
       (format t "~%Defining problem set ~s ..." ',list-name))
    (setf (get ',list-name :problems) ',problem-list)))

;;;---------------------------------------------------------------------------
;;; DOMAIN manipulation functions --- these should probably be moved
;;; out into a file of their own at some point. [2006/07/24:rpg]
;;;---------------------------------------------------------------------------
(defvar *defdomain-verbose* t)
(defmacro defdomain (name-and-options items)
  ;; note that we are copying name-and-options because we destructively modify
  ;; it later (or at least destructively modify the options list which might
  ;; share structure with it. [2009/01/28:rpg]
  (if (listp name-and-options)
      (setf name-and-options (copy-list name-and-options))
      (setf name-and-options (list name-and-options)))
  ;; it makes more sense to me that defdomain have a &rest argument,
  ;; instead of a single argument that's a list.... [2006/07/31:rpg]
  (push '(:operator (!!inop) () () () 0) items)
  (destructuring-bind (name &rest options &key (type 'domain) noset redefine-ok &allow-other-keys)
         name-and-options
    (unless (subtypep type 'domain)
         (error "Type argument to defdomain must be a subtype of DOMAIN. ~A is not acceptable." type))
    (remf options :type)
    (remf options :redefine-ok)
    (remf options :noset)
    `(progn
       (unless *define-silently*
         (when *defdomain-verbose*
         (format t "~%Defining domain ~a..." ',name)))
       (let ((domain (apply #'make-instance ',type
                       :name ',name
                       ',options
                       )))
         ;; I suspect that this should go away and be handled by
         ;; initialize-instance... [2006/07/31:rpg]
         (apply #'handle-domain-options domain ',options)
         (parse-domain-items domain ',items)
         (install-domain domain ,redefine-ok)
         (unless ,noset
           (setf *domain* domain))
         ;; previous addition of noset changed the behavior of defdomain to make
         ;; it NOT return the defined domain; this is inappropriate. [2009/03/26:rpg]
         domain))))

(defmethod install-domain ((domain domain) &optional redefine-ok)
  (when (get (domain-name domain) :domain)
    (unless redefine-ok
      (warn "Redefining domain named ~A" domain)))
  (setf (get (domain-name domain) :domain) domain))

(defun find-domain (name &optional (if-not-found :error))
  "Find and return a domain object with the name NAME.  Will return
the value in its IF-NOT-FOUND argument if no such domain is loaded.
IF-NOT-FOUND defaults to :error, which will raise an error condition."
  (let ((domain (get name :domain)))
    (cond (domain domain)
          ;; not found
          ((eq if-not-found :error)
           (error "No domain named ~A" name))
          (t if-not-found))))

(defun set-domain (name)
  "NAME argument is a symbol.  Will set the global variable *DOMAIN*
to the domain with domain-name NAME."
  (setf *domain*
        (find-domain name :error)))

;;; default null method
(defmethod handle-domain-options ((domain domain) &key type)
  (declare (ignore type))
  (values))

;;; I have refactored parse-domain-items to use eql method dispatch
;;; instead of a case statement dispatch... [2006/07/28:rpg]
(defmethod parse-domain-items ((domain domain) items)
  (with-slots (axioms operators methods) domain
    (setf axioms (make-hash-table :test 'eq)
          operators (make-hash-table :test 'eq)
          methods (make-hash-table :test 'eq))
    (set-variable-property domain items)        ; set primitive and variable properties
    (dolist (x (reverse items))
      (parse-domain-item domain (car x) x))
    (values)))

;;;---------------------------------------------------------------------------
;;; Parse-domain-item methods
;;;---------------------------------------------------------------------------

(defmethod parse-domain-item ((domain domain) (item-key (eql ':method)) item)
  (push (process-method domain item)
        (gethash (first (second item))
                 (slot-value domain 'methods))))

(defmethod parse-domain-item ((domain domain) (item-key (eql ':operator)) item)
  (let ((op-name (first (second item))))
    (with-slots (operators) domain
      (when (gethash op-name operators)
        (error "There is more than one operator named ~s" op-name))
      (setf (gethash op-name operators) (process-operator domain item)))))

(defmethod parse-domain-item ((domain domain) (item-key (eql ':-)) item)
  (with-slots (axioms) domain
    ;; convert SHOP 1.x axioms into SHOP 2 axioms if necessary
    (let ((regularized (regularize-axiom domain item)))
      (push regularized (gethash (first (second item)) axioms)))))







;;;---------------------------------------------------------------------------
;;; End of domain functions....
;;;---------------------------------------------------------------------------
