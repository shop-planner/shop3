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

(defvar *ignore-singleton-variables*
  nil
  "When T -- which should only be for legacy SHOP2 domains -- 
do NOT emit singleton variable warnings.")

;;; ------------------------------------------------------------------------
;;; Functions for creating and manipulating planning domains and problems
;;; ------------------------------------------------------------------------

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
        (setf *problem* problem-name)
        #+allegro
        (excl:record-source-file problem-name :type :shop2-problem)
        problem-inst))))

(defmethod initialize-problem ((problem problem) &key state tasks)
  (delete-problem (name problem))        ;get rid of old problem definition
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
         (unless (= (length pre) 3)
           (error "Ill-formed IMPLY expression: ~s" pre))
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
  (let* ((method (uniquify-anonymous-variables method))
         (method-head (second method))
         (method-name (car method-head))
         (task-variables (harvest-variables method-head))
         body-var-tables)
    (flet ((massage-task-net (clause)
             ;; this is a bunch of stuff for processing the task net which
             ;; I am sorry to say I don't fully understand.  I don't know
             ;; why this is quoted, honestly. [2010/05/19:rpg]
             (let ((first-of-clause (first clause)))
               (cond
                 ;; check to see if there is a quote or
                 ;; backquote in the front of this list (SHOP1
                 ;; or SHOP2 syntax) and process accordingly
                 ((or (eq first-of-clause 'quote)
                      (eq first-of-clause *back-quote-name*))
                  ;; this next bit of strangeness is to take the quote
                  ;; off the front of the task list,
                  ;; decompose the task list, then slap the quote back on
                  `(,first-of-clause ,(process-task-list (second clause))))
                 ((search-tree 'call clause)
                  `(simple-backquote ,(process-task-list clause)))
                 (t
                  `(quote ,(process-task-list clause)))))))

      ;; answer will be (:method <head> ...)
      (prog1
          ;; here's the transformed METHOD
      `(,(first method)
        ,method-head
        ,@(loop with tail = (cddr method)
                with pre
                with task-net
                with var-table
                ;; there's only one task net
                with singleton = (or (= (length tail) 1)
                                     ;; branch name and task net...
                                     (and (= (length tail) 2)
                                          (symbolp (first tail))
                                          (not (variablep (first tail)))))
                for branch-counter from 0
                until (null tail)
                ;; find or make a method label
                if (and (car tail)
                        (symbolp (car tail))
                        ;; could be a second-order HTN.
                        (not (variablep (first tail))))
                  collect (pop tail)
                else
                  collect (gensym (format nil "~A~D--"
                                          method-name branch-counter))
                ;; collect the preconditions
                do (setf pre (pop tail))
                   (setf task-net (pop tail))
                   (setf var-table (harvest-variables (cons pre task-net)))
                   (push var-table body-var-tables)
                   (check-for-singletons var-table :context-table task-variables
                                                   :construct-type (first method)
                                                   :construct-name method-name
                                                   :construct method
                                                   :branch-number (unless singleton (1+ branch-counter)))
                collect (process-pre domain pre)
                collect (massage-task-net task-net)))
        ;; just before returning, check for singletons in the method's head
        (check-for-singletons task-variables :context-tables body-var-tables
                                             :construct-type (first method)
                                             :construct-name method-name
                                             :construct method)))))


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

;;; this function pre-processes the operator, replaces every
;;; variable defined by the forall condition to a previously
;;; unused variable. It also addresses the issue of different
;;; syntaxes of operators in different versions of SHOP.
(defmethod process-operator ((domain domain) operator)
  (let* ((operator (uniquify-anonymous-variables operator))
         (var-table (harvest-variables operator))
         (lopt (length operator)))
    (check-for-singletons var-table :construct-type (first operator) :construct-name (first (second operator))
                          :construct operator)
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

(defmethod process-op ((domain domain) operator)
  (let ((operator (uniquify-anonymous-variables operator)))
    (destructuring-bind (keyword task &key add delete precond (cost 1.0)) operator
      (let* ((task-table (harvest-variables task))
             (add-table (harvest-variables add))
             (del-table (harvest-variables delete))
             (precond-table (harvest-variables precond))
             (cost-table (harvest-variables cost))
             (tables (list task-table add-table del-table precond-table cost-table)))
        (loop :for table :in tables
              :as others = (remove table tables :test 'eq)
              :do (check-for-singletons table :context-tables others :construct-type keyword
                                              :construct-name (first task)
                                              :construct operator)))
      (make-operator :head task :preconditions precond :deletions delete
                     :additions add :cost-fun cost))))

(defun check-for-singletons (var-table &key context-tables context-table construct-type construct-name construct branch-number)
  (unless *ignore-singleton-variables*
    (when context-table
      (assert (not context-tables))
      (setf context-tables (list context-table)))
    (let ((singletons nil))
      (maphash #'(lambda (k v)
                   (when (= v 1) (push k singletons))) var-table)
      (when singletons
        (when context-tables
          (setf singletons (filter-singletons singletons context-tables)))
        (when singletons
          (warn 'singleton-variable
                :variable-names singletons
                :construct-type construct-type
                :construct-name construct-name
                :construct construct
                :branch-number branch-number))))
  (values)))

(defun filter-singletons (singletons context-tables)
  (remove-if #'(lambda (singleton)
                     (some #'(lambda (table) (> (gethash singleton table 0) 0))
                           context-tables))
                 singletons))

(defun harvest-variables (sexp &optional all-vars)
  "Tree-walk the SEXP, harvesting all variables into a hash-table.  The hash-table
will have as keys variable names, and values will be counts of number of 
appearances.  Defaults to *not* counting variables with the \"ignore prefix\" of
underscore, unless ALL-VARS is non-NIL.
   PRECONDITION:  PROCESS-VARIABLE-PROPERTY must have been called on the surrounding
context, becasue this relies on VARIABLEP working."
  (let ((retval (make-hash-table :test 'eq)))
    (labels ((iter (sexp)
               (cond ((null sexp) (values))
                     ((variablep sexp)
                      (when (or all-vars
                                  (not (anonymous-var-p sexp)))
                        (bump-entry sexp))
                      (values))
                     ((consp sexp)
                      (iter (car sexp))
                      (iter (rest sexp)))
                     (t
                      ;; can't check for ATOM, because there might be arrays, or
                      ;; any old stuff in the SHOP2 code.
                      (values))))
             (bump-entry (var-name)
               (let ((entry (gethash var-name retval 0)))
                 (setf (gethash var-name retval) (1+ entry)))))
      (iter sexp)
      retval)))

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
  "\(DEFPROBLEM <name> <state> <tasks>\)
For backward compatibility, will support also
   \(DEFPROBLEM <name> <domain-name> <state> <tasks>\)
but <domain-name> will be ignored."
  ;; ARGS normally are state tasks
  ;; if extra arg is given, then the args are problem-name, domain-name, state,
  ;; and tasks respectively. in that case, we want to ignore domain-name
  (assert (or (= (length args) 3) (= (length args) 4)))
  `(apply 'make-problem ',problem-name ',args))

(defmacro def-problem-set (list-name problem-list)
  `(progn
     (unless *define-silently*
       (format t "~%Defining problem set ~s ..." ',list-name))
    (setf (get ',list-name :problems) ',problem-list)))

;;;---------------------------------------------------------------------------
;;; DOMAIN manipulation functions --- these should probably be moved
;;; out into a file of their own at some point. [2006/07/24:rpg]
;;;---------------------------------------------------------------------------
#+allegro (excl::define-simple-parser defdomain second :shop2-domain)
(defvar *defdomain-verbose* t)
(defvar *defdomain-pathname* nil
  "Dynamic variable used to enable INCLUDE directives to find files
relative to the file containing the DEFDOMAIN.  Necessary because ASDF
breaks usage of *load-truename* by moving the FASLs.")
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
  `(let ((*defdomain-pathname* ,(or *compile-file-truename*
                                    *load-truename*)))
      #+allegro (excl:without-redefinition-warnings
                  (excl:record-source-file ',(first name-and-options) :type :shop2-domain))
     (apply 'make-domain ',name-and-options ',items)))

(defun make-domain (name-and-options &rest items)
  (destructuring-bind (name &rest options &key (type 'domain) noset redefine-ok &allow-other-keys)
      name-and-options
    (unless *define-silently*
      (when *defdomain-verbose*
        (format t "~%Defining domain ~a...~%" name)))
    (unless (subtypep type 'domain)
      (error "Type argument to defdomain must be a subtype of DOMAIN. ~A is not acceptable." type))
    (remf options :type)
    (remf options :redefine-ok)
    (remf options :noset)
    (setf redefine-ok (or redefine-ok *define-silently*))
    (let ((domain (apply #'make-instance type
                         :name name
                         options)))
      ;; I suspect that this should go away and be handled by
      ;; initialize-instance... [2006/07/31:rpg]
      (apply #'handle-domain-options domain options)
      (setf items (expand-includes domain items))
      (let (warnings)
        (handler-bind
            ((domain-item-parse-warning
               #'(lambda (c)
                   (push c warnings)
                   (muffle-warning c))))
          (parse-domain-items domain items))
        (when warnings
          (let ((*print-pprint-dispatch* *shop-pprint-table*))
            (format T "Warnings:~{~&~a~%~%~}" (nreverse warnings)))))
      (install-domain domain redefine-ok)
      (unless noset
        (setf *domain* domain))
      ;; previous addition of noset changed the behavior of defdomain to make
      ;; it NOT return the defined domain; this is inappropriate. [2009/03/26:rpg]
      domain)))

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

;;; make QUERY easier to use
(defmethod shop2.theorem-prover:query :around (goals state &key just-one (domain *domain*))
  (if (symbolp domain)
      (call-next-method goals state :just-one just-one :domain (find-domain domain))
      (call-next-method)))
  

(defun delete-domain (name)
  (setf (get name :domain) nil))

(defun set-domain (name)
  "NAME argument is a symbol.  Will set the global variable *DOMAIN*
to the domain with domain-name NAME."
  (setf *domain*
        (find-domain name :error)))

;;; default null method
(defmethod handle-domain-options ((domain domain) &key type)
  (declare (ignore type))
  (values))

(defmethod parse-domain-items ((domain domain) items)
  (with-slots (operators methods) domain
    (setf operators (make-hash-table :test 'eq)
          methods (make-hash-table :test 'eq))
    (set-variable-property domain items)        ; set primitive and variable properties
    (dolist (x (reverse items))
      (parse-domain-item domain (car x) x))
    (values)))

(defun expand-includes (domain items)
  "Handle :INCLUDE directives, return a new items list
if any are found, otherwise the original list."
  ;; short-circuit if no include directives
  (if (member :include items :key 'first :test 'eq)
      (expand-include domain items)
   items))

(defun expand-include (domain items)
  (loop :for item :in items
        :if (eq (first item) :include)
          :append (translate-include domain item)
        :else
          :collect item))

(defun translate-include (domain include-item)
  (destructuring-bind (keyword domain-name &optional pathname)
      include-item
    (assert (eq keyword :include))
    (assert (symbolp domain-name))
    (assert (or (null pathname) (stringp pathname) (pathnamep pathname)))
    (unless pathname
      (setf pathname (make-pathname :name (string-downcase (symbol-name domain-name))
                                    :type "lisp")))
    (domain-include-parse domain domain-name (domain-include-search pathname))))

(defun domain-include-search (path)
  "Search for PATH relative to *COMPILE-FILE-TRUENAME*, *LOAD-TRUENAME*, the cached
location of the domain definition file, and *DEFAULT-PATHNAME-DEFAULTS*."
  (or
   (if (uiop:absolute-pathname-p path)
       (probe-file path)
     (let ((search (list
                    ;; to undo what's done by ASDF in moving the FASLs
                    *defdomain-pathname*
                    *compile-file-truename* *load-truename*
                    *default-pathname-defaults*)))
       (dolist (merge search)
         (let ((fullpath (when merge
                           (merge-pathnames path merge))))
           (when fullpath
             (when (probe-file fullpath)
               (return-from domain-include-search fullpath)))))
       nil))
   (error "No such include file: ~a" path)))

(defgeneric domain-include-parse (parent-domain domain-name path)
  (:documentation "Return domain items from including DOMAIN-NAME found in PATH.")
  (:method ((parent-domain domain) domain-name path)
    (declare (ignorable parent-domain))
    (let ((domain-form
            (with-open-file (str path :direction :input)
              (let ((*package* *package*))
                (loop :for x = (read str nil nil)
                      :while x
                      :with name
                      :if (eq (car x) 'in-package)
                        :do (set '*package* (find-package (second x)))
                      :else :if (eq (car x) 'defdomain)
                              :do (setf name (if (listp (second x)) (first (second x)) (second x)))
                              :and :when (equalp (symbol-name name) (symbol-name domain-name))
                                     :return x)))))
      (if domain-form
          ;; return the items
          (expand-include (third domain-form))
          (error "Did not find definition of domain named ~a in file ~a"
                 domain-name path)))))

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

(defmethod parse-domain-item ((domain domain) (item-key (eql ':op)) item)
  (let ((op-name (first (second item))))
    (with-slots (operators) domain
      (when (gethash op-name operators)
        (error "There is more than one operator named ~s" op-name))
      (setf (gethash op-name operators) (process-op domain item)))))

(defmethod parse-domain-item ((domain domain) (item-key (eql ':-)) item)
  (with-slots (axioms) domain
    ;; convert SHOP 1.x axioms into SHOP 2 axioms if necessary
    (let ((regularized (process-axiom domain item)))
      ;; FIXME: standardize anonymous variables, check for singletons
      ;; (setf regularized (uniquify-anonymous-variables regularized))
      (push regularized (gethash (first (second item)) axioms)))))

(defun process-axiom (domain axiom)
  (let* ((regularized (uniquify-anonymous-variables (regularize-axiom domain axiom)))
         (head (second regularized))
         (head-variables (harvest-variables head))
         all-variables)
    ;; at this point the tail will be of the form ( [<branch-name> <branch-body>]* )
    (loop with tail = (cddr regularized)
          with multi-tail = (> (length tail) 2)
          for branch from 0
          while tail
          do (let* ((body (second tail)) ;first is label
                    (var-table (harvest-variables body)))
               (check-for-singletons var-table
                                     :context-table head-variables
                                     :construct-type :-
                                     :construct-name (first head)
                                     :construct axiom
                                     :branch-number (when multi-tail (1+ branch)))
               (push var-table all-variables))
             (setf tail (cddr tail)))
    (check-for-singletons head-variables :context-tables all-variables
                                     :construct-type :-
                                     :construct-name (first head)
                                     :construct axiom)
    regularized))

          
  




;;;---------------------------------------------------------------------------
;;; End of domain functions....
;;;---------------------------------------------------------------------------
