(defpackage #:shop3-openstacks
  (:nicknames #:shop-openstacks #:shop2-openstacks)
  (:intern #:divergence
           ;; types
           #:order #:product #:count
           ;; predicates
           #:includes #:waiting #:started #:shipped #:made #:stacks-avail #:next-count
           ;; actions
           #:make-product
           #:start-order
           #:ship-order
           #:open-new-stack
           ;; top level task
           #:plan
           ;; domain name
           #:openstacks-sequencedstrips-ADL-included
           )
  (:use #:common-lisp #:shop3))

(defpackage openstacks-problem-converter
  (:use common-lisp iterate)
  (:import-from #:shop3 #:defproblem #:make-problem)
  (:import-from #:shop3-pddl-helpers #:typed-object-list->facts)
  (:import-from #:shop3-openstacks
                ;; types
                #:order #:product #:count
                ;; predicates
                #:includes #:waiting #:started #:shipped #:made #:stacks-avail #:next-count
                ;; actions
                #:make-product
                #:start-order
                #:ship-order
                #:open-new-stack
                ;; top level task
                #:plan
                ;; domain name
                #:openstacks-sequencedstrips-ADL-included
                ))

(in-package openstacks-problem-converter)

(defun convert-problems (L &key (output-defaults (make-pathname :type "lisp")))
  "Translate each of the pddl files in the list L into a new
lisp file.  Best to supply an output-defaults keyword argument being a pathname object
containing a :directory specification and :type \"lisp\"."
  (dolist (fn L)
    (let ((new-problem 
            (translate-openstacks-problem fn)))
      (with-open-file (str (make-pathname :name (pathname-name fn) :defaults output-defaults)
                           :direction :output :if-exists :supersede)
        (let ((*print-readably* t)
              (*package* (find-package :shop3-openstacks)))
          (print '(in-package :shop3-openstacks) str)
          (print new-problem str))))))

(defun translate-openstacks-problem (problem-file &key (package :shop-openstacks) )
  (let ((pddl-utils:*pddl-package* package))
    (let ((problem 
            (pddl-utils:read-pddl-file problem-file)))
      `(make-problem ',(pddl-utils:problem-name problem)
                     'openstacks-sequencedstrips-ADL-included
                     ;; state
                     ',(append
                       (pddl-utils:problem-state problem)
                       (typed-object-list->facts
                        (pddl-utils:canonicalize-types
                         (pddl-utils:problem-objects problem)))
                       `((:goal ,(pddl-utils:problem-goal problem))))
                     ;; tasks
                     ((plan))))))

