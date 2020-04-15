(in-package :shop-user)

(asdf:load-system "pddl-utils")

(defparameter *pddl-problem*
  (pddl-utils:read-pddl-file (asdf:system-relative-pathname "shop3" "examples/rovers/metric/pfile01.pddl")))

(defun shopify-tree (tree)
  (let ((pddl-utils:*pddl-package* (find-package :shop-user)))
    (pddl-utils:pddlify-tree tree)))

;;; from the Common Lisp Cookbook
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun goal-facts (pddl-problem)
  (let ((goals
          (rest                                 ; drop the conjunction
           (pddl-utils:problem-goal pddl-problem))))
    (shopify-tree 
     (loop :for (goal-pred . goal-args) :in goals
           :as name = (symbol-name goal-pred)
           :as imperative = (make-symbol (replace-all name (symbol-name '#:communicated) (symbol-name '#:communicate)))
           :collect `(,imperative ,@goal-args)))))

(defun type-facts (pddl-problem)
  (let* ((types (pddl-utils:problem-objects pddl-problem))
         (canonicalized (pddl-utils:canonicalize-types types)))
    (shopify-tree
     (loop :for (obj-name hyphen type-name . nil) :on canonicalized :by #'cdddr
           :do (assert (and (symbolp type-name) (symbolp obj-name) (eq hyphen '-)))
           :collecting `(,type-name ,obj-name)))))

(make-problem 'metric-rovers-1
              (append
               (type-facts *pddl-problem*)
               (goal-facts *pddl-problem*)
               (shopify-tree (pddl-utils:problem-state *pddl-problem*)))
              '(achieve-goals))
