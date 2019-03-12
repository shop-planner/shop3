(in-package :tp)


(defparameter *problem-files* (sort (directory (asdf:system-relative-pathname "translate-satellite-problems" "p*.pddl")) 'string-lessp :key 'namestring))

(defun translate-all (&key (problem-files *problem-files*) (output-directory (asdf:system-relative-pathname "translate-satellite-problems" "./")))
  (mapc #'(lambda (x) (translate-file x output-directory)) problem-files))

(defun translate-file (input-file output-directory)
  "Translate INPUT-FILE (a PDDL problem file) into a file with the same name, but with a \".lisp\" extension, in OUTPUT-DIRECTORY."
  (let ((new-problem (translate-problem (pddl-utils:read-pddl-file input-file)))
        (out-filename (merge-pathnames (make-pathname :name (pathname-name (parse-namestring input-file)) :type "lisp")
                                       output-directory)))
    (write-translated-problem new-problem out-filename)))

(defun write-translated-problem (new-problem out-filename)
  (with-open-file (out out-filename :direction :output :if-exists :supersede)
    (print '(in-package :shop-user) out)
    (let ((*package* (find-package :output-package))
          (*print-readably* t))
      (print (reintern-tree new-problem *package*) out)))
  out-filename)

(defun translate-problem (pddl-problem)
  (let ((name (pddl-utils:problem-name pddl-problem))
        (objects (pddl-utils:problem-objects pddl-problem))
        (goal (pddl-utils:problem-goal pddl-problem))
        (facts (pddl-utils:problem-state pddl-problem)))
    `(defproblem ,name
       ;; facts
         (,@(shop2-pddl-helpers:typed-object-list->facts objects )
          ,@facts
          ,@ (translate-goal-facts goal))
       ;; task
       (main)
       )))

(defun translate-goal-facts (goal-expr)
  (labels ((translate-exprs (goal-exprs acc)
             (if (null goal-exprs)
                 (nreverse acc)
                 ;; else
                 (let ((elem (first goal-exprs)))
                   (if (eq (first elem) 'and)
                       (translate-exprs (append (rest elem) (rest goal-exprs)) acc)
                       (let ((translated (goalify elem)))
                         (translate-exprs (rest goal-exprs) (cons translated acc))))))))
    (translate-exprs (list goal-expr) `((:original-goal ,goal-expr)))))

(defun goalify (goal)
  (let* ((pred (symbol-name (first goal)))
         (translated-pred 
           (cond ((equalp pred "POINTING")
                  :goal-pointing)
                 ((equalp pred "HAVE_IMAGE")
                  :goal-have-image)
                 (t (error "Unexpected goal predicate in goal: ~S" goal)))))
    (cons translated-pred (rest goal))))


(defun reintern-tree (tree package)
  (cond ((null tree) nil)
        ((symbolp tree)
         (intern (symbol-name tree) package))
        ((consp tree)
         (cons (reintern-tree (car tree) package) (reintern-tree (cdr tree) package)))
        (t tree)))
