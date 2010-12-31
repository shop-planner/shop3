; Some small Lisp routines to support the interface between Java and SHOP2

(let ((iteration-tag (gensym))
      problem-name stored-trees stored-plans plan-state-bindings 
		   current-tree-stream current-plan add-list delete-list)
  (defun jshop-find-plans (&rest args)
    (setf problem-name (first args))
    (setf plan-state-bindings nil)
    (let ((retval (apply #'find-plans (append args '(:plan-tree t)))))
      (setf stored-trees (first retval))
      (setf stored-plans (second retval)))
    nil)

  (defun plan-found-hook (state which-plans final-plan final-plan-cost
				depth)
    (declare (ignore which-plans))
    (declare (ignore final-plan-cost))
    (declare (ignore depth))
    (setf plan-state-bindings 
	  (cons
	   (list
	    (reverse final-plan)
	    (copy-list (state-atoms state)))
	   plan-state-bindings))
    (values))
  (defun jshop-next-plan ()
    (setf current-tree-stream (jshop-tree-stream (first stored-trees)))
    (setf current-plan (copy-list (first stored-plans)))
    (let ((start-state (get problem-name :state))
	  (end-state (second (assoc current-plan plan-state-bindings
				    :test #'equal))))
      (setf stored-trees (rest stored-trees))
      (setf stored-plans (rest stored-plans))
      (setf add-list
	    (copy-list (set-difference end-state start-state
				       :test #'equal)))
      (setf delete-list
	    (copy-list (set-difference start-state end-state 
				       :test #'equal))))
    (setf *current-tree-stream* current-tree-stream)
    (if current-plan t nil))

  (defun jshop-tree-stream (tree)
    (mapcan #'jshop-node-stream tree))

  (defun jshop-node-stream (node)
    (cond
     ((atom node) nil)
     ((atom (first node)) (list node nil))
     (t
      (cons
       (first node)
       (append
	(mapcan #'jshop-node-stream (rest node))
	(list nil))))))

  (defun jshop-get-tree-element ()
    (let ((first-element (first current-tree-stream)))
      (cond
       ((numberp (first first-element)) ; a new operator is encountered
	(setf (first current-tree-stream)
	      (cons iteration-tag first-element))
	:operator)
       ((eq (first first-element) iteration-tag) ; operator in progress
	(prog1 (second first-element)
	  (if (rest (rest first-element))
	      (setf (first current-tree-stream)
		    (cons iteration-tag (rest (rest first-element))))
	    (setf current-tree-stream (rest current-tree-stream)))))
       (t ; not an operator
	(prog1 (first current-tree-stream)
	  (setf current-tree-stream (rest current-tree-stream)))))))

  (defun jshop-get-add-atom ()
    (prog1
	(first add-list)
      (setf add-list (rest add-list))))
  (defun jshop-get-delete-atom ()
    (prog1
	(first delete-list)
      (setf delete-list (rest delete-list)))))

; If *all-problems* is equal to (a b c) then repeated calls to
;  get-problem-name will return the following results:
;    a b c () a b c () a b c () ...
(let ((all-problems t))
  (defun jshop-get-problem-name ()
    (if (eq all-problems t)
	(setf all-problems *all-problems*))
    (prog1
	(first all-problems)
      (if (first all-problems)
	  (setf all-problems (rest all-problems))
	(setf all-problems t)))))
