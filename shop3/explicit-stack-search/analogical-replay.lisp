(in-package :shop3)

(defparameter *analogical-replay-table* (make-hash-table :test 'eq)
  "Hash table from task names to method choices.")

(defstruct replay-table-entry
  (singleton nil :type symbol)
  (alist nil :type list))

(defgeneric %update-singleton (replay-table-entry)
  (:method ((replay-table-entry replay-table-entry))
    (let ((vals (remove-duplicates (mapcar #'cdr (replay-table-entry-alist replay-table-entry)))))
      (if (= (length vals) 1)
          (setf (replay-table-entry-singleton replay-table-entry)
                (first vals))
          (setf (replay-table-entry-singleton replay-table-entry)
                nil)))))

(declaim (type hash-table *analogical-replay-table*))

(defgeneric record-decomposition (domain task method-id &optional backtrack-stack)
  (:documentation "Record that TASK was achieved by the method with identifier METHOD-ID
    onto BACKTRACK-STACK.

    Recording method may be chosen based on the class of DOMAIN.

    New primary methods for this generic function should have argument specifiers
    that are strictly MORE specific than (domain list symbol list) in order to
    shadow the default method.")
  (:method :after (domain task method-id &optional (backtrack-stack nil backtrack-stack-supplied-p))
    (when backtrack-stack-supplied-p
      (push (make-record-expansion-for-replay :task task :method-id method-id)
            backtrack-stack)))
  (:method ((domain domain) (task list) (method-id symbol) &optional backtrack-stack)
    (declare (ignorable backtrack-stack))
    (%record-a-decomposition domain task method-id)))

(defgeneric find-decompositions (domain task)
  (:method ((domain domain) (task list))
    (gethash (task-name task) *analogical-replay-table*)))

(defgeneric clear-replay-table (domain table)
  (:method :around ((domain domain) (table hash-table))
    (uiop:delete-file-if-exists "/tmp/analogy.log"))
  (:method ((domain domain) (table hash-table))
    (declare (ignorable domain))
    (clrhash table)))

(defgeneric guidance (domain task table alternatives)
  (:documentation "Returns either NIL if no useful guidance is available, or a member
of ALTERNATIVES to choose first.")
  (:method :around (domain task table alternatives)
    (declare (ignorable domain task table alternatives))
    (let ((result (call-next-method)))
      (when result
        (with-open-file (str "/tmp/analogy.log" :direction :output :if-exists :append :if-does-not-exist :create)
          (format str "~&Guidance for ~S~%~twith alternatives:~{~T~T~S~%~}is ~S~%" task alternatives result)))
      result))
  (:method ((domain domain) (task list) (table hash-table) (alternatives list))
    (declare (ignorable domain))
    (unless (> (length alternatives) 1)
      (break "Should not be calling guidance if there are not multiple alternatives.")
      (return-from guidance nil))
    (let* ((task-id (task-name task))
           (lookup-results (gethash task-id *analogical-replay-table*)))
      (unless lookup-results (return-from guidance nil))
      (let ((meth-ids (mapcar #'(lambda (x)  (domain-id-for-method-lookup domain x)) alternatives)))
        (assert (every #'identity meth-ids))
        (alexandria:if-let ((singleton (replay-table-entry-singleton lookup-results)))
          (alexandria:when-let ((pos (position singleton meth-ids)))
            (nth pos alternatives))
          ;; try finding a match for the task.
          (alexandria:when-let ((method-id
                                 (alexandria:assoc-value (replay-table-entry-alist lookup-results) task :test #'unify)))
            (alexandria:when-let ((pos (position method-id meth-ids)))
              (nth pos alternatives))))))))


(defgeneric %record-a-decomposition (domain task method-id)
  (:method ((domain domain) (task list) (method-id symbol))
    (let* ((task-id (task-name task))
           (lookup-results (gethash task-id *analogical-replay-table* (make-replay-table-entry))))
      (declare (type replay-table-entry lookup-results))
      (push (cons task method-id) (replay-table-entry-alist lookup-results))
      ;; minor optimization - if you are only adding another entry for the same method,
      ;; you can skip the update
      (unless (eq method-id (replay-table-entry-singleton lookup-results))
        (%update-singleton lookup-results))
      ;; the following is necessary because LOOKUP-RESULTS may be newly created
      (setf (gethash task-id *analogical-replay-table*) lookup-results))))

(defgeneric %delete-a-decomposition (domain task method-id)
  (:method ((domain domain) (task list) (method-id symbol))
    (let* ((task-id (task-name task))
           (lookup-results (or (gethash task-id *analogical-replay-table*)
                               (error "Can't remove analogical replay table entry: no table entry for ~s found." task-id)))
           (prev-table-entry (or (alexandria:assoc-value
                                  (replay-table-entry-alist lookup-results)
                                  task)
                                 (error "Can't find match in analogical replay table for task ~s" task))))
      (declare (type replay-table-entry lookup-results))
      (unless (eq (cdr prev-table-entry) method-id)
        (error "Trying to remove association of task to ~s but found ~s there instead."
               method-id (cdr prev-table-entry)))
      (setf (replay-table-entry-alist lookup-results)
            (delete prev-table-entry (replay-table-entry-alist lookup-results)))
      (%update-singleton lookup-results))))
