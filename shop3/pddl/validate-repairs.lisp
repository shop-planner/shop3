;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; 
;;;
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Functions for using VAL to validate the effects of plan repair.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2017/10/25:rpg] Created.
;;;
;;;---------------------------------------------------------------------------

(in-package #:shop2-pddl-helpers)

(defun validate-replan (repaired-plan &key (shop-domain *domain*) (package *package*) pddl-domain pddl-problem
                                        (on-failure nil))
  (let* ((shop-domain (etypecase shop-domain
                        (symbol (shop2:find-domain shop-domain))
                        (shop2::domain shop-domain)))
         (pddl-domain (coerce-pddl-argument pddl-domain))
         (pddl-problem (coerce-pddl-argument pddl-problem))
         (pddl-plan-sexp (pddl-plan-for-replan repaired-plan :shop-domain shop-domain :package package))
         (divergence (find-divergence repaired-plan))
         (pddl-domain (pddl-domain-for-replan divergence pddl-problem pddl-domain))
         (pddl-problem-filename (etypecase pddl-problem
                                  (string pddl-problem)
                                  (pathname (namestring pddl-problem))
                                  (pddl-utils:problem
                                   (or (uiop:with-temporary-file (:pathname pname :stream str :keep t)
                                         (pddl-utils:pprint-pddl pddl-problem str)
                                         (namestring pname))
                                       (error "Error writing problem to file")))))
         (pddl-plan-filename
           (namestring (uiop:with-temporary-file (:pathname temp-file-pathname :stream str :keep t)
                         (pddl-utils:print-pddl-plan pddl-plan-sexp str)
                         temp-file-pathname)))
         (pddl-domain-filename
           (namestring (uiop:with-temporary-file (:pathname pddl-domain-name :stream str :keep t)
                         (pddl-utils:pprint-pddl pddl-domain str)
                         pddl-domain-name))))
    (let ((validation-command (format nil "validate -vv -x ~a ~a ~a"
                                      pddl-domain-filename pddl-problem-filename pddl-plan-filename)))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program validation-command
                            :ignore-error-status t
                            :output '(:string :stripped t)
                            :error-output '(:string :stripped t))
        (cond ((zerop exit-code)
               ;; only delete the files if the validation was successful.
               (when (> shop::*verbose* 0)
                 (format t "Validate output is:~%~T~A~%" output))
               (uiop:delete-file-if-exists pddl-plan-filename)
               (uiop:delete-file-if-exists pddl-domain-filename)
               (when (typep pddl-problem 'pddl-utils:problem)
                 (uiop:delete-file-if-exists pddl-problem-filename))
               t)
              ((eq on-failure :error)
               (cerror "Continue, returning nil"
                       "Validation failed with error code ~d~%Command: ~a~%Error output:~%~T~A~%Output:~%~T~A~%"
                       exit-code validation-command error-output output)
               nil)
              (t
               (format t "Validation failed with error code ~d~%Command: ~a~%Error output:~%~T~A~%Output:~%~T~A~%"
                       exit-code validation-command error-output output)
               nil))))))


(defun coerce-pddl-argument (pddl-argument)
  "Takes a PDDL entity designator, including strings for filenames and
pathnames, and returns the corresponding entity."
  (etypecase pddl-argument
    (cons pddl-argument)
    ((or string pathname) (pddl-utils:read-pddl-file pddl-argument))))

(defun pddl-domain-for-replan (divergence pddl-problem pddl-domain)
  "Make and return a new PDDL-UTILS DOMAIN object (as s-expression).
New domain is augmented with a special DIVERGENCE action that will be
injected into the original plan as a disturbance.
DIVERGENCE is a divergence expression -- (:DIVERGENCE ([:ADD|:DELETE] <fact>)*)"
  (flet ((find-all-keys (x)
           (mapcar 'second (remove-if-not #'(lambda (l) (eq (first l) x)) (rest divergence)))))
    (let* ((pddl-domain (pddl-utils:canonicalize-domain pddl-domain))
           (constant-defs (pddl-utils:typelist-to-alist (pddl-utils:canonicalize-types (pddl-utils:domain-constants pddl-domain))))
           (object-defs (pddl-utils:typelist-to-alist (pddl-utils:canonicalize-types (pddl-utils:problem-objects pddl-problem))))
           (add-list (find-all-keys :add))
           (delete-list (find-all-keys :delete))
           (constants (remove-duplicates
                       (append
                        (alexandria:mappend #'rest add-list)
                        (alexandria:mappend #'rest delete-list))))
           (new-constants (iter (for constant in constants)
                            (with predefined = (mapcar #'car constant-defs))
                            ;; note STRING-EQUAL because of possible package mismatches
                            (unless (member constant predefined :test 'string-equal)
                              (appending `(,constant - ,(or (alexandria:assoc-value object-defs constant :test 'string-equal)
                                                             (error "No type for constant ~a" constant)))))))
           (new-action (pddl-utils:make-action 'divergence nil
                                               :precondition nil
                                               :effect `(and ,@add-list
                                                             ,@(mapcar #'(lambda (fact)
                                                                           `(not ,fact))
                                                                       delete-list))))
           (new-domain (pddl-utils:insert-domain-actions pddl-domain (list new-action))))
      (pddl-utils:add-to-domain-constants new-domain new-constants)
      new-domain)))

(defun pddl-plan-for-replan (repaired-plan &key (shop-domain *domain*) (package *package*))
  "Generate and return a new plan (list of ground actions), from the repaired plan, but with
the divergence pseudo-action injected so that validate can process the result."
  (let ((pos (position :divergence repaired-plan :key #'(lambda (x) (and (listp x) (first x))))))
    (unless pos (error "No :DIVERGENCE in the replan."))
    (let ((new-plan (copy-list repaired-plan)))
      (setf (nth pos new-plan)
            (list (intern (string :divergence) package)))
      (pddl-utils:pddlify-tree (shop2::pddl-plan shop-domain  new-plan)))))

(defun find-divergence (shop-plan)
  (find-if #'(lambda (x) (when (listp x) ; ignore costs, if present
                           (eq (first x) :divergence)))
           shop-plan))
