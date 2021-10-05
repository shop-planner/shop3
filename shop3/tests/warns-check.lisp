(in-package :fiveam)

(defmacro warns (condition-spec
                 &body body)
  "Generates a pass if BODY signals a warning of type
CONDITION. BODY is evaluated in a block named NIL, CONDITION is
not evaluated.
  Is like SIGNALS, but does NOT abort the execution of BODY upon the signal
being raised."
  (let ((block-name (gensym))
        (signaled-p (gensym))
        (normal-return (gensym)))
    (destructuring-bind (condition &optional reason-control reason-args)
        (ensure-list condition-spec)
      `(let ((,signaled-p nil)
             ,normal-return)
         (block ,block-name
           (handler-bind ((,condition (lambda (c)
                                        (unless (typep c 'warning)
                                          (error "Cannot use FiveAM \"warns\" check for non-warning conditions."))
                                        ;; ok, body threw condition
                                        (add-result 'test-passed
                                                    :test-expr ',condition)
                                        (setf ,signaled-p t)
                                        (muffle-warning c))))
             (setf ,normal-return
                   (block nil
                     ,@body)))
           (when ,signaled-p (return-from ,block-name ,normal-return))
           (process-failure
            ',condition
            ,@(if reason-control
                  `(,reason-control ,@reason-args)
                  `("Failed to signal a ~S" ',condition)))
           (return-from ,block-name ,normal-return))))))
