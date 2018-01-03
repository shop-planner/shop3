(in-package :shop2)

(defun pprint-plan (plan &optional (stream t))
  (when *print-readably*
    (error 'print-not-readable :object plan))
  (mapc #'(lambda (step) (pprint-step stream step))
        (labels ((transform (x)
                   (typecase x
                     (null nil)
                     (symbol (symbol-name x))
                     (list (cons (transform (car x))
                                 (transform (cdr x))))
                     (t x))))
          (transform (shorter-plan plan))))
  (values))

(defun pprint-step (stream step)
  ;; FIXME: should do something to figure out how to make this line break well.
  (format stream "~&~A~%" step))

