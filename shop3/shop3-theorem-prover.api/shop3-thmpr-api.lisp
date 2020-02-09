
(in-package :shop2-thmpr-api)

(defparameter *just-one* t)

(defun init-thmpr-api (domain-name)
  (setf shop2::*domain* 
        (make-instance 'shop2::domain
          :name domain-name
          :axioms (make-hash-table))))

(defun find-satisfiers (pre state 
                        &key (just-one *just-one*))
  (shop2::find-satisfiers pre state :just-one just-one
                          :domain shop2::*domain*))

