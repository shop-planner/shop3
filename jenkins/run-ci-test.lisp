(cl:in-package :cl-user)

(require :asdf)

(prin1 (lisp-implementation-type))
(terpri)
(prin1 (lisp-implementation-version))
(terpri)

(declaim (optimize (speed 3)))
(asdf:load-system :shop3/test)

(defconstant +test-pkg+ (uiop:getenv "PKG"))
(defconstant +test-suite+ (uiop:getenv "SUITE"))

(let ((test (find-symbol +test-suite+ +test-pkg+)))
  (unless test
    (format t "Failed to find test suite symbol ~A::~A~%"
	    +test-pkg+ +test-suite+)
    (uiop:quit 1))
  (prin1 test)
  (terpri)
  (uiop:quit (if (typep (fiveam:run test) 'fiveam::test-failure) 1 0)))
