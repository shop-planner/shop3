(in-package :common-lisp-user)

(defpackage :test-satellite
  (:import-from #:fiveam #:def-suite #:def-suite* #:def-fixture #:test)
  (:use common-lisp shop2))

(in-package test-satellite)

(def-suite* satellite-adl-tests)

(fiveam:test test-plans
  (let ((shop2::*define-silently* t)
        (directory (asdf:system-relative-pathname "shop2" "examples/satellite/strips/")))
    (load (merge-pathnames "adlSat-cleaned.lisp" directory))
    (let ((domain-file (merge-pathnames "adlSat.pddl" directory)))
      (loop :for i from 1 :to 20 with standard-plan
            :as probfilename = (format nil "p~2,'0d.pddl" i)
            :as shop-probfilename = (format nil "p~2,'0d.lisp" i)
            :as problem-file = (merge-pathnames probfilename directory)
            :do (load (merge-pathnames shop-probfilename directory))
                (setf standard-plan (first (find-plans shop2::*problem* :domain 'shop2-user::adlsat :verbose 0)))
                (fiveam:is-true (or standard-plan
                                    (progn
                                      (warn "Failed to SHOP2 plan for problem ~d" i)
                                      nil)))
                (fiveam:is-true
                 (or
                  (validate-plan standard-plan domain-file problem-file)
                  (warn "Failed to validate plan for problem ~d" i)))))))


