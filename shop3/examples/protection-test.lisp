;;; -*- mode: common-lisp; coding: unix; -*-
;;;---------------------------------------------------------------------------
;;; Copyright 2008 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;;
;;;  This file made available together with the SHOP2 system, according to the
;;;  SHOP2 system's license
;;;
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This file is intended to supply a number of unit tests to
;;;    determine whether or not protections are working.  Propose the
;;;    following test structure:
;;;    1.  Check to make sure that an action can correctly be applied
;;;    to a state (background test).  This state will violate a
;;;    protection added by the action in 2, below.  Modified to make
;;;    two variants of the action, one which adds a proposition, and
;;;    one which deletes it.
;;;    2.  Create a protection-adding action and check that it can be
;;;    successfully applied.
;;;    3.  Check to see that the first action canNOT be successfully
;;;    applied after applying the second.
;;;    4.  Create a protection-removing action and check that it can
;;;    be successfully applied, both after the protection-adding
;;;    action and without that action having been applied.
;;;    5.  Check to see that the first action once again can be
;;;    successfully added after a protection has been added and
;;;    deleted.
;;;    6.  Do a simple test of protection arithmetic, verifying that
;;;    our first action cannot be added if a protection has been added
;;;    twice, but only removed once, and then verifying that the
;;;    action can be added if a protection has been added twice, then
;;;    deleted twice.
;;;
;;;    NOTE:  SIFT boilerplate above must be replaced with SHOP license...
;;;--------------------------------------------------------------------------

(in-package :protection-test)

(defun init-protections-domain ()
  (defdomain (protection-test-domain :noset t
                                     :redefine-ok t)
    (
     (:operator (!positive-op)
                ()                      ;no preconditions
                ()
                ((prop)))
     (:operator (!negative-op)
                ()                      ;no preconditions
                ((prop))
                ())
     (:operator (!add-protect)
                ()
                ()
                ((:protection (prop))))
     (:operator (!remove-protect)
                ()
                ((:protection (prop)))
                ())
     (:operator (!add-neg-protect)
                ()
                ()
                ((:protection (not (prop)))))
     (:operator (!remove-neg-protect)
                ()
                ((:protection (not (prop))))
                ()))))

(def-fixture protections-domain ()
    (let ((setup (init-protections-domain)))
      (&body)))


(def-fixture empty-state ()
    (let ((s (shop::make-initial-state setup
                                :list nil)))
      (&body)))

(defmacro apply-op (opname &key protections)
  `(apply-operator setup s
                    '( ,opname )
                    (shop::operator setup ',opname)
                    ,protections 0 nil))

(defmacro failed (body)
  `(with-fixture empty-state ()
     (fiveam:is (eq ,body 'fail))))

(defmacro unfailed (body)
  `(with-fixture empty-state ()
     (fiveam:is (not (eq ,body 'fail)))))

(test protection-test
  (with-fixture protections-domain ()
    ;; item number one
    (unfailed (apply-op !positive-op))
    (unfailed (apply-op !negative-op))
    ;; item number two
    (unfailed
     (progn (apply-op !positive-op)
            (apply-op !add-protect)))
    (unfailed (apply-op !remove-protect))
    (unfailed
     (apply-op !add-neg-protect))
    (unfailed (apply-op !remove-neg-protect))

    ;; check protections before adding
    (with-fixture empty-state ()
      (fiveam:signals error (apply-op !add-protect)))
    (with-fixture empty-state ()
      (fiveam:signals error
        (progn
          (apply-op !positive-op)
          (apply-op !add-neg-protect))))

    ;; item number three
    (failed
     (progn
       (apply-op !positive-op)
       (multiple-value-bind (op tag protections)
           (apply-op !add-protect)
         (declare (ignore op tag))
         (apply-operator setup s '(!negative-op)
                         (shop::operator setup '!negative-op)
                         protections 0 nil))))
    (failed
     (multiple-value-bind (op tag protections)
         (apply-op !add-neg-protect)
       (declare (ignore op tag))
       (apply-operator setup s '(!positive-op)
                       (shop::operator setup '!negative-op)
                       protections 0 nil)))

    ;; items number four and five
    (unfailed
     (progn
       (apply-op !positive-op)
       (multiple-value-bind (op tag protections)
           (apply-op !add-protect)
         (declare (ignore op tag))
         (multiple-value-bind (op tag protections)
             (apply-op !remove-protect :protections protections)
           (declare (ignore op tag))
           (apply-op !negative-op :protections protections)))))
    (unfailed
     (progn
       ;;      (format t "~&State S is: ~s~%" s)
       (multiple-value-bind (op tag protections)
           (apply-op !add-neg-protect)
         (declare (ignore op tag))
         (multiple-value-bind (op tag protections)
             (apply-op !remove-neg-protect :protections protections)
           (declare (ignore op tag))
           (apply-op !positive-op :protections protections)))))

    ;; item number six --- protection arithmetic
    (failed
     (progn
       (apply-op !positive-op)
       (multiple-value-bind (op tag protections)
           (apply-op !add-protect)
         (declare (ignore op tag))
         (multiple-value-bind (op tag protections)
             (apply-op !add-protect :protections protections)
           (declare (ignore op tag))
           (multiple-value-bind (op tag protections)
               (apply-op !remove-protect :protections protections)
             (declare (ignore op tag))
             (apply-op !negative-op :protections protections))))))
    (unfailed
     (progn
       (apply-op !positive-op)
       (multiple-value-bind (op tag protections)
           (apply-op !add-protect)
         (declare (ignore op tag))
         (multiple-value-bind (op tag protections)
             (apply-op !add-protect :protections protections)
           (declare (ignore op tag))
           (multiple-value-bind (op tag protections)
               (apply-op !remove-protect :protections protections)
             (declare (ignore op tag))
             (multiple-value-bind (op tag protections)
                 (apply-op !remove-protect :protections protections)
               (declare (ignore op tag))
               (apply-op !negative-op :protections protections)))))))
    (failed
     (multiple-value-bind (op tag protections)
         (apply-op !add-neg-protect)
       (declare (ignore op tag))
       (multiple-value-bind (op tag protections)
           (apply-op !add-neg-protect :protections protections)
         (declare (ignore op tag))
         (multiple-value-bind (op tag protections)
             (apply-op !remove-neg-protect :protections protections)
           (declare (ignore op tag))
           (apply-op !positive-op :protections protections)))))
    (unfailed
     (multiple-value-bind (op tag protections)
         (apply-op !add-neg-protect)
       (declare (ignore op tag))
       (multiple-value-bind (op tag protections)
           (apply-op !add-neg-protect :protections protections)
         (declare (ignore op tag))
         (multiple-value-bind (op tag protections)
             (apply-op !remove-neg-protect :protections protections)
           (declare (ignore op tag))
           (multiple-value-bind (op tag protections)
               (apply-op !remove-neg-protect :protections protections)
             (declare (ignore op tag))
             (apply-op !positive-op :protections protections))))))))

