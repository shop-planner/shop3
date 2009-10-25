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
  (defdomain (protection-test-domain :noset t)
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

(def-fixtures protections-domain ()
   (setup
    (init-protections-domain)))

(def-fixtures empty-state ()
   (s (shop::make-initial-state setup
                                :list nil)))

(defmacro apply-op (opname &key protections)
  `(locally
    (declare (special setup s))
    (apply-operator setup s
                    '( ,opname )
                    (shop::operator setup ',opname)
                    ,protections 0 nil)))

(nst:def-values-criterion (:failed () (retval &rest args)
                                   :declare ((ignore args)))
    `(if (eq retval 'fail)
         (sift.nst::make-check-result)
         (sift.nst:emit-failure)))

(nst:def-values-criterion (:unfailed () (retval &rest args)
                                    :declare ((ignore args)))
    `(if (eq retval 'fail)
         (sift.nst:emit-failure)
         (sift.nst::make-check-result)))



(def-test-group protection-test (protections-domain)
  ;; item number one
  (def-check  (can-apply-pos-op :fixtures (empty-state))
      (:unfailed) (apply-op !positive-op))
  (def-check  (can-apply-neg-op :fixtures (empty-state))
      (:unfailed) (apply-op !negative-op))
  ;; item number two
  (def-check  (can-apply-add-protect :fixtures (empty-state))
      (:unfailed)
    (progn (apply-op !positive-op)
           (apply-op !add-protect)))
  (def-check  (can-apply-remove-protect :fixtures (empty-state))
      (:unfailed) (apply-op !remove-protect))
  (def-check  (can-apply-add-neg-protect :fixtures (empty-state))
      (:unfailed) (apply-op !add-neg-protect))
  (def-check  (can-apply-remove-neg-protect :fixtures (empty-state))
      (:unfailed) (apply-op !remove-neg-protect))

  ;; check protections before adding
  (def-check  (protection-init-check :fixtures (empty-state))
      (:err) (apply-op !add-protect))
  (def-check  (protection-init-check-neg :fixtures (empty-state))
      (:err)
    (progn
      (apply-op !positive-op)
      (apply-op !add-neg-protect)))

  ;; item number three
  (def-check  (protection-effective :fixtures (empty-state))
      (:failed)
    (progn
      (apply-op !positive-op)
      (multiple-value-bind (op tag protections)
          (apply-op !add-protect)
        (declare (ignore op tag) (special setup s))
        (apply-operator setup s '(!negative-op)
                        (shop::operator setup '!negative-op)
                        protections 0 nil))))
  (def-check  (neg-protection-effective :fixtures (empty-state))
      (:failed)
    (multiple-value-bind (op tag protections)
        (apply-op !add-neg-protect)
      (declare (ignore op tag) (special setup s))
      (apply-operator setup s '(!positive-op)
                      (shop::operator setup '!negative-op)
                      protections 0 nil)))

  ;; items number four and five
  (def-check  (protection-add-and-remove :fixtures (empty-state))
      (:unfailed)
    (progn
      (apply-op !positive-op)
      (multiple-value-bind (op tag protections)
          (apply-op !add-protect)
        (declare (ignore op tag) (special setup s))
        (multiple-value-bind (op tag protections)
            (apply-op !remove-protect :protections protections)
          (declare (ignore op tag))
          (apply-op !negative-op :protections protections)))))
  (def-check  (neg-protection-add-and-remove :fixtures (empty-state))
      (:unfailed)
    (progn
      (multiple-value-bind (op tag protections)
          (apply-op !add-neg-protect)
        (declare (ignore op tag) (special setup s))
        (multiple-value-bind (op tag protections)
            (apply-op !remove-neg-protect :protections protections)
          (declare (ignore op tag))
          (apply-op !positive-op :protections protections)))))

  ;; item number six --- protection arithmetic
  (def-check  (protection-arithmetic-positive :fixtures (empty-state))
      (:failed)
    (progn
      (apply-op !positive-op)
      (multiple-value-bind (op tag protections)
          (apply-op !add-protect)
        (declare (ignore op tag) (special setup s))
        (multiple-value-bind (op tag protections)
            (apply-op !add-protect :protections protections)
          (declare (ignore op tag))
          (multiple-value-bind (op tag protections)
              (apply-op !remove-protect :protections protections)
            (declare (ignore op tag))
            (apply-op !negative-op :protections protections))))))
  (def-check  (protection-arithmetic-positive-2 :fixtures (empty-state))
      (:unfailed)
    (progn
      (apply-op !positive-op)
      (multiple-value-bind (op tag protections)
          (apply-op !add-protect)
        (declare (ignore op tag) (special setup s))
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
  (def-check  (protection-arithmentic-neg-1 :fixtures (empty-state))
      (:failed)
    (multiple-value-bind (op tag protections)
        (apply-op !add-neg-protect)
      (declare (ignore op tag) (special setup s))
      (multiple-value-bind (op tag protections)
          (apply-op !add-neg-protect :protections protections)
        (declare (ignore op tag))
        (multiple-value-bind (op tag protections)
            (apply-op !remove-neg-protect :protections protections)
          (declare (ignore op tag))
          (apply-op !positive-op :protections protections)))))
  (def-check  (protection-arithmentic-neg-2 :fixtures (empty-state))
      (:unfailed)
    (multiple-value-bind (op tag protections)
        (apply-op !add-neg-protect)
      (declare (ignore op tag) (special setup s))
      (multiple-value-bind (op tag protections)
          (apply-op !add-neg-protect :protections protections)
        (declare (ignore op tag))
        (multiple-value-bind (op tag protections)
            (apply-op !remove-neg-protect :protections protections)
          (declare (ignore op tag))
          (multiple-value-bind (op tag protections)
              (apply-op !remove-neg-protect :protections protections)
            (declare (ignore op tag))
            (apply-op !positive-op :protections protections))))))
  )
