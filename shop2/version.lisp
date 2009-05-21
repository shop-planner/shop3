(in-package :cl-user)

(let ((shop-version "2.4.0"))
  (if (boundp '+shop-version+)
      (unless (string-equal +shop-version+ shop-version)
        (cerror "Continue and reset shop version."
                "Resetting +shop-version+ constant from ~a to ~a.  This probably indicates an error."
                +shop-version+ shop-version))
      (defconstant +shop-version+ shop-version)))



