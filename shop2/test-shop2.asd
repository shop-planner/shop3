;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;;
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;;
;;; The Original Code is SHOP2.  ASDF system definitions developed by
;;; Robert P. Goldman, John Maraist.  Portions created by Drs. Goldman
;;; and Maraist are Copyright (C) 2004-2007 SIFT, LLC.  These
;;; additions and modifications are also available under the
;;; MPL/GPL/LGPL licensing terms.
;;;
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.
;;; ----------------------------------------------------------------------

;;; Smart Information Flow Technologies Copyright 2006-2009 Unpublished work
;;;
;;; GOVERNMENT PURPOSE RIGHTS
;;;
;;; Contract No.         FA8650-06-C-7606,
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;;
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.

(asdf:oos 'asdf:load-op :shop-asd)
(asdf:oos 'asdf:load-op :asdf-nst)
(in-package :shop2-asd)
(load (merge-pathnames "version.lisp" *load-truename*))
(defconstant +shop-examples-dir+
             '(:relative "examples"))
(defun examples-subdir (dirname)
  (append +shop-examples-dir+ (list dirname)))

(defclass stream-test-mixin ()
  ((result-stream
    :initarg :result-stream
    :reader result-stream
    :initform t
    :documentation "Stream to which output should be written."
    )
   (verbose
    :initform nil
    :initarg :verbose
    :reader verbose
    )
   )
  (:documentation "A mixin that can be added to the test-op that includes a stream
to which test output should be written."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member (find-class 'stream-test-mixin)
                  (class-direct-superclasses (find-class 'asdf:test-op)))
    (reinitialize-instance (find-class 'asdf:test-op)
                      :direct-superclasses (cons (find-class 'shop2-asd::stream-test-mixin)
                                                 (class-direct-superclasses (find-class 'asdf:test-op))))))

(defmethod perform :around ((o stream-test-mixin) (c nst-testable))
  "Bind the output stream to the result stream."
  (flet ((verbose ()
           (intern (symbol-name '#:*nst-verbosity*) :sift.nst)))
    (let (( cl-user::*nst-default-report-stream* (result-stream o)))
      (format cl-user::*nst-default-report-stream* "~&Running test on system: ~A~%"
              (component-name c))
      (force-output cl-user::*nst-default-report-stream*)
      (let ((oldval (symbol-value (verbose))))
        (set (verbose) t)
        (format t "~&Setting ~s to t~%" (verbose))
        (call-next-method)
        (set (verbose) oldval))
      (format cl-user::*nst-default-report-stream* "~&test on system ~A completed.~%"
              (component-name c)))))
(defclass tester-cl-source-file ( cl-file-with-defconstants )
  ()
  (:documentation "Special class that will have to recompile no matter
what..."))

(defmethod operation-done-p
           ((o compile-op)
            (c tester-cl-source-file))
  "We are crushing the normal operation-done-p for compile-op, so that
the tester files get recompiled, to take into account any changes to
shop2."
  (values nil))

(defvar cl-user::*nst-default-report-stream*)

(defsystem :test-shop2
    :class nst-testable
    :nst-systems (:protection-tests
                  :shop-blocks
                  :shop-depots
                  :shop-logistic
                  :shop-pddl-tests
                  :shop-umt)
    :depends-on ((:version "shop2" #.cl-user::+shop-version+)
                 (:version :nst "1.0"))
    :version #.cl-user::+shop-version+)

;;;
;;; NST infrastructure for all unit tests.
;;;
(defun nst-group-exec (names)
  "Build a call to run NST tests assuming that none of the relevant
packages have been loaded yet."
  (let ((fun-name (intern (symbol-name '#:run-nst-commands)
                          (find-package :nst))))
    ;; try to get the tables cleared...
    (eval `(,fun-name
            :cancel))
    (eval `(,fun-name
            :run-groups
            ',(loop for (package . sym) in names
                  collect (intern (symbol-name sym)
                                  (find-package package)))))))

(defsystem :shop-test-helper
    :depends-on (:shop2 (:version :nst "1.0"))
    :default-component-class tester-cl-source-file
    :in-order-to ((load-op (compile-op :shop-test-helper)))
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative  "tests")) *load-truename*)
    :components ((:file "nst-common")))

;;;
;;; First test application --- PDDL tests.
;;;
(defsystem :shop-pddl-tests
    :class nst-testable ;; shop-tester
    :depends-on (:shop2 (:version :nst "1.0"))
    :in-order-to ((test-op (load-op :shop-pddl-tests)))
    :nst-groups ((:shop2 . pddl-tests)
                 (:shop2 . add-del-tests)
                 (:shop2 . quantified-preconditions)
                 (:shop2 . simple-when)
                 (:shop2 . quantified-when))
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative "tests")) *load-truename*)
    :components ((:file "nst-pddl")))

(defsystem :protection-tests
    :class nst-testable ;; shop-tester
    :depends-on (:shop2 (:version :nst "1.0"))
    :in-order-to ((test-op (load-op :protection-tests)))
    :nst-group (:protection-test . protection-test)
    :pathname #.(merge-pathnames (make-pathname :directory +shop-examples-dir+) *load-truename*)
    :serial t
    :components ((:file "protection-test-package")
                 (:file "protection-test")))


;;;; handle SBCL's strict notion of the way DEFCONSTANT should work. [2006/05/16:rpg]
;;;#+sbcl
;;;(defmethod traverse ((op operation) (c shop-tester))
;;;  (handler-bind ((sb-ext:defconstant-uneql
;;;                  #'(lambda (c)
;;;                      (continue c))))
;;;    (call-next-method)))


#+sbcl
(defmethod perform :around ((op operation)
                            (c cl-file-with-defconstants))
  (handler-bind ((sb-ext:defconstant-uneql
                     #'(lambda (c)
                         (continue c))))
    (call-next-method)))


;;;
;;; Second test application --- SHOP-UMT domain tests.
;;;

(defsystem :shop-umt
    :class nst-testable ;; shop-tester
    :depends-on (:shop-test-helper)
    :default-component-class tester-cl-source-file
    :in-order-to ((test-op (load-op :shop-umt))
                  (load-op (compile-op :shop-umt)))
    :pathname #.(merge-pathnames (make-pathname :directory (examples-subdir "UMT2")) *load-truename*)
    :nst-group (:shop2-user . umt-tests)
    :components ((:file "UMT2")
                 (:file "pfile1" :depends-on ("UMT2"))
                 (:file "pfile2" :depends-on ("UMT2"))
                 ;; interestingly, pfile3 does not seem solvable.
                 ;; Haven't checked to see why [2006/05/10:rpg]
                 (:file "pfile3" :depends-on ("UMT2"))
                 (:file "nst-umt" :depends-on ("UMT2" "pfile1" "pfile2" "pfile3"))))

;;;
;;; Third test application --- blocksworld.
;;;
(defsystem :shop-blocks
    :class nst-testable ;; shop-tester
    :depends-on (:shop-test-helper)
    :default-component-class tester-cl-source-file
    :pathname #.(merge-pathnames (make-pathname :directory (examples-subdir "blocks")) *load-truename*)
    :in-order-to ((test-op (load-op :shop-blocks))
                  (load-op (compile-op :shop-blocks)))
    :nst-group (:shop2-user . blocks-tests)
    :components ((:file "block2")
                 (:file "problem100" :depends-on ("block2"))
                 (:file "problem200" :depends-on ("block2"))
                 (:file "problem300" :depends-on ("block2"))
                 (:file "nst-blocks"
                        :depends-on ("problem100" "problem200" "problem300"))))

;;;
;;; Four test application --- depots.
;;;

(defsystem :shop-depots
    :class nst-testable ;; shop-tester
    :default-component-class tester-cl-source-file
    :depends-on (:shop-test-helper)
    :pathname #.(merge-pathnames (make-pathname :directory (examples-subdir "depots")) *load-truename*)
    :in-order-to ((test-op (load-op :shop-depots))
                  (load-op (compile-op :shop-depots)))
    :nst-group (:shop2-user . depot-tests)
    :components ((:file "depots")
                 (:file "pfile1" :depends-on ("depots"))
                 (:file "pfile2" :depends-on ("depots"))
                 (:file "pfile3" :depends-on ("depots"))
                 (:file "pfile4" :depends-on ("depots"))
                 (:file "pfile5" :depends-on ("depots"))
                 (:file "pfile6" :depends-on ("depots"))
                 (:file "pfile7" :depends-on ("depots"))
                 (:file "pfile8" :depends-on ("depots"))
                 (:file "pfile9" :depends-on ("depots"))
                 (:file "pfile10" :depends-on ("depots"))
                 (:file "pfile11" :depends-on ("depots"))
                 (:file "pfile12" :depends-on ("depots"))
                 (:file "pfile13" :depends-on ("depots"))
                 (:file "pfile14" :depends-on ("depots"))
                 (:file "pfile15" :depends-on ("depots"))
                 (:file "pfile16" :depends-on ("depots"))
                 (:file "pfile17" :depends-on ("depots"))
                 (:file "pfile18" :depends-on ("depots"))
                 (:file "pfile19" :depends-on ("depots"))
                 (:file "pfile20" :depends-on ("depots"))
                 (:file "pfile21" :depends-on ("depots"))
                 (:file "pfile22" :depends-on ("depots"))
                 (:file "nst-depot")))

;;;
;;; Fifth test application --- logistics.
;;;

(defsystem :shop-logistic
    :class nst-testable ;; shop-tester
    :default-component-class tester-cl-source-file
    :depends-on (:shop-test-helper)
    :pathname #.(merge-pathnames (make-pathname :directory (examples-subdir "logistic")) *load-truename*)
    :nst-group (:shop2-user . logistic-tests)
    :components ((:file "logistic")
                 (:file "Log_ran_problems_15" :depends-on ("logistic"))
                 (:file "Log_ran_problems_20" :depends-on ("logistic"))
                 (:file "Log_ran_problems_25" :depends-on ("logistic"))
                 (:file "Log_ran_problems_30" :depends-on ("logistic"))
                 (:file "Log_ran_problems_35" :depends-on ("logistic"))
                 (:file "Log_ran_problems_40" :depends-on ("logistic"))
                 (:file "Log_ran_problems_45" :depends-on ("logistic"))
                 (:file "Log_ran_problems_50" :depends-on ("logistic"))
                 (:file "Log_ran_problems_55" :depends-on ("logistic"))
                 (:file "Log_ran_problems_60" :depends-on ("logistic"))
                 (:file "nst-logistic")
                 )
    :in-order-to ((test-op (load-op :shop-logistic))
                  (load-op (compile-op :shop-logistic))))

;;; make sure we don't do this only once...

;;;---------------------------------------------------------------------------
;;; SHOP2 Web Services tester.
;;; this seems bitrotted... [2007/10/22:rpg]
;;;---------------------------------------------------------------------------

#|
(defsystem :shop-semweb
    :class nst-testable ;; shop-tester
    :depends-on (:shop-test-helper)
    :default-component-class tester-cl-source-file
    :in-order-to ((test-op (load-op :shop-semweb))
                  (load-op (compile-op :shop-semweb)))
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative :up "examples" "WebServices")) *load-truename*)
    :components (
                 (:file "poirot")
                 (:file "PoirotSimulation")
                 (:file "poirot-problem" :depends-on ("poirot" "PoirotSimulation"))
                 (:file "plans")
                 ))

;;; needs to be changed to check the results we find.
;;;(defmethod perform :after ((op test-op) (component (eql (find-system :shop-semweb))))
;;;  (eval `(,(intern "TEST-POIROT-SHOP" :shop2))))
|#


