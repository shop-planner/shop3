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

;;; Smart Information Flow Technologies Copyright 2006-2007 Unpublished work
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
(in-package :shop2-asd)

;;;
;;; The main system.
;;;
(defsystem :shop2
    :serial t
    :default-component-class cl-file-with-defconstants
    :depends-on ((:version "shop2/common" #.cl-user::+shop-version+)
                 (:version "shop2/theorem-prover" #.cl-user::+shop-version+))
    :version #.cl-user::+shop-version+
    :in-order-to ((test-op (test-op :test-shop2)))
    :components  (
       (:file "package")
       (:file "decls")

       (:module io
                :components ((:file "input")
                                    (:file "output")
                                    (:file "debugging")))
       (:module pddl
                :components ((:file "pddl")))
       (:module search
                :pathname "planning-engine/"
                :components ((:file "protections")
                                     (:file "task-reductions")
                                     (:file "search")))
       (:module tree
                :pathname "planning-tree/"
                :components ((:file "tree-accessors")
                                     (:file "tree-reductions")))
       (:file "shop2")))

(defsystem :shop2/common
    :serial t
    :pathname "common/"
    :version #.cl-user::+shop-version+
    :depends-on (:shop2/unifier)
    :components ((:file "package-common")
                 (:file "common")
                 (:file "state-decls")
                 (:file "state-utils")))

(defsystem :shop2/theorem-prover
    :serial t
    :pathname "theorem-prover/"
    :depends-on ("shop2/common" "shop2/unifier")
    :version #.cl-user::+shop-version+
    :components ((:file "package-thpr")
                 (:file "decls")
                 (:file "theorem-prover")))


(defsystem :shop2/unifier
    :serial t
        :pathname "unification/"
        :depends-on ("shop-asd")
    :version #.cl-user::+shop-version+
    :components ((:file "package-unifier")
                 (:file "tracer")
                 (:file "unify")))

