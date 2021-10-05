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
;;; Robert P. Goldman, John Maraist, Ugur Kuter, and other SIFT
;;; employees.  Portions created by SIFT employees are Copyright (C)
;;; 2004-2017 SIFT, LLC.  These additions and modifications are also
;;; available under the MPL/GPL/LGPL licensing terms.
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

;;; Smart Information Flow Technologies Copyright 2006-2019 Unpublished work
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

(defpackage :shop-asd
    (:use :common-lisp :asdf)
    (:nicknames :shop2-asd :shop3-asd)
    #+(or allegro sbcl ccl clisp abcl lispworks ecl mkcl)
    (:import-from #+allegro aclmop     #+sbcl sb-mop
                  #+ccl ccl #+clisp clos #+cmucl pcl
                  #+abcl mop #+(or ecl mkcl) clos
                  #+lispworks hcl
                  #:class-direct-superclasses))
(in-package :shop3-asd)

;;;
;;; The main system.
;;;
(defsystem :shop3
    :serial t
    :license "Mozilla Public License"
    :depends-on ((:version "shop3/common" (:read-file-form "shop-version.lisp-expr"))
                 (:version "shop3/theorem-prover" (:read-file-form "shop-version.lisp-expr"))
                 "trivial-garbage"
                 :alexandria
                 :iterate)
    :version (:read-file-form "shop-version.lisp-expr")
    :in-order-to ((test-op (test-op :shop3/test)))
    :components  (
       (:file "package")
       (:file "decls")

       (:module io
                :components ((:file "input")
                             (:file "output")
                             (:file "debugging")
                             (:file "shop-pprint")))
       (:module pddl
                :serial t
                :components ((:file "decls")
                             (:file "fluents")
                             (:file "pddl")
                             (:file "prover")))
       (:module search
                :pathname "planning-engine/"
                :components ((:file "protections")
                             (:file "task-reductions")
                             (:file "search")))

       (:module "explicit-stack-search"
                :serial t
                :components ((:file "decls")
                             ;; this is for the new plan tree that contains
                             ;; dependency information.
                             (:file "plan-tree")
                             (:file "backtrack-stack")
                             (:file "explicit-search")))

       (:module "looping-tasks"
                :serial t
                :components ((:file "loop-extensions")))
       

       ;; this is for the original SHOP3 plan trees.
       (:module tree
        :pathname "planning-tree/"
        :components ((:file "tree-accessors")
                     (:file "tree-reductions")))
       ;; FIXME: depends on explicit-stack-search only, I believe, but
       ;; I don't have the energy to replace the :SERIAL t spec for
       ;; this system with something that is more refined.
       (:module "minimal-subtree"
        :description "When there's an upset in plan execution, find a
minimal affected subtree."
         :serial t
         :components ((:file "package")
                      (:file "decls")
                      (:file "minimal-subtree")))
       (:file "plan-repair"
              :pathname "explicit-stack-search/plan-repair"
              :depends-on ("tree" "explicit-stack-search"))
       (:file "shop3"
              :perform (load-op :before (op c)
                               (declare (ignorable op c))
                               (set (intern (symbol-name '#:*shop-version*)
                                            (find-package :shop3))
                                    (asdf:component-version (asdf:find-system "shop3")))))
       (:file "plan-printer" :depends-on ("package"
                                           "decls"))))

(defsystem :shop3/common
    :serial t
    :pathname "common/"
    :version (:read-file-form "shop-version.lisp-expr")
    :depends-on (:shop3/unifier :iterate :alexandria)
    :components ((:file "package-common")
                 (:file "common")
                 (:file "state-decls")
                 (:file "state-utils")))

(defsystem :shop3/theorem-prover
    :serial t
    :pathname "theorem-prover/"
    :depends-on ("shop3/common" "shop3/unifier")
    :version (:read-file-form "shop-version.lisp-expr")
    :components ((:file "package-thpr")
                 (:file "decls")
                 (:file "theorem-prover")))


(defsystem :shop3/unifier
  :serial t
  :pathname "unification/"
  :in-order-to ((test-op (test-op "shop3/test-unifier")))
  :version (:read-file-form "shop-version.lisp-expr")
  :components ((:file "package-unifier")
               (:file "tracer")
               (:file "unify")))


(defsystem :shop3/plan-grapher
  :depends-on ("shop3" "cl-dot")
  :serial t
  :pathname "plan-grapher/"
  :components ((:file "package")
               (:file "decls")
               (:file "graph-plan-tree"))
  )

(defsystem "shop3/pddl-helpers"
    :depends-on ("shop3" "pddl-utils")
  :pathname "pddl/"
  :serial t                             ; pddl-helpers contains defpackage
  :components ((:file "pddl-helpers")
               (:file "validate-repairs")))


;;;---------------------------------------------------------------------------
;;; Testing
;;;---------------------------------------------------------------------------

(asdf:load-system "fiveam-asdf")

(defclass tester-cl-source-file ( cl-file )
  ()
  (:documentation "Special class that will have to recompile no matter
what..."))

(defmethod operation-done-p
           ((o compile-op)
            (c tester-cl-source-file))
  "We are crushing the normal operation-done-p for compile-op, so that
the tester files get recompiled, to take into account any changes to
shop3."
  (values nil))

(defclass shop-tester-mixin ()
     ()
  (:documentation "Mixin that adds silent functioning of SHOP3."))


(defclass shop-fiveam-tester (shop-tester-mixin fiveam-tester-system) ())

(defsystem shop3/openstacks
    :depends-on (:shop3)
  :serial t
  :pathname "examples/openstacks-adl/"
  :components ((:file "package")
               (:file "domain")))

(defsystem shop3/rovers
    :depends-on (:shop3)
  :serial t
  :pathname "examples/rovers/strips/"
  :components ((:file "domain")))


(defsystem shop3/test
    :defsystem-depends-on ((:version "fiveam-asdf" "2"))
    :class shop-fiveam-tester
    :test-names ((pddl-tests . :shop3)  ; 141
                 (protection-test . :protection-test)  ; 16
                 ;; all the following are now subsumed into all-shop3-internal-tests
                 (arity-test . :arity-test) ; 6
                 (io-tests . :arity-test) ; 40
                 ;; end of internal tests
                 (umt-domain-tests . :shop3-user) ; 8
                 (blocks-tests . :shop3-user) ; 5
                 (depot-tests . :shop3-user) ; 44
                 (logistics-tests . :shop3-user) ; 408
                 (singleton-tests . :shop3-user) ; 44
                 (misc-tests . :shop3-user) ; 10
                 (minimal-subtree-tests . :shop3-user) ; 12
                 (enhanced-plan-tree . :shop3-user) ; 2
                 (theorem-prover-tests . :shop-theorem-prover-tests)  ; 4
                 (test-plan-repair . :shop-replan-tests) ; 3
                 (test-shop-states . :test-states) ; 110
                 )
    :num-checks 932
    :depends-on ((:version "shop3" (:read-file-form "shop-version.lisp-expr"))
                 "shop3/openstacks"
                 "shop3/pddl-helpers"
                 "pddl-utils")
    :version (:read-file-form "shop-version.lisp-expr")
    :components ((:module "shop-test-helper"
                          :pathname "tests/"
                          :components ((:file "common")))
                          
                 (:file "silent-shop-test")
                 (:file "theorem-prover-tests"
                        :pathname "tests/theorem-prover-tests")
                 (:module "shop-pddl-tests"
                          :pathname "tests/"
                          :components ((:file "pddl-tests")))
                 (:module "shop-protection-tests"
                          :pathname "examples/"
                          :serial t
                          :components ((:file "protection-test-package")
                                       (:file "protection-test")))
                 (:module "shop-internal-tests"
                          :pathname "tests/"
                          :components ((:file "warns-check")
                                       (:file "at-package" :depends-on ("warns-check"))
                                       (:file "shop-internal-test-suite")
                                       (:file "arity-tests" :depends-on ("at-package" "shop-internal-test-suite"))
                                       (:module "umt-domain"
                                          :components
                                          ((:file "umt2-domain")
                                           (:file "pfile1")
                                           (:static-file "axioms.lisp")
                                           (:static-file "operators.lisp")))
                                       (:file "io-tests" :depends-on ("at-package" "umt-domain"))
                                       (:file "singleton-tests" :depends-on ("at-package" "umt-domain"))
                                       (:file "state-tests" :depends-on ("at-package" "umt-domain"))
                                       (:file "misc" :depends-on ("at-package" "umt-domain"))))
                 ;;; FIXME: put these tests in a separate package, instead of in SHOP3-USER [2012/09/05:rpg]
                 (:module "shop-umt" 
                          :pathname "examples/UMT2/"
                          :components ((:file "UMT2")
                                       (:file "pfile1")
                                       (:file "pfile2")
                                       ;; FIXME: interestingly, pfile3 does not seem solvable.
                                       ;; Haven't checked to see why [2006/05/10:rpg]
                                       (:file "pfile3")
                                       (:file "umt-tests" :depends-on ("UMT2" "pfile1" "pfile2" "pfile3"))))
                 (:module "shop-blocks"
                          :pathname "examples/blocks"
                          :components ((:file "block2")
                                       (:file "problem100")
                                       (:file "problem200")
                                       (:file "problem300")
                                       (:file "tests"
                                              :depends-on ("problem100" "problem200" "problem300"))))
                 (:module "shop-depots"
                          :pathname "examples/depots/"
                          :components ((:file "depots")
                                       (:file "pfile1")
                                       (:file "pfile2")
                                       (:file "pfile3")
                                       (:file "pfile4")
                                       (:file "pfile5")
                                       (:file "pfile6")
                                       (:file "pfile7")
                                       (:file "pfile8")
                                       (:file "pfile9")
                                       (:file "pfile10")
                                       (:file "pfile11")
                                       (:file "pfile12")
                                       (:file "pfile13")
                                       (:file "pfile14")
                                       (:file "pfile15")
                                       (:file "pfile16")
                                       (:file "pfile17")
                                       (:file "pfile18")
                                       (:file "pfile19")
                                       (:file "pfile20")
                                       (:file "pfile21")
                                       (:file "pfile22")
                                       (:file "tests")))
                 (:module "shop-logistic"
                          :pathname "examples/logistic/"
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
                                       (:file "tests" :depends-on ("Log_ran_problems_15"
                                                                   "Log_ran_problems_20"
                                                                   "Log_ran_problems_25"
                                                                   "Log_ran_problems_30"
                                                                   "Log_ran_problems_35"
                                                                   "Log_ran_problems_40"
                                                                   "Log_ran_problems_45"
                                                                   "Log_ran_problems_50"
                                                                   "Log_ran_problems_55"
                                                                   "Log_ran_problems_60"))))
                 (:module "minimal-subtree"
                          :components ((:file "tests")))
                 (:file "replan-tests" :pathname "tests/replan-tests")))


(defsystem shop3/test-satellite
    :defsystem-depends-on ((:version "fiveam-asdf" "2"))
    :class shop-fiveam-tester
    :test-names (("SATELLITE-ADL-TESTS" . "TEST-SATELLITE"))
    :num-checks 80
    :depends-on ((:version "shop3" (:read-file-form "shop-version.lisp-expr"))
                 "pddl-utils")
    :version (:read-file-form "shop-version.lisp-expr")
    :pathname "examples/satellite/strips/"
    :serial t
    :components ((:file "test-satellite")))

(defsystem shop3/test-unifier
    :defsystem-depends-on ((:version "fiveam-asdf" "2"))
    :class shop-fiveam-tester
    :test-names (("TEST-SHOP-UNIFIER" . "SHOP-UNIFIER-TESTS"))
    :num-checks 36
    :depends-on ("shop3/unifier" "alexandria")
    :pathname "tests/"
    :serial t
    :components ((:file "unifier-tests")))

#+ecl
(defmethod perform :before ((op test-op)
                            (c (eql (asdf:find-system :shop3/test))))
  (macrolet ((set-limit (limited limit)
               `(unless (>= (ext:get-limit ',limited) ,limit)
                  (ext:set-limit ',limited ,limit ))))
    (set-limit ext:binding-stack 20480 )
    (set-limit ext:c-stack 10000000)
    (set-limit ext:lisp-stack 1000000)))
