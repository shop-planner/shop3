(defpackage :translate-satellite-problems-asd
  (:use :common-lisp :asdf)
  )
(in-package :translate-satellite-problems-asd)
(defsystem translate-satellite-problems
    :depends-on (:shop2/pddl-helpers :pddl-utils)
  :serial t
  :components ((:file "package")
               (:file "translate-problems"))
  )
