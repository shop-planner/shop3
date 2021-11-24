(defpackage :openstacks-problem-translator-asd
  (:use :common-lisp :asdf)
  )
(in-package :openstacks-problem-translator-asd)

(defsystem openstacks-problem-translator
    :depends-on (:iterate :shop3 :shop3/pddl-helpers :alexandria)
  :components ((:file "problem-converter"))
  )
