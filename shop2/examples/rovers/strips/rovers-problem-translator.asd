(defpackage :rovers-problem-translator-asd
  (:use :common-lisp :asdf)
  )
(in-package :rovers-problem-translator-asd)

(defsystem rovers-problem-translator
    :depends-on (:iterate :shop2 :alexandria)
  :components ((:file "problem-converter"))
  )