(defpackage :shop3-thmpr-api-asd    
    (:use :common-lisp :asdf))

(in-package shop3-thmpr-api-asd)

(defsystem :shop3-thmpr-api
    :serial t
    :depends-on (shop3)
    :components ((:file "package")
                 (:file "shop3-thmpr-api")
                 ))
