(defpackage :shop2-thmpr-api-asd    
    (:use :common-lisp :asdf))

(in-package shop2-thmpr-api-asd)

(defsystem :shop2-thmpr-api
    :serial t
    :depends-on (shop2)
    :components ((:file "package")
		 (:file "shop2-thmpr-api")
		 ))
