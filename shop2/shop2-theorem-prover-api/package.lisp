(in-package :common-lisp-user)    

(defpackage :shop2-thmpr-api
    (:nicknames :sthmp-api)
    (:use common-lisp)
    (:export #:unify
	     #:unify-p
	     #:unify-within-p
	     #:variablep
	     #:fail  
	     #:apply-substitution
	     #:compose-substitutions
    ))
