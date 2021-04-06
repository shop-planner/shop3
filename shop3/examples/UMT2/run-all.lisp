(IN-PACKAGE :COMMON-LISP-USER)

(SHOP3)

(ASDF:LOAD-SYSTEM "shop3")

(IN-PACKAGE :shop-user)

(defparameter +PROBLEM-DIR+ "/home/rpg/lisp/shop/shop3/examples/UMT2/")
(defparameter +DOMAIN-FILE+ "UMT2.lisp")
(defparameter +PROBLEM-PATTERN+ "pfile?.lisp")
(defmacro before-run ()
  '(define-umt-domain))

;;;--------------------------------------------------
;;; Configuration above -- code below should not have
;;; to change by domain.
;;;--------------------------------------------------

(load (concatenate 'string +PROBLEM-DIR+ +DOMAIN-FILE+))

(before-run)

(loop :for file :in (directory (concatenate 'string +PROBLEM-DIR+ +PROBLEM-PATTERN+))
      :do (load file)
          (with-open-file (str (concatenate 'string +PROBLEM-DIR+ "runall.log")
                               :direction :output :if-exists :append :if-does-not-exist :create)
            (format str
                    "~3%--------------------------------------------------~%~
                     ~a~%" (pathname-name file))

            (find-plans-stack shop::*problem* :verbose 1 :out-stream str )
            (terpri str)))
