(IN-PACKAGE :COMMON-LISP-USER)

(SHOP3)

(ASDF:LOAD-SYSTEM "shop3/openstacks")

(IN-PACKAGE "SHOP-OPENSTACKS")

(loop :for file :in (directory "/home/rpg/lisp/shop/shop3/examples/openstacks-adl/p??.lisp")
      :do (load file)
          (with-open-file (str (asdf:system-relative-pathname "shop3" "examples/openstacks-adl/runall.log")
                               :direction :output :if-exists :append :if-does-not-exist :create)
            (format str
                    "~3%--------------------------------------------------~%~
                     ~a~%" (pathname-name file))

            (find-plans-stack shop::*problem* :verbose 1 :out-stream str )
            (terpri str)))
