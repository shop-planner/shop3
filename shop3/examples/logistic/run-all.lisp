(IN-PACKAGE :COMMON-LISP-USER)

(SHOP3)

(ASDF:LOAD-SYSTEM "shop3")

(IN-PACKAGE :shop-user)

(defparameter +PROBLEM-DIR+ "/home/rpg/lisp/shop/shop3/examples/logistic/")
(defparameter +DOMAIN-FILE+ "logistic.lisp")
(defparameter +PROBLEM-PATTERN+ "Log_ran_problems_*.lisp")
(defmacro before-run ()
  '(logistics-domain))


;;;--------------------------------------------------
;;; Configuration above -- code below should not have
;;; to change by domain.
;;;--------------------------------------------------

(load (concatenate 'string +PROBLEM-DIR+ +DOMAIN-FILE+))

(before-run)

(loop :for file :in (directory (concatenate 'string +PROBLEM-DIR+ +PROBLEM-PATTERN+))
      :as file-name = (pathname-name file)
      :as probset-name = (intern
                          (concatenate 'string
                                       "SET"
                                       (subseq file-name (1+ (position #\_ file-name :from-end t))))
                          :shop-user)
      :do (load file)
          (loop :for probname :in (or (sort (copy-list  (get probset-name :problems))
                                             #'string-lessp)
                                      (error "No problem set named ~a" probset-name))
                :do (with-open-file (str (concatenate 'string +PROBLEM-DIR+ "runall.log")
                                         :direction :output :if-exists :append :if-does-not-exist :create)
                      (format str
                              "~3%--------------------------------------------------~%~
                     ~a~%" probname)

             (find-plans-stack probname :verbose 1 :out-stream str )
             (terpri str))))
