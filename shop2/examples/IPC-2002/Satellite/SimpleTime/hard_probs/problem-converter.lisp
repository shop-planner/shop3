(defparameter *debug-mode* t)

(defun convert-problems (L)
  (dolist (fn L)
    (problem-converter fn (concatenate 'string fn ".lisp"))
  )
)

(defun write-SHOP2-problem (shop2-problem-filename
                            problem-name
                            domain-name
                            objects-list
                            init-list
                            goal-list
                            metric-list)
  (with-open-file (outfile shop2-problem-filename :direction :output
                                                  :if-exists :supersede)
    (format outfile  "(defproblem ~A ~A~%" problem-name domain-name)
    (format outfile "  (~%")
    (when *debug-mode*
      (format outfile "    ;;;~%")
      (format outfile "    ;;;  facts~%")
      (format outfile "    ;;;~%")

      (do* ((ol objects-list (cdddr ol))
            (obj (first ol) (first ol))
            (cls (third ol) (third ol)))
           ((null ol))
        (format outfile "    (~A ~A)~%" cls obj)
      ))

    (format outfile "    ;;;~%")
    (format outfile "    ;;;  initial states~%")
    (format outfile "    ;;;~%")

    (dolist (s init-list)
      (format outfile "    ~A~%" s)
    )
    (format outfile "  )~%")
    (format outfile "  ;;;~%")
    (format outfile "  ;;; goals~%")
    (format outfile "  ;;;~%")
    (format outfile "  (:ordered ~%")
    (dolist (s (cdr goal-list))
      (format outfile "    ~A~%" (cons '\:task s))
    )
    (format outfile "    (:task main)~%")
    (format outfile "  )~%")
    (format outfile ")~%")
  )
)

(defun problem-converter (pddl-problem-filename shop2-problem-filename)
  (with-open-file (infile pddl-problem-filename :direction :input)
    (let* ((pddl-problem (read infile))
            problem-name
            domain-name
            objects-list
            init-list
            goal-list
            metric-list)
      ;;;
      ;;; read the PDDL problem
      ;;;
      (dolist (s pddl-problem)
        (when (and (listp s) (eql (first s) 'problem))
          (setf problem-name (second s)))
        (when (and (listp s) (eql (first s) :domain))
          (setf domain-name (second s)))
        (when (and (listp s) (eql (first s) :objects))
          (setf objects-list (cdr s)))
        (when (and (listp s) (eql (first s) :init))
          (setf init-list (cdr s)))
        (when (and (listp s) (eql (first s) :goal))
          (setf goal-list (second s)))
        (when (and (listp s) (eql (first s) :metric))
          (setf metric-list (second s)))
      )

      ;;;
      ;;; Override default values
      ;;;
      (setf domain-name  "adlsimpleTimeSat")

      (write-SHOP2-problem shop2-problem-filename
                           problem-name
                           domain-name
                           objects-list
                           init-list
                           goal-list
                           metric-list)
    )
  )
)

(convert-problems '(
  "pfile1"
  "pfile2"
  "pfile3"
  "pfile4"
  "pfile5"
  "pfile6"
  "pfile7"
  "pfile8"
  "pfile9"
  "pfile10"
  "pfile11"
  "pfile12"
  "pfile13"
  "pfile14"
  "pfile15"
  "pfile16"))


