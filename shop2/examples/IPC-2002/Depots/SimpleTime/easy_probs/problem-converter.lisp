(in-package :common-lisp-user)

(defparameter *debug-mode* t)

(defun convert-problems (L)
  (dolist (fn L)
    (problem-converter fn (concatenate 'string fn ".lisp") fn)
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
    (format outfile "(in-package :shop2-user)~%")
    (format outfile  "(defproblem ~A ~A~%" problem-name domain-name)
    (format outfile "  (~%")
    (when *debug-mode*
      (format outfile "    ;;;~%")
      (format outfile "    ;;;  facts~%")
      (format outfile "    ;;;~%")

      (let* (objects
             previous
             object-type)
        (dolist (ss objects-list)
          (cond ((and (not (eql ss '-))
                      (eql previous nil))
                 (cond ((eql objects nil)
                        (setf objects (list ss)))
                       ((not (eql objects nil))
                        (nconc objects (list ss)))))
                 ((eql ss '-)
                  (setf previous t))
                ((and (not (eql ss '-))
                      (eql previous t))
                  (setf object-type ss)
                        (dolist (s objects)
                                  (format outfile "    (~A ~A)~%" object-type s))
                (setf objects nil)
                (setf previous nil)))
     )))

;      (do* ((ol objects-list (cdddr ol))
;            (obj (first ol) (first ol))
;            (cls (third ol) (third ol)))
;           ((null ol))
;        (format outfile "    (~A ~A)~%" cls obj)
;      ))

    (format outfile "    ;;;~%")
    (format outfile "    ;;;  initial states~%")
    (format outfile "    ;;;~%")

    (dolist (s init-list)
      (if (eql (first s) '=)
        (format outfile "    ~A~%" (append (second s) (list (third s))))
        (format outfile "    ~A~%" s)
      )
    )

    (format outfile "  )~%")
    (format outfile "  ;;;~%")
    (format outfile "  ;;; goals~%")
    (format outfile "  ;;;~%")
    (format outfile "  ((achieve-goals (~%")
    (dolist (s (cdr goal-list))
      (format outfile "    ~A~%" s))
    (format outfile "  )))~%")
    (format outfile ")~%")
  )
)

(defun problem-converter (pddl-problem-filename shop2-problem-filename problem-name)
  (with-open-file (infile pddl-problem-filename :direction :input)
    (let* ((pddl-problem (read infile))
            ; problem-name
            domain-name
            objects-list
            init-list
            goal-list
            metric-list)
      ;;;
      ;;; read the PDDL problem
      ;;;
      (dolist (s pddl-problem)
        ; (when (and (listp s) (eql (first s) 'problem))
        ;   (setf problem-name (second s)))
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
  "pfile16"
  "pfile17"
  "pfile18"
  "pfile19"
  "pfile20"
  "pfile21"
  "pfile22"))

