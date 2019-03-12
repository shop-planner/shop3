;;;
;;; *** Problem Converter ***
;;;
;;; This is a converter which translates ROVER PDDL problems to ROVER SHOP2 problems.
;;; The main function is problem-converter, which takes two parameters:
;;;
;;;  (1) the name of the input file containing the PDDL problem
;;;  (2) the name of the output file containing the SHOP2 problem
;;;
;;; It mainly adds the the time lines for rovers domain.
;;; One for each rover, one for each camera and one for each storage and one for the lander.
;;;
;;; *** USAGE NOTE ***
;;; It may not be a good idea to have all characters capitalized in the
;;; output file.  If you want to avoid that, you should run this program
;;; in Allegro's mlisp rather than alisp.
;;;
;;;*** update ***
;;; This version support typed PDDL file and will change (object - typename) 
;;; to (typename object)
;;;
;;; This version also change function (= (property ?variable) ?value) 
;;; to (property ?variable ?value)

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
    (format outfile   (concatenate 'string "(defproblem "  problem-name " ~A ~%") domain-name)
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
                     (format outfile "    (~A ~A)~%" object-type s)
                     (cond ((eql object-type 'rover)
                            (format outfile "    (timestamp ~A 0)~%" s)
                            (format outfile "    (max ~A 0)~%" s))
                           ((or (eql object-type 'camera) (eql object-type 'store) (eql object-type 'lander))
                             (format outfile "    (timestamp ~A 0)~%" s)))
                     )
                (setf objects nil)
                (setf previous nil)))
     ))

    (format outfile "    ;;;~%")
    (format outfile "    ;;;  initial states~%")
    (format outfile "    ;;;~%")

    (let* (out-list
	     value)
      (dolist (s init-list)
	            (cond ((eql (first s) '=)
                    (setf out-list (second s))
                    (setf value (third s))
			       (nconc out-list (list value))
                         (format outfile "    ~A~%" out-list))
                        ((not (eql (first s) '=))
                         (format outfile "    ~A~%" s)))
    ))
    (format outfile "  )~%")
    (format outfile "  ;;;~%")
    (format outfile "  ;;; goals~%")
    (format outfile "  ;;;~%")
    (format outfile "(:ordered ~%") 
    (format outfile "  (:task !!set-connectivity-info  \"~A.lisp\" )  ~%" problem-name)
    (format outfile "  (:task achieve-goals~%")
    (format outfile "    ~A~%" (cdr goal-list))
    
    (format outfile " ) )~%")
    (format outfile ")~%")
      )
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

      ;;;
      ;;; Override default values
      ;;;
      ;; (setf domain-name  "domain-name")

      ;;;
      ;;; Genetate the SHOP2 problem
      ;;;
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
  "pfile20"))


