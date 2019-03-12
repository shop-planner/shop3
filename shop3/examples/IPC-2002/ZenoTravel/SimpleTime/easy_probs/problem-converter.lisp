(in-package :common-lisp-user)

(defun parse-object-list (object-list)
   (unless (null object-list)
       (cons (list (third object-list) (first object-list)) (parse-object-list (cdr (cdr (cdr object-list)))))))

(setq problems '( 
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

(defun all-convert()
    (dolist (prob problems)
      (p-convert prob 
                 (concatenate 'string "./" prob)
                 (concatenate 'string "./" prob ".lisp"))))
    
(defun p-convert (problem-name pddl-file shop-file)
  (progn
    
    (with-open-file (instr pddl-file :direction :input)
      (let ((pddl (read instr)))
        (with-open-file (outstr shop-file :direction :output
                         :if-exists :supersede)
           (format outstr "(in-package :shop-user)~%")
           (format outstr "(defproblem ")
           (format outstr (concatenate 'string "" problem-name))
           (format outstr " ZENOTRAVEL ~%")
        
;;; getting the objects
           (setq object-list (cdr (nth 3 pddl)))
;;; getting the initial state
           (setq init-state (append (parse-object-list object-list) (cdr (nth 4 pddl))))
;;; getting the goals 
           (setq goal-list  (mapcar #'(lambda (x) (cdr x)) (rest (first (cdr (nth 5 pddl))))))
;;; getting the metric
          
          (setq metric (third (nth 6 pddl)))
          (setq a-list nil)
          (setq p-list nil)
          (format outstr "( ~%") 
          
          (when (eq (first metric) '+)
              (setq exp (second metric))
              (cond ((eq (first exp) '+)
                      (format outstr "(TOTALTIME-COEFF ~s)~%" (second (second exp)))
                      (format outstr "(DRIVEN-COEFF ~s)~%" (second (third exp)))
                      (format outstr "(WALKED-COEFF ~s)~%" (second (third metric))))
                    (t 
                      (format outstr "(TOTALTIME-COEFF ~s)~%" (second exp))
                      (format outstr "(FUELUSED-COEFF ~s)~%" (second (third metric))))))
                     
                
               
           (format outstr "(MAXTIME 0)~%")
           (dolist (atom init-state)
             (cond ((eq (first atom) 'aircraft)
                             (format outstr "~s ~%" atom)
                             (format outstr "(TIMESTAMP ~s 0)~%" (second atom))
                             (format outstr "(PTIMESTAMP ~s 0)~%" (second atom))
                             (setq a-list (cons (second atom) a-list))
                    )
                   ((eq (first atom) 'person)
                             (format outstr "~s ~%" atom)
                             (format outstr (if (setq x (assoc (second atom) goal-list))
                                                "~s~%"
                                                "")
                             (if x (cons 'GOAL x)))
                             (setq p-list (cons (second atom) p-list))
                    )
		   ((and (eq (first atom) '=)
			 (or (eq (first (second atom)) 'fuel)
			     (eq (first (second atom)) 'distance)
			     (eq (first (second atom)) 'refuel-rate)
			     (eq (first (second atom)) 'slow-speed)
			     (eq (first (second atom)) 'fast-speed)
			     (eq (first (second atom)) 'slow-burn)
			     (eq (first (second atom)) 'fast-burn)
			     (eq (first (second atom)) 'capacity)
			     (eq (first (second atom)) 'boarding-time)
			     (eq (first (second atom)) 'debarking-time)
			     (eq (first (second atom)) 'total-fuel-used)
			     (eq (first (second atom)) 'onboard)
			     (eq (first (second atom)) 'zoom-limit)
			     ))
		      (format outstr "~s~%" (append (second atom) (list (third atom))))
		    )
                   (t 
                      (format outstr "~s ~%" atom)
		   ))) 
          (format outstr ") ~%")
          (format outstr "(:ordered ~%")
;             (format outstr "(:task !!preprocessing ~s) ~%" shop-file)
             (format outstr "(:unordered ~%")
             (dolist (p (reverse p-list))
               (if (setq tuple (assoc p goal-list))
                 (format outstr "(:task transport-person ~s ~s)~%" 
                                (first tuple) (second tuple))))
             (format outstr ") ~%")
             (format outstr "(:unordered ~%")
             (dolist (ar (reverse a-list))
               (if (setq tuple (assoc ar goal-list))
                 (format outstr "(:task transport-aircraft ~s ~s)~%" 
                                (first tuple) (second tuple))))
             (format outstr ") ~%")
          (format outstr ") ~%")                            
          (format outstr ") ~%")
      )))))

(all-convert)

