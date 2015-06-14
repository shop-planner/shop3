(in-package :shop2-user)

(fiveam:def-suite singleton-tests)
(fiveam:in-suite singleton-tests)

;;; this could probably be done more smoothly using HANDLER-CASE
(defmacro with-singleton-warnings (&body form)
  (let ((warnvar (gentemp "WARN"))
        (retval (gentemp "RET")))
    (assert (= (length form) 1))
    `(let (,warnvar ,retval)
       (handler-bind
           ((singleton-variable
              #'(lambda (w)
                  (setf ,warnvar w)
                  (muffle-warning w))))
         (setf ,retval ,@form)
         (values ,retval ,warnvar)))))
                  
(defun check-operator (sexp name)
  (shop2::set-variable-property (make-instance 'domain) sexp)
  (let ((table (shop2::harvest-variables sexp)))
    (with-singleton-warnings 
      (shop2::check-for-singletons table :construct-type :operator :construct-name name))))

(defun check-method (sexp name)
  (shop2::set-variable-property (make-instance 'domain) sexp)
  (let ((table (shop2::harvest-variables sexp)))
    (with-singleton-warnings 
      (shop2::check-for-singletons table :construct-type :method :construct-name name))))


(fiveam:test blort-singleton
  (let ((warning 
          (nth-value 1
             (check-operator '(:operator (!assign-tanker-to-fire ?t ?x ?blort)
                                       ((available-tanker ?t))
                                       ;; delete
                                       ((available ?t))
                                       ;; add
                                       ((assigned-tanker ?t (fire ?x)))
                                       ;; cost
                                       0
                                       )
                             '!assign-tanker-to-fire))))
    (fiveam:is (eq (shop2::construct-type warning) :operator))
    (fiveam:is (eq (shop2::construct-name warning) '!assign-tanker-to-fire))
    (fiveam:is (equal (shop2::variable-names warning)
                      (list '?blort)))))

(fiveam:test anon-blort-singleton
  (let ((warning 
          (nth-value 1
             (check-operator '(:operator (!assign-tanker-to-fire ?t ?x ?_blort)
                                       ((available-tanker ?t))
                                       ;; delete
                                       ((available ?t))
                                       ;; add
                                       ((assigned-tanker ?t (fire ?x)))
                                       ;; cost
                                       0
                                       )
                             '!assign-tanker-to-fire))))
    (fiveam:is (null warning))))

(fiveam:test clean-operator
  (let ((warning 
          (nth-value 1
             (check-operator '(:operator (!assign-tanker-to-fire ?t ?x)
                                       ((available-tanker ?t))
                                       ;; delete
                                       ((available ?t))
                                       ;; add
                                       ((assigned-tanker ?t (fire ?x)))
                                       ;; cost
                                       0
                                       )
                             '!assign-tanker-to-fire))))
    (fiveam:is (null warning))))

(fiveam:test clean-method
   (let ((warning 
           (nth-value 1
                      (check-method
                       '(:method (assert-facts ?facts)
                         done
                         ((= ?facts nil))
                         ()
                         recurse
                         ((= ?facts (?fact . ?rest)))
                         (:ordered (!!assert ?fact)
                          (:immediate assert-facts ?rest)))
                       'assert-facts))))
     (fiveam:is (null warning))))


(fiveam:test method-with-singleton
   (let ((warning 
           (nth-value 1
                      (check-method
                       '(:method (assess-fire ?s ?x)
                         assess-fire
                         (;; preconditions
                          (extinguished-fire ?x)
                          (at ?x (pos ?fx ?fy ?fa))
                          (short-sensor ?s)
                          (altitude-band ?s ?abs))
                         ;; task list
                         (:ordered (!fly-to ?s (pos ?fx ?fy ?abs))
                          (!assess ?s ?x)))
                       'assess-fire))))
     (fiveam:is-true (typep warning 'singleton-variable))
     (fiveam:is (eq (shop2::construct-type warning)
                    :method))
     (fiveam:is (eq (shop2::construct-name warning)
                    'assess-fire))
     (fiveam:is (equal (shop2::variable-names warning)
                       '(?fa)))))

(fiveam:test mws-in-context
  (let ((warning
          (nth-value 1
                     (with-singleton-warnings 
                       (shop2::process-method (make-instance 'domain)
                                      '(:method (assess-fire ?s ?x)
                                        assess-fire
                                        (;; preconditions
                                         (extinguished-fire ?x)
                                         (at ?x (pos ?fx ?fy ?fa))
                                         (short-sensor ?s)
                                         (altitude-band ?s ?abs))
                                        ;; task list
                                        (:ordered (!fly-to ?s (pos ?fx ?fy ?abs))
                                         (!assess ?s ?x))))))))
    (fiveam:is-true warning)
    (when warning
      (fiveam:is-true (typep warning 'singleton-variable))
      (when  (typep warning 'singleton-variable)
        (fiveam:is (eq (shop2::construct-type warning)
                       :method))
        (fiveam:is (eq (shop2::construct-name warning)
                       'assess-fire))
        (fiveam:is (equal (shop2::variable-names warning)
                          '(?fa)))))))


(fiveam:test clean-in-context
  (let ((warning
          (nth-value 1 
                     (with-singleton-warnings 
                       (shop2::process-method (make-instance 'domain)
                               '(:method (assert-facts ?facts)
                                 done
                                 ((= ?facts nil))
                                 ()
                                 recurse
                                 ((= ?facts (?fact . ?rest)))
                                 (:ordered (!!assert ?fact)
                                  (:immediate assert-facts ?rest))))))))
    (fiveam:is (null warning))))
