(in-package :shop-user)

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

;;; better alternative than check-method...
(defun test-process-method (sexp)
  (shop2::set-variable-property (make-instance 'domain) sexp)
  (with-singleton-warnings 
    (shop2::process-method (make-instance 'domain) sexp)))

(defun test-process-operator (sexp)
  (shop2::set-variable-property (make-instance 'domain) sexp)
  (with-singleton-warnings 
    (shop2::process-operator (make-instance 'domain) sexp)))

(defun test-process-axiom (sexp)
  (let ((d (make-instance 'domain)))
    (shop2::set-variable-property d sexp)
    (with-singleton-warnings 
      (shop2::process-axiom d sexp))))

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
                     (test-process-method 
                      '(:method (assess-fire ?s ?x)
                        assess-fire
                        (;; preconditions
                         (extinguished-fire ?x)
                         (at ?x (pos ?fx ?fy ?fa))
                         (short-sensor ?s)
                         (altitude-band ?s ?abs))
                        ;; task list
                        (:ordered (!fly-to ?s (pos ?fx ?fy ?abs))
                         (!assess ?s ?x)))))))
    (fiveam:is-true warning)
    (when warning
      (fiveam:is-true (typep warning 'singleton-variable))
      (when  (typep warning 'singleton-variable)
        (fiveam:is (eq (shop2::construct-type warning)
                       :method))
        (fiveam:is (eq (shop2::construct-name warning)
                       'assess-fire))
        (fiveam:is (equal (shop2::variable-names warning)
                          '(?fa))))))
    (let ((warning
          (nth-value 1
                     (test-process-method 
                      '(:method (assess-fire ?s ?x)
                        assess-fire
                        (;; preconditions
                         (extinguished-fire ?x)
                         (at ?x (pos ?fx ?fy ?_fa))
                         (short-sensor ?s)
                         (altitude-band ?s ?abs))
                        ;; task list
                        (:ordered (!fly-to ?s (pos ?fx ?fy ?abs))
                         (!assess ?s ?x)))))))
      (fiveam:is-false warning))
    (let ((warning
          (nth-value 1
                     (test-process-method 
                      '(:method (assess-fire ?s ?x ?context)
                        assess-fire
                        (;; preconditions
                         (extinguished-fire ?x)
                         (at ?x (pos ?fx ?fy ?_fa))
                         (short-sensor ?s)
                         (altitude-band ?s ?abs))
                        ;; task list
                        (:ordered (!fly-to ?s (pos ?fx ?fy ?abs))
                         (!assess ?s ?x)))))))
      (fiveam:is-true warning)
      (when warning
        (fiveam:is-true (typep warning 'singleton-variable))
        (when  (typep warning 'singleton-variable)
          (fiveam:is (eq (shop2::construct-type warning)
                         :method))
          (fiveam:is (eq (shop2::construct-name warning)
                         'assess-fire))
          (fiveam:is (equal (shop2::variable-names warning)
                            '(?context))))))
)




(fiveam:test clean-in-context
  (let ((warning
          (nth-value 1 
                     (test-process-method
                      '(:method (assert-facts ?facts)
                        done
                        ((= ?facts nil))
                        ()
                        recurse
                        ((= ?facts (?fact . ?rest)))
                        (:ordered (!!assert ?fact)
                         (:immediate assert-facts ?rest)))))))
    (fiveam:is (null warning))))


(fiveam:test anonymous-variables-distinct-head
  (let ((method 
          (test-process-method
           '(:method (assert-facts ?facts ?_ ?_)
             done
             ((= ?facts nil))
             ()
             recurse
             ((= ?facts (?fact . ?rest)))
             (:ordered (!!assert ?fact)
              (:immediate assert-facts ?rest))))))
    (let ((method-task (second method)))
      (fiveam:is-false (eq (third method-task) (fourth method-task))))))

(fiveam:test anonymous-variables-distinct-body
  (let* ((raw-method
           '(:method (assert-facts ?facts ?_ ?_)
             done
             ((= ?facts nil)
              (check-pred ?_ ?_))
             ()
             recurse
             ((= ?facts (?fact . ?rest)))
             (:ordered (!!assert ?fact)
              (:immediate assert-facts ?rest))))
         (method 
          (test-process-method raw-method)))
    (let* ((done-precs (fourth method))
           (check-pred (second done-precs)))
      (fiveam:is-false (eq (second check-pred) (third check-pred))))
    (let* ((done-precs (fourth raw-method))
           (check-pred (second done-precs)))
      (fiveam:is-true (eq (second check-pred) (third check-pred))))))

(fiveam:test anonymous-variables-op
  (let ((raw-op '(:operator (!assign-tanker-to-fire ?_t ?_x)
                  ((available-tanker ?_t))
                  ;; delete
                  ((available ?_t))
                  ;; add
                  ((assigned-tanker ?_t (fire ?_x)))
                  ;; cost
                  0)))
    (multiple-value-bind (op warning)
        (test-process-operator raw-op)
      (fiveam:is-false warning)
      (let ((op-head (second raw-op))
            (op-prec (third raw-op))
            (op-del (fourth raw-op))
            (op-add (fifth raw-op)))
        ;; here are all the ?_t's 
        (fiveam:is (eq (second op-head) (second (first op-prec))))
        (fiveam:is (eq (second op-head) (second (first op-del))))
        (fiveam:is (eq (second op-head) (second (first op-add)))))
      (let ((op-head (second op))
            (op-prec (third op))
            (op-del (fourth op))
            (op-add (fifth op)))
        ;; here are all the ?_t's 
        (fiveam:is-false (eq (second op-head) (second (first op-prec))))
        (fiveam:is-false (eq (second op-head) (second (first op-del))))
        (fiveam:is-false (eq (second op-head) (second (first op-add))))))))

;;; FIXME: check for variables only appearing in the method's head...

(fiveam:test anonymous-variables-axiom
  (let ((warning
          (nth-value 1
                     (test-process-axiom
                      '(:- (foo ?x)
                        (()))))))
    (fiveam:is-true warning)
    (fiveam:is (eq (shop2::construct-type warning) ':-))
    (fiveam:is (eq (shop2::construct-name warning) 'foo))
    (fiveam:is (equal (shop2::variable-names warning) (list '?x))))
  (let ((warning
          (nth-value 1
                     (test-process-axiom
                      '(:- (foo)
                        ((bar ?x)))))))
    (fiveam:is-true warning)
    (fiveam:is (eq (shop2::construct-type warning) ':-))
    (fiveam:is (eq (shop2::construct-name warning) 'foo))
    (fiveam:is (equal (shop2::variable-names warning) (list '?x))))
  (let ((warning
          (nth-value 1
                     (test-process-axiom
                      '(:- (foo ?x)
                        ((bar ?x)))))))
    (fiveam:is-false warning))
  (let ((warning
          (nth-value 1
                     (test-process-axiom
                      '(:- (foo ?_)
                        (()))))))
    (fiveam:is-false warning))
  (let ((warning
          (nth-value 1
                     (test-process-axiom
                      '(:- (foo)
                        ((bar ?_)))))))
    (fiveam:is-false warning))
  (let ((warning
          (nth-value 1
                     (test-process-axiom
                      '(:- (foo ?x)
                        ((bar ?x)))))))
    (fiveam:is-false warning)))


    

