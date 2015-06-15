(in-package :shop2-user)

(fiveam:def-suite misc-tests)

(fiveam:in-suite misc-tests)

(fiveam:test check-stack-overflow-fix
  (let ((prob-name (gentemp "PROB")))
    (fiveam:is
     (equal
      (catch 'bad-problem-arg 
       (handler-bind
           ((error
              #'(lambda (c)
                  (throw 'bad-problem-arg
                    (with-output-to-string (x)
                      (format x "~a" c))))))
         (find-plans prob-name)))
      (format nil "No such problem: ~a" prob-name)))))
              
             
      
