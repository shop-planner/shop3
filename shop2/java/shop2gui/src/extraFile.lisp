;(compile-file "shop2")

(shop-trace '(:tasks :methods))

; Notification for when end of input has been reached
(let ((old-find-plans #'find-plans))
  (defun find-plans (&rest args)
    (prog1
	(apply old-find-plans args)
        (query-java (tag 7)))))


(defun trace-query-hook (type item formats state)
   (format t "~%TR: ~s" (list type item formats state))
   (cond 
    ((eq type :tasks)
     (query-java (tag 0 type))
     (query-java (tag 1 item))
     (query-java (tag 2 (extract (first formats))))
     (setf k 1)
     (dolist (f (rest formats))
         (if (eq k 3)
           (breakdown f 4)
           (query-java (tag 3 f)))
         (setf k (1+ k)))   
     (breakdown state 5))
    ((eq type :methods)
     (when (eq (extract (first formats)) 'applying)
       (query-java (tag 8 item))
       (query-java (tag 9 (fifth formats)))))))

(defun breakdown (formats tagcode)
   (cond
	((null formats) nil)
        (t (query-java (tag tagcode (first formats))) (breakdown (rest formats) tagcode)))
   
)

(defun tag (tag &optional object)
   (format nil "~s~s" tag object))

(defun extract (string)
    (cond 
        ((search "trying" string) 'trying)
        ((search "reduced" string) 'reduced)
        ((search "backtracking" string) 'backtracking)
        ((search "applying" string) 'applying)
    )
)


(defun plan-found-hook (&rest args)
    (query-java (tag 6)))


