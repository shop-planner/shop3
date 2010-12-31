; Code for connecting Java and Lisp.

(defvar *infile-path* (pathname "tolisp.txt"))
(defvar *outfile-path* (pathname "tojava.txt"))

; Number of seconds to wait when there is no input or partial input.
;  This is zero by default but you may want to set it higher if performing
;  many file accesses cause problems on your system.
(defvar *no-input-wait* 0) ; or maybe .5
(defvar *partial-input-wait* 0) ; or maybe .1

; A read-eval-print loop that continuously checks a file for new commands.
; At each loop iteration, the input file may be in one of four acceptable
; states:
;
;  1) It holds a Lisp expression with no newlines, followed by a newline.
;     This means that the Java module has asked for this expression to
;     be evaluated and the Lisp module has not yet done so.  In this case,
;     this routine first overwrites the input file with a single newline.
;     Then it evaluates the expression and writes the value to the output
;     file with no newlines, followed by a newline.
;  2) The first character is a newline.  In this case, the routine delays
;     for an interval of *no-input-wait* seconds.
;  3) It does not exist.   Again, the routine delays for *no-input-wait*
;     seconds.
;  4) The file contains no newline.  This means that the Java module
;     has started to write an expression but has not yet finished.
;     In this case, the routine delays for *partial-input-wait* seconds.
;
; The routine also displays status information to the standard output.
(defun file-rep ()
  (loop
   (let ((infile (open *infile-path* :if-does-not-exist nil)))
     (if (not infile) (sleep *no-input-wait*)
       (multiple-value-bind
	(str eof)
	(read-line infile nil nil)
	(close infile)
	(cond
	 (eof
	  (sleep *partial-input-wait*))
	 ((equal str "")
	  (sleep *no-input-wait*))
	 (t
	  (let ((old-infile (open *infile-path*
				  :direction :output
				  :if-exists :supersede))
		(expr (read-from-string str))
		(*print-readably* nil))
	    (format t "[exp] ~s~%" expr)
	    (terpri old-infile)
	    (close old-infile)
	    (let* ((val (eval expr))
		   (outfile (open *outfile-path* :direction :output
				  :if-exists :overwrite
				  :if-does-not-exist :create)))
	      (format t "[val] ~s~%" val)
	      (prin1 val outfile)
	      (terpri outfile)
	      (close outfile))))))))))

(file-rep)