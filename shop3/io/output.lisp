;;;
;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;;
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;;
;;; The Original Code is SHOP2.
;;;
;;; The Initial Developer of the Original Code is the University of
;;; Maryland. Portions created by the Initial Developer are Copyright (C)
;;; 2002,2003 the Initial Developer. All Rights Reserved.
;;;
;;; Additional developments made by Robert P. Goldman, John Maraist.
;;; Portions created by Drs. Goldman and Maraist are Copyright (C)
;;; 2004-2007 SIFT, LLC.  These additions and modifications are also
;;; available under the MPL/GPL/LGPL licensing terms.
;;;
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.
;;; ----------------------------------------------------------------------

;;; Smart Information Flow Technologies Copyright 2006-2007 Unpublished work
;;;
;;; GOVERNMENT PURPOSE RIGHTS
;;;
;;; Contract No.         FA8650-06-C-7606,
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;;
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.


(in-package :shop2)
;;; ------------------------------------------------------------------------
;;; Functions to create some of the output
;;; ------------------------------------------------------------------------

;;; determine-verbosity uses the current value of *verbose* to set
;;; global flags that tell SHOP2 what information to print out
(defun determine-verbosity (verbose)
  (ecase verbose
      ((0 nil)         (setq *print-stats* nil *print-plans* nil))
      ((1 :stats)      (setq *print-stats* t *print-plans* nil))
      ((2 :plans)      (setq *print-stats* t *print-plans* t *pshort* t))
      ((3 :long-plans) (setq *print-stats* t *print-plans* t *pshort* nil))))

;;; debugging output, indented by INDENTATION number of spaces
(defun indented-format (indentation &rest body)
  (format nil "~%~A" (make-string indentation :initial-element #\space))
  (apply #'format (cons nil body)))

(defun print-stats-header (label &optional (stream t))
  (when *print-stats*
    (format stream
            "~%~7@a Plans Mincost Maxcost Expansions Inferences  CPU time  Real time"
            label)))

(defun print-stats (depth plans tasks inferences runtime realtime
                    &optional (stream t))
  (when *print-stats*
    (format stream "~%~6@a~6@a ~7@a ~7@a~11@s~11@s~10,3f~11,3f~%"
            depth (length plans)
            (if plans (to-string (apply #'min (mapcar #'plan-cost plans)) 2) "-")
            (if plans (to-string (apply #'max (mapcar #'plan-cost plans)) 2) "-")
            tasks inferences
            (/ runtime internal-time-units-per-second)
            (/ realtime internal-time-units-per-second))))

; Converts a number to a string.
; Examples:
;   (to-string 3 2) => "3"
;   (to-string 3.0 2) => "3.0"
;   (to-string 3.8 2) => "3.8"
;   (to-string 3.775 2) => "3.78"
; This is sort of like the ~F format string, but it doesn't add extra 0's
;  at the end.
(defun to-string (num &optional (max-decimal 1))
  (if (integerp num) (format nil "~a" num)
    (let* ((base-string (format nil (format nil "~~,~aF" max-decimal) num))
           (trimmed-string (string-right-trim "0" base-string)))
      (if (eql #\. (char trimmed-string (1- (length trimmed-string))))
          (concatenate 'string trimmed-string "0")
        trimmed-string))))

(defmethod plan-cost ((plan list))
  (if (null plan)
      0
      (+ (second plan) (plan-cost (cddr plan)))))

(defun print-output-file (plan)
  (dolist (obj plan)
    (format t ", ")
    (print1 obj)))

(defun print1 (obj)
  (if (atom obj) (format t " ~A" obj)
      (progn
        (format t "(~S" (car obj))
        (dolist (obj1 (cdr obj))
          (print1 obj1))
        (format t ")"))))

;;;(defun get-alist (variables)
;;;  (let (alist vlist)
;;;    (setq vlist (mapcar #'(lambda (x) (declare (ignore x))
;;;                            (variable-gensym))
;;;                            variables))
;;;    (setq alist (pairlis variables vlist))
;;;    (values alist vlist)))

;;; Plan accessible debugging information.

;;; The following 4 routines, which were written by Chiu, are not
;;; invoked by any other SHOP routines.  Instead, they are intended to
;;; be invoked in call or eval forms within a SHOP domain; they are
;;; intended to assist with debugging a domain.

(defun query-current-state (first-symbol)
  (state-all-atoms-for-predicate *current-state* first-symbol))

(defun print-current-state (&key sorted (state *current-state*))
  (format t "~%~{~t~A~%~}~%" (if sorted (sort (copy-seq  (state-atoms state)) 'prop-sorter) (state-atoms state)))
  t)

(defun print-current-tasks ()
  (format t "~%~A~%" *current-tasks*)
  t)

(defun print-current-plan ()
  (let ((tplan *current-plan*)
        (fplan nil)
        (i 1))
    (do* ((cc (pop tplan) (pop tplan))
          (tt (pop tplan) (pop tplan)))
         ((not cc))
      (when (not (internal-operator-p (car tt)))
        (push tt fplan)))
    (dolist (s fplan)
      (format t "~A: ~A~%" i s)
      (setf i (1+ i)))
    t))

;;;---------------------------------------------------------------------------
;;; Debug function
;;;---------------------------------------------------------------------------
(defun backtrack (format-string &rest args)
  (when *break-on-backtrack*
    (apply #'break format-string args)))
