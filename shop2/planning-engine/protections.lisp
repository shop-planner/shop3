;;; -*- Mode: common-lisp; package: shop2; -*-

;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    This file contains functions for manipulating protections.
;;;    These have been pulled out of the main SHOP code to make it
;;;    easier to modify them.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2008/02/13:rpg] Created.
;;;
;;;---------------------------------------------------------------------------



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

;;; Smart Information Flow Technologies Copyright 2008 Unpublished work

(in-package :shop2)

;;; if the state has everything that the protections list has, then
;;; return true, else return nil
(defun protection-ok (state protections head)
  (dolist (p protections)
    (unless (shopthpr:find-satisfiers (car p) state t)
      (trace-print
       :operators (first head) state
       "~%Backtracking because operator ~s~%  violated the protected condition ~s"
       (first head) (car p))
      (backtrack "Protection violation ~S" (car p))
      (return-from protection-ok nil)))
  t)

;;; increase the count for PROTECT in the protection list
(defun add-protection (protections protect depth operator state)
  (let (p)
    (dolist (d protections)
      (if (equal protect (car d))
        (return (setq p d))))
    (cond ((null p)
           (trace-print :protections (car protect) state
                        "~2%Depth ~s, incrementing protection count to ~s~%      atom ~s~%  operator ~s"
                        depth 1 protect operator)
           (setq protections (cons (list protect 1) protections)))
          (t
           (setq protections (remove p protections))
           (setq p (cons (car p) (list (+ 1 (car (cdr p))))))
           (trace-print :protections (car protect) state
                        "~2%Depth ~s, incrementing protection count to ~s~%      atom ~s~%  operator ~s"
                        depth (cadr p) protect operator)
           (setq protections (cons p protections))))))

;;; decrease the count for PROTECT in the protection list
(defun delete-protection (protections protect depth operator state)
  (let (p)
    (dolist (d protections)
      (if (equal protect (car d))
        (return (setq p d))))
    (if (null p)
      (trace-print :protections (car protect) state
                   "~2%Depth ~s, protection count not decremented since it is already 0~%      atom ~s~%  operator ~s"
                   depth protect operator)
    (progn
      (setq protections (remove p protections))
      (if (eql 1 (car (cdr p)))
        protections
        (progn
          (setq p (cons (car p) (list (- (car (cdr p)) 1))))
          (trace-print :protections (car protect) state
                        "~2%Depth ~s, decrementing protection count to ~s~%      atom ~s~%  operator ~s"
                        depth (cadr p) protect operator)
          (setq protections (cons p protections))))))))
