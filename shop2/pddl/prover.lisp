;;; -*- Mode: common-lisp; package: shop2; -*-
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
;;; Additional developments made by Robert P. Goldman, and Ugur Kuter.
;;; Portions created by Drs. Goldman and Kuter are Copyright (C)
;;; 2017 SIFT, LLC.  These additions and modifications are also
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
;;; FIXME: this GPR statement is wrong.
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

(in-package :shop2.theorem-prover)

(def-logical-keyword (forall (domain shop2::universal-precondition-mixin))
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (pddl-satisfiers-for-forall domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))

(def-logical-keyword (exists (domain shop2::existential-precondition-mixin))
  (:satisfier-method (goal other-goals state bindings level just1 dependencies-in)
    (pddl-satisfiers-for-exists domain (cdr goal) other-goals
                                    state bindings (1+ level) just1 dependencies-in)))


(defun pddl-satisfiers-for-forall (domain arguments other-goals
                                   state bindings newlevel just1 dependencies-in)
  ;; in our PDDL FORALLs, the bounds are all going to be static type
  ;; predicates, which don't need their dependencies recorded.
  (let* ((bounds (second arguments))
         (conditions (third arguments))
         (mgu2 (find-satisfiers bounds state nil 0 :domain domain)))
    (dolist (m2 mgu2)
      (unless (seek-satisfiers (apply-substitution conditions m2)
                               state bindings 0 t :domain domain)
        (return-from pddl-satisfiers-for-forall nil))))
  (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies-in))

(defun pddl-satisfiers-for-exists (domain arguments other-goals
                                       state bindings newlevel just1 dependencies-in)
   (let* ((bounds (second arguments))
         (conditions (third arguments))
         (mgu2 (find-satisfiers bounds state nil 0 :domain domain)))
    (loop for m2 in mgu2
        when (seek-satisfiers (apply-substitution conditions m2)
                              state bindings 0 t :domain domain)
          return t                      ; short-circuit, and move on
                                        ; to remaining
                                        ; goals... [2007/07/15:rpg]
        finally ;; didn't find any value that worked
          (return-from pddl-satisfiers-for-exists nil)))

  ;; Satisfy other goals
  (seek-satisfiers other-goals state bindings newlevel just1 :domain domain :dependencies dependencies-in))


