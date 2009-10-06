;;; -*- Mode: common-lisp; package: common-lisp-user; -*-
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
(in-package :common-lisp-user)

(defpackage :shop2.unifier
    (:nicknames :shopunif)
    (:use :common-lisp)
    (:export #:trace-print
             #:*shop-trace* #:*shop-trace-stream* #:*trace-query*

             ;; the unifier interface
             #:unify-fail #:unify-p
             #:unify #:standardize #:fail
             #:apply-substitution #:compose-substitutions
             #:fix-uninterned-bindings #:binding-list-value
             #:make-binding-list #:make-binding
             #:binding-var #:binding-val
             #:variablep #:groundp
             ;; this is defined as a type, so you can use typep with it. [2009/10/06:rpg]
             #:shop-variable
             #:set-variable-property
             #:variable-gensym

             ;; [mpelican:20090520.2111CDT] This is a bit of a hack, but set-variable-property
             ;; also sets primitive property which is only of interest to planner.
             ;; primitivep which checks the primitive property lives in the shop2 package.
             ;; So, we export this symbol for the planner's benefit.
             #:primitive

             ;; utilities used internally by shop
             #:shop-union
             #:get-alist

             ;; hook for tracing -- later exported *through* the shop2 package
             #:trace-query-hook

             ))