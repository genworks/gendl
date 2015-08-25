;;
;; Copyright 2002-2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 


(in-package :gdl)

(#+allegro
 excl:without-redefinition-warnings
 #-allegro progn
 (#+allegro
  excl:without-package-locks
  #-allegro progn
  (defparameter *invalid-aggregate-behavior* :error)

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (export '*invalid-aggregate-behavior* :gdl))
      
  (defun add-notify-cons (notify-cons value &optional self message)
    ;;
    ;; FLAG -- this was added as a fix for github Issue #69, but causes
    ;; a regression by polluting the dependency graph with normal objects 
    ;; which may then become unbound spuriously. Retracted until a more
    ;; benign fix for Issue #69 can be determined. 
    ;;
    ;;(declare (ignore self message))
    ;;
    ;; FLAG -- we put back the extra tracking of notify-cons but only if
    ;; the message does not name a GDL object -- we don't want those
    ;; being the target of dependencies or they can be orphaned.
    ;;
    (when (and self message)
      (let ((aggregate  (gdl-acc::%aggregate% (first notify-cons))))
	(when (and (consp aggregate)
		   (not (consp (gdl-acc::%aggregate% self))))
	  (let ((agg (first aggregate)))
	    (if agg
		(let ((num-value (gdl-acc::number-of-elements agg)))
		  (when (and (consp num-value)
			     (let ((value (funcall message self)))
			       (not 
				(and (consp value) 
				     (typep (first value) 'gdl-basis)))))
		    (add-notify-cons (list self message) num-value)))

		(let ((error-message (format nil "
!!! 

 Invalid internal Gendl data structure detected. 

  notify-cons = ~s

  aggregate = ~a

  value = ~s

  self = ~a

  message = ~s


!!!
"
					     notify-cons aggregate value self message)))
		  (ecase *invalid-aggregate-behavior*
		    (:error (error error-message))
		    (:warn (warn error-message))
		    (nil ))))))))
    (let ((second (second value)))
      (if (and (listp second) (< (length second) *dep-hash-threshhold*))
	  (let ((matching-sublist (assoc (first notify-cons) (second value))))
	    (if matching-sublist (pushnew (second notify-cons) (rest matching-sublist))
		(push (copy-list notify-cons) (second value))))
	  (progn    
	    (when (listp second) (setf (second value) (alist-to-hash second)))
	    (pushnew (second notify-cons) (gethash (first notify-cons) (second value)))))))))
