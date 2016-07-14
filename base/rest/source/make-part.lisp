;;
;; Copyright 2002-2011 Genworks International 
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

(defparameter *warn-on-invalid-toplevel-inputs?* t)

(defun make-part (&rest args)
  (apply #'make-object args))


;;
;; FLAG -- cache the computation of input-slots based on type, and
;; look up input-slots on subsequent calls. Then reactivate this
;; make-object.
;;

#+nil
(defun make-object (object-type &rest args)
  "GDL Object. Instantiates an object with specified initial values for input-slots.

:arguments (object-name \"Symbol. Should name a GDL object type.\"
            arguments \"spliced-in plist. A plist of keyword symbols and values for initial <tt>input-slots</tt>.\")"
  (let (*notify-cons*
        (keys (plist-keys args))
        (vals (plist-values args)))
    (let ((object (apply #'make-instance 
                         object-type 
                         :allow-other-keys t
                         (list :%name% (list (format nil "~a" object-type) nil t)
			       :%parent% (list nil nil t)))))
      (let ((input-slots 
	     (append (the-object object (message-list :category :required-input-slots))
		     (the-object object (message-list :category :optional-input-slots))
		     (the-object object (message-list :category :settable-optional-input-slots))
		     (the-object object (message-list :category :defaulted-input-slots))
		     (the-object object (message-list :category :settable-defaulted-input-slots)))))
	(mapc #'(lambda(key val)
		  (if (member key input-slots)
		      (setf (slot-value object (glisp:intern key :gdl-acc))
			    (list val nil t))
		      (when *warn-on-invalid-toplevel-inputs?*
			(warn "~&~%~s is not a defined input-slot for ~s. 
The argument ~s, and its value ~s, have been ignored
 [you may (setq *warn-on-invalid-toplevel-inputs?* nil) to suppress this warning).~%~%"
			      key object-type key val)))) keys vals))
      
      (setf (gdl-acc::%root% object) object
            (gdl-acc::%toplevel-inputs% object) args) object)))




(defun make-object (object-type &rest args)
  "GDL Object. Instantiates an object with specified initial values for input-slots.

:arguments (object-name \"Symbol. Should name a GDL object type.\"
            arguments \"spliced-in plist. A plist of keyword symbols and values for initial <tt>input-slots</tt>.\")"
  (let (*notify-cons*
        (keys (plist-keys args))
        (vals (plist-values args)))
    (let ((object (apply #'make-instance 
                         object-type 
                         :allow-other-keys t
                         (append (list :%name% (list (format nil "~a" object-type) nil t)
                                       :%parent% (list nil nil t))
                                 (mapcan #'(lambda(key val)
                                             (list key (list val nil t)))
                                         keys vals)))))
      
      (setf (gdl-acc::%root% object) object
            (gdl-acc::%toplevel-inputs% object) args)

      (the-object object initialize-instance!) object)))




;;
;; FLAG  -- remove this defunct version. 
;;
#+nil
(defun make-object (object-type &rest args)
  "GDL Object. Instantiates an object with specified initial values for input-slots.

:arguments (object-name \"Symbol. Should name a GDL object type.\"
            arguments \"spliced-in plist. A plist of keyword symbols and values for initial <tt>input-slots</tt>.\")"
  (let (*notify-cons*
        (keys (plist-keys args))
        (vals (plist-values args)))
    (let ((object (apply #'make-instance 
                         object-type 
                         :allow-other-keys t
                         (append (list :%name% (list (format nil "~a" object-type) nil t)
                                       :%parent% (list nil nil t))
                                 (mapcan #'(lambda(key val)
                                             (list key (list val nil t)))
                                         keys vals)))))
      
      (setf (gdl-acc::%root% object) object
            (gdl-acc::%toplevel-inputs% object) args) object)))


(defun make-object-internal (object-type &rest args)
  (let ((object (apply #'make-instance object-type :allow-other-keys t args)))
    (the-object object initialize-instance!) object))

#+nil
(defun make-object-internal (object-type &rest args)
  (apply #'make-instance object-type :allow-other-keys t args))


(defun make-canonical-part (&rest args)
  (apply #'make-object args))

(defun make-self (object-type &rest args)
  (setf (symbol-value 'self) (apply #'make-object object-type args)))
 


