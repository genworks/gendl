;;
;; Copyright 2015 Genworks International 
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

(defparameter *1588p015-doc* 
  "Adds overridable initialize-instance! function to vanilla-mixin and calls this from make-object and make-object-internal.")

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


(defun make-object-internal (object-type &rest args)
  (let ((object (apply #'make-instance object-type :allow-other-keys t args)))
    (the-object object initialize-instance!) object))


(define-object-amendment vanilla-mixin ()
  :functions ((initialize-instance! ())))


(in-package :gdl-user)


(define-object make-object-regression& ()

  :functions
  ((initialize-instance! ()
			 (format *standard-output* "Hey Now to *standard-out*.~%")
			 (format t "Hey Now to t.~%"))))


(define-object make-object-regression ()
  :computed-slots ((regression-test-data (with-output-to-string (*standard-output*)
					   (make-object 'make-object-regression&)))


		   (seed-data "Hey Now to *standard-out*.
Hey Now to t.
" )

		   (pass? (equalp (the seed-data) (the regression-test-data)))))


(defun make-object-test () (the-object (make-object 'make-object-regression) pass?))


		   





