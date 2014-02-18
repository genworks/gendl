;;
;; Copyright 2014 Genworks International and Genworks BV 
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

(in-package :tasty)

(define-object part-type-form (base-html-sheet)
  :input-slots
  ((object-type "robot:assembly" :settable)
   make-instance-function set-root-function tatu-root package-default)
  
  :computed-slots
  (
   (object-expression "" :settable)
   (respondent (the tatu-root)))
  
  :functions
  ((after-set!
    ()
    (cond ((not (string-equal (the object-type) ""))
	   (funcall (the make-instance-function) (the object-type))
	   (the root-object-object update!))

	  ((not (string-equal (the object-expression) ""))
	   (funcall (the set-root-function) 
		    (let ((*package* (find-package (the package-default))))
		      (eval (read-safe-string (the object-expression))))))))))


(define-lens (html-format part-type-form)()
  :output-functions
  ((main-sheet     
    () 
    (with-cl-who ()
      (with-html-form ()
	(:table (:tr ((:td :colspan 2 :align :center)
		      (:h3 "Specify object package and type, or an expression which returns an object.")
		      (:i "Default Package: the " 
			  (:princ-safe (package-name (find-package (the package-default)))) " Package")))
	  (:tr ((:td :bgcolor (gethash :gold-bright *color-table*)) (:b "Class Package:Type"))
	       (:td ((:input :type :string :name :object-type :size 40 :value (the object-type)))))
	  (:tr ((:td :bgcolor (gethash :gold-bright *color-table*)) (:b "Object Expression"))
	       (:td ((:input :type :string :name :object-expression :size 40 :value 
			     (the object-expression)))))
	  (:tr (:td :br) (:td ((:input :type :submit :name :submit :value " Browse!"))))))))))


(defparameter *test-self* nil)

(define-object required-inputs-form (base-html-sheet)
  :input-slots
  (root-object tatu-root)
  
  :computed-slots
  ((respondent (the tatu-root))
   
   (required-input-slots (the root-object (message-list :category :required-input-slots)))

   (satisfied-toggle nil :settable)

   (satisfied? (progn (the satisfied-toggle)
		      (let ((result t))
			(dolist (slot (the required-input-slots))
			  (let ((status (the root-object (slot-status slot))))
			    (when (eql status :unbound) (setq result nil))))
		 
			(print-variables result)
		 
			result))))
  
  :objects
  ((required-input-inputs  :type 'text-form-control
			   :sequence (:size (length (the required-input-slots)))
			   :pseudo-inputs (keyword)
			   :size 20
			   :domain :pass-thru
			   :keyword (nth (the-child index)(the required-input-slots))
			   :default :unbound
			   :prompt (the-child keyword)))

  :functions 
  (
   (after-set!
    ()

    (setq *test-self* (the root-object))

    (format t "In the after-set! of the required-inputs-form...~%")

    (dolist (slot (list-elements (the required-input-inputs)))
      (unless (eql (the-object slot value) :unbound)
	(the root-object (set-slot! (the-object slot keyword)
				    (the-object slot value)))))
    
    (the (set-slot! :satisfied-toggle (not (the satisfied-toggle)))))))


(define-lens (html-format required-inputs-form)()
  :output-functions
  ((main-sheet     
    () 
    (with-cl-who ()
      (with-html-form (:cl-who? t)
	(dolist (slot (list-elements (the required-input-inputs)))
	  (htm (:p (str (the-object slot html-string)))))
	(:p ((:input :type :submit :name :submit :value " Browse!"))))))))


