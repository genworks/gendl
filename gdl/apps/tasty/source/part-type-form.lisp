;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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
