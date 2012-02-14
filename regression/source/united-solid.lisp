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

(in-package :gdl-lift-tests)


(define-object united-solid-test (united-solid)
  :computed-slots
  ((brep (the cylinder))
   (other-brep (list (the box) (the box2) (the box3)))
     
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :objects
  ((cylinder :type 'cylinder-solid
	     :radius 10 
	     :length 20)
     
   (box :type 'box-solid
	:length 10
	:height 10
	:width 30)
   
   (box2 :type 'box-solid
	 :length 5
	 :height 5
	 :width 20
	 :center (the box (face-center :right)))
     
   (box3 :type 'box-solid
	 :length 2
	 :height 2
	 :width 10
	 :center (the box2 (face-center :right)))))


(register-test-definition 'united-solid-test)



(define-object united-solid-bt-test (surf::boolean-tree)
  :computed-slots
  ((brep (the cylinder))
   (other-brep (the box))
   
   (breps (list (the brep) (the other-brep)))
   
   (operation :union)
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
    :objects
    ((cylinder :type 'cylinder-solid
	       :radius 10 
	       :length 20)
     
     (box :type 'box-solid
	  :length 10
	  :height 10
	  :width 30)
     

	   
     
     ))

(register-test-definition 'united-solid-test)


(defun gc-try (&key (times 1000))
  (dotimes (n times)
    (print-variables n)
    (make-self 'united-solid-test)
    (the volume)))


(defun gc-tries (&key (times 1000) (outer-times 1000))
  (dotimes (m outer-times)
    (print-variables  m)
    (gc-try :times times)
    (dolist (brep smlib::*breps-to-delete*) (smlib::delete-iwbrep brep))
    (setq smlib::*breps-to-delete* nil)
    (dolist (brep smlib::*booleans-to-delete*) (smlib::delete-iwbrep brep))
    (setq smlib::*booleans-to-delete* nil)
    (dolist (point smlib::*points3d-to-delete*) (smlib::delete-iwpoint3d point))
    (setq smlib::*points3d-to-delete* nil)))
