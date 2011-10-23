;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(in-package :geom-base)

(define-object sample-drawing (base-drawing)
  
  :documentation (:description "Defines a simple drawing with a single view for displaying objects or object-roots.")
    
  :input-slots
  ((objects nil) (object-roots nil) (projection-direction :top) (page-width 360) (page-length 360))
  
  :objects
  ((main-view :type 'base-view 
              :projection-vector (if (keywordp (the projection-direction))
                                     (getf *standard-views* (the projection-direction))
                                   (the projection-direction))
              :objects (ensure-list (the objects))
              :object-roots (ensure-list (the object-roots)))))



(defun generate-sample-drawing (&key objects object-roots 
                                     (format :pdf)
                                     (output-file  
                                      (merge-pathnames "example.pdf" (glisp:temporary-folder)))
                                     projection-direction
                                     page-length page-width)
  
  (let ((init-args (append (when objects (list :objects objects))
                           (when object-roots (list :object-roots object-roots))
                           (when projection-direction (list :projection-direction projection-direction))
                           (when page-length (list :page-length page-length))
                           (when page-width (list :page-width page-width)))))
    (let ((self (apply #'make-object 'sample-drawing init-args)))
      (ecase format 
        (:pdf
         (with-format (pdf output-file :page-length (the page-length) 
                           :page-width (the page-width)) 
           (write-the cad-output)))
        (:png 
         (with-format (png output-file :page-length (the page-length) 
                           :page-width (the page-width)) 
           (write-the cad-output)))))))
  
