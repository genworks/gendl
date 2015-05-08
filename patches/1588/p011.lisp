
;;
;; Copyright 2002-2015 Genworks International 
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

(defparameter *1588p011-doc* 
" Remove debug statements for x3d outpu9t")


(#+allegro 
 excl:without-redefinition-warnings
 #-allegro progn

 (define-lens (x3d base-object) ()

   :amend? t
   
   :output-functions
   ((material-properties
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (color (getf display-controls :color))
           (color  (or color (getf (the display-controls) :color) :black)))


      (cl-who:with-html-output (*stream* nil :indent nil)
	(:|Material| :|diffuseColor| (write-the (rgb-color color))
	  :|ambientIntensity| (getf (the display-controls) :ambient-intensity)
	  :|emissiveColor| (write-the (rgb-color (getf (the display-controls) :emissive-color)))
	  :|shininess| (getf (the display-controls) :shininess)
	  :|specularColor| (write-the (rgb-color (getf (the display-controls) :specular-color)))
	  :|transparency| (getf (the display-controls) :transparency)
	  )))))))
