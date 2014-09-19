;;
;; Copyright 2012 Genworks International
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

(define-format scad (base-format))


(defparameter *scad-path*
  (cond ((member :macosx *features*)
	 "/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD")
	((member :windows *features*)
	 "c:/program files/openscad/openscad.exe")))



(defun scad-to-png (scad-file &key 
				(png-file (make-pathname :type "png"
							 :defaults scad-file))
				camera-point
				center-point 
				image-width
				image-height
				(projection :ortho))

  (unless (probe-file *scad-path*)
    (error "OpenSCAD executable not found at ~s. Please set geom-base::*scad-path*.~%"
	   *scad-path*))

  (uiop:run-program (remove nil
			    (list *scad-path*
				  "--preview=throwntogether"
				  "-o" 
				  (namestring png-file)
				  (when (and camera-point center-point)
				    (format nil "--camera=eye~a,~a,~a,center~a,~a,~a"
					    (get-x camera-point)
					    (get-y camera-point)
					    (get-z camera-point)
					    (get-x center-point)
					    (get-y center-point)
					    (get-z center-point)))
				  (when (and image-width image-height)
				    (format nil "--imgsize=~a,~a" image-width image-height))
				  (when projection 
				    (format nil "--projection=~a"
					    (ecase projection 
					      (:ortho "o")
					      (:perspective "p"))))
				  (namestring (translate-logical-pathname scad-file)))))
  png-file)
		      

(define-lens (scad base-object)()
  :output-functions
  ((transform ())
  
   (rgb-color 
    ()
    (let ((decimal (lookup-color (getf (the display-controls) :color)
				 :format :decimal))
	  (transparency (getf (the display-controls) :transparency)))
      (let ((*read-default-float-format* 'single-float))
	(format *stream* "~&color([~a,~a,~a,~a])~%"
		(svref decimal 0)
		(svref decimal 1)
		(svref decimal 2)
		(- 1 (or transparency 0))))))
		

   (cad-output 
    ()
    (write-the (cad-output-tree)))


   (cad-output-tree 
    ()
    (write-the rgb-color)
    (format *stream* "~&{~%")
    (if (null (the children))
	(write-the shape)
	(mapc #'(lambda(child)
		  (write-the-object child (cad-output-tree :from-root? nil)))
	      (the children)))
    (format *stream* "~&}~%"))))
    

    


(define-lens (scad ifs-output-mixin)()
  :output-functions
  ((shape ()
    (format *stream* "~&polyhedron(~%")

    (format *stream* "~&points = [~{[~{~a~^,~}]~^,~}],~%" 
	    (map 'list #'(lambda(point) (list (get-x point) (get-y point) (get-z point)))
		    (the ifs-array)))

    (format *stream* "~&faces = [~{[~{~a~^,~}]~^,~}]~%" 
	    ;;(mapcar #'reverse (the ifs-indices))
	    (the ifs-indices)
	    )
    
    (format *stream* "~&);~%")
    ())))

