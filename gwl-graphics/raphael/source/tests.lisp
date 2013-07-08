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

(in-package :gwl-user)

(define-object dd (base-ajax-sheet base-object)
 
  :computed-slots ((use-raphael? t)

		   (too-far? (> (3d-distance (first (the poly-1 vertex-list))
					     (first (the poly-2 vertex-list)))
				3))

		   (main-sheet-body (with-cl-who-string ()
				      (str (the development-links))
				      (str (the main-area main-div))
				      (str (the drop-coord-section main-div))
				      ))
		   

		   (dropped-x-y (if (the main-area dropped-x-y)
				    (make-point (get-x (the main-area dropped-x-y))
						(- (get-y (the main-area dropped-x-y))
						   1)
						0)
				    (make-point 0 0 0))))
  

  :functions ((on-drop () (format t "~&We just dropped an object at ~s~%" (the main-area dropped-x-y))))

  :objects ((poly-0 :type 'global-polyline 
		    :display-controls (list :color :red :fill-color :orange)
		    :vertex-list (list (make-point -2 0 0)
				       (make-point -1 0 0)
				       (make-point -1 1 0)
				       (make-point -2 0 0)))
	    
	    (poly-1-up :type 'global-polyline 
		       :display-controls (list :color :red :fill-color :blue)
		       :vertex-list (list (make-point 0 2 0)
					  (make-point 1 2 0)
					  (make-point 1 3 0)
					  (make-point 0 2 0)))

	    (poly-1-down :type 'global-polyline 
			 :display-controls (list :color :red :fill-color :blue)
			 :vertex-list (list (make-point 0 -2 0)
					    (make-point 1 -2 0)
					    (make-point 1 -1 0)
					    (make-point 0 -2 0)))


	    (poly-1 :type 'global-polyline 
		    :display-controls (list :color :red :fill-color :blue)
		    :vertex-list (list (the dropped-x-y)
				       (translate (the dropped-x-y) :right 1)
				       (translate (the dropped-x-y) :right 1 :rear 1)
				       (the dropped-x-y)))

	    (poly-2 :type 'global-polyline 
		    :display-controls (list :color :black 
					    :fill-color (if (the too-far?) 
							    :red
							    :green))
		    :vertex-list (list (make-point 2 0 0)
				       (make-point 3 0 0)
				       (make-point 3 1 0)
				       (make-point 2 0 0)))

	    (drop-coord-section :type 'sheet-section
				:inner-html (with-cl-who-string ()
					      ((:table  :border 1)
						  (:tr (:td "dropped coord:")
						       (:td (str (the main-area dropped-x-y))))
						(:tr (:td "dropped dimensions:")
						       (:td (str (the main-area dropped-height-width))))
						(:tr (:td "dropped object:")
						       (:td (fmt "~s" (the main-area dropped-object)))))))
            
	    (main-area :type 'base-ajax-graphics-sheet
		       :respondent self


		       :on-drop-function 

		       #'(lambda()
			   (format t "Just called the on-drop hook with ~s, ~s, and ~s.~%"
				   (the main-area dropped-x-y)
				   (the main-area dropped-height-width)
				   (the main-area dropped-object)))

		       #+nil
		       :on-move-function 
		       #+nil
		       #'(lambda()
			   (format t "Just called the on-move hook with ~s, ~s, and ~s.~%"
				   (the main-area dropped-x-y)
				   (the main-area dropped-height-width)
				   (the main-area dropped-object)))

		       :vector-graphics-onclick? nil
		       :length 500 :width 500
		       :projection-vector (getf *standard-views* :top)
		       :display-list-objects (list (the poly-0) (the poly-1-up) (the poly-1-down) (the poly-1) (the poly-2)))))



            



#+nil
(define-object test-sheet (base-ajax-sheet)
  
  :computed-slots ((use-raphael? t)
                   (html-sections (list (the viewport))))
  
  :objects
  ((viewport :type 'test-vp
             :respondent self)))


#+nil
(define-lens (html-format test-sheet)()
  :output-functions
  ((main-sheet-body
    ()
    (with-html-output (*stream*)
      (write-the viewport main-div)))))





#+nil
(define-lens (html-format test-vp)()
  :output-functions
  ((main-view
    ()
    (with-html-output (*stream*)
      (write-the raphael-canvas)
      (str (the restore-button form-control-string))
      (str (the projection-direction html-string))
      ))))
           
#+nil
(defun publish-object (object &key (path (format nil "/~a" object)))
  (publish :path path
           :function #'(lambda(req ent)
                         (gwl-make-object req ent (format nil "~s" object)))))

;;(publish-object 'test-sheet)
