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

(in-package :geom-base)



(defun matrix-lh-to-rh (lh-matrix)
  (let ((rh-matrix (make-array (list 3 3) :element-type 'double-float)))
    (setf (aref rh-matrix 0 0) (aref lh-matrix 0 0)
          (aref rh-matrix 0 1) (aref lh-matrix 0 1)
          (aref rh-matrix 0 2) (- (aref lh-matrix 0 2))
          (aref rh-matrix 1 0) (aref lh-matrix 1 0)
          (aref rh-matrix 1 1) (aref lh-matrix 1 1)
          (aref rh-matrix 1 2) (- (aref lh-matrix 1 2))
          (aref rh-matrix 2 0) (- (aref lh-matrix 2 0))
          (aref rh-matrix 2 1) (- (aref lh-matrix 2 1))
          (aref rh-matrix 2 2) (aref lh-matrix 2 2))
    rh-matrix))

    
    
(define-lens (base-format base-object)()
  :output-functions
  ((cad-output-tree
    ()
    (if (the leaf?) (write-the cad-output)
      (progn
        (mapc #'(lambda(child) (write-the-object child cad-output-tree)) (the children)))))))


;;
;; FLAG -- is this really needed? Doesn't look like it does anything...
;;
(define-lens (2d-output base-object)()
  :output-functions
  ((cad-output 
    () 
    (call-next-method))))
  

(define-lens (pdf base-object)()
  :output-functions
  ((rgb-stroke-setting
      ()
      (let* ((display-controls (find-in-hash self *display-controls*))
	     (color (or (getf display-controls :color)
			(the color-decimal)))
	     (color-decimal (if (consp color) color 
				(coerce (lookup-color (or color (format-slot foreground-color))) 'list)))
	     (fill-color-decimal (coerce (lookup-color (getf (the display-controls) :fill-color)) 'list)))
      
	(apply #'pdf:set-rgb-stroke color-decimal)
	(apply #'pdf:set-rgb-fill (or fill-color-decimal color-decimal))))


   ;;
   ;; FLAG -- remove this defunct version.
   ;;
   #+nil 
   (rgb-stroke-setting
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (color (or (getf display-controls :color)
		      (getf (the display-controls) :color)
		      :black))
           (color-decimal (coerce (lookup-color (or color (format-slot foreground-color))) 'list))
           (fill-color-decimal (coerce (lookup-color (getf (the display-controls) :fill-color)) 'list)))
      
      (apply #'pdf:set-rgb-stroke color-decimal)
      (apply #'pdf:set-rgb-fill (or fill-color-decimal color-decimal))))
   
   (line-thickness-setting
    ()
    (let* ((display-controls (or (find-in-hash self *display-controls*) (the display-controls)))
           (line-thickness (getf display-controls :line-thickness))
           (line-thickness (or line-thickness (the line-thickness))))
      (pdf:set-line-width line-thickness)))
   
  
   (dash-pattern-setting
    ()
    (let* ((display-controls (or (find-in-hash self *display-controls*)
                                 (the display-controls)))
           (dash-pattern (getf display-controls :dash-pattern))
           (dash-pattern (or dash-pattern (the dash-pattern))))
      
      (when dash-pattern
        (pdf:set-dash-pattern (list (first dash-pattern) (second dash-pattern))
                              (or (third dash-pattern) 0)))))

   ))



(define-lens (dxf base-object)()
  :output-functions
  (
   ;;
   ;; FLAG -- fill in.
   ;;
   (line-thickness-setting
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (line-thickness (getf display-controls :line-thickness))
           (line-thickness (or line-thickness (the line-thickness))))
      (declare (ignorable line-thickness))
      (warn "Add the setting of DXF line thickness")))
   
   ;;
   ;; FLAG -- fill in.
   ;;
   (rgb-stroke-setting
    ()
    (let* ((display-controls (or (find-in-hash self *display-controls*)
                                 (the display-controls)))
           (color-decimal (getf display-controls :color-decimal))
           (color-decimal (or color-decimal (coerce (or (the color-decimal) 
                                                        (format-slot foreground-color)
                                                        (lookup-color (the color-decimal))) 'list)))
           (dxf-color-code (or (getf display-controls :dxf-color-code)
                               0))
           
           (dxf-layer-number (or (getf display-controls :dxf-layer-number)
                               0)))

      (declare (ignorable color-decimal))

      ;; FLAG -- work colors & layers back in.
      (unless (zerop dxf-color-code)
	(format *stream* " 62~%~a~%" dxf-color-code))

      (format *stream* " 8~%~a~%" (if (zerop dxf-layer-number) "Main" dxf-layer-number))
      
      ))
   
   
   (dash-pattern-setting
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (dash-pattern (getf display-controls :dash-pattern))
           (dash-pattern (or dash-pattern (the dash-pattern))))
      (declare (ignorable dash-pattern))
      (warn "Add the setting of DXF dash pattern")))))
   


;;
;; FLAG -- the following has been folded into %lines-to-draw% and %curves-to-draw% to avoid
;; the need for separate views. But it works this way for now.
;;

#+nil
(define-lens (dxf global-filleted-polyline)()
  :output-functions
  ((cad-output
    ()
    (let ((lines (the straights))
          (fillets (list-elements (the fillets))))
      
      ;;(write-the line-thickness-setting)
      ;;(write-the rgb-stroke-setting)
      
      (with-format-slots (view)

        (mapc #'(lambda(line)
                  (destructuring-bind (start end)
                      (mapcar #'(lambda(vertex) 
                                  (let ((vertex (if view (the-object view (view-point vertex)) vertex)))
                                    (add-vectors (subseq vertex 0 2) *dxf-translation*))) line)
                    
                    (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                            (get-x start) (get-y start) (get-x end) (get-y end))
                    
                    ;;(write-the line-thickness-setting)
                    ;;(write-the rgb-stroke-setting)

                    
                    )) lines)
        
        
        (mapc #'(lambda(arc)
                  (let ((angle-correction 
                         (angle-between-vectors-d +rear-vector+
                                                  (the  (face-normal-vector :rear))
                                                  +top-vector+)))
                    (let ((start-angle (+ (radians-to-degrees 
                                           (the-object arc start-angle-normalized)) angle-correction))
                          (end-angle (+ (radians-to-degrees 
                                         (the-object arc end-angle-normalized)) angle-correction))
                          (radius (* (the-object arc radius) (the-object view view-scale-total)))
                          (center (add-vectors *dxf-translation* 
                                               (let ((point (the-object arc center)))
                                                 (if view (the-object view (view-point point)) point))))
                          (top-vector (the-object arc (face-normal-vector :top))))
                      
                      
                      (format *stream* "  0~%ARC~% 10~%~3,16f~% 20~%~3,16f~% 30~%~3,16f~% 40~%~3,15f
 50~%~3,15f~% 51~%~3,15f~% 210~%~3,15f~% 220~%~3,15f~% 230~%~3,15f~%" 
                              (get-x center) (get-y center) 0 radius start-angle end-angle
                              (get-x top-vector) (get-y top-vector) (get-z top-vector)))
                    
                    
                    ;;(write-the line-thickness-setting)
                    ;;(write-the rgb-stroke-setting)
                    
                    )
                    
                    
                  ( )) fillets)
        
        )))))


#+nil
(define-lens (dxf global-polyline)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((line-index-pairs (the %%line-vertex-indices%%))
            (2d-vertices (map 'vector (if view #'(lambda(point) (the-object view (view-point point))) #'identity)
                              (the %%vertex-array%%))))
        
        ;;
        ;; FLAG -- add setting of fill color
        ;;
        (let ((2d-vertices (map 'vector #'(lambda(vertex) 
                                            (add-vectors (subseq vertex 0 2) *dxf-translation*)) 2d-vertices)))
          (mapc #'(lambda(line-index-pair)
                    (destructuring-bind (start-index end-index) line-index-pair
                      (let ((start   (svref 2d-vertices start-index))
                            (end   (svref 2d-vertices end-index)))
                        (format *stream* "  0~%LINE~%  8~%0~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                                (get-x start) (get-y start) (get-x end) (get-y end))))
                    (write-the line-thickness-setting)
                    (write-the rgb-stroke-setting)
                    )
                line-index-pairs)))))))

