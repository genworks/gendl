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


(defparameter *typeset-block-center-circ* nil)

(define-object typeset-block (base-object)
  
  :documentation (:description "Block of text typeset using cl-typesetting. This object
wraps the typeset block as a standard GDL object, so it can be placed in a view and 
positioned according to normal GDL positioning.

You can specify the width, and by default this object will compute its length automatically 
from the typeset content, to fit all the lines of text into the box. Because of this 
computed behavior of the length, the center of the box will not, in general, be in a 
known location compared to the start of the text. Because of this it is recommended
to use :corner, rather than :center, for positioning a base-view which contains
a typeset block. 

In the normal case, if you want a single block in a view on a drawing, you should
make the base-view object have the same width and length as the typeset-block. The
base-view should also probably have :left-margin 0 and :front-margin 0.")
 
    
  :input-slots
  (
   ("Number. The line number to start" start-line-index nil) 

   
   ("3D-point. Center of the text. Specify this or start, not both. 
NOTE that the center is no longer defaulting (so that it can self-compute properly when start 
is specified), so it is necessary to explicitly give either start or center for general-note."
    center (if *typeset-block-center-circ*
               (the parent center)
             (translate (the start) 
                        :right (half (the width))
                        :front (half (the length)))))
   
   
   ("Number. The length of the box to contain the compiled content. Defaults is (the length-default),
which will exactly fit the compiled content into the specified width. If you override it to be less 
than this default, the content will be cropped."
    length (the length-default))
    
   

   ("3D-point. Start of the text. Specify this or center, not both."
    start (let ((*typeset-block-center-circ* t))
            (translate (the center) 
                       :left (half (the width))
                       :rear (half (the length))))))
  
  :computed-slots
  ((end-line-plist (let ((lines (coerce (the lines) 'array))
                         (block-length (the length)))
                     
                     (let ((lines-length (length lines)))
                       (print-variables lines-length))
                     
                     (do* ((i (or (the start-line-index) 0) (1+ i))
                           (line (aref lines i) (aref lines i))
                           (y (typeset::dy line) (typeset::dy line))
                           (y-sum y (+ y-sum y)))
                         ((or (>= y-sum block-length)
                              (= i (1- (array-dimension lines 0))))
                          (list :end-line-index (1- i) 
                                :length (- y-sum y)))
                       
                       (let ((array-dim (array-dimension lines 0)))
                         (print-variables array-dim i y y-sum block-length))
                       )))
   
   (end-line-index (when (the start-line-index) (getf (the end-line-plist) :end-line-index)))
   
   ("List of typeset line objects. The list of lines in the nominal block."
    lines (typeset::fit-lines (the content) (the width) most-positive-fixnum ))

   
   ("Number. The computed length which will exactly fit the content based on (the width)."
    length-default (if (the start-line-index) (getf (the end-line-plist) :length)
                     (reduce #'+ (mapcar #'typeset::dy (the lines)))))
   
      
   (%corners% (list (the (vertex :top :left :rear))
                    (the (vertex :top :right  :rear))
                    (the (vertex :top :right :front))
                    (the (vertex :top :left :front))))
   
   
   (content-block (the content))
   
   (boxes (typeset::boxes (the content-block)))
   
   )
  
  ;;:objects ((box :type 'box))
  
  :functions
  ((content ())))


;; FLAG -- document and remove funcall when cl-typesetting stabilized.
;;
;;
;; FLAG -- update for auto-scaling outside base-view
;;

(define-lens (pdf typeset-block)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((view-scale (if view (the-object view view-scale-total) 1))
            (start 
             (if view 
                 (if (and (the-object view parent)
                          (find-package :gwl)
                          (typep (the-object view parent) (read-from-string "gwl:web-drawing")))
                     (the start)
                   (the-object view (view-point (the start))))
               (the start))))
        
        (let ((rotation (angle-between-vectors-d +rear-vector+ (the (face-normal-vector :rear)) +top-vector+)))
          (pdf:with-saved-state
              (pdf:scale view-scale view-scale)
            (with-translated-state (:pdf start)
              (write-the rgb-stroke-setting)
              (unless (zerop rotation) (pdf:rotate rotation))
              (when (the content)
                
                (funcall (symbol-function (read-from-string "typeset::gdl-draw-block")) 
                         (the content) 0 0 
                         (the width) 
                         (max (the length) (the length-default))
                         ;;10000
                         :start-line (the start-line-index)
                         :end-line (the end-line-index)
                         :all-lines (the lines)))
              
              (pdf:in-text-mode (write-the additional-output))
              
              (write-the additional-non-text)))))))

   (additional-output ())
   
   (additional-non-text ())))


(define-object typeset-blocks ()
  :input-slots (full-block length width)
  
  :computed-slots ((number-of-blocks (let ((number-of-blocks 1))
                                       (do ((block (make-object (the full-block type)
                                                                :start-line-index 0
                                                                :length (the length)
                                                                :width (the width))
                                              (make-object (the full-block type)
                                                           :start-line-index (the-object block end-line-index)
                                                           :length (the length)
                                                           :width (the width))))
                                           ;;
                                           ;; FLAG -- why does this need to be 2 lines early?
                                           ;;
                                           ((>= (the-object block end-line-index)
                                                (- (length (the full-block lines)) 2))
                                            number-of-blocks)
                                         (let ((end-line-index (the-object block end-line-index))
                                               (full-block-lines-length (length (the full-block lines))))
                                           (print-variables end-line-index full-block-lines-length number-of-blocks)
                                           (incf number-of-blocks))))))
  
  :objects ((blocks :type (the full-block type)
                    :sequence (:size (the number-of-blocks))
                    :width (the width)
                    :length (the length)
                    :start-line-index (if (the-child first?) 0 (the-child previous end-line-index)))))
  



(in-package :typeset)

(defmethod gdl-draw-block (content x y dx dy 
                       &key border (padding 5) rotation (v-align :top) special-fn
                            start-line end-line all-lines)
 ;;; On the current *page*
  (pdf:with-saved-state
    (pdf:translate x y)
    (when rotation
      (pdf:rotate rotation))
    
    
    (when border
      (with-quad (left top right bottom) padding
        (pdf:set-line-width border)
        (pdf:set-gray-stroke 0)
        (pdf:set-gray-fill 1)
        (pdf:basic-rect (- left) top (+ dx left right) (- (+ dy top bottom)))
        (pdf:fill-and-stroke)
        (pdf:set-gray-fill 0)))


    (let ((vbox (if (and start-line end-line)
                    (make-filled-vbox-from-part content dx dy :v-align v-align 
                                                :start-line start-line 
                                                :end-line end-line
                                                :all-lines all-lines)
                  (make-filled-vbox content dx dy v-align))))
      

      
      (when special-fn
        (funcall special-fn vbox 0 0))
      (stroke vbox 0 0))))

(defun make-filled-vbox-from-part (content dx dy 
                                   &key (v-align :top) (advance t)
                                        start-line end-line all-lines)
  
  (with-text-content (content)
    (let* ((lines (or all-lines 
                      (fit-lines content dx  dy
                                 v-align advance)))
           (length (length lines)))
      (gdl:print-variables length)
      (multiple-value-bind (boxes) (subseq lines start-line end-line)
        (when boxes
          (let* ((vbox (make-instance 'vbox  :dx dx :dy dy  :boxes boxes  :fixed-size t)))
            (do-layout vbox)
            vbox))))))
