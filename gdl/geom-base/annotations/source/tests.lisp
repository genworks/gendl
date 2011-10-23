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

(in-package :gdl-user)

#|

NOTE: before compiling/loading this file, do:

  (cl-lite "~/<genworks>/gdl/src/demos/robot/")

|#

(defun run-tests ()
  "Initiate all test in file"
  (ang)
  (draw 'dxf)
  (draw 'pdf)
  (draw 'png)
  (cont)
  (contp)
  (draw2 'dxf)
  (draw2 'pdf)
  (draw2 'png)
  ;;(draw3 'dxf)
  ;;(draw3 'pdf)
  ;;(draw3 'png)
  )
  

(define-object vertical-0 (base-object)
  :input-slots ((length 1)(width 3)(height 0)
                (character-size 0.125))
    
  :objects
  ((p1 :type 'point :center (the (vertex :left :front :top)))
   (p2 :type 'point :center (the (vertex :right :rear :top)))
   (d1 :type 'vertical-dimension
       :pass-down (character-size)
       :start-point (the p1 center)
       :end-point (the p2 center))
   

   (d2 :type 'vertical-dimension
       :pass-down (character-size)
       :start-point (the p2 center)
       :end-point (the p1 center)
       :witness-line-length 0.5
       :flip-leaders? t
       :outside-leaders? t
       :text-above-leader? t
       :leader-line-length 0.5
       :arrowhead-style :triangle)))


(define-object vertical-1-drawing (base-drawing)
  :objects
  ((vert :type 'vertical-dimension
         :arrowhead-style :none
         :arrowhead-style-2 :none
         :start-point (the box (vertex :top :right :front))
         :end-point (the box (vertex :top :right :rear)))
   
   (box :type 'box :length 10 :width 20 :height 0)

   (poly :type 'global-polyline
         :vertex-list (list (make-point 0 0 0)
                            (make-point 1 1 0)
                            
                            (make-point 2 0 0)
                            ))
                            
   (main :type 'base-view
         :objects (list (the box) 
                        (the poly)
                        (the vert)
                   ))))


(define-object vertical-1 (base-object)
  
  :computed-slots
  ((start-point (make-point 449.8032 0 0))
   (end-point (make-point 405.42 -195 0)))
  
  :objects
  (
   
   (vertical :type 'vertical-dimension
             :pass-down (start-point end-point))))



(define-object  vertical-2 (base-object)
  
   :objects
   ((box :type 'box
         :length 10 :width (* (the-child length) +phi+)
         :height (* (the-child :width) +phi+))
   
    (length-dimension :type 'vertical-dimension
                      :font-size (/ (the box length) 20)
                      :arrowhead-width (/ (the-child font-size) 3)
                      :flip-leaders? t
                      :start-point (the box (vertex :top :left :front))
                      :end-point (the box (vertex :top :left :rear)))))


(define-object horizontal-1 (base-object)
  
  :objects
  ((box :type 'box
        :length 10 :width (* (the-child length) +phi+)
        :height (* (the-child :width) +phi+))
   
   (width-dimension :type 'horizontal-dimension
                    :font-size (/ (the box length) 20)
                    :arrowhead-width (/ (the-child font-size) 3)
                    :start-point (the box (vertex :top :left :rear))
                    :end-point (the box (vertex :top :right :rear)))))


(define-object par-1-draw (base-drawing)
  :objects 
  ((par :type 'parallel-1)
   
   (main-view :type 'base-view
              :objects (list ;;(the par box) 
                             (the par length-dimension)
                             ))))


(define-object parallel-1 (base-object)
  
  :objects
  ((box :type 'box
        :length 10 :width (* (the-child length) +phi+)
        :height (* (the-child :width) +phi+))
   
   (length-dimension :type 'parallel-dimension
                     :text-along-axis? t
                     :text-above-leader? t
                     :font-size (/ (the box length) 20)
                     :arrowhead-width (/ (the-child font-size) 3)
                     :start-point (the box (vertex :top :left :front))
                     :end-point (the box (vertex :top :right :rear)))))



(define-object angular-1 (base-drawing)

  :input-slots 
  ((length 792) (width 612)
   (character-size 0.1))

  
  :computed-slots
  ((p1 (translate (the center) :right 2.0 :rear 0.0))
   (p2 (translate (the center) :right 1.8 :rear 1.2))
   (p3 (translate (the center) :right 0.25 :rear 1.0))
   (p4 (translate (the center) :right -0.25 :rear 1.0)))


  :hidden-objects
  
  ((main-view :type 'base-view
              :border-box? t
              :objects (set-difference (the hidden-children) (list (the main-view)) :test #'eql))
   
   (center-point :type 'point :center (the center))
   (point-1 :type 'point :center (the p1))
   (point-2 :type 'point :center (the p2))
   (point-3 :type 'point :center (the p3))
   (point-4 :type 'point :center (the p4))
   
   (inside-part :type 'angular-dimension
                ;;:pass-down (character-size)
                :character-size 0.3
                :center-point (the center)
                :start-point (the p1)
                :end-point (the p2)
                :text-along-axis? t
                :witness-1-to-center? t
                ;;:dim-text-start-offset (make-vector .05 .05 0)
                )
   

   (above-part :type 'angular-dimension
               :pass-down (character-size)
               :center-point (the center)
               :start-point (the p2)
               :end-point (the p3)
               :arrowhead-style :wedge
               :text-along-axis? t
               :dim-angle-precision 0
               :suppress-decimal? t)


   (outside-part :type 'angular-dimension
                 :pass-down (character-size)
                 :center-point (the center)
                 :start-point (the p4)
                 :text-along-axis? t
                 :end-point (the p3))
   

   (big-angle :type 'angular-dimension
              :pass-down (character-size)
              :text-along-axis? t
              :center-point (the center)
              :start-point (the p4)
              :end-point (the p1)
              :dim-angle-style :degrees-minutes-seconds
              :leader-radius 1.5)))


(defun ang ()
  (with-format (pdf "/tmp/ang-try.pdf")
    (write-the-object (make-object 'angular-1) cad-output)))

(defun ang-dxf ()
  (with-format (dxf "/tmp/ang-try.dxf")
    (write-the-object (make-object 'angular-1) cad-output)))


(define-object test-matrix (base-object)
  
  :input-slots ((length (half 792)) (width 612)
                (character-size 16)
                (leader-line-length (* (the character-size) 3))
                (witness-line-length (the character-size))
                (witness-line-ext (the witness-line-length))
                (witness-line-gap (half (the witness-line-ext))))
  
  :computed-slots (
                   ;;(ui-display-list-leaves (list-elements (the box-matrix)))
                   )
  
  :objects
  (
   
   (rear-box :type 'box
             :display-controls (list :dxf-color-code 1)
             :length 700
             :width 500
             :center (translate (the center) :rear 450))
   
   (hey-now :type 'general-note 
            :display-controls (list :dxf-color-code 1)
            ;;:center (the rear-box center)
            :start (translate (the rear-box (vertex :rear :top :left))
                              :front
                              (the-child character-size))
            :character-size 50
            ;;:font "Courier"
            :font "Helvetica"
            
            :orientation 
            (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
                                              30
                                              (the (face-normal-vector :top))))
            :strings (list "hey now" "I'm Henk"
                           "The quick brown"
                           "fox jumps over"
                           "the lazy dog"
                           ))
   

   
   
   (box :type 'box :length 10 :width 10 :height 10)
   

   (hey :type 'general-note
        :strings "hey again" )

   

   (vertical-dims :type 'vertical-dimension
                  ;;:hidden? t
                  :sequence (:matrix :lateral 2 :longitudinal 2)
                  ;;:outline-shape-type :bubble
                  :display-controls (list :dxf-color-code 1)
                  :pass-down (leader-line-length 
                              character-size witness-line-length witness-line-ext witness-line-gap)
                  :outside-leaders? (oddp (first (the-child index)))
                  :text-above-leader? nil
                  
                  :start-point (the (box-matrix (first (the-child index))
                                                (second (the-child index)))
                                 (vertex :rear :right :top))
                  :end-point (the (box-matrix (first (the-child index))
                                              (second (the-child index)))
                               (vertex :front :right :top))
                  
                  
                  ;;:start-point (if (oddp (second (the-child index)))
                  ;;(the-child end-point-n) (the-child start-point-n))
                  
                  ;;:end-point (if (oddp (second (the-child index)))
                  ;;(the-child start-point-n) (the-child end-point-n))
                  
                  )
  


   (vertical-dims-flipped :type 'vertical-dimension
                          ;;:hidden? t
                          :display-controls (list :dxf-color-code 1)
                          :sequence (:matrix :lateral 2 :longitudinal 2)
                          :pass-down (leader-line-length
                                      character-size witness-line-length 
                                      witness-line-ext witness-line-gap)
                          :text-along-axis? t
                          :outside-leaders? (oddp (first (the-child index)))
                          
                          :flip-leaders? t
                          :start-point (if (oddp (second (the-child index)))
                                           (the-child end-point-n) (the-child start-point-n))
                  
                          :end-point (if (oddp (second (the-child index)))
                                         (the-child start-point-n) (the-child end-point-n))
                          
                          :start-point-n (the (box-matrix (first (the-child index))
                                                          (second (the-child index)))
                                           (vertex :rear :left :top))
                          :end-point-n (the (box-matrix (first (the-child index))
                                                        (second (the-child index)))
                                         (vertex :front :left :top)))
   

   (horizontal-dims  :type 'horizontal-dimension
                     ;;:hidden? t
                     ;;:display-controls (list :dxf-color-code 1)
                     :sequence (:matrix :lateral 2 :longitudinal 2)
                     :pass-down (leader-line-length
                                 character-size witness-line-length witness-line-ext witness-line-gap)
                     
                     ;;:outline-shape-type :bubble
                     
                     :outside-leaders? (evenp (first (the-child index)))
                     
                     :text-above-leader? nil
                     
                     ;;:start-point (if (oddp (second (the-child index)))
                     ;;(the-child end-point-n) (the-child start-point-n))
                  
                     ;;:end-point (if (oddp (second (the-child index)))
                     ;;(the-child start-point-n) (the-child end-point-n))
                     
                     :start-point (the (box-matrix (first (the-child index))
                                                     (second (the-child index)))
                                    (vertex :rear :left :top))
                     
                     :end-point (the (box-matrix (first (the-child index))
                                                   (second (the-child index)))
                                    (vertex :rear :right :top)))
   
   

   (horizontal-label :type 'label
                     :pass-down (character-size)
                     :strings "test label"
                     :leader-path (list (the center)
                                        (translate (the center) :right 50)
                                        (translate (the center) 
                                                   :right 50
                                                   :rear 50)))

                                        
   
   (horizontal-dims-flipped  :type 'horizontal-dimension
                             ;;:display-controls (list :dxf-color-code 1)
                             ;;:hidden? t
                             :sequence (:matrix :lateral 2 :longitudinal 2)
                             :pass-down (leader-line-length
                                         character-size witness-line-length 
                                         witness-line-ext witness-line-gap)
                             :outside-leaders? (evenp (first (the-child index)))
                             :flip-leaders? t
                             
                             :start-point (if (oddp (second (the-child index)))
                                              (the-child end-point-n) (the-child start-point-n))
                  
                             :end-point (if (oddp (second (the-child index)))
                                            (the-child start-point-n) (the-child end-point-n))
                             
                             :start-point-n (the (box-matrix (first (the-child index))
                                                             (second (the-child index)))
                                              (vertex :front :left :top))
                             :end-point-n (the (box-matrix (first (the-child index))
                                                           (second (the-child index)))
                                            (vertex :front :right :top)))

   (box-matrix :type 'box
               ;;:display-controls (list :dxf-color-code 1)
               ;;:hidden? t
               :sequence (:matrix :lateral 2 :longitudinal 2)
               :length (/ (the length) 4)
               :width (/ (the width) 4)
               :quantify-box (the box-matrix-quant-box))
   

   (box-matrix-quant-box :type 'box
                         ;;:display-controls (list :dxf-color-code 1)
                         ;;:hidden? t
                         )
   
   
   ))




(defmethod draw ((format (eql 'dxf)))
  (with-format (dxf "/tmp/draw-try.dxf")
    (write-the-object (make-object 'busy-drawing) cad-output)))

(defmethod draw ((format (eql 'pdf)))
  (with-format (pdf "/tmp/draw-try.pdf")
    (write-the-object (make-object 'busy-drawing) cad-output)))

(defmethod draw ((format (eql 'png)))
  (with-format (png "/tmp/draw-try.png")
    (write-the-object (make-object 'busy-drawing) cad-output)))


(define-object busy-drawing (base-drawing)
  
  :input-slots
  ((page-length 792) (page-width 612))
  
  :objects
  ((large :type 'base-view
          :border-box? t
          ;;:left-margin 50
          ;;:center (translate (the center) :left 50)
          :objects (the matrix children)
          ;;:objects (list (the matrix hey-now))
          )

   
   (matrix :type 'test-matrix))
  
  :functions
  ((dxf 
    ()
    (with-format (dxf "/tmp/try.dxf")
      (write-the-object self cad-output)))))


(define-object container (gwl:web-drawing)
  
  :computed-slots
  (;;(user-center (make-point 160 0 0))
   
   (objects (list (the annotated-drawing))))
  
  :objects
  ((annotated-drawing :type 'annotated-drawing)))


(defun cont ()
  (with-format (pdf "/tmp/cont-try.pdf")
    (write-the-object (make-object 'container) cad-output)))


(defun contp ()
  (with-format (png "/tmp/cont-try.png")
    (write-the-object (make-object 'container) cad-output)))

(define-object annotated-drawing (base-drawing)
  :input-slots
  ((page-length 700) (page-width 800))
  
  :objects
  (


   (main-view :type 'base-view
              :projection-vector (getf *standard-views* :trimetric)
              :width (* (the width) .95)
              :length (* (the length) .5)
              :center (translate (the center) :front (half (the-child length)))
              :objects (cons (the rear-box) (the box-matrix children))
              :annotation-objects  (list 
                                    (the vertical-dim) 
                                    (the horizontal-dim)))
   

   
   (top-view  :type 'base-view
              :projection-vector (getf *standard-views* :top)
              :width (* (the width) .95)
              :length (* (the length) .5)
              :objects (cons (the rear-box) (the box-matrix children))
              :center (translate (the center) :rear (half (the-child length)))
              )
   

   (rear-box :type 'box
             :length (the box-matrix length)
             :width (the box-matrix width)
             :center (the box-matrix (face-center :rear)))
   
   
   
   (box-matrix :type 'box-matrix
               :length 300 
               :width 200)


   (angular-dim :type 'angular-dimension
                :character-size 15
                :witness-line-length 30
                :witness-line-ext 30
                :witness-line-gap 10
                   
                :center-point (the main-view (view-point 
                                               (the box-matrix (boxes 0 0) (vertex :front :left :top))))
                :end-point (the main-view (view-point 
                                              (the box-matrix (boxes 0 0) (vertex :rear :left :top))))
                
                :start-point (the main-view (view-point 
                                              (the box-matrix (boxes 0 0) (vertex :front :right :top)))))

   (horizontal-dim :type 'horizontal-dimension
                   :start-point (the main-view (view-point 
                                                (the box-matrix (boxes 0 0) (vertex :top :rear :left))))
                   :end-point (the main-view (view-point 
                                              (the box-matrix (boxes 0 0) (vertex :top :rear :right))))
                   :witness-line-length 30
                   :witness-line-ext 30
                   :witness-line-gap 10
                   :dim-scale (/ (the main-view view-scale))
                   :character-size 10)

   (vertical-dim :type 'vertical-dimension
                 :start-point (the main-view (view-point (the box-matrix (boxes 0 0) 
                                                              (vertex :top :front :right))))
                 :end-point (the main-view (view-point (the box-matrix (boxes 0 0) 
                                                            (vertex :top :rear :right))))
                 :witness-line-length 30
                 :witness-line-ext 30
                 :witness-line-gap 10
                 :leader-line-length 50
                 :dim-scale (/ (the main-view view-scale))
                 :outside-leaders? t
                 :character-size 15)))


(define-object box-matrix (base-object)
  :objects
  ((boxes :type 'box
          :sequence (:matrix :lateral 2 :longitudinal 2)
          :length (/ (the length) 4)
          :width (/ (the width) 4)
          :height 10
          :display-controls (when (equalp (the-child index) (list 0 0))
                              (list :color :red))
          :quantify-box (the boxes-quantify-box))
   
   (boxes-quantify-box :type 'box :height 0)))


(defmethod draw2 ((format (eql 'dxf)))
  (with-format (dxf "/tmp/draw2-try.dxf")
    (write-the-object (make-object 'annotated-drawing) cad-output)))

(defmethod draw2 ((format (eql 'pdf)))
  (with-format (pdf "/tmp/draw2-try.pdf" :page-length 700 :page-width 800)
    (write-the-object (make-object 'annotated-drawing) cad-output)))

(defmethod draw2 ((format (eql 'png)))
  (with-format (png "/tmp/draw2-try.png" :page-length 700 :page-width 800)
    (write-the-object (make-object 'annotated-drawing) cad-output)))



(define-object robot-web (gwl:web-drawing)
  :input-slots
  ((page-length 700) (page-width 900))
  
  :computed-slots
  ((projection-vector (getf *standard-views* :trimetric))
   
   (object-roots (list (the robot robot))))
  
  :objects
  ((robot :type 'robot:assembly)))



(define-object robot-drawing (base-drawing)
  :input-slots
  ((page-length 792) (page-width 612))
  
  :objects
  ((main-view :type 'base-view
              :fit-to-page? t
              :projection-vector (getf *standard-views* :top)
              :width (* (the width) .95)
              :length (* (the length) .95)
              :object-roots (list (the robot))
              )

   (robot :type 'robot:assembly)))



(defmethod draw3 ((format (eql 'dxf)))
  (with-format (dxf "/tmp/draw3-try.dxf")
    (write-the-object (make-object 'robot-drawing) cad-output)))


(defmethod draw3 ((format (eql 'pdf)))
  (let ((self (make-object 'robot-drawing)))
    (with-format (pdf "/tmp/draw3-try.pdf" :page-length (the page-length) :page-width (the page-width))
      (write-the cad-output))))


(defmethod draw3 ((format (eql 'png)))
  (let ((self (make-object 'robot-drawing)))
    (with-format (png "/tmp/draw3-try.png")
      (write-the cad-output))))



(define-object box-with-drawing (base-object)

  :objects      
  ((drawing :type 'dimensioned-drawing
            :objects (list (the box) (the length-dim)))
     
   (length-dim :type 'horizontal-dimension
               :start-point (the box (vertex :rear :top :left))
               :end-point (the box (vertex :rear :top :right)))
     
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object dimensioned-drawing (base-drawing)
  :input-slots (objects)
    
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :objects (the objects))))



(define-object box-with-two-viewed-drawing (base-object)

  :objects      
  ((drawing :type 'two-viewed-drawing
            :objects (list (the box) (the length-dim)))
     
   (length-dim :type 'horizontal-dimension
               :start-point (the box (vertex :rear :top :left))
               :end-point (the box (vertex :rear :top :right)))
     
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object two-viewed-drawing (base-drawing)
  :input-slots (objects)
    
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :length (half (the length))
              :center (translate (the center) :rear (half (the-child length)))
              :objects (the objects))
   
   (top-view :type 'base-view 
             :projection-vector (getf *standard-views* :top)
             :length (half (the length))
             :center (translate (the center) :front (half (the-child length)))
             :objects (the objects))))
   


(define-object box-with-annotated-drawing (base-object)

  :objects      
  ((drawing :type 'box-annotated-drawing
            :objects (list (the box)))
     
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object box-annotated-drawing (base-drawing)
  :input-slots (objects (character-size 15)
                (witness-line-gap 10)
                (witness-line-length 15)
                (witness-line-ext 5))
    
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :length (half (the length))
              :center (translate (the center) :rear (half (the-child length)))
              :objects (the objects)
              :annotation-objects (list (the main-length-dim)))
   
   
   (main-length-dim :type 'vertical-dimension
                    :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                    :start-point (the main-view (view-point (the box (vertex :rear :top :right))))
                    :end-point (the main-view (view-point (the box (vertex :rear :bottom :right))))
                    :dim-value (3d-distance (the box (vertex :rear :top :right))
                                            (the box (vertex :rear :bottom :right)))
                    :text-above-leader? nil)
                    
   
   (top-view :type 'base-view 
             :projection-vector (getf *standard-views* :front)
             :length (half (the length))
             :center (translate (the center) :front (half (the-child length)))
             :objects (the objects)
             :annotation-objects (list (the top-length-dim)))
   
   (top-length-dim :type 'vertical-dimension
                   :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                   :start-point (the top-view (view-point (the box (vertex :rear :top :right))))
                   :dim-scale (/ (the top-view view-scale))
                   :text-above-leader? nil
                   :end-point (the top-view (view-point (the box (vertex :rear :bottom :right)))))))





(define-object box-with-immune-dimension (base-object)

  :objects      
  ((drawing :type 'immune-dimension-drawing
            :objects (list (the box)))
     
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object immune-dimension-drawing (base-drawing)
  
  :input-slots (objects (character-size 20) (witness-line-gap 10)
                (witness-line-length 15) (witness-line-ext 5))
    
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :objects (append (the objects) (list (the length-dim)))
              :immune-objects (list (the length-dim)))
   
   (length-dim :type 'horizontal-dimension
               :character-size (/ (the character-size) (the main-view view-scale))
               :witness-line-gap (/ (the witness-line-gap) (the main-view view-scale))
               :witness-line-length (/ (the witness-line-length) (the main-view view-scale))
               :witness-line-ext (/ (the witness-line-ext) (the main-view view-scale))
               :start-point (the box (vertex :rear :top :left))
               :end-point (the box (vertex :rear :top :right))

               )))
                    



