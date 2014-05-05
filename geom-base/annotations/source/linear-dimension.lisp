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


(define-object linear-dimension (outline-specialization-mixin base-object)
  
  :documentation (:description "Creates a dimension along either the
horizontal, vertical, or an arbitray axis. Use
<tt>horizontal-dimension</tt>, <tt>vertical-dimension</tt>, or
<tt>parallel-dimension</tt>, respectively, to achieve these.")
  
  :input-slots
  (
   "Must be specified in the subclass except for angular"
   witness-direction-vector
  
   "Must be specified in the subclass except for angular"
   leader-direction-1-vector
 
   "Must be specified in the subclass except for angular"
   leader-direction-2-vector
  
   "Must be specified in the subclass except for angular"
   base-plane-normal


   ("Keyword symbol. Currently can be :bubble, :rectangle, or :none. Default is :none."
    outline-shape-type :none)
   
   ("GDL"
    underline? nil)
   
  
   "3D Point. Actual point where the dimension will start measuring"
   start-point 
   
   "3D Point. Actual point where the dimension will stop measuring"
   end-point 
   
   
   ("Keyword symbol, :left, :right, or :center. 
     For multi-line dim-text, this justification is applied."
    justification :left)
   
   ("Boolean. Where applicable, determines whether text direction follows leader-line direction" 
    text-along-axis? nil)

   (dim-scale 1)
   
   ("Number in Percentage. Adjusts the character width for DXF output. Defaults to the text-x-scale."
    dxf-text-x-scale (the text-x-scale))
   
   ("Number. The scale factor for DXF character size vs PDF character size. Default is 0.8"
    dxf-size-ratio 0.8)
   
   ("String. This names the DXF font for this general-note. Defaults to <tt>(the font)</tt>."
    dxf-font (the font))
   
   ("Number. The start of text will be offset by this amount for DXF output. Default is 2."
    dxf-offset 3)

   
   ("Number. 2D distance relative to the base-plane-normal. Can be over-ridden in the subclass"
    dim-value (* (3d-distance (inter-line-plane (inter-line-plane (the start-point)
                                                                  (the base-plane-normal)
                                                                  (the center)
                                                                  (the base-plane-normal))
                                                
                                                (the (face-normal-vector :top))
                                                (the center)
                                                (the (face-normal-vector :top)))
                              (inter-line-plane (inter-line-plane (the end-point)
                                                                  (the base-plane-normal)
                                                                  (the center)
                                                                  (the base-plane-normal))
                                                (the (face-normal-vector :top))
                                                (the center)
                                                (the (face-normal-vector :top)))) (the dim-scale)))
   
   
   ("Boolean. Indicates whether to display a witness line coming off the <tt>start-point</tt>. Default is T"
    witness-line? t) 
   
   ("Boolean. Indicates whether to display a witness line coming off the <tt>end-point</tt>. Default is T"
    witness-line-2? t) 
   
   ("Number. Distance from the <tt>start-point</tt> and <tt>end-point</tt> to the start of each witness-line. Default is 0.1"
    witness-line-gap 0.1)
   
   ("Number. Distance the witness line(s) extend beyond the leader line. Default is 0.3"
    witness-line-ext 0.3)
   
   ("Boolean. Indicates whether the text is to the right or above the leader line, rather than in-line with it. Default is T."
    text-above-leader? t) 
   
   ("Number. Amount of gap between leader lines and dimension text, when the dimension text is within the leader.
Defaults to half the character-size."
    leader-text-gap (half (the character-size)))
   
   
   ("Number in Percentage. Adjusts the character width for the dimension-text and currently only applies only to PDF output"
    text-x-scale 100)
   
   ("Boolean. Indicates which direction the witness lines should take from the start and end points. The Default is NIL,
  which indicates :rear (i.e. ``up'') for <tt>horizontal-dimensions</tt> and :right for <tt>vertical-dimensions</tt>"
    flip-leaders? nil)
   
   ("Boolean. Indicates whether the leader line(s) should be inside or outside the interval between the start and 
end points. The default is NIL, which indicates that the leader line(s) should be inside the interval"
    outside-leaders? nil)
   
   ("Boolean. Indicates whether the first (or only) leader line should be displayed. The Default is T"
    leader-1? t) 
   
   ("Boolean. Indicates whether the second leader line should be displayed. The Default is T"
    leader-2? t)
   
   ("Number. Indicates the length of the first leader for the case when <tt>outside-leaders?</tt> is non-NIL"
    leader-line-length (* (the arrowhead-length) (the outside-leaders-length-factor)))
   
   ("Number. Indicates the length of the full leader when <tt>outside-leaders?</tt> is nil. This defaults to nil,
 which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point."
    full-leader-line-length nil)
   
   ("Number. Indicates the default length of the outside-leaders as a multiple of arrowhead-length.
     Defaults to 3."
    outside-leaders-length-factor 3)
   
   ("Number. Indicates the length of the second leader for the case when <tt>outside-leaders?</tt> is non-NIL"
    leader-line-length-2 (the leader-line-length))
   
   ("Number. Length of the witness lines (or of the shorter witness line in case they are different lengths)"
    witness-line-length 1)
   
   ("Keyword Symbol. Style for arrowhead on end of <tt>leader-line</tt>. Currently supported values
are <tt>:none</tt>, <tt>:wedge</tt>  (the Default), and <tt>:double-wedge</tt>."
    arrowhead-style :wedge)
   
   ("Keyword Symbol. Style for arrowhead on end of <tt>leader-line</tt>. Currently supported values
are <tt>:none</tt> (the Default), <tt>:wedge</tt>, and <tt>:double-wedge</tt>."
    arrowhead-style-2 (the arrowhead-style))
   
   ("Width of arrowhead glyph. Defaults to half the character-size."
    arrowhead-width (half (the character-size)))
   
   ("Length (from tip to tail) of arrowhead glyph. Defaults to twice the <tt>arrowhead-width</tt>"
    arrowhead-length (* (the arrowhead-width) 2))
   
   ("String naming a standard PDF font. Font for the label text. Defaults to \"Helvetica\""
    font "Helvetica")
   
   ("Number. Size (glyph height) of the label text, in model units. Defaults to 1."
    character-size 1)
   
   ("GDL object or NIL. View object which will use this dimension. Defaults to NIL."
    view-reference-object nil)
   
   
   ("String. Determines the text which shows up as the dimension label. Defaults to the dim-value, 
which is computed specially in each specific dimension type."
    dim-text (format nil "~3,1,,,'0f" (the dim-value)))
   
   ("3D Vector (normally only 2D are used). 
The dim-text-start is offset by this vector, in model space. Defaults to #(0.0 0.0 0.0)" 
    dim-text-start-offset (make-vector 0 0 0))
   
   ("3D Point. Determines where the text will start. Defaults to halfway between start-point 
and end-point."
    dim-text-start (translate-along-vector (the start-point)
                                           (subtract-vectors (the end-point) (the start-point))
                                           (half (3d-distance (the start-point) (the end-point)))))
                                       
   
   ("Keyword symbol, :start, :end, or :center. Indicates where to position the text in the case when
     <b>outside-leaders?</b> is non-nil. Defaults to :center"
    dim-text-bias :center)
   
   
   (dim-text-above-gap-factor 1.45)
   
   
   (orientation (when (the view-reference-object) (the view-reference-object orientation)))
   
   )
  
  :computed-slots
  (   
   
   
   (%corners% (list (the start-point) (the end-point)))
   
   
   (safe-distance (+ (3d-distance (the start-point) (the center))
                     (3d-distance (the end-point) (the center))))
   
   (safe-center (translate-along-vector (the center)
                                        (reverse-vector (the base-plane-normal))
                                        (the safe-distance)))
                     
   
   (start-base-distance (let ((intersect (inter-line-plane (the start-point)
                                                           (the base-plane-normal)
                                                           (the safe-center)
                                                           (the base-plane-normal))))
                          (3d-distance (the start-point) intersect)))
   
   (end-base-distance (let ((intersect (inter-line-plane (the end-point)
                                                         (the base-plane-normal)
                                                         (the safe-center)
                                                         (the base-plane-normal))))
                        (3d-distance (the end-point) intersect)))

   
   (witness-line-overages (let ((difference (- (the start-base-distance)
                                               (the end-base-distance))))
                            (if (the flip-leaders?)
                                (if (plusp difference) 
                                    (list :1 difference  :2 0) 
                                  (list :1 0  :2 (- difference)))
                              (if (minusp difference) (list :1 (- difference) :2 0) 
                                (list :1 0 :2 difference)))))


   #+nil
   (witness-line-overages (let ((difference (- (the start-base-distance)
                                               (the end-base-distance))))
                            (if (the flip-leaders?)
                                (if (plusp difference) 
                                    (list :1 0  :2 (- difference)) 
                                  (list :1 (- difference) :2 0))
                              (if (minusp difference) (list :1 (- difference) :2 0) 
                                (list :1 0 :2 difference)))))
   
   (witness-line-length-1 (+ (the witness-line-length) (getf (the witness-line-overages) :1)))
   (witness-line-length-2 (+ (the witness-line-length) (getf (the witness-line-overages) :2)))
   
   (start-end-swapped? (let ((sample-point (translate (the start-point)
                                                       (the sample-point-direction) 
                                                       (3d-distance (the end-point) (the start-point)))))
                          (not (in-halfspace? (the start-point)
                                              (subtract-vectors (the end-point) (the start-point))
                                              sample-point (the end-point)))))
   
   
   (leader-start (translate-along-vector (the start-point) 
                                         (the witness-direction-vector)
                                         (+ (the witness-line-length-1)
                                            (the witness-line-gap))))
   
   (leader-end (let ((nominal (translate-along-vector (the end-point)
                                                      (the witness-direction-vector)
                                                      (+ (the witness-line-length-2)
                                                         (the witness-line-gap)))))
                 (if (and (not (the outside-leaders?)) (the full-leader-line-length))
                     (translate-along-vector (the leader-start)
                                             (subtract-vectors nominal (the leader-start))
                                             (the full-leader-line-length))
                   nominal)))
   

   (full-leader-break-points 
    (when *break-leaders?*
      (let ((points (list (first (the dimension-text
                                   (line-intersection-points (first (the full-leader path-points))
                                                             (the full-leader leader-vector))))
                          (first (the dimension-text
                                   (line-intersection-points (lastcar (the full-leader path-points))
                                                             (reverse-vector
                                                              (the full-leader leader-vector))))))))
        (when (and (first points) (second points)
                   (between? (first points) (first (the full-leader path-points))
                             (lastcar (the full-leader path-points)))
                   (between? (second points) (first (the full-leader path-points))
                             (lastcar (the full-leader path-points)))) 
                                  
          (list (translate-along-vector (first points)
                                        (reverse-vector (the full-leader leader-vector))
                                        (the leader-text-gap))
                (translate-along-vector (second points)
                                        (the full-leader leader-vector)
                                        (the leader-text-gap))))))))

  
  :hidden-objects
  (
   ;;(start-cross :type 'point :center (the start-point) :display-controls (list :color :red))
   
   ;;(end-cross :type 'point :center (the end-point) :display-controls (list :color :green))
   
   (full-leader :type (if (not (the outside-leaders?)) 'leader-line 'null-part)
                :path-points (list (the leader-start) (the leader-end))
                
                :break-points (the full-leader-break-points)
                

                                        
                :pass-down (arrowhead-style arrowhead-style-2 arrowhead-length arrowhead-width))
   
   
   (leader-1 :type (if (and (the outside-leaders?) (the leader-1?)) 'leader-line 'null-part)
             :path-points (list (the leader-start)
                                (translate-along-vector (the leader-start)
                                                        (the leader-direction-1-vector) 
                                                        (the leader-line-length)))
             :pass-down (arrowhead-style arrowhead-length arrowhead-width)
             :arrowhead-style-2 :none)
   
   (leader-2 :type (if (and (the outside-leaders?) (the leader-2?)) 'leader-line 'null-part)
             :path-points (list (the leader-end)
                                (translate-along-vector (the leader-end)
                                                        (the leader-direction-2-vector) 
                                                        (the leader-line-length-2)))
             :pass-down (arrowhead-style arrowhead-length arrowhead-width)
             :arrowhead-style-2 :none)
      
   
   (witness-line-1 :type  (if (the witness-line?) 'line 'null-part)
                   :start (translate-along-vector (the start-point) 
                                                  (the witness-direction-vector) (the witness-line-gap))
                   :end   (translate-along-vector (the-child start)
                                                  (the witness-direction-vector)
                                                  (+ (the witness-line-length-1) 
                                                     (the witness-line-ext))))
   
   (witness-line-2 :type  (if (the witness-line-2?) 'line 'null-part)
                   :start (translate-along-vector (the end-point) (the witness-direction-vector) 
                                                  (the witness-line-gap))
                   :end   (translate-along-vector (the-child start)
                                                  (the witness-direction-vector) 
                                                  (+ (the witness-line-length-2) 
                                                     (the witness-line-ext))))

   (dimension-text :type 'general-note
                   :start (add-vectors (the dim-text-start) (the dim-text-start-offset))
                   :strings (ensure-list (the dim-text))
                   :orientation (if (the text-along-axis?)
                                    (alignment :top (the (face-normal-vector :top))
                                               :right (subtract-vectors (the leader-end)
                                                                       (the leader-start)))
                                  (the orientation))
                   :pass-down (font character-size text-x-scale justification outline-shape-type underline?
                                    dxf-text-x-scale dxf-size-ratio dxf-font dxf-offset
 				    onclick-function))))

