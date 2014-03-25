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

(defparameter *general-note-center-circ* nil)

;;
;;
;; Contributed by Liberating Insight, www.li.com, modified by Genworks.
;;
;;

(define-object general-note (outline-specialization-mixin base-object)
   :documentation (:description "Creates a text note in the graphical view port and in a PDF DXF output file." 
                   :examples "<pre> 
 (define-object general-note-test (base-object)
  
  :computed-slots
  
  ((blocks-note 
   (list
    \"David Brown\" \"Created by\" \"ABC 2\"
    \"Jane Smith\" \"Approved by\" \"CCD 2\"))
   (blocks-center 
    (list '(-15  5 0) '(-40  5 0) '(-55  5 0)
          '(-15 15 0) '(-40 15 0) '(-55 15 0)))
   (blocks-width (list 30 20 10 30 20 10)))
  
  :objects 
  
  ((title-block :type 'box
                :sequence (:size (length (the blocks-center)))
                :display-controls (list :color :red)
                :center (apply-make-point 
                         (nth (the-child index ) 
                              (the blocks-center)))
                :length 10
                :width (nth (the-child index ) 
                            (the blocks-width))
                :height 0)

   (general-note-sample :type 'general-note
                        :sequence (:size (length (the blocks-note)))
                        :center (the (title-block 
                                      (the-child index)) center)
                        :character-size 2.5
                        :strings (nth (the-child index) 
                                      (the blocks-note)))))
  (generate-sample-drawing 
  :objects (list-elements (make-object 'general-note-test)) 
  :projection-direction (getf *standard-views* :top))

</pre>")
  
  :input-slots
  (("List of Strings. The text to be displayed in the note."
    strings nil)
   
   ("Number. Specifies the character size in drawing units."
    character-size 1)
   
   ("Number in Percentage. Adjusts the character width for PDF output. Defaults to 100."
    text-x-scale 100)
   
   ("Number in Percentage. Adjusts the character width for DXF output. Defaults to the text-x-scale."
    dxf-text-x-scale (the text-x-scale))
   
   ("Number. The scale factor for DXF character size vs PDF character size. Default is 0.8"
    dxf-size-ratio 0.8)
   
   ("String. This names the DXF font for this general-note. Defaults to <tt>(the font)</tt>."
    dxf-font (the font))
   
   ("Number. The start of text will be offset by this amount for DXF output. Default is 0."
    dxf-offset 0)
   
   
   ("Keyword symbol. Currently can be :bubble, :rectangle, or :none. Default is :none."
    outline-shape-type :none)
   
   ("String. The font for PDF. Possibilities for built-in PDF fonts are:

<ul>
<li>courier</li>
<li>courier-bold</li>
<li>courier-boldoblique</li>
<li>courier-oblique</li>
<li>helvetica</li>
<li>helvetica-bold</li>
<li>helvetica-boldoblique</li>
<li>helvetica-oblique</li>
<li>symbol</li>
<li>times-roman</li>
<li>times-bold</li>
<li>times-bolditalic</li>
<li>times-italic</li>
<li>zapfdingbats</li>
</ul>

Defaults to \"Courier\"." font "Courier")
   

   
   
   ("3D-point. Center of the text. Specify this or start, not both. 
NOTE that the center is no longer defaulting (so that it can self-compute properly when start 
is specified), so it is necessary to explicitly give either start or center for general-note."
    center (if *general-note-center-circ*
               (the parent center)
             (translate (the start) 
                        :right (half (the width))
                        :front (- (half (the length)) 
                                  (- (the character-size)
                                     (the inter-line-space))))))
   
   ("3D-point. Start of the text. Specify this or center, not both."
    start (let ((*general-note-center-circ* t))
            (translate (the center) 
                       :left (half (the width))
                       :rear (- (half (the length)) 
                                (- (the character-size)
                                   (the inter-line-space))))))
   
   ("Keyword symbol, :left, :right, or :center. Justifies text with its box. Default is :left."
    justification :left)
   
   ("Number. Space between lines of text. Default is 1.2 times the character size." 
    leading (* (the character-size) 1.2))
   
   ("Boolean. Determines whether text is underlined."
    underline? nil)
   
   ("Number. Determines the width of the containing box. Default is the maximum-text-width." 
    width (the maximum-text-width))
   
   (length (- (* (the text-lines number-of-elements) (the leading))
              (- (the leading) (the character-size))))
   
   (height 1)
   
   (%corners% (unless (the underline?)
                (list (the (vertex :front :left :bottom))
                      (the (vertex :top :right :rear))))))

  
  :computed-slots
  ((string-list (if (listp (the strings)) (the strings) (list (the strings))))
   
   (inter-line-space (- (the leading) (the character-size)))
   
   ("Number. Convienence computation giving the maximum input width required to keep one line per string" 
    maximum-text-width (first (sort (mapsend (list-elements (the text-lines)) :text-width) #'>)))

   (outline-objects (append (list-elements (the text-lines))
                            (remove-if #'(lambda(obj) (typep obj 'null-part)) 
                                       (list-elements (the text-lines) 
                                                      (the-element underline)))
                            (list (the outline-shape))))

   (%lines-to-draw% (apply #'append (list-elements (the text-lines)
                                                   (the-element %lines-to-draw%)))))

  
  :hidden-objects
  ((outline-shape :type (ecase (or (the outline-shape-type) :none)
                          (:none 'null-part)
                          (:bubble 'global-filleted-polyline)
                          (:rectangle 'global-polyline))

                  
                  
                  :expansion (ecase (or (the outline-shape-type) :none)
                               ((:none :rectangle) (* (the width) .1))
                               (:bubble (the-child default-radius)))

                  
                  :vertex-list (list (translate (the (vertex :left :rear :top))
                                                :left (the-child expansion)
                                                :rear (* (the character-size) .3))

                                     (translate (the (vertex :right :rear :top))
                                                :right (the-child expansion)
                                                :rear (* (the character-size) .3))
                                     (translate (the (vertex :right :front :top))
                                                :right (the-child expansion))
                                     (translate (the (vertex :left :front :top))
                                                :left (the-child expansion))
                                     (translate (the (vertex :left :rear :top))
                                                :left (the-child expansion)
                                                :rear (* (the character-size) .3)))
                  :default-radius (half (the length)))
                                     
   
   (text-lines :type 'text-line
               :sequence (:size (length (the string-list)))
               :text (nth (the-child index) (the string-list))
               :pass-down (character-size text-x-scale font underline? width
                                          dxf-text-x-scale dxf-font dxf-offset dxf-size-ratio
                                          )
               :start (translate (the start) 
                                 :front (* (the-child index)(the leading))
                                 :right (ecase (the justification)
                                          (:left 0)
                                          (:center (the-child center-justify-space))
                                          (:right (the-child right-justify-space)))))))


(defparameter *text-line-center-circ* nil)

(define-object text-line (;;outline-specialization-mixin 
                          
                          base-object)

  :documentation (:description "Outputs a single line of text for graphical display.")
  :input-slots
  (text (underline? nil)
   (text-x-scale 100)
   dxf-text-x-scale dxf-font dxf-offset dxf-size-ratio
   (width (the text-width))
  
   ("3D-point. Center of the text. Specify this or start, not both."
    center (if *text-line-center-circ*
               (the parent center)
             (translate (the start) 
                        :right (half (the text-width))
                        :rear (half (the character-size)))))
   
   ("3D-point. Start of the text. Specify this or center, not both."
    start (let ((*text-line-center-circ* t))
            (translate (the center) 
                       :left (half (the text-width))
                       :front (half (the character-size)))))
   
   (corner (the start)) 
   
   character-size 
   
   font)
  
  :computed-slots
  ((length (the character-size))
   (text-width (* (/ 100)
                  (the text-x-scale)
                  (pdf::text-width (the text) (pdf:get-font (the font)) (the character-size))))
   (right-justify-space (- (the width) (the text-width)))
   (center-justify-space (half (the right-justify-space)))
   
   (%text-to-draw% (the text))
      
   ;;(%lines-to-draw% (when (the underline?) (the underline %lines-to-draw%)))
   
   ;;(outline-objects (list (the underline)))
   
   (%corners% (list (the (vertex :front :left :bottom))
                    (the (vertex :top :right :rear)))))
  :objects
  ((underline  :type (if (the underline?) 'line 'null-part)
               :start (the start)
               :end (translate (the start) :right (the text-width)))))

     
;;
;; FLAG -- update for auto-scaling outside base-view
;;
(define-lens (pdf text-line)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((view-scale (if view (the-object view view-scale-total) 1))
            (corner (if view (the-object view (view-point (the corner))) (the corner))))
        (let ((font-size (* (the character-size) view-scale))
              (rotation (angle-between-vectors-d 
                         (if view 
                             (matrix*vector (the-object view view-transform-inverse) +rear-vector+ )
                           +rear-vector+)
                         (the (face-normal-vector :rear)) +top-vector+)))
          (with-translated-state (:pdf corner)
            (write-the rgb-stroke-setting)
            (unless (zerop rotation) (pdf:rotate rotation))
            (pdf:in-text-mode (pdf:set-font (pdf:get-font (the font)) font-size)
                              (pdf:set-text-x-scale (the text-x-scale))
                              (pdf:draw-text (the %text-to-draw%))))))))))
      
          

(define-lens (dxf text-line)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((view-scale (if view (the-object view view-scale-total) 1))
            (corner (if view (the-object view (view-point (the corner))) (the corner))))
        (let ((corner (add-vectors *dxf-translation* corner))
              (font-size (* (the character-size) view-scale))
              (rotation (if view
                            (angle-between-vectors-d
                             (matrix*vector (the-object view view-transform-inverse) +rear-vector+ )
                             (the (face-normal-vector :rear)) +top-vector+)
                            0)))
          (with-translated-state (:dxf corner)

            (format *stream* "  0
TEXT
 10
~3,15f
 20
~3,15f
 40
~3,15f
  1
~a~a
 41
~3,15f
  7
~a
"

                    
                    (+ (get-x corner) (the dxf-offset)) (get-y corner) (* (the dxf-size-ratio) font-size)
                    (the %text-to-draw%)
                    (if (and rotation (not (zerop rotation)))
                        (format nil "~% 50~%~2,5f" rotation) "")
                    (div (the dxf-text-x-scale) 100)
                    (the dxf-font)
                    )
            
            (write-the rgb-stroke-setting)
            
            )))))))





