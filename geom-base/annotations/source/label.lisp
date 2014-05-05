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

(define-object label (outline-specialization-mixin base-object)
  :documentation (:description 
		  "Produces a text label for graphical output"
		  :examples "<pre>        

 (in-package :gdl-user)
                   
 (define-object  label-sample (base-object)
  
   :objects
   ((box :type 'box
         :length 10 :width (* (the-child length) +phi+)
         :height (* (the-child :width) +phi+))
   
    (corner-label :type 'label
                  :leader-path (let ((start (the box (vertex :top :right :rear))))
                                (list start
                                      (translate start :right (/ (the box width) 10)
                                                       :rear (/ (the box width) 10))
                                      (translate start :right (/ (the box width) 7)
                                                       :rear (/ (the box width) 10))))
                  :text \"The Corner\"
                  :character-size (/ (the box width) 15))))


 (generate-sample-drawing :object-roots (make-object 'label-sample))

</pre>")

  
  :input-slots
  ("List of 3D Points. List making up leader line, starting from where the arrowhead normally is."
   leader-path
   
   ("Width of arrowhead glyph. Defaults to five times the line thickness (2.5)"
    arrowhead-width (half (the character-size)))
   
   ("Length (from tip to tail) of arrowhead glyph. Defaults to twice the <tt>arrowhead-width</tt>"
    arrowhead-length (* (the arrowhead-width) 2))
   
   ("Keyword Symbol. Style for arrowhead at start of <tt>leader-path</tt>. Currently supported values
are <tt>:none</tt>, <tt>:wedge</tt>  (the Default), and <tt>:double-wedge</tt>."
    arrowhead-style :wedge)
   
   ("Keyword Symbol. Style for arrowhead on end of <tt>leader-path</tt>. Currently supported values
are <tt>:none</tt> (the Default), <tt>:wedge</tt>, and <tt>:double-wedge</tt>."
    arrowhead-style-2 :none)
   
   ("String. Text to be displayed as the label"
    text "")
   
   ("List of strings. Text lines to be displayed as the label. Specify this or text, not both."
    strings nil)
   
   ("String naming a standard PDF font. Font for the label text. Defaults to \"Helvetica\""
    font "Helvetica")
   
   ("Number. Size (glyph height) of the label text, in model units. Defaults to 10."
    character-size 10)
   
   ("Number. Amount of space between last point in leader-path and beginning of the label text. Defaults to the 
width of the letter \"A\" in the specified <tt>font</tt> and <tt>character-size</tt>."
    text-gap (pdf::text-width "A" (pdf:get-font (the font)) (the character-size)))
   
   ("Keyword Symbol, either <tt>:left</tt> or <tt>:right</tt>. Determines whether the label text sits to
the right or the left of the last point in the <tt>leader-path</tt>. The default is computed based on
the direction of the last segment of the leader-path."
    text-side (let ((test-point (translate (first (the leader-path))
                                           :right
                                           (3d-distance (first (the leader-path))
                                                        (lastcar (the leader-path))))))
                (if (> (3d-distance test-point (first (the leader-path)))
                       (3d-distance (lastcar (the leader-path)) (first (the leader-path))))
                    :left :right)))
   
   ("Keyword Symbol. Indicates shape of outline enclosing the text. Currently <tt>:none</tt>, <tt>:bubble</tt>, <tt>:rectangle</tt>, 
and <tt>nil</tt> are supported. The default is nil"
    outline-shape-type nil)
   
   
   ("GDL object or NIL. View object which will use this dimension. Defaults to NIL."
    view-reference-object nil)
   
   (text-x-scale 100)
   
   ("Number in Percentage. Adjusts the character width for DXF output. Defaults to the text-x-scale."
    dxf-text-x-scale (the text-x-scale))
   
   ("Number. The scale factor for DXF character size vs PDF character size. Default is 0.8"
    dxf-size-ratio 0.8)
   
   ("String. This names the DXF font for this general-note. Defaults to <tt>(the font)</tt>."
    dxf-font (the font))
   
   ("Number. The start of text will be offset by this amount for DXF output. Default is 2."
    dxf-offset 3)
   
   
   (%vertex-array% (coerce (append (the leader-path)
                                   (coerce (the note %corners%) 'list)) 'vector)))
  
  :computed-slots 
  ((orientation (when (the view-reference-object) (the view-reference-object orientation))))
  
  :hidden-objects
  ((leader :type (if (the leader-path) 'leader-line 'null-part)
           :path-points (the leader-path)
           :pass-down (arrowhead-width arrowhead-length arrowhead-style arrowhead-style-2))
   
   (note :type 'general-note
         :start (ecase (the text-side)
                  (:right (translate (lastcar (the leader-path)) :right (the text-gap)))
                  (:left (translate (lastcar (the leader-path)) :left (+ (the text-gap) (the-child width)))))
         :strings (or (ensure-list (the strings)) (ensure-list (the text)))
         :pass-down (onclick-function font outline-shape-type character-size dxf-text-x-scale dxf-size-ratio dxf-font dxf-offset))))

  
  
   
