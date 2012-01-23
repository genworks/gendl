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

(define-object pie-chart (base-object)
  :documentation (:description "Generates a standard Pie Chart with colored filled pie sections.

This object was inspired by the pie-chart in Marc Battyani's (marc.battyani@fractalconcept.com)
cl-pdf, with contributions from Carlos Ungil (Carlos.Ungil@cern.ch)."
                  
                  :examples "

 <pre>

 (in-package :gdl-user)
 
 (define-object pie-sample (pie-chart)
   :computed-slots
   ((data (list 30 70))
   
    (labels&colors '((\"Expenses\" :red) (\"Revenue\" :green)))
   
    (width 200) 
   
    (title \"Cash Flow\")))

 (generate-sample-drawing :objects (make-object 'pie-sample))

 </pre>")

  :input-slots 
  (("String. Currently this must be a PDF font name. Defaults to \"Helvetica.\""
    title-font "Helvetica")
   
   ("List of Numbers. The relative size for each pie piece. These will be normalized to percentages. 
Defaults to NIL, must be specified as non-NIL to get a result."
    data nil) 
   
   ("List of lists, each containing a string and a keyword symbol. This list should be the same
length as <tt>data</tt>. These colors and labels will be assigned to each pie piece and to the legend.
Defaults to NIL, must be specified as non-NIL to get a result."
    labels&colors nil)
   
   ("Number. The radius of the pie. Defaults to 0.35 times the <tt>width</tt>."
    radius (half (* (the width) 0.7)))
   
   (background-color :white)
   
   ("String. Title for the chart. Defaults to the empty string."
    title "")
   
   ("Number. Size in points of title font. Defaults to 12."
    title-font-size 12)
   
   ("Keyword symbol naming color from <tt>*color-table*</tt>. 
Color of title text. Defaults to :black."
    title-color :black)
   
   ("Keyword symbol naming color from <tt>*color-table*</tt>. 
Color of the outline of the pie. Defaults to :black."
    line-color :black)
   
   ("Boolean. Determines whether the Legend is included in standard output formats. Defaults to <tt>t</tt>."
    include-legend? t)
   
   (title-width (twice (the radius))))
  
  :computed-slots
  ((data-sum (apply #'+ (the data)))
   (start-point (translate (the center) :right (the radius)))
   
   (%vertex-array% (append (coerce (the title-block %vertex-array%) 'list)
                           (when (the include-legend?) (coerce (the legend boundary :%vertex-array%) 'list))
                           (list (translate (the center) :left (the radius) :rear (the radius))
                                 (translate (the center) :right (the radius) :rear (the radius))
                                 (translate (the center) :left (the radius) :front (the radius))
                                 (translate (the center) :right (the radius) :front (the radius))))))
  
  :hidden-objects
  ((title-block :type (if (string-equal (the title) "") 'null-part 'general-note)
                :strings (the title) :font (the title-font) 
                :character-size (the title-font-size)
                :center (translate (the center) 
                                   :rear (+ (the radius) (the-child leading))
                                   ;;:left (half (the-child width))
                                   ))
   (legend :type 'legend
           :upper-left (translate (the center) :right (* (the radius) 1.1) :rear (the radius))
           :pass-down (labels&colors))
   
   (pie-pieces :type 'pie-piece
               :sequence (:size (length (the data)))
               :datum (nth (the-child index) (the data))
               :label (first (nth (the-child :index) (the labels&colors)))
               :display-controls (list :color (second (nth (the-child :index) (the labels&colors))))
               :start-angle (if (the-child first?) 0 (the-child previous end-angle))
               :angle (* 2pi (the-child fraction))
               :fraction (/ (the-child datum) (the data-sum))
               :pass-down (radius start-point line-color))))


(define-lens (pdf pie-chart)()
  :output-functions
  ((cad-output
    ()
    
    (when (or (not (numberp (the radius))) (zerop (the radius))) 
      (error "Pie Chart must have nonzero radius."))
    (write-the title-block (cad-output))
    (when (the include-legend?) (write-the legend (cad-output)))
    (mapc #'(lambda(pie-piece) (write-the-object pie-piece cad-output)) (list-elements (the pie-pieces))))))


(define-object pie-piece (base-object)
  :input-slots
  (datum label color start-angle angle percentage radius start-point)
  
  :computed-slots
  ((end-angle (+ (the start-angle) (the angle))))
  
  :hidden-objects ((arc :type 'arc
                        :pass-down (start-angle end-angle radius))))

;;
;; FLAG -- remove this and handle in base-view output of generic
;; curves (if possible). Or, do only the fill part here.
(define-lens (pdf pie-piece)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((center (if view (the-object view (view-point (the center))) (the center)))
            (curves (if view (mapcar #'(lambda(curve)
                                         (mapcar #'(lambda(point) (the-object view (view-point point))) curve))
                                     (the arc %curves-to-draw%)) (the arc %curves-to-draw%))))
        (write-the line-thickness-setting)
        (write-the rgb-stroke-setting)
        (apply #'pdf:set-rgb-fill (coerce (lookup-color (the color-decimal) :ground :background) 'list))
        (pdf:move-to (get-x center) (get-y center))
        (pdf:line-to (get-x (first (first curves))) (get-y (first (first curves))))
        (mapc #'(lambda(curve) (apply #'pdf:bezier-to 
                                      (mapcan #'(lambda(point) (list (get-x point) (get-y point))) 
                                              (rest curve)))) curves)
        (pdf:line-to (get-x center) (get-y center))
        (pdf:fill-and-stroke))))))





