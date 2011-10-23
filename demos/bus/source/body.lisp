;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :genworks.demos.bus)

(define-object body (application-mixin)

  :input-slots
  (wheelbase
   track
   firewall-base
   frame-width)

  :computed-slots
  ((ui-display-list-objects (the children))
   (width (+ (the :wheelbase) (the :front-overhang) (the :rear-overhang)))
   (rear-overhang 84 :settable)
   (front-overhang 35 :settable)
   (cab-width 54 :settable)
   (height 72 :settable)
   (length 96 :settable)
  
   (use-local-box? nil))
  

  :objects
  (
   (extruded-box 
    :type 'global-filleted-polygon-projection
    :radius-list (list 10 10 10 10 10 10 1 1)
    :projection-depth (the :reference-box :width)
    :projection-vector (the (:face-normal-vector :right))
    :vertex-list (list (the :reference-box (:vertex :left :rear :bottom))
		       (translate (the :reference-box (:vertex :left :rear :top))
				  :down 12)
		       (translate (the :reference-box (:vertex :left :rear :top))
				  :front 20 :down 4)
		       (translate (the :reference-box (:vertex :left :rear :top))
				  :front 45)
		       (translate (the :reference-box (:vertex :left :front :top))
				  :rear 45)
		       (translate (the :reference-box (:vertex :left :front :top))
				  :rear 20 :down 4)
		       (translate (the :reference-box (:vertex :left :front :top))
				  :down 12)
		       (the :reference-box (:vertex :left :front :bottom)))
    :display-controls (list :color :gold-bright :ambient-intensity 0.0 :shininess 0.7 :specular-color :orange :transparency 0.4))
   
   (extruded-cab 
    :type 'global-filleted-polygon-projection
    :radius-list (list 7 7 1 1)
    :projection-depth (the :cab-box :height)
    :projection-vector (the (:face-normal-vector :bottom))
    :vertex-list (list (the :cab-box (:vertex :top :rear :right))
		       (translate (the :cab-box (:vertex :top :rear :left)) :front
				  16)
		       (translate (the :cab-box (:vertex :top :front :left)) :rear
				  16)
		       (the :cab-box (:vertex :top :front :right)))
    :display-controls (list :color :gold-bright :ambient-intensity 0.0 :shininess 0.7 :specular-color :orange)))

  :hidden-objects
  ((reference-box :type 'box
		  :center (translate (the :firewall-base) :up (half (the :height)) 
				     :right (* (half (the-child :width)) 1.001))
		  :width (- (the :width) (the :cab-width))
		  :length (* 1.01 (the length))
		  :display-controls (list :color :gold-bright))
   (cab-box :type 'box
	    :center (translate (the :firewall-base) :up (half (the-child :height)) 
			       :left (half (the-child :width)))
	    :height (half (the :height))
	    :width (the :cab-width)
	    :display-controls (list :color :gold-bright))))
