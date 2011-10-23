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


(define-object graph (base-object)
  :input-slots
  (x-values y-values (colors (list :red :orange :yellow :green :indigo :violet)))
  
  :computed-slots
  ((min-x (apply #'min (flatten (the :x-values))))
   (max-x (apply #'max (flatten (the :x-values))))
   
   (x-range (- (the :max-x) (the :min-x)))
   
   (min-y (apply #'min (flatten (the :y-values))))
   (max-y (apply #'max (flatten (the :y-values))))
   
   (y-range (- (the :max-y) (the :min-y))))

  
  :objects
  ((x-axis :type 'line
           :start (make-point (- (the :min-x) (* (the :x-range) 0.05)) 0 0)
           :end   (make-point (+ (the :max-x) (* (the :x-range) 0.05)) 0 0))
   
   (y-axis :type 'line
           :start (make-point 0 (- (the :min-y) (* (the :y-range) 0.05)) 0)
           :end (make-point 0 (+ (the :max-y) (* (the :y-range) 0.05)) 0))
   
   (value-lines :type 'global-polyline
                :sequence (:size (length (the :x-values)))
                :display-controls (list :color (nth (the-child :index) (the :colors))
                                        :line-thickness 5)
                :vertex-list (mapcar #'(lambda(x y)
                                         (make-point x y 0)) 
                                     (nth (the-child :index) (the :x-values))
                                     (nth (the-child :index) (the :y-values))))))





(define-object test-graph (graph)
  :computed-slots
  ((x-values (list (list 25 35 70 108 125)))
   (y-values (list (list 3 5 7 9 12)))))
