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

(in-package :surf)


(define-lens (nurbs vanilla-mixin)()
  :output-functions
  ((cad-output () (push self (format-slot objects)))
   
   (nurbs-reps 
    ()
    (warn "No NURBS representation for ~s, apparently of type ~s" self (the type)))))


(define-lens (nurbs line)()
  :output-functions
  ((nurbs-reps
    ()
    (make-object 'linear-curve :start (the start) :end (the end)))))


(define-lens (nurbs arc) ()
  :output-functions
  ((nurbs-reps
    ()
    (make-object 'arc-curve :radius (the radius) 
                 :start-angle (the start-angle-normalized) :end-angle (the end-angle-normalized)
                 :center (the center) :orientation (the orientation)))))

(define-lens (nurbs sphere) ()
  :output-functions
  ((nurbs-reps
    ()
    ;;
    ;; FLAG -- add support for spherical patches.
    ;;
    (make-object 'spherical-surface :center (the center) :radius (the radius)))))


(define-lens (nurbs global-filleted-polyline)()
  :output-functions
  ((nurbs-reps
    ()
    (append (mapcar #'(lambda(straight)
                        (destructuring-bind (start end) straight
                          (make-object 'linear-curve :start start :end end)))
                    (the straights))
            (mapcar #'(lambda(arc)
                        (make-object 'arc-curve :radius (the-object arc radius) 
                                     :start-angle (the-object arc start-angle-normalized) 
                                     :end-angle (the-object arc end-angle-normalized)
                                     :center (the-object arc center) :orientation (the-object arc orientation)))
                    (list-elements (the fillets)))))))

(define-lens (nurbs global-polyline)()
  :output-functions
  ((nurbs-reps
    ()
    (mapcar #'(lambda(line)
                (destructuring-bind (start end) line
                  (make-object 'linear-curve :start start :end end)))
            (the lines)))))


(define-lens (nurbs cylinder)()
  
  :output-functions
  ((nurbs-reps 
    ()
    (append (mapcar #'(lambda(arc)
                        (make-object 'arc-curve :radius (the-object arc radius) 
                                     :start-angle (the-object arc start-angle-normalized) 
                                     :end-angle (the-object arc end-angle-normalized)
                                     :center (the-object arc center) :orientation (the-object arc orientation)))
                    (list-elements (the end-arcs)))
            
            (mapcar #'(lambda(line) 
                        (destructuring-bind (start end) line
                          (make-object 'linear-curve :start start :end end)))
                    (the %lines-to-draw%))
            
            (unless (typep (the inner-cylinder) 'null-part)
              (write-the inner-cylinder nurbs-reps))))))
            


(define-lens (nurbs global-polygon-projection)()
  :output-functions
  ((nurbs-reps
    ()
    (append (write-the polygon-original nurbs-reps)
            (write-the polygon-1 nurbs-reps)
            (when (typep (the :polygon-2) (the :polygon-type))
              (write-the polygon-2 nurbs-reps))
            (mapcar #'(lambda(line)
                        (make-object 'linear-curve :start (first line) :end (second line)))
                    (the projection-lines))))))











