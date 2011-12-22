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


(in-package :gdl-lift-tests)


(define-object silhouette-curves-test (base-object)

  :computed-slots ((section-tolerance 0.1)
		   (fit-tolerance 0.1))

  
  :objects
  (
   ;;(surface :type 'test-b-spline-surface)
   
   ;;(surface :type 'planar-surface-test)
   
   (surface :type 'spherical-surface
	    :display-controls (list :isos (list :n-u 2 :n-v 2))
	    :radius 10)
   

   (top-silhouette :type 'silhouette-curves
		   :surface (the surface)
		   :eye-point-or-vector (the surface (face-normal-vector :front)))

   
   ))
