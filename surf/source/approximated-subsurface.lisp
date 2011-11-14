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


(define-object approximated-subsurface (surface)
  
  :input-slots
  (surface-in
   curve-top
   curve-bottom
   curve-left
   curve-right
   
   (curve-top-uv (the %curve-top-dropped% uv-curve))
   (curve-bottom-uv (the %curve-bottom-dropped% uv-curve))
   (curve-left-uv (the %curve-left-dropped% uv-curve))
   (curve-right-uv (the %curve-right-dropped% uv-curve))
   
   (make-left-right-compatible? nil)
   (make-top-bottom-compatible? nil)
   )
  
  :computed-slots
  ((native-surface (make-approximated-subsurface *geometry-kernel*
                                                 :surface (the surface-in)
                                                 :curve-top (the %curve-top-dropped%)
                                                 :curve-bottom (the %curve-bottom-dropped%)
                                                 :curve-left (the %curve-left-dropped%)
                                                 :curve-right (the %curve-right-dropped%)
                                                 :curve-top-uv (the curve-top-uv)
                                                 :curve-bottom-uv (the curve-bottom-uv)
                                                 :curve-left-uv (the curve-left-uv)
                                                 :curve-right-uv (the curve-right-uv))))
  
  :objects
  ((compatible-top-bottom :type (if (the make-top-bottom-compatible?)
                                    'compatible-curves 
                                  'sequenced-curves)
                          :curve-list (list (the curve-top)
                                            (the curve-bottom)))
   
   (compatible-left-right :type (if (the make-left-right-compatible?)
                                    'compatible-curves 
                                  'sequenced-curves)
                          :curve-list (list (the curve-left)
                                            (the curve-right)))
   
   
   
   (%curve-top-dropped% :type 'dropped-curve
                        :curve-in (the compatible-top-bottom (curves 0))
                        :surface (the surface-in))
   
   (%curve-bottom-dropped% :type 'dropped-curve
                           :curve-in (the compatible-top-bottom (curves 1))
                           :surface (the surface-in))

   
   (%curve-left-dropped% :type 'dropped-curve
                         :curve-in (the compatible-left-right (curves 0))
                         :surface (the surface-in))
   
   (%curve-right-dropped% :type 'dropped-curve
                          :curve-in (the compatible-left-right (curves 1))
                          :surface (the surface-in))
   

   
   ))
   

