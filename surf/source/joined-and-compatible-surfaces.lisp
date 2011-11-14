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

;;-------------------------------------------------------------------------

(define-object joined-surfaces (surface)
  :documentation (:description "This  routine joins two surfaces at a common boundary. The surfaces must already be compatible in the direction of the common boundary (same knots). If the surfaces are not compatible you can use first compatible-surfaces if applicable" 
                  :examples "<pre>

 (in-package :gdl-surf-user)

 (define-object join-surfaces-test (base-object) 

   :computed-slots ((surface-list (list (the surf-A) (the surf-B))))
  
   :objects
   ((surf-A :type 'rectangular-surface
            :display-controls (list :color :green-spring-medium)
            :length 10
            :width 10 )
                                        
    (surf-B :type 'rectangular-surface
            :display-controls (list :color :red)
            :center (make-point 10 0 0 )
            :length 10
            :width 10 )
   
    (join-A-and-B :type 'joined-surfaces
                  :display-controls (list :line-thickness 2)
                  :surface (the surf-A)
                  :other-surface (the surf-B))))


 (generate-sample-drawing :object-roots (list (the-object (make-object 'join-surfaces-test) 
                                               join-A-and-B)))
 

</pre>
")
  
  :input-slots ("Gdl surface object. The first surface to be joined. Its u-max or v-max lays at the common boundary." surface 
                "Gdl surface object. The second surface to be joined . Its u-min or v-min lays at the common boundary." other-surface 
                ("Number. This is a tolerance used for Knot removal.  The knot corresponding to the merged boundary has multiplicity equal to the degree. Knot removal will be attempted using this tolerance. Default is *3d-tolerance-default*" tolerance *3d-tolerance-default*) 
                ("Keyword symbol, one of :u or :v. If :u the common boundary is for first surface u-max and for the second surface u-min. Surfaces must already be compatible in the u-direction. If :v the common boundary is for first surface v-max and for the second surface v-min. Surfaces must already be compatible in the v-direction. Default is :u. " direction :u))
  
  :computed-slots ((native-surface (join-surfaces *geometry-kernel* (the surface) (the other-surface) 
                                                  :tolerance (the tolerance) 
                                                  :direction (the direction )))))


(define-object compatible-surfaces (base-object)
  :documentation (:examples "<pre>

 (in-package :gdl-surf-user)

 (define-object compatible-surfaces-test (surface) 

   :computed-slots ((surface-list (list (the surf-A) (the surf-B))))
  
   :objects
   ((make-compatible-A-and-B :type 'compatible-surfaces
                             :display-controls (list :line-thickness 2)
                             :surface-list (the surface-list))

    (surf-A :type 'rectangular-surface
            :display-controls (list :color :green-spring-medium)
            :length 10
            :width 10 )
                                        
    (surf-B :type 'rectangular-surface
            :display-controls (list :color :red)
            :center (make-point 10 0 0 )
            :length 10
            :width 10 )))

 (generate-sample-drawing :object-roots (list (the-object (make-object 'join-surfaces-test) 
                                                         join-A-and-B )))
 
</pre>
")
  :input-slots (surface-list)
  
  :computed-slots ((native-surfaces-list (return-compatible-surfaces *geometry-kernel* (the surface-list))))
  
  :objects ((surfaces :type 'surface
                      :sequence (:size (length (the native-surfaces-list)))
                      :native-surface (nth (the-child index) (the native-surfaces-list)))))






