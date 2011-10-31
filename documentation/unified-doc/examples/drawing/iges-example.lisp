(in-package :gdl-user)

(define-object iges-example (base-object)
  
  :computed-slots
  ((points (list
            (list (make-point 3.5 1 0)(make-point 9 6 0.2)(make-point 6 10 0.2))
            (list (make-point 4.5 1 0)(make-point 9 6 -0.2)(make-point 6 10 -0.2)))))
  :objects 
  
  ((vase :type 'vrml-example
         :hidden? t)
   
   (handle-curve :type 'fitted-curve
                 :hidden? t
                 :sequence (:size 2)
                 :points (nth (the-child index)(the points)))
   
   (handle :type 'ruled-surface
           :hidden? t
           :curve-1 (first (list-elements (the handle-curve)))
           :curve-2 (second (list-elements (the handle-curve))))
   
   (handle-surf :type 'separated-solid 
                :hidden? t
                :other-brep (the vase vase (surfaces 0) brep)
                :brep (the handle brep))
   
   (h-vase :type 'united-solid
           :other-brep (the vase vase (surfaces 0) brep)
           :brep (the handle-surf (breps 1)))))

 
