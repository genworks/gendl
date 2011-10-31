        (defun vrml-spline-surface (surf-in)
          (let ((vDimension (length (the surf-in control-points)))       
                (uDimension (length (first (the surf-in  control-points))))     
                (uKnot (mapcar 'float (the surf-in u-knot-vector)))     
                (vKnot (mapcar 'float (the surf-in v-knot-vector)))        
                (uOrder (+ 1 (the surf-in u-degree)))       
                (vOrder (+ 1 (the surf-in v-degree))) 
                (controlPoint (flatten (the surf-in control-points))) 
                (weight (flatten (the surf-in weights))))
            (with-open-file (out "d:/test.wrl"
                             :direction :output
                             :if-exists :new-version
                             :if-does-not-exist :create)
              (format out "#VRML V2.0 utf8
WorldInfo {}
Background {skyColor [1.0 1.0 1.0]}
Transform {
 children
 [Shape 
 {geometry
NurbsSurface { ~&")
              (format out " uDimension ~a ~&" (the uDimension)) 
              (format out " vDimension ~a ~&" (the vDimension)) 
              (format out " uKnot [")        
              (dolist (point (the uKnot))
                (format out " ~,6f," point))
              (format out " ] ~&")
              (format out " vKnot [")        
              (dolist (point (the vKnot))
                (format out " ~,6f," point))
              (format out " ] ~&")
              (format out " uOrder ~a ~&" (the uOrder)) 
              (format out " vOrder ~a ~&" (the vOrder)) 
              (format out " controlPoint [ ")
              (dolist (point (the controlPoint))
                (format out " ~,6f ~,6f ~,6f," (get-x point) 
                        (get-y point) (get-z point)))      
              (format out " ]~&")
              (format out " weight [")               
              (dolist (point (the  weight))
                (format out " ~,6f," point))
              (format out " ]~&")
              (format out " uTessellation    100 ~& vTessellation    100 ~& ") 
              (format out "solid FALSE } 
appearance Appearance {
material Material {diffuseColor .3000000 .8000000 .0000000}}}]}"))))
