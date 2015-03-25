(in-package :surf)

(defparameter *1588p005-doc* 
" Fix the order of the returned u,v point for the point function of uv-iso-curve.")


(#+allegro 
 excl:without-redefinition-warnings
 #-allegro progn

 (define-object uv-iso-curve ()
  
   :documentation (:description "Convenience object to return UV parameter values on surface.")

   :input-slots (surface parameter u-or-v)
  
   :functions
   (("2D point. The UV surface representation at the given parameter value.

:arguments (parameter \"Number. The parameter value you want.\")"
     point 
     (parameter)
     (ecase (the u-or-v)
       (:v (make-point (the parameter) parameter))
       (:u (make-point parameter (the parameter))))))))



