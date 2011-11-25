(in-package :genworks.com)


(define-object options (base-site-sheet)
  
  :input-slots (pricing-footer)
  
  
  :computed-slots ((body-class "products options columns_two"))
  
  :objects
  (
   (column-left 
    :type 'sheet-section
    :main-view nil)

   (column-right 
    :type 'sheet-section
    :main-view nil)

   
   (column-center :type 'sheet-section
		  :main-view 
		  (with-cl-who-string ()
		    ((:div :class "content") 
		     (:h2 "GDL Product Options")
		     ((:div :class "column row_bottom") 
		      (:h5 "NURBS Surfaces and Solids Geometry Kernel")
		      (:p
		       "As an optional module, both the Professional and Enterprise Edition come with an integrated NURBS 
surfaces and Solids modeling kernel, based on the SMLib product from Solid Modeling Solutions, Inc. The combination of 
GDL and SMLib adds powerful surface and solids capabilities to GDL's built-in 3D wireframe facilities. SMLib provides 
extensible filleting, as well as full support for non-manifold topology (e.g. edges sharing more than two faces) 
for boundary-representation solids. Along with the dynamic modeling capability, the SMLib option also provides input 
and output support for the standard IGES and STEP formats as well as the possibility of direct CAD translation."))
		     ((:div :class "column row_bottom") 
		      (:h5 "Standardized Process Planning Architecture")
		      (:p
		       "SPPA is a generalized framework for modeling the breakdown from a finished product to the 
purchased parts and raw materials making it up. SPPA can be used to model both geometric 
and non-geometric products.")
		      (:p
		       "Genworks offers SPPA as an add-on module in source-code form, licensed on a site wide basis 
and with no extra charge for runtime deployment."))
		     ((:p :class "clear") "&nbsp;")
		     )))))
		     
