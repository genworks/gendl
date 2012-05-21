;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :genworks.com)


(define-object options (base-site-sheet)
  
  :input-slots (pricing-footer)
  
  
  :computed-slots ((body-class "products options columns_two"))
  
  :objects
  (
   (column-left 
    :type 'sheet-section
    :inner-html nil)

   (column-right 
    :type 'sheet-section
    :inner-html nil)

   
   (column-center :type 'sheet-section
		  :inner-html
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
		     
