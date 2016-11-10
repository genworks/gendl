;;
;; Copyright 2002-2011 Genworks International 
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

(define-object geometry-kernel-object-mixin ()
  
  :documentation (:description "This mixin provides messages common to all NURBS-based objects whose underlying implementation
comes from a modular geometry kernel such as SMLib.")
  
  :input-slots ((from-iges? nil))
  
  :input-slots
  (
   ("List of integers. The IGES-compatible levels (layers) on which this object resides. GDL does not currently support writing
out multiple levels (layers) through the IGES writer ; only the first of these will be output if the object is exported with
the IGES output-format (please contact Genworks if you need all levels (layers) to be written out)."
    levels (get-levels *geometry-kernel* self))
   
   ("Integer. The primary IGES-compatible level (layer) on which this object resides. Defaults to the first of the levels. This 
slot can be overridden in user code to specify a new layer which will be written out when this object is exported with the IGES
output-format."
    layer (first (the levels)))
   
   ("Integer. Synonym for the layer."
    iges-level (the layer))
   
   (color-hex (lookup-color (the color-decimal) :format :hex))

   
   ("Vector of three real numbers. The RGB color of this object as imported from an external format (e.g. IGES) or as specified in :display-controls. 
Defaults to the foreground color specified in <tt>*colors-default*</tt>. This message should not normally be overridden in user application code."
    color-decimal (let ((color (when (the from-iges?) (get-color *geometry-kernel* self))))
                    (or color (multiple-value-bind (result found?) (lookup-color (getf (the display-controls) :color))
                                (when found? result))))))
  
  :computed-slots
  ((%line-vertex-indices% (let ((count -1))
                            (mapcar #'(lambda(line)
                                        (declare (ignore line))
                                        (list (incf count) (incf count)))
                                    (the %lines-to-draw%))))

   (%vertex-array% (append (flatten-lines (the %lines-to-draw%)) (flatten-curves (the %curves-to-draw%))))


   (%unique-id% (or (get-long *geometry-kernel* self)
		    (let ((id (read-from-string (subseq (string (gensym)) 1))))
		      (format t "Setting object %unique-id% on-demand for object ~s.~%" self)
		      (set-long *geometry-kernel* self id)
		      id)))))
   
   
