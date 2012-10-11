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

(define-object cad-assembly (base-object)

  :input-slots 
  ((file-name nil)

   ("GDL smlib:assembly-import object. Normally this should be left to its default value. 
Defaults to the assembly-import produced from the file-name. " imported-assembly (the assembly-import))


   (display-controls (the imported-assembly display-controls))
   (orientation (the imported-assembly orientation))
   (center (the imported-assembly center))
   
   (sub-assemblies-pruned (the imported-assembly effective-subassies))

   (entities-pruned (remove-if #'(lambda(entity) (the-object entity prune?))
			       (list-elements (the imported-assembly hw-objects))))

   (strings-for-display (the imported-assembly strings-for-display))
   )

  

  :objects
  ((sub-assemblies :type 'cad-assembly
		   :sequence (:size (length (the sub-assemblies-pruned)))
		   :imported-assembly (nth (the-child index) (the sub-assemblies-pruned)))
   
   (entities :type 'cad-entity
	     :sequence (:size (length (the entities-pruned)))
	     :imported-entity (nth (the-child index) (the entities-pruned))))


  :hidden-objects
  ((assembly-import :type (if (the file-name) (read-from-string "smlib::assembly-import") 
			      'null-object)
		    :pass-down (file-name))))


(define-object cad-entity (base-object)
  :input-slots (imported-entity)

  :computed-slots ((display-controls (the imported-entity display-controls))
		   (strings-for-display (the imported-entity strings-for-display))
		   (imported-item (the imported-entity item))
		   (brep-transformed (the imported-entity brep-transformed))
		   (effective-coord-sys? (the imported-entity effective-coord-sys?)))

  :objects
  ((item :type (the imported-item type)
	 :strings-for-display (the imported-item strings-for-display)
	 :hidden? (the imported-item hidden?)
	 :center (the imported-item center)
	 :native-pointer (the imported-item native-pointer)
	 :effective-coord-sys? (the imported-item effective-coord-sys?))

   (brep :type (the brep-transformed type)
	 :brep (the brep-transformed brep)
	 :hidden? (the brep-transformed hidden?)
	 :strings-for-display (the brep-transformed strings-for-display)
	 :to-orientation (the brep-transformed to-orientation)
	 :from-orientation (the brep-transformed from-orientation)
	 :to-location (the brep-transformed to-location)
	 :from-location (the brep-transformed from-location))))


