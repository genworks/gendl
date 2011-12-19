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

(define-object iges-reader-test (iges-reader)
  
  :computed-slots
  ((file-name (merge-pathnames "nurb_fillet.igs"
			       gdl-lift-utils::*lift-data-directory*))
   (make-single-brep? t)
   
   (properties-tolerance (* 1.0e-3 (the (breps 0) max-extent)))
   (regression-test-data (append (multiple-value-list
				  (the (breps 0) (precise-properties :tolerance (the properties-tolerance))))
				 (the (breps 0) %curves-to-draw%)
				 (the (breps 0) %lines-to-draw%)))))

(register-test-definition 'iges-reader-test)
