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




(define-object pegasus-regioned-solid-test (base-object)
    :computed-slots
    ((regression-test-data
      (mapcar #'(lambda(brep)
		  (let ((properties-tolerance (* 1.0e-5 (the-object brep max-extent))))
		    (append (multiple-value-list
			     (the-object brep (precise-properties :tolerance properties-tolerance)))
			    (the-object brep %curves-to-draw%)
			    (the-object brep %lines-to-draw%))))
	      (list-elements (the regioned breps)))))

    :objects
    ((subtracted :type 'native-reader
                 :file-name (merge-pathnames "pegasus-subtracted.iwp" gdl-lift-utils::*lift-data-directory*))

     (regioned :type 'regioned-solid
               :brep (the subtracted (breps 0)))))

;; (register-test-definition 'pegasus-regioned-solid-test)


