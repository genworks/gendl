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

(define-object step-writer-test (base-object)
  
  :computed-slots
  ((output-file-name
    (let ((output (merge-pathnames "round-trip.stp" gdl-lift-utils::*lift-data-directory*)))
      (with-format (step output) (write-the test-part (breps 0) cad-output))
      output))

   (regression-test-data
    (append (multiple-value-list (the input (breps 0) precise-properties))
            (the input (breps 0) %curves-to-draw%)
            (the input (breps 0) %lines-to-draw%))))

  :objects
  ((test-part :type 'native-reader
	       :file-name (merge-pathnames "iraro.iwp" gdl-lift-utils::*lift-data-directory*))

   (input :type 'step-reader
	  :file-name (the output-file-name)
          :make-single-brep? nil)))

(register-test-definition 'step-writer-test)
