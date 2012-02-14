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

#+nil
(defparameter *input-files-pathname* 
  (merge-pathnames 
    (make-pathname :directory (append (butlast (pathname-directory *source-pathname*))
				      (list "input-files"))
		   :device (pathname-device *source-pathname*))))



#+nil
(defparameter *airfoil-library-pathname* 
    (make-pathname :directory (append (butlast (pathname-directory *source-pathname*))
				      (list "airfoil-library"))
		   :device (pathname-device *source-pathname*)))


