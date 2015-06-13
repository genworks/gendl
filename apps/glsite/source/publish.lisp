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

(in-package :glsite)

(let ((static (namestring 
	       (make-pathname 
		:name nil 
		:type nil 
		:defaults (merge-pathnames "../static/" 
					   (translate-logical-pathname 
					    (glisp:source-pathname)))))))
  (publish-directory :prefix "/" :destination static))

;;
;; FLAG Put this back in when we get cookies working and lose the ugly
;;   "/sessions/..." url
;; 
;;(publish-gwl-app "/" "glsite:landing")
;;

(publish-shared :path "/" :object-type 'landing)




