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


(let ((static (namestring 
	       (merge-pathnames "site/static/" 
				(glisp:system-home "gdl-demos")))))
  (publish-directory :prefix "/site-static/"
		     :destination static))


;;(publish-gwl-app "/" "genworks.com:assembly")

#+nil
(dolist (host (list "cl-users.com" "cl-users.org" "cl-users.net"
		    "www.cl-users.com" "www.cl-users.org" "www.cl-users.net"))
  (publish-file :path "/index.html" 
		:host host
		:file (merge-pathnames "../static/aclu.html" *source-pathname*)))



