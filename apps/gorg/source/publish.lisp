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

(defparameter *gorg-hosts*
  
  (list "gendl.org" "www.gendl.org"
	"gendl.com" "www.gendl.com")

  #+nil
  (list "localhost" "xenie" 
	"gendlformacosx.com" "www.gendlformacosx.com"
	"gendlformacos.com" "www.gendlformacos.com"
	"gendlformacosx.org" "www.gendlformacosx.org"
	"gendlformacos.org" "www.gendlformacos.org"
	"macos.gendl.com" "www.macos.gendl.com"
	"macos.gendl.org" "www.macos.gendl.org"
	"macosx.gendl.com" "www.macosx.gendl.com"
	"macosx.gendl.org"  "www.macosx.gendl.org"
	"gendl.org" "www.gendl.org"
	"gendl.com" "www.gendl.com"))


(defun initialize ()


  (let ((static (or 
		 (when (glisp:source-pathname)
		   (probe-file 
		    (make-pathname 
		     :name nil 
		     :type nil 
		     :defaults (merge-pathnames "../static/" 
						(translate-logical-pathname 
						 (glisp:source-pathname))))))
		 (probe-file (merge-pathnames "static/" *system-home*))
		 (probe-file (merge-pathnames "gorg-static/" glisp:*gdl-program-home*)))))
    (if static 
	(progn (setq static (namestring static))
	       (dolist (host *gorg-hosts*)
		 (publish-directory :prefix "/gorgstat/" :destination static :host host)))
	(warn "static directory does not exist in gorg publish.")))


  ;;
  ;; FLAG Put this back in when we get cookies working and lose the ugly
  ;;   "/sessions/..." url
  ;; 
  ;;(publish-gwl-app "/" "glsite:landing")
  ;;

  (publish-shared :host "gendl.org" :path "/" :object-type 'landing)
  
  #+nil
  (dolist (host *gorg-hosts*)
    (publish-shared :host host :path "/" :object-type 'landing)))


  
;;
;; FLAG -- arrange to call this on production startup. 
;;
;;(initialize)
