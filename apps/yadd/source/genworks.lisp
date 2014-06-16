;;
;; Copyright 2002-2011 Genworks International 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;

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

(in-package :com.genworks.lisp)


(defun autoloaded-packages ()
  (append (list :win :windows :regexp) 
	  (remove-duplicates 
	   #-allegro nil
	   #+allegro (append (mapcar #'(lambda(name) (glisp:intern (glisp:upcase name) :keyword))
				     (mapcar #'first excl::*autoload-package-name-alist*))
			     (mapcar #'rest excl::*autoload-package-name-alist*))
	   )))



#-(or allegro lispworks sbcl cmu ccl) (error "Need implementation for function-documentation for the currently running Lisp.~%")
(defun function-documentation (function-symbol)
  #+allegro (get function-symbol 'excl::%fun-documentation)
  #+lispworks (get function-symbol 'system::%fun-documentation)
  #+(or cmu sbcl ccl) (documentation function-symbol 'function))

#-(or allegro lispworks sbcl cmu ccl) (error "Need implementation for variable-documentation for the currently running Lisp.~%")
(defun variable-documentation (function-symbol)
  #+allegro (get function-symbol 'excl::%var-documentation)
  #+lispworks (get function-symbol 'system::%var-documentation)
  #+(or cmu sbcl ccl) (documentation function-symbol 'variable))

