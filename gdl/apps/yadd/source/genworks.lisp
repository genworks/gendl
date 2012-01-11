;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:export #:autoloaded-packages
	     #:package-documentation
	     #:function-documentation
	     #:variable-documentation)))


(defun autoloaded-packages ()
  (append (list :win :windows :regexp) 
	  (remove-duplicates 
	   #-allegro nil
	   #+allegro (mapcar #'(lambda(pair) (intern (first pair) :keyword)) excl::*autoload-package-name-alist*)
	   )))

#-(or allegro lispworks sbcl) (error "Need implementation for package-documentation for the currently running Lisp.~%")
(defun package-documentation (package)
  (#+(or allegro lispworks) documentation
     #+sbcl sb-kernel:package-doc-string 
     (find-package package)
     #+allegro 'package))

#-(or allegro lispworks sbcl cmu) (error "Need implementation for function-documentation for the currently running Lisp.~%")
(defun function-documentation (function-symbol)
  #+allegro (get function-symbol 'excl::%fun-documentation)
  #+lispworks (get symbol 'system::%fun-documentation)
  #+(or cmu sbcl) (documentation function-symbol 'function))

#-(or allegro lispworks sbcl cmu) (error "Need implementation for variable-documentation for the currently running Lisp.~%")
(defun variable-documentation (function-symbol)
  #+allegro (get function-symbol 'excl::%var-documentation)
  #+lispworks (get symbol 'system::%var-documentation)
  #+(or cmu sbcl) (documentation function-symbol 'variable))

