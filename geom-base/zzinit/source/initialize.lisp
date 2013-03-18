;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :geom-base)

(defparameter *settings* (list (list 'cl-who:*prologue* cl-who:*prologue* "<!doctype HTML>")
			       (list 'cl-who:*attribute-quote-char* cl-who:*attribute-quote-char* #\")
			       (list 'cl-who:*downcase-tokens-p* cl-who:*downcase-tokens-p* nil)
			       (list 'gdl:*gs-path* gdl:*gs-path* #'(lambda() (glisp:find-gs-path)))))

(defun initialize () (glisp:set-settings *settings*))

