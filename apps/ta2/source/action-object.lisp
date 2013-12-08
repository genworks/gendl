;;
;; Copyright 2013 Genworks International
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


(in-package :ta2)

(define-object action-object (base-html-sheet)
  :input-slots
  (node tatu-root tatu-color click-mode kids-error)

  :computed-slots
  ((strings-for-display (multiple-value-bind (strings error)
			    (ignore-errors (the node strings-for-display))
			  (cond ((typep error 'error)
				 (format nil "! ~a ! from strings-for-display" error))
				((the color-error?)
				 (format nil "~a ! ~a ! from color-hex" strings (the color-or-error)))
				(t strings))))
   
   (color-or-error (multiple-value-bind (color error)
		       (ignore-errors (the node color-hex))
		     (if (typep error 'error) error color)))
   
   (color-error? (typep (the color-or-error) 'error))
   
   (color-hex (if (the color-error?) (lookup-color :red :format :hex) (the color-or-error))))
  
  :functions
  ((before-present! 
    ()
    ;;
    ;; FLAG -- consider passing just the perform-action function and not the whole object
    ;;
    
    (when (not (eql (the click-mode) :ui))
      (the tatu-root (perform-action! (the node) :kids-error (the kids-error)))))))
   

