;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :geom-base)


(#+allegro 
 excl:without-package-locks #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings #-allegro progn
  (define-object-amendment global-filleted-polyline-mixin ()
    :computed-slots 
    ((path-info (append
		 (let ((first? t))
		   (mapcan #'(lambda(straight curve-set)
			       (append (if first? (progn (setq first? nil)
							 (list :move (first straight) :line (second straight)))
					   (list :line (second straight)))

				       (apply #'append
					      (mapcar #'(lambda(curve) (cons :curve (rest (reverse curve))))
						      (reverse (the-object curve-set %curves-to-draw%))))))
			   (the straights) (list-elements (the fillets))))

		 (when (> (length (the straights)) (the fillets number-of-elements))
		   (list :line (second (lastcar (the straights)))))))))))
	     
