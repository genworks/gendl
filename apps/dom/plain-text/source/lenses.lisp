;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :com.genworks.dom-plain-text)

(defparameter *line-length* 72)

(define-lens (plain-text assembly) ()
  :output-functions
  ((base
    ()
    (dolist (chapter (list-elements (the :chapters)))
      (write-the-object chapter (base))))))


(define-lens (plain-text section)()
  :output-functions
  ((base
    ()
    (write-env (:set-line-position 0)
	       (:set-newline-count 0))
    (write-env (:newline-out)
	       (:newline-out)
	       (:a (the :heading-number))
	       " "
	       (:a (the :title))
	       (:newline-out)
	       (:newline-out))
    (dolist (element (list-elements (the :elements)))
      (write-the-object element (base))))))


(define-lens (plain-text marked-up-string)()
  :output-functions
  ((base
    ()
    (write-the :string (base)))))

(define-lens (plain-text item-list)()
  :output-functions
  ((base
    ()
    (write-env (:set-line-position 0)
	       (:set-newline-count 0))
    (write-env (:newline-out)
	       (:newline-out)
	       "List Here"
	       (:newline-out)
	       (:newline-out)))))

(define-lens (plain-text figure)()
  :output-functions
  ((base
    ())))


(define-lens (plain-text text-string)()
  :output-functions
  ((base
    ()
    (case (the :markup-tag)
      (:verbatim
       (write-env (:set-line-position 0))
       (write-env (:newline-out)
		  (:newline-out)
		  (:a (the :data))
		  (:newline-out)
		  (:newline-out)))
      (:index)
      (otherwise
       (let ((length (length (the :word-list)))
	     (count -1))
	 (dolist (word (the :word-list))
	   (if (eql word :newline)
	       (let ((new-value (1+ (write-env (:get-newline-count)))))
		 (write-env (:set-newline-count new-value))
		 (when (= new-value 2)
		   (write-env (:set-line-position 0)
			      (:newline-out)(:newline-out))))
	     (let ((word (case (the :markup-tag)
			   (:ref "??")
			   (otherwise word))))
	       (write-env (:set-newline-count 0))
	       (when (> (+ (write-env (:get-line-position))
			   (length word)) *line-length*)
		 (write-env (:newline-out)
			    (:set-line-position 0)))
	       (write-env (:a word))
	       (if (/= (incf count) length)
		   (write-env " ")
		 (write-env ""))
	       (let ((new-value (+ (write-env (:get-line-position)) (length word) 1)))
		 (write-env (:set-line-position  new-value))))))))))))



