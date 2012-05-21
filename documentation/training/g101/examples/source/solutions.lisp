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

(in-package :gdl-user)

(defun dots (num)
  (dotimes (n num)
    (princ ".")))

(defun dots-r (num)
  (unless (zerop num)
    (princ ".")
    (dots-r (- num 1))))

(defun nth-rest (list num)
  (if (zerop num) list
    (rest (nth-rest list (1- num)))))

(defun our-nth (num list)
  (first (nth-rest list num)))
  
(defun our-getf (list key)
  (when list
    (if (eql key (first list))
	(second list)
      (our-getf (rest (rest list)) key))))


(defun position-filter (function list)
  (let ((counter -1) result)
    (dolist (element list (reverse result))
      (when (funcall function (incf counter))
	(push element result)))))


(defun how-much (n)
  (cond ((> n 10) "It's a lot")
        ((= n 10) "It's kind of a lot")
        (t "It's not a lot")))


(defun greeting (&key (username "Jake") (greeting "how are you?"))
  (format nil "Hello there ~a, ~a" username greeting))

(defun comment (color)
  (ecase color
    (:blue "Blue is okay")
    (:red "Red is actually her favorite color")
    (:green "Are you nuts?")))



(let ((hash (make-hash-table)))
  (defun factorial (n) 
    (or (gethash n hash)
	(setf (gethash n hash)
	  (if (<= n 1) 1 
	    (* n (factorial (1- n))))))))



(defun sort-names (lists &key (ordering :ascending))
  (let ((comparator (ecase ordering
		      ((:ascending :up) #'string-lessp)
		      ((:descending :down) #'string-greaterp))))
    (let ((predicate #'(lambda(list1 list2) 
			 (funcall comparator (first list1) (first list2)))))
      (safe-sort lists predicate))))


(defun sort-lists (lists &key (ordering :ascending))
  (let ((comparator (ecase ordering
		      ((:ascending :up) #'<)
		      ((:descending :down) #'>))))
    (let ((predicate #'(lambda(list1 list2) 
			 (funcall comparator (first list1) (first list2)))))
      (safe-sort lists predicate))))


(defun sort-numbers (numbers &key (ordering :ascending))
  (let ((predicate (ecase ordering
		       ((:ascending :up) #'<)
		       (:descending #'>))))
    (safe-sort numbers predicate)))


	       
