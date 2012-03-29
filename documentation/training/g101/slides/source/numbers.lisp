;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :training-g101)

(define-object numbers (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Numbers" :uncached)
  
   (slide-data 
    `((:title 
       "Types"
       :bullet-points
       ((:description
	 "Integers"
	 :examples
	 ((:code 1)
	  (:code 38)
	  (:code 3456765499788684748455767674834355858333442)))
       
	(:description
	 "Floating Point"
	 :examples
	 ((:code 253.75)
	  (:code 5.32e72)))
       
	(:description
	 "Ratios"
	 :examples
	 ((:code 6/7)
	  (:code 354/355)))
       
	(:description
	 "Complex"
	 :examples
	 ((:code 
	   (sqrt -25)
	   :return-value ,(sqrt -25))))))

     
      (:title "Comparison: ``=''"
	      :bullet-points 
	      ((:description "Any number of arguments")
	       (:description "Returns <i>T</i> if difference among arguments is Zero")
	       (:description "More than two args same as conjunction"
			     :examples ((:code (= 3 3.0 3.00))
					(:code (and (= 3 3.0) (= 3.0 3.00)))))
	       (:description "Do not use ``='' for floating-point numbers:"
			     :examples ((:code (= 1.599999999 1.6)
					       :return-value nil)))
	       (:description "Check the magnitude of the difference or GDL defines ``near-to?'' for this purpose:"
			     :examples ((:code (near-to? 1.555555559 1.6)
					       :return-value t)))))
     

      (:title "Comparison, cont'd: ``&gt;,'' ``&lt;,'' ``&lt;=,'' ``&gt;=,'' and ``/=''"
	      :bullet-points
	      ((:description "Any number of arguments")
	       (:description "More than two args same as conjunction"
			     :examples ((:code (> 3 4 5))
					(:code (and (> 3 4) (> 4 5)))))))
     
      (:title "Predicates"
	      :bullet-points
	      ((:description "plusp"
			     :examples ((:code (plusp 7)
					       :return-value t)))
	       (:description "minusp"
			     :examples ((:code (minusp 7)
					       :return-value nil)
					(:code (minusp -7)
					       :return-value t)))
	       (:description "zerop" 
			     :examples ((:code (zerop (- 7 7))
					       :return-value t)))
       
	       (:description "oddp, evenp"  
			     :examples ((:code (oddp 7)
					       :return-value t)
					(:code (evenp (twice 7))
					       :return-value t)))))
     
      (:title "Max, Min"
	      :bullet-points
	      ((:description "Take any number of arguments"
			     :examples ((:code (max 3 4 5 3 4 2 6 4 2 3)
					       :return-value 6)
					(:code (min 3 4 5 3 4 2 6 4 2 3)
					       :return-value 2)))
	       (:description "If you have a list you must use <i>apply</i> with #'min or #'max"
			     :examples ((:code (setq list (list 3 4 5 3 4 2 6 4 2 3)))
					(:code (apply #'max list)
					       :return-value 6)
					(:code (apply #'min list)
					       :return-value 2)))
	       (:description "For very long lists, use (GDL-provided) <i>most</i> and <i>least</i>")))
				       
     
      (:title
       "Arithmetic Operations"
       :bullet-points
       ((:description
	 "fn arg1 arg2 ... argn"
	 :examples
	 ((:code
	   (+ 1 2)
	   :return-value
	   3)
	  (:code
	   (+ 1 2 3 4 5)
	   :return-value
	   15)
	  (:code
	   (* (+ 1 2) 2)
	   :return-value
	   6)
	  (:code
	   (/ 4 2)
	   :return-value
	   2)
	  (:code
	   (+)
	   :return-value
	   0)))))
     
      (:title 
       "Incrementing and Decrementing"
       :bullet-points
       ((:description "Nondestructive Operators"
		      :examples 
		      ((:code (1+ 7)
			      :return-value 8)
		       (:code (1- 7)
			      :return-value 6)))
       
	(:description "Destructive Operators"
		      :examples
		      ((:code (setq num 7))
		       (:code (incf num)
			      :return-value 8)
		       (:code num
			      :return-value 8)
		       (:code (decf num)
			      :return-value 7)
		       (:code num
			      :return-value 7)))))))))


