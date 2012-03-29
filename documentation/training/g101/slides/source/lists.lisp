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


(define-object lists (slide-show-leaf)
  :computed-slots
  ((strings-for-display "Lists")
   (:slide-data 
    '((:title
       "Simple Conditionals"
       :bullet-points
       ((:description 
         "Introduced here so we can use them in List examples")
        (:description 
         "<i>if test then-form else-form</i>"
         :examples
         ((:code
           (if (> 3 4) 'yes 'no)
           :return-value no)))
        (:description
         "<i>when test result-forms</i>"
         :examples
         ((:code 
           (when (< 3 4) 
             (action-1)
             (action-2)
             'yes)
           :return-value 'yes)
          (:code 
           (when (> 3 4) 
             (action-1)
             (action-2)
             'yes)
           :return-value nil)
          
          ))
        (:description 
         "<i>unless test result-forms</i> (opposite of <i>when</i>)"
         :examples
         ((:code 
           (unless (< 3 4) 
             (action-1)
             (action-2)
             'yes)
           :return-value nil)
          (:code 
           (unless (> 3 4) 
             (action-1)
             (action-2)
             'yes)
           :return-value 'yes)))))
      
      (:title 
       "Length of a List"
       :bullet-points
       ((:description
         "length list"
         :examples
         ((:code
           (length '(gm ford chrysler volkswagen))
           :return-value
           4)
          (:code
           (length nil) 
           :return-value
           0)
          (:code
           (defun our-length (list)
             (if (null list) 0 (+ (our-length (rest list)) 1)))
           :return-value our-length)
          (:code
           (our-length '(gm ford chrysler volkswagen))
           :return-value
           4)))))
        
      (:title
       "Member of a List"
       :bullet-points
       ((:description
         "member item list"
         :examples
         ((:code
           (member 
            'dallas 
            '(boston 
              san-francisco 
              portland))
           :return-value
           nil)
          
          (:code
           (member 'san-francisco '(boston san-francisco portland))
           :return-value
           (san-francisco portland))
          
          (:code
           (member 'portland '(boston san-francisco portland))
           :return-value
           (portland))
          
          (:code
           (member 'boston '(detroit chicago boston san-francisco portland))
           :return-value
           (boston san-francisco portland))
          
          (:code
           (defun our-member (elem list)
             (if (null list)
                 nil
               (if (eql elem (first list)) 
                   list 
                 (our-member elem (rest list)))))
           :return-value our-member)
          
          (:code
           (our-member 'boston '(detroit chicago boston san-francisco portland))
           :return-value
           (boston san-francisco portland))))
          
        (:description
         "rest of list starting at found member is returned.")
        (:description
         "<b>Note:</b> anything that is NOT NIL is considered TRUE."
         :suppress-end-dot? t)))
      
      
      (:title
       "Getting part of a List (treating the list as a Sequence)"
       :bullet-points
       ((:description
         "<i>subseq list start-position end-position</i><br><br>
<i><font color=\"red\">Note: element at end-position is NOT included in the result; 
the preceding element will be the last element in the result.</font></i><br><br>"
         
         :suppress-end-dot? t
         :examples
         ((:code
           (subseq '(a b c d) 1 3)
           :return-value
           (b c))
          (:code
           (subseq '(a b c d) 1 2)
           :return-value
           (b))
          (:code
           (subseq '(a b c d) 1)
           :return-value
           (b c d))))))

      
      (:title
       "Appending Lists"
       :bullet-points
       ((:description
         "append list1 list2"
         :examples
         ((:code
           (setq *my-slides* '(introduction welcome lists functions))
           :return-value
           (introduction welcome lists functions))
          (:code
           (append *my-slides* '(numbers))
           :return-value (introduction welcome lists functions numbers))
          (:code 
           *my-slides*
           :return-value (introduction welcome lists functions))
          (:code
           (setq *my-slides* (append *my-slides* '(numbers)))
           :return-value
           (introduction welcome lists functions numbers))
          (:code
           *my-slides*
           :return-value
           (introduction welcome lists functions numbers))))))

      
      (:title 
       "Adding Elements to a List"
       :bullet-points
       ((:description
         "<i>cons elem list</i>"
         :examples
         ((:code 
           (cons 'a '(b c d))
           :return-value (a b c d))))))
      

      (:title
       "Removing Elements from Lists"
       :bullet-points
       ((:description
         "remove element list"
         :examples
         ((:code
           (setq *data* '(1 2 3 1000 4))
           :return-value
           (1 2 3 1000 4))
             
          (:code
           (remove 1000 *data*)
           :return-value (1 2 3 4))
             
          (:code 
           *data*
           :return-value (1 2 3 1000 4))
             
          (:code
           (setq *data* (remove 1000 *data*))
           :return-value
           (1 2 3 4))
             
          (:code
           *data*
           :return-value
           (1 2 3 4))))))
      

      (:title
       "Sorting Lists"
       :bullet-points
       ((:description
         "<i>sort list predicate-fn</i>"
         :examples
         ((:code
           (setq *data* (list 1 3 5 7 2 4 6))
           :return-value
           (1 3 5 7 2 4 6))
          (:code
           (setq *data* (sort *data* #'<))
           :return-value
           (1 2 3 4 5 6 7))
          (:code
           (setq *data* (sort *data* #'>))
           :return-value
           (7 6 5 4 3 2 1))))
        (:description
         "<font color=\"red\">Warning: sort is a DESTRUCTIVE function</font>")
        (:description
         "Most Lisp functions are NON-destructive; they simply return
values, without modifying their arguments.")
        (:description
         "Destructive functions can be used in later iterations of
development to save on memory and improve performance.")
        (:description
         "safe-sort is non-destructive version of sort defined by Genworks GDL.")))


      (:title
       "Treating a List as a Set"
       :bullet-points
       ((:description
         "No inherent ordering in a Set")
        (:description
         "<i>union list1 list2 ... listn</i>"
         :examples
         ((:code
           (union '(1 2 3) '(2 3 4))
           :return-value
           (1 2 3 4))
          (:code
           (union '(1 2 3) '(2 3 4))
           :return-value
           (2 3 4 1))))
        (:description
         "<i>intersection list1 list2</i>"
         :examples
         ((:code
           (intersection '(1 2 3) '(2 3 4))
           :return-value
           (2 3))))
        (:description
         "<i>set-difference list1 list2</i>"
         :examples
         ((:code
           (set-difference '(1 2 3 4) '(2 3))
           :return-value
           (4 1))))))


      (:title 
       "Mapping"
       :bullet-points
       ((:description
         "<i>mapcar #'fn list1 list2 ... listn</i>"
         :examples
         ((:code
           (mapcar #'twice '(1 2 3 4))
           :return-value
           (2 4 6 8))))
        
        (:description "Lambda (unnamed) functions are used <b>very</b> frequently with mapcar. More on this later.")
        
        (:description "Also mapc, mapcan, map, etc... more on these later.")))


      (:title
       "Property Lists"
       :bullet-points
       ((:description
         "A.K.A. ``Plists''")
        (:description
         "Provide a simple yet powerful way to handle keyword-value pairs")
        (:description "A keyword is a symbol beginning with a colon. More on them later.")
        (:description
         "A Plist is a list made up of keyword/value pairs. The values may or may not be keywords.")
        (:description
         "A Plist <b>must</b> have an even number of elements or you will land in the debugger.")
        (:description
         "Access using <i>getf plist keyword</i>"
         :examples
         ((:code 
           (getf (list :michigan :lansing :illinois :springfield :pennsylvania :harrisburg) :illinois)
           :return-value :springfield)))
        (:description
         "Can update using <i>setf (getf plist keyword) value</i>"
         :examples
         ((:code
           (setq *state-data* (list :michigan :lansing :illinois :springfield :pennsylvania :harrisburg))
           :return-value (:michigan :lansing :illinois :springfield :pennsylvania :harrisburg))
          
          (:code
           (setf (getf *state-data* :michigan) :pontiac)
           :return-value :pontiac)
          
          (:code
           (getf *state-data* :michigan)
           :return-value :pontiac)))))
      
      (:title
       "Property Lists, Cont'd"
       :bullet-points
       ((:description
         "More Examples:"
         :examples
         ((:code
           (setq os-ratings (list :unix "good" :windows "bad"))
           :return-value (:unix "good" :windows "bad"))
          (:code
           (getf os-ratings :windows)
           :return-value  "bad")
          (:code
           (setf (getf os-ratings :windows) "atrocious") 
           :return-value "atrocious")
          (:code
           (getf os-ratings :windows)
           :return-value "atrocious")))))

      (:title
       "Property Lists, Cont'd"
       :bullet-points
       ((:description
         "Plists can be and often are ``nested:''"
         :examples
         ((:code
           (setq os-ratings (list :unix "good"
                                  :linux "better"
                                  :windows 
                                  (list :openness    "low"
                                        :performance "low"
                                        :security    "low"
                                        :price       "high")))
           :return-value (:unix "good" :linux "better" :windows
                                (:openness 
                                 "low" 
                                 :performance "low" 
                                 :security "low" 
                                 :price "high")))
          (:code
           (getf (getf os-ratings :windows) :performance)
           :return-value "low")
              
          (:code
           (setf (getf (getf os-ratings :windows) :performance)
             "abysmal")
           :return-value "abysmal")
              
          (:code
           (getf (getf os-ratings :windows) :performance)
           :return-value "abysmal")))))
      
      
      (:title "Exercises for Session 3 : Lists"
       :bullet-points
       (        
        (:description "Write a recursive and iterative version of
a function <i>dots number</i> which takes a number and prints that many dots
 (you can print a dot by calling <i>(princ \".\")</i>).")
        
        
        (:description "Write a recursive function <i>nth-rest n list</i>, which returns the
result of applying <i>rest</i> n times to <i>list</i> (equivalent to the Lisp function <i>nthcdr</i>)."
         :examples
         ((:code
           (nth-rest 2 '(a b c d e f))
           :return-value (c d e f))
          (:code
           (rest (rest '(a b c d e f)))
           :return-value (c d e f))))
        
        (:description "Write a recursive function <i>our-nth n list</i>, which returns the
nth element of list (equivalent to the Lisp function <i>nth</i>.<br>
<i>Hint: implement this using nth-rest from previous exercise</i>
 (My solution is one line)"

         :examples
         ((:code
           (our-nth 2 '(a b c d e f))
           :return-value c)))
        
        
        (:description "Write a recursive function <i>our-getf plist keyword</i>, which behaves as <i>getf</i>
 (don't worry about being able to <i>setf (getf plist keyword> value</i>)<br>
<i>Hint: nth-rest might be of assistance in implementing this as well.</i>
 (My solution is four lines)."

         :examples
         ((:code 
           (our-getf '(:germany :berlin :england :london :finland :helsinki :norway :oslo :france :paris) :finland)
           :return-value :helsinki)))

        
        (:description
         "<i>The following are bonus exercises which may require some concepts
we have not learned yet -- we will revisit them later if you can't do
them now.</i>")
        
        
        (:description 
         "Write a function, <i>filter-in fn list</i>,
which takes a <i>function</i> and a <i>list</i>, and returns a new 
list containing all elements of <i>list</i> for which the <i>function</i>
returns non-nil (equivalent to the CL function <i>remove-if-not</i>).
 (My solution is four lines)."

         :examples
         ((:code
           (filter-in #'oddp '(4 2 6 5 8 3 4 9))
           :return-value (5 3 9))
          (:code
           (filter-in #'listp '(hello (how are you) well (i am okay) 
                                but (a bit tired)))
           :return-value ((how are you) (i am okay) (a bit tired)))))
        
        
        (:description
         "Write a function, <i>position-filter fn list</i>, which takes
a <i>function</i> and a <i>list</i>, and returns a new list containing
all elements from <i>list</i> for which the <i>function</i> returns
true when applied to that element's (zero-indexed) position in the <i>list</i>:
 (My solution is six lines)."
         :examples
         ((:code
           (position-filter #'oddp '(a b c d e f))
           :return-value (b d f))
          (:code
           (position-filter #'evenp '(:germany :berlin :england :london :finland :helsinki :norway :oslo :france :paris))
           :return-value (:GERMANY :ENGLAND :FINLAND :NORWAY :FRANCE))))
                
        
        (:description 
         "Write a function, <i>set-difference-o set1 set2</i>,
which works just like <i>set-difference</i>, except it retains the 
ordering of elements in the set1.

 (My solution is four lines)."
         :examples
         ((:code
           (set-difference-o '(a b c d e f) '(e d b))
           :return-value (a c f))
          (:code
           (set-difference '(a b c d e f) '(e d b))
           :return-value (f c a))))
        
        (:description 
         "Write a function, <i>intersection-o set1 set2</i>,
which works just like <i>intersection</i>, except it retains the 
ordering of elements in the set1.

 (My solution is four lines)."
         :examples
         ((:code
           (intersection-o '(a b c d e f) '(e d b))
           :return-value (b d e))
          (:code
           (intersection '(a b c d e f) '(e d b))
           :return-value (e d b))))))))))
