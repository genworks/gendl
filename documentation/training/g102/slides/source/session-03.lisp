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

(in-package :training-g102)



#|

  (sqrt (* gamma R T))

  (half (* rho (* V V) S C-of-lift))


|#

(define-object session-03 (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Functional vs Declarative Style")
   (slide-data `((:title "Functional Style" :bullet-points
                         ((:description "Programs Work by Returning Values,
instead of Modifying Things")
                          (:description "Dominant paradigm in Lisp") (:description "Allows Interactive Testing")
                          (:description "Test each Function as you Write it")
                          (:description "Instant feedback and confidence make
a huge difference in the aggregate")))
                 (:title "Declarative, Functional Style" :bullet-points
                         ((:description "Brings Functional Style to an even higher level")
                          (:description "Dominant paradigm in GDL")
                          (:description "Allows Dynamic, Interactive, Order-independent Testing")
                          (:description "Test each ``rule'' (attribute) as you write it")
                          (:description "Continuous feedback loop makes huge difference in the aggregate")))
                 (:title "Procedural vs. Declarative Computation" :bullet-points
                         ((:description "Many computational problems require that we compute
several <i>intermediate results</i> before we can arrive at our <i>final output</i>")
                          (:description "In Procedural Programming, we have basically two 
choices for doing this:
<ol>
<li>Sequentially set variables to our intermediate results, then use
these variables to compute our final result</li>
<li>Compute all intermediate results using ``nested'' function calls</li>")))
                 (:title "Procedural vs. Declarative Computation, Cont'd: <i>Let</i> and <i>Let*</i>" 
                         :bullet-points
                         ((:description "Note that in Lisp, <i>Let</i> assigns variables
in <i>parallel</i> (i.e. logically, they all get set simultaneously),
so the value one <i>let</i> variable cannot depend on the value of another" 
                                        :examples ((:code (let ((a 3) (b (twice a))) b) 
                                                          :return-value "Error: Attempt to take the value 
of the unbound variable `A'")))
                          (:description "Common Lisp does define <i>Let*</i> for the purpose
of sequentially assigning variables" :examples ((:code (let* ((a 3) (b (twice a))) b) :return-value 6)))
                          (:description "However, the use of <i>let*</i> is discouraged by
Lisp purists because it smacks of side-effecting and introduces 
a lot of order-dependency")
                          (:description "<i>Let*</i> forms actually get macro-expanded
into nested <i>Let</i>'s:" :examples ((:code (let ((a 3)) (let ((b (twice a))) b)) :return-value 6)))
                          (:description "So from this perspective, you can see
that, under the hood, <i>let*</i>'s can get messy pretty fast!")
                          (:description "As we will see in a moment, a Defun with a long <i>let*</I> is
a good indication that you would probably be better off using 
an object in that case!" :suppress-end-dot? t)))
                 (:title "<i>Let*, Cont'd</i>" :bullet-points
                         ((:description "Assume we need a function to take a number, double it,
add 1000 to it, then, if the result is greater than 5000, add 5% to it, then divide 
that result by 24" :examples ((:code
                               (defun hairy-calc (num)
                                 (let* ((a (twice num)) (b (+ a 1000)) (c (if (> b 5000) (+ b (* b 0.05)) b)) (d (/ c 24))) d))
                               :return-value hairy-calc)
                              (:code (hairy-calc 50) :return-value 275/6)))
                          (:description "You have to pay close attention to the order in which you assign the <i>let*</i> variables")
                          (:description "This program would be difficult to debug
if there were an error or typo in any one of the <i>Let*</i> assignments.")))
                 (:title "Nested Function Calls" :bullet-points
                         ((:description "Here is <i>hairy-calc</i> using nested function calls" :examples
               ((:code
                 (defun hairy-calc (num)
                   (/ (if (> (+ (twice num) 1000) 5000)
                          (+ (+ (twice num) 1000) (* (+ (twice num) 1000) 0.05))
                        (+ (twice num) 1000))
                      24))
                 :return-value hairy-calc)
                (:code (hairy-calc 50) :return-value 275/6)))
 (:description "Already it is difficult to read, contains 
a lot of repeated code, and would be unpleasant to debug. As 
programs grow in complexity this problem grows even faster.")))
                 (:title "Declarative Style" 
                         :bullet-points
                         ((:description "Using a Object, we can express the solution in <i>declarative style</i>" 
                                        :examples
                                        ((:define-object hairy-calc)
                                         
                                         (:code (defun hairy-calc (num) (the-object (make-part 'hairy-calc :num num) :d)) :return-value
                                          hairy-calc)
                                         (:code (hairy-calc 50) :return-value 275/6)))
                          (:description "We can specify the attributes in any order. The only
requirement is that we have no <i>circular references</i> (more on this later)" 
                                        :examples ((:define-object hairy-calc)))))))))
                 
                 
                 

