;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
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

(define-object user-interface (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Human Interfaces")

   (slide-data 
    `((:title 
       "<span class=gdl-object-def style=\"font-size: 1em;\">base-ajax-sheet</span>"
       :bullet-points
       ((:description "Code for a web interface goes in-package 
 <span class=lisp-code>:gwl-user</span>, or define your own with 
 <span class=gdl-operator>gwl:define-package</span>.")
        (:description "Basic mixin to make a web page:  
<span class=gdl-object-def>base-ajax-sheet</span>.")
        (:description "You include a 
<span class=gdl-message-name>main-sheet-body</span> 
to compute a string of HTML for your page content.")
        (:description "<span class=lisp-code>with-cl-who-string</span>
is an operator from <a href=http://weitz.de/cl-who/>Edi Weitz</a> used 
for generating the actual HTML tags and content.")
        (:description 
         "You <i>publish</i> the object as a web page using 
<span class=lisp-code>publish-gwl-app</span>"
         :examples
         ((:define-object wing-surface-and-volume
              :include-page-link? t)))))
      
      (:title 
       "Separate Sheet Sections and Form Controls"
       :bullet-points
       ((:description "Ajax can update just the sheet sections which change")
        (:description "Form Controls are child objects with
<span class=gdl-message-name>default</span> and <span class=gdl-message-name>value</span>")
        (:description "You include a separate 
<span class=gdl-object-def>sheet-section</span> for each section which should
be able to update as a unit.")
        (:description 
         "The main-div from each section should be laid out in the 
main sheet."
         :examples
         ((:define-object wing-surface-and-volume-ajax
              :include-page-link? t)))))
      
      (:title
       "<i>Exercise 10</i>"
       :bullet-points
       ((:description 
         "Modify the example from the previous slide to use your two-spar wing 
from Exercise 9, and add a Form Control for the profile scaling.")))
      
      (:title 
       "Including Graphics for Geometric Objects"
       :bullet-points
       ((:description "Make a child object of type 
<span class=gdl-object-def>base-ajax-graphics-sheet</span>")
        (:description "Basic inputs are
<ul>
<li><span class=gdl-message-name>:display-list-objects</span>
<li><span class=gdl-message-name>:display-list-object-roots</span>
<li><span class=gdl-message-name>:view-direction-default</span>
<li><span class=gdl-message-name>:length</span>
<li><span class=gdl-message-name>:width</span>
</ul>")
        (:description 
         "The main-div for the viewport should be laid out in the main sheet."
         :examples
         ((:define-object wing-with-graphics
              :include-page-link? t)))))
      
      (:title
       "<i>Exercise 11</i>"
       :bullet-points
       ((:description 
         "Modify the example from the previous slide to include graphics for 
your two-spar wing from Exercise 9, and an input for the profile scaling.")))

      (:title 
       "Linking to other Sheets"
       :bullet-points
       ((:description 
         "The <span class=gdl-message-name>url</span> message will
produce a link to the page:"
         :examples
         ((:define-object several-wings
              :include-page-link? t)))))
      
      (:title 
       "Calling Gendl Functions and Setting Slot Values"
       :bullet-points
       ((:description 
         "the <span class=gdl-message-name>gdl-ajax-call</span> Gendl 
function will generate a Javascript function call which can invoke a Gendl function:"
         :examples
         ((:define-object wing-with-restore)))
        
        
        (:description 
         "With <span class=gdl-message-name>gdl-ajax-call</span> you can also 
include a list of form-controls whose values should be set (\"bashed\") into the model:"
         :examples
         ((:define-object wing-with-submit)))))
      
      (:title 
       "<i>Exercise 12</i>"
       :bullet-points  
       ((:description 
         "Modify the <span class=gdl-object-def>several-wings</span> example to 
have a button which restores the defaults on the spans of all the child wing models.")))
        
      
      ))))

        
        
        
                         
