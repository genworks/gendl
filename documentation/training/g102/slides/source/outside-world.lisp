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

(define-object outside-world (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Outside World")
   
   (slide-data 
    `((:title 
       "2D Drawings"
       :bullet-points
       ((:description "<span class=gdl-object-def>base-drawing</span> represents
a physical sheet of paper with 
<span class=gdl-message-name>width</span> and
<span class=gdl-message-name>length</span> measured in 
<i>points</i> (72 per inch, 28.29 per centimeter) and zero 
<span class=gdl-message-name>height</span>.")
        (:description "<span class=gdl-object-def>base-view</span> represents 
one rectangular area contained within a sheet.")
        (:description "<span class=gdl-object-def>base-view</span> cannot exist on
its own, it must be within the context of a drawing.")
        (:description "Only the full drawing of type
<span class=gdl-object-def>base-drawing</span> can can be viewed in Tasty (with Add 
Node in Top view) or outputted -- instances of 
<span class=gdl-object-def>base-view</span> cannot be drawn or outputted on their own.")
        (:description "<span class=gdl-object-def>base-view</span> takes 
a list of <span class=gdl-message-name>objects</span> (their selves are included)
<span class=gdl-message-name>object-roots</span> (their leaves are included).")
        (:description "Other optional input-slots to base-view include:
<dl>
<dt><span class=gdl-message-name>projection-vector</span>
<dd>defaults to <span class=lisp-code>(getf *standard-views* :top)
<dt><span class=gdl-message-name>view-scale</span>
<dd>defaults to a scale which fits the objects in the view
<dt><span class=gdl-message-name>width</span> and 
    <span class=gdl-message-name>length</span>
<dd>default to <span class=gdl-message-name>width</span> and 
               <span class=gdl-message-name>length</span> from the drawing. 
</dl>")))
      
      (:title 
       "Example Drawings"
       :bullet-points
       ((:description 
         ,(format nil "Wing Drawing (click 
<a href=~awing-drawing.pdf>here</a> to view sample)" 
                  (the image-base-url))
         :examples 
         ((:define-object wing-drawing)))
        
        (:description 
         ,(format nil "Wing Drawing with Dimension (click 
<a href=~awing-drawing-with-dimension.pdf>here</a> to view sample)" 
                  (the image-base-url))
         :examples 
         ((:define-object wing-drawing-with-dimension
              :image-url "wing-drawing-with-dimension.pdf")))
        
        (:description 
         ,(format nil "Wing Drawing with Typeset Block (click 
<a href=~awing-drawing-with-typeset-block.pdf>here</a> to view sample)" 
                  (the image-base-url))
         :examples 
         ((:define-object wing-drawing-with-typeset-block)
          (:define-object wing-drawing-typeset-block)))))

      
      (:title 
       "PDF and PNG output"
       :bullet-points
       ((:description "2D formats only work on Drawings, not on 3D geometry")
        (:description 
         "You can call the 
<span class=gdl-message-name>cad-output</span> 
<span class=gdl-section-keyword>:output-function</span>  to output the 
drawing in PDF, PNG, JPEG, DXF, or Raphael format"
         :examples
         ((:define-object wing-drawing-with-output)))))
      
      
      (:title 
       "Iges/STEP/native output"
       :bullet-points
       ((:description "You can output 3D geometry directly in these formats")
        (:description 
         "Use 
<span class=gdl-message-name>cad-output</span> for the self, and
<span class=gdl-message-name>cad-output-tree</span> for the leaves."
         :examples
         ((:define-object wing-with-iges-and-step)))
        (:description
         "Output is flat collection of entities; assembly hierarchy output is 
under development.")))

      
      (:title 
       "Streaming Output from a Web Page"

       :bullet-points
       ((:description "You publish a URL (web address) with a 
<span class=lisp-code>:path</span> (the actual web address) and a 
<span class=lisp-code>:function</span> (the function which responds to requests
made of that address)")
        (:description 
         "The response function should write the data to the 
<span class=lisp-code>request-reply-stream</span>, using the applicable
Gendl format and <span class=gdl-message-name>output-function</span>")
        (:description
         "The slot which contains the URL should do the publishing, then
return the URL for use in laying out the actual web page"
         :examples
         ((:define-object wing-with-iges-download
              :include-page-link? t)
          (:define-object wing-drawing-with-input-airfoil)))))

      
      (:title 
       "Iges/STEP/native input"
       :bullet-points
       ((:description 
         "Readers are objects of type 
<span class=gdl-object-def>iges-reader</span>, 
<span class=gdl-object-def>step-reader</span>, and
<span class=gdl-object-def>native-reader</span>."
         :examples
         ((:define-object wing-readers)))
        
        (:description "If you want to save geometry for yourself or exchange with
other Gendl users, use the native format and \".iwp\" file extension.")
        
        (:description "The readers yield a flat list of entities currently;
Assembly tree and attribute readers and direct Catia/ProE/UG readers are 
under development.")))
      
      (:title 
       "Reading Lisp data from a File"
       :bullet-points 
       ((:description 
         "If your data is already formatted as lists (or one big list) 
in a text file, then nothing can be easier than reading it into Lisp:"
         :examples
         ((:code (with-open-file (in "my-file-name.lisp")
                   (read in)))))))
      
      (:title 
       "Reading Text data from a File"
       :bullet-points 
       ((:description ,(format nil "See this 
<a href=~afile-IO.pdf>section of your course manual</a> 
which discusses reading points data from a file." (the image-base-url)))
        (:description "A few notes about this manual section:
<ul>
<li>The double-backslash is <i>not</i> the most foolproof pathname naming 
scheme in Common Lisp. It will break on Linux and MacOS. The most foolproof
naming scheme is to use proper CL pathnames (created with functions such
as <span class=lisp-code>make-pathname</span>,
<span class=lisp-code>merge-pathname</span>, etc.)
 
<li>A more convenient approach might sometimes be to use 
Unix-style forward slashes (e.g. \"/foo/bar/goo.txt\"). These will
also work on Windows.

<li>If you have control over your file formats, it is almost always
best to use a \"Lispy\" format to store your data, because you have
 a built-in Lisp parser always available in Lisp. In this format
 you can read in your data with a simple call to 
<span class=lisp-code>read</span>.
</ul>
")))
      
      (:title 
       "Other outside world interface examples"
       :bullet-points
       ((:description "Relational Database interface (ODBC, MySQL, Oracle)")
        (:description "Webserver (allegroserve) and Web client (allegroserve client)")
        (:description "XML and HTML parsers and generators")
        (:description "JSON, Javascript (cl-json, cl-parenscript)")
        (:description "C and FORTRAN foreign-function interface")))
      
      (:title "<i>Exercise 13</i>"
              :bullet-points
              ((:description "Modify your airfoil exercise to read its points 
from a Lisp-formatted external file.")
               (:description ,(format nil "Modify your airfoil exercise 
to read its points from a plain text external file, as described in the
<a href=~afile-IO.pdf>course manual</a>." (the image-base-url)))))))))



        
              

      
      
      
      
      
      
      
