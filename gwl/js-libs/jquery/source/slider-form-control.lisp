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

(in-package :jquery)

(define-object slider-form-control (text-form-control)

  :input-slots
  (
   (slider-min 0)
   (slider-max 200)
   (slider-onmouseup nil)
   (read-only? nil)
   (default 0)
   (size 5)
   (readonly? (the read-only?))
   (style "margin-left: 10px; height: 19px; top: 10px; font: 0.7em Trebuchet MS, Arial, Helvetica, sans-serif;")
   (title "slider value")
   (css-file "/jquery/css/slider.css"))

  
  :computed-slots
  (
   (slider-id (format nil "~a-slider" (the id)))
   (slider-value (the value)) ;(the default)
   (slider-stepping 1)
   
   
   (slider-init-function (format nil "

 var slider1Value               = ~a;
 var slider1Stepping            = ~a;
 var slider1Min                 = ~a;
 var slider1Max                 = ~a;

 var slider_init = function() { 



    $(\"#~a\").slider({
            'stepping': slider1Stepping,
            'min': slider1Min,  
            'max': slider1Max,
            'startValue': ~a, //slider1Value,  
            'slide': function(e, ui){  
                document.getElementById(\"~a\").value = ui.value;}});
    


    $(\"#~a\").attr(\"value\",slider1Value);

    $(\"#~a\").blur(function(){
            var slider1Value = this.value; 
            if (slider1Value >= slider1Min && slider1Value <= slider1Max){ 
                $('#~a').slider('moveTo',slider1Value);
            }else{ 
                alert('Please enter a value between '+slider1Min+' and '+slider1Max);
                return false; 
            }});

    //alert('About to move slider...');


    $('#~a').slider('moveTo',~a);

     };

"
                                 (the slider-value) (the slider-stepping) 
                                 
                                 (the slider-min) 
                                 
                                 (the slider-max) 
                                 (the slider-id) (the slider-value) (the id) 
                                 
                                 
                                 (the id) 
                                 
                                 (the id) (the slider-id)
                                 
                                 (the slider-id) (the slider-value)
                                 ))
   
   
   (js-string (string-append (the slider-init-function) "$(document).ready(slider_init);"))))

    

(define-lens (html-format slider-form-control)()
    
  :output-functions
  ((form-control
    ()
    
    (html-stream 
     *stream*
     (:div
      
      ;;
      ;; FLAG -- insert only one of these if any sliders detected in page.
      ;;
      ((:link :rel "stylesheet" :type "text/css" :href (format nil "~a" (the css-file))))
      
      ((:script :type "text/javascript" :language "JavaScript")
       (:princ (the js-string)))
      
   
      (:table
       (:tr
        (:td (:princ (format nil "~a" (the slider-min))))
        
        
        (:td ((:div :id (the slider-id) 
                    :class "ui-slider-1" 
                    :style "margin: 2px; margin-left: 2px; margin-right: 2px;"
                    :if* (the slider-onmouseup) :onmouseup (the slider-onmouseup))

              ((:div :class "ui-slider-handle"))))
        
        (:td  (:princ (format nil "~a" (the slider-max))))
        
        
        (:td (call-next-method))))
      
      
      )))))
      
        
        








