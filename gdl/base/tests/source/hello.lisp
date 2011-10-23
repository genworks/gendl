;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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


(in-package :gdl-base-tests)

(define-object try ()

  :input-slots
  ((foo "bar"))

  :trickle-down-slots
  (value-13 key try-d)

  :computed-slots
  ((favorite "blue" :settable)
   (foo-dep (the foo) :settable)
   (other-favorite (the favorite) :settable)
   (try-d "hey man")
   (value (+ 1 2))
   (value-13  (+ (the value) (the value)))
   (this-value (the this color))
   (this-type 'that)
   (key :favorite)
   (mine (the (evaluate (the key))))
   (intermediate (the favorite))
   (my-other-color (string-append "other-" (the intermediate)))
   (my-color (the intermediate)))

  :objects 
  ((these :type (the this-type)
          :sequence (:size 2)
          :other-color "blue"
          :dummy "dummy"
          :try-another (+ (the value) (the value-13)))
   (this :type (the this-type)
         :try "from-parent"
         ;;:parameters (list :parm-1 "parm-1" :parm-2 (the value-13) :parm-3 "third's a charm")
         :parameters-x (list :parm-1 "parm-1" :parm-2 (the value-13) :parm-3 "third's a charm")
         :other-color (string-append (the-child :color)
                                     "-"
                                     (the-child :another :other-color)
                                     "-"
                                     "blue")))
  
  :hidden-objects 
  ((these-h :type (the this-type)
            :sequence (:size 2)
            :other-color "blue"
            :dummy "dummy"
            :try-another (+ (the value) (the value-13)))
   (this-h :type (the this-type)
           :try "from-parent"
           :parameters-x (list :parm-1 "parm-1" :parm-2 (the value-13) :parm-3 "third's a charm")
           :other-color (string-append (the-child :color)
                                       "-"
                                       (the-child :another :other-color)
                                       "-"
                                       "blue")))
  
  :methods 
  ((colors-m 
    () (string-append (the my-other-color) (the my-color)))
   
   (new-value-m 
    (&key factor) (* (the value) factor)))
  
  :functions
  ((colors-f 
    () (string-append (the my-other-color) (the my-color)))
   
   (new-value-f 
    (&key factor) (* (the value) factor))))


(define-object that ()
  
  :input-slots
  (parm-1 
   other-color
   (try "try-string")
   (try-d "try-string" :defaulting))
  
  :computed-slots
  ((color "red"))
  
  :objects
  ((another :type 'that
            :other-color "green")))

(in-package :gdl-user)

(defun gdl-base-ref () 
  (let ((self (make-object 'gdl-base-tests::try)))
    (with-open-file (out 
		     ;;
		     ;; !!! hard coded file name must be changed !!! 
		     ;;
		     "d:/date/svn/gdl-base-ref.dat"
		     :direction :output
                     :if-exists :new-version
                     :if-does-not-exist :create)
      (format out"(~%")
      (format out ":foo \"~a\"~%" (the foo))
      (format out ":value-13 \"~a\"~%" (the value-13))
      (format out ":key  \"~a\"~%" (the key))
      (format out ":try-d \"~a\"~%" (the try-d))
      (format out ":favorite \"~a\"~%" (the favorite))
      (format out ":other-favorite \"~a\"~%" (the other-favorite))
      (format out ":value \"~a\"~%" (the value))
      (format out ":this-value \"~a\"~%" (the this-value))
      (format out ":this-type \"~a\"~%" (the this-type))
      (format out ":mine \"~a\"~%" (the mine ))
      (format out ":intermediate \"~a\"~%" (the intermediate))
      (format out ":my-other-color \"~a\"~%" (the my-other-color))
      (format out ":my-color \"~a\"~%" (the my-color))
      (format out ":these-0-other-color \"~a\"~%" (the (these 0) other-color))
      (format out ":these-1-other-color \"~a\"~%" (the (these 1) other-color))
      (format out ":these-0-dummy \"~a\"~%" (the (these 0) dummy))
      (format out ":these-1-dummy \"~a\"~%" (the (these 1) dummy))
      (format out ":these-0-try-another \"~a\"~%" (the (these 0) try-another))
      (format out ":these-1-try-another \"~a\"~%" (the (these 1) try-another))
      (format out ":these-0-value-13 \"~a\"~%" (the (these 0) value-13))
      (format out ":these-1-value-13 \"~a\"~%" (the (these 1) value-13))
      (format out ":these-0-key \"~a\"~%" (the (these 0) key))
      (format out ":these-1-key \"~a\"~%" (the (these 1) key))
      (format out ":this-try \"~a\"~%" (the this try))
      (format out ":this-parameters-x-p-1 \"~a\"~%" (getf (the this parameters-x ):parm-1 ))
      (format out ":this-parameters-x-p-2 \"~a\"~%" (getf (the this parameters-x ):parm-2 ))
      (format out ":this-parameters-x-p-3 \"~a\"~%" (getf (the this parameters-x ):parm-3 ))
      (format out ":this-other-color \"~a\"~%" (the this other-color ))
      (format out ":these-h-0-other-color \"~a\"~%" (the (these-h 0) other-color))
      (format out ":these-h-1-other-color \"~a\"~%" (the (these-h 1) other-color))
      (format out ":these-h-0-dummy \"~a\"~%" (the (these-h 0) dummy))
      (format out ":these-h-1-dummy \"~a\"~%" (the (these-h 1) dummy))
      (format out ":these-h-0-try-another \"~a\"~%" (the (these-h 0) try-another))
      (format out ":these-h-1-try-another \"~a\"~%" (the (these-h 1) try-another))
      (format out ":these-h-0-value-13 \"~a\"~%" (the (these-h 0) value-13))
      (format out ":these-h-1-value-13 \"~a\"~%" (the (these-h 1) value-13))
      (format out ":these-h-0-key \"~a\"~%" (the (these-h 0) key))
      (format out ":these-h-1-key \"~a\"~%" (the (these-h 1) key))
      (format out ":this-h-try \"~a\"~%" (the this-h try))
      (format out ":this-h-parameters-x-p-1 \"~a\"~%" (getf (the this-h parameters-x ):parm-1 ))
      (format out ":this-h-parameters-x-p-2 \"~a\"~%" (getf (the this-h parameters-x ):parm-2 ))
      (format out ":this-h-parameters-x-p-3 \"~a\"~%" (getf (the this-h parameters-x ):parm-3 ))
      (format out ":this-h-other-color \"~a\"~%" (the this-h other-color ))
      (format out ":colors-m \"~a\"~%" (the colors-m ))
      (format out ":colors-f \"~a\"~%" (the colors-f ))
      (format out ":new-value-m \"~a\"~%" (the (new-value-m :factor 2)))
      (format out ":new-value-f \"~a\"~%" (the (new-value-f :factor 3)))
      (format out ":favorite-set \"~a\"~%" (progn (the (set-slot! :favorite "favorite-is-set" ))(the favorite)))
      (format out ":other-favorite-set \"~a\"~%" (the other-favorite))
      (format out ":favorite-d-set \"~a\"~%" (progn (the (restore-slot-default! :favorite))(the favorite)))
      (format out ":other-favorite-d-set \"~a\"~%" (the other-favorite))
      (format out")~%")
    )))

        
    
