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

(in-package :gwl)

(define-object grid-form-control (skeleton-form-control)
  
  :documentation (:description "Beginnings of spread-sheet-like 
grid control.

To do: Add row button, sort by column values, 
save & restore snapshot. Easy way for user to 
customize layout and markup.

Allow for all types of form-control for each column.

")

  
  :input-slots
  (("List of lists. These values become the default row and column
values for the grid." 
    default (error "You must specify a default"))
   
   (keys (if (the key-row?) (first (the default)) 
           (error "You need to specify keys.")))
   
   (key-row? (and (every #'keywordp (first (the default)))
                  (not (every #'keywordp (second (the default))))))
   
   ("List of plists. Each plist contains the desired form-control 
inputs for the respective column in the table."
    form-control-attributes nil)
   
   
   ("List of lists plists. Each list corresponds to one row 
and contains plists desired form-control inputs for the 
respective column in the table."
    form-control-inputs nil)
   
   
   ("List of symbols naming GDL object types. This must be 
the same length as a row of the table. The corresponding 
form-element in the grid will be of the specified type. 
Default is nil, which means all the form-controls will 
be of type 'text-form-control." form-control-types nil)
   
   (value (mapsend (the rows) :value))
   
   
   ("Boolean. Should each row have a delete button? 
Default is nil." 
    include-delete-buttons? nil)
   
   
   ("List of strings. One for each row." row-labels nil)
   )

  
  :computed-slots
  ((raw-rows (if (the key-row?) (rest (the default)) (the default)))
   (raw-row-array (coerce (the raw-rows) 'array))
   
   (form-controls (apply #'append (list-elements (the rows) (the-element form-controls)))))


  :objects
  ((rows :type 'grid-row
         :sequence (:indices (list-of-numbers 0 (1- (array-dimension (the raw-row-array) 0))))
         :default (aref (the raw-row-array) (the-child index))
         :form-control-inputs (nth (the-child index) (the form-control-inputs))
         :include-delete-buttons? (the include-delete-buttons?)
         :row-label (nth (the-child index) (the row-labels))
         :pass-down (form-control-attributes form-control-types)))
  
  :functions
  ((restore-defaults! 
    ()
    (dolist (row (list-elements (the rows)))
      (dolist (cell (list-elements (the-object row cells)))
        (the-object cell (restore-slot-default! :value)))))))
   


(define-lens (html-format grid-form-control)()
  :output-functions
  ((form-control
    ()
    (with-html-output (*stream* nil)
      ((:table :border 1)
       (:tr (when (the include-delete-buttons?)
              (htm (:th :br)))
            (when (the row-labels)
              (htm (:th :br)))
            (dolist (key (the keys))
              (htm (:th (str key)))))
       (dolist (row (list-elements (the rows)))
         (htm (:tr (write-the-object row form-control)))))))))

(define-object grid-row (skeleton-form-control)
  :input-slots (default form-control-attributes
                   (form-control-types nil)
                 (form-control-inputs nil)
                 include-delete-buttons?
                 row-label)
  
  :computed-slots ((default-array (coerce (the default) 'array))
                   (value (mapsend (the cells) :value))
                   
                   (%form-control-types (or (the form-control-types)
                                            (make-list (length (the default)) 
                                                       :initial-element 'text-form-control)))
                   
                   (form-controls (if (typep (the %delete-button%) 'null-part)
                                      (list-elements (the cells))
                                    (cons (the %delete-button%) (list-elements (the cells))))))
  
  ;;
  ;; FLAG -- support list of types here.
  ;;
  :objects ((%delete-button% :type (if (the include-delete-buttons?) 'button-form-control 'null-part)
                             :button-type :submit
                             :default "x")
            
            (cells :type (:sequence (the %form-control-types))
                   :parameters (append (nth (the-child index) (the form-control-attributes))
                                       (nth (the-child index) (the form-control-inputs)))
                   :sequence (:size (length (the default)))
                   :default (aref (the default-array) (the-child index)))))


(define-lens (html-format grid-row)()
  :output-functions 
  ((form-control 
    ()
    (with-html-output (*stream* nil)
      (when (the include-delete-buttons?)
        (htm (:td (write-the %delete-button% form-control))))
      (when (the row-label)
        (htm (:td (str (the row-label)))))
      (dolist (cell (list-elements (the cells)))
        (htm (:td (write-the-object cell form-control))))))))
