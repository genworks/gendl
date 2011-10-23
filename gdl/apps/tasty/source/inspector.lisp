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

(in-package :tasty)



(defmacro ignore-errors-with-warning (&rest body)
  (let ((values (gensym))
        (error (gensym)))
    `(let* ((,values (multiple-value-list (ignore-errors ,@body)))
            (,error (second ,values)))
       (if (and ,error (typep ,error 'error))
           (progn (warn "~a" ,error)
                  (format nil "! ERROR: ~a !" ,error))
         (apply #'values ,values)))))


;;
;; FLAG -- promote to utilities. 
;;
(defun one-line (string)
  (glisp:replace-regexp (glisp:replace-regexp string (format nil "~%") " ") "'" "\\'" ))

(defparameter *suppress-%%-messages?* t)

;;
;; FLAG -- replace with abstract associative map
;;
(defparameter *internal-keywords*
  (list :query-plist :aggregate :all-mixins :background-color 
        :base64-encoded-root-path :bashee :beziers :bounding-bbox
        :check-sanity? :color-decimal :color-hex :color-palette :cookies-received
        :cookies-to-send :dash-pattern :dom-id :fill-color-decimal
        :fixed-url-prefix :header-plist :home-page :instance-id 
        :last-visited-root-path :left-handed? :local-bbox :local-box
        :local-center :local-left-handed? :local-orientation :main-view
        :obliqueness :parent-tree :plain-url? :possible-nils 
        :quantify-box :query-toplevel :refresh-toggle :remote-id :respondent 
        :return-object :root-path-string :target :time-instantiated 
        :time-last-touched :transitory-slots :tree-root 
        :url-encoded-root-path :use-local-box? :viewable-slots 
        :viewpoints :visible-children :vrml-center))

(define-object inspector (base-html-sheet)


  :input-slots
  ((node-root-path nil :settable) 
   root-object
   tree-toplevel
   tatu-root colors-default tree-type tree-node  
   display-controls-hash
   perform-action-function click-mode 
   (relative-font-size "-1")
   
   
   (node (when (the root-object) (the root-object (follow-root-path (the node-root-path)))))

   (expand? (< (the depth) 1) :settable)
   (depth 0)
   
   )

  :computed-slots
  (   
   
   (show-settables? nil :settable)
   

   (messages-local (set-difference (the node (message-list :message-type :local))
                                   (append (the node (message-list :message-type :local 
                                                                   :category :methods))
                                           (the node (message-list :message-type :local
                                                                   :category :uncached-attributes)))))
   (messages (safe-sort
              (remove-if
               #'(lambda (keyword)
                   (or (member keyword *internal-keywords*)
                       (and *suppress-%%-messages?*
                            (let ((string (string keyword)))
                              (and
                               (> (length string) 2)
                               (or (string-equal (subseq string 0 2) "$$")
                                   (and (eql (aref string 0) #\%)
                                        (or (eql (aref string 1) #\%)
                                            (eql
                                             (aref string (1- (length string)))
                                             #\%)))))))))
               (set-difference (the :node (:message-list))
                               (append (the
                                           :node
                                         (:message-list :category :methods))
                                       (the :node (:message-list
                                                   :category
                                                   :uncached-attributes)))))
              #'string<))
   
   
   (check-links nil :settable))

  
  :hidden-objects
  ((values :type 'value-inspector 
           :depth (1+ (the depth))
           :sequence (:size (length (the messages)))
           :respondent (the respondent)
           :message (nth (the-child index) (the messages))
           :parent-node (the node)
           :pass-down (
                       tatu-root root-object
                       tree-toplevel colors-default 
                       tree-type tree-node display-controls-hash
                       perform-action-function click-mode  relative-font-size))

   
   (settables-form :type 'settables-form
                   :pass-down (show-settables? node tatu-root)))

  
  :functions
  (   
   (visit-definition-in-emacs
    (definition)

    (error "Sorry! This is supposed to do 'lisp-find-definition' in emacs, but not working yet... Please send email to support@genworks.com"))
   
   #+nil
   (visit-definition-in-emacs
    (definition)
    
    (let ((emacs-lisp-command (format nil "(fi:lisp-find-definition ~s)" definition)))
      (print-variables emacs-lisp-command)
      #+allegro (lep::eval-in-emacs emacs-lisp-command)))
    
    
   (perform-action!
    (object)
    (the tatu-root (perform-action! object)))
   
   (message-evaluate 
    (message) 
    ;;
    ;; FLAG -- note we have to bash the view-toggle because no bashing
    ;; is actually happening when we evaluate the message, so the view
    ;; does not update by itself.
    ;;
    (the toggle-view-toggle!) 
    
    (the (set-slot! :expand? t))
    
    
    (ignore-errors-with-warning (the node (evaluate message)))
    
    #+nil
    (multiple-value-bind (value error) (ignore-errors (the node (evaluate message)))
      (let* ((error? (and error (typep error 'error))) 
             (value (if  error? (format nil "ERROR: ~a" error) value)))
                         
        (when error? (the node (set-slot! message value))))))
   
   (set-object! (object) (the (set-slot! :node-root-path (the-object object root-path))))))


(define-lens (html-format inspector)()
  
  :output-functions
  ((main-view
    ()
    (with-cl-who ()
      (if (the node) 
          (write-the inspector-body)
        (htm "No Object Instantiated"))))
   
   
   (inspector-body
    ()
    (with-cl-who ()
      
      (:center
       ((:span :style "font-style: italic; font-weight: bold;")
        (:p "Inspecting: " 
            (esc (format nil "~s" 
                         (cons 'the (reverse (butlast (the node-root-path))))))
            (unless (butlast (the node-root-path))
              (htm " [the root object]")))
        (:p " of type:  " ((:span :style "color: blue; cursor: pointer;" 
                                  :onclick (the (gdl-ajax-call :function-key :visit-definition-in-emacs
                                                               :arguments (list (format nil "~s" (the node type)))))
                                  
                                  
                                  )
                           (esc (format nil "~s" (the node type)))))))
      
      ((:table :class "inspector-table" :border 1)
       (:tr (:td "Settables") (:td (str (the settables-form control-view))))
       
       
       (when (the settables-form show-settables?)
         (htm (:tr ((:td :colspan 2)
                    (str (the settables-form main-div))))))
       
       (let ((count -1))
         (dolist (value-inspector (list-elements (the values)))
           (let ((message (the-object value-inspector message)))
             (incf count)
             (htm (:tr
                    (:td (str (the-object value-inspector message)))
                    (:td 
                     ;;
                     ;; FLAG -- push everything to do with node into value-inspector
                     ;;
                     (let ((slot-status (if (the expand?) 
                                            (the node (slot-status message)) :unbound)))
                       (case slot-status
                         (:unbound 
                            (htm ((:span :style "color: blue; font-style: italic; cursor: pointer;"
                                         :onclick (the (gdl-ajax-call 
                                                        :function-key :message-evaluate
                                                        :arguments (list message))))
                                  "Unbound")))

                         (otherwise (str (the-object value-inspector value-display))))))))))))))))



(define-object value-inspector (inspector)
  
  :input-slots ((message nil)
                parent-node
                root-object
                tatu-root
                click-mode
                (value (with-error-handling () 
                         (the parent-node (evaluate (the message))))))
  
  :computed-slots 
  ((value-type (if (the 3d-vector?)
                   :gdl-3d-point
                 (typecase (the value)
                   (list (when (consp (the value)) :list))
                   (gdl::quantification :gdl-sequence)
                   (gdl::gdl-basis :gdl-atom))))


   (value-cardinality (case (the value-type)
                        (:list (length (the value)))
                        (:gdl-sequence (the value number-of-elements))
                        (otherwise 0)))

   (truncated-sequence (unless (zerop (the value-cardinality))
                         (with-output-to-string (ss)
                           (let ((*print-length* 3))
                             (pprint (the value) ss)))))
                   
   (expanded? nil :settable)

   (clickable? (and (not (the expanded?))
                    (or (member (the value-type)
                                (list :list :gdl-sequence :gdl-atom))
                        (and (eql (the value-type) :gdl-3d-point)
                             (member (the click-mode)
                                     (list :add-leaves
                                           :add-leaves*
                                           :add-node
                                           :draw-leaves
                                           :draw-node
                                           :delete-leaves))))))

   (3d-vector? (and (typep (the value) 'vector)
                    (= (length (the value)) 3)
                    (every #'floatp (the value))))
                   
   (value-display
    (ignore-errors-with-warning
     (let* ((value (the value))
            (gdl-object? (eql (class-of (class-of value))
                              (find-class 'gdl-class))))
       (with-cl-who-string ()
         ((:span :style (format nil "color: ~a; ~a" 
                                (if (the clickable?) "blue" "black")
                                (if (the clickable?) "cursor: pointer;" ""))
                 :onclick (case (the value-type)
                            (:gdl-atom
                             (the (gdl-ajax-call 
                                   :function-key :perform-action!
                                   :arguments (list value))))

                            (:gdl-3d-point
                             (when (the clickable?)
                               (the (gdl-ajax-call
                                     :function-key :perform-action!
                                     :arguments (list (the point))))))

                            ((:list :gdl-sequence)
                             (unless (the expanded?)
                               (the (gdl-ajax-call
                                     ;;:bashee self
                                     :function-key :set-slot!
                                     :arguments (list :expanded? t)))))))

          (cond ((or (eql (the value-type) :list)
                     (eql (the value-type) :gdl-sequence))
                 (if (the expanded?)
                     (htm ((:table :border 1)
                           (:tr 
                            (:td ((:span :style "color: blue; cursor: pointer;"
                                         :onclick (the (gdl-ajax-call
                                                        :function-key :set-slot!
                                                        :arguments (list :expanded? nil))))
                                  "X"))
                            (:td (str
                                  (ecase (the value-type)
                                    (:list "A List")
                                    (:gdl-sequence "A GDL Sequence")))))
                           (let ((index -1))
                             (dolist (row (list-elements (the sequence-elements)))
                               (htm (:tr (:td (str (incf index)))
                                         (:td (str (the-object row value-display)))))))))
                   (htm (:pre (esc (the truncated-sequence))))))
                (gdl-object? (htm (esc (format nil "~s" value))))
                ((eql (the value-type) :gdl-3d-point)
                 (htm (esc (the point strings-for-display))))
                (t (htm (fmt "~s" value))))))))))
    

  :objects ((point :type (if (the 3d-vector?) 'point 'null-object)
                   :center (when (the 3d-vector?) (the value))
                   :strings-for-display (format nil "~s" (the value)))

            (sequence-elements :type 'value-inspector
                               :sequence (:size (the value-cardinality))
                               :pass-down (root-object tatu-root click-mode)
                               :value (the (get-value-element (the-child index)))))

  :functions
  ((get-value-element
    (index)
    (case (the value-type)
      (:list (nth index (the value)))
      (:gdl-sequence (the value (get-member index)))))))



(define-object settables-form (base-html-sheet)
  
  :input-slots
  (show-settables? node tatu-root)
  
  
  :computed-slots
  ((settables 
    (let (settables) 
      (maphash #'(lambda(key val)(declare (ignore val)) 
                        (push key settables)) 
               (the node %settable-slots%))
      (nconc settables (the node (message-list :category :required-input-slots)))               
      (sort (remove-if
             #'(lambda (keyword)
                 (or (member keyword *internal-keywords*)
                     (and *suppress-%%-messages?*
                          (let ((string (string keyword)))
                            (or (and (> (length string) 2) 
                                     (string-equal (subseq string 0 2) "$$"))
                                (and (eql (aref string 0) #\%)
                                     (or (eql (aref string 1) #\%)
                                         (eql
                                          (aref string (1- (length string)))
                                          #\%))))))
                               
                     (let ((value (with-error-handling ()
                                    (the node (evaluate keyword)))))
                       (or (eql (class-of (class-of value)) (find-class 'gdl-class))
                           (and (consp value) 
                                (some #'(lambda(item) (eql (class-of (class-of item)) 
                                                           (find-class 'gdl-class)))
                                      value))))))
             settables) #'string<)))
   
   (respondent (the tatu-root))
   
   ;;(bashee (the node))
   
   
   (control-view (with-cl-who-string ()
                   ((:span :style "cursor: pointer; color: blue; font-style: oblique;"
                           :onclick (the (gdl-ajax-call 
                                          :bashee self
                                          :function-key :set-slot!
                                          :arguments (list :show-settables? (not (the show-settables?))))))
                    (if (the show-settables?) (htm "X") (htm "Show Settables!")))))
   
   (form-fields-hash (let ((ht (make-hash-table)))
                       (dolist (form-field (list-elements (the form-fields)) ht)
                         (setf (gethash  (the-object form-field field-name) ht) form-field)))))

  
  :hidden-objects ((reset-buttons :type 'button-form-control
                                  :sequence (:size (length (the settables)))
                                  :label "R"
                                  :onclick (the (gdl-ajax-call 
                                                 :function-key :restore-default-values! 
                                                 :arguments (list (the (form-fields (the-child index)) 
                                                                    keyword)))))
                   
                   (submit-fields-button :type 'button-form-control
                                         :label " OK "
                                         :onclick (the (gdl-ajax-call 
                                                        :form-controls (list-elements (the form-fields)))))
                   
                   (form-fields :type 'text-form-control
                                :sequence (:size (length (the settables)))
                                ;;
                                ;; FLAG -- precompute this in a slot, and allow for unbound until clicked. 
                                ;;
                                :size (length (format nil "~s" (the-child default)))
                                
                                :validation-function 
                                #'(lambda(value)
                                    (progn
                                      (the node (set-slot-if-needed! 
                                                 (the-child keyword) value :infer-types? nil))
                                      t))

                                ;;
                                ;; FLAG -- Comment this out to disable ajax submittal
                                ;;
                                :ajax-submit-on-change? t
                                
                                :domain :pass-thru
                                :keyword (nth (the-child index)(the settables))
                                :default (with-error-handling () 
                                           (the node (evaluate (the-child keyword))))
                                :prompt (the-child keyword)))
  
  :functions ((restore-default-values!
               (message-keyword)
               (the node (restore-slot-default! message-keyword))
               ;;
               ;; FLAG -- this seems to be unecessary -- why?
               ;;
               (the-object (gethash message-keyword (the form-fields-hash)) 
                           (restore-slot-default! :value)))))



(define-lens (html-format settables-form)()
  :output-functions
  ((main-view
    ()
    (with-cl-who ()
      (when (the show-settables?) 
        (with-html-form (:cl-who? t :on-submit "return false;")
          (:table
           (mapc #'(lambda(form-control reset-button)
                     (let ((message (the-object form-control keyword)))
                       (let ((remarks (the node (message-remarks message))))
                         (htm (:tr (:td (str (the-object reset-button form-control-string))
                                        ((:td :bgcolor :yellow) 
                                         (if (null remarks) 
                                             (htm (:b (str message)))
                                           (htm ((:span :style "cursor: arrow;" 
                                                        :title (one-line (second remarks)))
                                                 (:b (str message))))))
                                        (:td (str (the-object form-control form-control-string)))
                                        ))))))
                 (list-elements (the form-fields)) (list-elements (the reset-buttons))))
          
          ;;(:p ((:input :type :submit :value "OK")))
          
          (:p (str (the submit-fields-button form-control-string)))
          
          ))))))




(define-object viewable-link (base-html-sheet)
  :input-slots (node message)
  
  :computed-slots ((strings-for-display (format nil "~a" (the message))))
  
  :functions
  ((before-present!
    ()
    (with-error-handling () (the node (evaluate (the message)))))))


(define-lens (html-format viewable-link)()
  :output-functions
  ((main-sheet () (write-the return-object main-sheet))))



(define-object switch-viewables (base-html-sheet)

  :input-slots
  (return-object)
  
  :computed-slots
  ((show-viewables? nil :settable)
   (strings-for-display (if (the show-viewables?) "X" "Uncached Slots")))
  
  :functions
  ((before-present! () (the (set-slot! :show-viewables? (not (the show-viewables?)))))))


(define-lens (html-format switch-viewables)()
  :output-functions
  ((main-sheet () (write-the return-object main-sheet))))


;;
;; Test object to get form-controls working for readable Lisp objects
;;

#+nil
(define-object sample-form (base-ajax-sheet)

  :computed-slots
  ((main-sheet-body (with-cl-who-string ()
                      (str (the item-1 html-string)))))
  
  :objects
  ((item-1 :type 'text-form-control
           :ajax-submit-on-change? t
           :domain :symbol
           :default "hey now")))
