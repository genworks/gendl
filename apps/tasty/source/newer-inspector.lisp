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


#|
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



(defun one-line (string)
  (replace-regexp* (replace-regexp* string (format nil "~%") " ") "'" "\\'" ))

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

(define-object newer-inspector (sheet-section) ;;(base-html-sheet)


  :input-slots
  ((node-root-path nil :settable) 
   root-object
   tree-toplevel
   tatu-root 
   colors-default 
   tree-type 
   tree-node  
   display-controls-hash
   perform-action-function
   (relative-font-size "-1")
   
   (message nil)
   (message-type nil)
   
   (parent-node nil)
   click-mode
   (value (with-error-handling 
	      () 
	    (the parent-node (evaluate (the message)))))
   
   
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
   
   (messages-2 (the (filter-messages
		   (set-difference (the :node (:message-list :return-category? t))
			       (append (the
					   :node
					 (:message-list :category :methods :return-category? t))
				       (the :node (:message-list
						   :category
						   :uncached-attributes :return-category? t)))))))
   
   (messages (the (filter-messages
		   (set-difference (the :node (:message-list))
			       (append (the
					   :node
					 (:message-list :category :methods))
				       (the :node (:message-list
						   :category
						   :uncached-attributes)))))))
   
   (check-links nil :settable)
   
   (value-type (if (the 3d-vector?)
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
			    (print-variables (the clickable?)
					     (the click-mode))
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
			  (:tr (:td ((:span :style "color: blue; cursor: pointer;"
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
	       (t (htm (fmt "~s" value))))))))
   
   
   
   
   
   (main-view
    (with-cl-who-string ()
      (if (the node)
	  (str (the inspector-node-view))
	(str "No Object Instantiated"))))

   (inspector-node-view
    (with-cl-who-string ()
      ;; header first, conforming jquery ui style framework
      ((:div :class "header ui-widget-header")
       (str (the node strings-for-display)))
          ;;(esc (format nil "~s" (cons 'the (reverse (butlast (the node-root-path))))))
          ;;(unless (butlast (the node-root-path))
	  ;;(htm " Root Object")))
      ((:div :class "header")
       ((:span :class "ui-icon ui-icon-tag fltlft" :title "this Object is of type ..."))
       "type: "(esc (format nil "~s" (the node type))))
      ;; rest of the inspector below, inside an UL/LI list, with tables for prop-value
      ;; all conforming ui style framework
      ((:ul :id "inspector-root" :class "tasty-tree") ;;"tasty-inspector") 
       ((:li)
	;;
	;; FLAG 090908-JB: removing this arbitrary display code from the inspector for now
	;;
	((:table :class "inspector-table")
	 ((:thead)
	  (:th "slot-type")
	  (:th "slot-name")
	  (:th "value")
	  )
	 ((:tbody)
	  ((:tr)
	   (:td (:span :class "ui-icon ui-icon-pencil"))
	   (:td "Settables")
	   (:td (str (the settables-form main-div))))
	  ;;
	  ;; FLAG 090908-JB: this is old code from that builds the table
	  ;;
	  (let ((count -1))
	    (dolist (inspector-node (list-elements (the nodes)))
	      (let ((message (the-object inspector-node message)))
		(incf count)
		(htm (:tr
		      (:td (str (the-object inspector-node message-type)))
		      (:td (str (the-object inspector-node message)))
		      (:td 
		       ;;
		       ;; FLAG -- push everything to do with node into inspector-node
		       ;;
		       (let ((slot-status (if (the expand?) (the node (slot-status message)) :unbound)))
			 (case slot-status
			   (:unbound 
			    (htm ((:span :style "color: blue; font-style: italic; cursor: pointer;"
					 :onclick (the (gdl-ajax-call :function-key :message-evaluate
								      :arguments (list message))))
				  "Unbound")))
			   (otherwise (str (the-object inspector-node value-display)))))))))))))	  
	  ))))
 
   
   )
  
  :objects
  ((point :type (if (the 3d-vector?) 'point 'null-object)
		   :center (when (the 3d-vector?) (the value))
		   :strings-for-display (format nil "~s" (the value)))

	    (sequence-elements :type 'inspector-node
			       :sequence (:size (the value-cardinality))
			       :pass-down (root-object tatu-root click-mode)
			       :value (the (get-value-element (the-child index)))))
  
  :hidden-objects
  ((nodes :type 'newer-inspector ;;inspector-node 
	   :depth (1+ (the depth))
	   :sequence (:size (length (the messages)))
	   :respondent (the respondent)
	   :message (nth (the-child index) (the messages))
	   :parent-node (the node)
	   :pass-down (
		       tatu-root root-object
		       tree-toplevel tatu-root colors-default 
		       tree-type tree-node display-controls-hash
		       perform-action-function click-mode  relative-font-size))

   
   (settables-form :type 'settables-form
		   :pass-down (show-settables?)))

  
  :functions
  (   
   
   (get-value-element
    (index)
    (case (the value-type)
      (:list (nth index (the value)))
      (:gdl-sequence (the value (get-member index)))))
  
   (filter-messages 
    (message-list)
    (safe-sort
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
      message-list)
     #'string<))
   
   
   
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
    
    (multiple-value-bind (value error) (ignore-errors (the node (evaluate message)))
      (let* ((error? (and error (typep error 'error))) (value (if  error? (format nil "ERROR: ~a" error) value)))
			 
	(when error? (the node (set-slot! message value))))))
   
   (set-object! (object) (the (set-slot! :node-root-path (the-object object root-path))))))



(define-object settables-form (base-html-sheet)
  
  :input-slots
  (show-settables?))



(define-lens (html-format settables-form)()
  :output-functions
  ((main-view
    ()
    (with-cl-who ()
      (htm
       ((:span :style "cursor: pointer; color: blue; font-style: oblique;"
	       :onclick (the (gdl-ajax-call :function-key :set-slot!
					    :arguments (list :show-settables? (not (the show-settables?))))))
	(str (if (the show-settables?) "X" "Show Settables!"))))))))




(define-object viewable-link (base-html-sheet)
  :input-slots (node message)
  
  :computed-slots ((strings-for-display (format nil "~a" (the message))))
  
  :functions
  ((before-present!
    ()
    (ignore-errors-with-warning (the node (evaluate (the message)))))))


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



|#
