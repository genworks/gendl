(in-package :ta2)


(defmacro ignore-errors-with-warning (&rest body)
  `(progn ,@body))

;;
;; FLAG - promote this to utilities before gwl-graphcs.
;;
(defun one-line (string)
  (glisp:replace-regexp (glisp:replace-regexp string (format nil "~%") " ") "'" "\\'" ))

(defparameter *suppress-%%-messages?* t)

(defparameter *internal-keywords* (list :query-plist :aggregate :all-mixins :background-color 
                                        :base64-encoded-root-path :bashee :beziers :bounding-bbox
                                        :check-sanity? :color-decimal :color-hex :color-palette :cookies-received
                                        :cookies-to-send :dash-pattern :dom-id :fill-color-decimal
                                        :fixed-url-prefix :header-plist :home-page :instance-id 
                                        :last-visited-root-path :left-handed? :local-bbox :local-box
                                        :local-center :local-left-handed? :local-orientation :inner-html
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
   (relative-font-size "-1"))

  :computed-slots
  ((dom-id "ta2inspector")
   
   (show-settables? nil :settable)
   
   (node (when (the root-object) (the root-object (follow-root-path (the node-root-path)))))
   
   (tatu-color (or (the (find-color (the node)))
                   (gethash (getf (the node display-controls) :color) *color-table*)))

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
               (append ;;(the messages-local)
                       (set-difference (the :node (:message-list))
                                       (append (the
                                                   :node
                                                 (:message-list :category :methods))
                                               (the :node (:message-list
                                                           :category
                                                           :uncached-attributes))))))
              #'string<))
   
   (check-links nil :settable))

  
  :hidden-objects
  ((settables-form :type 'settables-form
                   :pass-down (node colors-default tatu-root  relative-font-size show-settables?)
                   :inspector-object self)
   
   (viewables-list :type 'viewables-list
                   :pass-down (node colors-default tatu-root  relative-font-size)
                   :inspector-object self))

  
  :functions
  (   
   (find-color
    (node)
    (or (gethash (getf (gethash node (the display-controls-hash)) :color) *color-table*)
        (when (the-object node parent) (the (find-color (the-object node parent))))))
    
   (set-object! (object) (the (set-slot! :node-root-path (the-object object root-path))))))


(define-lens (html-format inspector)()
  :output-functions
  ((main-sheet
    ()
    (html (:html (:head (:title "TaTU Inspector") ((:base :target "_top")))
                 (:body (write-the (inner-html))))))
   
   (inner-html
    ()
    (html-stream *stream* 
                 ((:table :width "100%" :height "100%" :bgcolor (gethash :grey *color-table*))
                  (:tr ((:td :align (if (the node) :left :center) 
                             :valign (if (the node) :top :center))
                        (if (the node) 
                            (write-the inspector-body)
                          (html "No Object Instantiated")))))))
   
   
   (message-value
    (message)
    (let* ((value (ignore-errors-with-warning (the node (evaluate (make-keyword message)))))
           (gdl-object? (eql (class-of (class-of value)) (find-class 'gdl-class))))
      (with-cl-who ()
        ((:span :style (format nil "color: ~a; ~a" 
                               (if gdl-object? "blue" "black")
                               (if gdl-object? "cursor: pointer;" ""))
                :onclick (when gdl-object?
                           (format nil "return ta2operate('~a', '~a');"
                                   (the instance-id)
                                   (base64-encode-safe 
                                    (format nil "~s" (the-object value root-path))))))
         (cond ((consp value) (htm (:pre (esc (with-output-to-string (ss) (pprint value ss))))))
               (gdl-object? (htm (esc (format nil "~s" value))))
               (t (htm (fmt "~s" value))))))))
   
   
   (inspector-body
    ()
    (html-stream *stream*
     (:p ((:font :color (if (the tatu-color) (the tatu-color) :black))
          (:h3 (:princ (the :node :strings-for-display)) :br
               ((:font :size (the relative-font-size))))))
                     
     (:p (write-the settables-form inner-html))

     (:p (write-the viewables-list inner-html))
                     
     (let ((messages (the :messages)))
       (html ((:table :border 1)
              ((:font :size (the relative-font-size))
               (let ((count -1))
                 (dolist (message messages)
                   (incf count)
                   (html (:tr
                          ((:td :bgcolor (getf (the colors-default) :headers))
                           (:b (:princ message)))
                          (:td 
                           ((:span :id (format nil "inspector_~a" message)
                                   :style (format nil "overflow:auto; width: ~a" (the tree width)))
                            (let ((slot-status (the node (slot-status message))))
                              
                              (case slot-status
                                (:unbound 
                                 (html ((:span :style "cursor: pointer; color: blue; font-style: oblique;"
                                               :onclick (format nil "return ta2evaluate('~a', '~a');" 
                                                                (the instance-id) 
                                                                (base64-encode-safe (string message)))) 
                                        "Unbound")))
                                (:evaluated
                                 (write-the (message-value message)))
                                
                                (otherwise (write-the (message-value message)))))))))))))))))))


(define-object settables-form (base-html-sheet)
  :input-slots
  (node inspector-object show-settables? colors-default  tatu-root) 
  
  :computed-slots
  ((settables (let (settables) 
                (maphash #'(lambda(key val)(declare (ignore val)) 
                                  (push key settables)) 
                         (the node %settable-slots%))
                (nconc settables (the node (message-list :category :required-input-slots)))             
                (sort (remove-if
                       #'(lambda (keyword)
                           (or (member keyword *internal-keywords*)
                               (and *suppress-%%-messages?*
                                    (let ((string (string keyword)))
                                      (or (and (> (length string) 2) (string-equal (subseq string 0 2) "$$"))
                                          (and (eql (aref string 0) #\%)
                                               (or (eql (aref string 1) #\%)
                                                   (eql
                                                    (aref string (1- (length string)))
                                                    #\%))))))
                               
                               (let ((value (ignore-errors (the node (evaluate keyword)))))
                                 (or (eql (class-of (class-of value)) (find-class 'gdl-class))
                                     (and (consp value) 
                                          (some #'(lambda(item) (eql (class-of (class-of item)) 
                                                                     (find-class 'gdl-class)))
                                                value))))))
                       settables) #'string<)))

   (respondent (the tatu-root))
   (bashee (the node))))



(define-lens (html-format settables-form)()
  :output-functions
  ((inner-html
    ()
    (when (not (the show-settables?))
      (html
       ((:span :style "cursor: pointer; color: blue; font-style: oblique;"
               :onclick (format nil "return gdlsetcontrol('~a', '~a', 'show-settables?', 't')"
                                (the instance-id) (the inspector-object root-path))) "Show Settables")))
    
    (when (the show-settables?)
      (let ((settables (the settables)))
        (with-html-form (:name "settables_form" :on-submit "return false;")
          (:p
           ((:table :border 1)
            (:tr  ((:td :colspan 2)
                   ((:table :border 0 :width "100%")
                    ((:tr :bgcolor (getf (the colors-default) :headers))
                     ((:td  :align :left) (:b "Settable Slots:"))
                     ((:td  :align :right) 
                      ((:span :style "cursor: pointer; color: blue; font-style: oblique;"
                              :onclick (format nil "return gdlsetcontrol('~a', '~a', 'show-settables?', 'nil')"
                                               (the instance-id) (the inspector-object root-path))) "X"))))))
            (dolist (message settables)
              (let ((remarks (the node (message-remarks message)))
                    (value (ignore-errors-with-warning
                            (the node (evaluate message)))))
                (html (:tr ((:td :bgcolor (getf (the colors-default) :headers)) 
                            (if (null remarks) (html (:b (:princ message)))
                              (html ((:a :href (the tatu-root url)
                                         :title (one-line (second remarks)))
                                     (:b (:princ (string-capitalize message)))))))
                           (:td ((:font :size (the relative-font-size)) 
                                 ((:input :type :text 
                                          ;;:name (replace-regexp* (string message) "-" "_") 
                                          :name (base64-encode-safe (string message))
                                          
                                          :value (format nil "~s" value)))))))))))
          (:p ((:input :type :submit :name :submit :value "Set Slots!"
                       :onclick (format nil "var keys = '(' + ~{'~s '~^ + ~} + ')';
                                             var values = encode64('(' + ~{~a~^ + ' ' + ~} + ')').replace(/=/g,'');
                                             return gdlsetslots('~a', '~a', keys, values);"
                                        settables
                                        (mapcar #'(lambda(message)
                                                    (format nil "document.settables_form.~a.value" 
                                                            
                                                            ;;(replace-regexp* (string message) "-" "_")
                                                            
                                                            (base64-encode-safe (string message))))
                                                settables)
                                        (the instance-id) 
                                        (base64-encode-list (the node root-path) )
                                        
                                        ))))))))))



(define-object viewables-list (base-html-sheet)
  :input-slots
  (node inspector-object colors-default  tatu-root)
  
  :computed-slots
  ((viewables (the node (message-list :category :uncached-computed-slots)))

   (respondent (the tatu-root))
   (bashee (the node)))

  
  :objects
  ((switch-viewables :type 'switch-viewables
                     :return-object (the tatu-root))
   
   (viewable-links :type 'viewable-link
                   :sequence (:size (length (the viewables)))
                   :node (the node)
                   :return-object (the tatu-root)
                   :message (nth (the-child index) (the viewables)))))
  




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



(define-lens (html-format viewables-list)()
  
  :output-functions
  ((inner-html
    ()
    (when (the viewables) (the switch-viewables (write-self-link )))
    (when (the switch-viewables show-viewables?)
      (dolist (link (list-elements (the viewable-links)))
        (html (:li (the-object link write-self-link))))))))
        


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



