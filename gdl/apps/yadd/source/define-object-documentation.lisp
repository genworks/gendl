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

(in-package :yadd)

(defvar *debug?* nil)

(defparameter *adsense?* nil)

(define-object object-doc (doc-links-mixin)


  :input-slots
  ((show-supported-flag nil))

  :computed-slots
  ((heading "Object Definitions")
   (part-type 'object-dokumentation)
   (symbols (remove-duplicates
             (let (result (package-object (the :package-object)))
               (do-symbols (symbol (the :package)
                             (sort (nreverse result) #'string< :key #'symbol-name))
                
                 (when (and (eql (symbol-package symbol) package-object)
                            (typep symbol 'gdl-object-symbol)
                            (or (gdl::gdl-documentation (find-class symbol))
                                (gdl::message-documentation (find-class symbol))))
                   (push symbol result))))))
   
   (symbols-all (remove-duplicates
                 (let (result (package-object (the :package-object)))
                   (do-symbols (symbol (the :package)
                                 (sort (nreverse result) #'string< :key #'symbol-name))
                     (when (and (eql (symbol-package symbol) package-object)
                                (typep symbol 'gdl-object-symbol))
                       (push symbol result))))))
   
   (symbols-external (remove-duplicates
                      (let (result (package-object (the :package-object)))
                        (do-external-symbols (symbol
                                                 (the :package)
                                               (sort

                                                (nreverse result)
                                                #'string<

                                                :key
                                                #'symbol-name))
                          (when (and (eql (symbol-package symbol) package-object)
                                     (typep symbol 'gdl-object-symbol)
                                     (or (gdl::gdl-documentation (find-class symbol))
                                         (gdl::message-documentation (find-class symbol))))
                            (push symbol result))))))
   
   
   (symbol-ht (let ((ht (make-hash-table :size (the dokumentation-all number-of-elements))))
                (dolist (dok (list-elements (the dokumentation-all)) ht)
                  (setf (gethash (the-object dok symbol) ht) dok)))))

  :trickle-down-slots
  (show-supported-flag))


(define-object object-dokumentation (remark-writers-mixin base-yadd-sheet)

  :input-slots
  ((show-supported-flag nil)
   (instance-supplied nil)
   (part-symbol-supplied nil)
   (part-package-supplied nil)
   (part-package (cond ((the :part-package-supplied) (the :part-package-supplied))
                       ((the :instance-supplied)
                        (package-name (symbol-package (the
                                                          :instance-supplied
                                                        :type))))
                       ((symbolp (the :symbol))
                        (package-name (symbol-package (the :symbol))))
                       (t (package-name *package*))))

   ;;
   ;; FLAG -- append with the symbols-for-index from each section.
   ;;
   (symbols-for-index (cons (list self (format nil "~a" (the symbol)))
                            (apply #'append (mapsend (list-elements (the sections)) :symbols-for-index))))
   
   (symbol (cond ((the :part-symbol-supplied) (the :part-symbol-supplied))
                 ((the :instance-supplied) (the :instance-supplied :type))
                 (t nil)))
   (instance (progn (when (or (and (the :instance-supplied)
                                   (the :part-symbol-supplied))
                              (and (the :instance-supplied)
                                   (the :part-package-supplied)))
                      (error "
If you specify :instance-supplied, do not specify :part-symbol-supplied or :part-package-supplied.

If you specify :part-symbol-supplied, do not specify :instance-supplied."))
                    (or (the :instance-supplied)
                        (make-canonical-part (the :part-full-symbol)))))
   ;;(inherit? t
   (inherit? nil)
   
   (show-all-messages? t))

  :computed-slots
  (
   #+nil
   (definition `(define-object ,(the part-full-symbol) (,@(the mixins-list))
                  ,@(mapcan #'(lambda(section)
                                (list (the-object section category)
                                      (the-object section bindings)))
                     (list-elements (the sections)))))
   

   
   
   (main-sheet-body
    (with-cl-who-string ()
      (str (the main-section main-div))))
   
   
   (documentation (with-output-to-string (*html-stream*)
                    (the write-documentation)))
   
   
   
   (pretty-definition 
    (let ((*package* (find-package (the part-package))))
      (with-cl-who-string ()
        (:pre
         ((:div :class "gdl-object-def")
          "(" ((:span :class "gdl-operator") "define-object") "&nbsp;"
          ((:span :class "gdl-defined-symbol") (str (the part-full-symbol)))
          (fmt " (~{~a~^ ~})" (remove 'vanilla-mixin (the mixins-list))))
          
         (mapc #'(lambda(section) 
                   (str (with-output-to-string (*html-stream*)
                          (write-the-object section pretty-definition))))
               (list-elements (the code-sections))) ")"))))
   
   
   (strings-for-display (format nil "~a" (the :part-full-symbol)))
   
   (strings-for-display-verbose (format nil "Dokumentation for ~s"
                                        (the :part-full-symbol)))
   
   
   
   (part-full-symbol (read-from-string
                      (string-append (format nil "~a" (the :part-package)) "::"
                                     (format nil "~a" (the :symbol)))))
   (part-messages (the instance
                    (message-list :category :all 
                                  :message-type (if (the :inherit?) :global :local) 
                                  :return-category? t
                                  :sort-order :by-category :filter
                                  #'(lambda (category msg)
                                      (declare (ignore category))
                                      (not (eql :documentation msg))))))
   
   (category-headings (list :required-input-slots "Input Slots (required)"
                            :optional-input-slots "Input Slots (optional)"
                            :settable-optional-input-slots "Input Slots (optional, settable)"
                            :defaulted-input-slots "Input Slots (optional, defaulting)"
                            :settable-defaulted-input-slots "Input Slots (optional, defaulting, settable)"
                            :settable-computed-slots "Computed Slots (settable)"
                            :computed-slots "Computed Slots" :trickle-down-slots
                            "Trickle-down Slots" :query-slots "Query Slots"
                            :objects "Objects" :quantified-objects
                            "Objects (sequence)" :hidden-objects "Hidden Objects"
                            :quantified-hidden-objects "Hidden Objects (sequence)"
                            :functions "GDL Functions"))
   
   (relevant-categories (let ((all-categories
                               (plist-keys (the category-headings)))
                              (found-categories
                               (plist-values (the part-messages))))
                  
                          (remove-if-not
                           #'(lambda (category)
                               (member category found-categories))
                           all-categories)))
   
   (part-documentation-plist (the :instance (:documentation)))
   
   
   (example-code (let ((examples (getf (the part-documentation-plist) :examples)))
                   (when examples 
                     (let ((start-pre (search "<pre>" examples))
                           (end-pre (search "</pre>" examples)))
                       (when (and (numberp start-pre) (numberp end-pre))
                         (subseq examples (+ start-pre 5) end-pre))))))
   
   (mixins-list (the :instance (:mixins :local? t)))
   
   ;;(show-vrml? (getf (the instance %renderer-info%) :vrml?))
   
   
   (show-vrml? nil)
   
   (load-example (progn (with-open-file (out (the lisp-file) :direction :output
                                         :if-exists :supersede 
                                         :if-does-not-exist :create)
                          (princ (the example-code) out))
                        (let (*redefinition-warnings*) (load (compile-file (the lisp-file))))) :uncached)
   
   (image-file (merge-pathnames (funcall gdl::*make-temp-file-name-func*) #p"*.png"))
   
   (image-url (format nil "/images/~a" (make-pathname :name (pathname-name (the image-file))
                                                      :type (pathname-type (the image-file)))))
   (lisp-file (make-pathname :directory (pathname-directory (the image-file))
                             :name (pathname-name (the image-file))
                             :device (pathname-device (the image-file))
                             :type "lisp"))
   
   (code-section-keywords '(:input-slots (:required-input-slots 
                                          :optional-input-slots
                                          (:settable-optional-input-slots :settable)
                                          (:defaulted-input-slots :defaulting)
                                          (:settable-defaulted-input-slots :settable :defaulting))
                            :computed-slots (:computed-slots 
                                             (:settable-computed-slots :settable)
                                             (:uncached-computed-slots :uncached))
                            :objects (:objects :quantified-objects)
                            :hidden-objects (:hidden-objects :quantified-hidden-objects)
                            :functions (:functions)
                            :methods (:methods))))
  
  :objects
  ((sections :type 'object-dokumentation-category
             :sequence (:size (length (the :relevant-categories)))
             :category (nth (the-child :index) (the :relevant-categories))
             :heading (getf (the category-headings) (the-child category))
             :instance (the :instance)
             :inherit? (the :inherit?))
   
   
   (code-sections :type 'code-section
                  :sequence (:size (length (plist-keys (the code-section-keywords))))
                  :all-sections (list-elements (the sections))
                  :section-keys (getf (the code-section-keywords) (the-child keyword))
                  :keyword (nth (the-child index) (plist-keys (the code-section-keywords)))))
  
  :hidden-objects
  (
   (main-section :type 'sheet-section
                 :main-view (with-cl-who-string
                                ()
                              
                              (when *developing?* (htm (:p (str (the devo-links-string)))))
                              
                              (when (the :return-object) 
                                (htm (:p (str (the (back-link 
                                                    :display-string "Package Documentation"))))))
                              
                              (str (the :documentation))
                              
                              
                              
                                
                              
                              (when (the :return-object) (htm (:p (str (the (back-link
                                                                             :display-string 
                                                                             "Package Documentation"))))))
                              
                              ;;(:p (str (the pretty-definition)))
                              
                              (:p (str (the :footer)))))
   
   (vrml-sample :type (if (the show-vrml?)
                          (with-input-from-string (ss (the example-code)) 
                            (let ((*package* (find-package (second (read ss)))))
                              (intern (symbol-name (second (read ss))) *package*)))
                        'null-part))
   
   (vrml-view :type (if (the show-vrml?) 'web-drawing 'null-part)
              ;;:view-default (getf (the instance %renderer-info%) :view-default :trimetric)
              :object-roots (list (the vrml-sample)))
   
   
   (vrml-sheet :type (if (the show-vrml?) 'vrml-sheet 'null-part)
               :pass-down (vrml-view load-example))
   
   )

  
  :functions
  (   
   #+nil
   (write-html-sheet
    nil
    (html (:html
           (:head
            (the default-header-content)
            (:title "Documentation for "
                    (:princ (string (the :part-full-symbol)))
                    " in Package "
                    (:princ
                     (string (package-name (the :part-package))))))
           (:body (when *developing?* (html (:p (the (write-development-links)))))
                  ;;(when *adsense?* (html (:p (:princ *adsense-code*))))
                  (when (the :return-object) (html (:p (the (:write-back-link :display-string "&lt;-Back")))))
                  (the (:write-documentation))
                  (when (the :return-object) (html (:p (the (:write-back-link :display-string "&lt;-Back")))))
                  (:p (the :write-footer))))))

   (write-documentation
    nil
    (html ((:table :bgcolor "#eeeeff" :width "100%" :border 0 :cellpadding 1
                   :cellspacing 0)
           (:tr
            (:td
             (:b
              ((:font :size "+1") "Object: "
                                  (:princ (string (the :part-full-symbol)))
                                  ((:font :size -1)
                                   (:i " (The :"
                                       (:princ
                                        (string (package-name (the :part-package))))
                                       " Package)"))))))
           (:tr
            (:td (:b "Mixins: ")
                 (format *html-stream* "~{~a~^, ~}"
                         (mapcar #'(lambda (sym)
                                     (when *debug?* (print-variables sym))
                                     (let ((page-object (gethash sym (the index-ht))))
                                       (cond ((eql sym 'standard-object)
                                              (with-output-to-string (ss)
                                                (html-stream
                                                 ss
                                                 ((:a
                                                   :href
                                                   "http://www-2.cs.cmu.edu/Groups/AI/html/hyperspec/HyperSpec/Body/cla_standard-object.html")
                                                  (:princ
                                                   (string sym))))))
                                             (page-object
                                              (with-output-to-string (ss)
                                                (html-stream
                                                 ss
                                                 ((:a
                                                   :href
                                                   (the-object page-object url))
                                                  (:princ
                                                   (string sym))))))
                                             (t (string sym)))))
                                 (the mixins-list))))))
          (dolist (keyword
                      (remove :examples (safe-sort *allowed-part-documentation-keywords* #'string<)))
            (when (getf (the :part-documentation-plist) keyword)
              (html (:p (:h3 (:princ (format nil "~@(~a~)" keyword)))
                        (let ((text (getf (the :part-documentation-plist) keyword)))
                          (html (:princ text)))))))
          (let ((trickle-down-slots
                 (safe-sort (remove-if-not
                             #'(lambda (message)
                                 (let ((remark
                                        (the instance
                                          (message-remarks message))))
                                   (and remark
                                        (eql (first remark)
                                             (the part-full-symbol)))))
                             (the instance
                               (message-list :category :trickle-down-slots
                                             :message-type :local)))
                            #'string-lessp)))
            (when trickle-down-slots
              (html ((:table :width "100%" :border 0 :cellpadding 1 :cellspacing 1)
                     ((:tr :bgcolor "#eeeeff")
                      ((:td :colspan 2)
                       (:b ((:font :size "+1") "Trickle-Down Slots"))))
                     (:tr
                      ((:td :align :left)
                       (format *html-stream* "~{~a~^, ~}"
                               trickle-down-slots)))))))
          (mapc #'(lambda (section)
                    (let ((values (the-object section message-and-remarks)))
                      
                      (when values
                        (html ((:table :width "100%" :border 0 :cellpadding 1
                                       :cellspacing 0)
                               ((:tr :bgcolor "#eeeeff")
                                ((:td :colspan 2)
                                 (:b
                                  ((:font :size "+1")
                                   (:princ (the-object section heading))))))
                               (:tr (:td :br))
                               (dolist (value values)
                                 (when (or (the :show-all-messages?)
                                           (the
                                               (:supported?
                                                (getf (fourth value) :type))))
                                   (html (:tr
                                          ((:td :align :left)
                                           (:b
                                            (:princ
                                             (first value)))
                                           (when (sixth value)
                                             (html " [" (:i (:princ (sixth value))) "] "))
                                           (when
                                               (getf (fourth value) :type)
                                             (html
                                              " "
                                              (:i
                                               (the
                                                   (:write-type
                                                    (replace-substring
                                                     (getf (fourth value) :type)
                                                     "-dot-"
                                                     ".")
                                                    :show-supported-flag?
                                                    (the :show-supported-flag))))))
                                           
                                           (when (fifth value)
                                             (html (:pre (:prin1 (fifth value)))))
                                           )
                                          ((:td :align :right) :br))
                                         (:tr
                                          ((:td :colspan 2)
                                           (the
                                               (:write-remark-body (fourth value)))))
                                         (:tr ((:td :colspan 2) :br)))))
                               (:tr :br))))))
                (list-elements (the sections)))
          
          
          (when (getf (the :part-documentation-plist) :examples)
              (html (:p (:h3 (:princ (format nil "~@(~a~)" :examples)))
                        (let ((text (getf (the :part-documentation-plist) :examples)))
                          (html (:princ text))))))
          
          
          (when (and (the example-code) (not (search "(generate-sample-drawing" (the example-code))))
            (warn "Sample code found in ~s, but no call to generate-sample-drawing was found."
                  (the part-full-symbol)))
          
          (when (and (the example-code) 
                     (or (search "(generate-sample-drawing" (the example-code))
                         (search "(with-format (pdf" (the example-code))))
            (let ((image-file (the image-file)) (image-format :png)
                  (pdf-file (merge-pathnames "example.pdf" 
                                             #+allegro (sys:temporary-directory) 
                                             #-allegro sys::*temp-directory*))
                  (url (the image-url)) (lisp-file (the lisp-file)))
              
              
              (publish :path url
                       :content-type "image/png"
                       :format :binary
                       :function #'(lambda(req ent)
                                     (format t "~&~%Loading example code and generating example image for ~s...~%"
                                             (the part-full-symbol))
                                     (the load-example)
                                     (let ((command (format nil "\"~a\" -q -sDEVICE=~a \"-sOutputFile=~a\" -dTextAlphaBits=~a -dGraphicsAlphaBits=~a -dSAFER -dBATCH -dNOPAUSE  \"~a\""
                                                            *gs-path* (ecase image-format
                                                                        (:png "png16m")
                                                                        (:gif "gif")
                                                                        ((:jpg :jpeg) "jpeg"))
                                                            image-file
                                                            *gs-text-alpha-bits*
                                                            *gs-graphics-alpha-bits*
                                                            pdf-file)))
                                       
                                       (let ((return-value
                                              (#+allegro 
                                               run-shell-command
                                               #+lispworks system:call-system
                                               #+cmu asdf:run-shell-command
                                               command
                                               #+allegro :show-window
                                               #+allegro :hide)))
                                     
                                         (with-http-response (req ent)
                                           (setf (reply-header-slot-value req :cache-control) "no-cache")
                                           (setf (reply-header-slot-value req :pragma) "no-cache")
                                           (with-http-body (req ent)
                                             (let
                                                 ((reply-stream (request-reply-stream req)))
                                               (with-open-file 
                                                   (image-stream image-file :element-type '(unsigned-byte 8))
                                                 (do ((val (read-byte image-stream nil nil)
                                                           (read-byte image-stream nil nil)))
                                                     ((null val))
                                                   (write-byte val reply-stream))))))
                                       
                                         (when (zerop return-value)
                                           (delete-file pdf-file)
                                           (delete-file image-file)
                                           (delete-file lisp-file)
                                           (delete-file (make-pathname :name (pathname-name lisp-file)
                                                                       :type gdl::*fasl-default-type*
                                                                       :directory (pathname-directory lisp-file)
                                                                       :device (pathname-device lisp-file))))))))
              
              (push url (gethash (make-keyword (the instance-id)) gwl::*url-hash-table*))
              
              (html ((:table   :cellpadding 1)
                     ((:tr :bgcolor "#cccccc") (:td ((:img :src url)))
                                 (when (the show-vrml?)
                                   (html ((:td :valign :top) 
                                          (:ul (:li (the vrml-sheet (write-self-link :display-string "VRML...")))
                                               (:li ((:font :color :grey) "RenderMan...")))))))))))))))



(define-object code-section ()
  :input-slots (keyword all-sections section-keys)
  
  
  :computed-slots ((sections-and-modifiers (remove 
                                            nil
                                            (mapcar #'(lambda(key-group)
                                                        (let ((key (first (ensure-list key-group))))
                                                          (let ((section (find key (the all-sections) 
                                                                               :key #'(lambda(section)
                                                                                        (the-object section category)))))
                                                            (when section
                                                              (list section 
                                                                    (rest (ensure-list key-group))))))) (the section-keys)))))
  
  :objects
  ((sections :type 'code-section-section
             :sequence (:size (length (the sections-and-modifiers)))
             :objects? (member (the keyword) '(:objects :quantified-objects :hidden-objects :quantified-hidden-objects))
             :functions? (member (the keyword) '(:functions :cached-functions :methods :cached-methods))
             :key-group (nth (the-child index) (the sections-and-modifiers))
             :pseudo-inputs (key-group)
             :section (first (the-child key-group))
             :modifiers (second (the-child key-group)))))


(define-object code-section-section ()
  :input-slots (section modifiers objects? functions?)
  
  :computed-slots ((message-objects (list-elements (the section messages))))
  
  
  :objects
  ((messages :type 'code-section-message
             :pass-down (objects? functions? section)
             :sequence (:size (length (the message-objects)))
             :message-object (nth (the-child index) (the message-objects)))))


(define-object code-section-message ()
  :input-slots
  (message-object objects? functions?
   section
   (source-indent 4)
   (print-margin 70)
   )

  
  :computed-slots
  ((message-name-length (length (string (the message-name))))
   
   (source-string (with-output-to-string (ss) (let ((*print-right-margin* (the print-margin))
                                                    *print-level* *print-length*)
                                                (when (the functions?) ;;(the lambda-list)
                                                  (if (the lambda-list)
                                                      (pprint (the lambda-list) ss)
                                                    (princ "()" ss)))
                                                (if (the functions?) ;;(the lambda-list)
                                                    (mapc #'(lambda(sexpr)
                                                              (pprint sexpr ss)) (the source))
                                                  (pprint (first (the source)) ss)))))
                    

   (source-object-inputs (mapcar #'(lambda(key expression)
                                     (list (make-keyword key) (let ((*print-right-margin* (the print-margin))
                                                                    *print-level* *print-length*)
                                                                (subseq (with-output-to-string(ss)
                                                                          (pprint expression ss)) 1))))
                                 (plist-keys (first (the source)))
                                 (plist-values (first (the source)))))
                                 
   (source-strings (unless (the objects?) (strings-to-list (the source-string))))
   
   
   (source-first-length (length (first (the source-strings))))
   (source-max-length (apply #'max (mapcar #'length (the source-strings))))
   
   (source-strings-indented (apply #'string-append (indent-lines (the source-strings)
                                                                 :first-indent 1
                                                                 :rest-indent (+ (the message-name-length) 5))))
   (message-name (the message-object message-name))
   (instance (the message-object instance))
   (first-message? (and (the first?) (the parent first?)))
   (last-message? (and (the last?) (the parent last?)))
   (source-expression (let ((expr (the instance (slot-source (the message-name)))))
                        (if (the functions?) (second expr) (rest expr))))
   (lambda-list (when (the functions?) (first (the source-expression))))
   (source (if (the functions?)
               (rest (the source-expression))
             (the source-expression)))))
             



(define-object object-dokumentation-category (;;base-object
                                              )

  :input-slots
  (category
   heading
   instance
   inherit? )

  :computed-slots
  (
   
   (do-inherit? (and (the inherit?)
                     (member (the category)
                             (list :required-input-slots :optional-input-slots :settable-optional-input-slots :defaulted-input-slots
                                   :settable-defaulted-input-slots))))
   (url (the parent url))
   
   (strings-for-display (string (the :category)))
   (message-and-remarks (remove nil
                                (mapcar #'(lambda
                                              (a)
                                            (when
                                                (the-object a :remark-string)
                                              (list
                                               (the-object a :message-name)
                                               (the-object a :remark-string)
                                               (the-object a :remark-mixin)
                                               (the-object a :section-plist)
                                               nil
                                               (the-object a inheritance-note)
                                               ;;(the-object a source-code)                                            
                                               )))
                                        (list-elements (the :messages)))))
   
   (bindings 
    `(,@(mapcar #'(lambda(message)
                    (cons (the-object message message-name)
                          (the-object message source)))
                  (list-elements (the messages)))))

   
   (symbols-for-index (remove-if-not 
                       #'(lambda(message)
                           (the instance (message-remarks (second message))))
                       (mapcar #'(lambda(message)
                                   (list message (the-object message message-name)))
                               (list-elements (the messages)))))
   
   (message-names (if (defaulting (the :instance :ignore-messages-from) nil)
                      (let (rlist)
                        (dolist (symbol (the :instance :ignore-messages-from)
                                  rlist)
                          (let ((msg-list
                                 (the :instance
                                   (:message-list :category (the :category)
                                                  :message-type
                                                  (if (the do-inherit?) :global :local)
                                                  :return-category? nil :base-part-type
                                                  symbol :sort-order :by-name))))
                            (if rlist
                                (setq rlist (intersection rlist msg-list))
                              (setq rlist msg-list)))))
                    (the :instance
                      (:message-list :category (the :category) :message-type
                                     (if (the do-inherit?) :global :local) :return-category?
                                     nil :sort-order :by-name)))))

  :objects
  ((messages :type 'object-dokumentation-category-message
             :sequence (:size (length (the :message-names)))
             :message-name (nth (the-child :index) (the :message-names))
             :pass-down (:category :instance))))

(define-object object-dokumentation-category-message (doc-string-parser-mixin
                                                      
                                                      base-html-sheet
                                                      
                                                      ;;base-object
                                                      )

  :input-slots
  (message-name
   category
   instance)

  :computed-slots
  ((strings-for-display (string (the :message-name)))
   
   (source-code (the instance (slot-source (the message-name))))
   
   (url (the parent url))
   (dok-type (format nil "GDL object message on ~s" (the instance type)))
   (symbol (the message-name))
   (package (symbol-package (the instance type)))
   
   (remark-list (let ((try
                       (the :instance (:message-remarks (the :message-name)))))
                  (if (keywordp (first try)) (second try) try)))
   
   (remark-string (second (the remark-list)))
   
   (inheritance-note (unless (eql (the remark-mixin) (the instance type))
                       (let ((note (with-cl-who-string ()
                                     "from "
                                     (let ((page-object (gethash (the remark-mixin) (the index-ht))))
                                       (htm ((:a :href (the-object page-object url))
                                             (str (the remark-mixin))))))))
                         (print-variables note)
                         note)))
   
   (remark-mixin (first (the :remark-list)))))



(define-lens (html-format object-dokumentation)()
  :output-functions
  ((in-package-form
    ()
    (let ((*package* (find-package (the part-package))))
      (html (:pre
             ((:div :class "gdl-object-def")
              "(in-package " (:prin1 (make-keyword (package-name (find-package *package*)))) ")")))))
    
   
   (pretty-definition 
    ()
    (let ((*package* (find-package (the part-package))))
      (html-stream *stream*
                   (:pre
                    ((:div :class "gdl-object-def")
                     "(" ((:span :class "gdl-operator") "define-object") "&nbsp;"
                     ((:span :class "gdl-defined-symbol") (:princ (the part-full-symbol)))
                     (:princ (format nil " (~{~a~^ ~})" (remove 'vanilla-mixin (the mixins-list))))
           
                     (mapc #'(lambda(section) (write-the-object section pretty-definition))
                           (list-elements (the code-sections)))
           
                     ")")))))))

(define-lens (html-format code-section) ()
  :output-functions
  ((pretty-definition
    ()
    (when (list-elements (the sections))
      (html :br 
            "  "
            ((:span :class "gdl-section-keyword")
             (:prin1 (the keyword))) :br
            "  ("
            (dolist (section (list-elements (the sections)))
              (write-the-object section pretty-definition)) ")")))))


(define-lens (html-format code-section-section)()
  :output-functions
  ((pretty-definition
    ()
    (dolist (message (list-elements (the messages)))
      (write-the-object message pretty-definition)))))


(define-lens (html-format code-section-message)()
  :output-functions
  ((pretty-definition
    ()
    
    (html 
     (:princ (if (and (the first?) (the parent first?))
                 "" "   "))
     (:princ (if (member (the section category) (list  :required-input-slots)) "" "("))
     ((:span :class "gdl-message-name") (:princ (the message-name))))

    
    (if (the objects?)
        (progn
          (dolist (pair (the source-object-inputs))
            (html :br
                  (:princ "    ")
                  ((:span :class "gdl-object-input-key") (:prin1 (first pair)))
                  (:princ (apply #'string-append
                                 (indent-lines (strings-to-list (second pair)) 
                                               :first-indent 1 
                                               :rest-indent  (+ 6 (length (string (first pair)))))))))
          (html ")" 
                (unless (and (the last?) (the parent last?)) (html :br))
                ;;(html :br)
                
                ))
      (progn
        (unless (member (the section category) (list  :required-input-slots))
          (let ((source-string (the source-strings-indented)))
            (html (:princ source-string) ")")))
        
        (unless (and (the  last?) (the parent last?)) (html :br))
        
        )))))




(define-object vrml-sheet (base-html-sheet)
  
  :input-slots
  (vrml-view load-example))



(define-lens (html-format vrml-sheet)()
  :output-functions
  ((main-sheet
    ()
    (html
     (:html (:head 
             (the default-header-content)
             (:title "Example in VRML format"))
            (:body (the (write-back-link :display-string "&lt;-Back"))
                   (:h2 (:center "Example in VRML format"))
                   (:p (let ((vrml-url (string-append "/vrml/" 
                                                      (file-namestring 
                                                       (make-pathname 
                                                        :name 
                                                        (pathname-name 
                                                         (funcall gdl::*make-temp-file-name-func*)) :type "wrl")))))
                         (publish :path vrml-url
                                  :content-type "x-world/x-vrml"
                                  :function #'(lambda(req ent)
                                                (the load-example)
                                                (with-http-response (req ent)
                                                  (setf (reply-header-slot-value req :cache-control) "no-cache")
                                                  (setf (reply-header-slot-value req :pragma) "no-cache")
                                                  (with-http-body (req ent)
                                                    (let ((reply-stream (request-reply-stream req)))
                                                      (when *debug?*
                                                        (with-format (vrml "/tmp/try.wrl")
                                                          (write-the vrml-view (cad-output))))
                                                      (with-format (vrml reply-stream)
                                                        (write-the vrml-view (cad-output))))))))
                         (push vrml-url (gethash (make-keyword (the instance-id)) gwl::*url-hash-table*))
                         
                         ;;
                         ;; FLAG -- put on another page
                         ;;
                         (html ((:embed :src vrml-url :width 600 :height 500))
                               (:p (:i "requires VRML plugin, see www.x3d.org for avaialable VRML2 browser plugins.")))))))))))
                       
  

(defun indent-lines (string-list &key first-indent rest-indent)
  (let ((first? t) last? (count -1) (length (length string-list)))
    (mapcar #'(lambda(string)
                (when (= (incf count) (1- length)) (setq last? t))
                
                (prog1
                    (string-append (make-string (if first? first-indent rest-indent)
                                                :initial-element #\space) 
                                   string 
                                   (if last? "" (format nil "~%")))
                  (setq first? nil)))
            string-list)))


(defun strings-to-list (string)
  (let (lines)
    (with-input-from-string (ss string)
      (do ((line (read-line ss nil nil) (read-line ss nil nil)))
          ((null line) (nreverse lines))
        (when (not (string-equal line "")) (push line lines))))))
