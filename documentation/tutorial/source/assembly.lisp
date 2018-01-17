;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :gendl-doc)

(defparameter *data* nil)


(defun initialize-data ()
  (setq *data*
	`((:document :class ("11pt" "book")
		     :textwidth 6.5
		     :topmargin 0
		     :textheight 8.5
		     :oddsidemargin 0
		     :evensidemargin 0
		     :pdfimageresolution "135"
		     :usepackage ("datetime")
		     :title "Genworks GDL: A User's Manual"
		     :date ("\\monthname\\ \\the\\year")
		     :author "Dave Cooper"
		     :usepackage ("dvips" "graphicx")
		     :usepackage ("usenames, dvipsnames" "color")
		     :usepackage ("makeidx")
		     :usepackage ("textcomp")
		     :usepackage ("colorlinks=true, urlcolor=cyan" "hyperref")
		     :usepackage ("nottoc, numbib" "tocbibind")
		     :newsavebox ("\\boxedverb")
		     :makeindex nil)
	  :frontmatter
	  :maketitle
	  (:footnotetext "Copyright "
			 (:copyright)
			 " 2012, Genworks International. Duplication, by any means, in whole or in part, requires 
written consent from Genworks International.")
	  :tableofcontents
	  :mainmatter
	  ,*introduction*
	  ,*installation*
	  ,*basic-operation*
	  ,*understanding-common-lisp*
	  ,*understanding-gendl*
	  ,*tasty-environment*
	  ,*gendl-geometry*
	  ,*custom-user-interfaces*
	  ;;,*advanced-common-lisp*
	  ;;,*advanced-gendl*
	  ,*upgrade-notes*
	  ,(the-object (make-self 'yadd::assy) dom-chapter)
	  :backmatter
	  ,*bibliography*      
	  :printindex
	  )))



(defmacro run-prog (command)
  `(multiple-value-bind (output error return-code)
       (uiop:run-program ,command :output :string :error :string :ignore-error-status t)
     (when (or error (not (zerop return-code)))
       (format t "Command was: ~a~%" ,command)
       (warn "Error was ~a, ~a and return-code was ~a" output error return-code))))

(defun make (&optional (level 1))

  (let ((pdf-path (merge-pathnames "pdf/" *system-home*))

	(pdftex-path (cond ((probe-file "/usr/texbin/pdflatex")
			    "/usr/texbin/pdflatex")
			   ((probe-file "/opt/local/bin/pdflatex")
			    "/opt/local/bin/pdflatex")
			   ((probe-file "/Library/TeX/texbin/pdflatex")
			    "/Library/TeX/texbin/pdflatex")))
	(makeindex-path (or (probe-file "/usr/texbin/makeindex")
			    (probe-file "/opt/local/bin/makeindex")
			    (probe-file "/Library/TeX/texbin/makeindex"))))
			    


    (load (merge-pathnames "../source/assembly.lisp" pdf-path))

    (initialize-data)

    (let ((object (make-part 'com.genworks.dom:assembly :data *data*)))
      (ensure-directories-exist (translate-logical-pathname pdf-path))
      (with-format (com.genworks.dom-writers:latex (merge-pathnames "pdf/tutorial.tex" *system-home*))
	(write-the-object object (:base))))
    
    (run-prog (format nil "cd '~a'; ~a -interaction=nonstopmode tutorial.tex"  pdf-path pdftex-path))
    (run-prog (format nil "cd '~a'; ~a tutorial" pdf-path makeindex-path))

    (dotimes (n level)
      (run-prog (format nil "cd '~a'; ~a -interaction=nonstopmode tutorial.tex" 
			pdf-path pdftex-path)))))

(define-object assembly (com.genworks.dom:assembly)
  :input-slots ((data *data*)
		(style-url "tutorial.css")))


(publish :path "/dom-doc"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "gendl-doc:assembly")))


(defun make-html (&key (output-directory (merge-pathnames "tmp/" (user-homedir-pathname)))
		  (output-file-namestring "tutorial.html"))
  (ensure-directories-exist output-directory) 
  (initialize-data)
  (let ((self (make-object 'assembly)))
    (let ((output-path (merge-pathnames output-file-namestring output-directory)))
      (with-format (html-format output-path) (write-the cl-who-out))
      output-path)))


(defun make-css (&key (output-directory (merge-pathnames "tmp/" (user-homedir-pathname)))
		 (output-file-namestring "tutorial.css"))
  (let ((output-path (merge-pathnames output-file-namestring output-directory)) 
	(css-string 
	 (reduce 
	  (lambda (x y) 
	    (concatenate 'string x y))
	  (mapcar 
	   (lambda (style)
	     (format nil "~a{~a}" 
		     (car style)
		     (format nil "~{~a~}" 
			     (mapcar (lambda (pair)
				       (format nil "~a:~a;" 
					       (car pair)
					       (cadr pair))) 
				     (cdr style)))))
	   *styles*))))

    (with-open-file (stream output-path 
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (format stream css-string)
      output-path)))

      
(defun make-html-and-css (&key (output-directory (merge-pathnames "tmp/" (user-homedir-pathname)))
		 (html-output-file-namestring "tutorial.html")
			    (css-output-file-namestring "tutorial.css"))
  (let ((output-images (merge-pathnames "images/" output-directory)))
    (when (probe-file output-images)
      (uiop/filesystem:delete-directory-tree output-images :validate t))
    (com.genworks.lisp:copy-directory 
     (merge-pathnames "documentation/tutorial/images/"
		      (if (asdf:system-source-directory :gendl)
			(or (probe-file (asdf:system-source-directory :gendl))
			    (probe-file "~/genworks/gendl/"))  (probe-file "~/genworks/gendl/")))
     output-images))
  (let ((html-path (make-html :output-directory output-directory 
			      :output-file-namestring html-output-file-namestring))
	(css-path (make-css :output-directory output-directory 
			    :output-file-namestring css-output-file-namestring)))
    (values html-path css-path)))



(in-package :yadd)

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
          (fmt " (~{~a~^ ~})" (remove 'gdl::vanilla-mixin (the mixins-list))))
          
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
			;;
			;; FLAG -- try to muffle redefinition warnings here. 
			;;
                        (load (compile-file (the lisp-file)))) :uncached)
   
   (image-file (glisp:temporary-file :extension "png"))
   
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
                 :inner-html (with-cl-who-string
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
   (dom-body
    ()

    
    (when (and (the example-code) (not (search "(generate-sample-drawing" (the example-code))))
      (warn "Sample code found in ~s, but no call to generate-sample-drawing was found."
	    (the part-full-symbol)))
          
    (let (pdf-file named-pdf-file (net.html.parser::*ch-format* '(:i :b  :big :small :strike :s :u :em :strong :font)))

      (when (and (the example-code) 
		 (or (search "(generate-sample-drawing" (the example-code))
		     (search "(with-format (pdf" (the example-code))))
	(format t "~&~%Loading example code and generating example image for ~s...~%"
		(the part-full-symbol))

	(the load-example)
	
	(setq pdf-file (merge-pathnames "example.pdf" (glisp:temporary-folder))
	      named-pdf-file (merge-pathnames (format nil "example-~(~a~).pdf" (the part-full-symbol))
					      "~/genworks/gendl/documentation/tutorial/images/"))
	(when (probe-file named-pdf-file) (delete-file named-pdf-file))
	(uiop:copy-file pdf-file named-pdf-file))
      
      `((:p (:textbf (:underline "Mixins:")) " " 
	    ,(format nil "~{~a~^, ~}" (mapcar #'(lambda (sym) (string sym)) (the mixins-list))))
	(:p ((:list :style :description)
	     ,@(mapcar #'(lambda(keyword)
			   `((:item :word (:underline ,(format nil "~@(~a~)" keyword)))
			     (:p ,@(net.html.parser:parse-html (getf (the :part-documentation-plist) keyword)))))
		       (remove-if-not #'(lambda(keyword)
					  (getf (the part-documentation-plist) keyword))
				      (remove :examples (safe-sort *allowed-part-documentation-keywords* #'string<))))))
	
	,(when (the example-code)
	       `((:boxed-figure :caption ,(format nil "Example Code for ~s" (the part-full-symbol))
				:label ,(format nil "fig:example-code-~s" (the part-full-symbol)))
		 (:small (:verbatim ,(the example-code)))))
	       

	,(when named-pdf-file
	       `((:image-figure :image-file ,named-pdf-file
				:width "3in"  :height "3in"
				:caption ,(format nil "~s example" (the part-full-symbol))
				:label ,(format nil "fig:~s" (the part-full-symbol)))))

	(:p ,@(or 
	       (remove 
		nil
		(mapcar #'(lambda (section)
			    (let ((values (the-object section message-and-remarks)))
			      (when values
				`(:p 
				  (:textbf (:underline ,(format nil "~@(~a~):" (the-object section heading))))
				  ((:list :style :description)
				   ,@(mapcar #'(lambda(value)
						 `((:item :word ,(format nil "~@(~a~)" 
									 (first value)
									 #+nil ;; reformat inheritance note
									 (if (sixth value)
									     (format nil "[~a]" (sixth value))
									     "")))

						   (:index ,(format nil "~@(~a~)~%[~(~a~)]" 
								    (first value)
								    (the symbol)))

						   ,(if (getf (fourth value) :type)
							
							#+nil
							`(:emph ,(replace-substring
								  (getf (fourth value) :type)
								  "-dot-" "."))
							
							`(:emph ,(let ((parsed 
									(net.html.parser:parse-html
									 (replace-substring
									  (getf (fourth value) :type)
									  "-dot-" "."))))
								      parsed))
							"")

						   (:p ,@(let ((parsed-intro (net.html.parser:parse-html (getf (fourth value) :intro))))
							      parsed-intro))))
					     values))))))

			(list-elements (the sections))))
	       `(""))))))

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
		       (pdf-file (merge-pathnames "example.pdf"  (glisp:temporary-folder)))
		       (url (the image-url)) (lisp-file (the lisp-file)))
		   (with-all-servers (server)
		       (publish :path url
				:server server
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
						       (glisp:run-program command :show-window? nil)))
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
                                       
						  (when (or (null return-value) (zerop return-value))
						    (delete-file pdf-file)
						    (delete-file image-file)
						    (delete-file lisp-file)
						    (delete-file (make-pathname :name (pathname-name lisp-file)
										:type glisp:*fasl-extension*
										:directory (pathname-directory lisp-file)
										:device (pathname-device lisp-file)))))))))
              
		   (push url (gethash (make-keyword (the instance-id)) gwl::*url-hash-table*))
              
		   (html ((:table   :cellpadding 1)
			  ((:tr :bgcolor "#cccccc") (:td ((:img :src url)))
			   (when (the show-vrml?)
			     (html ((:td :valign :top) 
				    (:ul (:li (the vrml-sheet (write-self-link :display-string "VRML...")))
					 (:li ((:font :color :grey) "RenderMan...")))))))))))))))



(in-package :surf)

(define-object fitted-surface (surface)
  
  :documentation (:description "Fits a surface through a net of points with given degrees and parameterizations. Currently
only interpolated surfaces are supported, this will be extended to allow smooth fitting without the surface necessarily
interpolating (going through) each of the points."
                  
                  :examples "<pre>

 (in-package :gdl-user)

 (define-object c11-test (surface) 

  :input-slots () 

  :computed-slots ()

  :objects
  ((surf-test :type 'fitted-surface
              :hidden nil
              :c11? t
              :points (list (list (make-point -1 0 0)
                                  (make-point 0 0 0) 
                                  (make-point 0.001 0.0 0)
                                  (make-point 1 1 0)
                                  (make-point 1.001 1 0)
                                  (make-point 2 1 0)
                                  (make-point 2.001 1 0)
                                  (make-point 3 2 0)
                                  (make-point 3.001 2.001 0)
                                  (make-point 4 3 0) 
                                  (make-point 5 4 0))   
                            (list
                             (make-point -1 0 1)
                             (make-point 0 0 1) 
                             (make-point 0.001 0.0 1)
                             (make-point 1 1 1)
                             (make-point 1.001 1 1)
                             (make-point 2 1 1)
                             (make-point 2.001 1 1)
                             (make-point 3 2 1)
                             (make-point 3.001 2.001 1)
                             (make-point 4 3 1)
                             (make-point 5 4 1))))))


 (#+allegro excl:without-package-locks #-allegro progn
  (define-object test-fitted-surface (fitted-surface) 

   :input-slots
   ((display-controls (list :color :green-spring :isos (list :n-v 19 :n-u 19)))
   
    (grid-width 4 :settable) (grid-length 4 :settable) (grid-height 4 :settable))
  
   :computed-slots
   ((points (list (list (make-point 0 0 0)
                        (make-point (/ (the grid-width) 4) 0 0)
                        (make-point (half (the grid-width)) 0 0)
                        (make-point (* 3/4 (the grid-width)) 0 0)
                        (make-point (the grid-width) 0 0))
                         
                  (list (make-point 0 (/ (the grid-length) 4) 0)
                        (make-point (/ (the grid-width) 4) 
                                    (/ (the grid-length) 4) 
                                    (/ (the grid-height) 4))
                        (make-point (half (the grid-width)) 
                                    (/ (the grid-length) 4) 
                                    (* (/ (the grid-height) 4) 1.6))
                        (make-point (* 3/4 (the grid-width)) 
                                    (/ (the grid-length) 4) 
                                    (/ (the grid-height) 4))
                        (make-point (the grid-width) 
                                    (/ (the grid-length) 4) 0))
                         
                  (list (make-point 0 (half (the grid-length)) 0)
                        (make-point (/ (the grid-width) 4) 
                                    (half (the grid-length)) 
                                    (* (/ (the grid-height) 4) 1.8))
                        (make-point (half (the grid-width)) 
                                    (half (the grid-length)) 
                                    (the grid-height))
                        (make-point (* 3/4 (the grid-width)) 
                                    (half (the grid-length)) 
                                    (* 3/4 (the grid-height)))
                        (make-point (the grid-width) (half (the grid-length)) 0))
                         
                  (list (make-point 0 (* 3/4 (the grid-length)) 0)
                        (make-point (/ (the grid-width) 4) 
                                    (* 3/4 (the grid-length)) 
                                    (min (* (/ (the grid-height) 4) 
                                            (* (/ (the grid-height) 4) 1.4)) 
                                         (the grid-height)))
                        (make-point (half (the grid-width)) 
                                    (* 3/4 (the grid-length)) 
                                    (min (* (/ (the grid-height) 4) 
                                            (* (/ (the grid-height) 4) 1.8)) 
                                         (the grid-height)))
                        (make-point (* 3/4 (the grid-width)) 
                                    (* 3/4 (the grid-length)) 
                                    (/ (the grid-height) 4))
                        (make-point (the grid-width) 
                                    (* 3/4 (the grid-length)) 0))
                         
                  (list (make-point 0 (the grid-length) 0)
                        (make-point (/ (the grid-width) 4) (the grid-length) 0)
                        (make-point (half (the grid-width)) (the grid-length) 0)
                        (make-point (* 3/4 (the grid-width)) (the grid-length) 0)
                        (make-point (the grid-width) (the grid-length) 0)))))))

 (generate-sample-drawing :objects (make-object 'test-fitted-surface)
                          :projection-direction :trimetric)


 </pre>")
                  
  
  :input-slots
  ("List of lists of 3D Points. The points for fitting, with inner lists representing U direction and outer lists V direction." 
   points 
   
   ("List of 3D vectors of same length as points, or nil. If given, these are the surface normals at each point." 
    normals nil)
   
   
   ("Integer. The starting degree for the fit algorithm in the U direction. Default is 1." 
    u-start 1)
   
   ("Integer. The starting degree for the fit algorithm in the V direction. Default is 1." 
    v-start 1)
   
   ("Integer. The desired degree of the resultant surface in the U direction. Default is 3." 
    u-degree 3)
   
   ("Integer. The desired degree of the resultant surface in the V direction. Default is 3." 
    v-degree 3)
   
   ("Keyword symbol, one of :uniform, :chord-length, :centripetal. The parameterization to use in the resultant surface if
interpolant? is t. Default is :chord-length" 
    parameterization :chord-length)
   
   
   ("Boolean. Indicates whether the surface will interpolate the points. Defaults to t." 
    interpolant? t)
   
   ("Boolean. If interpolated, indicates whether to compute a C11 continuous nonrational bicubic NURBS surface. Defaults to nil."
    c11? nil)
   
   ("Keyword symbol, one of :bessel, :akima.  The method used to compute tangents. Defaults to :akima."
    tangent-method :akima)
   
   ("Number. Tolerance for fit. Defaults to *3d-approximation-tolerance-default*." 
    tolerance *3d-approximation-tolerance-default*))

  
  :computed-slots
  ((native-surface (cond ((the interpolant?)
                          (cond ((the c11?)
                                 (interpolate-c11-surface *geometry-kernel* (the points)
                                                          :tangent-method (the tangent-method)))
                                (t (interpolate-surface *geometry-kernel* (the points) 
                                                        (the u-degree) (the v-degree) 
                                                        (the parameterization)))))
                         ((the normals)
                          (approximate-surface *geometry-kernel* (the points) (the normals)))
                         (t (approximate-surface *geometry-kernel* (the points) (the normals)
                                                 :u-start (the u-start) :u-required (the u-degree)
                                                 :v-start (the v-start) :v-required (the v-degree)
                                                 :tolerance (the tolerance)))))
   
   (bounding-box (bounding-box-from-points (flatten (the points))))))

