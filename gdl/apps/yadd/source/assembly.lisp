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


(define-object assembly (base-yadd-sheet)

  :documentation
  (:author "Dave Cooper (Genworks)"
   :description "``Yet Another Definition Documenter.'' Generates 
documentation for all the relevant packages in the current Lisp session. Presents a 
standard :write-html-sheet method which can also be crawled with a call to 
<pre>

     (gwl:crawl \"yadd:assembly\")

</pre>

The packages to be documented, and whether the green/red supported messages flags show up,
can be controlled with optional-inputs.

The format used to document user-defined objects is not yet fully stabilized, and will 
be documented in a future GDL release.  Currently, YADD has been used to document the internal
GDL and GWL objects, functions, macros, parameters, and constants.

Please contact Genworks if you wish to start using YADD now as a beta-tester, before it is 
fully documented.")

  :input-slots
  (("List of keyword symbols. These packages will be ignored. This list defaults to
            standard internal and test packages"
    packages-to-ignore (list :gwl-user :yadd-test :genworks :yadd-sample :gdl :gwl :yadd :gwl-tree :surf :geom-base))
   
   ("Boolean. This defaults to nil, if it is set to t, only exported symbols will be
            considered for documentation."
    external-only? nil))

  :computed-slots
  ((packages-to-document 
    
    ;;
    ;; FLAG -- look up interesting symbols in a portable way
    ;;
    ;;#-allegro nil

    ;;#+allegro
    (sort
     (remove-duplicates
      (set-difference
       (let (list (autoloads (append (list :win :windows :regexp) 
                                     (remove-duplicates 
                                      #-allegro nil
                                      #+allegro (mapcar #'rest excl::*autoload-package-name-alist*)
                                      ))))
         (do-symbols (sym :keyword list)
           (when (and (not (member sym autoloads)) 
                      (find-package sym)
                      (or (eql sym :gdl) 
                          (member (find-package :gdl) 
                                  (package-use-list 
                                   (find-package sym)))))
             (push sym list))))
       (the :packages-to-ignore))

      :key #'find-package)
     #'string-lessp))
   (relevant-doks (remove-if-not
                   #'(lambda (dok)
                       (or (the-object dok :object-docs :doks)
                           (the-object dok :function-docs :doks)
                           (the-object dok :variable-docs :doks)))
                   (list-elements (the :package-dokumentations))))
   (symbols-for-index (sort
                       (apply #'append (mapcar #'(lambda (package) (the-object package :symbols-for-index))
                                               (the :relevant-doks)))
                       #'string-lessp
                       :key #'second))
   
   (%index-ht (let ((ht (make-hash-table :size (length (the :symbols-for-index)))))
               (dolist (pair (the symbols-for-index) ht)
                 (let ((symbol (intern  (second pair)
                                        (if (typep (first pair) 'object-dokumentation-category-message)
                                            :keyword
                                            (the-object (first pair) package)))))
                   (pushnew (first pair) (gethash symbol ht))))))

   (index-array (let ((array (make-array (list (hash-table-count (the %index-ht)))))
                      (count -1))
                  (maphash #'(lambda(key val)
                               (setf (aref array (incf count))
                                     (list key val)))
                           (the %index-ht))
                  array))

   (index-ht (let ((ht (make-hash-table :size (the master-index-grouped number-of-elements))))
               (dolist (item (list-elements (the master-index-grouped)) ht)
                 (setf (gethash (the-object item key) ht) item))))
                               
   
   (package-ht (let ((ht (make-hash-table :size (the package-dokumentations number-of-elements))))
                 (dolist (package (list-elements (the package-dokumentations)) ht)
                   (setf (gethash (the-object package package) ht) package))))
   
   (title "GDL Reference Documentation")
   
   )


  :trickle-down-slots
  (index-ht)

  :objects
  ((master-index-grouped :type 'index-item
                         :hidden? t
                         :sequence (:size (length (the index-array)))
                         :key (first (aref (the index-array) (the-child index)))
                         :items (second (aref (the index-array) (the-child index))))


   ("package-dokumentation. Quantified, one for each :package-to-document"
    package-dokumentations :type 'package-dokumentation
    :sequence (:size (length (the packages-to-document)))
    :package (nth (the-child :index) (the :packages-to-document))
    :pass-down (:external-only?)
    :show-supported-flag nil)

   
   ("index. Master index of all symbols (objects, functions, parameters, variables, constants)"
    master-index :type 'master-index
    :hidden? t
    :pass-down (:symbols-for-index)))

  :functions
  ((find-object-doc
    (symbol)
    (let* ((package (make-keyword (package-name (symbol-package symbol))))
           (package-doc (when package (gethash package (the package-ht)))))
      (when package-doc (gethash symbol (the-object package-doc object-docs symbol-ht)))))
    

   
   
   (main-sheet-body 
    ()
    (with-cl-who ()
      (when *developing?* (html (:p (the (write-development-links)))))
      (:p
       (:center (:h2 "Reference Documentation")
                (:i "for Genworks" (:sup (:small "&reg;"))
                    " General-purpose Declarative Language, Related Packages, and User packages")))
      (:p
                       
       (:h3 "Documented Packages:")
                       
       (:ul
        (dolist (package (the :relevant-doks))
          (html (:li (the-object package 
                                 (:write-self-link 
                                  :display-string 
                                  (the-object package
                                              strings-for-display-verbose))))))))
      (:p
       (:h3
        (the :master-index
          (:write-self-link :display-string "Master Index"))))
      (:p (str (the :footer)))))
   
   #+nil
   ("Void. Prints toplevel sheet to *html-stream* with listing of packages and link to Master Index."
    write-html-sheet
    ()
    (let ((query (request-query *req*)))
      
      (if query
          (multiple-value-bind (symbol error)
              (ignore-errors (read-safe-string (rest (assoc "object" query :test #'string-equal))))
          (let* ((package (when symbol (make-keyword (package-name (symbol-package symbol)))))
                 (package-doc (when package (gethash package (the package-ht))))
                 (dok (when package-doc (gethash symbol (the-object package-doc object-docs symbol-ht)))))
            (html (:html (:head
                          (the default-header-content)
                          (:title "Definition for " (:princ symbol)))
                         (:body 
                          (if dok
                              (with-format (html-format *html-stream*) 
                                (let ((*package* (find-package package)))
                                  (write-the-object dok pretty-definition)))
                            (html (:h2 "Error: No Definition Found for " 
                                       (:princ symbol)
                                       (when error (html :br (:princ error)))))))))))
        (html (:html
               (:head
                (the default-header-content)
                (:title
                 "Reference Documentation for General-purpose Declarative Language and Related Packages"))
               (:body (when *developing?* (html (:p (the (write-development-links)))))
                      (:p
                       (:center (:h2 "Reference Documentation")
                                (:i "for Genworks" (:sup (:small "&reg;"))
                                    " General-purpose Declarative Language and Related Packages")))
                      (:p
                       
                       (:h3 "Documented Packages:")
                       
                       (:ul
                        (dolist (package (the :relevant-doks))
                          (html (:li (the-object package 
                                                 (:write-self-link 
                                                  :display-string 
                                                  (the-object package
                                                              strings-for-display-verbose))))))))
                      (:p
                       (:h3
                        (the :master-index
                          (:write-self-link :display-string "Master Index"))))
                      (:p (the :write-footer))
                      
                      ))))))))



(define-object master-index (base-yadd-sheet)

  :documentation
  (:author "Dave Cooper (Genworks)"
   :description "Prints bullet list of symbols as links to their documentation pages.")

  :input-slots
  ("List of lists. Each list contains the page object for the symbol's 
   documentation and the symbol's print-name. The list should be sorted 
   based on the symbols' print-names."
   symbols-for-index)

  :computed-slots
  ((use-jquery? t)
   
   
   
   (additional-header-js-content 
    (with-cl-who-string ()
      (str (the additional-header-js))
      ((:script :type "text/javascript")
       "$(document).ready(function () {$('ul li').quicksearch({ attached: 'ul:first',position: 'before', 
labelText: 'Search GDL Documentation:',inputText: 'Enter term...',
loaderImg: '/static/gwl/tasty-unpix/loader.gif',loaderText: 'Narrowing Down...'});});")))
   
   
   (main-sheet-body 
    (with-cl-who-string ()
      (when *developing?* (htm (:p (the (write-development-links)))))
      (:p (:center (:h2 "Index for Relevant GDL Symbols")))
      (:p (str (the (back-link :display-string "Documentation Home"))))
      (:p
       (:ul
        (dolist (symbol (the :symbols-for-index))
          (htm (:li
                (str (the-object (first symbol)
                                 (self-link :display-string
                                            (second symbol))))
                " ("
                (:i (str (the-object (first symbol) :dok-type)) ", "
                    (str
                     (string-downcase
                      (package-name (symbol-package (the-object
                                                     (first symbol)
                                                     :symbol)))))
                    ")"))))))
      (:p (str (the back-link)))
      
      (:p (str (the footer)))
      
      ))))

(define-object package-dokumentation (base-yadd-sheet)

  :documentation
  (:author "Dave Cooper"
   :description "Prepares documentation for all relevant symbols in a given Lisp package.")

  :input-slots
  (("String or keyword symbol. Names the package, or a nickname of the package, to be documented."
    package (the :package-form :package))
   ("boolean. Determines whether to show red/green flag on each message indicating whether it is a 
            supported message."
    show-supported-flag nil)
   ("Boolean. Determines whether to consider all symbols in the package or just the exported ones."
    external-only? (the :package-form :external-only?)))

  :computed-slots
  ((title (with-output-to-string (ss)
            (html-stream ss "The " (:i (:princ (the strings-for-display-verbose)))
                         " Package")))
   (package-key (make-keyword (the :package)))
   
   (strings-for-display (package-name (find-package (the package-key))))
   
   (strings-for-display-verbose (let ((nicknames
                                       (package-nicknames (find-package
                                                           (the :package-key))))
                                      (documentation
                                       (#+(or allegro lispworks) documentation 
                                          #+cmu cl::package-doc-string (find-package (the :package-key)) nil)))
                          
                                  (format nil "~a ~a~a~{~:(~a~)~^, ~}"
                                          (package-name (find-package (the package-key)))
                                          (if documentation (format nil "(~a)" documentation) "")
                                          (if nicknames " Nicknames: " "")
                                          nicknames)))

   (symbols-for-index (append 
                       
                       
                       (apply #'append
                              (mapcar #'(lambda(doc) (the-object doc symbols-for-index))
                                      (the object-docs doks)))
                       
                       #+nil
                       (mapcar #'(lambda (part)
                                   (let
                                       ((name
                                         (string-downcase

                                          (format
                                           nil
                                           "~a"
                                           (the-object part :symbol)))))
                                     (list part name)))
                               (the :object-docs :doks))
                       
                       
                       (mapcar #'(lambda (function)
                                   (list
                                    function
                                    (string-downcase

                                     (format
                                      nil
                                      "~a"
                                      (the-object function :symbol)))))
                               (the :function-docs :doks))
                       (mapcar #'(lambda (variable)
                                   (list
                                    variable
                                    (string-downcase

                                     (format
                                      nil
                                      "~a"
                                      (the-object variable :symbol)))))
                               (the :variable-docs :doks)))))

  :objects
  (("object-doc. Container for set of all Object documentation sheets."
    object-docs :type 'object-doc
    :return-object self
    :show-supported-flag (the :show-supported-flag)
    :pass-down (:external-only?)
    :package (the :package-key))
   ("function-doc. Container for set of all Function documentation sheets."
    function-docs :type 'function-doc
    :return-object self
    :pass-down (:external-only?)
    :package (the :package-key))
   ("variable-doc. Container for set of all Parameter/Variable/Constant documentation sheets."
    variable-docs :type 'variable-doc
    :return-object self
    :pass-down (:external-only?)
    :package (the :package-key)))

  :hidden-objects
  (("package-form. Allows user to modify toplevel optional-inputs."
    package-form :type 'package-form))

  :functions
  (("Void. Prints to *html-stream* a bulleted list for each of the three categories of docs in the package."
    write-html-sheet
    nil
    (if (the :package)
        (html (:html
               (:head
                (the default-header-content)
                (:title "The " (:princ (the strings-for-display-verbose)) " Package"))
               (:body
                (when *developing?* (html (:p (the (write-development-links)))))
                (:p (the (:write-back-link :display-string "Documentation Home")))
                (:p
                 (:center (:h2 (:princ (the :title)))
                          (when (package-nicknames (the :package-key))
                            (html (:i
                                   (format *html-stream* "a.k.a. ~{~:(~a~)~^, ~}"
                                           (package-nicknames (the :package-key)))))))
                 (mapc #'(lambda (child)
                           (the-object child (:write-documentation-links)))
                       (the :children)))
                (:p (the (:write-back-link :display-string "Documentation Home")))
                (:p (the :write-footer)))))
      (the :package-form (:write-html-sheet))))))


(define-object package-form (base-yadd-sheet)

  :documentation
  (:author "Dave Cooper (Genworks)"
   :description "Presents a form to the user to be able to modify the Package, 
supported-flag, and external flag.")

  :computed-slots
  ((package (getf (the :query-plist) :package))
   (show-supported-flag (getf (the :query-plist) :show-supported-flag))
   (external-only? (getf (the :query-plist) :external-only))
   (respondant (the return-object)))

  :functions
  ((write-html-sheet
    nil
    (html (:html
           (:head
            (the default-header-content)
            (:title
             "Please Specify a Common Lisp Package to Product Documentation"))
           (:body (when *developing?* (html (:p (the (write-development-links)))))
                  (with-html-form ()
                    (:table
                     (:tr (:td (:b "Package Name"))
                          (:td
                           ((:input :type :string :name :package :size 40 :value
                                    (or (the :package) "")))))
                     (:tr (:td (:b "Flag Supported Messages?"))
                          (:td
                           ((:input :type :checkbox :name :show-supported-flag :checked
                                    :checked))))
                     (:tr (:td (:b "Suppress Unexported Symbols?"))
                          (:td ((:input :type :checkbox :name :external-only)))))
                   (:p ((:input :type :submit :value " OK " :name :submit)))
                   (:p (the :write-footer)))))))))




(define-object index-item (base-yadd-sheet)
  :input-slots
  (key items)
  
  
  :computed-slots
  ((additional-header-content
    (with-cl-who-string ()
      (the default-header-content)
      (:title (fmt "Matches for ~s" (the key)))))
   
   (main-sheet-body 
    (with-cl-who-string ()
      (when *developing?* (htm (:p (the (write-development-links)))))
      (:p
       (:center
        (:h2 (fmt "Matches for ~s" (the key)))))
      (:p
       (dolist (item (the items))
         (htm (:li
               (the-object item (:write-self-link :display-string (the  key)))
                " ("
                (:i (str (the-object item :dok-type)) ", "
                    (str
                     (string-downcase
                      (package-name (symbol-package (the-object
                                                     item
                                                     :symbol)))))")")))))))))

        





