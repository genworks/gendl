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

(define-object base-yadd-sheet (base-ajax-sheet)
   :documentation
  (:author "Dave Cooper (Genworks)"
   :description "Base mixin for a yadd sheet")
  
  :computed-slots
  (;; FLAG JB-090817 added default header content with links to style-sheet and icons
   ("Contains default header contents for yadd html files.
This computed-slot is available in all children of this object. It contains links 
to default header content of a HTML generated yadd page. This contains a
link to the favicon.ico and a link to a default CSS sheet. All these elements can be
found in the *gdl-install-dir*/static/gwl/ directories."
    default-header-content (html 
                            ((:link :href "/static/gwl/images/favicon.ico"
                                    :type "image/x-icon" :rel "icon"))
                            
                            ;;
                            ;; FLAG -- nil'ed out for now, this was causing tiny fonts. 
                            ;;
                            #+nil
                            ((:link :href "/static/gwl/style/tasty-layout.css" 
                                    :type "text/css" :rel "stylesheet"))
                            ))
   ("Contains standard jQuery files to include in the header for additional search funcionality.
This computed-slot contains javascript files, found in the *gdl-install-dir* 
and used throughout the yadd pages for the generation of automatic search forms (like the master-index).
The javascript loaded is jquery."
    additional-header-js (with-cl-who-string ()
                           ((:script :type "text/javascript" 
                                     :src "/static/3rdpty/jquery/js/jquery-1.3.2.min.js"))
                           ((:script :type "text/javascript" 
                                     :src "/static/3rdpty/jquery/js/jquery.quicksearch.js"))))
   
   (footer (let* ((version "1581")
                  (patch 9))
             (with-cl-who-string ()
               ((:p :class "copyrightFooter")
                "Copyright &copy; "
                (multiple-value-bind (seconds minutes hours date month year)
                    (get-decoded-time)
                  (declare (ignore seconds minutes hours date month))
                  (str year))
                " "
                ((:a :href "http://www.genworks.com") "Genworks")
                (:sup "&reg;") " "
                ((:a :href "http://www.genworks.com")
                 "International") 
                ". All rights reserved. "
                (:i (:small "Genworks Build: "
                            (str version)
                            (when patch (htm "p"))
                            (str (format nil "~3,,,'0@a" patch)))))
               ((:p :class "copyrightFooter")
                (:i "User Code Packages copyright &copy; their respective authors"))))))
  
  :functions
  ((compute-url
    ()
    (the descriptive-url))
   

   (write-footer
    ()
    (let ((*stream* (or *html-stream* *stream*)))
      (let* ((version "1581")
             (patch 9))
        (with-cl-who()
          ((:p :class "copyrightFooter")
           "Copyright &copy; "
           (multiple-value-bind (seconds minutes hours date month year)
               (get-decoded-time)
             (declare (ignore seconds minutes hours date month))
             (str year))
           " "
           ((:a :href "http://www.genworks.com") "Genworks")
           (:sup "&reg;") " "
           ((:a :href "http://www.genworks.com")
            "International") 
           ". All rights reserved. "
           (:i (:small "Genworks Build: "
                       (str version)
                       (when patch (htm "p"))
                       (str (format nil "~3,,,'0@a" patch)))))))))))



(define-object remark-writers-mixin ()

  :computed-slots
  ((dok-type (ecase (the :type)
               (object-dokumentation "Object")
               (function-dokumentation (if (the :macro?) "Macro" "Function"))
               (variable-dokumentation "Parameter or Constant")))
   (macro? nil))

  :functions
  ((supported?
    (type-string)
    (search "(supported)" type-string))

   (write-type
    (type-string &key (show-supported-flag? t))
    (let* ((supported? (the (:supported? type-string)))
           (type-string (replace-substring type-string "(supported)" ""))
           (type-page
            (gethash (ignore-errors (intern (string-upcase type-string)
                                            (the package)))
                     (the index-ht))))
      (if type-page
          (html ((:a :href (the-object type-page :url))
                 (:princ (string-downcase type-string))))
        (html (:princ (string-downcase type-string))))
      (when show-supported-flag?
        (html " "
              (if supported?
                  (html ((:font :color :green) (:b "[Supported Message]")))
                (html ((:font :color :red)
                       (:b "[Internal, Unsupported Message]"))))))))

   (write-remark-body
    (plist &key show-package)
    
    (html (:p (:princ (getf plist :intro)))
          (:dl
           (when show-package
             (html (:dt (:i "Package:"))
                   (:dd
                    (:tt
                     (:small
                      (:princ (string-downcase (package-name show-package)))))
                    (let ((nicknames
                           (mapcar #'string-downcase
                                   (package-nicknames show-package))))
                      (when nicknames
                        (html " (a.k.a. "
                              (:tt
                               (:small
                                (:princ (format nil "~{~a~^, ~}" nicknames))))
                              ")"))))))
           (mapcar #'(lambda (key value)
                       (html (:dt
                              (:i
                               (:princ
                                (string-downcase
                                 (case key
                                   (:&optional "Optional Arguments")
                                   (:&key "Keyword Arguments")
                                   (:&rest "Rest Arguments")
                                   (:&body "Body Arguments")
                                   (otherwise key))))
                               ":"))
                             (:dd
                              (case key
                                ((:arguments :&rest :&body) (the (:write-arguments value)))
                                ((:&optional :&key)
                                 (the (:write-optional-arguments value)))
                                (otherwise (html (:princ value)))))))
                   (plist-keys (rest (rest (rest (rest plist)))))
                   (plist-values (rest (rest (rest (rest plist)))))))))

   (write-optional-arguments
    (plist)
    (html (:dl
           (mapcar #'(lambda (key value)
                       (let* ((arg (if (listp key) (first key) key))
                              (default (when (listp key) (second key)))
                              (value-strings (split value #\.))
                              (type (first value-strings))
                              (description-strings (rest value-strings))
                              (description-string
                               (format nil "~{~a~^.~}" description-strings)))
                         (html (:small
                                (:dt (:b (:princ (string-downcase arg))) " "
                                     (:i (:princ type)) ", Default Value: "
                                     (:tt (:prin1 default)))
                                (:dd (:princ description-string))))))
                   (plist-keys plist) (plist-values plist)))))

   (write-arguments
    (plist)
    (html (:dl
           (mapcar #'(lambda (key value)
                       (let* ((value-strings (split value #\.))
                              (type (first value-strings))
                              (description-strings (rest value-strings))
                              (description-string
                               (format nil "~{~a~^.~}" description-strings)))
                         (html (:small
                                (:dt (:b (:princ (string-downcase key))) " "
                                     (:i (:princ type)))
                                (:dd (:princ description-string))))))
                   (plist-keys plist) (plist-values plist)))))))


(define-object doc-links-mixin (base-html-sheet)

  :input-slots
  (package
   return-object
   heading
   external-only?)

  :computed-slots
  ((package-object (find-package (format nil "~a" (the :package))))
   
   (doks (list-elements (if (the :external-only?)
                            (the :dokumentation-external)
                          (the :dokumentation)))))

  :objects
  ((dokumentation :type (the :part-type)
                  :sequence (:size (length (the :symbols)))
                  :symbol (nth (the-child :index) (the :symbols))
                  :pass-down (:return-object :package))
   
   (dokumentation-all :type (the :part-type)
                      :sequence (:size (length (the :symbols-all)))
                      :symbol (nth (the-child :index) (the :symbols-all))
                      :pass-down (:return-object :package))
   
   (dokumentation-external :type (the :part-type)
                           :sequence (:size (length (the :symbols-external)))
                           :symbol (nth (the-child :index) (the :symbols-external))
                           :pass-down (:return-object)))

  :functions
  ((write-documentation-links
    nil
    (let ((documentations
           (safe-sort (the :doks) #'string-lessp :key
                      #'(lambda (obj)
                          (format nil "~a" (the-object obj :symbol))))))
      (when documentations
        (html (:p (:h3 (:princ (the :heading)))
                  (:ul
                   (dolist (documentation documentations)
                     (html (:li
                            (the-object documentation
                                        (:write-self-link :display-string
                                                          (string-downcase
                                                           (format nil "~a"
                                                                   (the-object
                                                                    documentation
                                                                    :symbol)))))
                            (when (the-object documentation :macro?)
                              (html " [Macro]")))))))))))))


(define-object doc-string-parser-mixin nil

  :input-slots
  (remark-string
   package)

  :computed-slots
  ((string-lines (let (in-pre?)
                   (mapcar #'(lambda (string)
                               (when (search "<pre>" string) (setq in-pre? t))
                               (when (search "</pre>" string) (setq in-pre? nil))
                               (if in-pre?
                                   string
                                 (string-trim (list #\Space #\Tab) string)))
                           (remove :newline
                                   (split (the :remark-string)
                                          (list #\Newline #\Return))))))
   (remark-type (when (and (find #\. (first (the :string-lines)))
                           (not (all-caps? (first (split (first (the :string-lines)))))))
                  (first (split (first (the :string-lines)) #\.))))
   
   (string-lines-rest (if (the :remark-type)
                          (cons (format nil "~{~a~^.~}~a"
                                        (rest (split
                                               (first (the :string-lines))
                                               (list #\.)))
                                        (let ((line
                                               (string-trim
                                                (list #\Space #\Tab)
                                                (first (the :string-lines)))))
                                          (if
                                              (eql
                                               (aref line (1- (length line)))
                                               #\.)
                                              "."
                                            "")))
                                (rest (the :string-lines)))
                        (the :string-lines)))
   
   (section-start-positions (let ((count -1) result in-pre?)
                              (dolist (line (the :string-lines-rest)
                                        (nreverse result))
                                (incf count)
                                (when (and (not in-pre?)
                                           (not (zerop (length line)))
                                           (or
                                            (eql
                                             (aref
                                              (string-trim
                                               (list #\Space #\Tab)
                                               line)
                                              0)
                                             #\:)
                                            (all-caps? (first (split line)))))
                                  (push count result))
                                (when (search "<pre>" line) (setq in-pre? t))
                                (when (search "</pre>" line) (setq in-pre? nil)))))
   
   (section-plist (append (list :type (the :remark-type) :intro
                                (apply #'string-append
                                       (mapcar #'(lambda
                                                     (string)
                                                   (string-append
                                                    string
                                                    (format nil "~%")))
                                               (subseq
                                                (the :string-lines-rest)
                                                0
                                                (first
                                                 (the
                                                     :section-start-positions))))))
                          (mapcan #'(lambda (start end)
                                      (let ((key
                                             (make-keyword
                                              (read-from-string

                                               (first
                                                (split
                                                 (first
                                                  (subseq
                                                   (the :string-lines-rest)
                                                   start
                                                   (1+ start))))))))
                                            (value
                                             (apply
                                              #'string-append
                                              (cons
                                               (apply
                                                #'string-append
                                                (append
                                                 (mapcar
                                                  #'(lambda
                                                        (string)
                                                      (string-append string " "))
                                                  (rest
                                                   (split
                                                    (first
                                                     (subseq
                                                      (the :string-lines-rest)
                                                      start
                                                      (1+ start))))))
                                                 (list (format nil "~%"))))
                                               (mapcar
                                                #'(lambda
                                                      (string)
                                                    (string-append
                                                     string
                                                     (format nil "~%")))
                                                (subseq
                                                 (the :string-lines-rest)
                                                 (1+ start)
                                                 end))))))
                                        (list key
                                              (case
                                                  key
                                                ((:arguments :&optional :&key :&rest)
                                                 (read-from-string value))
                                                (otherwise value)))))
                                  (the :section-start-positions)
                                  (append (rest (the :section-start-positions))
                                          (list nil)))))))

(defparameter *keyword-length-threshhold* 3)

(defun all-caps? (string)
  (and (> (length string) *keyword-length-threshhold*)
       (every #'(lambda(char) (find char "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) string)))
             



