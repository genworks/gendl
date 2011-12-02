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

(define-object function-doc (doc-links-mixin)


  :input-slots
  (package
   return-object)

  :computed-slots
  ((heading "Function and Macro Definitions")
   (part-type 'function-dokumentation)
   (symbols (remove-duplicates
             (let (result (package-object (the :package-object)))
               (do-symbols (symbol (the :package)
                             (sort (nreverse result) #'string< :key #'symbol-name))
                 (when (and (eql (symbol-package symbol) package-object)
			    (glisp:function-documentation symbol))
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
				     (glisp:function-documentation symbol))
                            (push symbol result))))))))


(define-object function-dokumentation (doc-string-parser-mixin
                                       remark-writers-mixin 
                                       base-yadd-sheet)

  :input-slots
  (return-object
   symbol
   (show-package? nil))

  :computed-slots
  ((strings-for-display (format nil "~a" (the symbol)))
   (doc-string (glisp:function-documentation symbol))
   (macro? (macro-function (the symbol))))

  :functions
  ((write-html-sheet
    nil
    (html (:html
           (:head
            (the default-header-content)
            (:title
             (:princ
              (format nil "~a: ~s" (if (the macro?) "Macro" "Function")
                      (the :symbol)))))
           (:body (when *developing?* (html (:p (the (write-development-links)))))
                  (when *adsense?* (html (:p (:princ *adsense-code*))))
                  (:p (when (the :return-object) (the (:write-back-link :display-string "&lt;-Back"))))
                  (:h2
                   (:princ
                    (format nil "~a: ~:(~s~)" (if (the macro?) "Macro" "Function")
                            (the :symbol))))
                  (:p
                   ((:table :width "100%" :border 0 :cellpadding 1 :cellspacing 0)
                    (the (:write-row :show-package? (the :show-package?)))))
                  (:p (when (the :return-object) (the (:write-back-link :display-string "&lt;-Back"))))
                  (:p (the :write-footer))))))

   (write-row
    (&key show-package?)
    (html (:tr
           ((:td :align :left)
            (:b (:princ (format nil "~a" (the :symbol))))
            (when (getf (the :section-plist) :type)
              (html " "
                    (:i
                     (the (:write-type (getf (the :section-plist) :type)
                                       :show-supported-flag? nil))))))
           ((:td :align :right) :br))
          (:tr
           ((:td :colspan 2)
            (the (:write-remark-body (the :section-plist) :show-package
                                     (when show-package? (symbol-package (the :symbol)))))))
          (:tr ((:td :colspan 2) :br))))

   (remark-string
    nil
    (the :doc-string))))

   
   
   
   
   
   
