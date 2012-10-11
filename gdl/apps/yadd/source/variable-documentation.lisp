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

(in-package :yadd)

(define-object variable-doc (doc-links-mixin)


  :computed-slots
  ((heading "Variables and Constants")
   (part-type 'variable-dokumentation)
   (symbols (remove-duplicates
             (let (result (package-object (the :package-object)))
               (do-symbols (symbol (the :package)
                             (sort (nreverse result) #'string< :key #'symbol-name))
                 (when (and (eql (symbol-package symbol) package-object)
			    (glisp:variable-documentation symbol))
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
				     (glisp:variable-documentation symbol))
                            (push symbol result))))))))


(define-object variable-dokumentation (doc-string-parser-mixin
                                       remark-writers-mixin base-yadd-sheet)

  :input-slots
  (
   ;;symbol 
   
   (show-package? nil))

  :computed-slots
  ((remark-string (glisp:variable-documentation (the symbol))))

  :functions
  ((write-html-sheet
    nil
    (html (:html
           (:head
            (the default-header-content)
            (:title (:princ (format nil "Documentation for ~s" (the :symbol)))))
           (:body (when *developing?* (html (:p (the (write-development-links)))))
                  (:p (when (the :return-object) (the (:write-back-link :display-string "&lt;-Back"))))
                  (:h2
                   (:princ
                    (format nil "~a: ~:(~s~)"
                            (if (constantp (the symbol)) "Constant" "Parameter")
                            (the :symbol))))
                  (:p
                   ((:table :width "100%" :border 0 :cellpadding 1 :cellspacing 0)
                    (the (:write-row))))
                  (let ((new-list (copy-list (the :section-plist))))
                    (remf new-list :type)
                    (remf new-list :value-default)
                    (remf new-list :intro)
                    (mapcar #'(lambda (key value)
                                (html (:dt
                                       (:i
                                        (:princ
                                         (string-capitalize
                                          (case key
                                            (:&optional "Optional Arguments")
                                            (:&key "Keyword Arguments")
                                            (otherwise key))))
                                        ":"))
                                      (:dd
                                       (case key
                                         (:arguments (the (:write-arguments value)))
                                         ((:&optional :&key)
                                          (the (:write-optional-arguments value)))
                                         (otherwise (html (:princ value)))))))
                            (plist-keys new-list) (plist-values new-list)))
                  (:p (when (the :return-object) (the (:write-back-link :display-string "&lt;-Back"))))
                  (:p (the :write-footer))))))

   (write-row
    nil
    (html (:tr
           ((:td :align :left)
            (:b (:princ (format nil "~a" (the :symbol))))
            (when (getf (the :section-plist) :type)
              (html " "
                    (:i
                     (the (:write-type (getf (the :section-plist) :type)
                                       :show-supported-flag? nil)))))
            (when (getf (the :section-plist) :value-default)
              (html ", " "Default Value: "
                    (:tt
                     (:princ
                      (read-from-string
                       (getf (the :section-plist) :value-default))))))))
          (:tr ((:td :align :left) (:princ (getf (the :section-plist) :intro))))))))






