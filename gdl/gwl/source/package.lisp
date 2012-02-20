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

(in-package :common-lisp-user)

(setq cl-who:*prologue* "<!doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
(setq cl-who:*attribute-quote-char* #\")
(setq cl-who:*downcase-tokens-p* nil)

(gdl:define-package :gwl
    (:use :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
  (:documentation "Generative Web Language")
  (:shadow #:define-package)
  (:export
   #:define-package
   #:remote-object
   #:*req*
   #:*ent*
   #:*html-referrer*
   #:*instance-hash-table*
   #:*max-node-depth*
   #:*developing?*
   #:*mime-file-types*
   #:*max-id-value*
   #:*server*
   #:*query-plist*
   #:*field-plist*
   #:*ssl-server*
   #:*adsense-code*
   #:*break-on-set-self?*
   #:*jump-to-toplevel-on-set-self?*
   #:*process-cookies?*
   #:encode-root-path
   #:base-html-sheet
   #:skeleton-ui-element
   #:sheet-section
   #:session-control-mixin
   #:base-html-graphics-sheet
   #:color-palette
   #:crawl
   #:html-submit-button
   #:html-button
   #:html-checkbox
   #:html-radio-button
   #:html-select-choices
   #:html-file
   #:html-multi-line-text
   #:html-string
   #:html-password
   #:html-title
   #:html-anchor
   #:html-cell
   #:html-row
   #:html-table
   #:html-static-text
   #:untagify
   #:make-new-session-id
   #:html-format
   #:html-format
   ;;#:define-package
   #:my
   #:my-child
   #:from-my
   #:my-object
   #:defpage
   #:root-path-to-query-arg
   #:query-arg-to-root-path
   #:gwl-make-part
   #:gwl-make-object
   #:replace-substring
   #:custom-base-html-sheet-mixin
   #:fix-lhtml
   #:start-log-maker
   #:clear-log-buffer
   #:*log-report-buffer*
   #:pdf-output-sheet
   #:graphics-preferences
   #:application-mixin
   #:node-mixin
   #:gwl-rule-object
   #:*js-incremental-search*
   
   #:html-form
   
   #:form-mixin
   #:clear-all-instances
   #:clear-instance
   
   #:with-html-form
   
   #:publish-shared
   #:with-all-servers
   #:web-drawing
   
   #:infinite
   
   #:base64-decode-safe
   #:base64-encode-safe
   #:base64-decode-list
   #:base64-encode-list
   
   #:start-gwl
   ;;
   ;; FLAG -- test this in 1575 build
   ;;
   ;;#:update-gdl
   ;;#:*patch-fasls*
   
   
   ;;
   ;;
   ;;

   #:base-form-control
   #:button-form-control
   #:checkbox-form-control
   #:radio-form-control
   #:menu-form-control
   #:text-form-control
   #:password-form-control
   #:file-form-control
   #:hidden-form-control
   #:object-form-control
   #:grid-form-control
   
   #:encode-for-ajax
   #:decode-from-ajax
   #:form-element-processor
   
   #:*clicked-x*
   #:*clicked-y*
   
   ;;
   ;; Ajax stuff
   ;;

   #:base-ajax-sheet
   #:base-ajax-graphics-sheet

   #:*standard-ajax-javascript*
   #:*ajax-timeout*
   #:*bypass-security-check?*
   
   #:with-cl-who
   #:with-cl-who-string
   #:publish-gwl-app
   #:publish-string-content
   ))


(defmacro gwl:define-package (name &rest args)
  `(gdl:define-package ,name 
       (:shadow #:define-package)
     (:use :gwl :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
     ,@args))



(gwl:define-package :gwl-user)

