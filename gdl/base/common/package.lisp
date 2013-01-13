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

(defpackage :gdl
  (:documentation "General-purpose Declarative Language")
  (:use :common-lisp)
  (:shadow #:the)
  (:export #:%bottom-margin%
           #:%left-margin%
           #:%paper-height%
           #:%paper-width%
           #:%projection-vector%
           #:%view-plane-inverse%
           #:%view-plane-normal%
           #:*%format%*
           #:*allowed-part-documentation-keywords*
           #:*color-table*
           #:*color-table-decimal*
           #:*colors-default*
           #:*compile-circular-reference-detection?*
           #:*compile-dependency-tracking?*
           #:*compile-documentation-database?*
           #:*compile-for-dgdl?*
           #:*compile-source-code-database?*
           #:*compile-with-dependency-recording?*
           #:*current-version*
           #:*curve-chords*
           #:*display-controls*
           #:*dummy-version*
           #:*ensure-lists-when-bashing?*
           #:*error-on-reserved-words?*
           #:*force-restore-slot-default?*
           #:*gdl-init-functions*
           #:*genworks-oss-svn-version*
           #:*genworks-svn-version*
           #:*ghostscript-path*
           #:*gs-path*
           #:*load-documentation-database?*
           #:*load-source-code-database?*
           #:*out-of-bounds-sequence-reference-action*
           #:*paper-size-plist*
           #:*paper-size-table*
           #:*patch-fasls*
           #:*patches-root-dir-default*
           #:*recompile-gdl?*
           #:*remember-previous-slot-values?*
           #:*report-gdl-redefinitions?*
           #:*reserved-word-protected-packages*
           #:*retain-object-identity-on-type-change?*
           #:*rgb-cube-colors*
           #:*run-with-circular-reference-detection?*
           #:*run-with-dependency-recording?*
           #:*run-with-dependency-tracking?*
           #:*skin*
           #:*stream*
           #:*ui-server*
           #:*undeclared-parameters-enabled?*
           #:*zero-epsilon*
           #:+kappa+
           #:+phi+
           #:2pi
           #:alist2plist
           #:always
           #:append-elements
           #:assembly-leaf-mixin
           #:assembly-node-mixin
           #:base-format
           #:base-rule-object
           #:base-spec-sheet
           #:binary-search
           #:cl
           #:cl-dir
           #:cl-lite
           #:clean
           #:clean-dir
           #:copy-files-to
           #:cyclic-nth
           #:decode-for-http
           #:defaulting
           #:defcompanion
           #:define-color-set
           #:define-colored-icon
           #:define-format
           #:define-icon
           #:define-lens
           #:define-object
           #:define-object-amendment
           #:define-object-documentation
           #:define-package
           #:define-skin
           #:define-view
           #:definition-tree
           #:defpart
           #:defwriter
           #:distribute
           #:distribute-dir
           #:div
           #:encode-for-http
           #:ensure-list
           #:evaluate
           #:evaluate-object
           #:expandit
           #:find-messages-used-by
           #:find-messages-which-use
           #:flatten
           #:format-slot
           #:fround-to-nearest
           #:gdl-class
           #:gdl-format-class
           #:gdl-format-symbol
           #:gdl-object-symbol
           #:gdl-object-symbol
           #:gdl-remote
           #:gdl-skin-class
           #:get-version
           #:half
           #:ignore-errors-with-backtrace
           #:index-filter
           #:iso-8601-date
	   #:universal-time-from-iso-8601
	   #:universal-time-to-plist
           #:lastcar
           #:least
           #:let-streams
           #:list-elements
           #:list-hash
           #:list-of-numbers
           #:lookup-color
           #:make-canonical-part
           #:make-gdl-app
           #:make-keyword
	   #:ensure-keyword
           #:make-keyword-sensitive
           #:make-object
           #:make-part
           #:make-self
           #:mapsend
           #:maptree
           #:match-regexp*
           #:max-of-elements
           #:merge-common-keys
           #:message-popup
           #:min-max-search
           #:min-of-elements
           #:most
           #:near-to?
           #:near-zero?
           #:never
           #:null-object
           #:null-part
           #:number-format
           #:number-round
           #:peruse-file
           #:pi/2
           #:plist-keys
           #:plist-values
           #:print-hash
           #:print-messages
           #:print-variables
           quantification
           #:query-collect
           #:read-safe-string
           #:read-snapshot
           #:readable-expression
           ;;#:record-source-file
           #:register-asdf-systems
           #:remote-object
           #:remove-plist-key
	   #:remove-plist-entry
           #:remove-plist-keys
           #:replace-regexp*
           #:replace-substring
           #:restore-ui-object
           #:retitle-emacs
           #:retrieve
           #:rgb-cube-colors
           #:round-to-nearest
           #:safe-float
           #:safe-sort
           #:self
           #:send
           #:series
           #:set-format-slot
           #:set-self
           #:split
           #:standard-query
           #:start-gdl
           #:start-gdl-runtime
           #:status-message
           #:string-append
           #:sum-elements
           #:tak
           #:the
           #:the-child
           #:the-element
           #:the-object
           #:to-double-float
           #:to-single-float
           #:traverse-tree
           #:twice
           #:undefine-object
           #:update
           #:update-gdl
           #:vanilla-mixin
           #:vanilla-mixin*
           #:vanilla-remote
           #:version-sequence
           #:with-error-handling
           #:with-format
           #:with-format-slots
           #:with-oracle
           #:with-version
           #:write-env
           #:write-objects
           #:write-plist
           #:write-the
           #:write-the-object
           #:xml-reader
           #:^2
	   #:room-report
	   #:*onclick-function*))

(defpackage :gdl-toplevel (:use))
(defpackage :gdl-rule (:size 25) (:use) 
            (:export #:%not-handled% #:write-env #:write-of #:%unbound%))
(defpackage :gdl-slots (:size 10000) (:use) (:export))
(defpackage :gdl-inputs (:size 10000) (:use) (:export))
(defpackage :gdl-trickle-downs (:use) (:export))
(defpackage :gdl-acc (:size 10000) (:use) (:export))
(defpackage :gdl-format (:use) (:export))
(defpackage :gdl-output (:use) (:export))

;;
;; Pre-define these just in case.  a bit of a kludge because
;; gdl:define-package gets redefined when these modules
;; are loaded, to :use these packages. 
;;
(defpackage :geom-base (:use))
(defpackage :surf (:use) (:shadow #:step) (:export #:step))


(defmacro gdl:define-package (name &rest body)
  `(defpackage ,name 
     (:shadowing-import-from :gdl #:the)
     (:use :common-lisp :gdl)
     ,@body))


;;
;; FLAG working on this one to do a proper redefinition. 
;;
#+nil
(defmacro gdl:define-package (name &rest body)
  (let ((%exports (gensym))
        (%uses (gensym))
        (%shadowing-import-froms (gensym))
        (%shadows (gensym))
        (%import-froms (gensym))
        (existing-package (gensym)))
    `(flet ((,%exports (exports)
              (export (mapcar #'(lambda(symbol) (glisp:intern symbol ,name)) exports) ,name))
            (,%uses (uses)
              (use-package uses ,name))
	      
            (,%shadowing-import-froms (imports)
              (destructuring-bind (package &rest symbols) imports
                (shadowing-import (mapcar #'(lambda(symbol) (glisp:intern symbol package)) symbols)
                                  ,name)))
            (,%shadows (symbols) (shadow symbols ,name))
            (,%import-froms (imports)
              (destructuring-bind (package &rest symbols) imports
                (import (mapcar #'(lambda(symbol) (glisp:intern symbol package)) symbols) ,name))))
       (let ((,existing-package (find-package ,name)))
         (if ,existing-package
             (progn
               (,%exports (rest (find :export ',body :key #'first)))
               (,%uses (rest (find :use ',body :key #'first)))
               (,%shadowing-import-froms (rest (find :shadowing-import-from ',body :key #'first)))
               (,%shadows (rest (find :shadow ',body :key #'first)))
               (,%import-froms (rest (find :import-from ',body :key #'first))))
             (defpackage ,name 
               (:shadowing-import-from :gdl #:the) (:use :common-lisp :gdl) ,@body))))))



(gdl:define-package :gdl-user)


