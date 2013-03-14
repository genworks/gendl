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


(in-package :gwl)

;;(glisp:set-compile-settings)

(defclass gwl-gdl-ui ()())

(setq *ui-server* (make-instance 'gwl-gdl-ui))

(defparameter *keys-to-preserve* (list :/yadd))

(defparameter *bypass-security-check?* nil
  "Boolean. Indicates whether security check for same IP address is bypassed. Defaults to nil.")

(defparameter *max-id-value* 999 "Integer. The maximum number to use for the randomly-generated 
instance ids for URI identifiers. Defaults to 999." )


(defparameter *server* nil)

(defparameter *mime-file-types* (let ((ht (make-hash-table :size (hash-table-size net.aserve::*mime-types*) :test #'equalp)))
                                  (maphash #'(lambda(file-type mime-type)
                                               (let ((value (gethash mime-type ht)))
                                                 (if value
                                                     (nconc (gethash mime-type ht) (list file-type))
                                                   (setf (gethash mime-type ht) (list file-type)))))
                                           net.aserve::*mime-types*) ht))


(defparameter *query-plist* nil)
(defparameter *field-plist* nil)

(defparameter *max-node-depth* 100)


(defparameter *js-incremental-search*
    ;;
    ;; FLAG -- rewrite as func with params.
    ;;
    (list :script
          "
function incrementalSearch(which_key, modifiers)
{searchString = document.main_form.selection.value;
 for(i=0;i<document.main_form.INGREDIENT.options.length;i++)
   {listIndex=i;
    if (searchString.toUpperCase() <= document.main_form.INGREDIENT.options[i].text.toUpperCase())
      {break;}}
 document.main_form.INGREDIENT.selectedIndex = listIndex;}"
          :on-key-up "this.blur();this.focus();incrementalSearch(event.which, event.modifiers);"))

(defparameter *req* nil 
  "AllegroServe Request object. Dynamically bound to the current request 
object within a write-html-sheet or main-sheet function.")

(defparameter *ent* nil "AllegroServe Entity object. Dynamically bound to the current entity 
object within a write-html-sheet or main-sheet function.")

(defparameter *query* nil "Association list. Dynamically bound to the current query 
list within any of the form response methods.")

(defvar *instance-hash-table* (make-hash-table)
  "Hash table. Keys are session ids and values are lists 
with GWL root objects and a dummy (for now) version.")

(defvar *break-on-set-self?* nil
  "Boolean. Set this to non-nil if you want a break and *background-interaction* in
emacs when the setSelf link is pressed. Defaults to nil.")


(defvar *jump-to-toplevel-on-set-self?* nil
  "Boolean. Set this to non-nil if you want to jump to the toplevel interaction buffer
in emacs when setSelf link is pressed and <tt>*break-on-set-self?*</t> is nil. 
Defaults to t.")


(defparameter *remote-objects-hash* (make-hash-table))
(defparameter *remote-proxies-hash* (make-hash-table :test #'equalp))
(defparameter *ipaddr* nil)

(defparameter *compass-regions*
    '((:rect    ((30 30) (50 50)) :recenter)
      (:polygon ((28 27) (40 2) (54 27)) :north)
      (:polygon ((28 56) (42 81) (54 56)) :south)
      (:polygon ((2 41) (26 30) (26 54)) :west)
      (:polygon ((55 29) (81 42) (55 54)) :east)
      (:polygon ((18 30) (11 12) (29 18)) :northwest)
      (:polygon ((58 18) (71 12) (66 30)) :northeast)
      (:polygon ((12 71) (16 53) (30 66)) :southwest)
      (:polygon ((53 65) (65 52) (71 71)) :southeast)
      ))

(defvar *url-hash-table* (make-hash-table :test #'equalp))

(defvar *debug?* nil)

(defvar *ssl-server* nil)


(defvar *graphics-mode* :raster)

(defparameter *clicked-x* nil)
(defparameter *clicked-y* nil)




(defparameter *descriptive-url-hash* (make-hash-table :test #'equalp))

(defparameter *failed-request-url* "/" "The redirection url that will be returned for a failed-request")

(defparameter *process-cookies?* nil)

(defparameter *recovery-url-default* "/" "The url that the session-control-mixin will use for the recovery url")

;;
;; FLAG --- cl-who customizations -- should be in own file
;;
(setq cl-who:*prologue* "<!doctype HTML>")
(setq cl-who:*attribute-quote-char* #\")
(setq cl-who:*downcase-tokens-p* nil)
