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

(in-package :gwl)


(defun base64-encode-list (list)
    "String. Encodes a list into base64 without the trailing = signs.

:arguments (list \"list\")

"
  (base64-encode-safe (format nil "~s" list)))

(defun base64-encode-safe (string)
  "String. Encodes a string into base64 without the trailing = signs.

:arguments (string \"string\")

"
  (let ((cl-ppcre:*allow-quoting* t))
    (glisp:replace-regexp
     (glisp:replace-regexp 
      (glisp:replace-regexp (funcall glisp:*base64-encode-func* string) "=" "")
      "\\Q+\\E" "-") "/" "_")))


(defun base64-decode-list (string)
  "List. Decodes a base64 string into a Lisp list.

:arguments (string \"string\")

"
  
  (read-safe-string (base64-decode-safe string)))
"String. Decodes a base64 string without need for trailing = signs into a decoded string.

:arguments (string \"string\")

"

(defun base64-decode-safe (string)
  (let ((padding (make-string (mod (- 4 (mod (length string) 4)) 4) :initial-element #\=)))
    (funcall glisp:*base64-decode-func* 
             (string-append (glisp:replace-regexp (glisp:replace-regexp string "-" "+") "_" "/") padding))))

                           

