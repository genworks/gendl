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
      (glisp:replace-regexp
       (glisp:replace-regexp 
	(glisp:replace-regexp (funcall glisp:*base64-encode-func* string) "=" "")
	"\\Q+\\E" "-") "/" "_") "\\n" "") "\\r" "")))


(defun base64-decode-list (string)
  "List. Decodes a base64 string into a Lisp list.

:arguments (string \"string\")

"
  ;;
  ;; FLAG -- removed string-downcase for the decoded-string. Watch for this; don't know 
  ;;         why it was there in the first place. 
  ;;
  (let ((decoded-string (base64-decode-safe string)))
    (read-safe-string decoded-string)))




(defun base64-decode-safe (string)
  "String. Decodes a base64 string without need for trailing = signs into a decoded string.

:arguments (string \"string\")

"

  (when *debug?* (print-variables string))


  (let ((padding (make-string (mod (- 4 (mod (length string) 4)) 4) :initial-element #\=)))
    (funcall glisp:*base64-decode-func* 
	     (string-append (glisp:replace-regexp (glisp:replace-regexp string "-" "+") "_" "/") padding))))

                           

#+allegro
(defun string-to-compressed-base64-string (string)
  (excl:usb8-array-to-base64-string 
   (let* ((vector (make-array 5 :element-type '(unsigned-byte 8)))
	  (deflate-stream (make-instance 'util.zip:deflate-stream :target vector)))
     (write-string string deflate-stream)
     (close deflate-stream)
     (util.zip:deflate-stream-vector-combined deflate-stream))))

#+allegro
(defun compressed-base64-string-to-string (string)
  (let ((array (excl:base64-string-to-usb8-array string)))
    (excl:with-input-from-buffer (in array)
      (util.zip:skip-gzip-header in)
      (let ((inflate-stream (make-instance 'util.zip:inflate-stream :input-handle in))
	    (result nil) (newline (format nil "~%")))
	(do ((line (read-line inflate-stream nil nil) (read-line inflate-stream nil nil)))
	    ((null line) result)
	  (setq result (if result (string-append result newline line) line)))))))


