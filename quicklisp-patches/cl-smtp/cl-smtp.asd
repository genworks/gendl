;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005/2006/2007/2008/2009/2010 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-smtp.asd
;;; Description: cl-smtp ASDF system definition file

(asdf:defsystem :cl-smtp
  :description "Common Lisp smtp client."
  :licence "LLGPL"
  :author "Jan Idzikowski <jidzikowski@common-lisp.net>"
  :maintainer "Jan Idzikowski <jidzikowski@common-lisp.net>"
  :version "20200731.1"
  :perform (load-op :after (op webpage)
                    (pushnew :cl-smtp cl:*features*))
  :depends-on (:usocket 
               :trivial-gray-streams
               :flexi-streams
               #-allegro :cl-base64 
               #-allegro :cl+ssl)
  :serial t
  :components ((:file "package")
               (:file "attachments")
               (:file "cl-smtp")
               (:file "mime-types")))

