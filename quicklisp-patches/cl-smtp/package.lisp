;;; -*- mode: Lisp -*-
       
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005/2006/2007 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: package.lisp
;;; Description: cl-smtp package definition file

(in-package :cl-user)

(defpackage :cl-smtp
  (:use :cl :flexi-streams :trivial-gray-streams)
  (:export "SEND-EMAIL"
           "WITH-SMTP-MAIL"
           "SMTP-ERROR"
           "SMTP-PROTOCOL-ERROR"
           "NO-SUPPORTED-AUTHENTICATION-METHOD"
           "RCPT-FAILED"
           "IGNORE-RECIPIENT"
	   "ATTACHMENT"
	   "MAKE-ATTACHMENT"
	   "ATTACHMENT-NAME"
	   "ATTACHMENT-DATA-PATHNAME"
	   "ATTACHMENT-MIME-TYPE"
           "RFC2045-Q-ENCODE-STRING"
           "RFC2231-ENCODE-STRING"
           "WRITE-RFC8822-MESSAGE"))

(in-package :cl-smtp)

(defparameter *debug* nil)

(defmacro print-debug (str)
  `(when *debug*
      (print ,str)))

