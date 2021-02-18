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

;;; File: cl-smtp.lisp
;;; Description: main smtp client logic

(in-package :cl-smtp)

(defparameter *x-mailer* (format nil "cl-smtp (~A ~A)" 
				 (lisp-implementation-type)
				 (lisp-implementation-version)))

(defun check-arg (arg name)
  (cond
   ((or (stringp arg)
        (pathnamep arg))
    (list arg))
   ((listp arg)
    arg)
   (t
    (error "the \"~A\" argument is not a string or cons" name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *return-newline* #.(format nil "~C~C" #\Return #\NewLine)))

(defun mask-dot (str)
   "Replace all occurences of single line #\. with #\.#\." 
     (with-output-to-string (s)
       (mask-dot-stream str s)))

(defun mask-dot-stream (str stream)
  "Replace all occurences of single line #\. with #\.#\."
  (let ((l (length str)))
    (loop for c across str
       for i from 0
       for pc = (when (> i 0) (elt str (1- i)))
       for nc = (when (< (1+ i) l)  (elt str (1+ i)))
       do (if (and (eq c #\.) (or (eq pc #\Newline) (not pc))
                   (or (eq nc #\Return) (eq nc #\Newline) (not nc)))
              (write-sequence ".." stream)
              (write-char c stream)))))

(defun string-to-base64-string (str &key (external-format :utf-8)
                                (columns 80))
  (let ((exformat (flex:make-external-format external-format)))
  #+allegro (excl:usb8-array-to-base64-string 
             (flex:string-to-octets str :external-format exformat)
             :wrap-at-column (if (and (numberp columns) (= columns 0))
                                  nil columns))
  #-allegro (cl-base64:usb8-array-to-base64-string 
             (flex:string-to-octets str :external-format exformat)
             :columns columns)))

(defun string-has-non-ascii (str)
  (loop for c across str
     when (< 127 (char-code c)) do (return t)))

(defun rfc2045-q-encode-string (str &key (external-format :utf-8))
  (let ((line-has-non-ascii (string-has-non-ascii str))
        (estart (concatenate 'string "=?"
                             (string-upcase (symbol-name external-format))
                             "?Q?"))
        (last-line-break 0)
        (len (length str))
        (exformat (flex:make-external-format external-format)))
    (with-output-to-string (s)
      (when line-has-non-ascii
        (write-sequence estart s))
      (loop for c across str
         for n from 0 to len
         for column = (- n last-line-break)
         do
           (when (and (>= column 74)
                      line-has-non-ascii)
             (write-sequence "?=" s)
             (write-blank-line s)
             (write-char #\Space s)
             (write-sequence estart s)
             (setf last-line-break n))
           (cond
             (line-has-non-ascii
              (loop for byte across (flex:string-to-octets 
                                     (make-string 1 :initial-element c)
                                     :external-format exformat)
                 do (format s "~:@(=~2,'0X~)" byte)))
             ((or (char= c #\NewLine)
                  (and (char= c #\Space)
                       (>= column 74)))
              (setf last-line-break n)
              (write-blank-line s)
              (write-char #\Space s))
             (t
              (unless (char= c #\Return)
                (write-char c s)))))
      (when line-has-non-ascii
        (write-sequence "?=" s)))))

(defun rfc2045-q-encode-string-to-stream (str stream 
                                          &key (external-format :utf-8) 
                                          (columns 74))
  (let ((exformat (flex:make-external-format external-format))
        (last-line-break 0)
        (len (length str)))
    (loop for c across str
          for n from 0 to len 
          for column = (- n last-line-break)
          for nc = (when (< (1+ n) len) (elt str (1+ n)))
       do
         (when (>= column columns)
           (write-char #\= stream)
           (write-blank-line stream)
           (setf last-line-break n))
         (cond
           ((and (char= c #\.) (= n last-line-break))
            (write-char #\. stream)
            (write-char #\. stream))
           ((char= c #\NewLine)
            (setf last-line-break (1+ n))
            (write-blank-line stream))
           ((or (char= c #\Space)
                (char= c #\Tab))
            (if (eq nc #\NewLine)
                (format stream "~:@(=~2,'0X~)" (char-code c))
                (write-char c stream)))
           ((or (< 127 (char-code c))
                (> 33 (char-code c))
                (char= c #\=))
            (loop for byte across (flex:string-to-octets 
                                     (make-string 1 :initial-element c)
                                     :external-format exformat)
                 do (format stream "~:@(=~2,'0X~)" byte)))
           (t
            (write-char c stream))
           ))
    ))

(defun substitute-return-newline (str)
  "Replace all occurences of \r\n in STR with spaces"
  (let ((resultstr ""))
    (labels ((mask (tempstr)
	       (let ((n (search *return-newline* tempstr)))
		 (cond
                   (n
                    (setf resultstr (concatenate 'string resultstr 
                                                 (subseq tempstr 0 n)
                                                 " "))
                    (mask (subseq tempstr (+ n 2))))
                   (t
                    (setf resultstr (concatenate 'string resultstr 
                                                 tempstr)))))))
      (mask str))
    resultstr))

(define-condition smtp-error (error)
  ())

(define-condition smtp-protocol-error (smtp-error)
  ((command :initarg :command :reader command)
   (expected-response-code :initarg :expected-response-code :reader expected-response-code)
   (response-code :initarg :response-code :reader response-code)
   (response-message :initarg :response-message :reader response-message))
  (:report (lambda (condition stream)
             (print-unreadable-object (condition stream :type t)
               (format stream "a command failed:~%command: ~S expected: ~A response-code: ~A response-message: ~A"
                       (command condition)
                       (expected-response-code condition)
                       (response-code condition)
                       (response-message condition))))))

(define-condition rcpt-failed (smtp-protocol-error)
  ((recipient :initarg :recipient
              :reader recipient))
  (:report (lambda (condition stream)
             (print-unreadable-object (condition stream :type t)
               (format stream "while trying to send email through SMTP, the server rejected the recipient ~A: ~A"
                       (recipient condition)
                       (response-message condition))))))

(defun smtp-command (stream command expected-response-code
                     &key (condition-class 'smtp-protocol-error) 
                     condition-arguments)
  (when command
    (write-to-smtp stream command))
  (multiple-value-bind (code msgstr lines)
      (read-from-smtp stream)
    (when (/= code expected-response-code)
      (apply #'error
             condition-class
             (append condition-arguments
                     (list :command command
                           :expected-response-code expected-response-code
                           :response-code code
                           :response-message msgstr))))
    (append lines (list msgstr))))

(defun do-with-smtp-mail (host envelope-sender to thunk &key port authentication ssl local-hostname (external-format :utf-8))
  (usocket:with-client-socket (socket stream host port 
                                      :element-type '(unsigned-byte 8))
    (setf stream (flexi-streams:make-flexi-stream 
                  stream
                  :external-format 
                  (flexi-streams:make-external-format 
                   external-format :eol-style :lf)))
    (let ((stream (smtp-handshake stream
                                  :authentication authentication 
                                  :ssl ssl
                                  :local-hostname local-hostname)))
      (initiate-smtp-mail stream envelope-sender to)
      (funcall thunk stream)
      (finish-smtp-mail stream))))

(defmacro with-smtp-mail ((stream-var host envelope-sender to &key ssl (port (if (eq :tls ssl) 465 25)) authentication local-hostname (external-format :utf-8))
                          &body body)
  "Encapsulate a SMTP MAIl conversation.  A connection to the SMTP
   server on HOST and PORT is established and a MAIL command is
   initiated with FROM being the mail sender and TO being the list of
   recipients.  BODY is evaluated with STREAM-VAR being the stream
   connected to the remote SMTP server.  BODY is expected to write the
   RFC2821 message (headers and body) to STREAM-VAR."
  `(do-with-smtp-mail ,host ,envelope-sender ,to
                      (lambda (,stream-var) ,@body)
                      :port ,port
                      :authentication ,authentication 
                      :ssl ,ssl
                      :local-hostname ,local-hostname
                      :external-format ,external-format))

(defun send-email (host from to subject message 
		   &key ssl (port (if (eq :tls ssl) 465 25)) cc bcc reply-to extra-headers
		   html-message display-name authentication
		   attachments (buffer-size 256) envelope-sender 
                   (external-format :utf-8) local-hostname)
  (send-smtp host from (check-arg to "to") subject (mask-dot message)
	     :port port :cc (check-arg cc "cc") :bcc (check-arg bcc "bcc")
	     :reply-to reply-to 
	     :extra-headers extra-headers
	     :html-message html-message
	     :display-name display-name
	     :authentication authentication
	     :attachments (check-arg attachments "attachments")
	     :buffer-size (if (numberp buffer-size) 
			      buffer-size
			      256)
	     :envelope-sender (or envelope-sender from)
             :external-format external-format
	     :ssl ssl
             :local-hostname (or local-hostname (usocket::get-host-name))))

(defun send-smtp (host from to subject message
                  &key ssl (port (if (eq :tls ssl) 465 25)) cc bcc
		  reply-to extra-headers html-message display-name
		  authentication attachments buffer-size
                  (local-hostname (usocket::get-host-name))
		  (envelope-sender from)
		  (external-format :utf-8))
  (with-smtp-mail (stream host envelope-sender (append to cc bcc)
                          :port port
                          :authentication authentication 
                          :ssl ssl
                          :local-hostname local-hostname
                          :external-format external-format)
    (write-rfc8822-message stream from to subject message 
                           :cc cc :reply-to reply-to 
                           :extra-headers extra-headers 
                           :html-message html-message
                           :display-name display-name
                           :attachments attachments 
                           :buffer-size buffer-size
                           :external-format external-format)))

(define-condition no-supported-authentication-method (smtp-error)
  ((features :initarg :features :reader features))
  (:report (lambda (condition stream)
             (print-unreadable-object (condition stream :type t)
               (format stream "SMTP authentication has been requested, but the SMTP server did not advertise any ~
                               supported authentication scheme.  Features announced: ~{~S~^, ~}"
                       (features condition))))))

(defun smtp-authenticate (stream authentication features)
  "Authenticate to the SMTP server connected on STREAM.
   AUTHENTICATION is a list of two or three elements.  If the first
   element is a keyword, it specifies the desired authentication
   method (:PLAIN or :LOGIN), which is currently ignored.  The actual
   method used is determined by looking at the advertised features of
   the SMTP server.  The (other) two elements of the AUTHENTICATION
   list are the login username and password.  FEATURES is the list of
   features announced by the SMTP server.

   If the server does not announce any compatible authentication scheme,
   the NO-SUPPORTED-AUTHENTICATION-METHOD error is signalled."
  (when (keywordp (car authentication))
    (pop authentication))
  (let ((server-authentication (loop for i in features
                                  for e = (search "AUTH " i :test #'equal)
                                  when (and e (= e 0))
                                  return i)))
    (destructuring-bind (username password) authentication
      (cond
        ((search " PLAIN" server-authentication :test #'equal)
         (smtp-command stream (format nil "AUTH PLAIN ~A" 
                                      (string-to-base64-string
                                       (format nil "~A~C~A~C~A" 
                                               username
                                               #\null username
                                               #\null password)
                                       :columns 0))
                       235))
        ((search " LOGIN" server-authentication :test #'equal)
         (smtp-command stream "AUTH LOGIN"
                       334)
         (smtp-command stream (string-to-base64-string username :columns 0)
                       334)
         (smtp-command stream (string-to-base64-string password :columns 0)
                       235))
        (t
         (error 'no-supported-authentication-method :features features))))))

(defun smtp-handshake (stream &key authentication ssl local-hostname)
  "Perform the initial SMTP handshake on STREAM.  Returns the stream
   to use further down in the conversation, which may be different from
   the original stream if we switched to SSL."

  (unless (or ssl authentication)
    ;; Unless we want ESMTP features, perform classic SMTP handshake and return
    ;; Read the initial greeting from the SMTP server
    (smtp-command stream nil 220)
    (smtp-command stream (format nil "HELO ~A" 
                                 (usocket::get-host-name))
                  250)
    (return-from smtp-handshake stream))

  ;; When SSL or authentication requested, perform ESMTP EHLO
  (let ((features)
        (flexi-external-format (flexi-streams:flexi-stream-external-format stream)))
    (labels
        ((read-greetings ()
	   ;; Read the initial greeting from the SMTP server
	   (smtp-command stream nil 220))
	 (do-ehlo ()
           (setf features (rest (smtp-command stream (format nil "EHLO ~A" local-hostname)
                                              250))))
         (convert-connection-to-ssl ()
           (setf stream 
                 #+allegro (socket:make-ssl-client-stream stream)
                 #-allegro
                 (let ((s (flexi-streams:flexi-stream-stream stream)))
                   (cl+ssl:make-ssl-client-stream 
                    (cl+ssl:stream-fd s)
                    :close-callback (lambda () (close s)))))
           #-allegro
           (setf stream (flexi-streams:make-flexi-stream 
                         stream
                         :external-format flexi-external-format))))
      (ecase ssl
        ((or t :starttls)
	 (read-greetings)
         (do-ehlo)
         (unless (find "STARTTLS" features :test #'equal)
           (error "this server does not supports TLS"))
         (print-debug "this server supports TLS")
         (smtp-command stream "STARTTLS"
                       220)
         (convert-connection-to-ssl)
         ;; After STARTTLS, the connection is "like new".  Re-do the
         ;; EHLO command to switch the server to ESMTP mode and read
         ;; the list of announced features again.
         (do-ehlo))
        (:tls
         ;; Plain SSL connection
         (convert-connection-to-ssl)
	 (read-greetings)
         (do-ehlo))
        ((nil)
	 (read-greetings)
         (do-ehlo))))
    (when authentication
      (smtp-authenticate stream authentication features)))
  stream)
  
(defun initiate-smtp-mail (stream envelope-sender to)
  "Initiate an SMTP MAIL command, sending a MAIL FROM command for the
   email address in FROM and RCPT commands for all receipients in TO,
   which is expected to be a list.

   If any of the TO addresses is not accepted, a RCPT-FAILED condition
   is signalled.  This condition may be handled by the caller in order
   to send the email anyway."
  (smtp-command stream 
                (format nil "MAIL FROM:<~A>" (substitute-return-newline envelope-sender))
                250)
  (dolist (address to)
    (restart-case 
        (smtp-command stream (format nil "RCPT TO:<~A>" 
                                     (substitute-return-newline address))
                      250
                      :condition-class 'rcpt-failed
                      :condition-arguments (list :recipient address))
      (ignore-recipient ())))
  (smtp-command stream "DATA"
                354))

(defun finish-smtp-mail (stream)
  "Finish sending an email to the SMTP server connected to on STREAM.
   The server is expected to be inside of the DATA SMTP command.  The
   connection is then terminated by sending a QUIT command."
  (write-to-smtp stream "")
  (smtp-command stream "." 250)
  (smtp-command stream "QUIT" 221))

(defun send-mail-headers (stream 
			  &key from to cc reply-to 
			  extra-headers display-name subject 
                          (external-format :utf-8))
  "Send email headers according to the given arguments to the SMTP
   server connected to on STREAM.  The server is expected to have
   previously accepted the DATA SMTP command."
  (write-to-smtp stream (format nil "Date: ~A" (get-email-date-string)))
  (if display-name
      (write-to-smtp stream (format nil "From: ~A <~A>" 
                                    (rfc2045-q-encode-string 
                                     display-name :external-format external-format)
                                    from))
      (write-to-smtp stream (format nil "From: ~A" from)))
  (write-to-smtp stream (format nil "To: ~{ ~a~^,~}" to))
  (when cc
    (write-to-smtp stream (format nil "Cc: ~{ ~a~^,~}" cc)))
  (write-to-smtp stream (format nil "Subject: ~A" 
                                (rfc2045-q-encode-string 
                                 subject :external-format external-format)))
  (write-to-smtp stream (format nil "X-Mailer: ~A" 
				(rfc2045-q-encode-string 
                                 *x-mailer* :external-format external-format)))
  (when reply-to
    (write-to-smtp stream (format nil "Reply-To: ~A" 
                                  (rfc2045-q-encode-string 
                                   reply-to :external-format external-format))))
  (when (and extra-headers
	     (listp extra-headers))
    (dolist (l extra-headers)
      (write-to-smtp stream 
		     (format nil "~A: ~{~a~^,~}" (car l) (rest l)))))
  (write-to-smtp stream "Mime-Version: 1.0"))

(defun send-multipart-headers (stream &key attachment-boundary html-boundary)
  (cond (attachment-boundary
	 (generate-multipart-header stream attachment-boundary 
				    :multipart-type "mixed"))
	(html-boundary (generate-multipart-header 
			stream html-boundary 
			:multipart-type "alternative"))
	(t nil)))

(defun write-rfc8822-message (stream from to subject message
                              &key cc reply-to extra-headers html-message 
                              display-name attachments buffer-size
                              (external-format :utf-8))
  (let* ((boundary (make-random-boundary))
         (message-transfer-encoding (when (string-has-non-ascii message)
                                      "quoted-printable"))
                                      
         (html-boundary (if (and attachments html-message)
                            (make-random-boundary)
                            boundary))
         (content-type 
          (format nil "text/plain; charset=~S" 
                  (string-upcase (symbol-name external-format)))))
    (send-mail-headers stream
                       :from from
                       :to to
                       :cc cc
                       :reply-to reply-to
                       :display-name display-name 
                       :extra-headers extra-headers :subject subject
                       :external-format external-format)
    (when (or attachments html-message)
      (send-multipart-headers stream
                              :attachment-boundary (when attachments boundary) 
                              :html-boundary html-boundary)
      (write-blank-line stream))
    ;;----------- Send  the body Message ---------------------------
    ;;--- Send the proper headers depending on plain-text, 
    ;;--- multi-part or html email 
    (cond ((and attachments html-message)
           ;; if both present, start attachment section, 
           ;; then define alternative section, 
           ;; then write alternative header
           (progn 
             (generate-message-header 
              stream :boundary boundary :include-blank-line? nil)
             (generate-multipart-header stream html-boundary 
                                        :multipart-type "alternative")
             (write-blank-line stream)
             (generate-message-header 
              stream :boundary html-boundary :content-type content-type 
              :content-transfer-encoding message-transfer-encoding
              :content-disposition "inline" :include-blank-line? nil)))
          (attachments 
           (generate-message-header 
            stream :boundary boundary 
            :content-type content-type :content-disposition "inline"
            :content-transfer-encoding message-transfer-encoding
            :include-blank-line? nil))
          (html-message
           (generate-message-header 
            stream :boundary html-boundary :content-type content-type 
            :content-transfer-encoding message-transfer-encoding
            :content-disposition "inline"))
          (t 
           (generate-message-header 
            stream :content-type content-type
            :content-transfer-encoding message-transfer-encoding
            :include-blank-line? nil)))
    (write-blank-line stream)
    (if message-transfer-encoding
        (progn
          (print-debug (format nil "to server body quoted-printable: ~A"
                               message))
          (rfc2045-q-encode-string-to-stream message stream 
                                             :external-format external-format))
        (mask-dot-stream message stream))
    (write-blank-line stream)
    (write-blank-line stream)
    ;;---------- Send  Html text if needed -------------------------
    (when html-message
      (let ((non-ascii-p (string-has-non-ascii html-message)))
        (generate-message-header 
         stream :boundary html-boundary 
         :content-type (format nil "text/html; charset=~S" 
                               (string-upcase (symbol-name external-format)))
         :content-transfer-encoding (when non-ascii-p "quoted-printable")
         :content-disposition "inline")
        (if non-ascii-p
            (progn
              (print-debug 
               (format nil "to server html-message quoted-printable: ~A"
                       html-message))
              (rfc2045-q-encode-string-to-stream 
               html-message stream :external-format external-format))
            (mask-dot-stream html-message stream))
        (send-end-marker stream html-boundary)))
    ;;---------- Send Attachments -----------------------------------
    (when attachments
      (dolist (attachment attachments)
        (send-attachment stream attachment boundary buffer-size 
                         external-format))
      (send-end-marker stream boundary))))

(defun write-to-smtp (stream command)
  (print-debug (format nil "to server: ~A" command)) 
  (write-sequence command stream)
  (write-char #\Return stream)
  (write-char #\NewLine stream)
  (force-output stream))

(defun write-blank-line (stream)
  (write-char #\Return stream)
  (write-char #\NewLine stream)
  (force-output stream))

(defun read-from-smtp (stream &optional lines)
  (let* ((line (read-line stream))
	 (response (string-trim '(#\Return #\NewLine) (subseq line 4)))
	 (response-code (parse-integer line :start 0 :junk-allowed t)))
    (print-debug (format nil "from server: ~A" line))
    (if (= (char-code (elt line 3)) (char-code #\-))
	(read-from-smtp stream (append lines (list response)))
	(values response-code response lines))))

(defun get-email-date-string ()
  (multiple-value-bind (sec min h d m y wd) (get-decoded-time)
    (let* ((month (elt '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (- m 1)))
	   (weekday (elt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") wd))
	   (timezone (get-timezone-from-integer
		      (- (encode-universal-time sec min h d m y 0)
			 (get-universal-time)))))
      (format nil "~A, ~2,'0d ~A ~d ~2,'0d:~2,'0d:~2,'0d ~D" 
	      weekday d month y h min sec timezone))))

(defun get-timezone-from-integer (x)
  (let ((min (/ x 60))
	(hour (/ x 3600)))
    (if (integerp hour)
        (cond
	  ((>= hour 0)
	   (format nil "+~2,'0d00" hour))
	  ((< hour 0)
          (format nil "-~2,'0d00" (* -1 hour))))
        (multiple-value-bind (h m) (truncate min 60)
          (cond
	  ((>= hour 0)
	   (format nil "+~2,'0d~2,'0d" h (truncate m)))
	  ((< hour 0)
	   (format nil "-~2,'0d~2,'0d" (* -1 h) (* -1 (truncate m)))))))))
