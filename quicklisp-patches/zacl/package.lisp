;;;; package.lisp

(defpackage #:zacl
  (:use #:cl)
  #+sbcl
  (:import-from #:sb-ext
                #:without-package-locks
                #:defglobal)
  #+sbcl
  (:import-from #:sb-sys
                ;; XXX Private package
                #:without-interrupts
                #:fd-stream-fd)
  #+sbcl
  (:import-from #:usocket
		#:get-host-by-name)
  
  (:import-from #:cl+ssl
                #:make-ssl-client-stream
                #:make-ssl-server-stream)
  #+ccl
  (:import-from #:ccl
                #:without-interrupts
                #:defstaticvar
                #:symbol-value-in-process
                #:socket-error-identifier
                #:socket-error-code
                ;; XXX private
                #:*initial-process*)
  #+ccl
  (:import-from #:ccl
                #:lookup-hostname
                #:ipaddr-to-hostname)
  #+ccl
  (:import-from #:ccl
                #:stream-device)
  (:import-from #:alexandria
                #:hash-table-alist
                #:extremum)
  (:import-from #:cl-ppcre
                #:create-scanner
                #:scan
                #:split)
  (:import-from #:md5
                #:make-md5-state
                #:update-md5-state
                #:finalize-md5-state)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:flexi-streams
                #:with-output-to-sequence
                #:get-output-stream-sequence
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:usocket
                #:socket-listen
                #:socket-accept
                #:socket-connect
                #:get-local-address
                #:get-local-port
                #:socket-option
                #:get-peer-address
                #:usocket-p
                #:usocket
                #:stream-usocket
                #:socket-stream
                #:socket-close
                #:wait-for-input)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:current-thread
                #:make-lock
                #:release-lock
                #:acquire-lock
                #:with-lock-held
                #:with-timeout
                #:timeout
                #:make-condition-variable
                #:condition-wait
                #:condition-notify
                #:interrupt-thread
                #:thread-yield
                #:thread-alive-p)
  (:import-from #:queues
                #:make-queue
                #:qpush
                #:qpop)
  (:import-from #:trivial-gray-streams
                #:fundamental-stream
                #:fundamental-binary-output-stream
                #:fundamental-binary-input-stream
                #:fundamental-character-output-stream
                #:fundamental-character-input-stream
                #:stream-write-char
                #:stream-write-byte
                #:stream-write-string
                #:stream-write-sequence
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-read-byte
                #:stream-read-sequence
                #:stream-force-output)
  (:import-from #:trivial-garbage
                #:make-weak-hash-table
                #:finalize)
  (:import-from #:uiop
                #:command-line-arguments)
  (:import-from #:trivial-backtrace
                #:print-backtrace-to-stream)
  (:import-from #:quri
                #:make-uri
                #:render-uri
                #:uri
                #:copy-uri
                #:merge-uris
                #:uri-path
                #:uri-host
                #:uri-scheme
                #:uri-port
                #:uri-fragment
                #:uri-userinfo
                #:uri-query
                #:parse-uri)
  (:import-from #:cl-store
                #:store
                #:restore
                #:*check-for-circs*))

(defpackage #:zacl-reader
  (:use #:cl)
  (:export #:*allegro-rewriting-readtable* #:cl-file))

(defpackage #:zacl-if-star
  (:use #:cl)
  (:export #:if*
           #:then
           #:thenret
           #:else))

(defpackage #:zacl-cl
  (:use)
  (:export #:read-sequence
           #:macroexpand
           #:stream-external-format
           #:streamp
           #:read-char
           #:read-char-no-hang))

;;; Allegro packages

(defpackage #:excl
  (:use #:zacl-if-star)
  (:import-from #:cl
                #:file-write-date)
  (:intern #:stream-property-list
           #:with-dynamic-extend-usb8-array
           #:unix-signal
           #:make-basic-lock
           #:*std-control-out-table*
           #:socket-bytes-written
           #:fast
           #:.atomically
           #:filesys-size
           #:filesys-write-date
           #:filesys-type
           #:merge-to-physical
           #:ssl-context)
  (:export #:if*
           #:then
           #:thenret
           #:else)
  (:export #:md5-init
           #:md5-update
           #:md5-final)
  (:export #:gc
           #:atomic-conditional-setf
           #:run-shell-command
           #:rename-file-raw
           #:delimited-string-to-list
           #:stream-input-fn
           #:fasl-write
           #:fasl-read
           #:pop-atomic
           #:push-atomic
           #:single-channel-simple-stream
           #:install-single-channel-character-strategy
           #:device-open
           #:device-close
           #:device-read
           #:device-write
           #:add-stream-instance-flags
           #:write-vector
           #:read-vector
           #:crlf-base-ef
           #:ef-name
           #:synchronizing-structure
           #:socket-error
           #:with-locked-structure
           #:incf-atomic
           #:decf-atomic
           #:named-function
           #:featurep
           #:find-external-format
           #:fixnump
           #:split-into-words
           #:split-on-character
           #:native-string-sizeof
           #:mb-to-string
           #:string-to-mb
           #:string-to-octets
           #:octets-to-string
           #:schedule-finalization
           #:without-package-locks
           #:without-interrupts
           #:defvar-nonbindable
           #:*initial-terminal-io*
           #:*cl-default-special-bindings*
           #:*required-top-level-bindings*
           #:*current-case-mode*)
  (:export #:with-locked-object
           #:with-locked-objects
           #:lockable-object)
  (:export #:match-regexp
           #:match-re
           #:replace-regexp
           #:compile-regexp)
  (:export #:with-output-to-buffer
           #:get-output-stream-buffer)
  (:export #:def-stream-class
           #:terminal-simple-stream
           #:with-stream-class
           #:device-read
           #:device-close
           #:stream-closed-error
           #:stream-error-identifier
           #:stream-error-code)
  (:export #:sm
           #:errorset)
  (:export #:string-to-base64-string
	   #:base64-string-to-string))


(defpackage #:ff
  (:use)
  (:export #:def-foreign-call))

(defpackage #:socket
  (:use)
  (:intern #:make-ssl-server-stream
           #:make-ssl-client-stream)
  (:export #:make-socket
           #:socket-error
           #:accept-connection
           #:socket-control
           #:local-port
           #:local-host
           #:set-socket-options
           #:ipaddr-to-dotted
           #:ipaddr-to-hostname
           #:dotted-to-ipaddr
           #:lookup-hostname
           #:remote-host
           #:with-pending-connect))

(defpackage #:user
  (:use #:cl
        #:excl)
  (:shadow #:defpackage)
  (:export #:defpackage))

(defpackage #:sys
  (:use)
  (:export #:reap-os-subprocess
           #:with-timeout
           #:*current-process*
           #:command-line-arguments
           #:*tilde-expand-namestrings*
           #:gsgc-switch
           #:gsgc-parameter
           #:defpatch))

(defpackage #:util.zip
  (:use)
  (:export #:inflate-stream #:deflate-stream #:deflate-target-stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((net.uri (find-package :net.uri))
	(net.aserve (find-package :net.aserve)))
    (if (and net.uri (not (and net.aserve (member net.uri (package-use-list net.aserve)))))
	(let ((puri? (and (find-package :puri)
			  (string-equal (first (package-nicknames :puri)) "net.uri"))))

	  (if puri?

	      (progn
		(warn "

Existing \"net.uri\" package was detected as a nickname of the \"puri\" package. 

Removing the \"net.uri\" nickname from \"puri\" because ZACL needs to
define this package name.

")
		(rename-package :puri :puri nil))

	      (progn
		(warn  "

-Existing \"net.uri\" package was detected. 

Deleting the \"net.uri\" package, because ZACL needs to define this
package name.

")
		(delete-package :net.uri)))))))


(defpackage #:net.uri
  (:use)
  (:intern #:uri-string
           #:.uri-parsed-path
           #:uri-path-etc)
  (:export #:uri-path
           #:uri-plist
           #:copy-uri
           #:render-uri
           #:merge-uris
           #:uri-host
           #:uri-scheme
           #:uri-port
           #:parse-uri
           #:uri-query
           #:uri-userinfo
           #:uri-fragment
           #:uri))

(defpackage #:net.html.generator
  (:use))

(defpackage #:mp
  (:use)
  (:import-from #:sys
                #:*current-process*
                #:with-timeout)
  (:export #:with-timeout
           #:without-scheduling)
  (:export #:make-gate
           #:open-gate
           #:close-gate
           #:gate-open-p)
  (:export #:queue
           #:enqueue
           #:dequeue)
  (:export #:make-process-lock
           #:with-process-lock
           #:process-lock)
  (:export #:wait-for-input-available)
  (:export #:make-process
           #:process-wait
           #:process-wait-with-timeout
           #:process-reset
           #:process-allow-schedule
           #:process-kill
           #:process-run-function
           #:process-keeps-lisp-alive-p
           #:process-preset
           #:process-property-list
           #:process-run-reasons
           #:process-add-run-reason
           #:process-revoke-run-reason
           #:process-name
           #:process-thread
           #:*current-process*))

(defpackage #:si
  (:use)
  (:export #:without-scheduling
           #:global-symbol-value))

(defpackage #:excl.osi
  (:use)
  (:export #:stat
           #:stat-mtime))

(defpackage #:top-level.debug
  (:use)
  (:export #:zoom))

(defpackage #:util.string
  (:use)
  (:export #:string+))

(defpackage #:acl-socket (:use))
