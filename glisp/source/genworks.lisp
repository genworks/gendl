;;
;; Copyright 2002-2011, 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;

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

(in-package :com.genworks.lisp)


(defparameter *temporary-folder-function* #'(lambda() (merge-pathnames "tmp/" (user-homedir-pathname))))


(defparameter *utf-16-ef*
  #+allegro :unicode
  #+sbcl :utf-16le
  #+(or ccl abcl) :utf-16)


#-(or allegro lispworks sbcl ccl) 
(warn "~&Please implement concatenate-fasls for the currently running lisp.~%")

;;
;; FLAG -- OBSOLETED by asdf/bundle:compile-bundle-op and asdf/bundle:monolithic-compile-bundle-op
;;     (except for current apparent bug where it leaves out a depended-upon uiop). 
;;
(defun concatenate-fasls (files dest)
  #-(or allegro lispworks sbcl ccl) (declare (ignore files dest))
  #-(or allegro lispworks sbcl ccl) (error "~&Please implement concatenate-fasls for the currently running lisp.~%")

  
  #+(or allegro sbcl)
  ;;
  ;; Provided by Franz:
  ;;
  ;; copy the contents of all files to the file named dest.
  ;; append .fasl to the filenames
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (p dest :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (dolist (file files)
        (with-open-file (in (if (pathname-type file)
                                file
                              (format nil "~a.~a" file *fasl-extension*))
                         :element-type '(unsigned-byte 8))
          (do ((count (read-sequence buffer in) (read-sequence buffer in)))
              ((<= count 0))
            (cl:write-sequence buffer p :end count))))))

  #+lispworks
    ;; created a defsystem on the fly and use concatenate-system.
  (let ((defsys (concatenate `string
                  (format nil "(defsystem gdl::my-system () :members (")
                  (let (result
                        (bin-files (mapcar #'(lambda(bin-file)
                                               (namestring bin-file))
                                           files)))
                    (dolist (file bin-files)
                      (setq result (concatenate 'string result (format nil "~s~%" file))))
                    result)
                  (format nil "))~%"))))
    (eval (read-from-string defsys))
    (lispworks:concatenate-system dest 'my-system))

  #+ccl (ccl:fasl-concatenate dest files :if-exists :supersede))


(defvar *wild-entry*
  (make-pathname :name :wild :type :wild :version :wild))


#+(or allegro clozure)
(defun copy-directory (from-dir to-dir &rest args)
  (declare (ignore args))
  #+allegro (excl:copy-directory from-dir to-dir)
  #+clozure (ccl::recursive-copy-directory from-dir to-dir ))

#+(or allegro lispworks clozure)
(defun copy-file (from to)
  #+allegro (sys:copy-file from to)
  #+lispworks (lw:copy-file from to)
  #+clozure (ccl:copy-file from to))



#+nil
(defun delete-directory-and-files (target &key force quiet (if-does-not-exist :error))
  #-allegro (declare (ignore force quiet))
  (cond ((probe-file target)
	 #+lispworks (system:run-shell-command (format nil "rm -rf ~a" target))
	 #+allegro (excl.osi:delete-directory-and-files 
		    target :force force :quiet quiet)
	 #-(or allegro (and unix lispworks))
	 (cl-fad:delete-directory-and-files target :if-does-not-exist if-does-not-exist))
	((null if-does-not-exist) nil)
	(t (ecase if-does-not-exist
	     (:ignore nil)
	     (:warn (warn "Target ~s does not exist.~%" target))
	     (:error (error "Target ~s does not exist.~%" target))))))


(defun directory-list (pathspec)
  "(derived from quicklisp ql-impl-util:directory-entries): 
Return all directory entries of DIRECTORY as a
list, or NIL if there are no directory entries. Excludes the \".\"
and \"..\" entries."

  #+allegro
  (directory pathspec :directories-are-files nil)

  #+lispworks
  (directory (merge-pathnames *wild-entry* pathspec)
              :directories t
              :link-transparency nil)
  #+sbcl
  (directory (merge-pathnames *wild-entry* pathspec)
             :resolve-symlinks nil)
  #+ccl
  (directory (merge-pathnames *wild-entry* pathspec)
                :directories t)
  #+abcl
  (directory (merge-pathnames *wild-entry* pathspec)
                :resolve-symlinks  nil)
  
  #+clisp
  (mapcar 'first
            (nconc (directory (merge-pathnames *wild-entry* directory)
                               :full  t)
                   (directory (merge-pathnames *wild-relative* directory)
                               :full  t))))


(defun file-directory-p (file)
  "Returns non-nil if the path is a directory."
  (uiop/filesystem:directory-exists-p file))


(defun glisp:run-command (&key command args directory)
  (let ((command-line (cons command args)))
    (multiple-value-bind (output error result)
	(uiop:with-current-directory (directory)
	  (uiop:run-program command-line :ignore-error-status t :error-output :string :output :string))
      (declare (ignore output))
      (unless (zerop result)
	(error "~&~a threw result code ~s with error: ~a
The command line was: ~%~%~s~%" command result error command-line)))))

;;
;; temporary-folder is potentially platform-specific so it is defined here. 
;;
(defun temporary-folder (&key (create? t))
  (let ((folder (funcall *temporary-folder-function*)))
    (if create? (ensure-directories-exist folder) folder)))


(defun temporary-file (&key (extension nil) create?)
  (let ((file (merge-pathnames (make-pathname :name (string (gensym))
                                              :type extension)
                               (temporary-folder))))
    (when create? 
      (with-open-file 
          (out file :direction :output :if-does-not-exist :create :if-exists :append)
	(declare (ignorable out))))
    ;;
    ;; FLAG -- change this to return true pathname, fix code which uses it
    ;;
    (namestring file)))





#-(or allegro lispworks sbcl ccl abcl clisp) (error "Need implementation for get-pid for currently running lisp~%")
(defun get-pid ()
  #+allegro (excl.osi:getpid) 
  #+lispworks (multiple-value-bind (status pid) 
                  (system:call-system-showing-output "echo $PPID" 
                                                     :show-cmd nil :output-stream nil) 
                (declare (ignore status))
                (read-from-string pid))
  #+sbcl (sb-posix:getpid)
  #+ccl (ccl::getpid)

  
  ;;
  ;; This ABCL implementation is lifted from Quicklisp's
  ;; slime-20121125-cvs/swank-abcl.lisp:
  ;;
  #+abcl
  (handler-case 
      (let* ((runtime 
              (java:jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java:jnew-array-from-array 
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod 		
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java:jmethod "java.lang.Runtime" "exec" 
                            (java:jcall 
                             (java:jmethod "java.lang.Object" "getClass")
                             command)))
             (process 
              (java:jcall runtime-exec-jmethod runtime command))
             (output 
              (java:jcall (java:jmethod "java.lang.Process" "getInputStream")
                          process)))
         (java:jcall (java:jmethod "java.lang.Process" "waitFor")
                     process)
	 (loop :with b :do 
	    (setq b 
		  (java:jcall (java:jmethod "java.io.InputStream" "read")
			      output))
	    :until (member b '(-1 #x0a))	; Either EOF or LF
	    :collecting (code-char b) :into result
	    :finally (return 
		       (parse-integer (coerce result 'string)))))
    (t () 0)))

;;
;; FLAG Lifted from swank-clisp.lisp:
;;
#+clisp
(let ((getpid (or (find-symbol "PROCESS-ID" :system)
		  ;; old name prior to 2005-03-01, clisp <= 2.33.2
		  (find-symbol "PROGRAM-ID" :system)
		  #+win32 ; integrated into the above since 2005-02-24
		  (and (find-package :win32) ; optional modules/win32
		       (find-symbol "GetCurrentProcessId" :win32)))))
  (defun getpid ()			; a required interface
    (cond
      (getpid (funcall getpid))
      #+win32 ((ext:getenv "PID"))	; where does that come from?
      (t -1))))

#-(and mswindows lispworks)
(defun run-gs (command)
  "Shell out a ghostscript command and handle errors."
  (gdl:print-variables command)
  (multiple-value-bind (output error result)
      (run-program command)
    (declare (ignore output))
    (unless (zerop result) (error "Ghostscript threw error: ~a~%" error))))

#+(and mswindows lispworks)
(defun run-gs (command)
  "Shell out a ghostscript command and handle errors."
  (let ((result 
	 (system:run-shell-command command)))
    (unless (zerop result) (error "Ghostscript threw error"))))



(defun run-program (command &rest keys
		    &key ignore-error-status force-shell
		      (input nil inputp) (if-input-does-not-exist :error)
		      output (if-output-exists :supersede)
		      (error-output nil error-output-p) (if-error-output-exists :supersede)
		      (element-type #-clozure uiop:*default-stream-element-type* #+clozure 'character)
		      (external-format uiop:*utf-8-external-format*)
                      &allow-other-keys)
  "Three values: output, error-output, and return-code. This is a trivial wrapper on uiop:run-program with all the same arguments."
  (cond ((and error-output-p inputp) (apply #'uiop:run-program command 
					    :ignore-error-status ignore-error-status
					    :force-shell force-shell
					    :input input
					    :if-input-does-not-exist if-input-does-not-exist
					    :output output
					    :if-output-exists if-output-exists
					    :error-output error-output
					    :if-error-output-exists if-error-output-exists
					    :element-type element-type
					    :external-format external-format
					    keys))
	(inputp (apply #'uiop:run-program command 
		       :ignore-error-status ignore-error-status
		       :force-shell force-shell
		       :input input
		       :if-input-does-not-exist if-input-does-not-exist
		       :output output
		       :if-output-exists if-output-exists
		       :if-error-output-exists if-error-output-exists
		       :element-type element-type
		       :external-format external-format
		       keys))
	(t (apply #'uiop:run-program command 
		  :ignore-error-status ignore-error-status
		  :force-shell force-shell
		  :input input
		  :if-input-does-not-exist if-input-does-not-exist
		  :output output
		  :if-output-exists if-output-exists
		  :if-error-output-exists if-error-output-exists
		  :element-type element-type
		  :external-format external-format
		  keys))))
	

(defun run-shell-command (&rest args)
  (warn "~&run-shell-command is deprecated, please use run-program.~%")
  (apply #'run-program args))


(defun find-windows-gs ()
  (let ((gs-dirs (list "gs8.63/" "gs9.10/"))
	(parent-dirs (remove 
		      nil
		      (list (and glisp:*gdl-home* (probe-file (merge-pathnames "gpl/gs/" glisp:*gdl-home*)))
			    (and glisp:*gdl-home* (probe-file (merge-pathnames "../gpl/gs/" glisp:*gdl-home*)))
			    (and glisp:*gdl-program-home* (probe-file (merge-pathnames "gpl/gs/" glisp:*gdl-program-home*)))
			    (and glisp:*genworks-source-home* 
				 (probe-file (merge-pathnames "gpl/gs/" glisp:*genworks-source-home*)))
			    "c:/gs/"))))
    (block :daddy
    (dolist (gs-dir gs-dirs)
      (dolist (parent-dir parent-dirs)
	(let ((candidate (merge-pathnames "bin/gswin32c.exe" 
					  (merge-pathnames gs-dir parent-dir))))
	  (when (probe-file candidate) (return-from :daddy candidate))))))))

(defun find-gs-path (&optional gs-path)
  (let ((gs-path
	 (or gs-path
	     (if (or (featurep :os-windows)
                     (featurep :windows))
		 (find-windows-gs)
		 (or (probe-file #p"~/bin/gs")
		     (probe-file #p"/usr/local/bin/gs")
		     (probe-file #p"/sw/bin/gs")
		     (probe-file #p"/opt/local/bin/gs")
		     (probe-file #p"/usr/bin/gs") "gs")))))
    (unless  (and gs-path (probe-file gs-path))
      (warn "Gnu Ghostscript was not found. PNG and JPEG output will not function.

You can set it manually with (glisp:set-gs-path <path-to-gs-executable>).~%"))
    gs-path))

(defun set-gs-path (&optional gs-path)
  (setq gdl:*gs-path* (find-gs-path gs-path))
  (format t "gdl:*gs-path* has been set to ~a.~%" gdl:*gs-path*)) 



(defparameter *enable-utf8?* t)

(defparameter *base64-encode-func* 
  #+allegro #'(lambda(string)
		(excl:string-to-base64-string string :external-format (if *enable-utf8?* :utf-8 :default)))
  #-allegro
  #'(lambda(string)
      (if *enable-utf8?*
	  (cl-base64:usb8-array-to-base64-string 
	   (babel:string-to-octets string :encoding :utf-8))
	  (cl-base64:string-to-base64-string string))))


(defparameter *base64-decode-func* 
  #+allegro
  #'(lambda(string)
      (excl:base64-string-to-string string :external-format (if *enable-utf8?* :utf-8 :default)))
  #-allegro
  #'(lambda(string)
      (if *enable-utf8?*
	  (babel:octets-to-string 
	   (cl-base64:base64-string-to-usb8-array string) :encoding :utf-8)
	  (cl-base64:base64-string-to-string string))))



(defun class-slots (class)
  #-(or allegro lispworks sbcl ccl) (error "Need implementation for class-slots for currently running lisp.~%")
  (#+allegro mop:class-slots
   #+lispworks hcl:class-slots 
   #+sbcl sb-mop:class-slots 
   #+ccl ccl:class-slots class))

;;
;; from Hunchentoot:
;;

(defun get-backtrace ()
  "Returns a string with a backtrace of what the Lisp system thinks is
the \"current\" error."
  (handler-case
      (with-output-to-string (s) (uiop:print-backtrace :stream s))
    (error (condition)
      (format nil "Could not generate backtrace: ~A." condition))))


(defun gc-full ()
  "Force a garbage collection."
  #+allegro  (excl:gc t)
  #+lispworks  (hcl:gc-all)
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  )


(defun initialize-multiprocessing ()
  #+lispworks (mp:initialize-multiprocessing)
  ;;
  ;; Don't have to do it in Allegro - see about other CLs when we get here. 
  ;;
  )



(defun match-regexp (string-or-regexp string-to-match
                     &key newlines-special case-fold return
		       (start 0) end shortest)

  (when (or newlines-special case-fold return shortest)
    (error "keyword arguments :newlines-special, :case-fold, :return, and :shortest 
are no longer supported by glisp:match-regexp.~%"))

  #+allegro (excl:match-regexp string-or-regexp string-to-match :start start :end end)
  
  #-allegro (let ((end-default (or end (length string-to-match))))
	      (cl-ppcre:scan string-or-regexp string-to-match :start start :end end-default)))


(defun patches-dir ()
  nil)


(defun process-run-function (name-or-options preset-function &rest initial-bindings)
  (bt:make-thread preset-function :name name-or-options :initial-bindings initial-bindings))

(defun sexpr-from-file (pathname &optional if-does-not-exist-error?)
  (if (probe-file pathname)
      (with-open-file (in pathname) (read in))
      (when if-does-not-exist-error? (error "~&The File `~a' does not exist.~%" pathname))))


(defun replace-regexp (string regexp to-string)
  (cl-ppcre:regex-replace-all regexp string to-string))



#+(or allegro sbcl)
(defun room-report (&optional (gc? t))
  "Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. 
 This is the main output from this object and gives a general overview of memory state. The especially
 noteworthy values are usually the :free-used-other and :old-used-other.

:&optional ((gc nil) \"Determines whether to do a full gc before probing the room data.\")
"
  (when gc? 
    (if (integerp gc?) (dotimes (n gc?) (glisp:gc-full)) (glisp:gc-full))) 
  (let ((gdl:self (gdl:make-object 'room-report))) (gdl:the room-data)))
    


;;#+ccl 
#+nil
(gdl:define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (gdl:the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (list :new-free-cons 0 :new-used-cons 0 :new-free-other 0 :new-used-other 0))

   (old-data (list :old-free-cons 0 :old-used-cons (gdl:the cons-bytes) :old-used-other (- (gdl:the space-total) (gdl:the cons-bytes))))
   
   (cons-bytes (gdl:the (parse-line-data "cons objects.")))

   (space-total (gdl:the (parse-line-data "space total.")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (let* ((data (append (gdl:the new-data) (gdl:the old-data)))
		     (total (+ (getf data :new-used-cons)
			       (getf data :old-used-cons)
			       (getf data :new-used-other)
			       (getf data :old-used-other))))
		(let ((mb (/ (gdl:round-to-nearest total 1000000) 1000000)))
		  (append data (list :total total
				     :MB-int mb
				     :MB (format nil "~a MB" mb)))))))

  :functions ((parse-line-data
	       (report-label)
	       (let* ((line (find report-label (gdl:the room-lines) :test #'search))
		      (components (remove "" (glisp:split-regexp "\\s" line) :test #'string-equal))
		      

		 components)))))



#+sbcl
(gdl:define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (gdl:the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (list :new-free-cons 0 :new-used-cons 0 :new-free-other 0 :new-used-other 0))

   (old-data (list :old-free-cons 0 :old-used-cons (gdl:the cons-bytes) :old-used-other (- (gdl:the space-total) (gdl:the cons-bytes))))
   
   (cons-bytes (gdl:the (parse-line-data "cons objects.")))

   (space-total (gdl:the (parse-line-data "space total.")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (let* ((data (append (gdl:the new-data) (gdl:the old-data)))
		     (total (+ (getf data :new-used-cons)
			       (getf data :old-used-cons)
			       (getf data :new-used-other)
			       (getf data :old-used-other))))
		(let ((mb (/ (gdl:round-to-nearest total 1000000) 1000000)))
		  (append data (list :total total
				     :MB-int mb
				     :MB (format nil "~a MB" mb)))))))

  :functions ((parse-line-data
	       (report-label)
	       (let ((line (find report-label (gdl:the room-lines) :test #'search)))
		 (parse-integer (glisp:replace-regexp (first (glisp:split-regexp "\\s" (string-trim (list #\space) line))) "," ""))))))



#+allegro
(gdl:define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (gdl:the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (gdl:the (parse-line-data "New")))
   (old-data (gdl:the (parse-line-data "OTot")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (append (gdl:the new-data) (gdl:the old-data))))

  :functions ((parse-line-data
	       (report-label)
	       (let ((prefix (cond ((string-equal report-label "New") "new")
				   ((string-equal report-label "OTot") "old"))))
		 (let ((report-line (find t (gdl:the room-lines)
				   :test #'(lambda(item report-line)
					     (declare (ignore item))
					     (and (search report-label report-line)
						  (not (search "-----" report-line)))))))
		   (let ((fields (remove "" (glisp:split-regexp "\\s" report-line) :test #'string-equal)))
		 
		     (let ((cons (third fields))
			   (other (fourth fields)))
		       (append
			(destructuring-bind (free used)
			    (glisp:split-regexp ":" cons)
			  (list (gdl:make-keyword (format nil "~a-free-cons" prefix)) (parse-integer free)
				(gdl:make-keyword (format nil "~a-used-cons" prefix)) (parse-integer used)))
			(destructuring-bind (free used)
			    (glisp:split-regexp ":" other)
			  (list (gdl:make-keyword (format nil "~a-free-other" prefix)) (parse-integer free)
				(gdl:make-keyword (format nil "~a-used-other" prefix)) (parse-integer used)))))))))))


(defun slot-definition-name (slot-definition)
  #-(or allegro lispworks sbcl ccl) (error "Need implementation for slot-definition-name for currently running lisp.~%")
  (#+allegro mop:slot-definition-name
   #+lispworks hcl:slot-definition-name 
   #+sbcl sb-mop:slot-definition-name 
   #+ccl ccl:slot-definition-name slot-definition))

(defun snap-folder ()
  (or #+allegro (probe-file (merge-pathnames "snaps/" "sys:"))
      (ensure-directories-exist (merge-pathnames "snaps/" (glisp:temporary-folder)))))

(defun socket-bytes-written (socket)
  #-allegro (declare (ignore socket))
  #+allegro (excl::socket-bytes-written socket)
  #-allegro (progn (warn "Returning 0 for socket-bytes-written -- 

please find implementation for the currently running lisp.~%")
                   0))

(defun split-regexp (regexp string)
  (cl-ppcre:split regexp string))

(defmacro with-heuristic-case-mode ((&rest args) &body body)
  (declare (ignore args))
  #-allegro `(let ((*print-case* :downcase)) ,@body)
  #+allegro `(progn ,@body))








(defmacro without-redefinition-warnings (&rest body)
  #+allegro
  `(excl:without-redefinition-warnings ,@body)
  #+sbcl
  `(locally 
       (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
	 ((sb-kernel:redefinition-warning #'muffle-warning)) ,@body))
  #-(or allegro sbcl)
  (progn
    (warn "Need an implementation for without-redefinition-warnings for ~a~%." (lisp-implementation-type))
    `(progn ,@body)))

(defun rsync (source dest &key directory 
                            print-command?
                            dry-run?
			    options-plist
                            (options (list "a"))
                            long-form-options)
  #+os-windows (declare (ignore source dest directory  print-command? dry-run?
                                options long-form-options))
  #+os-windows (error "~&Sorry, glisp:rsync is not yet implemented for MS Windows.~%")

  #-mswindows
  (flet ((expanded-pathname-string (pathname)
	   (let ((namestring (namestring pathname)))
	     (if (or (search "@" namestring) (search ":" namestring)) pathname
		 (replace-regexp (namestring (translate-logical-pathname pathname)) "~/"
				 (namestring (user-homedir-pathname))))))
         (dotted-relative-pathname (source-list)
           (destructuring-bind (head tail) source-list
             (format nil "/~{~a/~}./~{~a/~}" head tail))))

    (labels ((plist-keys (plist) (when plist
				   (cons (first plist) (plist-keys (rest (rest plist))))))
	     (plist-values (plist) (when (consp (rest plist))
				     (cons (second plist) (plist-values (rest (rest plist)))))))

      (let ((command-list 
             (remove nil
                     (cons "rsync" 
                           (cons (when options (format nil "-~{~a~}" options))
				 (append 
                                  (mapcar #'(lambda(option) (concatenate 'string "--" option))
                                          long-form-options)

				  (mapcan #'(lambda(key value)
					      (list (format nil "--~(~a~)" key)
						    value)) (plist-keys options-plist) (plist-values options-plist))
				
				  (list
                                   (etypecase source
                                     (list (dotted-relative-pathname source))
                                     ((or string pathname)
                                      (expanded-pathname-string source))))

                                  (list (if (search ":" (namestring dest)) dest
                                            (expanded-pathname-string (namestring dest))))))))))
	(when (or print-command? dry-run?) (format t "~s~%" command-list))
	(unless dry-run?
          (uiop:with-current-directory (directory) 
	    ;;
	    ;; FLAG capture this pattern as a standard form of run-program:
	    ;;
            (multiple-value-bind (output error-output return-code)
		(run-program command-list :output :string :error-output :string :ignore-error-status t)
              (unless (zerop return-code)
		(error "The following command returned an error:

~s

The output was:

~a

The error-output was:

~a

and the return-code was:

~a

"
                       command-list output error-output return-code)))))))))

#+nil
(defun rsync (source dest &key directory 
                            print-command?
                            dry-run?
                            (options (list "a"))
                            long-form-options)
  #+os-windows (declare (ignore source dest directory  print-command? dry-run?
                                options long-form-options))
  #+os-windows (error "~&Sorry, glisp:rsync is not yet implemented for MS Windows.~%")
  #-mswindows
  (labels ((expanded-pathname-string (pathname)
             (replace-regexp (namestring (translate-logical-pathname pathname)) "~/"
                             (namestring (user-homedir-pathname))))
           (dotted-relative-pathname (source-list)
             (destructuring-bind (head tail) source-list
               (format nil "/~{~a/~}./~{~a/~}" head tail))))

    (let ((command-list 
           (remove nil
                   (cons "rsync" 
                         (cons (when options (format nil "-~{~a~}" options))
                               (append 
                                (mapcar #'(lambda(option) (concatenate 'string "--" option))
                                        long-form-options)
                                (let ((source
                                       (list
                                        (etypecase source
                                          (list (dotted-relative-pathname source))
                                          ((or string pathname)
                                           (expanded-pathname-string (namestring source)))))))
                                  (gdl:print-variables source) source)
                                (list (if (search ":" (namestring dest)) dest
                                          (expanded-pathname-string (namestring dest))))))))))
      (when (or print-command? dry-run?) (format t "~s~%" command-list))
      (unless dry-run?
        (uiop:with-current-directory (directory) 
          ;;
          ;; FLAG capture this pattern as a standard form of run-program:
          ;;
          (multiple-value-bind (output error-output return-code)
              (run-program command-list :output :string :error-output :string :ignore-error-status t)
            (unless (zerop return-code)
              (error "The following command returned an error:

~s

The output was:

~a

The error-output was:

~a

and the return-code was:

~a

"
                     command-list output error-output return-code))))))))
