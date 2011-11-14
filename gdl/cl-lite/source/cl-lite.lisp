(in-package :gdl)

(defun cl-lite (pathname &rest args)
  "
   Traverses pathname in an alphabetical depth-first order, compiling
   and loading any lisp files found in source/ subdirectories. A lisp
   source file will only be compiled if it is newer than the
   corresponding compiled fasl binary file, or if the corresponding
   compiled fasl binary file does not exist. A bin/source/ will be
   created, as a sibling to each source/ subdirectory, to contain the
   compiled fasl files.

   If the :create-fasl? keyword argument is specified as non-NIL, a
   concatenated fasl file, named after the last directory component of
   pathname, will be created in. 

   Please see codebase-directory-node object for additional inputs (which can be 
given as keyword args to this function)."
  (glisp:begin-redefinitions-ok)
  (let ((result
         (let ((package-file-ht (glisp:make-sans-value-equalp-hash-table)))
           (remf args :show-redefinition-warnings? )
           (with-compilation-unit ()
             (let ((object (apply #'make-object 'codebase-directory-node :pathname pathname args)))
               (unless (the-object object dry-run?)
                 (the-object object (compile-and-load :package-file-ht package-file-ht)))
               (when (the-object object create-asd-file?)
                 (the-object object (write-asd-file))))))))
    (glisp:end-redefinitions-ok)
    result))


(defun cl-patch (pathname &rest args)
  " 
   Traverses pathname in a manner identical to cl-lite, but only those
   files for which the source is newer than the corresponding fasl
   binary file (or for which the corresponding fasl binary file does
   not exist) will be loaded. Use this for incremental updates where
   the unmodified source files do not depend on the modified source
   files."
  (apply #'cl-lite pathname :load-always? nil args))


(defun cl-config (pathname-host config-name &key create-fasl?)
  (let* ((base-dir (pathname-directory
                    (translate-logical-pathname pathname-host)))
         (base-device (pathname-device 
                       (translate-logical-pathname pathname-host)))
         
         (config-spec (with-open-file (in (make-pathname :directory (append base-dir (list "configs"))
                                                         :device base-device
                                                         :name config-name
                                                         :type "config"))
                        (read in)))
         (bin-files
          (apply #'append
                 (mapcar #'cl-lite 
                         (mapcar #'(lambda(component)
                                     (make-pathname :directory (append base-dir component)
                                                    :device base-device))
                                 config-spec)))))
      
    (when create-fasl?
      (glisp:concatenate-fasls bin-files (make-pathname :directory (append base-dir (list "configs"))
                                                        :device base-device
                                                        :name config-name
                                                        :type glisp:*fasl-extension*)))))


(eval-when (compile load eval)
  (define-object directory-node nil

    :input-slots
    (pathname
     (strings-for-display (format nil "~a" (the :local-name)))
     (subdir-type 'directory-node)
     (additional-parameters nil))

    :computed-slots
    ((device (pathname-device (the ppathname)))
     (ppathname (translate-logical-pathname (the pathname)))
     (contents (sort (glisp:directory-list (the ppathname))
                     #'(lambda (x y)
                         (string< (file-namestring x) (file-namestring y)))))
     (local-name (lastcar (pathname-directory (the ppathname))))
     (subdir-pathnames (mapcar #'pathname-directory
                               (remove-if-not #'glisp:file-directory-p (the contents)))))

    :trickle-down-slots
    (device)

    :objects
    ((subdirs :type (the :subdir-type)
              :sequence (:size (length (the :subdir-pathnames)))
              :pathname (make-pathname
                         :device (the :device)
                         :directory (nth (the-child :index) (the :subdir-pathnames)))
              :parameters (the :additional-parameters)))))


(define-object codebase-directory-node (directory-node)

  :documentation (:description "Models a filesystem directory for use by the cl-lite program."
                  )


  :input-slots
  (("Plist of keywords and lists of strings. Maps directory names to their default type classifications."
    type-mapping (list :tables '(("tables") ("table")) :source
                       '(("source" "icons") ("lisp" "cl" "gdl")) :qa '(("qa") ("lisp"))
                       :exercises '(("exercises"))))
   ("List of strings. Identifies the names of directories considered to hold binaries.
Default is (list \"bin\" \"patch\")"
    bin-subdir-names (list "bin" "patch"))
   
   ("List of strings. Identifies the names of directories which are part of a vc-system control files
and therefore should be treated as special subdirectories. 
Default is (list \"CVS\")"
    special-subdir-names (list "CVS"))
   
   
   (fasl-type glisp:*fasl-extension*)

   ("String or pathname object. Designates the pathname for the filesystem directory in which
the built concatenated fasls are written. Defaults to <tt>(glisp:temporary-folder)</tt>" 
    fasl-output-path (glisp:temporary-folder))
   
   (fasl-output-device (pathname-device (the fasl-output-path)))
   
   (fasl-output-directory (pathname-directory (the fasl-output-path)))
   
   ("String. Names the built concatenated fasl when <tt>(the create-fasl?)</tt> is non-nil. 
Defaults to <tt>(the local-name)</tt>
"
    fasl-output-name (the local-name))
   
   ("String. Names the fasl extension used by the compiler. Defaults to the local fasl output type."
    fasl-output-type glisp:*fasl-extension*)
   
   (parent-dir (the :parent))
   
   ("List of strings. Lists directory names which should be ignored as having 
compilable source code for the build."
    source-files-to-ignore (list "system"))
   
   ("Boolean. Determines whether to write a concatenated fasl for the build. Defaults to nil.

NOTE: this is not currently supported in cl-lite.
"
    create-fasl? nil)

   (create-asd-file? nil)
   
   (dry-run? (the create-asd-file?))
   
   ("Boolean. Determines whether to load the individual compiled fasls even if the source has not changed.
Defaults to nil (i.e. we assume we are loading into a clean system and need all the initial definitions.)."
    load-always? t))

  :computed-slots
  ((additional-parameters (list :type-mapping (the :type-mapping)
                                :bin-subdir-names (the :bin-subdir-names)
                                :special-subdir-names (the :special-subdir-names)
                                :fasl-type (the :fasl-type) :load-always?
                                (the :load-always?)))
   (subdir-type 'codebase-directory-node)
   (subdir-pathnames (the :subdir-computation :subdir-pathnames))
   (type-matches (the :subdir-computation :type-matches))
   (relevant-files (the :file-computation :relevant-files))
   (binary-directory (let ((base-dir (pathname-directory (the :ppathname))))
                       (let ((binary-directory
                              (make-pathname
                               :device (the :device)
                               :directory (append
                                           (butlast base-dir)
                                           (list "bin" (lastcar base-dir))))))
                         (ensure-directories-exist binary-directory)
                         (pathname-directory binary-directory))))
   (local-package-files (remove-if-not
                         #'(lambda (file)
                             (and (string-equal (pathname-name file) "package")
                                  (or (string-equal (pathname-type file) "lisp")
                                      (string-equal (pathname-type file)
                                                    "cl"))))
                         (the :contents)))
   (package-files (sort
                   (flatten (append (the :local-package-files)
                                    (mapcar #'(lambda
                                                  (subdir)
                                                (the-object
                                                 subdir
                                                 :package-files))
                                            (list-elements (the :subdirs)))))
                   #'string<
                   :key #'pathname-name))
   
   (asd-file (merge-pathnames (make-pathname :name (string-append "gdl-" (the fasl-output-name))
                                             :type "asd")
                              (the ppathname)))
   
   (asdf-depends-on (let ((depends-on-file 
                           (merge-pathnames (make-pathname :name "depends-on"
                                                           :type "isc")
                                            (the ppathname))))
                      (when (probe-file depends-on-file)
                        (with-open-file (in depends-on-file)
                          (read in)))))
   
   (asdf-system-list 
    (let ((binaries (the compile-and-load)))
      (append `(asdf:defsystem ,(read-from-string (format nil "#:~a" (pathname-name (the asd-file))))
                   :description "Auto-generated asdf defsys from Genworks GDL cl-lite."
                   :author "Genworks and Dave Cooper unless otherwise indicated"
                   :license "AGPL unless otherwise indicated"
                   :serial t
                   :version ,(string-append 
                              (replace-substring (iso-8601-date (get-universal-time)) "-" "") "00")
                   :depends-on ,(the asdf-depends-on)
                   :components ,(mapcar #'(lambda(binary) 
                                            (let ((binary (make-pathname :directory (remove "bin" 
                                                                                            (pathname-directory binary)
                                                                                            :test #'string-equal)
                                                                         :defaults binary)))
                                              (list :file 
                                                    (namestring 
                                                     (make-pathname :name (pathname-name binary)
                                                                    :type nil
                                                                    :defaults
                                                                    (enough-namestring binary (the ppathname)))))))
                                        binaries))))
    :uncached))
   

  :hidden-objects
  ((subdir-computation :type 'subdir-computation
                       :exclude-names (append (the :bin-subdir-names)
                                              (first (getf (the :type-mapping) :exercises)))
                       :subdirs-unordered-in (the :subdirs-unordered)
                       :subdirs-in (the :subdirs)
                       :pathname (the :ppathname)
                       :pass-down (:local-name :contents :type-mapping :parent-dir))
   (file-computation :type 'file-computation
                     :pass-down (:pathname :type-mapping :type-matches :contents :subdir-pathnames
                                           :source-files-to-ignore))
   (subdirs-unordered :type (the :subdir-type)
                      :sequence (:size (length (the :subdir-computation :subdir-pathnames-unordered)))
                      :pathname (make-pathname
                                 :device (the :device)
                                 :directory (nth (the-child :index)
                                                 (the :subdir-computation
                                                   :subdir-pathnames-unordered)))
                      :parameters (the :additional-parameters)))

  :functions
  ((write-asd-file 
    ()
    (with-open-file (out (the asd-file) :direction :output 
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (let ((*print-right-margin* 70))
        (pprint (the asdf-system-list) out)))
    (the asd-file))
   
   
   (compile-and-load
    (&key (create-fasl? (the :create-fasl?)) 
          (package-file-ht (make-hash-table))
          (dry-run? (the dry-run?)))
    (let ((list-of-binaries nil)
          (sources-found?
           (and (or (getf (the :type-matches) :source)
                    (getf (the :type-matches) :tables))
                (or (getf (the :relevant-files) :source)
                    (getf (the :relevant-files) :tables))))
          (packages-found? (the :package-files)))
      (when (or sources-found? packages-found?)
        (format *trace-output* "~&~%; CL-LITE is now processing: ~a...~%~%"
                (the :ppathname)))
      (when (and (not dry-run?) packages-found?)
        (mapc #'(lambda (file)
                  (if (null (gethash file package-file-ht))
                      (progn (load file) (setf (gethash file package-file-ht) t))))
              (the :package-files)))
      (dolist (subdir (list-elements (the :subdirs)))
        (let ((list
               (the-object subdir
                           (:compile-and-load :package-file-ht package-file-ht
                                              :dry-run? dry-run?))))
          (setq list-of-binaries (append list-of-binaries list))))
      (when (getf (the :type-matches) :source)
        (dolist (file (getf (the :relevant-files) :source))
          (let ((output-file
                 (make-pathname
                  :directory (the :binary-directory)
                  :device (the :device)
                  :name (pathname-name file)
                  :type (the :fasl-type))))
            (let ((output-date (file-write-date output-file))
                  (file-date (file-write-date file)))
              (when (and (not dry-run?)
                         (or (null output-date) (<= output-date file-date)))
                (compile-file file :output-file output-file))
              (setq list-of-binaries (append list-of-binaries (list output-file)))
              (when (and (not dry-run?)
                         (or (the :load-always?)
                             (or (null output-date) (<= output-date file-date))))
                (load output-file))))))
      
      (when (getf (the :type-matches) :tables)
        (dolist (file (getf (the :relevant-files) :tables))
          (let ((output-file
                 (make-pathname
                  :directory (the :binary-directory)
                  :device (the :device)
                  :name (pathname-name file)
                  :type (the :fasl-type))))
            (let ((output-date (file-write-date output-file))
                  (file-date (file-write-date file)))
              (when (or (null output-date) (<= output-date file-date))
                (funcall (read-from-string "peruse-file") file :load nil :output-file output-file))
              (setq list-of-binaries (append list-of-binaries (list output-file)))
              (when (or (the :load-always?)
                        (or (null output-date) (<= output-date file-date)))
                (load output-file))))))
      
      (if create-fasl?
          (let ((fasl-name
                 (make-pathname
                  :directory (the fasl-output-directory)
                  :device (the fasl-output-device)
                  :name (the fasl-output-name)
                  :type (the fasl-output-type))))
            (glisp:concatenate-fasls list-of-binaries fasl-name)
            (format *trace-output* "~%~%Created Fasl File: ~a~%~%" fasl-name) 
            fasl-name
            )
        list-of-binaries)))))


(define-object file-computation ()

  :input-slots
  (pathname
   type-mapping
   type-matches
   contents
   subdir-pathnames
   source-files-to-ignore)

  :computed-slots
  ((ordering-bias (let ((ordering-info-file
                         (make-pathname
                          :directory (pathname-directory (the :pathname))
                          :device (the :device)
                          :name "file-ordering"
                          :type "isc")))
                    (when (probe-file ordering-info-file)
                      (with-open-file (in ordering-info-file) (read in)))))
   (ignore-list (let ((ignore-list-file
                       (make-pathname
                        :directory (pathname-directory (the :pathname))
                        :device (the :device)
                        :name "ignore-list"
                        :type "isc")))
                  (when (probe-file ignore-list-file)
                    (with-open-file (in ignore-list-file) (read in)))))
   (relevant-files (let (key)
                     (mapcar #'(lambda (thing)
                                 (if (not (listp thing))
                                     (setq key thing)
                                   (let ((ordered-pathnames
                                          (remove

                                           nil
                                           (mapcar
                                            #'(lambda
                                                  (name)
                                                (find

                                                 name
                                                 (getf
                                                  (the :relevant-files-unordered)
                                                  key)

                                                 :test
                                                 #'string=
                                                 :key
                                                 #'pathname-name))
                                            (the :ordering-bias)))))
                                     (remove-duplicates
                                      (append ordered-pathnames thing)
                                      :test #'equalp
                                      :from-end t))))
                             (the :relevant-files-unordered))))
   (relevant-files-unordered (apply #'append
                                    (mapcar #'(lambda
                                                  (key)
                                                (list
                                                 key
                                                 (when
                                                     (getf (the :type-matches) key)
                                                   (sort

                                                    (remove-if

                                                     #'(lambda
                                                           (file)
                                                         (and
                                                          (eql key :source)
                                                          (member

                                                           (pathname-name file)
                                                           (append
                                                            (the
                                                                :source-files-to-ignore)
                                                            (the :ignore-list))

                                                           :test
                                                           #'string=)))
                                                     (remove-if-not

                                                      #'(lambda
                                                            (file)
                                                          (member

                                                           (pathname-type file)
                                                           (second
                                                            (getf
                                                             (the :type-mapping)
                                                             key))

                                                           :test
                                                           #'string=))
                                                      (set-difference

                                                       (the :contents)
                                                       (the :subdir-pathnames))))
                                                    #'(lambda
                                                          (x y)
                                                        (string<

                                                         (file-namestring x)
                                                         (file-namestring y)))))))
                                            (ind-filter
                                             #'evenp
                                             (the :type-mapping)))))))


(define-object subdir-computation ()

  :input-slots
  (pathname
   local-name
   contents
   exclude-names
   subdirs-in
   subdirs-unordered-in
   type-mapping
   parent-dir)

  :computed-slots
  ((ordering-bias (let ((ordering-info-file
                         (make-pathname
                          :directory (pathname-directory (the :pathname))
                          :device (the :device)
                          :name "system-ordering"
                          :type "isc")))
                    (when (probe-file ordering-info-file)
                      (with-open-file (in ordering-info-file) (read in)))))
   (ignore-list (let ((ignore-list-file
                       (make-pathname
                        :directory (pathname-directory (the :pathname))
                        :device (the :device)
                        :name "ignore-list"
                        :type "isc")))
                  (when (probe-file ignore-list-file)
                    (with-open-file (in ignore-list-file) (read in)))))
   (subdir-pathnames-unordered (sort
                                (set-difference
                                 (mapcar #'pathname-directory
                                         (remove-if-not #'glisp:file-directory-p
                                                        (the :contents)))
                                 (the :exclude-names)
                                 :test #'(lambda
                                             (path name)
                                           (and
                                            (the :exclude-names)
                                            (string-equal

                                             name
                                             (lastcar path)))))
                                #'string-lessp
                                :key #'lastcar))
   (subdir-pathnames (remove-if
                      #'(lambda (pathname)
                          (and (the :ignore-list)
                               (member pathname (the :ignore-list)
                                       :test #'(lambda
                                                   (path name)
                                                 (string-equal name (lastcar path))))))
                      (remove-duplicates
                       (append (mapcar #'(lambda
                                             (name)
                                           (append
                                            (pathname-directory

                                             (the :pathname))
                                            (list name)))
                                       (the :ordering-bias))
                               (the :subdir-pathnames-unordered))

                       :from-end t
                       :test #'equalp)))
   (type-matches (apply #'append
                        (mapcar #'(lambda (key)
                                    (list key
                                          (member

                                           (the :local-name)
                                           (first (getf (the :type-mapping) key))

                                           :test
                                           #'string=)))
                                (ind-filter #'evenp (the :type-mapping)))))))

   

(defun ind-filter (fn list)
  (let (result
        (count -1))
    (dolist (elem list (nreverse result))
      (when (funcall fn (incf count))
        (push elem result)))))



