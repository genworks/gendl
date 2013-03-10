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

(defparameter *fasl-extension*
    #+allegro excl:*fasl-default-type*
    #+lispworks compiler:*fasl-extension-string*
    #+sbcl sb-fasl:*fasl-file-type*
    #+ccl (namestring ccl:*.fasl-pathname*)
    #+abcl "abcl"
    #+clisp "fas"
    #-(or allegro lispworks sbcl ccl abcl clisp) (error "Need fasl extension string for the currently running lisp.~%"))


#-(or allegro lispworks sbcl ccl) 
(warn "~&Please implement concatenate-fasls for the currently running lisp.~%")

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


;;
;; FLAG -- replace with cl-fad version.
;;
#-(or allegro lispworks clozure)
(warn "please find a copy-directory from cl-fad or elsewhere for ~a~%" (lisp-implementation-type))
(defun copy-directory (from-dir to-dir &rest args)
  (declare (ignore args))
  #+allegro (excl:copy-directory from-dir to-dir)
  #+lispworks (cl-copy-directory to-dir from-dir)
  #+clozure (ccl::recursive-copy-directory from-dir to-dir )
  #-(or allegro lispworks clozure) 
  (error "~&copy-directory needed for ~a. Consider cl-fad.~%" (lisp-implementation-type)))


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
  (#-(or allegro lispworks) cl-fad:directory-pathname-p 
     #+allegro excl:file-directory-p 
     #+lispworks lw:file-directory-p file))

;;
;; temporary-folder is potentially platform-specific so it is defined here. 
;;
(defun temporary-folder (&key (create? t))
  (let ((folder (merge-pathnames "tmp/" (user-homedir-pathname))))
    (when create? (ensure-directories-exist folder))))

(defun temporary-file (&key (extension nil) create?)
  (let ((file (merge-pathnames (make-pathname :name (string (gensym))
                                              :type extension)
                               (temporary-folder))))
    (when create? 
      (with-open-file 
          (out file :direction :output :if-does-not-exist :create :if-exists :append)
	(declare (ignore out))))
    ;;
    ;; FLAG -- change this to return true pathname, fix code which uses it
    ;;
    (namestring file)))




