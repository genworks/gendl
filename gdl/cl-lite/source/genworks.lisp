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

(glisp:without-package-variance-warnings
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:nicknames :glisp)
    (:export #:*fasl-extension*
             #:concatenate-fasls
             #:directory-list
             #:file-directory-p
             #:temporary-folder
             #:temporary-file)))

(defparameter *fasl-extension*
    #+allegro excl:*fasl-default-type*
    #+lispworks compiler:*fasl-extension-string*
    #+sbcl sb-fasl:*fasl-file-type*
    #-(or allegro lispworks sbcl) (error "Need fasl extension string for the currently running lisp.~%"))


(defun concatenate-fasls (files dest)
  #-(or allegro lispworks sbcl) (error "~&Please implement concatenate-fasls for the currently running lisp.~%")

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
    (lispworks:concatenate-system dest 'my-system)))


(defvar *wild-entry*
  (make-pathname :name :wild :type :wild :version :wild))

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
             :resolve-symlinks nil))


(defun file-directory-p (file)
  "Returns non-nil if the path is a directory."
  (#-(or allegro lispworks) asdf-utils:directory-pathname-p 
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
          (out file :direction :output :if-does-not-exist :create :if-exists :append))) 
    ;;
    ;; FLAG -- change this to return true pathname, fix code which uses it
    ;;
    (namestring file)))




