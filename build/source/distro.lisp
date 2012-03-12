;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(in-package :gdl-build)

(define-object distro ()
  
  :input-slots
  ((overwrite? nil)

   (staging-directory #+mswindows "e:/staging/" #-mswindows "~/share/staging/")

   (release-directory #+mswindows "e:/release/" #-mswindows "~/share/staging/")

   (release-name-mapping '(("acl-8.2m-linux-x86" .  "gdl1581-linux")
			   ("acl-8.2m-win-x86" .  "gdl1581-windows")))

   (os-name-mapping '(("win" . "windows")
		      ("linux" . "linux")
		      ("macos" . "macos")))

   (known-releases (mapcar #'first (the release-name-mapping))))

  :computed-slots
  ((staging-release-pairs (let (result)
			    (dolist (entry (glisp:directory-list (the staging-directory)) (nreverse result))
			      (when (glisp:file-directory-p entry)
				(let* ((full-name (lastcar (pathname-directory entry)))
				       (base (when (>= (length full-name)(length "-2011010100"))
					       (subseq full-name 0 (- (length full-name)
								      (length "-2011010100"))))))
				  (when (member base (the known-releases) :test #'string-equal)
				    (push (cons entry 
						(merge-pathnames (format nil "~a/~a~a/program/"
									 (the (os-name-lookup base))
									 (rest (assoc base (the release-name-mapping)
										      :test #'string-equal))
									 (subseq full-name (length base)))
								 (the release-directory))) result))))))))

  :objects
  ((pairs :type 'staging-release-pair
	  :sequence (:size (length (the staging-release-pairs)))
	  :pair (nth (the-child index) (the staging-release-pairs))
	  :pass-down (overwrite?)))

  :functions
  ((os-name-lookup 
    (base)
    (dolist (mapping (the os-name-mapping))
      (when (search (first mapping) base) (return (rest mapping)))))

   (make!
    ()
    (dolist (pair (list-elements (the pairs))) (the-object pair make!)))))


(define-object staging-release-pair ()
  :input-slots (pair overwrite?)

  :computed-slots ((source (first (the pair)))
		   (target (rest (the pair)))
		   (target-parent (merge-pathnames "../" (the target)))
		   (os (lastcar (butlast (pathname-directory (the target-parent))))))

  :functions
  ((make! 
    ()
    (when (and (probe-file (the target)) (the overwrite?))
      (glisp:delete-directory-and-files (the target-parent) :quiet t))

    (unless (probe-file (the target-parent))
      (glisp:copy-directory (the source) (the target))
      ;;
      ;; Documentation:
      ;;
      (ensure-directories-exist (merge-pathnames "documentation/" (the target-parent)))
      (glisp:copy-file (merge-pathnames "documentation/gdl-documentation.pdf" glisp:*genworks-source-home*)
		       (merge-pathnames "documentation/gdl-documentation.pdf" (the target-parent)))
      ;;
      ;; GDL source code:
      ;;
      (glisp:copy-directory glisp:*genworks-source-home*
			    (merge-pathnames "src/" (the target-parent)))
      (glisp:delete-directory-and-files (merge-pathnames "src/.git/" (the target-parent)) :quiet t)
      (delete-file  (merge-pathnames "src/.gitignore" (the target-parent)))
      ;;
      ;; Quicklisp:
      ;;
      (glisp:copy-directory (merge-pathnames "../../common/quicklisp/" glisp:*genworks-source-home*)
			    (merge-pathnames "quicklisp/" (the target-parent)))
      ;;
      ;; eli: 
      ;;
      (ensure-directories-exist (merge-pathnames "emacs/" (the target-parent)))
      (glisp:copy-directory (translate-logical-pathname "sys:eli;")
			    (merge-pathnames "emacs/eli/" (the target-parent)))
      (glisp:copy-file (merge-pathnames "dist/emacs/gdl.el" glisp:*genworks-source-home*)
		       (merge-pathnames "emacs/gdl.el" (the target-parent)))
      ;;
      ;; Startup batch file or script:
      ;;
      (if (string-equal (the os) "windows")
	  (glisp:copy-file (merge-pathnames "dist/run-gdl.bat" glisp:*genworks-source-home*)
			   (merge-pathnames "run-gdl.bat" (the target-parent)))
	  (glisp:copy-file (merge-pathnames "dist/run-gdl" glisp:*genworks-source-home*)
			   (merge-pathnames "run-gdl" (the target-parent))))

      ;;
      ;; smlib shared library
      ;;
      
      (let ((smlib-version "8.45"))
	(ensure-directories-exist (merge-pathnames (format nil "SMLib~a/" smlib-version) (the target-parent)))
	(let ((smlib-name (if (find-package :smlib) (funcall (read-from-string "glisp:smlib-name"))
			      (error "smlib-name not known (smlib module probably not loaded)."))))
	  (glisp:copy-file (merge-pathnames (format nil "../../common/SMLib~a/~a" smlib-version smlib-name)
					    glisp:*genworks-source-home*)
			   (merge-pathnames (format nil "SMLib~a/~a" smlib-version smlib-name)
					    (the target-parent)))))
      ;;
      ;; gpl on windows
      ;;
      (when (string-equal (the os) "windows")
	(glisp:copy-directory (merge-pathnames "../../common/gpl/" glisp:*genworks-source-home*)
			      (merge-pathnames "gpl/" (the target-parent))))))))


(defun distro (&rest args)
  (let ((self (apply #'make-object 'distro args)))
    (the make!)))