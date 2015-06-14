(in-package :glsite)

;;
;; FLAG -- this is built-in in gdl1587p006 and later. 
;;

(defun glisp:source-pathname ()
  #+allegro excl:*source-pathname*
  #+lispworks dspec:*source-pathname*
  #+sbcl (error "need source-pathname in sbcl~%")
  #+ccl ccl:*loading-file-source-file*
  #+clisp (error "need source-pathname in ccl~%")
  #+abcl (extensions:source-pathname)
  )


;;
;; FLAG -- In a production environment (where source directory might
;; not be there) this parameter must be set at initialization/startup
;; time to the correct location.
;;
(defparameter *templates-folder* 
  (translate-logical-pathname
   (make-pathname :name nil :type nil
		  :defaults (merge-pathnames "../static/templates/"
					     (glisp:source-pathname)))))

(defparameter *cache-static-content?* nil)
;;
;; Quick hack for a read-from-static function with local demand-filled
;; hash table, which can be used by all object instances so they can
;; share the static data. If we have a toplevel Gendl object presiding
;; over all session root-level objects, we might be able to do
;; something for this which is more elegant and uniform with Gendl
;; style:
;;
(let ((templates-folder *templates-folder*)
      (static-content-ht (make-hash-table)))

  (defun clear-static () (clrhash static-content-ht))
  
  (defun read-static (name)
    (let ((key (make-keyword name)))
      (or (gethash key static-content-ht)
	  (let ((value 
		 (let ((template 
			(merge-pathnames (format nil "~a.html" name) templates-folder)))
		   (if (probe-file template)
		       (with-open-file (stream template)
			 (let ((data (make-string (file-length stream))))
			   (read-sequence data stream) data))
		       (let ((warn-string (format nil "<p>No template found for ~a<p>" template)))
			 (progn (warn warn-string) warn-string))))))
	    (if *cache-static-content?* 
		(setf (gethash key static-content-ht) value) value))))))

; the landing page computes the header and footer
; and also holds references for all of the pages.
(define-object landing (base-ajax-sheet) 

  :computed-slots 
   ((section-names (list "about" "license" "downloads" "documentation" "demos" "getinvolved")) 

    (title "Gendl")
    
    (main-sheet-body 
     (with-cl-who-string ()
       (str (read-static "header"))

       ;;
       ;; FLAG - this wants to go at the top of every "page" 
       ;;
       (when gwl:*developing?* (str (the development-links)))

       (str (reduce #'(lambda (x y) (concatenate 'string x y))
		    (mapsend (the sections) :main-sheet-body)))
       (str (read-static "footer")))))

  :objects 
  ((sections :type 'section 
	     :sequence (:size (length (the section-names)))
	     :name (nth (the-child index) (the section-names)))))

(define-object section (base-ajax-sheet) 
  :input-slots 
  (name)

  :computed-slots 
  ((main-sheet-body 
    (concatenate 'string 
		 (with-cl-who-string () ((:a :name (the name))))
		 (read-static (the name))))))
