(in-package :blog-engine)

(defparameter *source-path* 
  (make-pathname :directory (pathname-directory (glisp:source-pathname))
					   :name nil :type nil))
(defparameter *db-path* 
  (merge-pathnames "../db/blog.db" *source-path*))
(defun initialize ()
  (publish-directory 
   :prefix "/blog-static/"
   :destination (format nil "~a" (probe-file (merge-pathnames "../static/" *source-path*)))))
(initialize)

; ------------------------------------------------------------------------------ ; 
; OBJECT: User
(define-object blog-user () 
  :input-slots 
  ((username)
   (password)
   (email)))

; OBJECT: Blog entry
(define-object blog-entry () 
  :input-slots 
  ((title)
   (content)
   (author)
   (datetime)))

; OBJECT: Comment
(define-object comment () 
  :input-slots 
  ((content)
   (author)
   (datetime)))

; ------------------------------------------------------------------------------ ; 
; blog-sheet: 
; Given a username (author), loads a sequence of blog-entries and displays 
; them neatly, with a link to each entry. 
(define-object blog-sheet (sheet-section)
  :input-slots ((author)) 

  :computed-slots
  ; Blog entries as lists of slots/strings
  ; Here is where the loading from the database should occur. 
  ((blog-entry-list 
    
    (progn 
      ; connect to the database
      
      ; query for all blog entries by (the author)
      ; for each one, map over and create a plist of the slots.
    )
   (inner-html (with-cl-who-string () 
		 )))

  :objects 
  ; sequence of blog entries as child objects
  ((blog-entries :type 'blog-entry 
		 :sequence (:size (length (the blog-entry-list)))
		 :title (getf (nth (the-child index) (the blog-entry-list)) 
			      :title)
		 :content (getf (nth (the-child index) (the blog-entry-list)) 
				:content)
		 :author (getf (nth (the-child index) (the blog-entry-list))
			       :author)
		 :datetime (getf (nth (the-child index) (the blog-entry-list))
				 :datetime))))
		 
; blog-entry-sheet: 
; Given a blog-entry, loads a sequence of comments and displays the 
; blog entry and the comments, with a form to comment as well. 
(define-object blog-entry-sheet (base-html-sheet) 
)

; ------------------------------------------------------------------------------ ; 

; blog-engine/viewer: 
; This page displays blogs and blog entries (with comments). The latter also 
; contains a form for entering new comments. You do not need to be logged in 
; to see this page. Displays blog-sheet by default. 
(define-object blog-engine-new (base-ajax-sheet)

  :computed-slots
  ((main-sheet-body (with-cl-who-string () 
		      ()))))

; blog-engine/new: 
; This page contains the login/register form which can be used to log in or 
; to create a new account. 
(define-object blog-engine-new 
)