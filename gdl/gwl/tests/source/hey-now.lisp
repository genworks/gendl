(in-package :gwl-user)

(defparameter *locale-hash*
  (let ((strings 
	 `(:hey-now
	   (:english "Hey Now"
	    :chinese "嘿，现在"))))
    (let ((ht (make-hash-table :size (length strings))))
      (mapc #'(lambda(key value) (setf (gethash key ht) value))
	    (plist-keys strings) (plist-values strings)) ht)))
  
(defmacro locale-string (key) 
  (format t  "NOTE: Remove this locale-string version with May quicklisp release")
  `(let ((string (getf (gethash ,key *locale-hash*) (the lang))))
     (or string 
	 (progn (warn "~s not found in *locale-hash* for current *lang*: ~a"
		      ,key (the lang))
		(format nil "!! ~s !!" ,key)))))


(define-object hey-now (base-ajax-sheet)
 
  :input-slots ((language-default :english))

  :computed-slots
  ((lang (the language-choice value))

   (html-sections (list (the main-section)))

   (main-sheet-body (with-cl-who-string ()
		      (str (the main-section main-div)))))


  :objects
  ((main-section :type 'sheet-section 
		 :inner-html (with-cl-who-string ()
			       (:p (str (the development-links)))
			       (:p (:h1 (str (locale-string :hey-now))))
			       (:p (:fieldset (str (the language-choice html-string))))))

   (language-choice :type 'menu-form-control
		    :size 1
		    :prompt "Choose Language"
		    :ajax-submit-on-change? t
		    :default (the language-default)
		    :choice-plist (list :english "English" :chinese "中国的"))))
		    
(publish-gwl-app "/hey-now" 'hey-now)