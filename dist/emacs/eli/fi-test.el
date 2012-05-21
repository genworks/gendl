
(setq load-path (cons default-directory load-path))

(load "fi-site-init")

(defmacro test-equal (expected-value test-form)
  (let ((expect (gensym))
	(val (gensym)))
    `(let* ((,expect ,expected-value)
	    (,val ,test-form))
       (when (not (equal ,expect ,val))
	 (error "expected %s, got %s for %s" ,expect ,val ',test-form)))))

(defun fi-test::tests ()
  (interactive)
  (test-equal '("12345612345" "6")
	      (fi::explode "123456123456" 1))
  (test-equal '("123456123456" "")
	      (fi::explode "123456123456" 1))
  (test-equal '("" "123456123456" "")
	      (fi::explode "123456123456" 1))
  (test-equal '("" "" "" "")
	      (fi::explode "" 1))
  (test-equal '("a" "b" "c" "d" "e")
	      (fi::explode "abcde" 1))
  (test-equal '("aaaa")
	      (fi::explode "aaaa" 1))
  (test-equal '("aa" "bb" "cc" "dd" "ee")
	      (fi::explode "aabbccddee" 1))
  )

(defun fi-test::prep-arglist-for-insertion ()
  (interactive)
  (test-equal '(a b c d)
	      (fi::prep-arglist-for-insertion-1
	       '(a b c d)
	       nil))
  
  (test-equal '(a "[b]" "[c]" "[d]" "args...")
	      (fi::prep-arglist-for-insertion-1
	       '(a &optional b c d &rest args)
	       nil))
  
  (test-equal '(a ":b" b ":c" c ":d" d)
	      (fi::prep-arglist-for-insertion-1
	       '(a &key b c d)
	       nil))
  
  (test-equal '(a ":b" b ":c" c ":d" d)
	      (fi::prep-arglist-for-insertion-1
	       '(a &key b c d &allow-other-keys)
	       nil))
  
  (test-equal '(a ":b" b "'c" c)
	      (fi::prep-arglist-for-insertion-1
	       '(a &key (b 'bee) ((c c1) 'cee))
	       nil))
  
  (test-equal '(a "[b]" "[c]" "[d]")
	      (fi::prep-arglist-for-insertion-1
	       '(a &optional b c d)
	       nil))
  
  (test-equal '(object ":stream" stream "':array" :array "':base" :base) 
	      (fi::prep-arglist-for-insertion-1
	       '(object &key stream ((:array *print-array*))
		 ((:base *print-base*)))
	       nil))
  
  (test-equal '((var "open-args...") "body...")
	      (fi::prep-arglist-for-insertion-1
	       '((var &rest open-args) &body body)
	       t))
  
  (test-equal '((var ":foo" foo ":bar" bar ":baz" baz) "body...")
	      (fi::prep-arglist-for-insertion-1
	       '((var &key foo bar baz) &body body)
	       t))
  
  (test-equal '(name varlist "body...")
	      (fi::prep-arglist-for-insertion-1 '(name varlist &rest body)
						t))
  )

(fi-test::tests)
(kill-emacs)
