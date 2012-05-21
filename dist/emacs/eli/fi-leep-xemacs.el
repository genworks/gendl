;; Copyright (c) 1993-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; The xemacs side of presentations in a lisp-listener window.

(defvar composer::init-presentations
    "(progn
      (princ \";; Converting *terminal-io* for presentations...\n\")
      (force-output)
      (setq excl::*command-table*
	    (excl::find-command-table 'lep::listener-command-table))
      (lep::mogrify-terminal-io)
      (force-output)
      (values))\n")

(defvar fi::window-stream-presentation)
(defvar fi::presentation-stack)
(defvar fi::incomplete-input)
(defvar fi::mode-motion-extent)

(defun composer::make-presenting-listener (&optional new-screen-p)
  (interactive)
  (let* ((proc (fi:open-lisp-listener
		-1
		nil
		(function
		 (lambda (proc)
		   (concat
		    composer::init-presentations
		    (fi::setup-tcp-connection proc))))))
	 (buffer (process-buffer proc)))
    (set-buffer buffer)

    (fi:setup-epoch-gesture-bindings)

    (make-local-variable 'fi::window-stream-presentation)
    (setq fi::window-stream-presentation
      (make-presentation :start 0 :end 8388607))
  
    (make-local-variable 'fi::presentation-stack)
    (setq fi::presentation-stack (list fi::window-stream-presentation))
    
    (make-local-variable 'fi::incomplete-input)
    (setq fi::incomplete-input nil)
    (setq fi::subprocess-filter-output-preprocess-hook
      'fi::leep-subprocess-filter-output-preprocess-hook)
    (setq fi::subprocess-filter-insert-output-hook
      'fi::leep-subprocess-filter-insert-output-hook)
    (set-process-filter proc 'fi::subprocess-filter)
    
    (make-local-variable 'fi::mode-motion-extent)
    (setq fi::mode-motion-extent nil)
    (setq mode-motion-hook 'fi::mode-motion-highlight)

    proc))

(defun fi::leep-subprocess-filter-output-preprocess-hook (output)
  (when fi::incomplete-input
    (setq output (concat fi::incomplete-input output))
    (setq fi::incomplete-input nil))
  output)

(defun fi::leep-subprocess-filter-insert-output-hook (output marker)
  ;; Presentation escape sequences are:
  ;;  &&		- escape a single &
  ;;  &lll<		- start a presentation at level lll
  ;;  &ddd>		- end presentation number ddd (arbitrary decimal int)
  (do ((pnt 0)
       (len (length output))
;;;       level
;;;       end
       index)
      ((or (eq pnt len)
	   (null (setq index (string-match "&" output pnt))))
       (when (< pnt len)
	 (fi::insert-string output pnt len)
	 (set-marker marker (point))))
    (unless (eq pnt index)
      (fi::insert-string output pnt index)
      (set-marker marker (point))
      (setq pnt index))
    (setq index (+ index 1))
    (cond ((eq index len)
	   (setq fi::incomplete-input "&"
		 pnt len))
	  ((eq (aref output index) ?&)
	   (insert-char ?& 1)
	   (set-marker marker (point))
	   (setq pnt (+ index 1)))
	  ((eq (aref output index) ?<)
;;	   (eq index (string-match "\\([0-9]\\)*<" output index))

;;	   (setq pnt (match-end 1))
;;	   (setq end (match-end 0))
;;	   (when (> pnt index)
;;	     (condition-case nil
;;		 (setq level (car (read-from-string
;;				   (substring output index pnt))))
;;	       (error (setq level nil))))
;;	   (when level
;;	     (while (> (length fi::presentation-stack) (1+ level))
;;	       (pop fi::presentation-stack)))
	   (let* ((pres (make-presentation :start (point) :end 0 :data 0))
		  (parent (car fi::presentation-stack))
		  (subs (presentation-subpresentation-vector parent))
		  ;;flag stream busy
		  (fi::window-stream-presentation nil))
	     (if subs
		 (let ((len (length subs))
		       (next (aref subs 0)))
		   (when (= next len)
		     (let ((new (make-vector (+ len len) nil)))
		       (setf (presentation-subpresentation-vector parent) new)
		       (dotimes (i next)
			 (aset new i (aref subs i)))
		       (setq subs new)))
		   (aset subs next pres)
		   (aset subs 0 (+ next 1)))
	       (setf (presentation-subpresentation-vector parent)
		 (vector 2 pres nil nil)))
	     (push pres fi::presentation-stack))
	   (setq pnt (+ index 1))
;;	   (setq pnt end)
	   )
	  ((eq index (string-match "\\([0-9]\\)+>" output index))
	   (setq pnt (match-end 0))
	   (let ((pres (pop fi::presentation-stack))
		 (fi::window-stream-presentation nil)) ;flag stream busy
	     (setf (presentation-end pres) (point))
	     (setf (presentation-data pres)
	       (car (read-from-string
		     output (match-beginning 1) (match-end 1))))
	     (set-marker marker (point))))
	  ((> (- len pnt) 10)	;broken protocol!!!
	   (fi::insert-string output pnt len)
	   (set-marker marker (point))
	   (setq pnt len))
	  (t (setq fi::incomplete-input (substring output (- index 1) len)
		   pnt len)))))

(defun fi::mode-motion-highlight (event)
  (let* ((window (event-window event))
	 ;;(screen (if window (window-frame window) (selected-frame)))
	 (buffer (and window (window-buffer window)))
	 (point (and buffer (event-point event)))
	 presentation)
    (save-excursion
      (set-buffer buffer)
      (if (and point
	       (setq presentation
		 (fi::presentation-at-point point
					    fi::window-stream-presentation)))
	  (let ((start (presentation-start presentation))
		(end (presentation-end presentation)))
	    (if (and fi::mode-motion-extent
		     (extent-object fi::mode-motion-extent))
		(set-extent-endpoints fi::mode-motion-extent start end)
	      (setq fi::mode-motion-extent (make-extent start end))
	      (set-extent-property fi::mode-motion-extent 'highlight)))
	;; zero the extent
	(if (and fi::mode-motion-extent
		 (extent-object fi::mode-motion-extent)
		 (not (eq (extent-start-position fi::mode-motion-extent)
			  (extent-end-position fi::mode-motion-extent))))
	    (set-extent-endpoints fi::mode-motion-extent 1 1))))))

(defvar fi:default-epoch-gesture-binding-list
    (list (list 'fi:epoch-gesture-describe   '(shift button1))
	  (list 'fi:epoch-gesture-inspect    '(control button1))
	  (list 'fi:epoch-gesture-edit       'button2)
	  (list 'fi:epoch-gesture-select     '(shift button2))
	  (list 'fi:epoch-gesture-menu       'button3))
  "*The mapping of mouse clicks onto logical gestures.
Each entry is a list of:

   - The command to send the gesture, and
   - the button on which to bind the gesture.

The function should be defined in this way:

   (defun fi:epoch-gesture-select (e)
     (interactive \"e\")
     (fi::interrupt-process-for-gesture e ':select))
")

(defun fi:setup-epoch-gesture-bindings ()
  (dolist (e fi:default-epoch-gesture-binding-list)
    (local-set-key (second e) (first e))))

(defun fi:epoch-gesture-select (e)
  (interactive "e")
  (fi::interrupt-process-for-gesture e ':select))

(defun fi:epoch-gesture-inspect (e)
  (interactive "e")
  (fi::interrupt-process-for-gesture e ':inspect))

(defun fi:epoch-gesture-edit (e)
  (interactive "e")
  (fi::interrupt-process-for-gesture e ':edit))

(defun fi:epoch-gesture-menu (e)
  (interactive "e")
  (fi::interrupt-process-for-gesture e ':menu))

(defun fi:epoch-gesture-describe (e)
  (interactive "e")
  (fi::interrupt-process-for-gesture e ':describe))

(defun fi::interrupt-process-for-gesture (event gesture)
  (save-excursion
    (set-buffer (window-buffer (event-window event)))
    (fi:eval-in-lisp
     (format
      "(mp:process-interrupt
		(mp::process-name-to-process \"%s\")
		#'composer::epoch-gesture %s %s)\n"
      (buffer-name (current-buffer))
      (let ((pres (fi::presentation-at-point (event-point event)
					     fi::window-stream-presentation)))
	(when pres (presentation-data pres)))
      gesture))))

;; This assumes that the presentations in the subpresentation-vector
;; do not have overlapping extents.

(defun fi::presentation-at-point (point p)
  (when (and point		;sometimes is nil
	     p)			;nil flags that window is being written
    (do ((winner nil)
	 subs)
	((or (null p)
	     (null (setq subs (presentation-subpresentation-vector p))))
	 winner)
      (setq p nil)
      (let* ((low 1)
	     (hih (aref subs 0)))
	(do (pres n nn)
	    ((or p
		 (eq n (setq nn (/ (+ low hih) 2)))))
	  (setq n nn)
	  (setq pres (aref subs n))
	  (cond ((<  point (presentation-start pres)) (setq hih n))
		((>= point (presentation-end   pres)) (setq low n))
		(t (setq p pres)
		   (setq winner pres))))))))
