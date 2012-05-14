;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; The epoch side of presentations in a lisp-listener window.

(defvar fi::epoch-has-zones (fboundp 'epoch::make-zone))

(defvar fi::highlighted-zone-style nil)
(defvar fi::highlighted-style nil)

(defvar fi::highlighted-zone-color "slategray")

(defun fi::ensure-minibuffer-visible ()
  (if (fboundp 'epoch::mapraised-screen)
      (epoch::mapraised-screen (minibuf-screen))))

(unless fi::epoch-has-zones
  (defun make-style ()
    (epoch::make-style))
  
  (defun epoch::move-zone (a b c)
    (epoch::move-button a b c))
  
  (defun epoch::make-zone ()
    (epoch::make-button))

  (defun epoch::set-zone-read-only (a b)
    (epoch::set-button-read-only a b))
  
  (defun epoch::set-zone-style (a b)
    (set-button-attribute a b))
  )

(defun fi::initialize-for-presenting-listeners ()
  (setq fi::highlighted-style (make-style))
  (if (and fi::highlighted-zone-color
	   fi::epoch-has-zones
	   (fboundp 'epoch::number-of-colors)
	   (> (epoch::number-of-colors) 2))
      (epoch::set-style-background fi::highlighted-style
				   fi::highlighted-zone-color)
    (epoch::set-style-background fi::highlighted-style (epoch::background)))
  
  (unless fi::epoch-has-zones
    (when fi::highlighted-zone-style
      (release-attribute fi::highlighted-zone-style))
    (setq fi::highlighted-zone-style (reserve-attribute)))
  
  (epoch::set-style-foreground fi::highlighted-style (epoch::foreground))
  
  (epoch::set-style-underline fi::highlighted-style
			      (if (and fi::highlighted-zone-color
				       fi::epoch-has-zones
				       (fboundp 'epoch::number-of-colors)
				       (> (epoch::number-of-colors) 2))
				  nil
				(epoch::foreground)))
  (if fi::epoch-has-zones
      (progn
	(setq fi::normal-style (make-style))
	(epoch::set-style-foreground fi::normal-style (epoch::foreground))
	(epoch::set-style-background fi::normal-style (epoch::background))

	(setq fi::highlighted-zone-style fi::highlighted-style))
    (epoch::set-attribute-style fi::highlighted-zone-style
				fi::highlighted-style)))

(push '(if (eq fi::emacs-type 'epoch)
	   (fi::initialize-for-presenting-listeners))
      fi::initialization-forms)

(defun composer::setup-buffer-for-presentations (buffer)
  (set-buffer buffer)
  (make-local-variable 'highlighted-presentation)
  (setq-default highlighted-presentation 'no-value) ;default value for
						    ;non-lisp buffers

  (make-local-variable 'window-stream-presentation)
  (setq window-stream-presentation (make-presentation :start 0 :end 8388607))
  
  ;; The presentation-stack local variable is a stack of presentations opened
  ;; but not yet closed.
  (make-local-variable 'presentation-stack)
  (setq presentation-stack (list window-stream-presentation))
  
  (make-local-variable 'incomplete-input)
  (setq incomplete-input nil)
  
  (make-local-variable 'highlighted-zone)
  (setq highlighted-zone (epoch::make-zone))
  
  (make-local-variable 'read-only-zone)
  (setq read-only-zone (epoch::make-zone))
  
  (epoch::move-zone read-only-zone 1 (point-max))
  (epoch::set-zone-read-only read-only-zone t)
  (epoch::set-zone-style highlighted-zone fi::highlighted-zone-style)
  (fi:setup-epoch-gesture-bindings))

(defvar fi:default-epoch-gesture-binding-list
    (and (eq fi::emacs-type 'epoch)
	 (list (list 'fi:epoch-gesture-describe   mouse-left   (+ mouse-shift))
	       (list 'fi:epoch-gesture-inspect    mouse-left   (+ mouse-control))
	       (list 'fi:epoch-gesture-edit       mouse-middle 0)
	       (list 'fi:epoch-gesture-select     mouse-middle (+ mouse-shift))
	       (list 'fi:epoch-gesture-menu       mouse-right  0)))
  "*The mapping of mouse clicks onto logical gestures.
Each entry is a list of length 3: The command to send the gesture,
the numeric epoch mouse code, and the epoch numeric shifts.
The function should be defined in this way:
  (defun fi:epoch-gesture-select (x)
    (fi::interrupt-process-for-gesture ':select))")

(defun fi:setup-epoch-gesture-bindings ()
  (dolist (e fi:default-epoch-gesture-binding-list)
    (local-set-mouse (second e) (third e) (first e))))

(defun fi:epoch-gesture-select (x)
  (fi::interrupt-process-for-gesture ':select))

(defun fi:epoch-gesture-inspect (x)
  (fi::interrupt-process-for-gesture ':inspect))

(defun fi:epoch-gesture-edit (x)
  (fi::interrupt-process-for-gesture ':edit))

(defun fi:epoch-gesture-menu (x)
  (fi::interrupt-process-for-gesture ':menu))

(defun fi:epoch-gesture-describe (x)
  (fi::interrupt-process-for-gesture ':describe))

(defvar composer::init-presentations
    "(progn
      (princ \";; Converting *terminal-io* for presentations...\n\")
      (force-output)
      (setq excl::*command-table*
	    (excl::find-command-table 'lep::listener-command-table))
      (lep::mogrify-terminal-io)
      (force-output)
      (values))\n")

(defun composer::make-presenting-listener (&optional new-screen-p)
  (when (and new-screen-p (fboundp 'create-screen))
    (let ((screen (create-screen "*listener*" epoch::screen-properties)))
      (epoch::map-screen screen)
      (epoch::select-screen screen)
      screen))
  (let* ((proc (fi:open-lisp-listener
		-1
		nil
		(function
		 (lambda (proc)
		   (concat
		    composer::init-presentations
		    (fi::setup-tcp-connection proc))))))
	 (buffer (process-buffer proc)))
    (composer::setup-buffer-for-presentations buffer)
    (set-process-filter proc 'fi::leep-subprocess-filter)
    proc))

(defun fi::leep-subprocess-filter (process output &optional stay cruft)
  "Filter output to buffer including presentations."
  ;;(set-buffer (process-buffer process))
  (let ((inhibit-quit t))
    (if cruft
	(setq output (fi::substitute-chars-in-string '((?\r)) output)))
    (when incomplete-input
      (setq output (concat incomplete-input output))
      (setq incomplete-input nil))
    (let* ((old-buffer (current-buffer))
	   (buffer (process-buffer process))
	   (in-buffer (eq buffer old-buffer))
	   (window-of-buffer (get-buffer-window buffer))
	   (no-window (or (null window-of-buffer)
			  (not (windowp window-of-buffer))))
	   (xmarker (process-mark process))
	   (marker (if (marker-position xmarker)
		       xmarker
		     (set-marker (make-marker) 0 buffer)))
	   (marker-point (marker-position marker))
	   (output-length (length output))
	   old-point
	   point-not-before-marker
	   new-point)
      ;; The three symbols below are not bound above because `(window-point)'
      ;;   for the selected window does not always return the same thing as the
      ;;   function `(point)' in that window!  [Version 18 is supposed to fix
      ;;   this bug.]
      ;; Note that there is no function that returns all of the windows that
      ;;   are currently displaying a buffer.  Because of this, not all windows
      ;;   will be updated properly by this filter function.  What should be
      ;;   done is to loop through all windows displaying the buffer and do
      ;;   `(set-window-point)' in each.
      (if (not in-buffer)
	  (progn
	    (set-buffer buffer)
	    (setq old-point
	      (if no-window
		  (point)
		(window-point window-of-buffer))))
	(setq old-point (point)))
      (setq point-not-before-marker (>= old-point marker-point))
      (setq new-point (if point-not-before-marker
			  (+ old-point output-length)
			old-point))
      (save-excursion
	;; Go to point of last output by fi::make-process and insert new
	;;   output there, preserving position of the marker.
	(goto-char marker-point)
	;; The code below works around what appears to be a display bug
	;;   in GNU Emacs 17.  If `(insert-before-markers)' is used when
	;;   the process marker (process-mark), window-start point
	;;   (window-start), and window point (point) are all coincident,
	;;   the window display `sticks' on the topmost line.  We use
	;;   `(insert-string)' followed by `(set-marker)' to avoid this
	;;   problem.  This also happens to be the way
	;;   `handle_process_output()' deals with this in `process.c'.

	;; Presentation escape sequences are:
	;;  &&		- escape a single &
	;;  &<		- start a presentation
	;;  &ddd>	- end presentation number ddd (arbitrary decimal int)

	(do ((pnt 0)
	     (len (length output))
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
		 (setq incomplete-input "&"
		       pnt len))
		((eq (aref output index) ?&)
		 (insert-char ?& 1)
		 (set-marker marker (point))
		 (setq pnt (+ index 1)))
		((eq (aref output index) ?<)
		 (let* ((pres (make-presentation :start (point) :end 0 :data 0))
			(parent (car presentation-stack))
			(subs (presentation-subpresentation-vector parent))
			(window-stream-presentation nil)) ;flag stream busy
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
		   (push pres presentation-stack))
		 (setq pnt (+ index 1)))
		((eq index (string-match "\\([0-9]\\)+>" output index))
		 (setq pnt (match-end 0))
		 (let ((pres (pop presentation-stack))
		       (window-stream-presentation nil)) ;flag stream busy
		   (setf (presentation-end pres) (point))
		   (setf (presentation-data pres)
		     (car (read-from-string output (match-beginning 1) (match-end 1))))
		   (let ((p (point)))
		     ;;(message "point is %s" (point))(sleep-for 2)
		     (set-marker marker p)
		     (epoch::move-zone read-only-zone 1 p))))
		((> (- len pnt) 10)	;broken protocol!!!
		 (fi::insert-string output pnt len)
		 (set-marker marker (point))
		 (setq pnt len))
		(t (setq incomplete-input (substring output (- index 1) len)
			 pnt len)))))

      (if (not in-buffer)
	  (if (and fi:subprocess-continuously-show-output-in-visible-buffer
		   point-not-before-marker)
	      ;; Keep window's notion of `point' in a constant relationship to
	      ;;   the process output marker.
	      (if no-window
		  (goto-char new-point)
		(set-window-point window-of-buffer new-point))
	    (if no-window
		t ;; Still there.
	      (set-window-point window-of-buffer old-point)))
	(goto-char new-point))
      (cond
       (in-buffer nil)
       (stay old-buffer)
       (t (set-buffer old-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mouse tracking for presentation highlighting...
;;;

(defun fi::leep-mouse-tracker (type value scr)
  ;; The existence of the buffer variable serves as a flag that
  ;; mouse events are interesting.
  (when (fi::leep-mouse-p)
    (fi::leep-mouse-tracker1 type value scr))

  ;; Pass event to next handler.
  (fi::pass-event type value scr)

  ;; Keep feeding myself fresh motion events.
  (epoch::query-pointer))

(defun fi::leep-mouse-button-handler (type value scr)
  (when (fi::leep-mouse-p)
    (fi::leep-mouse-button1 type value scr))
  (fi::pass-event type value scr))

;;; Pass event to next handler...
(defun fi::pass-event (type value scr)
  (let* ((myself (pop-event type))
	 (next-handler (pop-event type)))
    (push-event type next-handler)
    (when (and next-handler (functionp next-handler))
      (funcall next-handler type value scr))
    (push-event type myself)))

;;; Button handler...
(defun fi::leep-mouse-button1 (type value scr)
  ;; Clear presentations when mouse is down.
  (when (and (boundp 'mouse::downp) mouse::downp)
    (fi::leep-mouse-tracker1 type value scr)))

;;; Predicate for determining when handler is applicable.
(defun fi::leep-mouse-p ()
  (let ((epoch::event-handler-abort nil)
	(coords (fi::coords-at-mouse)))
    (when coords
      (save-excursion
	;; Look at the value of highlighted-presentation in the buffer
	;; where the mouse is, (not the current buffer which may not be
	;; where the mouse is).  See if there is a value assigned that
	;; indicates that this is a presenting listener...
	(set-buffer (cadr coords))
	(if (and (boundp 'highlighted-presentation)
		 (not (eq highlighted-presentation 'no-value)))
	    t
	  nil)))))

;;; Tracker...
(defun fi::leep-mouse-tracker1 (type value scr)
  ;; By default, Epoch only passes motion events when a button is down
  ;; (yuch).  Modified to pass all motion events, this *actually* gives
  ;; mouse sensitive, highlighted presentation

  ;; Highlight applicable presentation.
  (let ((epoch::event-handler-abort nil)
	(coords (fi::coords-at-mouse))
	presentation)
    (when coords
      (save-excursion
	(set-buffer (cadr coords))
	(setq presentation
	  (fi::presentation-at-point (car coords) window-stream-presentation))

	(cond ((null presentation)	;outside a presentation
	       (fi::set-highlight-zone-to-presentation nil))

	      ;; Make sure dragging outside and back in again highlights
	      ;; presentation.
	      ((eq presentation highlighted-presentation)
	       (fi::set-highlight-zone-to-presentation highlighted-presentation))

	      ;; New highlighted presentation.
	      (t
	       (setq highlighted-presentation presentation)
	       (fi::set-highlight-zone-to-presentation
		highlighted-presentation)))))))

(defun fi::add-leep-mouse-tracker ()
  (push-event 'motion 'fi::leep-mouse-tracker)
  (push-event 'button 'fi::leep-mouse-button-handler))

;;; this is dangerous.
(defun fi::remove-leep-mouse-tracker ()
  (pop-event 'motion)
  (pop-event 'button))

(push '(fi::add-leep-mouse-tracker) fi::initialization-forms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::coords-at-mouse ()
  (let* (x y pos
	 (w (selected-window))
	 (w-edges
	  (if fi::epoch-has-zones
	      (window-pixedges w)
	    (window-edges w)))
	 (left (car w-edges))
	 (top (elt w-edges 1)))
    (setq pos
      (if fi::epoch-has-zones
	  (query-pointer)
	(query-mouse)))
    ;;convert to window relative co-ordinates
    (setq x (- (car pos) left))
    (setq y (- (elt pos 1) top))
    (epoch::coords-to-point (+ x left) (+ y top))))

;; This assumes that the presentations in the subpresentation-vector
;; do not have overlapping extents.

(defun fi::presentation-at-point (point p)
  (when (and point			;sometimes is nil
	     p)				;nil flags that window is being written
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

(defun fi::set-highlight-zone-to-presentation (presentation)
  (when highlighted-zone
    (if presentation
	(epoch::move-zone highlighted-zone
			  (presentation-start presentation)
			  (presentation-end   presentation))
      (epoch::move-zone highlighted-zone 1 1))))

(defun fi::interrupt-process-for-gesture (gesture)
  (let ((coords (fi::coords-at-mouse)))
    (when coords
      (save-excursion
	(set-buffer (cadr coords))
	(fi:eval-in-lisp
	 (format
	  "(mp:process-interrupt
		(mp::process-name-to-process \"%s\")
		#'composer::epoch-gesture %s %s)\n"
	  (buffer-name (current-buffer))
	  (let ((pres (fi::presentation-at-point (car coords) window-stream-presentation)))
	    (when pres (presentation-data pres)))
	  gesture))))))
