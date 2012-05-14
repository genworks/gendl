;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

(defvar fi::ss-help)
(setq fi::ss-help
    "Debugger commands:\\<fi:scan-stack-mode-map>

\\[fi:ss-continue]	:continue
\\[fi:ss-pop]	:pop
\\[fi:ss-reset]	:reset
\\[fi:lisp-delete-pop-up-window]	same as \\[fi:ss-quit] below
\\[fi:ss-set-current]	make frame under the point the current frame
\\[fi:ss-disassemble]	disassemble the function
\\[fi:ss-restart]	restart function (give prefix to specify different form)
\\[fi:ss-toggle-all]	toggle visibility of all frames (by default a subset are visible)
\\[fi:ss-next-frame]	next frame
\\[fi:ss-edit]	edit source corresponding to function
\\[fi:ss-revert-stack]	revert stack from Lisp
\\[fi:ss-toggle-help-text]	Toggle help text
\\[fi:ss-locals]	display the lexical variables
\\[fi:ss-pprint]	pretty print
\\[fi:ss-quit]	switch back to \"%s\" buffer
\\[fi:ss-return]	return a value
\\[fi:ss-previous-frame]	previous frame

Type \\[fi:ss-toggle-help-text] to hide this help summary.

")

(defvar fi:scan-stack-mode-map nil)
(defvar fi:scan-stack-mode-display-help t
  "*If non-nil, then display help text at the beginning of the scan stack
mode buffer.")

;;;;;

(defconst fi::ss-current-frame-regexp "^ ->")
(defconst fi::ss-ok-frame-regexp "^   (")
(defconst fi::ss-ghost-frame-regexp "^   \\[\\.\\.\\. ")
(defvar fi::ss-show-all-frames nil)

(defvar fi::ss-process-name nil)
(make-variable-buffer-local 'fi::ss-process-name)

(defvar fi::ss-debugger-from-buffer nil)
(make-variable-buffer-local 'fi::ss-debugger-from-buffer)

(defun fi:scan-stack (&optional all internal)
  "Debug a Common Lisp process, which is read, with completion, from the
minibuffer.   The \"Initial Lisp Listener\" is the default process.  The
debugging occurs on a stack scan, created by :zoom on the Common Lisp
process. With argument ALL, do a \":zoom :all t\"."
  (interactive "P")
  (when (not internal) (fi:lisp-push-window-configuration))
  (let ((process-name (fi::buffer-process))
	(from-buffer (when fi:subprocess-mode
		       (current-buffer))))
    (fi::make-request
     (lep::zoom-session :process-name process-name
			:all (or fi::ss-show-all-frames all))
     ;; Normal continuation
     ((from-buffer process-name) (stack)
      (let ((buffer-name (format "*debugger:%s*"
				 (fi::make-pretty-process-name
				  process-name))))
	(pop-to-buffer buffer-name)
	(if buffer-read-only (toggle-read-only))
	(erase-buffer)
	(insert stack)
	(goto-char (point-min))
	(fi:scan-stack-mode from-buffer process-name)
	(let ((buffer-read-only nil))
	  (insert (format (substitute-command-keys fi::ss-help)
			  (buffer-name
			   (or from-buffer
			       fi::ss-debugger-from-buffer))))
;;;;TODO:
;;;	  (when (null fi:scan-stack-mode-display-help)
;;;	    (fi:ss-hide-help-text))
	  )
	(goto-char (point-min))
	(re-search-forward fi::ss-current-frame-regexp)
	(beginning-of-line)))
     ;; Error continuation
     (() (error)
      (message "Cannot zoom on stack: %s" error)))))

(defun fi::make-pretty-process-name (process-name)
  ;; spaces to hyphens, and remove *'s
  (let ((s process-name)
	(i 0)
	(max (length process-name))
	(res "")
	(c nil))
    (while (< i max)
      (setq c (aref s i))
      (cond ((= ?* c))
	    ((= ?  c)
	     (setq res (concat res "-")))
	    (t (setq res (concat res (char-to-string c)))))
      (setq i (+ i 1)))
    res))

(defun fi:scan-stack-mode (&optional from-buffer process-name)
  "Major mode for debugging a Common Lisp process.
The keymap for this mode is bound to fi:scan-stack-mode-map

<font face=\"Courier New\">\\{fi:scan-stack-mode-map}</font>
Entry to this mode runs the fi:scan-stack-mode-hook hook."
  (let ((saved-from-buffer
	 ;; KILL-ALL-LOCAL-VARIABLES will kill fi::ss-debugger-from-buffer
	 fi::ss-debugger-from-buffer))
    (kill-all-local-variables)
    (setq fi::ss-debugger-from-buffer (or from-buffer
					saved-from-buffer)))
  (setq fi::ss-process-name process-name)
  (setq major-mode 'fi:scan-stack-mode)
  (setq mode-name "Scan stack mode")
  (if (null fi:scan-stack-mode-map)
      (let ((map (make-keymap))
	    (ccmap (make-keymap)))
	(define-key ccmap "\C-c"	'fi:ss-continue)
	(define-key ccmap "\C-p"	'fi:ss-pop)
	(define-key ccmap "\C-r"	'fi:ss-reset)
	(define-key ccmap " "		'fi:lisp-delete-pop-up-window)
	(define-key map "\C-c"	ccmap)
	
;;;;TODO: SPC is a bad binding for this:
	(define-key map " "	'fi:ss-toggle-help-text)
	(define-key map "."	'fi:ss-set-current)
	(define-key map "D"	'fi:ss-disassemble)
	(define-key map "R"	'fi:ss-restart)
	(define-key map "d"	'fi:ss-next-frame)
	(define-key map "a"	'fi:ss-toggle-all)
	(define-key map "e"	'fi:ss-edit)
	(define-key map "g"	'fi:ss-revert-stack)
	(define-key map "h"	'fi:ss-toggle-help-text)
	(define-key map "l"	'fi:ss-locals)
	(define-key map "p"	'fi:ss-pprint)
	(define-key map "q"	'fi:ss-quit)
	(define-key map "r"	'fi:ss-return)
	(define-key map "u"	'fi:ss-previous-frame)
	(setq fi:scan-stack-mode-map map)))
  (use-local-map fi:scan-stack-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t)
  (run-hooks 'fi:scan-stack-mode-hook))

(defun fi:ss-next-frame ()
  "Go to the next frame."
  (interactive)
  (beginning-of-line)
  (forward-char 3)
  (forward-sexp 1)
  (forward-line 1))

(defun fi:ss-previous-frame ()
  "Go to the previous frame."
  (interactive)
  (beginning-of-line)
  (forward-sexp -1)
  (beginning-of-line))

(defun fi:ss-reset ()
  "Do a :reset on the process being debugged.  This causes the process
being debugged to throw out to the outer most read-eval-print loop, and
causes the debugger buffer to be buried and the window configuration as it
was before this mode was entered to be restored."
  (interactive)
  (fi::do-tpl-command-on-process t nil "reset")
  (fi:ss-quit))

(defun fi:ss-continue ()
  "Do a :continue on the process being debugged.  This causes the process
being debugged to continue from a continuable error, taking the default
restart (restart number 0)."
  (interactive)
  (fi::do-tpl-command-on-process t nil "continue")
  (fi:ss-quit))

(defun fi:ss-pop ()
  "Do a :pop on the process being debugged.  This causes the process being
debugged to pop out to the next outer most read-eval-print loop, and
causes the debugger buffer to be buried and the window configuration as it
was before this mode was entered to be restored."
  (interactive)
  (fi::do-tpl-command-on-process t nil "pop")
  (if (> (fi::ss-current-break-level) 0)
      (fi:scan-stack fi::ss-show-all-frames t)
    (fi:ss-quit)))

(defun fi:ss-return ()
  "Do a :return on the process being debugged.  This causes the process
being debugged to return a value from the current frame, as if the error
never occured.  The form to evaluate to obtain the return value for the
current frame is read from the minibuffer and evaluated in the Common Lisp
environment.  The debugger buffer is buried and the window configuration as
it was before this mode was entered is restored."
  (interactive)
  (fi::do-tpl-command-on-process
      t
    t
    "return"
    (list 'read-from-string
	  (read-string "Form (evaluated in the Lisp environment): " "nil")))
  (fi:ss-quit))

(defun fi:ss-restart (new-form)
  "Do a :restart on the process being debugged.  This causes the process
being debugged to restart the execution of the function associated with the
current frame.  With argument NEW-FORM, a form to evaluate to obtain the
function and arguments to be restarted is read from the minibuffer and
evaluated in the Common Lisp environment.  The default function and
arguments are the ones in the current frame.   The debugger buffer is
buried and the window configuration as it was before this mode was entered
is restored."
  (interactive "P")
  (fi::do-tpl-command-on-process
      t
    t
    "restart"
    (when new-form
      (list 'read-from-string
	    (read-string "Form (evaluated in the Lisp environment): "))))
  (fi:ss-quit))

(defun fi:ss-edit ()
  "Find the source file associated with the function in the current frame
and pop up a buffer with that definition visible."
  (interactive)
  (fi::do-tpl-command-on-process nil t "edit"))

(defun fi:ss-revert-stack ()
  "Cause the stack in the debugger buffer to be synchronized with the
actual stack in the Common Lisp environment.  This is useful when commands
are typed in the *common-lisp* buffer which change the state of the process
being debugged."
  (interactive)
  (fi:scan-stack fi::ss-show-all-frames t))

(defun fi:ss-toggle-all ()
  "Toggle showing all frames in the currently debugged process stack.  By
default, there are certain types of frames hidden because they offer no
additional information."
  (interactive)
  (setq fi::ss-show-all-frames (not fi::ss-show-all-frames))
  (fi:scan-stack fi::ss-show-all-frames t))

(defun fi:ss-set-current ()
  "Make the frame to which the point lies the current frame for future
operations.  It is not necessary to use this command, usually, since most
commands make the frame to which the point lies the current frame before
performing their assigned action."
  (interactive)
  (let ((offset (fi::offset-from-current-frame)))
    (if offset
	(progn
	  (if (> offset 0)
	      (fi::do-tpl-command-on-process
	       nil nil "dn" offset ':zoom nil)
	    (fi::do-tpl-command-on-process
	     nil nil "up" (- offset) ':zoom nil))
	  (fi::make-stack-frame-current offset)))))

(defun fi:ss-quit ()
  "Quit debugging the Common Lisp process.  The debugger buffer is buried
and the window configuration as it was before this mode was entered is
restored."
  (interactive)
  (bury-buffer)
  (fi:lisp-delete-pop-up-window)
  (goto-char (point-max)))

(defun fi:ss-disassemble ()
  "Disassemble the function associated with the current frame, putting the
disassembly into a help buffer and positioning the point on the instruction
that will next be executed if the current error can be continued."
  (interactive)
  (let ((process-name (fi::buffer-process))
	(offset (fi::offset-from-current-frame)))
    (fi::make-request
     (lep::disassemble-session :process-name process-name :offset offset)
     ((offset) (text pc)
      (when offset (fi::make-stack-frame-current offset))
      (fi::show-some-text-1 text nil 'fi::disassemble-hook pc))
     (() (error)
      (message "Cannot dissassemble: %s" error)))))

(defun fi::disassemble-hook (pc)
  (when pc
    (when (re-search-forward (format "^[ \t]*%s:" pc) nil t)
      (beginning-of-line)
      (insert ">"))))

(defun fi:ss-locals ()
  "Find the local variables to the function associated with the current
frame, and display them in a help buffer.  See the Allegro CL compiler
switch compiler:save-local-names-switch for information on accessing local
variables in the debugger."
  (interactive)
  (let ((process-name (fi::buffer-process))
	(offset (fi::offset-from-current-frame)))
    (fi::make-request
     (lep::local-session :process-name process-name
			 :offset offset)
     ((offset) (text)
      (when offset (fi::make-stack-frame-current offset))
      (fi::show-some-text-1 text nil))
     (() (error)
      (message "Cannot find locals: %s" error)))))

(defun fi:ss-pprint ()
  "Pretty print the current frame, function and arguments, into a help
buffer."
  (interactive)
  (let ((process-name (fi::buffer-process))
	(offset (fi::offset-from-current-frame)))
    (fi::make-request
     (lep::pprint-frame-session :process-name process-name
				:offset offset)
     ((offset) (text)
      (when offset (fi::make-stack-frame-current offset))
;;;; figure out how to find the package
      (fi::show-some-text-1 text nil))
     (() (error)
      (message "Cannot pprint: %s" error)))))

(defun fi:ss-toggle-help-text ()
  "Toggle the help text at the beginning of the debugger buffer."
  (interactive)
  (cond (fi:scan-stack-mode-display-help
	 ;; hide help
	 (save-excursion
	   (widen)
	   (goto-char (point-min))
	   (or (re-search-forward "^Evaluation stack:$" nil t)
	       (goto-char (point-max)))
	   (beginning-of-line)
	   (narrow-to-region (point) (point-max))
	   (setq fi:scan-stack-mode-display-help nil)))
	(t ;; show help
	 (save-excursion (widen))
	 (recenter)
	 (setq fi:scan-stack-mode-display-help t))))

;;;
;;; internals
;;;

(defun fi::ss-current-break-level ()
  (fi:eval-in-lisp
   (format
    "(mp:process-progn (mp:process-name-to-process \"%s\")
      tpl::*break-level*)"
    (fi::buffer-process))))

(defun fi::do-tpl-command-on-process (done set-current-frame command
				      &rest args)
  (let ((process-name (fi::buffer-process))
	(offset (when set-current-frame
		  (fi::offset-from-current-frame))))
    (fi::make-request
     (lep::tpl-command-session :process-name process-name
			       :command command
			       :args args
			       :done done
			       :offset offset)
     ;; Normal continuation
     ((offset) (done)
      (if done
	  (fi:ss-quit)
	(when offset (fi::make-stack-frame-current offset))))
     ;; Error continuation
     ((process-name) (error)
      (message "Lisp error: %s" error)
      (beep)
      (sit-for 2)
      (if (y-or-n-p
	   (format "Revert stack from process \"%s\"? "
		   process-name))
	  (fi:scan-stack fi::ss-show-all-frames t))))))

(defun fi::offset-from-current-frame ()
  (beginning-of-line)
  (cond
   ((looking-at fi::ss-current-frame-regexp) nil)
   (t
    (when (looking-at fi::ss-ghost-frame-regexp)
      (error "Can't set the current frame to a ghost frame."))
    (when (not (looking-at fi::ss-ok-frame-regexp))
      (error "Not on a frame."))
    (let* ((down (save-excursion
		   (if (re-search-forward fi::ss-current-frame-regexp nil t)
		       nil
		     (if (re-search-backward fi::ss-current-frame-regexp nil t)
			 t
		       (error "Can't find current frame indicator.")))))
	   (lines 0)
	   ghost-frame)
      (save-excursion
	(while (progn (beginning-of-line)
		      (setq ghost-frame nil)
		      (and (not (looking-at fi::ss-current-frame-regexp))
			   (or (looking-at fi::ss-ok-frame-regexp)
			       (setq ghost-frame 
				 (looking-at fi::ss-ghost-frame-regexp)))))
	  (when (not ghost-frame) (setq lines (+ lines 1)))
	  (if down
	      (backward-sexp 1)
	    (forward-sexp 1)
	    (forward-line 1))))
      (if down lines (- lines))))))

(defun fi::make-stack-frame-current (offset)
  (toggle-read-only)
  (delete-char 3)
  (insert " ->")
  (toggle-read-only)
  (beginning-of-line)
  (save-excursion
    (let ((found
	   (if (< offset 0)
	       (progn
		 (end-of-line)
		 (re-search-forward fi::ss-current-frame-regexp nil t))
	     (re-search-backward fi::ss-current-frame-regexp nil t))))
      (when (not found)
	(error "Could not find old current frame."))
      (toggle-read-only)
      (replace-match "   ")
      (toggle-read-only))))

(defun fi::buffer-process ()
  (cond
   (fi::ss-process-name)
   (t (fi::read-lisp-process-name "Process to debug: "))))

(defun fi::read-lisp-process-name (prompt)
  (let* ((processes
	  (cdr (car (lep::eval-session-in-lisp
		     'lep::list-all-processes-session))))
	 (completions
	  (mapcar (function (lambda (x) (list x))) processes)))
;;;    (if (fboundp 'epoch::mapraised-screen)
;;;	(epoch::mapraised-screen (minibuf-screen)))
    (completing-read prompt completions nil t
		     "Initial Lisp Listener")))
