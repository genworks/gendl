;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

(defmacro fi::with-keywords (variables rest-arg &rest body)
  (let ((let-bindings nil))
    (dolist (var variables)
      (push (list var
		  (list 'fi::getf-property 
			rest-arg 
			(list 'quote (intern (concat ":" (symbol-name var))))))
	    let-bindings))
    (list* 'let
	   (reverse let-bindings)
	   body)))

(defvar session) ;; yuck

(defun lep::create-listener-stream (&optional args)
  ;; This function has problems because it may be invoked asynchronously
  ;; by background computation.  See bug3267.
  ;; FSF emacs has strange scroll position when a buffer pops up because
  ;; it won't listen to any positioning operations until emacs input quiesces.
  
  (fi::with-keywords (parent x y width height splitp name no-select) args
    (let* ((conf (fi:lisp-push-window-configuration))
	   (fi::listener-protocol ':stream)
	   (proc
	    (save-window-excursion
	      (fi:open-lisp-listener
	       -1
	       (if name name "background-interaction")
	       (function
		(lambda (proc)
		  (format "%d\n%d\n"
			  (fi::session-id session)
			  (fi::tcp-listener-generation proc)))))))
	   (buffer (process-buffer proc)))
      (cond ((or parent x y width height)
	     (fi::create-new-mapped-screen-for-stream parent x y width height))
	    ((get-buffer-window buffer)
	     (select-window (get-buffer-window buffer)))
	    (splitp (split-window-vertically)))
      (when (get-buffer (buffer-name buffer))
	(when (null no-select)
	  (switch-to-buffer buffer)
	  (fi::ensure-buffer-visible buffer)
	  ;;(recenter 0)
	  )
	(setf (second conf) buffer))
      buffer)))

(defun fi::create-new-mapped-screen-for-stream (parent x y width height)
;;;  (and (boundp 'epoch::screen-properties)
;;;       (let ((props epoch::screen-properties))
;;;	 (when (stringp parent)
;;;	   (push
;;;	    (cons 'parent 
;;;		  (epoch::string-to-resource parent 
;;;					     (epoch::intern-atom "WINDOW")))
;;;	    props))
;;;	 ;; ** Do something with X,Y,WIDTH,HEIGHT
;;;	 (let ((screen (create-screen "*foo*" props)))
;;;	   (epoch::map-screen screen)
;;;	   (epoch::select-screen screen)
;;;	   screen)))
  )

