
;; This file has its (distant) roots in lisp/shell.el, so:
;;
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; This file is derived from part of GNU Emacs.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defun fi:set-associated-sublisp (buffer-name mode)
  "Use BUFFER-NAME as the name of a buffer which contains a Lisp subprocess
to be used for Emacs-Lisp interactions (evaluating expressions, etc) in all
buffers in MODE.  New buffers created in MODE will also use BUFFER-NAME."
  (interactive
   (let* ((buffer-name
	   (read-buffer "Buffer name containing a Lisp process: " nil t))
	  
	  (mode (or (and (memq major-mode '(fi:common-lisp-mode
					    fi:franz-lisp-mode))
			 major-mode)
					(let* ((alist '(("common-lisp" . fi:common-lisp-mode)
				    ("franz-lisp" . fi:franz-lisp-mode)))
			   (type
			    (completing-read "Lisp type: "
					     alist nil t "common-lisp")))
		      (cdr (assoc type alist))))))
     (list buffer-name mode)))
  (let* ((buffers (buffer-list))
	 (process (or (get-buffer-process (get-buffer buffer-name))
		      (error "There is no process associated with buffer %s!"
			     buffer-name)))
	 (proc-name (process-name process)))
    (cond ((eq mode 'fi:common-lisp-mode)
	   (setq fi::common-lisp-backdoor-main-process-name proc-name))
	  ((eq mode 'fi:franz-lisp-mode)
	   (setq fi:franz-lisp-process-name proc-name)))
    (while buffers
      (if (eq mode (fi::symbol-value-in-buffer 'major-mode
					       (car buffers)))
	  (fi::set-in-buffer 'fi::process-name proc-name
			     (car buffers)))
      (setq buffers (cdr buffers)))))

(defun fi::sublisp-select ()
  "Find a sublisp for eval commands to send code to.  Result stored in
the variable fi::process-name.  If fi::process-name is set, and there is an
associated process buffer, thats that. If fi::process-name is nil, or if
there is no process buffer with that name, then try for
freshest-<franz,common>-sublisp-name, which should contain the name of the
most recently started sublisp.  If neither of these exist, runs the command
franz-lisp or common-lisp, depending on the major mode of the buffer."
  ;; see if sublisp is named yet.  if its not, name it intelligently.
  (cond (fi::process-name)
	((or (eq major-mode 'fi:inferior-common-lisp-mode)
	     (eq major-mode 'fi:lisp-listener-mode))
	 (setq fi::process-name fi::common-lisp-backdoor-main-process-name))
	((eq major-mode 'fi:inferior-franz-lisp-mode)
	 (setq fi::process-name fi:franz-lisp-process-name))
	((eq major-mode 'fi:franz-lisp-mode)
	 (if fi:franz-lisp-process-name
	     (setq fi::process-name fi:franz-lisp-process-name)))
	((eq major-mode 'fi:common-lisp-mode)
	 (if fi::common-lisp-backdoor-main-process-name
	     (setq fi::process-name
	       fi::common-lisp-backdoor-main-process-name)))
	(t
	 (if fi::common-lisp-backdoor-main-process-name
	     (setq fi::process-name fi::common-lisp-backdoor-main-process-name)
	   (error "Cant start a subprocess for Major mode %s." major-mode))))
  ;; start-up the sublisp process if necessary and possible
  (cond ((and fi::process-name
	      (let ((p (get-process fi::process-name)))
		(fi:process-running-p p))))
	((eq major-mode 'fi:franz-lisp-mode)
	 (if (and fi:franz-lisp-process-name 
		  (get-process fi:franz-lisp-process-name))
	     (setq fi::process-name fi:franz-lisp-process-name)
	   (setq fi::process-name (save-excursion (fi:franz-lisp)))))
	((eq major-mode 'fi:common-lisp-mode)
	 (if (and fi::common-lisp-backdoor-main-process-name 
		  (get-process fi::common-lisp-backdoor-main-process-name))
	     (setq fi::process-name fi::common-lisp-backdoor-main-process-name)
	   (setq fi::process-name (save-excursion (fi:common-lisp)))))
	(t (error "Can't start a subprocess for sublisp-name %s."
		  fi::process-name)))
  (if (processp fi::process-name)
      (setq fi::process-name (process-name fi::process-name)))
  nil)

;; Support for mode-line status indicators.

(defvar fi:allegro-run-status-string "    ")

(defun fi:show-run-status ()
  "Cause CL to show Run/Wait/GC status in ACL buffer mode lines.
This is normally called automatically from fi:start-lisp-interface-hook."
  (interactive "")
  (if fi::started-via-file
      (fi::ensure-lep-connection)
    (if (fi::lep-open-connection-p)
	nil
      (if (or (null fi::common-lisp-backdoor-main-process-name)
	      (not (fi:process-running-p
		    (get-process fi::common-lisp-backdoor-main-process-name))))
	  (error "Common Lisp must be running to show run bars."))))
  (save-excursion
    (let* ((buffer
	    (if fi::common-lisp-backdoor-main-process-name
		(process-buffer
		 (get-process fi::common-lisp-backdoor-main-process-name))
	      (fi::connection-buffer fi::*connection*)))
	   (proc (fi::open-network-stream "Run Bar Process"
					  nil
					  (fi::get-buffer-host buffer)
					  (fi::get-buffer-port buffer))))
      (set-process-filter   proc 'fi::show-run-status-filter)
      (set-process-sentinel proc 'fi::show-run-status-sentinel)
      (process-send-string
       proc (format "%s \n" (fi::prin1-to-string fi::listener-protocol)))
      (process-send-string proc (format "\"%s\" \n" (process-name proc)))
      (process-send-string
       proc (format "%d \n" (fi::get-buffer-password buffer)))
      (process-send-string proc "
 (progn (ignore-errors (excl::run-status-process))
        (mp:process-kill mp:*current-process*)) 
")
      proc)))

(defun fi::show-run-status-sentinel (process status)
  (setq fi:allegro-run-status-string "    ")
  t)

(defun fi::show-run-status-filter (proc string)
  (let ((len (length string)))
    (if (> len 4)
	(setq string (substring string (- len 4)))))
  (setq fi:allegro-run-status-string string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))

(defun fi::install-mode-line-run-status ()
  (setq mode-line-buffer-identification
    '("ACL " fi:allegro-run-status-string " %12b")))

(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::install-mode-line-run-status)
(add-hook 'fi:common-lisp-mode-hook          'fi::install-mode-line-run-status)
(add-hook 'fi:lisp-listener-mode-hook        'fi::install-mode-line-run-status)

(add-hook 'fi:start-lisp-interface-hook      'fi:show-run-status t)
