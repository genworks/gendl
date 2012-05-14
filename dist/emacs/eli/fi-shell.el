;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

(defvar fi:shell-mode-map nil
  "The shell major-mode keymap.")

(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in shell mode.")

(defvar fi:shell-image-name
    (if (on-ms-windows)
	(if *on-windows-nt*
	    (format "%s/system32/cmd.exe" (getenv "WINDIR"))
	  "C:\\COMMAND.COM")
      "csh")
  "*Default Shell image to invoke from (fi:shell).  If the value
is a string then it names the image file or image path that
`fi:shell' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:shell-image-arguments
    (if (on-ms-windows)
	(if *on-windows-nt*
	    '("/q")
	  nil)
      '("-i"))
  "*Default Shell image arguments when invoked from (fi:shell).")

(defvar fi:shell-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar fi:shell-mode-use-history nil
  "*If non-nil when fi:shell-mode is first entered, setup a binding that
causes ! to do history processing and substitute the values from the
history list into the current command line.")

(defun fi:shell-mode (&optional mode-hook)
  "Major mode for interacting with an inferior shell.
The keymap for this mode is bound to fi:shell-mode-map:

<font face=\"Courier New\">\\{fi:shell-mode-map}</font>
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:shell-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (fi::kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:shell-mode)
  (setq mode-name "Shell")
  (fi::initialize-mode-map 'fi:shell-mode-map 'fi:shell-super-key-map 'shell)
  (use-local-map fi:shell-mode-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:shell-mode-hook))

(defun fi:shell-mode-bang (&optional arg)
  "Expand !$ in shell mode."
  (interactive "*p")
  (message "!-")
  (let ((c (read-char)))
    (cond
     ;;((= c ?!) (fi:pop-input arg))
     ((= c ?$) (fi:pop-input-last-word arg))
     (t (insert "!")
	;;(setq unread-command-char c)
	(insert-char c 1)))))

(defun fi:shell (&optional buffer-number)
  "Start a shell in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*shell*'
and end with an optional \"<N>\".  If BUFFER-NUMBER is not given it defaults
to 1.  If BUFFER-NUMBER is 1, then the trailing \"<1>\" is omited.  If
BUFFER-NUMBER is < 0, then the first available buffer name is chosen (a
buffer with no process attached to it.

The shell image file and image arguments are taken from the variables
`fi:shell-image-name' and `fi:shell-image-arguments'."
  (interactive "p")
  (fi::make-subprocess nil
		       "shell"
		       buffer-number
		       default-directory
		       'fi:shell-mode
		       fi:shell-prompt-pattern
		       fi:shell-image-name
		       fi:shell-image-arguments
		       (when (on-ms-windows)
			 'fi::subprocess-dos-filter)))

(defun fi::subprocess-dos-filter (process output &optional stay cruft)
  (fi::subprocess-filter process output stay t))
