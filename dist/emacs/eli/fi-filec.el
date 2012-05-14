;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; Command and file name completion

(defvar fi:shell-token-pattern "[ \t\n()<>&|;=]"
  "*The regular expression used by file name completion to mark path name
boundaries.")

(defvar fi::shell-completions-window nil
  "If non-nil, completion window requires cleaning up.")

(defun fi:shell-do-completion ()
  "Do either command or file name completion in a subprocess buffer
containing a shell (or other subprocess for which it would be useful, such
as Common Lisp or rlogin buffers)."
  ;; First, find out whether or not we are completing a command or file
  ;; name.  Then, if a command, determine if it can be reduced to file name
  ;; completion because it contains a slash (either absolute or relative,
  ;; it doesn't matter).
  (interactive)
  (let* ((completion-ignore-case nil)
	 (opoint (point))
	 (input-start
	  (save-excursion
	    (goto-char fi::last-input-end)
	    (if (re-search-forward fi::prompt-pattern opoint t)
		(point)))))
    (if input-start
	(if (save-excursion
	      (or (re-search-backward "[ \t]" input-start t)
		  (search-backward "/" input-start t)))
	    (call-interactively 'fi:shell-file-name-completion)
	  (call-interactively 'fi:shell-command-completion)))))

(defun fi:shell-file-name-completion ()
  "Perform file name completion in subprocess modes.  A completion buffer
is displayed when there is more than one completion for a partially
completed file name."
  (interactive)
  (let* ((shell-expand-string
	  (substitute-in-file-name (fi::shell-completion-default-prefix)))
	 (shell-expand-abbrev-string
	  (fi::expand-file-name-abbrevs shell-expand-string))
	 (shell-expand-dir nil)
	 (shell-expand-file nil)
	 (shell-expand-completion nil))
    
    ;; replace abbrev, if different than shell-expand-string
    (if (and shell-expand-abbrev-string
	     (not (string= shell-expand-string
			   shell-expand-abbrev-string)))
	(progn
	  (when (search-backward shell-expand-string nil t)
	    (replace-match shell-expand-abbrev-string t t)
	    (setq shell-expand-string shell-expand-abbrev-string))))
    
    ;; directory part of name
    (setq shell-expand-dir
      (or (file-name-directory shell-expand-string) default-directory))

    ;; file part of name
    (setq shell-expand-file
      (file-name-nondirectory shell-expand-string))
    
    ;; do the expansion
    (setq shell-expand-completion
      (file-name-completion shell-expand-file shell-expand-dir))

    ;; display the results
    (if (eq shell-expand-completion t) (message "Sole completion")
      (if (eq shell-expand-completion nil)
	  (message "No match")
	(if (equal shell-expand-completion shell-expand-file)
	    (progn
	      (if fi::shell-completions-window nil
		(setq fi::shell-completions-window
		  (current-window-configuration)))
	      (message "Making completion list...")
	      (with-output-to-temp-buffer " *Completions*"
		(display-completion-list
		  (sort (file-name-all-completions
			  shell-expand-completion shell-expand-dir)
			'string-lessp)))
	      (message ""))
	  ;; put in the expansion
	  (search-backward shell-expand-file)
	  (replace-match shell-expand-completion t t))))))

(defun fi::expand-file-name-abbrevs (filename)
  (catch 'fi::expand-file-name-abbrevs
    (let* ((flist (fi::explode filename ?/))
	   (fl flist)
	   (fn nil)
	   ft)
      (if (string= "" (car flist))
	  (progn
	    (setq flist (cdr flist))
	    (setq fl flist)))
      (while fl
	(setq ft (concat fn (if fn "/" "") (car fl)))
	(if (file-exists-p ft)
	    (setq fn ft)
	  (let ((c (condition-case ()
		       (file-name-completion (car fl) (or fn "."))
		     (error nil))))
	    (when (null c)
	      (throw 'fi::expand-file-name-abbrevs nil))
	    (setq fn
	      (concat (or fn "")
		      (if fn "/" "")
		      (if (and c (string-match "\\(.*\\)/$" c))
			  (substring c (match-beginning 1) (match-end 1))
			c)))))
	(setq fl (cdr fl)))
      fn)))

(defun fi:shell-command-completion ()
  "Perform command name completion in subprocess modes.  A completion buffer
is displayed when there is more than one completion for a partially
completed file name."
  (interactive)
  (let ((shell-expand-string (fi::shell-completion-default-prefix))
	(completions nil)
	(complete-alist nil)
	(dirs exec-path))
    ;;
    ;; Find all possible completions of `shell-expand-string' in the
    ;; exec-path (ie, PATH environment variable), comprised of only
    ;; executable file names.
    ;;
    (while dirs
      (condition-case ()
	  (let* ((dir (expand-file-name (car dirs)))
		 (res
		  (fi::executable-files
		   (file-name-all-completions shell-expand-string dir)
		   dir)))
	    (if res
		(setq completions (append res completions))))
	(error nil))
      (setq dirs (cdr dirs)))
    
    ;; `completions' is now a list of all possible completions

    ;; build `complete-alist' for `try-completions'
    (let ((names completions))
      (while names
	(setq complete-alist (cons (cons (car names) (car names))
				   complete-alist))
	(setq names (cdr names))))
    
    (cond
      ((null completions)
       (message "No match"))
      ((= 1 (length completions))
       (cond ((string= shell-expand-string (car completions))
	      (fi::shell-completion-cleanup)
	      (message "Sole completion"))
	     (t (search-backward shell-expand-string)
		(replace-match (car completions) t t))))
      (t ;; display the completions
       (let ((new-command (try-completion shell-expand-string complete-alist)))
	 (cond
	   ((and new-command (not (string= new-command shell-expand-string)))
	    (search-backward shell-expand-string)
	    (replace-match new-command t t)))
	 (if (not fi::shell-completions-window)
	     (setq fi::shell-completions-window
	       (current-window-configuration)))
	 (with-output-to-temp-buffer " *Completions*"
	   (display-completion-list
	    (sort completions 'string-lessp))))))))

(defun fi::executable-files (files dir)
  (cond
    (files
     (let (res file)
       (while files
	 (setq file (concat dir "/" (car files)))
	 (if (and (not (file-directory-p file))
		  (not (zerop (logand 73 (file-modes file)))))
	     (setq res (cons (car files) res)))
	 (setq files (cdr files)))
       res))))

(defun fi::shell-completion-default-prefix ()
  (re-search-backward fi:shell-token-pattern nil t)
  (forward-char)
  (buffer-substring
   (point)
   (progn
     (if (re-search-forward fi:shell-token-pattern nil 0) (backward-char))
     (point))))

(defun fi::shell-completion-cleanup ()
  "Clean up the window used for name completion after shell file name or
command completion."
  (interactive)
  (if fi::shell-completions-window
      (save-excursion
 	(set-window-configuration fi::shell-completions-window)
 	(setq fi::shell-completions-window nil))))
