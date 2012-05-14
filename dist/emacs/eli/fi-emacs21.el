;; gnu emacs v21 specific hacks for the Franz Inc. emacs-lisp interface
;;
;; Copyright (c) 1987-2003 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs specific stuff

(defun fi::switch-to-buffer-new-screen (buffer)
  (cond
   (fi:new-screen-for-common-lisp-buffer
    ;; There should be some parameters for the make-frame call.
    (let ((screen (make-frame)))
      (select-frame screen)
      ;; make sure the buffer is visible
      (fi::switch-to-buffer buffer)))
   (t (fi::switch-to-buffer buffer))))

(defun fi::ensure-buffer-visible (buffer)
  (let ((window (get-buffer-window buffer)))
    (when window
      (let ((frame (window-frame window)))
	(when frame (raise-frame frame))))))

(defun fi::ensure-minibuffer-visible ()
  (let ((frame (window-frame (minibuffer-window))))
    (when frame (raise-frame frame))))

(defun fi::defontify-string (str)
  (cond ((fboundp 'set-text-properties)
	 (set-text-properties 0 (length str) nil str)
	 str)
	(t (format "%s" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu bar stuff

(defvar fi::menu-bar-initialized nil)

(defun fi::keys (keys)
  (if fi:menu-bar-single-item
      (vconcat [menu-bar acl] keys)
    (vconcat [menu-bar] keys)))

(defun fi::menu (name &optional func &key enable)
  (let ((keys (where-is-internal func (current-local-map) t nil)))
    (append (list 'menu-item name func
		  :keys (when keys (key-description keys)))
	    (when enable (list :enable enable)))))

(defun fi::reinitialize-menu-bar-map ()
  (interactive)
  (setq fi::menu-bar-initialized nil)
  (fi::initialize-menu-bar-map))

(defun fi::initialize-menu-bar-map ()
  (when (not (memq major-mode fi::menu-bar-initialized))
    (let* ((composer (not (on-ms-windows)))
	   (map (cond
		 ((eq major-mode 'fi:common-lisp-mode) fi:common-lisp-mode-map)
		 ((eq major-mode 'fi:inferior-common-lisp-mode)
		  fi:inferior-common-lisp-mode-map)
		 ((eq major-mode 'fi:lisp-listener-mode)
		  fi:lisp-listener-mode-map)
		 (t (error "can't add these menus in this mode: %s"
			   major-mode))))
	   (acl-name "ACL")
	   (acl-map (make-keymap acl-name))
	   (help-name (if fi:menu-bar-single-item "Help" "ACLHelp"))
	   (help-map (make-sparse-keymap help-name))
	   (composer-name "Composer")
	   (composer-map (when composer (make-sparse-keymap composer-name)))
	   (debug-name (if fi:menu-bar-single-item "Debug" "ACLDebug"))
	   (debug-map (make-sparse-keymap debug-name))
	   (edit-name (if fi:menu-bar-single-item "Edit" "ACLEdit"))
	   (edit-map (make-sparse-keymap edit-name))
	   (file-name (if fi:menu-bar-single-item "File" "ACLFile"))
	   (file-map (make-sparse-keymap file-name)))
      (cond
       (fi:menu-bar-single-item
	(define-key map [menu-bar acl] (cons acl-name acl-map))
	(when composer
	  (define-key map [menu-bar acl composer]
	    (cons composer-name composer-map)))
	(define-key map [menu-bar acl acldebug] (cons debug-name debug-map))
	(define-key map [menu-bar acl acledit] (cons edit-name edit-map))
	(define-key map [menu-bar acl aclfile] (cons file-name file-map))
	
	(define-key map [menu-bar acl listener-new-frame]
	  (fi::menu "Create New Listener, new frame"
		    'fi:menu-open-lisp-listener-new-frame
		    :enable '(fi::connection-open)))

	(define-key map [menu-bar acl listener]
	  (fi::menu "Create New Listener"
		    'fi:menu-open-lisp-listener
		    :enable '(fi::connection-open)))

	(define-key map [menu-bar acl run-new-frame]
	  (fi::menu "Run/Restart Common Lisp, new frame"
		    'fi:menu-common-lisp-new-frame
		    ;; Per rfe6058:
		    ;; :enable '(fi::connection-not-open)
		    ))
    
	(define-key map [menu-bar acl run]
	  (fi::menu "Run/Restart Common Lisp" 'fi:menu-common-lisp
		    ;; Per rfe6058:
		    ;; :enable '(fi::connection-not-open)
		    )))
       (t
	(define-key map [menu-bar aclhelp] (cons help-name help-map))
	(when composer
	  (define-key map [menu-bar composer]
	    (cons composer-name composer-map)))
	(define-key map [menu-bar acldebug] (cons debug-name debug-map))
	(define-key map [menu-bar acledit] (cons edit-name edit-map))
	(define-key map [menu-bar aclfile] (cons file-name file-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE:
;;;; Blech.  Have to build the menus in reverse order, since
;;;; define-key-after doesn't work with [menu-bar ...].
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ACLFile menu:
    
      (define-key map (fi::keys [aclfile exit])
	(fi::menu "Exit Allegro CL" 'fi:exit-lisp
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [aclfile aclfile-sep3])
	(fi::menu "----"))
    
      (define-key map (fi::keys [aclfile list-buffer-definitions])
	(fi::menu "List buffer definitions"
		  'fi:list-buffer-definitions :enable '(fi::source-buffer-p)))

      (define-key map (fi::keys [aclfile compile-changed-definitions])
	(cons "Changed definitions"
	      (make-sparse-keymap "Changed definitions")))
    
      (define-key map (fi::keys [aclfile aclfile-sep2])
	(fi::menu "----"))

      (define-key map (fi::keys [aclfile load-file])
	(fi::menu "Load file" 'fi:load-file :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile compile-file])
	(fi::menu "Compile file" 'fi:compile-file
		  :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile compile-and-load])
	(fi::menu "Compile and load file" 'fi:compile-and-load-file
		  :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile aclfile-sep1])
	(fi::menu "----"))

      (define-key map (fi::keys [aclfile compile-other])
	(cons "Compile other" (make-sparse-keymap "Compile other")))
    
      (define-key map (fi::keys [aclfile compile-form])
	(fi::menu "Compile form" 'fi:lisp-compile-active-region-or-defun
		  :enable '(fi::acl-buffer-p)))
      
      (when (null fi:menu-bar-single-item)
	(define-key map (fi::keys [aclfile aclfile-sep1])
	  (fi::menu "----"))

	(define-key map (fi::keys [aclfile listener-new-frame])
	  (fi::menu "Create New Listener, new frame"
		    'fi:menu-open-lisp-listener-new-frame
		    :enable '(fi::connection-open)))

	(define-key map (fi::keys [aclfile listener])
	  (fi::menu "Create New Listener"
		    'fi:menu-open-lisp-listener
		    :enable '(fi::connection-open)))

	(define-key map (fi::keys [aclfile run-new-frame])
	  (fi::menu "Run/Restart Common Lisp, new frame"
		    'fi:menu-common-lisp-new-frame
		    :enable '(fi::connection-not-open)))
    
	(define-key map (fi::keys [aclfile run])
	  (fi::menu "Run/Restart Common Lisp" 'fi:menu-common-lisp
		    :enable '(fi::connection-not-open))))

;;;; ACLFile > Compile other menu:

      (define-key map (fi::keys [aclfile compile-other compile-buffer])
	(fi::menu "buffer" 'fi:lisp-compile-current-buffer
		  :enable '(fi::source-buffer-p)))

      (define-key map (fi::keys [aclfile compile-other compile-sexp])
	(fi::menu "s-exp before point" 'fi:lisp-compile-last-sexp
		  :enable '(fi::acl-buffer-p)))

      (define-key map (fi::keys [aclfile compile-other compile-region])
	(fi::menu "region" 'fi:lisp-compile-region
		  :enable '(fi::acl-buffer-p)))

;;;; ACLFile > Changed definitions menu:

      (define-key map (fi::keys [aclfile compile-changed-definitions compare])
	(fi::menu "Compare source files" 'fi:compare-source-files
		  :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile compile-changed-definitions
					 copy-buffer])
	(fi::menu "Copy buffer changed definitions to kill ring"
		  'fi:copy-buffer-changed-definitions
		  :enable '(fi::source-buffer-p)))

      (define-key map (fi::keys [aclfile compile-changed-definitions copy])
	(fi::menu "Copy all changed definitions to kill ring"
		  'fi:copy-changed-definitions
		  :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile compile-changed-definitions
					 eval-buffer])
	(fi::menu "Eval buffer changed definitions"
		  'fi:eval-buffer-changed-definitions
		  :enable '(fi::source-buffer-p)))

      (define-key map (fi::keys [aclfile compile-changed-definitions eval])
	(fi::menu "Eval all changed definitions"
		  'fi:eval-changed-definitions :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile compile-changed-definitions
					 compile-buffer])
	(fi::menu "Compile buffer changed definitions"
		  'fi:compile-buffer-changed-definitions
		  :enable '(fi::source-buffer-p)))

      (define-key map (fi::keys [aclfile compile-changed-definitions compile])
	(fi::menu "Compile all changed definitions"
		  'fi:compile-changed-definitions
		  :enable '(fi::connection-open)))

      (define-key map (fi::keys [aclfile compile-changed-definitions
					 list-buffer])
	(fi::menu "List buffer changed definitions"
		  'fi:list-buffer-changed-definitions
		  :enable '(fi::source-buffer-p)))

      (define-key map (fi::keys [aclfile compile-changed-definitions list])
	(fi::menu "List all changed definitions"
		  'fi:list-changed-definitions :enable '(fi::connection-open)))
    
;;;; ACLEdit menu:
    
      (define-key map (fi::keys [acledit pop-input])
	(fi::menu "Previous input" 'fi:pop-input
		  :enable '(fi::subproc-buffer-p)))
      
      (define-key map (fi::keys [acledit push-input])
	(fi::menu "Next input" 'fi:push-input
		  :enable '(fi::subproc-buffer-p)))
      
      (define-key map (fi::keys [acledit backward-search-input])
	(fi::menu "Search input backward" 'fi:re-search-backward-input
		  :enable '(fi::subproc-buffer-p)))
      
      (define-key map (fi::keys [acledit forward-search-input])
	(fi::menu "Search input forward" 'fi:re-search-forward-input
		  :enable '(fi::subproc-buffer-p)))
      
      (define-key map (fi::keys [acledit list-input])
	(fi::menu "List input" 'fi:list-input-ring
		  :enable '(fi::subproc-buffer-p)))
      
      (define-key map (fi::keys [acledit show-output])
	(fi::menu "Show last command output" 'fi:subprocess-show-output
		  :enable '(fi::subproc-buffer-p)))

      (define-key map (fi::keys [acledit acledit-sep3]) (fi::menu "----"))
      
      (define-key map (fi::keys [acledit previous-tpl-form])
	(fi::menu "Previous Top-level form" 'fi:previous-top-level-form))

      (define-key map (fi::keys [acledit next-tpl-form])
	(fi::menu "Next Top-level form" 'fi:next-top-level-form))

      (define-key map (fi::keys [acledit center-defun])
	(fi::menu "Center defun" 'fi:center-defun))
      
      (define-key map (fi::keys [acledit acledit-sep2]) (fi::menu "----"))

      (define-key map (fi::keys [acledit extract-list])
	(fi::menu "Extract List" 'fi:extract-list))
      
      (define-key map (fi::keys [acledit log-change])
	(fi::menu "Log change" 'fi:log-functional-change
		  :enable '(fi::source-buffer-p)))
      
      (define-key map (fi::keys [acledit indent-sexp])
	(fi::menu "Indent S-Exp"
		  (if fi:lisp-do-indentation 'fi:indent-sexp 'indent-sexp)))
      
      (define-key map (fi::keys [acledit uncomment-region])
	(fi::menu "Uncomment region" 'fi:uncomment-region))
    
      (define-key map (fi::keys [acledit comment-region])
	(fi::menu "Comment region" 'fi:comment-region))
    
      (define-key map (fi::keys [acledit uncomment-form])
	(fi::menu "Uncomment form" 'fi:uncomment-form))
    
      (define-key map (fi::keys [acledit comment-form])
	(fi::menu "Comment form" 'fi:comment-form))
    
      (define-key map (fi::keys [acledit super-paren])
	(fi::menu "Close all parens" 'fi:super-paren))
    
      (define-key map (fi::keys [acledit complete-symbol])
	(fi::menu "Complete symbol" 'fi:lisp-complete-symbol
		  :enable '(fi::connection-open)))
      
      (define-key map (fi::keys [acledit acledit-sep1]) (fi::menu "----"))
      
      (define-key map (fi::keys [acledit pop-definition-mark])
	(fi::menu "Pop definition mark"
		  'fi:pop-definition-mark
		  :enable '(fi::connection-open)))                 

      (define-key map (fi::keys [acledit find-next-definition])
	(fi::menu "Find next definition" 'fi:lisp-find-next-definition
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acledit find-definition-other-window])
	(fi::menu "Find definition other window"
		  'fi:lisp-find-definition-other-window
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acledit find-definition])
	(fi::menu "Find definition"
		  'fi:lisp-find-definition :enable '(fi::connection-open)))
          
;;;; ACLDebug

      (define-key map (fi::keys [acldebug xref])
	(cons "Cross reference" (make-sparse-keymap "Cross reference")))
    
      (define-key map (fi::keys [acldebug edit-methods])
	(fi::menu "Edit generic function methods"
		  'fi:edit-generic-function-methods
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug list-methods])
	(fi::menu "List generic function methods"
		  'fi:list-generic-function-methods
		  :enable '(fi::connection-open)))

      (define-key map (fi::keys [acldebug acldebug-sep1])
	(fi::menu "----"))

      (define-key map (fi::keys [acldebug list-unused-functions])
	(fi::menu "List unused functions" 'fi:list-unused-functions
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug list-undefined-functions])
	(fi::menu "List undefined functions" 'fi:list-undefined-functions
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug recursive-macroexpand])
	(fi::menu "Recursive macroexpand" 'fi:lisp-macroexpand-recursively
		  :enable '(fi::acl-buffer-p)))
    
      (define-key map (fi::keys [acldebug macroexpand])
	(fi::menu "Macroexpand" 'fi:lisp-macroexpand
		  :enable '(fi::acl-buffer-p)))
    
      (define-key map (fi::keys [acldebug debug-process])
	(fi::menu "Debug process" 'fi:scan-stack
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug trace-definer])
	(fi::menu "Trace definer" 'fi:trace-definer
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug toggle-trace])
	(fi::menu "Toggle trace" 'fi:toggle-trace-definition
		  :enable '(fi::connection-open)))
      
;;;; ACLDebug > Cross Reference menu:
    
      (define-key map (fi::keys [acldebug xref edit-who-is-called-by])
	(fi::menu "Edit callers of" 'fi:edit-who-is-called-by
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug xref edit-who-calls])
	(fi::menu "Edit calls to" 'fi:edit-who-calls
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug xref list-who-is-called-by])
	(fi::menu "List callers of" 'fi:list-who-is-called-by
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [acldebug xref list-who-calls])
	(fi::menu "List calls to" 'fi:list-who-calls
		  :enable '(fi::connection-open)))

;;;; Composer

      (when composer
	(define-key map (fi::keys [composer exit-composer])
	  (fi::menu "Exit Composer/Common Windows" 'fi:composer-exit
		    :enable '(fi::composer-connection-open-uncache)))
    
	(define-key map (fi::keys [composer composer-sep1])
	  (fi::menu "----"))
    
	(define-key map (fi::keys [composer help])
	  (cons "Help" (make-sparse-keymap "Help")))
    
	(define-key map (fi::keys [composer other])
	  (cons "Other" (make-sparse-keymap "Other")))
    
	(define-key map (fi::keys [composer composer-other-options])
	  (fi::menu "Options" 'fi:composer-other-options
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer profiler])
	  (cons "Profiler" (make-sparse-keymap "Profiler")))
    
	(define-key map (fi::keys [composer xref])
	  (cons "Xref" (make-sparse-keymap "Xref")))
    
	(define-key map (fi::keys [composer clos])
	  (cons "CLOS" (make-sparse-keymap "CLOS")))
    
	(define-key map (fi::keys [composer inspect])
	  (fi::menu "Inspect" 'fi:inspect-value
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer composer-sep2])
	  (fi::menu "----"))
    
	(define-key map (fi::keys [composer start-composer])
	  (fi::menu "Start Composer" 'fi:start-composer
		    :enable
		    '(fi::connection-open-composer-loaded-and-stopped)))

;;;; Composer > Help
    
	(define-key map (fi::keys [composer help
					    composer-help-gesture-bindings])
	  (fi::menu "Current pointer gesture bindings"
		    'fi:composer-help-gesture-bindings
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer help composer-help])
	  (fi::menu "Help" 'fi:composer-help
		    :enable '(fi::composer-connection-open)))

;;;; Composer > Other
    
	(define-key map (fi::keys [composer other reinitialize-resources])
	  (fi::menu "Reinitialize Composer resources"
		    'fi:composer-reinitialize-resources
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer other defsys-browser])
	  (fi::menu "System Browser" 'fi:composer-defsys-browser
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer other process-browser])
	  (fi::menu "Process Browser" 'fi:composer-process-browser
		    :enable '(fi::composer-connection-open)))
    
;;;; Composer > Profiler
    
	(define-key map (fi::keys [composer profiler options])
	  (fi::menu "Options" 'fi:composer-profiler-options
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer profiler display-space])
	  (fi::menu "Display space" 'fi:composer-display-space-profiler
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer profiler display-time])
	  (fi::menu "Display time" 'fi:composer-display-time-profiler
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer profiler stop-profiler])
	  (fi::menu "Stop profiler" 'fi:composer-stop-profiler
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer profiler start-space-profiler])
	  (fi::menu "Start space profiler" 'fi:composer-start-space-profiler
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer profiler start-time-profiler])
	  (fi::menu "Start time profiler" 'fi:composer-start-time-profiler
		    :enable '(fi::composer-connection-open)))

;;;; Composer > Xref
    
	(define-key map (fi::keys [composer xref discard])
	  (fi::menu "Discard Xref info" 'fi:discard-xref-info
		    :enable '(fi::connection-open)))
    
	(define-key map (fi::keys [composer xref composer-ref-sep1])
	  (fi::menu "----"))
    
	(define-key map (fi::keys [composer xref show-calls-to-and-from])
	  (fi::menu "Show calls to and from" 'fi:show-calls-to-and-from
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer xref show-calls-from])
	  (fi::menu "Show calls from" 'fi:show-calls-from
		    :enable '(fi::composer-connection-open)))

	(define-key map (fi::keys [composer xref show-calls-to])
	  (fi::menu "Show calls to" 'fi:show-calls-to
		    :enable '(fi::composer-connection-open)))

;;;; Composer > CLOS
    
	(define-key map (fi::keys [composer clos show-superclasses])
	  (fi::menu "Show class superclasses" 'fi:show-superclasses
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer clos show-subclasses])
	  (fi::menu "Show class subclasses" 'fi:show-subclasses
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer clos inspect-function])
	  (fi::menu "Inspect generic function" 'fi:inspect-function
		    :enable '(fi::composer-connection-open)))
    
	(define-key map (fi::keys [composer clos inspect-class])
	  (fi::menu "Inspect class" 'fi:inspect-class
		    :enable '(fi::composer-connection-open))))
    
;;;; ACLHelp

      (define-key map (fi::keys [aclhelp function-documentation])
	(fi::menu "Function Documentation" 'fi:lisp-function-documentation
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [aclhelp apropos])
	(fi::menu "Apropos" 'fi:lisp-apropos :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [aclhelp describe])
	(fi::menu "Describe" 'fi:describe-symbol
		  :enable '(fi::connection-open)))
    
      (define-key map (fi::keys [aclhelp arglist])
	(fi::menu "Arglist" 'fi:lisp-arglist :enable '(fi::connection-open)))
    
      (push major-mode fi::menu-bar-initialized))))

(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::initialize-menu-bar-map)
(add-hook 'fi:common-lisp-mode-hook 'fi::initialize-menu-bar-map)
(add-hook 'fi:lisp-listener-mode-hook 'fi::initialize-menu-bar-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi:menu-common-lisp ()
  (interactive)
  (let ((fi:new-screen-for-common-lisp-buffer nil))
    (call-interactively 'fi:common-lisp)))

(defun fi:menu-common-lisp-new-frame ()
  (interactive)
  (let ((get-screen-for-buffer-default-screen-name 'common-lisp)
	(fi:new-screen-for-common-lisp-buffer t))
    (call-interactively 'fi:common-lisp)))

(defun fi:menu-open-lisp-listener ()
  (interactive)
  (fi:open-lisp-listener -1))

(defun fi:menu-open-lisp-listener-new-frame ()
  (interactive)
  (let ((get-screen-for-buffer-default-screen-name 'lisp-listener)
	(fi:new-screen-for-common-lisp-buffer t))
    (fi:open-lisp-listener -1)))

(defun fi::region-active ()
  (and transient-mark-mode mark-active))

(defun fi:lisp-compile-active-region-or-defun ()
  (interactive)
  (if (fi::region-active)
      (fi:lisp-compile-region)
    (fi:lisp-compile-defun)))

(defun fi:comment-form (&optional uncomment)
  (interactive)
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (fi:beginning-of-defun)
		  (point))))
    (fi:comment-region start end uncomment)))

(defun fi:uncomment-form ()
  (interactive)
  (fi:comment-form t))

(defun fi:exit-lisp ()
  (interactive)
  (message "Exiting Allegro CL...")
  (fi:eval-in-lisp
   "(mp:process-interrupt
      (mp:process-name-to-process \"Initial Lisp Listener\")
      #'excl:exit 0)")
  (let ((cl-buffer (get-buffer fi:common-lisp-buffer-name)))
    (when (and cl-buffer (get-buffer-window cl-buffer))
      (set-buffer cl-buffer)
      (bury-buffer)))
  (message "Exiting Allegro CL...done."))

(defun fi:start-composer ()
  (interactive)
  (message "Starting Allegro Composer...")
  (fi:eval-in-lisp "(progn(wt::start-composer :mouse-line nil)nil)")
  (message "Starting Allegro Composer...done.")
  (setq fi::composer-running 'yes)
  (setq fi::composer-connection-open 'yes))

(defun fi:discard-xref-info ()
  (interactive)
  (message "Discarding cross reference info...")
  (fi:eval-in-lisp "(progn(xref:discard-all-xref-info)nil)")
  (message "Discarding cross reference info...done."))

(defun fi:composer-other-options ()
  (interactive)
  (message "Creating Composer options dialog...")
  (fi:eval-in-lisp
   "wt:(progn
	(unless
          (and(composer-initialized-p)*mouse-line*)(start-mouse-line-process))
	(do()((and(composer-initialized-p)*mouse-line*)) (sleep 2))
	(run-motif-application 'make-main-options)nil)")
  (message "Creating Composer options dialog...done."))

(defun fi:composer-help ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::print-startup-help)nil)"))

(defun fi:composer-help-gesture-bindings ()
  (interactive)
  (fi:eval-in-lisp "(progn(wt::composer-report-gestures-command t)nil)"))

(defun fi:composer-exit ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer:stop-composer :kill-cw t)nil)")
  (setq fi::composer-running 'no)
  (setq fi::composer-connection-open 'no))

(defun fi:composer-process-browser ()
  (interactive)
  (message "Starting process browser...")
  (fi:eval-in-lisp "(progn(composer::process-browser)nil)")
  (message "Starting process browser...done."))

(defun fi:composer-reinitialize-resources ()
  (interactive)
  (message "Reinitializing resources...")
  (fi:eval-in-lisp "(progn(composer::init-resource-database)nil)")
  (message "Reinitializing resources...done."))

(defun fi:composer-defsys-browser ()
  (interactive)
  (message "Starting defsystem browser...")
  (fi:eval-in-lisp "(progn(composer::defsys-browser)nil)")
  (message "Starting defsystem browser...done."))

(defun fi:composer-start-time-profiler ()
  (interactive)
  (message "Starting time profiler...")
  (fi:eval-in-lisp "(progn(composer::start-profiler-command-1 :time)nil)")
  (message "Starting time profiler...done."))

(defun fi:composer-start-space-profiler ()
  (interactive)
  (message "Starting space profiler...")
  (fi:eval-in-lisp "(progn(composer::start-profiler-command-1 :space)nil)")
  (message "Starting space profiler...done."))

(defun fi:composer-display-time-profiler ()
  (interactive)
  (message "Displaying time profile graph...")
  (fi:eval-in-lisp "(progn(composer::display-profile-command-1 :time)nil)")
  (message "Displaying time profile graph...done."))

(defun fi:composer-display-space-profiler ()
  (interactive)
  (message "Displaying space profile graph...")
  (fi:eval-in-lisp "(progn(composer::display-profile-command-1 :space)nil)")
  (message "Displaying space profile graph...done."))

(defun fi:composer-stop-profiler ()
  (interactive)
  (message "Stopping the profiler...")
  (fi:eval-in-lisp "(progn(composer::stop-profiler-command-1)nil)")
  (message "Stopping the profiler...done."))

(defun fi:composer-profiler-options ()
  (interactive)
  (message "Creating profiler options dialog...")
  (fi:eval-in-lisp
   "wt:(progn
	(unless
         (and(composer-initialized-p)*mouse-line*)(start-mouse-line-process))
	(do()((and(composer-initialized-p)*mouse-line*)) (sleep 2))
	(run-motif-application 'make-profiler-options)nil)")
  (message "Creating profiler options dialog...done."))

(defun fi::source-buffer-p ()
  (and (fi::connection-open)
       (eq major-mode 'fi:common-lisp-mode)))

(defun fi::subproc-buffer-p ()
  (and (fi::connection-open)
       (member major-mode '(fi:inferior-common-lisp-mode
			    fi:lisp-listener-mode))))

(defun fi::acl-buffer-p ()
  (and (fi::connection-open)
       (member major-mode '(fi:common-lisp-mode
			    fi:inferior-common-lisp-mode
			    fi:lisp-listener-mode))))

(defun fi::connection-open ()
  (fi::lep-open-connection-p))

(defun fi::connection-not-open ()
  (not (fi::lep-open-connection-p)))

(defvar fi::connection-open-composer-loaded nil)

(defvar fi::connection-open-composer-loaded-cached nil)

(defvar fi::composer-connection-open nil)
(defvar fi::composer-running nil)

(defvar fi::composer-cached-connection nil)

(defun fi::connection-open-composer-loaded ()
  (when (not (eq fi::*connection* fi::composer-cached-connection))
    ;; the lisp was (possibly) restarted
    (setq fi::connection-open-composer-loaded nil)
    (setq fi::composer-running nil))
  (and (fi::lep-open-connection-p)
       (or (when (or (null fi::connection-open-composer-loaded)
		     ;; check again, it might have been require'd
		     (eq fi::connection-open-composer-loaded 'no))
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp "(when (find-package :wt) t)"))
		 (setq fi::connection-open-composer-loaded 'yes)
	       (setq fi::connection-open-composer-loaded 'no))
	     (setq fi::composer-cached-connection fi::*connection*)
	     nil)
	   (eq fi::connection-open-composer-loaded 'yes))))

(defun fi::connection-open-composer-loaded-cached ()
  (if fi::connection-open-composer-loaded-cached
      (eq fi::connection-open-composer-loaded-cached 'yes)
    (prog1 (fi::connection-open-composer-loaded)
      (setq fi::connection-open-composer-loaded-cached
	fi::connection-open-composer-loaded))))

(defun fi::connection-open-composer-loaded-and-stopped ()
  (and (fi::connection-open-composer-loaded-cached)
       (or (unless fi::composer-running
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp
		    "(when (and (find-package :wt)
                                (wt::common-windows-initialized-p)
				(wt::connected-to-server-p))
                       t)"))
		 (setq fi::composer-running 'yes)
	       (setq fi::composer-running 'no))
	     nil)
	   (eq fi::composer-running 'no))))

(defun fi::composer-connection-open ()
  (and (fi::connection-open-composer-loaded-cached)
       (or (unless fi::composer-connection-open
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp
		    "wt::(and ;;(connected-to-epoch-p)
 			      (common-windows-initialized-p)
			      (connected-to-server-p))"))
		 (setq fi::composer-connection-open 'yes)
	       (setq fi::composer-connection-open 'no))
	     nil)
	   (eq fi::composer-connection-open 'yes))))

(defun fi::composer-connection-open-uncache ()
  (prog1 
      (and (fi::connection-open-composer-loaded-cached)
	   (or (unless fi::composer-connection-open
		 (if (let ((fi:package nil))
		       (fi:eval-in-lisp
			"wt::(and ;;(connected-to-epoch-p)
 			      (common-windows-initialized-p)
			      (connected-to-server-p))"))
		     (setq fi::composer-connection-open 'yes)
		   (setq fi::composer-connection-open 'no))
		 nil)
	       (eq fi::composer-connection-open 'yes)))
    (setq fi::connection-open-composer-loaded-cached nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi::popup-menu-initialized nil)

(defun fi::initialize-popup-menu-map ()
  (when (not (memq major-mode fi::popup-menu-initialized))
    (cond
     ((eq major-mode 'fi:common-lisp-mode)
      (fi::add-common-lisp-popup-menu-items (current-local-map)))
     ((eq major-mode 'fi:inferior-common-lisp-mode)
      (fi::add-shell-popup-menu-items "Inferior CL popup menu"
				      (current-local-map))
      (fi::add-inferior-cl-popup-menu-items (lookup-key (current-local-map)
							[down-mouse-3])))
     ((eq major-mode 'fi:lisp-listener-mode)
      (fi::add-shell-popup-menu-items "Inferior CL popup menu"
				      (current-local-map))
      (fi::add-inferior-cl-popup-menu-items (lookup-key (current-local-map)
							[down-mouse-3])))
     ((memq major-mode '(fi:shell-mode fi:rlogin-mode fi:su-mode
			 fi:remote-su-mode fi:telnet-mode))
      (fi::add-shell-popup-menu-items "Subprocess popup menu"
				      (current-local-map))))
    
    (push major-mode fi::popup-menu-initialized)))

(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::initialize-popup-menu-map)
(add-hook 'fi:common-lisp-mode-hook 'fi::initialize-popup-menu-map)
(add-hook 'fi:lisp-listener-mode-hook 'fi::initialize-popup-menu-map)
(add-hook 'fi:shell-mode-hook 'fi::initialize-popup-menu-map)
(add-hook 'fi:rlogin-mode-hook 'fi::initialize-popup-menu-map)
(add-hook 'fi:su-mode-hook 'fi::initialize-popup-menu-map)
(add-hook 'fi:telnet-mode-hook 'fi::initialize-popup-menu-map)

(defun fi::add-shell-popup-menu-items (name main-map)
  (let* ((map (make-sparse-keymap name)))
    (define-key main-map [down-mouse-3] (cons name map))
  
    (define-key map [sep1] (fi::menu "----"))
    
    (define-key map [garbage-filter]
      (fi::menu "Start ^M garbage filter" 'fi:telnet-start-garbage-filter))
    
    (define-key map [show-output]
	(fi::menu "Show last command output" 'fi:subprocess-show-output))
    (define-key map [list-input] (fi::menu "List input" 'fi:list-input-ring))
    (define-key map [search-input-forward]
      (fi::menu "Search input forward" 'fi:re-search-forward-input))
    (define-key map [search-input]
      (fi::menu "Search input backward" 'fi:re-search-backward-input))
    (define-key map [push-input] (fi::menu "Next input" 'fi:push-input))
    (define-key map [pop-input] (fi::menu "Previous input" 'fi:pop-input))
;;;;;;....    
    ))

(defun fi::add-common-lisp-popup-menu-items (main-map)
  (let* ((name "CL popup menu")
	 (map (make-sparse-keymap name)))
    (define-key main-map [down-mouse-3] (cons name map))
  
    (define-key map [macroexpand-recursively]
      (fi::menu "Macroexpand recursively" 'fi:lisp-macroexpand-recursively
		:enable '(fi::connection-open)))
  
    (define-key map [macroexpand]
      (fi::menu "Macroexpand" 'fi:lisp-macroexpand
		:enable '(fi::connection-open)))
  
    (define-key map [toggle-trace]
      (fi::menu "Toggle trace" 'fi:menu-toggle-trace-definition
		:enable '(fi::connection-open)))
  
    (define-key map [arglist]
      (fi::menu "Arglist" 'fi:menu-lisp-arglist
		:enable '(fi::connection-open)))
  
    (define-key map [find-next-definition]
      (fi::menu "Find next definition" 'fi:lisp-find-next-definition
		:enable '(fi::connection-open)))
  
    (define-key map [lisp-find-definition]
      (fi::menu "Find definition" 'fi:menu-lisp-find-definition
		:enable '(fi::connection-open)))
  
    (define-key map [sep1] (fi::menu "----"))
  
    (define-key map [compile-file]
      (fi::menu "Compile file" 'fi:menu-compile-file
		:enable '(fi::source-buffer-p)))
  
    (define-key map [compile-and-load-file]
      (fi::menu "Compile and load file" 'fi:menu-compile-and-load-file
		:enable '(fi::source-buffer-p)))
  
    (define-key map [compile-region]
      (fi::menu "Compile region" 'fi:lisp-compile-region
		:enable '(fi::acl-buffer-p)))
  
    (define-key map [compile-active-region-or-defun]
      (fi::menu "Compile form" 'fi:lisp-compile-active-region-or-defun
		:enable '(fi::acl-buffer-p)))))

(defun fi::add-inferior-cl-popup-menu-items (map)
  (define-key map [list-processes]
    (fi::menu "List processes" 'fi:debug-menu-processes
	      :enable '(fi::connection-open)))

  (define-key map [sep1] (fi::menu "----"))
    
  (define-key map [reset]
    (fi::menu "Reset" 'fi:debug-menu-reset :enable '(fi::connection-open)))
    
  (define-key map [pop]
    (fi::menu "Pop" 'fi:debug-menu-pop :enable '(fi::connection-open)))
    
  (define-key map [restart]
    (fi::menu "Restart" 'fi:debug-menu-restart
	      :enable '(fi::connection-open)))
    
  (define-key map [continue]
    (fi::menu "Continue" 'fi:debug-menu-continue
	      :enable '(fi::connection-open)))
    
  (define-key map [sep2] (fi::menu "----"))
    
  (define-key map [locals]
    (fi::menu "Locals for frame" 'fi:debug-menu-locals
	      :enable '(fi::connection-open)))
    
  (define-key map [edit-frame]
    (fi::menu "Edit frame" 'fi:debug-menu-edit-frame
	      :enable '(fi::connection-open)))
    
  (define-key map [up-frame]
    (fi::menu "Up frame" 'fi:debug-menu-up-frame
	      :enable '(fi::connection-open)))
    
  (define-key map [down-frame]
    (fi::menu "Down frame" 'fi:debug-menu-down-frame
	      :enable '(fi::connection-open)))
    
  (define-key map [zoom]
    (fi::menu "Zoom" 'fi:debug-menu-zoom
	      :enable '(fi::connection-open))))

(defun fi:menu-lisp-find-definition ()
  (interactive)
  (let ((fi::use-symbol-at-point t))
    (call-interactively 'fi:lisp-find-definition)))

(defun fi:menu-lisp-arglist ()
  (interactive)
  (let ((fi::use-symbol-at-point t))
    (call-interactively 'fi:lisp-arglist)))

(defun fi:menu-toggle-trace-definition ()
  (interactive)
  (let ((fi::use-symbol-at-point t))
    (call-interactively 'fi:toggle-trace-definition)))

(defun fi:menu-compile-and-load-file ()
  (interactive)
  (when (buffer-file-name)
    (fi:compile-and-load-file (buffer-file-name))))

(defun fi:menu-compile-file ()
  (interactive)
  (when (buffer-file-name)
    (fi:compile-file (buffer-file-name))))

(defun fi:menu-load-file ()
  (interactive)
  (when (buffer-file-name)
    (fi:load-file (buffer-file-name))))

(defun fi:debug-menu-reset ()
  (interactive)
  (insert ":reset")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-pop ()
  (interactive)
  (insert ":pop")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-restart ()
  (interactive)
  (insert ":restart")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-continue ()
  (interactive)
  (insert ":continue")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-zoom ()
  (interactive)
  (insert ":zoom")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-down-frame ()
  (interactive)
  (insert ":dn")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-up-frame ()
  (interactive)
  (insert ":up")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-edit-frame ()
  (interactive)
  (insert ":edit")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-locals ()
  (interactive)
  (insert ":local")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-processes ()
  (interactive)
  (insert ":processes")
  (fi:inferior-lisp-newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock

(require 'font-lock)

(defvar fi:lisp-font-lock-keywords lisp-font-lock-keywords-2)

(defun fi::turn-on-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
    '(fi:lisp-font-lock-keywords	; KEYWORDS
      nil				; KEYWORDS-ONLY
      nil				; CASE-FOLD
      (("+-*/.<>=!?$%_&~^:" . "w"))	; SYNTAX-ALIST
      beginning-of-defun		; SYNTAX-BEGIN
      (font-lock-mark-block-function . mark-defun)))
  (turn-on-font-lock))

(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::turn-on-font-lock)
(add-hook 'fi:common-lisp-mode-hook 'fi::turn-on-font-lock)
(add-hook 'fi:emacs-lisp-mode-hook 'fi::turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resize support

;; rfe5597, from smh

;; This still completely punts deciding what to do with multiple
;; frames.  We could associate the initial *common-lisp* buffer with
;; the frame that is selected when the buffer is created, and then
;; only respond to changes in that frame.  Of course, there are still
;; ways this could fail, such as if the initial frame were
;; subsequently closed...

(defun fi::window-config-changed ()
  (setq fi::*new-frame-width* (cons (1- (window-width)) (selected-frame))))

(add-hook 'window-configuration-change-hook 'fi::window-config-changed)

(defun fi::maybe-update-default-right-margin ()
  (ignore-errors
   (when (and fi::*new-frame-width* fi::*current-frame-width*)
     (let ((new-width (car fi::*new-frame-width*))
	   (frame (cdr fi::*new-frame-width*)))
       (setq fi::*new-frame-width* nil)
       (when (and (/= fi::*current-frame-width* new-width)
		  (fi::lep-open-connection-p))
	 (fi:eval-in-lisp "(setq excl::*default-right-margin* %d)"
			  (setq fi::*current-frame-width* new-width)))))))
