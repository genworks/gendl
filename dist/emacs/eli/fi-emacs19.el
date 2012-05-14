;; gnu emacs v19 specific hacks for the Franz Inc. emacs-lisp interface
;;
;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

(cond
 ((and (eq fi::emacs-type 'emacs19)
       (boundp 'emacs-minor-version)
       (<= emacs-minor-version 22))
  (require 'menubar "lmenu"))
 ((eq fi::emacs-type 'emacs18))
 (t (require 'lmenu "lmenu")))

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

(defun fi::source-buffer-p ()
  (and (fi::connection-open)
       (eq major-mode 'fi:common-lisp-mode)))

(defun fi::acl-buffer-p ()
  (and (fi::connection-open)
       (member major-mode '(fi:common-lisp-mode
			    fi:inferior-common-lisp-mode
			    fi:lisp-listener-mode))))

(defconst fi:allegro-file-menu
    '("ACLFile"
      ["Run/Restart Common Lisp" fi:menu-common-lisp (fi::connection-not-open)]
      ["Run/Restart Common Lisp, new frame" fi:menu-common-lisp-new-screen
       (fi::connection-not-open)]
      ["Create New Listener" fi:menu-open-lisp-listener (fi::connection-open)]
      ["Create New Listener, new frame" fi:menu-open-lisp-listener-new-screen
       (fi::connection-open)]
      "----"
      ;; Unfortunately, the region-or-form versions only work reasonably if the user is
      ;; known to use zmacs-style regions, and we can't customize the menus for that.
      ;; But a user who always runs in transient-mark-mode might want to customize this.
      ;;["Compile region or form" fi:lisp-compile-active-region-or-defun (fi::acl-buffer-p)]
      ["Compile form" fi:lisp-compile-active-region-or-defun (fi::acl-buffer-p)]
      ("Compile other"
       ["region" fi:lisp-compile-region (fi::acl-buffer-p)]
       ["s-exp before point" fi:lisp-compile-last-sexp (fi::acl-buffer-p)]
       ["buffer" fi:lisp-compile-current-buffer (fi::source-buffer-p)])
      "----"
      ["Compile and load file" fi:compile-and-load-file (fi::connection-open)]
      ["Compile file" fi:compile-file (fi::connection-open)]
      ["Load file" fi:load-file (fi::connection-open)]
      "----"
      ("Changed definitions"
       ["List all changed definitions" fi:list-changed-definitions (fi::connection-open)]
       ["List buffer changed definitions" fi:list-buffer-changed-definitions
	(fi::source-buffer-p)]
       ["Compile all changed definitions" fi:compile-changed-definitions (fi::connection-open)]
       ["Compile buffer changed definitions" fi:compile-buffer-changed-definitions
	(fi::source-buffer-p)]
       ["Eval all changed definitions" fi:eval-changed-definitions (fi::connection-open)]
       ["Eval buffer changed definitions" fi:eval-buffer-changed-definitions
	(fi::source-buffer-p)]
       ["Copy all changed definitions to kill ring" fi:copy-changed-definitions
	(fi::connection-open)]
       ["Copy buffer changed definitions to kill ring" fi:copy-buffer-changed-definitions
	(fi::source-buffer-p)]
       ["Compare source files" fi:compare-source-files (fi::connection-open)]
       )
      ["List buffer definitions" fi:list-buffer-definitions (fi::source-buffer-p)]
      "----"
      ["Exit Allegro CL" fi:exit-lisp (fi::connection-open)]
      ))

(defconst fi:allegro-edit-menu
    '("ACLEdit"
      ["Find definition" fi:lisp-find-definition (fi::connection-open)]
      ["Find definition other window" fi:lisp-find-definition-other-window (fi::connection-open)]
      ["Find next definition" fi:lisp-find-next-definition (fi::connection-open)]
      "----"
      ["Center defun" fi:center-defun (fi::source-buffer-p)]
      ;; ["Extract list" fi:extract-list t]
      ["Close all parens" fi:super-paren (fi::acl-buffer-p)]
      ;; Unfortunately, the region-or-form versions only work reasonably if the user is
      ;; known to use zmacs-style regions, and we can't customize the menus for that.
      ;; But a user who always runs in transient-mark-mode might want to customize this.
      ;;["Comment region or form" fi:comment-region-or-form (fi::source-buffer-p)]
      ;;["Uncomment region or form" fi:uncomment-region-or-form (fi::source-buffer-p)]
      ["Comment form" fi:comment-form (fi::source-buffer-p)]
      ["Uncomment form" fi:uncomment-form (fi::source-buffer-p)]
      ["Comment region" fi:comment-region (fi::source-buffer-p)]
      ["Uncomment region" fi:uncomment-region (fi::source-buffer-p)]
      ))

(defconst fi:allegro-help-menu
    '("ACLHelp"
      ["Arglist" fi:lisp-arglist (fi::connection-open)]
      ["Describe" fi:describe-symbol (fi::connection-open)]
      ["Apropos" fi:lisp-apropos (fi::connection-open)]
      ["Function Documentation" fi:lisp-function-documentation (fi::connection-open)]
      ))

(defconst fi:allegro-debug-menu
    '("ACLDebug"
      ["Toggle trace" fi:toggle-trace-definition (fi::connection-open)]
      ["Debug process" fi:scan-stack (fi::connection-open)]
      ["Macroexpand" fi:lisp-macroexpand (fi::acl-buffer-p)]
      ["Recursive macroexpand" fi:lisp-macroexpand-recursively (fi::acl-buffer-p)]
      ["List undefined functions" fi:list-undefined-functions (fi::connection-open)]
      ["List unused functions" fi:list-unused-functions (fi::connection-open)]
      ;; ["Kill definition" fi:kill-definition (fi::connection-open)]
      "----"
      ["List generic function methods" fi:list-generic-function-methods (fi::connection-open)]
      ["Edit generic function methods" fi:edit-generic-function-methods (fi::connection-open)]
      ("Cross reference"
       ["List calls to" fi:list-who-calls (fi::connection-open)]
       ["List callers of" fi:list-who-is-called-by (fi::connection-open)]
       ["Edit calls to" fi:edit-who-calls (fi::connection-open)]
       ["Edit callers of" fi:edit-who-is-called-by (fi::connection-open)])
      ))

(defconst fi:composer-menu
    '("Composer"
      ["Start Composer" fi:start-composer (fi::connection-open-composer-loaded-and-stopped)]
;;;      ["Start Composer with Podium" fi:start-composer-mouse-line
;;;       (fi::connection-open-composer-loaded-and-stopped)]
      "----"
      ["Inspect" fi:inspect-value (fi::composer-connection-open)]
      ("CLOS"
       ["Inspect class" fi:inspect-class (fi::composer-connection-open)]
       ["Inspect generic function" fi:inspect-function (fi::composer-connection-open)]
       ["Show class subclasses" fi:show-subclasses (fi::composer-connection-open)]
       ["Show class superclasses" fi:show-superclasses (fi::composer-connection-open)]
       )
      ("Xref"
       ["Show calls to" fi:show-calls-to (fi::composer-connection-open)]
       ["Show calls from" fi:show-calls-from (fi::composer-connection-open)]
       ["Show calls to and from" fi:show-calls-to-and-from (fi::composer-connection-open)]
       "----"
       ["Discard Xref info" fi:discard-xref-info (fi::connection-open)]
       )
      ("Profiler"
       ["Start time profiler" fi:composer-start-time-profiler (fi::composer-connection-open)]
       ["Start space profiler" fi:composer-start-space-profiler (fi::composer-connection-open)]
       ["Stop profiler" fi:composer-stop-profiler (fi::composer-connection-open)]
       ["Display time" fi:composer-display-time-profiler (fi::composer-connection-open)]
       ["Display space" fi:composer-display-space-profiler (fi::composer-connection-open)]
       ["Options" fi:composer-profiler-options (fi::composer-connection-open)]
       )
      ["Options" fi:composer-other-options (fi::composer-connection-open)]
      ("Other"
;      ["Presenting Listener" composer::make-presenting-listener (fi::composer-connection-open)]
       ["Process Browser" fi:composer-process-browser (fi::composer-connection-open)]
       ["System Browser" fi:composer-defsys-browser (fi::composer-connection-open)]
       ["Reinitialize Composer resources" fi:composer-reinitialize-resources
	(fi::composer-connection-open)]
       )
      ("Help"
       ["Help" fi:composer-help (fi::composer-connection-open)]
       ["Current pointer gesture bindings" fi:composer-help-gesture-bindings
	(fi::composer-connection-open)])
      "----"
      ["Exit Composer/Common Windows" fi:composer-exit (fi::composer-connection-open-uncache)]
      ))

(defun fi::connection-open ()
  (fi::lep-open-connection-p))

(defun fi::connection-not-open ()
  (not (fi::lep-open-connection-p)))

(defun fi::connection-once-open ()
  (and (not (fi::lep-open-connection-p))
       (not fi::common-lisp-first-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		 (set-menubar-dirty-flag)
		 nil)
	       (eq fi::composer-connection-open 'yes)))
    (setq fi::connection-open-composer-loaded-cached nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-local-variable 'current-menubar)

(defun fi::set-buffer-menubar (menubar)
  (setq current-menubar menubar)
  ;; this slows things down a lot.  Why??????????
;;;;(set-menubar-dirty-flag)
  )

(defun fi::install-menubar (menu-bar)
  (add-menu nil (car menu-bar) (cdr menu-bar) "Help"))

(defvar fi::buffer-local-menubar nil)

(defun fi::install-buffer-local-emacs19-acl-menubar ()
  (when (eq current-menubar (default-value 'current-menubar))
    ;; current-menubar isn't yet buffer local
    (cond
     (fi::buffer-local-menubar
      (fi::set-buffer-menubar fi::buffer-local-menubar))
     (t
      (set-buffer-menubar current-menubar)
      (fi::install-menubar fi:allegro-file-menu)
      (fi::install-menubar fi:allegro-edit-menu)
      (fi::install-menubar fi:allegro-debug-menu)
      (fi::install-menubar fi:allegro-help-menu)
      (when (and fi:composer-menu (not (on-ms-windows)))
	(fi::install-menubar fi:composer-menu))
      (setq fi::buffer-local-menubar current-menubar)))))

(defvar fi::menubar-initialization)
(setq fi::menubar-initialization
  'fi::install-buffer-local-emacs19-acl-menubar)

(defun fi:menu-common-lisp ()
  (interactive)
  (let ((fi:new-screen-for-common-lisp-buffer nil))
    (call-interactively 'fi:common-lisp)))

(defun fi:menu-common-lisp-new-screen ()
  (interactive)
  (let ((get-screen-for-buffer-default-screen-name 'common-lisp)
	(fi:new-screen-for-common-lisp-buffer t))
    (call-interactively 'fi:common-lisp)))

(defun fi:menu-open-lisp-listener ()
  (interactive)
  (fi:open-lisp-listener -1))

(defun fi:menu-open-lisp-listener-new-screen ()
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

(defun fi:comment-region-or-form ()
  (interactive)
  (if (fi::region-active)
      (fi:comment-region (point) (mark))
    (let* ((end (save-excursion (end-of-defun) (point)))
	   (start (save-excursion
		    (fi:beginning-of-defun)
		    (point))))
      (fi:comment-region start end))))

(defun fi:uncomment-region-or-form ()
  (interactive)
  (if (fi::region-active)
      (fi:comment-region (point) (mark) t)
    (let* ((end (save-excursion (end-of-defun) (point)))
	   (start (save-excursion
		    (fi:beginning-of-defun)
		    (point))))
      (fi:comment-region start end t))))

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

(defun fi:start-composer-mouse-line ()
  (interactive)
  (message "Starting Allegro Composer...")
  (fi:eval-in-lisp "(progn(wt::start-composer :mouse-line t)nil)")
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
	(unless(and(composer-initialized-p)*mouse-line*)(start-mouse-line-process))
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
	(unless(and(composer-initialized-p)*mouse-line*)(start-mouse-line-process))
	(do()((and(composer-initialized-p)*mouse-line*)) (sleep 2))
	(run-motif-application 'make-profiler-options)nil)")
  (message "Creating profiler options dialog...done."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fi:common-lisp-mode-popup-menu
    '("common lisp mode popup menu"
      ;; Unfortunately, the region-or-form versions only work reasonably if the user is
      ;; known to use zmacs-style regions, and we can't customize the menus for that.
      ;; But a user who always runs in transient-mark-mode might want to customize this.
      ;;["Compile region or form" fi:lisp-compile-active-region-or-defun (fi::connection-open)]
      ["Compile form" fi:lisp-compile-active-region-or-defun (fi::acl-buffer-p)]
      ["Compile region" fi:lisp-compile-region (fi::acl-buffer-p)]
      ["Compile and load file" fi:menu-compile-and-load-file (fi::source-buffer-p)]
      ["Compile file" fi:menu-compile-file (fi::source-buffer-p)]
      "----"
      ["Find definition" fi:menu-lisp-find-definition (fi::connection-open)]
      ["Find next definition" fi:lisp-find-next-definition (fi::connection-open)]
      ["Arglist" fi:menu-lisp-arglist (fi::connection-open)]
      ["Toggle trace" fi:menu-toggle-trace-definition (fi::connection-open)]
      ["Macroexpand" fi:lisp-macroexpand (fi::connection-open)]
      ["Macroexpand recursively" fi:lisp-macroexpand-recursively (fi::connection-open)]
      ))

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

(defun fi:common-lisp-mode-popup-menu (e)
  (interactive "@e")
  (mouse-set-point e)
  (popup-menu fi:common-lisp-mode-popup-menu))

(defconst fi:inferior-common-lisp-mode-popup-menu
    '("inferior common lisp mode popup menu"
      ["Zoom" fi:debug-menu-zoom (fi::connection-open)]
      ["Down frame" fi:debug-menu-down-frame (fi::connection-open)]
      ["Up frame" fi:debug-menu-up-frame (fi::connection-open)]
      ["Edit frame" fi:debug-menu-edit-frame (fi::connection-open)]
      ["Locals for frame" fi:debug-menu-locals (fi::connection-open)]
      "----"
      ["Continue" fi:debug-menu-continue (fi::connection-open)]
      ["Restart" fi:debug-menu-restart (fi::connection-open)]
      ["Pop" fi:debug-menu-pop (fi::connection-open)]
      ["Reset" fi:debug-menu-reset (fi::connection-open)]
      "----"
      ["List processes" fi:debug-menu-processes (fi::connection-open)]
      ))

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

(defun fi:inferior-common-lisp-mode-popup-menu ()
  (interactive "@")
  (goto-char (point-max))
  (popup-menu fi:inferior-common-lisp-mode-popup-menu))

(defun fi::install-mode-menus ()
  (cond
   ((eq major-mode 'fi:common-lisp-mode)
    (define-key fi:common-lisp-mode-map [down-mouse-3]
      'fi:common-lisp-mode-popup-menu))
   ((eq major-mode 'fi:inferior-common-lisp-mode)
    (define-key fi:inferior-common-lisp-mode-map [down-mouse-3]
      'fi:inferior-common-lisp-mode-popup-menu))
   ((eq major-mode 'fi:lisp-listener-mode)
    (define-key fi:lisp-listener-mode-map [down-mouse-3]
      'fi:inferior-common-lisp-mode-popup-menu))))

(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::install-mode-menus)
(add-hook 'fi:common-lisp-mode-hook 'fi::install-mode-menus)
(add-hook 'fi:lisp-listener-mode-hook 'fi::install-mode-menus)

(require 'font-lock)

(add-hook 'fi:common-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'fi:emacs-lisp-mode-hook 'turn-on-font-lock)

(defvar fi::lisp-font-lock-keywords)
(setq fi::lisp-font-lock-keywords
  '(("^(\\(def\\(\\(const\\(ant\\|\\)\\|ine-key\\(\\|-after\\)\\|var\\)\\|\\(class\\|struct\\|type\\)\\|\\([^ \t\n()]+\\)\\)\\)[ \t'(]*\\([-.a-zA-Z0-9]+\\)?"
     (1 font-lock-keyword-face) (8 (cond ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 6) font-lock-type-face) (t font-lock-function-name-face)) nil t))))

(push '(fi:common-lisp-mode
	(fi::lisp-font-lock-keywords)
	nil
	nil
	(("+-*/.<>=!?$%_&~^:" . "w"))
	beginning-of-defun
	(font-lock-comment-start-regexp . ";")
	(font-lock-mark-block-function . mark-defun))
      font-lock-defaults-alist)

;;why redefine this????
;;(defun set-menubar-dirty-flag ()	;smh 31oct94
;;  (setq lucid-menu-bar-dirty-flag t))
