(require 'cl)


(setq inferior-lisp-program "c:/path/to/start-swank.bat ../../common/acl82-linux-x86/alisp")




;;(load (expand-file-name "../../common/quicklisp/slime-helper.el"))

;;
;; FLAG -- learn how to add command line args to inferior-lisp-program
;; to load load.lisp for Genworks GDL automatically.
;;

(setq slime-lisp-implementations 
      (cond ((and (string= (getenv "cl_platform") "LispWorks")
                  (string= (getenv "os_platform") "Darwin"))
             '((lw60-macosx-x86 ("../../common/lw60-macosx-x86/lw-console"))))
            ((and (string= (getenv "cl_platform") "Allegro")
                  (string= (getenv "os_platform") "Linux"))
             '((acl82m-linux-x86 ("../../common/acl82-linux-x86/mlisp"))
               (acl82a-linux-x86 ("../../common/acl82-linux-x86/alisp"))))
            (t '((acl82m-win-x86 ("../../common/acl82-win-x86/mlisp.exe"))
                 (acl82a-win-x86 ("../../common/acl82-win-x86/alisp.exe"))))))

(slime-setup '(slime-fancy))

(defun gdl () (interactive) (slime ))
(setq gdl:*gdl-toplevel* (concat "*" "slime-repl " (symbol-name (first (first slime-lisp-implementations))) "*"))
(setq gdl:*inferior-lisp* "*inferior-lisp*")

(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path "/usr/local/slime/contrib")
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))


(global-set-key "\C-x&" '(lambda()(interactive) (switch-to-buffer gdl:*gdl-toplevel*)))
(global-set-key "\C-x*" '(lambda()(interactive) (switch-to-buffer gdl:*inferior-lisp*)))
(global-set-key "\C-xy" '(lambda() (interactive) (other-window -1)))


;;
;; FLAG -- comment the following if you want a tool bar.
;;         (errors ignored in case running in a console terminal)
(ignore-errors (tool-bar-mode -1))

(setq visible-bell t)


;;
;; FLAG -- uncomment the following if you don't need a menu bar.
;;         (errors ignored in case running in a console terminal)
;;
;;(ignore-errors (menu-bar-mode -1))
;;
;; Use M-x clear-kill-ring in case you have too much garbage in the
;; kill ring
;;
(defun clear-kill-ring () (interactive) (setq kill-ring nil))

;;
;; FLAG -- you can customize default GDL emacs colors here.
;;
(defun gdl:set-colors ()
  (interactive)
  (progn
    (set-foreground-color "white")
    (set-background-color "midnight blue")
    (setq default-frame-alist (append default-frame-alist '((background-color . "midnight blue"))))
    (set-face-foreground 'default "white")
    (set-face-foreground 'font-lock-type-face "hotpink")
    (set-face-foreground 'font-lock-keyword-face "Cyan")
    (set-face-foreground 'font-lock-string-face "#88FFAA")
    (set-face-foreground 'font-lock-function-name-face "#55ff88")
    (set-face-foreground 'font-lock-comment-face "#888888")
    (set-face-foreground 'font-lock-builtin-face "#ffff99")
    (add-to-list 'default-frame-alist '(cursor-color . "RosyBrown4"))
    (set-cursor-color "#55FFFF")))


;; ignore errors when setting colors in case of running in console
;; terminal (there's probably a better way to test for this).
(ignore-errors (gdl:set-colors))

(defun gdl:set-font ()
  (interactive)
  (ignore-errors (set-frame-font "-bitstream-Courier 10 Pitch-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))

(gdl:set-font)

(show-paren-mode t)



(defun add-gdl-font-lock-keywords ()
  (let ((definition-keywords 
          (regexp-opt (list "define-object-amendment" "define-object" "define-view" "define-lens" "define-format" "define-skin" 
                            "defpart" "defwriter" "defcompanion" "write-the-object" "write-the"
                            "the " "the-child" "the-object" "the-element"  ) t))
        (keyword-keywords
         (regexp-opt (list ":documentation"
                           ":input-slots" ":computed-slots" ":objects" ":hidden-objects" ":functions" ":methods"
                           ":output-functions" ":skin" ":trickle-down-slots" ":type" ":sequence" 
                           ":size" ":parameters" ":pass-down" ":inputs" ":optional-inputs"  
                           ":modifiable-optional-inputs" ":descendant-attributes" ":attributes" 
                           ":parts" ":pseudo-parts" ":methods") t)))
    (pushnew (list definition-keywords 0 font-lock-keyword-face) lisp-font-lock-keywords)
    (pushnew (list keyword-keywords 0 font-lock-type-face) lisp-font-lock-keywords)))


(add-gdl-font-lock-keywords)


(defmacro defindent (name indentation)
  (let* ((keyword-list '(lisp-font-lock-keywords t t t nil nil nil "&allow-other-keys"))
         (spec (if (eq indentation 'gdl-object-style) `((1 t ,keyword-list) (0 t 2)) indentation)))
    `(put ',name 'lisp-indent-hook ',spec)))

(defun gdl:define-indents ()
  (defindent define-object gdl-object-style)
  (defindent define-view (like define-object))
  (defindent define-format (like define-object))
  (defindent defpart (like define-object))
  (defindent defwriter (like define-object))
  (defindent defcompanion (like define-object)))

;;
;; From EvW
;;

(setq next-line-add-newlines nil)

(defun ISO-current-time-string ()
  (format-time-string "%Y-%m-%dT%T%z"))

(defun EvW-insert-date ()
  "time and date function suggested by Tom Capey (2000-11-28T19:21:04+0100)"
  (interactive)
  (insert (ISO-current-time-string)))

(defun EvW-save-and-kill-buffer (bufname)
  "Saves current buffer, then kills it"
  (interactive "b")
  (let ((cb (current-buffer)))
    (save-buffer cb)
    (kill-buffer cb)))


(global-set-key "\M-t" 'EvW-insert-date)
(global-set-key "\C-x\C-k" 'EvW-save-and-kill-buffer)

(defvar font-lock-maximum-decoration t)
(global-font-lock-mode t)

(add-hook 'write-file-hooks 'time-stamp)
(defvar time-stamp-active t)
(defvar time-stamp-format "%3a, %:y-%02m-%02dT%02H:%02M:%02S -- %f")


;; if you encounter a file with ^M or ... at the end of every line,
;; this means a worng copy by samba or floppy disk of the DOS file to UNIX.
;; get rid of them by pressing [F5].
(defun cut-ctrlM ()
  (interactive)
  (goto-char (point-min));(beginning-of-buffer)
  (while (search-forward "\r" nil t)
    (replace-match "" nil t))
  (not-modified)
  (goto-char (point-min)))

(defun replace-ctrlM (ch)               ;2001-04-17T17:46:41+0200
  (interactive)
  "Replace all visible ^M with argument (defaulting to the empty string.)"
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match ch nil t))
  ;;(not-modified)
  (goto-char (point-min)))

(defun remove-trailing-whitespace ()
  "Removes trailing blanks and tabs from the buffer."
  (interactive)
  (save-excursion                       ;let deactivate-mark nil?
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))

(defun tidy-up ()
  "Removes trailing whitespace and ^M from the end of lines.
Bug: does not leave point at the position it had when the function was called"
  (interactive)
  (save-excursion
    (let ((curpos (point)))
      (cut-ctrlM)
      (remove-trailing-whitespace)
      (goto-char curpos))))

;;; EOF
