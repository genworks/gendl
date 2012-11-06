(require 'cl)

(defvar gdl:*gdl-home* (concat default-directory "../"))

(defvar gdl:*gdl-program-home* default-directory)
(defvar gdl:*gdl-toplevel-base* "*gdl toplevel*")
(defvar gdl:*mgdl-image-name* "gdl")

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


(add-gdl-font-lock-keywords)
(gdl:define-indents)
(setq font-lock-verbose nil)


;;
;; Normal ELI startup
;;
(defun gdl () (interactive)
  (load-file (concat gdl:*gdl-home* "emacs/eli/fi-site-init.el"))  
  (gdl-devo gdl:*mgdl-image-name*)
  ;;(add-gdl-font-lock-keywords)
  (setq fi:lisp-mode-hook
          (function
           (lambda ()
             (let ((map (current-local-map)))
               (define-key map "\C-c."  'find-tag)
               (define-key map "\C-c,"  'tags-loop-continue)
               (define-key map "\e."    'fi:lisp-find-definition)
               (define-key map "\e,"    'fi:lisp-find-next-definition)))))

  ;;(gdl:define-indents)
  )



(defun gdl-devo (image-name) 
  (interactive)
  (let ((executable (or (fi::probe-file (concat gdl:*gdl-program-home* image-name ".exe"))
                        (fi::probe-file (concat gdl:*gdl-program-home* image-name)))))
    (setq gdl:*gdl-toplevel* 
          (concat gdl:*gdl-toplevel-base*
                  (if (equalp (subseq (file-name-sans-extension executable)
                                      0 1) "a")
                      "(ANSI)" "(modern)")))
    (fi:common-lisp gdl:*gdl-toplevel* gdl:*gdl-home* executable nil)))


;;
;; Some handy global keybindings
;;
(defun gdl:global-keys ()
  (global-set-key "\M-p" 'fi:pop-input)
  (global-set-key "\M-n" 'fi:push-input)
  (global-set-key "\C-x&" '(lambda()(interactive) (switch-to-buffer gdl:*gdl-toplevel*)))
  (global-set-key "\C-x*" 'fi:open-lisp-listener)
  (global-set-key "\C-xy" '(lambda() (interactive) (other-window -1)))
  ;;
  ;; Use C-j for newline-and-indent. If you really like it, you can bind
  ;; it to [Enter] by uncommenting the following line (from
  ;; http://emacswiki.org/emacs/AutoIndentation).
  ;;
  ;;(global-set-key (kbd "C-m") 'newline-and-indent)
  )


(gdl:global-keys)


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
;;(ignore-errors (gdl:set-colors))

(defun gdl:set-font ()
  (interactive)
  (ignore-errors (set-frame-font "-bitstream-Courier 10 Pitch-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))

(gdl:set-font)

(show-paren-mode t)



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


(cd gdl:*gdl-home*)


;;
;; SLIME setup
;;

(load-file "quicklisp/slime-helper.el")
(setq inferior-lisp-program (if (file-exists-p "program/gdl.exe")
                                (concat "program/run-gdl-slime.bat"
                                        " "
                                        "program\\gdl.exe")
                              "program/gdl"))

(defun glime () (interactive) (cd gdl:*gdl-home*) (slime))
(defun gendl () (interactive) (glime))
(defun quit-gendl () (interactive (slime-quit-lisp)))


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(slime-setup '(slime-fancy))
(require 'slime-autoloads)
(eval-after-load "slime"
  '(progn
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (set-slime-shortcuts)
    (add-hook 'slime-repl-mode-hook 'remove-dos-eol)

    (add-to-list 'auto-mode-alist '("\\.gdl\\'" . lisp-mode))
    (add-to-list 'auto-mode-alist '("\\.gendl\\'" . lisp-mode))

    ))

;;
;; To switch to slime buffer shortcuts
;;
(defun set-slime-shortcuts ()
  (interactive)
  (global-set-key "\C-x&" '(lambda()(interactive) (switch-to-buffer "*slime-repl allegro*")))
  (global-set-key "\C-x*" '(lambda()(interactive) (switch-to-buffer "*inferior-lisp*"))))

;;
;; To switch back to ELI bindings
;;
(defun set-eli-shortcuts ()
  (interactive)
  (global-set-key "\C-x&" '(lambda()(interactive) (switch-to-buffer gdl:*gdl-toplevel*)))
  (global-set-key "\C-x*" 'fi:open-lisp-listener))


(find-file "emacs/README.txt")
(toggle-read-only)

(cd gdl:*gdl-home*)

;;
;; Set up color-theme and solarized color-themes:
;;

(add-to-list 'load-path "emacs/emacs-color-theme")

(require 'color-theme)
(color-theme-initialize)
(color-theme-sitaramv-solaris)

;;
;; end of color-theme setup.
;;

(defun frame-retitle (title)
  (modify-frame-parameters nil (list (cons 'name title))))

(frame-retitle "Genworks GenDL Interactive Authoring Environment")

(message "Please see the Startup section of this document for starting the Gendl environment.")

(when (file-exists-p "~/.emacs-gendl")
  (load-file "~/.emacs-gendl"))

(cd gdl:*gdl-home*)

;;; EOF
