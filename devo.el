
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;
;; FLAG -- learn how to add command line args to inferior-lisp-program
;; to load load.lisp for Genworks GDL automatically.
;;
(if (string= (getenv "cl_platform") "LispWorks")
    (setq inferior-lisp-program "~/bin/lw-console")
  (if (string= (getenv "cl_platform") "Allegro")
    (setq inferior-lisp-program "/usr/local/acl/mlisp")
   (error "Don't know what CL executable to use")))


(defun gdl () (interactive) (slime))


(setq gdl:*gdl-toplevel* "*slime-repl allegro*")
(setq gdl:*inferior-lisp* "*inferior-lisp*")

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

(require 'cl)

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




