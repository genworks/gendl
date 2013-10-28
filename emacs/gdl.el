;;;; -*- coding: utf-8 -*-
;;                              GDL.EL
;;
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to configure emacs to load SLIME
;; and start an inferior lisp process which will itself load and run
;; the GENDL development environment.
;;
;; The intended readership is users and project developers.
;;
;; This document is not confidential. See end for copyright
;; information.

(require 'cl)


(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt) (w32-send-sys-command 61488))
  (unless (eq system-type 'windows-nt) (set-frame-parameter nil 'fullscreen 'maximized)))

(add-hook 'window-setup-hook 'maximize-frame t)


;; 2. WHERE ARE WE?

(defvar *gendl-home* (file-truename (concat (file-name-directory 
					     (file-truename load-file-name)) "../")))


;; 3. CONFIGURE EMACS
;;
;; This section is not SLIME-specific.
;;
;; 3.1 Flags, for commenting in or out.

;; FLAG -- comment the following if you want a tool bar.
;;         (errors ignored in case running in a console terminal)
(ignore-errors (tool-bar-mode -1))

(setq visible-bell t)

;; FLAG -- uncomment the following if you don't need a menu bar.
;;         (errors ignored in case running in a console terminal)
;;
;;(ignore-errors (menu-bar-mode -1))


;; 3.2. Set up color-theme and solarized color-themes

(add-to-list 'load-path (concat *gendl-home* "emacs/emacs-color-theme"))
(require 'color-theme)
(color-theme-initialize)
(color-theme-sitaramv-solaris)
;;(color-theme-feng-shui)

;; 3.3. Indents

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

(gdl:define-indents)

;; 3.4. Some handy global keybindings

(defun gdl:global-keys ()
  (global-set-key "\M-p" 'fi:pop-input)
  (global-set-key "\M-n" 'fi:push-input)
  (global-set-key "\C-xy" '(lambda() (interactive) (other-window -1)))
  ;;
  ;; Use C-j for newline-and-indent. If you really like it, you can bind
  ;; it to [Enter] by uncommenting the following line (from
  ;; http://emacswiki.org/emacs/AutoIndentation).
  ;;
  ;;(global-set-key (kbd "C-m") 'newline-and-indent)
  
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture))


(gdl:global-keys)

;; 3.5. Font

(defun gdl:set-font ()
  (interactive)
  ;;(ignore-errors (set-frame-font "-bitstream-Courier 10 Pitch-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
  (ignore-errors (set-frame-font  "-outline-Consolas-normal-normal-normal-mono-15-*-*-*-c-*-fontset-auto2")))

(gdl:set-font)

;; 3.6. Miscellania

(show-paren-mode t)

;; Use M-x clear-kill-ring in case you have too much garbage in the
;; kill ring
;;
(defun clear-kill-ring () (interactive) (setq kill-ring nil))


;; 4.  SLIME SETUP
;;
;; 4.1. Some synonyms

(defun gendl () (interactive) 
  (add-hook 'slime-connected-hook 'load-and-or-start-gendl t)
  ;;(add-hook 'slime-connected-hook 'load-base-ql)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.gdl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.gendl\\'" . lisp-mode))
  (with-temp-buffer (cd *gendl-home*) (slime)))
(defun gdl () (interactive) (gendl))
(defun glime () (interactive) (gendl))

(defun gdl-quit () (interactive) (slime-quit-lisp))
(defun gq () (interactive) (gdl-quit))
(defun gendl-quit () (interactive) (gdl-quit))


;; 4.2. Locate Common Lisp / Locate SLIME
;;
;; Where is our Common Lisp implementation? Where is slime-helper.el?
;;
;; If you don't have slime loaded yet, and/or you don't have 
;; slime-lisp-implementations set, then please make a file
;; .configure-glime.el in your home directory which sets
;; correct slime-lisp-implementations and quicklisp-slime-helper
;;
;; Loading the latter means we can load slime-autoloads.el (which
;; enables SLIME itself).
;;
;;

(defun configure-slime ()
  (setq quicklisp-slime-helper nil)
  (let ((built-in-config (concat *gendl-home* "emacs/configure-glime.el"))
	(home-config "~/.configure-glime.el"))
    (let ((config-file (if (file-exists-p built-in-config) built-in-config home-config)))
      (when (file-exists-p config-file) (load-file config-file))
      ;;
      ;; path-to-quicklisp-helper should have been set by config-file,
      ;; or slime must already be set up.
      ;;
      (when (file-exists-p quicklisp-slime-helper)
	(load-file quicklisp-slime-helper)))))


(configure-slime)
(require 'slime-autoloads)


;; 4.3. Configure SLIME

(eval-after-load "slime"
  '(progn
    (slime-setup '(slime-fancy slime-banner))
    (add-hook 'slime-connected-hook 'set-slime-shortcuts)
    (add-hook 'slime-connected-hook 'customise-slime)
    (add-hook 'slime-repl-mode-hook 'remove-dos-eol)
    (add-hook 'slime-connected-hook 'load-user-emacs-glime)))



(defun load-base-ql ()
  (slime-repl)
  (insert "(load (merge-pathnames \".load-base-ql.lisp\" (user-homedir-pathname)))")
  (slime-repl-return))


(defun load-and-or-start-gendl ()
  (slime-repl)
  (insert "(unless (find-package :gendl) (load (merge-pathnames \".load-gendl.lisp\" (user-homedir-pathname))))")
  (slime-repl-return)
  (insert (format "(when (find-package :gendl) (load (compile-file \"%semacs/glime.lisp\")))" *gendl-home*))
  (slime-repl-return)
  (insert "(when (find-package :gendl) (funcall (symbol-function (read-from-string \"gendl::startup-banner\"))))")
  (slime-repl-return)
  (insert "(let ((gendl-loaded? (find-package :gendl)) (genworks-gdl-loaded? (find-package :genworks-gdl)))
             (cond (genworks-gdl-loaded? (funcall (symbol-function (read-from-string \"gdl:start-gdl!\"))))
                   (gendl-loaded? (funcall (symbol-function (read-from-string \"gendl:start-gendl!\"))))
                   (t (format t  \"~%~%***~%Gendl or GDL is not loaded and did not load successfully from .load-gendl.lisp in your home directory.~%***~%~%\"))))")
  (slime-repl-return)
  (insert "(when (find-package :gendl) (in-package :gdl-user))")
  (slime-repl-return)
  (end-of-buffer))

(defun set-slime-shortcuts ()
  "Set keybindings for switching to slime buffers"
  (interactive)
  (global-set-key "\C-x&" '(lambda()(interactive) (switch-to-buffer (slime-repl-buffer))))
  (global-set-key "\C-x*" '(lambda()(interactive) (switch-to-buffer "*inferior-lisp*"))))

(defun customise-slime ()
  (setq slime-autodoc-use-multiline-p t))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;; 4.4. Prior to SLIME



(defun prior-to-glime ()
  (find-file (concat *gendl-home* "emacs/README.txt"))
  (toggle-read-only)
  (let ((frame-title "Genworks® GDL Interactive Authoring Environment"))
    (modify-frame-parameters nil (list (cons 'name frame-title))))
  (setq inhibit-splash-screen t)
  (cd "~/"))


(defun load-user-emacs-glime ()
  (when (file-exists-p "~/.emacs-glime") (load-file "~/.emacs-glime"))
  (when (file-exists-p "~/.emacs-glime.el") (load-file "~/.emacs-glime.el")))


(ignore-errors 
  (require 'package)
  ;; Add the original Emacs Lisp Package Archive
  (add-to-list 'package-archives
	       '("elpa" . "http://tromey.com/elpa/"))
  ;; Add the user-contributed repository
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/")))


;; 5.  MAKE IT HAPPEN

(prior-to-glime)
(glime)


;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright 2013 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;;
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.


