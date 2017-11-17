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


(defvar *default-font-size* 15)

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

;;(setq visible-bell t)

;; FLAG -- uncomment the following if you don't need a menu bar.
;;         (errors ignored in case running in a console terminal)
;;
;;(ignore-errors (menu-bar-mode -1))


;; 3.2. Set up color-theme and solarized color-themes
;;(setq calendar-latitude 42.58 calendar-longitude -83.3 calendar-location-name "Detroit")
;;(setq sunrise-sunset (sunrise-sunset))
;; FLAG -- figure out how to use this information with (current-time) to pick a light or dark color-theme. 
(add-to-list 'load-path (concat *gendl-home* "emacs/emacs-color-theme")) 
(require 'color-theme)
(color-theme-initialize)
;;(color-theme-subtle-hacker)
;;(color-theme-high-contrast)
;;(color-theme-taming-mr-arneson)
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
  )

(gdl:global-keys)

;; 3.5. Font

(defun gdl:set-font ()
  (interactive)
  (let ((font-size *default-font-size*)) ;; (/ (display-mm-height) 16)
    (set-frame-font
     (format 
      (case system-type
	(darwin  "-*-Courier New-normal-normal-normal-*-%s-*-*-*-m-0-iso10646-1" ;; -apple-Courier_New-medium-normal-normal-*-%s-*-*-*-m-0-iso10646-1"
		 )
	;;(windows-nt "-outline-Courier New-normal-normal-normal-mono-%s-*-*-*-c-*-iso8859-1")
	(windows-nt "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-utf8")
	(gnu/linux "-bitstream-Courier 10 Pitch-normal-normal-normal-*-%s-*-*-*-m-0-iso10646-1"))
      font-size))))

(ignore-errors (gdl:set-font))


;; 3.6. Miscellania

(show-paren-mode t)


;; Use M-x clear-kill-ring in case you have too much garbage in the
;; kill ring
;;
(defun clear-kill-ring () (interactive) (setq kill-ring nil))


;; 4.  SLIME SETUP
;;
;; 4.1. Some synonyms

(defun gendl (&optional exe) (interactive) 
  (add-hook 'slime-connected-hook 'load-and-or-start-gendl t)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.gdl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.gendl\\'" . lisp-mode))
  (with-temp-buffer (cd *gendl-home*) (slime exe)))

(defun gdl () (interactive) (gendl 'gdl))
(defun agdl8 () (interactive) (gendl 'agdl8))
(defun agdl () (interactive) (gendl 'agdl))
(defun glime () (interactive) (gendl))

(defun gdl-quit () (interactive) (slime-quit-lisp))
(defun gq () (interactive) (gdl-quit))
(defun gendl-quit () (interactive) (gdl-quit))
(defun quit-gendl () (interactive) (gdl-quit))
(defun quit-gdl () (interactive) (gdl-quit))



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
    (slime-setup '(slime-fancy slime-banner slime-tramp))
    (add-hook 'slime-connected-hook 'set-slime-shortcuts)
    (add-hook 'slime-connected-hook 'customise-slime)
    (add-hook 'slime-repl-mode-hook 'remove-dos-eol)
    (add-hook 'slime-connected-hook 'load-user-emacs-glime)))



(defun load-base-ql ()
  (slime-repl)
  (insert "(load (merge-pathnames \".load-base-ql.lisp\" (user-homedir-pathname)))")
  (slime-repl-return))

(defvar gdl-startup-string nil)


(setq gdl-startup-string 
  (format 
   "(progn (unless (find-package :gendl)
	    (let ((load-file (probe-file (merge-pathnames \".load-gendl.lisp\" (user-homedir-pathname)))))
	      (load load-file)))

	  (let ((gendl-loaded? (find-package :gendl)) (genworks-gdl-loaded? (find-package :genworks-gdl)))
            (when (or gendl-loaded? genworks-gdl-loaded?)
              (funcall (symbol-function (read-from-string \"uiop:setup-temporary-directory\"))))
	    (cond (genworks-gdl-loaded? (funcall (symbol-function (read-from-string \"gdl:start-gdl!\"))))
		  (gendl-loaded? (funcall (symbol-function (read-from-string \"gendl:start-gendl!\"))))
		  (t (format t  \"~%%~%%***~%%Gendl or GDL is not loaded and did not load successfully 
  from .load-gendl.lisp in your home directory.~%%***~%%~%%\"))))
	  (when (find-package :gendl) (in-package :gdl-user))
         (when (find-package :gwl) (funcall (read-from-string \"gwl:announce-server-port\"))))" *gendl-home*))


;;
;; DAP -- user can also setq this in ~/.emacs-gdl
;;
(defvar user-startup-string "")

(defun load-and-or-start-gendl ()
  (setq slime-enable-evaluate-in-emacs t)
  (slime-repl)
  (insert gdl-startup-string)
  (slime-repl-return)
  (end-of-buffer)

  ;;
  ;; DAP  -- Added to support user-startup-string
  ;;
  (slime-repl-return)
  (insert user-startup-string)
  (slime-repl-return)
  )



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
  (dolist (file (list "~/.emacs-glime" "~/.emacs-glime.el"))
    (when (file-exists-p file) (load-file file))))

(defun load-user-emacs-gendl ()
  (dolist (file (list "~/.emacs-gendl" "~/.emacs-gdl" 
		      "~/.emacs-gendl.el" "~/.emacs-gdl.el"))
    (when (file-exists-p file) (load-file file))))




(ignore-errors 
  (require 'package)
  ;; Add the original Emacs Lisp Package Archive
  (add-to-list 'package-archives
	       '("elpa" . "http://tromey.com/elpa/"))
  ;; Add the user-contributed repository
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))

  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  
  )


;; 5.  MAKE IT HAPPEN

(prior-to-glime)

;;
;; FLAG -- conditionalize based on running slime or ELI
;;
;;(glime)

(defvar *eli-init* (concat *gendl-home* "program/eli/fi-site-init.el"))


(when (file-exists-p *eli-init*)

  (defun get-executable (exe-name)

      (let* ((info (assoc exe-name slime-lisp-implementations )))
	(replace-regexp-in-string 
	 "/program/program/" "/program/"
	 (case system-type 
	   (windows-nt
	    (concat (file-name-directory (first (second info)))
		    (replace-regexp-in-string "\\\\" "/" (second (second info)))))
	   ((darwin gnu/linux)
	    (first (second info)))))))


  (defvar gdl:*gdl-toplevel-base* "*gdl toplevel*")


  (defun agdl8e () (interactive)
    (gdl-devo (get-executable 'agdl8)))

  (defun agdle () (interactive)
     (gdl-devo (get-executable 'agdl)))
  
  (defun gdle () (interactive)
    (gdl-devo (get-executable 'gdl)))

  (defun gdl-devo (executable) 
    (interactive)
    (load-file *eli-init*)
	  
    (setq gdl:*gdl-toplevel* 
	  (concat gdl:*gdl-toplevel-base*
		  (if (equalp (subseq (file-name-sans-extension (file-name-nondirectory executable))
				      0 1) "a")
		      "(ANSI)" "(modern)")))
    (fi:common-lisp gdl:*gdl-toplevel* *gendl-home* executable nil)


    (global-set-key "\C-x&" '(lambda()(interactive) (switch-to-buffer gdl:*gdl-toplevel*)))

    (switch-to-buffer gdl:*gdl-toplevel*)
    (end-of-buffer)
    (goto-char (point-max))
    (insert gdl-startup-string)
    (fi:inferior-lisp-newline)
    (end-of-buffer)


    ;;
    ;; DAP - Added to support user-startup-string
    ;;
    (end-of-buffer)
    (goto-char (point-max))
    (insert user-startup-string)
    (fi:inferior-lisp-newline)
    (end-of-buffer)))


(defun timestamp ()
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))

;;
;; See http://pomodorotechnique.com/
;;
(defun pomodoro ()
  (timestamp)
  (insert (format-time-string ";%Y-%m-%dT%H:%M:%S;" 
			      (time-add (current-time) (seconds-to-time (* 25 60))))))
(global-set-key "\C-t" '(lambda() (interactive) (pomodoro)))

(defun post-pomodoro ()
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S" 
			      (time-subtract (current-time) (seconds-to-time (* 25 60)))))
  (insert (format-time-string ";%Y-%m-%dT%H:%M:%S;" (current-time))))

(global-set-key "\M-t" '(lambda() (interactive) (post-pomodoro)))



;;
;; Establish input method for I.A.S.T diacritics
;;
(let ((file (concat *gendl-home* "emacs/sa-translit.el"))) 
  (when (file-exists-p file)
    (load-file file)
    (register-input-method
     "sa-translit" "Sanskrit Transliteration" 'quail-use-package
     "sa-translit" "Converts Harvard-Kyoto and ITRANS scheme to IAST diacritics."
     file)))

(maximize-frame)

(load-user-emacs-gendl)




;;(defun mac-switch-meta nil 
;;  "switch meta between Option and Command"
;;  (interactive)
;;  (if (eq mac-option-modifier nil)
;;      (progn
;;	(setq mac-option-modifier 'meta)
;;	(setq mac-command-modifier 'control))
;;    (progn 
;;      (setq mac-option-modifier nil)
;;      (setq mac-command-modifier 'meta))))

(defun mac-switch-meta nil
  "make option key behave as meta"
  (setq mac-option-modifier 'meta))

(when (eql system-type 'darwin)
  (mac-switch-meta))



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


