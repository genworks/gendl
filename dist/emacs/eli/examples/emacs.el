;; A sample ~/.emacs file.
;;
;; $Id: emacs.el,v 3.0 2003/12/15 22:52:58 layer Exp $

(defvar *eli-directory*)
(setq *eli-directory* (expand-file-name "~/cl-ultra/src/eli/"))

(when (and (not (string-match "xemacs" emacs-version))
	   (= emacs-major-version 20)
	   (<= emacs-minor-version 2))
  (setq load-path (cons *eli-directory* load-path)))

(load (format "%sfi-site-init" *eli-directory*))

(setq fi:common-lisp-image-name "/usr/local/cl-5.0")
(setq fi:common-lisp-host "ultra")

;; This function starts up lisp with your defaults.
(defun run-common-lisp ()
  (interactive)
  (fi:common-lisp fi:common-lisp-buffer-name
		  fi:common-lisp-directory
		  fi:common-lisp-image-name
		  fi:common-lisp-image-arguments
		  fi:common-lisp-host
                  fi:common-lisp-image-file))

;; Set up a keybinding for `run-common-lisp', two possible ways:
(progn
  (setq ctlx-3-map (make-keymap))
  (define-key ctl-x-map "3" ctlx-3-map)
  (define-key ctlx-3-map "l" 'run-common-lisp))
;; or this:
(define-key global-map "\C-xl" 'run-common-lisp)

;; Run cl each time emacs is run:
(run-common-lisp)
