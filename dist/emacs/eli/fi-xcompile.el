
(require 'cl)

;;We're going with emacs 20.x now, and the following doesn't variable
;;having a non-nil value doesn't work compiling eli... 
;;(setq byte-compile-compatibility t)

(require 'font-lock) ;; makes some compiler warnings go away
(require 'comint) ;; makes some compiler warnings go away
(defvar mode-motion-hook) ;; makes xemacs compiler warning go away

(push (expand-file-name ".") load-path)

(setq fi::load-el-files-only t)
(load "fi-site-init")
(load "fi-leep0.el")

(setq fi-files '("fi-leep-xemacs" "fi-xemacs"))

(dolist (file fi-files)
  (let ((el (format "%s.el" file))
	(elc (format "%s.elc" file)))
    (unless (file-newer-than-file-p elc el)
      (message "--------------------------------------------------------")
      (byte-compile-file el))))

