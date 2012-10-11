;;; color-theme_seldefcustom.el --- color-theme selection via customize interface
;;; Commentary:
;; 
;; Peter S Galbraith <psg@debian.org>, 2005-10-25
;; License: GPLV2 or later.

;; A color-theme can can selected and enabled for future sessions by
;; customizing the variable `color-theme-selection' and saving the setting
;; instead of calling the interactive command `color-theme-select'

;;; Code:

(require 'color-theme)

(defcustom color-theme-selection nil
  "Color theme selection.
Select and save to enable your choice in future sessions.
There is very limited undo capability to the previous state only."
  :type (progn
          (setq color-themes (delq (assq 'color-theme-snapshot color-themes)
                                   color-themes)
                color-themes (delq (assq 'bury-buffer color-themes)
                                   color-themes))
          (append
           '(radio)
           (cons '(const :tag "Undo" nil)
                 (mapcar (function (lambda (arg) `(const ,arg)))
                         (mapcar '(lambda (x) (elt x 1)) color-themes)))))
  :set (lambda (symbol value)
         (set-default symbol value)
         (require 'color-theme)         ; :load doesn't seem to work
         (unless color-theme-initialized (color-theme-initialize))
         (cond
          (value
           (fset 'color-theme-snapshot (color-theme-make-snapshot))
           (eval
            (delq nil
                  (mapcar
                   '(lambda (x) (if (string-equal (elt x 1) value)
                                    (car x)))
                   color-themes))))
          ((fboundp 'color-theme-snapshot)
           (color-theme-snapshot))))
  :group 'color-theme
  :require 'color-theme_seldefcustom)

(provide 'color-theme_seldefcustom)

;;; color-theme_seldefcustom.el ends here
