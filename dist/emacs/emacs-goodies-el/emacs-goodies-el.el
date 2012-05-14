;;; emacs-goodies-el.el --- startup file for the emacs-goodies-el package

;;; Commentary:
;;
;; This file is loaded from /etc/emacs/site-start.d/50emacs-goodies-el.el

;;; History:
;;
;; 2009-02-22 Peter Galbraith
;;  - Replace $ by \\' in auto-mode-alist entries (Closes: #570293)
;; 2006-11-26 - Ramkumar R.
;;  - Obey `emacs-goodies-el-defaults' for xrdb-mode.
;; 2003-06-14 - Peter Galbraith
;;  - Delete autoloads that can be generated automatically.
;; 2003-05-14 - Peter Galbraith
;;  - Created from 50emacs-goodies-el.el contents.

;;; Code:

(defgroup emacs-goodies-el nil
  "Debian emacs-goodies-el package customization."
  :group 'convenience)

(require 'emacs-goodies-loaddefs)
(require 'emacs-goodies-custom)

(defcustom emacs-goodies-el-defaults nil
  "Whether default settings are chosen conservatively or aggressively.
non-nil means aggressive.
Setting to aggressive will enable features that supercede Emacs defaults."
  :type '(radio (const :tag "conservative" nil)
                (const :tag "aggressive" t))
  :link '(custom-manual "(emacs-goodies-el)Top")
  :group 'emacs-goodies-el)

;; align-string.el
(autoload 'align-string "align-string"
  "Align first occurrence of REGEXP in each line of region."
  t)
(autoload 'align-all-strings "align-string"
  "Align all occurrences of REGEXP in each line of region."
  t)

;; apache-mode.el
(add-to-list 'auto-mode-alist '("apache2\\.conf\\'"  . apache-mode))

;; clipper.el
(autoload 'clipper-create "clipper" "Create a new 'clip' for use within Emacs."
  t)
(autoload 'clipper-delete "clipper" "Delete an existing 'clip'." t)
(autoload 'clipper-insert "clipper"
  "Insert a new 'clip' into the current buffer."
  t)
(autoload 'clipper-edit-clip "clipper" "Edit an existing 'clip'." t)

;; cvs-mode.el
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; cyclebuffer.el
(autoload 'cyclebuffer-forward "cyclebuffer"
  "Cycle buffer forward."
  t)
(autoload 'cyclebuffer-backward "cyclebuffer"
  "Cycle buffer backward."
  t)

;; dict.el
(autoload 'dict "dict" "Lookup a word in the dictionary" t)
(autoload 'dict-region "dict" "Lookup a region in the dictionary" t)

;; ff-paths.el
(defcustom ff-paths-install emacs-goodies-el-defaults
  "Whether to setup ff-paths for use.
find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (ff-paths-install)))
  :load 'ff-paths
;;  :require 'ff-paths
  :group 'emacs-goodies-el
  :group 'ff-paths)

(defcustom ff-paths-use-ffap emacs-goodies-el-defaults
  "Whether to setup ffap for use.

Usually packages don't advertise or try to setup other packages, but
ff-paths works well in combination with ffap (Find FILENAME, guessing a
default from text around point) and so I recommend it here.

find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (require 'ffap)
           (ff-paths-in-ffap-install)))
;;  :require 'ff-paths
  :load 'ff-paths
  :group 'emacs-goodies-el
  :group 'ff-paths)

;; filladapt
(autoload 'turn-on-filladapt-mode "filladapt"
  "Unconditionally turn on Filladapt mode in the current buffer."
  t)

(defcustom filladapt-turn-on-mode-hooks nil
  "*List of hooks for which to turn-on filladapt.
Filladapt works well with any language that uses comments that
start with some character sequence and terminate at end of line.
So it is good for Postscript, Lisp, Perl, C++ and shell modes.
It's not good for C mode because C's comments are multiline."
  :type '(set (const text-mode-hook)
              (const awk-mode-hook)
              (const lisp-mode-hook)
              (const emacs-lisp-mode-hook)
              (const perl-mode-hook))
  :set (lambda (symbol value)
         ;; Remove old values since user may have deleted entries
         (if (and (boundp 'filladapt-mode-hooks) filladapt-mode-hooks)
             (mapcar (lambda (hook) (remove-hook hook 'turn-on-filladapt-mode))
                     filladapt-mode-hooks))
         (set-default symbol value)
         ;; Set entries selected by the user.
         (mapcar (lambda (hook) (add-hook hook 'turn-on-filladapt-mode))
                 value))
  :load 'filladapt
  :group 'emacs-goodies-el
  :group 'filladapt)

;; highlight-completion.el
(autoload 'highlight-completion-mode "highlight-completion"
  "Activate highlight-completion."
  t)

;; highlight-current-line.el - compatibility
(autoload 'highlight-current-line-on "highlight-current-line"
  "Switch highlighting of cursor-line on/off globally."
  t)

;; home-end.el
(defvar home-end-end-enable nil
  "Whether `home-end-enable' was activated.
Stores the value of the prior `end' keybinding.")
(defvar home-end-home-enable nil
  "Whether `home-end-enable' was activated.
Stores the value of the prior `home' keybinding.")
(defcustom home-end-enable emacs-goodies-el-defaults
  "*Define [home] and [end] keys to act differently when hit 1, 2 or 3 times."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond
          (value
           (setq home-end-end-enable (key-binding [end])
                 home-end-home-enable (key-binding [home]))
           (global-set-key [end]  'home-end-end)
           (global-set-key [home] 'home-end-home))
          (t
           (if home-end-end-enable
               (global-set-key [end] home-end-end-enable))
           (if home-end-home-enable
               (global-set-key [home] home-end-home-enable)))))
  :load 'home-end
  :group 'emacs-goodies-el)

;; keydef.el
(autoload 'keydef "keydef"
  "Define the key sequence SEQ, written in kbd form, to run CMD."
  t)

;; keywiz.el
(autoload 'keywiz "keywiz"
  "Start a key sequence quiz."
  t)

;; map-lines.el
(autoload 'map-lines "map-lines"
  "Map COMMAND over lines matching REGEX."
  t)

;; maplev
(autoload 'maplev-mode "maplev" "Maple editing mode" t)
(autoload 'cmaple      "maplev" "Start maple process" t)
(add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))

;; matlab
(defcustom matlab-auto-mode nil
  "*Enter matlab-mode when editing .m files.
Technically, this adjusts the `auto-mode-list' when set.
To unset, you will have to restart Emacs."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond
          (value
           (add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode)))))
  :load 'matlab
  :group 'emacs-goodies-el
  :require 'matlab)

;; minibuf-electric.el
(defcustom minibuffer-electric-file-name-behavior nil
  "*If non-nil, slash and tilde in certain places cause immediate deletion.
These are the same places where this behavior would occur later on anyway,
in `substitute-in-file-name'."
  :type 'boolean
  :require 'minibuf-electric
  :load 'minibuf-electric
  :group 'emacs-goodies-el
  :group 'minibuffer)

;; mutt-alias.el
(autoload 'mutt-alias-insert "mutt-alias"
  "Insert the expansion for ALIAS into the current buffer."
  t)
(autoload 'mutt-alias-lookup "mutt-alias"
  "Lookup and display the expansion for ALIAS."
  t)

;; muttrc-mode.el
(add-to-list 'auto-mode-alist '("muttrc" . muttrc-mode))


;; pod-mode.el
(add-to-list 'auto-mode-alist '("\\.pod\\'" . pod-mode))

;; rfcview
(add-to-list 'auto-mode-alist
             '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . rfcview-mode))

;; session.el
(autoload 'session-initialize "session"
  "Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'."
  t)

;; setnu.el
(autoload 'setnu-mode "setnu"
  "Toggle setnu-mode."
  t)
(autoload 'turn-on-setnu-mode "setnu"
  "Turn on setnu-mode."
  nil)

;; slang-mode.el
(setq auto-mode-alist
      (append '(("\\.sl\\'" . slang-mode)) auto-mode-alist))

;; todoo.el
(when (not (featurep 'xemacs))
  (autoload 'todoo "todoo"
    "TODO Mode."
    t)
  (autoload 'todoo-mode "todoo"
    "TODO Mode"
    t)
  (add-to-list 'auto-mode-alist '("TODO\\'" . todoo-mode)))

;; toggle-option.el
(autoload 'toggle-option "toggle-option"
  "Easily toggle frequently toggled options."
  t)

;; upstart-mode.el
(when (not (featurep 'xemacs))
  (autoload 'upstart-mode "upstart-mode"
    "major mode for .upstart files."
    t)
  (add-to-list 'auto-mode-alist '("\\.upstart\\'" . upstart-mode)))

;; xrdb-mode.el

(defun xrdb-mode-setup-auto-mode-alist ()
  (add-to-list 'auto-mode-alist '("\\.Xdefaults\\'" . xrdb-mode))
  (add-to-list 'auto-mode-alist '("\\.Xenvironment\\'". xrdb-mode))
  (add-to-list 'auto-mode-alist '("\\.Xresources\\'". xrdb-mode))
  (add-to-list 'auto-mode-alist '("\\.ad\\'". xrdb-mode))
  (add-to-list 'auto-mode-alist '("/app-defaults/". xrdb-mode))
  (add-to-list 'auto-mode-alist '("/Xresources/". xrdb-mode)))

(defcustom xrdb-mode-setup-auto-mode-alist
  (or
   ;; Check if conf-xdefaults-mode is present
   (not (fboundp 'conf-xdefaults-mode))
   ;; Check if default setup provides bindings for conf-xdefaults-mode
   (< emacs-major-version 22)
   (featurep 'xemacs)
   ;; Check if the user wants settings to be clobbered
   emacs-goodies-el-defaults)
  "Whether to setup mode-alists for xrdb mode.

Newer versions of Emacs have a conf-xdefaults-mode which provides
this functionality. `xrdb' still has some features (like
electricity) which are absent in that mode. Setting this to
non-nil clobbers the default bindings in such cases.

This variable defaults to t for older emacsen and the value
`emacs-goodies-el-defaults' for newer ones.

Customizing this variable might require restarting emacs for the
effects to take effect."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (xrdb-mode-setup-auto-mode-alist)))
  :group 'emacs-goodies-el
  :group 'xrdb)

(provide 'emacs-goodies-el)

;;; emacs-goodies-el.el ends here
