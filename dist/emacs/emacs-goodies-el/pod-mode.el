;;; pod-mode.el --- Major mode for editing .pod-files

;;; POD is the Plain Old Documentation format of Perl.

;;; Copyright 2003-2010 Steffen Schwigon

;;; Author: Steffen Schwigon <ss5@renormalist.net>
;;;
;;; Keywords: perl pod
;;; X-URL: http://search.cpan.org/~schwigon/pod-mode/

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;;; MA 02110-1301, USA.

;;; This code is supposed to work on all platforms on both GNU Emacs
;;; and XEmacs at least as of version 21.2.1 and 21.4,
;;; respectively. Please speak up if it doesn't on your platform or
;;; recent-ish version of an Emacs of your choice

;;; Commentary:

;;; This mode is built with help of the
;;; "Emacs language mode creation tutorial" at
;;;
;;;   http://two-wugs.net/emacs/mode-tutorial.html
;;;
;;; which disapeared from the net and is now hosted at
;;;
;;;   http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial
;;;

;;; Usage:

;;; Put this file into your load-path and the following into your ~/.emacs:
;;;
;;;    (require 'pod-mode)
;;;
;;;
;;; To associate pod-mode with .pod files add the following to your ~/.emacs
;;;
;;;    (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
;;;
;;;
;;; To automatically turn on font-lock-mode add the following to your ~/.emacs
;;;
;;;    (add-hook 'pod-mode-hook 'font-lock-mode)
;;;
;;;
;;; In addition to the standard POD commands, custom commands as
;;; defined by a Pod::Weaver configuration are supported. However, for
;;; those to work, eproject.el as available at
;;; http://github.com/jrockway/eproject is required.
;;;
;;; Make sure to require eproject.el or create an autoload for
;;; eproject-maybe-turn-on if you expect custom commands to work.
;;;
;;;
;;; When automatically inserting hyperlink formatting codes to modules
;;; or sections within modules, autocompletion for module names will
;;; be provided if perldoc.el, as available at
;;; git://gaffer.ptitcanardnoir.org/perldoc-el.git, is present.
;;;

;;; Code:

(require 'cl)

(defgroup pod-mode nil
  "Mode for editing POD files"
  :group 'faces)

(defgroup pod-mode-faces nil
  "Faces for highlighting POD constructs"
  :prefix "pod-mode-"
  :group 'pod-mode)

(defface pod-mode-command-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to highlight POD commands"
  :group 'pod-mode-faces)

(defface pod-mode-head-face
  '((t (:inherit pod-mode-command-face)))
  "Face used to highlight =head commands"
  :group 'pod-mode-faces)

(defface pod-mode-command-text-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark))
     )
    (t (:weight bold :slant italic)))
  "Face used to highlight text after POD commands"
  :group 'pod-mode-faces)

(defface pod-mode-verbatim-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Face used to highlight verbatim paragraphs in POD"
  :group 'pod-mode-faces)

(defface pod-mode-formatting-code-character-face
  '((((class grayscale) (background light))
     (:foreground "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "sienna"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight light))
    (t (:weight bold :slant italic)))
  "Face used to highlight formatting codes in POD"
  :group 'pod-mode-faces)

(defface pod-mode-formatting-code-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light)) (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Face used to highlight text within formatting codes in POD"
  :group 'pod-mode-faces)

(defface pod-mode-formatting-code-i-face
  '((t (:inherit pod-mode-formatting-code-face :slant italic)))
  "Face used to highlight I<> formatting codes in POD"
  :group 'pod-mode-faces)

(defface pod-mode-formatting-code-b-face
  '((t (:inherit pod-mode-formatting-code-face :weight bold)))
  "Face used to highlight B<> formatting codes in POD"
  :group 'pod-mode-faces)

(defface pod-mode-alternative-formatting-code-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Alternative face used to highlight formatting codes in POD.
This is used for E<> escapes and for the link target in L<>
escapes."
  :group 'pod-mode-faces)

(defface pod-mode-string-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight quoted strings in POD"
  :group 'pod-mode-faces)

(defvar pod-mode-hook nil
  "List of functions to be called when activating `pod-mode'.")

;;; Version: 1.01
(defvar pod-version "1.01"
  "Version of POD mode.")

(let* ((head-sizes '(1.9 1.7 1.5 1.3))
       (heads (loop for i from 1 to (length head-sizes) collect
                    (cons i (nth (- i 1) head-sizes)))))
  (defconst pod-font-lock-keywords-1
    (append
     (loop for (n . s) in heads collect
           (let ((head-face-name (intern (format "pod-mode-head%d-face" n)))
                 (text-face-name (intern (format "pod-mode-head%d-text-face" n))))
             (eval `(defface ,head-face-name
                      '((t (:inherit pod-mode-head-face :height ,s)))
                      ,(format "Face used to highlight head%d commands" n)
                      :group 'pod-mode-faces))
             (eval `(defface ,text-face-name
                      '((t (:inherit pod-mode-command-text-face :height ,s)))
                      ,(format "Face used to hightlight text in head%d commands" n)
                      :group 'pod-mode-faces))
             `(,(format "^\\(=head%d\\)\\(.*\\)" n)
               (1 (quote ,head-face-name))
               (2 (quote ,text-face-name)))))
     `((,(format "^\\(=%s\\)\\(.*\\)"
                 (regexp-opt '("item" "over" "back" "cut" "pod"
                               "for" "begin" "end" "encoding")))
        (1 'pod-mode-command-face)
        (2 'pod-mode-command-text-face))))
    "Minimal highlighting expressions for POD mode."))

(defconst pod-font-lock-keywords-2
  (append pod-font-lock-keywords-1 '())
  "Additional Keywords to highlight in POD mode.")

(defun pod-matcher-for-code (code body)
  "Create a matcher function for a given POD formatting CODE.
Will return a quoted lambda as expected by `font-lock-keywords'
as MATCHER.

When executing the lambda, it will match a POD formatting code
introduced with the character CODE and as described in perlpod.

BODY is expected to be a quoted lambda.  It will be executed
after a successful match of a well-balanced formatting code.
It'll get two arguments, the start and end position of the text
contained in the formatting code.  It should return a list of
positions suitable to use as match data for later highlighting by
`font-lock-keywords'."
  `(lambda (limit)
     (when (re-search-forward
            ,(concat
              code
              "\\(?:\\(?:\\(<\\)[^<]\\)\\|\\(?:\\(<\\{2,\\}\\)\s\\)\\)")
            limit t)
       (let ((beg (or (match-end 1)
                      (match-end 2)))
             (n-lt (length (or (match-string-no-properties 1)
                               (match-string-no-properties 2)))))
         (goto-char (- (point) 1))
         (when (re-search-forward
                (concat (when (> n-lt 1) "\s")
                        "\\("
                        (apply 'concat (loop for i from 1 to n-lt collect ">"))
                        "\\)")
                limit t)
           (let* ((end (match-beginning 1))
                  (match-data (funcall ,body beg end)))
             (when (match-data)
               (store-match-data (append
                                  (list (- beg n-lt 1) beg)
                                  match-data
                                  (list end (+ end n-lt))))
               t)))))))

(defun pod-keyword-for-simple-code (code face)
  "Build a `font-lock-keywords' keyword for a POD formatting code.
CODE is the character introducing the formatting code to be
matched.  FACE is the face that should be used to map the text
within the formattign code.

In addition to matching the code's content with FACE, the
formatting code itself will be highlighted using
`pod-mode-formatting-code-character-face'."
  `(,(pod-matcher-for-code code '(lambda (beg end)
                                   (list beg end)))
    (0 'pod-mode-formatting-code-character-face prepend)
    (1 ',face append)
    (2 'pod-mode-formatting-code-character-face prepend)))

(defconst pod-font-lock-keywords-3
  (append pod-font-lock-keywords-2
          (loop for code in '("C" "F" "X" "Z" "S")
                collect (pod-keyword-for-simple-code
                         code 'pod-mode-formatting-code-face))
          (list
           (pod-keyword-for-simple-code
            "E" 'pod-mode-alternative-formatting-code-face)
           (pod-keyword-for-simple-code "I" 'pod-mode-formatting-code-i-face)
           (pod-keyword-for-simple-code "B" 'pod-mode-formatting-code-b-face)
           `(,(pod-matcher-for-code
               "L" (lambda (beg end)
                     (goto-char beg)
                     (if (re-search-forward "\\([^|]\\)|" end t)
                         (list beg (match-end 1)
                               (+ (match-end 1) 1) end)
                       (list 0 0 beg end))))
             (0 'pod-mode-formatting-code-character-face prepend)
             (1 'pod-mode-formatting-code-face append)
             (2 'pod-mode-alternative-formatting-code-face append)
             (3 'pod-mode-formatting-code-character-face prepend))
           '("\"\\([^\"]+\\)\""
             (0 'pod-mode-string-face))
           '("^[ \t]+\\(.*\\)$" 1 'pod-mode-verbatim-face prepend)))
  "Balls-out highlighting in POD mode.")

(defvar pod-font-lock-keywords pod-font-lock-keywords-3
  "Default highlighting expressions for POD mode.")

(defvar pod-weaver-section-keywords nil
  "List of custom Pod::Weaver keywords describing sections.
This is an alist, mapping strings with pod commands to a number
describing their level within the document.")
(make-local-variable 'pod-weaver-section-keywords)

(defun pod-linkable-sections-for-buffer (buffer &optional section-keywords)
  "Extract POD sections from BUFFER.
Returns a list of POD section names with BUFFER.  By default only
=head commands are looked for.  The optional second argument
SECTION-KEYWORDS may be used to also extract section names from
additional pod commands."
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (loop while (re-search-forward
                     (format "^=%s\s+\\(.*\\)$"
                             (regexp-opt
                              (append
                               (loop for i from 1 to 4
                                     collect (format "head%d" i))
                               '("item")
                               section-keywords)))
                     nil t)
              collect (match-string-no-properties 1))))))

(defun pod-linkable-sections-for-module (module)
  "Extract POD sections from MODULE.
Opens the documentation of an installed perl MODULE and returns a
list of all section names in it.

`pod-linkable-sections-for-buffer' is used to actually extract
the sections."
  (with-current-buffer (get-buffer-create (concat "*POD " module "*"))
    (unwind-protect
        (progn
          (kill-all-local-variables)
          (erase-buffer)
          (text-mode)
          (let ((default-directory "/"))
            (call-process "perldoc" nil (current-buffer) nil "-T" "-u" module)
            (goto-char (point-min))
            (when (and (> (count-lines (point-min) (point-max)) 1)
                       (not (re-search-forward
                             "No documentation found for .*" nil t)))
              (pod-linkable-sections-for-buffer (current-buffer)))))
      (kill-buffer (current-buffer)))))

(defun pod-linkable-sections (&optional module)
  "Extract POD sections.
Extracts all POD sections from either the current buffer, or, if
MODULE is given, from the POD documentation of an installed
module.

If MODULE is given, `pod-linkable-sections-for-module' will be
called.  Otherwise `pod-linkable-sections-for-buffer' for
`current-buffer', and with all additional POD section keywords as
provided by `pod-weaver-section-keywords'."
  (if module
      (pod-linkable-sections-for-module module)
    (pod-linkable-sections-for-buffer
     (current-buffer)
     (mapcar (lambda (i) (car i))
             pod-weaver-section-keywords))))

(defun pod-linkable-modules (&optional re-cache)
  "Find all installed perl modules.
Returns a list of all installed perl modules, as provided by
function `perldoc-modules-alist'.  This requires `perldoc' to be
loadable.

If the optional argument RE-CACHE is non-nil, a possibly cached
version of the module list will be discarded and rebuilt."
  (save-current-buffer
    (when (ignore-errors (require 'perldoc))
      (when (or re-cache (not perldoc-modules-alist))
        (message "Building completion list of all perl modules..."))
      (mapcar (lambda (i) (car i)) (perldoc-modules-alist re-cache)))))

(defun pod-link (link &optional text)
  "Insert a POD hyperlink formatting code.
Inserts a POD L<> formatting code at point.  The content of the
code will be LINK.

If the optional argument TEXT is a string and contains anything
that's not whitespace, it will be used as the link title."
  (insert (concat "L<"
                  (when (and (stringp text)
                             (string-match-p "[^\s]" text))
                    (concat text "|"))
                  link
                  ">")))

(defun pod-completing-read (prompt choices)
  "Use `completing-read' to do a completing read."
  (completing-read prompt choices))

(defun pod-icompleting-read (prompt choices)
  "Use iswitchb to do a completing read."
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (unwind-protect
        (progn
          (when (not iswitchb-mode)
            (add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup))
          (iswitchb-read-buffer prompt))
      (when (not iswitchb-mode)
        (remove-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)))))

(defun pod-ido-completing-read (prompt choices)
  "Use ido to do a completing read."
  (ido-completing-read prompt choices))

(defcustom pod-completing-read-function
  #'pod-icompleting-read
  "Ask the user to select a single item from a list.
Used by `pod-link-section', `pod-link-module', and
`pod-link-module-section'."
  :group 'pod-mode
  :type '(radio (function-item
                 :doc "Use Emacs' standard `completing-read' function."
                 pod-completing-read)
                (function-item :doc "Use iswitchb's completing-read function."
                               pod-icompleting-read)
                (function-item :doc "Use ido's completing-read function."
                               pod-ido-completing-read)
                (function)))

(defun pod-do-completing-read (&rest args)
  "Do a completing read with the configured `pod-completing-read-function'."
  (apply pod-completing-read-function args))

(defun pod-link-uri (uri &optional text)
  "Insert POD hyperlink formatting code for a URL.
Calls `pod-link' with URI and TEXT.

When called interactively, URI and TEXT will be read from the
minibuffer."
  (interactive
   (list (read-string "URI: ")
         (read-string "Text: ")))
  (pod-link uri text))

(defun pod-link-section (section &optional text)
  "Insert hyperlink formatting code for a POD section.
Insert an L<> formatting code pointing to a section within the
current document.

When called interactively, SECTION and TEXT will be read using
`pod-do-completing-read'.

When reading SECTION, `pod-linkable-sections' will be used to
provide completions."
  (interactive
   (list (pod-do-completing-read "Section: " (pod-linkable-sections))
         (read-string "Text: ")))
  (pod-link-module-section "" section text))

(defun pod-link-module (module &optional text)
  "Insert POD hyperlink formatting code for a module.
Insert an L<> formatting code pointing to a MODULE.

When called interactively, MODULE and TEXT will be read using
`pod-do-completing-read'.

When reading MODULE, `pod-linkable-modules' will be used to
provide completions."
  (interactive
   (list (pod-do-completing-read "Module: "
                                 (pod-linkable-modules current-prefix-arg))
         (read-string "Text: ")))
  (pod-link module text))

(defun pod-link-module-section (module section &optional text)
  "Insert POD hyperlink formatting code for a section in a module.
Insert an L<> formatting code pointing to a part of MODULE
documentation as described by SECTION.

When called interactive, MODULE, SECTION, and TEXT will be read
using `pod-do-completing-read'.

When reading MODULE and SECTION, `pod-linkable-modules' and
`pod-linkable-sections', respectively, will be used to provide
completions."
  (interactive
   (let ((module (pod-do-completing-read
                  "Module: "
                  (pod-linkable-modules current-prefix-arg))))
     (list module
           (pod-do-completing-read "Section: " (pod-linkable-sections module))
           (read-string "Text: "))))
  (pod-link
   (concat module
           "/"
           (if (string-match-p "\s" section)
               (concat "\"" section "\"")
             section))
   text))

(defvar pod-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l u") 'pod-link-uri)
    (define-key map (kbd "C-c C-l s") 'pod-link-section)
    (define-key map (kbd "C-c C-l m") 'pod-link-module)
    (define-key map (kbd "C-c C-l M") 'pod-link-module-section)
    map)
  "Keymap for POD major mode.")

(defvar pod-mode-syntax-table
  (let ((st (make-syntax-table)))
    st)
  "Syntax table for `pod-mode'.")

(defun pod-add-support-for-outline-minor-mode (&rest sections)
  "Provides additional menus from section commands for function
`outline-minor-mode'.

SECTIONS can be used to supply section commands in addition to
the POD defaults."
  (make-local-variable 'outline-regexp)
  (setq outline-regexp
        (format "=%s\s"
         (regexp-opt
          (append (loop for i from 1 to 4 collect (format "head%d" i))
                  '("item") sections))))
  (make-local-variable 'outline-level)
  (setq outline-level
        (lambda ()
          (save-excursion
            (save-match-data
              (let ((sect (format "^=%s\s"
                                  (regexp-opt
                                   (mapcar (lambda (i) (car i))
                                           pod-weaver-section-keywords) t))))
                (cond
                 ((looking-at sect)
                  (cdr (assoc (match-string-no-properties 1)
                              pod-weaver-section-keywords)))
                 ((looking-at "^=item\s") 5)
                 ((string-to-number (buffer-substring
                                     (+ (point) 5)
                                     (+ (point) 6)))))))))))

(defun pod-add-support-for-imenu (&rest sections)
  "Set up `imenu-generic-expression' for pod section commands.
SECTIONS can be used to supply section commands in addition to
the POD defaults."
  (setq imenu-generic-expression
        `((nil ,(format "^=%s\s+\\(.*\\)"
                        (regexp-opt
                         (append
                          (loop for i from 1 to 4 collect (format "head%d" i))
                          '("item") sections)))
               1))))

(defun pod-enable-weaver-collector-keywords (collectors)
  "Enable support for Pod::Weaver collector commands.
Enables fontification for all commands described by COLLECTORS.

Also updates `pod-weaver-section-keywords', `outline-regexp', and
`imenu-generic-expression' accordingly."
  (let ((collectors-by-replacement))
    (save-match-data
      (setf pod-weaver-section-keywords
            (loop for col in collectors
                  with cmd with new-cmd with new-name
                  do (progn
                       (setq cmd (getf col 'command)
                             new-cmd (getf col 'new_command)
                             new-name (symbol-name new-cmd))
                       (let ((pos (loop for i in collectors-by-replacement do
                                        (when (equal (car i) new-cmd)
                                          (return i)))))
                         (if (not pos)
                             (push (list new-cmd cmd) collectors-by-replacement)
                           (setcdr (last pos) (list cmd)))))
                  when (string-match "^head\\([1-4]\\)$" new-name)
                  collect (cons (symbol-name cmd)
                                (string-to-number
                                 (match-string-no-properties 1 new-name)))
                  when (string-match "^item$" new-name)
                  collect (cons (symbol-name cmd) 5))))
    (let ((sections (mapcar (lambda (i) (car i))
                            pod-weaver-section-keywords)))
      (apply #'pod-add-support-for-outline-minor-mode sections)
      (apply #'pod-add-support-for-imenu sections))
    (setf
     pod-font-lock-keywords
     (append
      (mapcar (lambda (i)
                (append
                 (list (format "^\\(=%s\\)\\(.*\\)"
                               (regexp-opt (mapcar (lambda (k) (symbol-name k))
                                                   (cdr i)))))
                 (let ((n (symbol-name (car i))))
                   (if (string-match-p "^head[1-4]$" n)
                       (list
                        `(1 (quote
                             ,(intern (format "pod-mode-%s-face" n))))
                        `(2 (quote
                             ,(intern (format "pod-mode-%s-text-face" n)))))
                     (list
                      '(1 'pod-mode-command-face)
                      '(2 'pod-mode-command-text-face))))))
              collectors-by-replacement)
      pod-font-lock-keywords))
    (setq font-lock-mode-major-mode nil)
    (font-lock-fontify-buffer)))

(defun pod-enable-weaver-features (buffer weaver-config)
  "Enable support for Pod::Weaver features.
Enables support for custom Pod::Weaver commands within a BUFFER.

WEAVER-CONFIG is a structure as returned by
\"dzil weaverconf -f lisp\".

Currently only supports collector commands via
`pod-enable-weaver-collector-keywords'."
  (with-current-buffer buffer
    (pod-enable-weaver-collector-keywords (getf weaver-config 'collectors))
    (message "Pod::Weaver keywords loaded.")))

(defun pod-load-weaver-config (dir)
  "Load additional pod keywords from dist.ini/weaver.ini in DIR."
  (let* ((proc (start-process-shell-command
                (concat "weaverconf-" (buffer-name (current-buffer)))
                nil (format "cd %s; dzil weaverconf -f lisp" dir))))
    (set-process-plist proc (list :buffer (current-buffer)
                                  :output ""))
    (set-process-filter
     proc (lambda (proc str)
            (let ((plist (process-plist proc)))
              (plist-put plist :output (concat (plist-get plist :output) str)))))
    (set-process-sentinel
     proc (lambda (proc event)
            (if (string-equal event "finished\n")
                (let* ((plist (process-plist proc))
                       (weaver-config
                        (ignore-errors
                          (eval (car (read-from-string
                                      (plist-get plist :output)))))))
                  (if weaver-config (pod-enable-weaver-features
                                     (plist-get (process-plist proc) :buffer)
                                     weaver-config))))))))

(defun pod-add-support-for-weaver ()
  "Enable support for Pod::Weaver features in the current buffer.
Calls `pod-load-weaver-config' with the project directory of the
current buffer's file.  To be able to successfully determine the
project directory, `eproject-maybe-turn-on' will be used and
'eproject.el' is expected to be loaded.

Does nothing if finding the project directory fails."
  (let ((project-root (ignore-errors (eproject-maybe-turn-on))))
    (if project-root (pod-load-weaver-config project-root))))

;;;###autoload
(defun pod-mode ()
  "Major mode for editing POD files (Plain Old Documentation for Perl).

Commands:\\<pod-mode-map>
\\[pod-link]  `pod-link'
\\[pod-link-section]     `pod-link-section'
\\[pod-link-module]     `pod-link-module'
\\[pod-link-module-section]     `pod-link-module-section'

Turning on pod mode calls the hooks in `pod-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table pod-mode-syntax-table)
  (use-local-map pod-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pod-font-lock-keywords 't))
  (setq major-mode 'pod-mode)
  (setq mode-name "POD")
  (pod-add-support-for-imenu)
  (pod-add-support-for-outline-minor-mode)
  (run-hooks 'pod-mode-hook)
  (pod-add-support-for-weaver))

(provide 'pod-mode)

;;; pod-mode.el ends here
