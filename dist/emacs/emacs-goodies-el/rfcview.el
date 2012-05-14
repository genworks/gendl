;;; rfcview.el -- view IETF RFCs with readability-improved formatting

;; Copyright (C) 2001-2002 Neil W. Van Dyke
;; Copyright (C) 2006, 2008 Free Software Foundation, Inc.
;;    (mods by Dave Love <fx@gnu.org>)

;; Author:   Neil W. Van Dyke <neil@neilvandyke.org>
;; Author: Dave Love <fx@gnu.org>
;; Version:  0.12
;; X-URL:    http://www.loveshack.ukfsn.org/emacs/rfcview.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; [Van Dyke's original is GPL 2 or later.]

;;; COMMENTARY:

;; Introduction:
;;
;;   For historical reasons, IETF Internet RFCs are required to be in a plain
;;   ASCII text format that's best-suited for sending directly to a 6-lpi
;;   US-letter-size printer.  This makes them suboptimal for viewing on-screen,
;;   as you will be doing for countless hours if you're ever doing network
;;   programming to one of them.  Fortunately, the ASCII format is usually
;;   close to what you, the Emacs zealot, *truly* want -- which is a format
;;   suited to more pleasurably viewing the RFC in Emacs.
;;
;;   The `rfcview' package uses Emacs overlays to add some fontification and
;;   hide the page headers and footers (which it replaces with one-line page
;;   number references that look like "(p.1)", right-justified).  The file is
;;   never modified, and you can see the raw ASCII text by pressing `t'.

;; System Requirements:
;;
;;   The `rfcview.el' package was first written using FSF GNU Emacs 20.7 on a
;;   GNU/Linux system, and is now maintained under Emacs 21.4.  It should work
;;   with recent Emacs versions on Unix variants.  `rfcview.el' has not yet
;;   been tested with the XEmacs fork of Emacs, and I'd welcome any necessary
;;   patches.

;; Installation:
;;
;;   1. Put this `rfcview.el' file somewhere in your Emacs Lisp load path.
;;
;;   2. Add the following lines to your `~/.emacs' file:
;;
;;          (add-to-list 'auto-mode-alist
;;                       '("/\\(rfc\\|std\\)[0-9]+\\.txt\\'" . rfcview-mode))
;;
;;          (autoload 'rfcview-mode "rfcview" nil t)
;;
;;      The next time you visit an RFC file, it should be
;;      displayed prettily using `rfcview-mode'.  (Do this before turning
;;      on `auto-compression-mode', so that the `.gz' extension comes before
;;      `.txt' in `auto-mode-alist'; then compressed RFCs will work too.)
;;
;;   4. Optionally, do `M-x rfcview-customize RET' to customize the mode
;;      options.

;; Things for the Author to Someday Do (but Probably Not):
;;
;;   * RFC 1700 (STD 2) has unnumbered headings and column-zero body text.  We
;;     don't try to cope right now, but we might assume, e.g., that lines in
;;     all-caps with preceding and succeeding blank lines are headings.
;;
;;   * Hide extraneous blank lines.
;;
;;   * Handle "Table of Contents" heading centered, such as in RFC 1035 and RFC
;;     1157.
;;
;;   * Display bibliographic references in other-window
;;     vertically-sized to fit only the reference (or min window height).
;;
;;   * Download RFCs on demand, and cache them.  Probably integrate one of the
;;     existing one or two packages that do this.
;;
;;   * Make an RFCedit mode.
;;
;;   * Handle multi-line heading like:
;;
;;         19.6.1.1 Changes to Simplify Multi-homed Web Servers and Conserve IP
;;         Addresses
;;
;;   * Have a stack for (internal) hyperlinks a la Info.
;;
;;   * Deal with an index (e.g. RFC 3986).

;;; CHANGE LOG:

;; [Version 0.11, 2008-02-02]  (Dave Love)
;; * rfcview-find-rfc.
;;
;; [Version 0.10, 2008-01-28]  (Dave Love)
;; * Fix rfcview-find-location-of-rfc-mouse interactive spec.
;; * Get speedbar working.
;; * Allow list of alternative locations for RFCs.
;;
;; [Version 0.9, 2007-10-14]  (Dave Love)
;; * Fix view-mode require and fix overlay type for reference.
;;
;; [Version 0.8, 2007-04-25]  (Dave Love)
;; * Fix rfcview-overriding-map; modify rfcview-headlink-face for dark b/g.
;;
;; [Version 0.7, 2006-10-01]  (Dave Love)
;; * Use ange-ftp, not browse-url;
;; * Handle STDs as well as RFCs.
;;
;; [Version 0.6, 2006-07-07] Hyperlinking, imenu (Dave Love).
;;
;; [Version 0.5, 15-Oct-2002] Updated email address.
;;
;; [Version 0.4, 26-Feb-2002]
;; * Added `rfcview-use-view-mode-p' feature (suggested by Andreas Fuchs).
;; * Added `.gz' handling to `auto-mode-alist' example for people whose Emacs
;;   auto-decompression features don't strip the compression extension before
;;   doing the `auto-mode-alist' lookup.  (thanks to Andreas Fuchs)
;;
;; [Version 0.3, 22-Feb-2002]
;; * Added autoload cookie (suggested by Daniel Pittman).

;; [Version 0.2, 22-Feb-2002]
;; * Tweaks to support some Internet-Drafts.
;; * In heading patterns, `.' is now optional after single-integer heading
;;   numbers, but remains mandatory after alphabetic (appendix) section
;;   numbers.
;; * Hides carriage return characters (which is already done in some Emacs
;;   configurations, but not in others).

;; [Version 0.1, 17-Mar-2001] Initial release.  Note that there's some
;; hyperlink-related code, but it's not finished, so pretend it's not there --
;; but the static reformatting stuff works and is useful, and I can't spend any
;; more time on this package in the near future, so I'm releasing the package
;; now.

;;; CODE:

(require 'goto-addr)
(require 'view)

;; Customization:

(defgroup rfcview nil
  "View IETF RFC files with formatting."
  :group  'hypermedia
  :prefix "rfcview-")

(defcustom rfcview-mode-hook nil
  "Hook variable for `rfcview-mode'."
  :group 'rfcview
  :type  'hook)

(defcustom rfcview-use-view-mode-p t
  "If non-nil, start `view-mode' when `rfcview-mode' is started."
  :group 'rfcview
  :type  'boolean)

;; Note that this is also defined by `ffap-rfc-path', though Emacs
;; 21's value of that is wrong, and we probably don't want to require
;; ffap.
;; Fixme: This should be a path, e.g. local directory plus rfc-editor site.
(defcustom rfcview-rfc-location-pattern
  "/ftp@ftp.rfc-editor.org:/in-notes/rfc%s.txt"
  "Pattern to generate the location of a numbered RFC.
Must contain a single `%s' to be substituted with the RFC's number.
On a Debian-style system, with the doc-rfc packages installed, this could be
\"/usr/share/doc/RFC/links/rfc%s.txt.gz\" to read local copies.
A list of such patterns is also valid; its elements are tried in order
to find the RFC.  Typically you want to try a local directory first and
then the IETF site.  If you have installed suitable file handlers, e.g.
with `url-handler-mode', you can use arbitrary URL patterns.
See also `rfcview-std-location-pattern'."
  :type '(choice string (repeat string))
  :group 'rfcview)

(defcustom rfcview-std-location-pattern
  "/ftp@ftp.rfc-editor.org:/in-notes/std/std%s.txt"
  "Pattern to generate the location of a numbered STD.
Must contain a single `%s' to be substituted with the STD's number.
A list of such patterns is also valid; its elements are tried in order
to find the RFC.  Typically you want to try a local directory first and
then the IETF site.  See also `rfcview-rfc-location-pattern'."
  :type '(choice string (repeat string))
  :group 'rfcview)

(defcustom rfcview-index-location
  (if (consp rfcview-rfc-location-pattern)
      (mapcar (lambda (elt)
		(replace-regexp-in-string "%s.txt" "-index.txt" elt))
	      rfcview-rfc-location-pattern)
    (replace-regexp-in-string "%s.txt" "-index.txt"
			      rfcview-rfc-location-pattern))
  "Location, or list of locations in which to find the RFC index.
The index is usually rfc-index.txt in the RFC directory."
  :group 'rfcview
  :type '(choice string (repeat string))
  :set-after '(rfcview-rfc-location-pattern))

(defface rfcview-title-face
  '((t (:bold t)))
  "Face used for titles."
  :group 'rfcview)

(defface rfcview-headname-face
  '((t (:bold t :underline t)))
  "Face used for heading names."
  :group 'rfcview)

(defface rfcview-headnum-face
  '((t (:bold t)))
  "Face used for heading numbers."
  :group 'rfcview)

(defface rfcview-headlink-face
  '((((type tty pc) (class color)) (:foreground "blue" :weight light))
    (((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:bold t)))
  "Face used for hyperlinks to headings."
  :group 'rfcview)

(defface rfcview-mouseover-face
  '((t (:inherit highlight)))
  "Face used for mousing over a hyperlink."
  :group 'rfcview)

(defface rfcview-rfcnum-face
  '((t (:bold t)))
  "Face used for RFC number in the header."
  :group 'rfcview)

(defface rfcview-stdnum-face
  '((t (:bold t)))
  "Face used for STD number in the header."
  :group 'rfcview)

;; Global Variables:

(defvar rfcview-debug-show-hidden-p nil)

(defvar rfcview-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "t" 'rfcview-textmode)
    (define-key km "q" 'rfcview-quit)
    (define-key km "\t" 'rfcview-next-button)
    (easy-menu-define rfcview-mode-menu km
      "Menu for RFCview."
      '("RFCview"
	["Quit"      rfcview-quit     t]
	["Text Mode" rfcview-textmode t]
	["Next Button" rfcview-next-button t]))
    km))

(defvar rfcview-stock-section-names
  '("abstract"
    "acknowledgement"
    "acknowledgements"
    "acknowledgment"
    "acknowledgments"
    "appendices"
    "author's address"
    "authors' addresses"
    "bibliography"
    "chair's address"
    "copyright notice"
    "copyright statement"
    "editor's address"
    "editors' addresses"
    "full copyright notice"
    "full copyright statement"
    "iesg note"
    "index"
    "introduction"
    "references and bibliography"
    "references"
    "security considerations"
    "status of this memo"
    "table of contents"
    "informative references"
    "normative references"))

(defvar rfcview-headlink-ovlcat nil)
(defvar rfcview-headname-ovlcat nil)
(defvar rfcview-headnum-ovlcat  nil)
(defvar rfcview-hide-ovlcat     nil)
(defvar rfcview-pagenum-ovlcat  nil)
(defvar rfcview-title-ovlcat    nil)

;; Buffer-Local Variables:

(defvar rfcview-local-heading-alist nil)

(defvar rfcview-ref-alist nil
  "Alist of RFC references `(<reference> . <position>)'.")

;; Functions:

(defun rfcview-add-overlay (begin end category)
  (unless category (error "rfcview-add-overlay nil category"))
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'category category)
    overlay))

;;;###autoload
(defun rfcview-customize ()
  "Enter the RFCview Custom group."
  (interactive)
  (customize-group 'rfcview))

(defun rfcview-grok-buffer ()
  "Add overlays to the buffer to modify its presentation."
  (interactive)
  (let ((case-fold-search nil)
        (top-point        (point-min))
        (title-line-point nil))
    
    ;; Clean up everything.
    (rfcview-remove-all-overlays)
    (make-local-variable 'rfcview-local-heading-alist)
    (setq rfcview-local-heading-alist '())

    ;; Hide any CRs.
    (goto-char (point-min))
    (while (re-search-forward "\r+" nil t)
      (rfcview-hide-region (match-beginning 0) (match-end 0)))

    ;; Add hiding overlay for whitespace at start of file, and set `top-point'
    ;; to just after it.
    (goto-char (point-min))
    (when (re-search-forward "\\`\\([ \t\f]*\r?\n\\)+" nil t)
      (rfcview-hide-region (match-beginning 0) (match-end 0))
      (setq top-point (point)))
    
    ;; Add overlays for page headers and footers.
    (let ((headerfooter-re (concat "^[ \t]*"
                                   "\\(\r?\n\\)"         ; #1
                                   "\\([ \t]*\r?\n\\)*"  ; #2
                                   "[^ \t\f].*\\[Page "
                                   "\\([0-9iIvVxX]+\\)"  ; #3
                                   "\\][ ]*\r?\n?"
                                   "\\("                 ; <#4
                                   "\f"
                                   "\\([ \t]*\r?\n\\)?"  ; #5
                                   "\\("                 ; <#6
                                   "\\("                 ; <#7
                                   "RFC [0-9]+"
                                   "\\|"                 ; |#7
                                   "Internet-Draft[ \t]"
                                   "\\)"                 ; >#7
                                   ".*\r?\n"
                                   "\\([ \t]*\r?\n\\)*"  ; #8
                                   "\\)?"                ; >#6
                                   "\\)?"                ; >#4
                                   )))
      (while (re-search-forward headerfooter-re nil t)
        (rfcview-hide-region (match-end 1) (match-end 0))
        (when (match-beginning 6)
          (let ((overlay  (rfcview-add-overlay (match-beginning 1)
                                               (match-end 1)
                                               'rfcview-pagenum-ovlcat))
                ;; Note: If we wanted to do this right, we would save a marker
                ;;       and then backpatch once we see the next page footer.
                (page-str (format
                           "(p.%s)"
                           (let ((n (string-to-number (match-string 3))))
                             (if (= n 0) "?" (1+ n))))))
            (overlay-put overlay
                         'before-string
                         (concat (make-string (max (- 79
                                                      (- (match-beginning 1)
                                                         (match-beginning 0))
                                                      (length page-str))
                                                   0)
                                              32)
                                 page-str))))))

    ;; Find the first blank line (which should be between top headers and
    ;; before title), remember point, and hide any extraneous blank lines.
    (goto-char top-point)
    (unless (re-search-forward (concat "^[ \t]*\r?\n"
                                       "\\(\\([ \t]*\r?\n\\)+\\)?")
                               nil t)
      (error "This doesn't seem to be an RFC - no blank line before title"))
    (when (match-beginning 1)
      (rfcview-hide-region (match-beginning 1) (match-end 1)))
    (setq title-line-point (point))

    ;; Add overlay for the RFC number.
    (goto-char top-point)
    (when (let ((case-fold-search t))
            (re-search-forward "^request for comments:[ \t]+\\([0-9X]+\\)"
                               title-line-point t))
      (rfcview-add-overlay (match-beginning 1)
                           (match-end 1)
                           'rfcview-rfcnum-ovlcat))

    ;; Add overlay for the STD number.
    (goto-char top-point)
    (when (let ((case-fold-search nil))
            (re-search-forward "^STD:[ \t]+[0-9]+"
                               title-line-point t))
      (rfcview-add-overlay (match-beginning 0)
                           (match-end 0)
                           'rfcview-stdnum-ovlcat))

    ;; Add overlays to the title line(s).  Note that we currently assume no
    ;; blank lines in the title; otherwise we have to do a perfect job of
    ;; identifying the first non-title line (usually a section heading, which
    ;; some some RFCs make difficult to always identify).
    (goto-char title-line-point)
    (if (re-search-forward (concat
                            "\\([^ \t\f\r\n].*[^ \t\f\r\n]\\)"
                            "\\(\r?\n[ \t]*[^ \t\f\r\n].*[^ \t\f\r\n]\\)*"))
        (rfcview-add-overlay (match-beginning 0)
                             (match-end       0)
                             'rfcview-title-ovlcat))

    ;; Find all the headings.  Add overlays for them, and build
    ;; `rfcview-local-heading-alist'.
    (goto-char title-line-point)
    (let ((case-fold-search t)
          ;; Note: We can't just look for lines that begin in column 0, since
          ;; some RFCs put source code, ASCII-art, description list headings,
          ;; body text, and other stuff in column 0.  So we look for stock
          ;; headings and ones that appear to begin with section numbers.
          (heading-re (concat
                       "^"
                       "\\("                     ; <#1
                       "\\("                     ; <#2 = numbered section
                       "\\("                     ; <#3 = number
                       "\\([0-9]+\\.?\\|[A-Z]\\.\\)" ; #4
                       "\\([0-9]+\\.?\\)*"       ; #5
                       "\\)"                     ; >#3 = number
                       "[ \t]+"
                       "\\([^\r\n]+\\)"          ; #6 = name
                       "\\)"                     ; >#2 = numbered section
                       "\\|"                     ; |#1
                       "\\("                     ; <#7 = stock section
                       "\\("                     ; <#8
                       (mapconcat 'identity rfcview-stock-section-names "\\|")
                       "\\)"                     ; >#8
                       ":?[ \t]*$"
                       "\\)"                     ; >#7 = stock section
                       "\\|"                     ; |#1
                       "\\("                     ; <#9 = lit-appendix
                       
                       "appendix[ \t]+"
                       "\\([A-Z]\\)"             ; #10 = number
                       
                       "\\(\\.\\|:\\|[ \t]+-\\)" ; #11
                       "[ \t]+"
                       "\\([^\r\n]+\\)"          ; #12 = name

                       "\\)"                     ; >#9 = lit-appendix
                       "\\)"                     ; >#1
                       )))
      (while (re-search-forward heading-re nil t)
        (let ((num-match           nil)
              (num-highlight-begin nil)
              (num-highlight-end   nil)
              (name-match          nil))
          ;; Get the match data numbers.
          (cond ((match-beginning 3) (setq num-match  3
                                           name-match 6))
                ((match-beginning 8) (setq num-match  nil
                                           name-match 8))
                ((match-beginning 9) (setq num-match  10
                                           name-match 12)
                 (setq num-highlight-begin (match-beginning 9)
                       num-highlight-end   (match-end       11)))
                (t (error "This should never happen")))

          ;; Add overlays.
          (when num-match
            (rfcview-add-overlay (or num-highlight-begin
                                     (match-beginning num-match))
                                 (or num-highlight-end
                                     (match-end       num-match))
                                 'rfcview-headnum-ovlcat))
          (rfcview-add-overlay (match-beginning name-match)
                               (match-end       name-match)
                               'rfcview-headname-ovlcat)
          ;; Prepend the `rfcview-local-heading-alist' entry.
          (setq rfcview-local-heading-alist
                (let ((num  (when num-match
                              (upcase (match-string-no-properties num-match))))
                      (name (match-string-no-properties name-match)))
                  (cons (cons (downcase (or num name))
                              (vector
                               num
                               name
                               (match-beginning 0)
                               (match-end 0)))
                        rfcview-local-heading-alist))))))
    ;; Reverse `rfcview-local-heading-alist'.
    (setq rfcview-local-heading-alist (nreverse rfcview-local-heading-alist))

    ;; Hyperlink the contents and references
    (rfcview-hyperlink-contents)
    (rfcview-hyperlink-refs)

    ;; Hyperlink URLs.  `goto-address-fontify-maximum-size' is only
    ;; 30000 by default.
    (let ((goto-address-fontify-maximum-size (point-max))
	  (goto-address-highlight-p t)
	  (goto-address-mail-regexp "\\<\\>")) ; don't match emails
      (goto-address))

    ;; Leave the point at the visible top of the buffer.
    (goto-char top-point))
  
  (message "This RFC has been reformatted for viewing in Emacs."))

(defun rfcview-hide-region (start end)
  (rfcview-add-overlay start end 'rfcview-hide-ovlcat))

;; Hyperlinking

(defun rfcview-imenu-index-function ()
  "`imenu-create-index-function' for RFCview."
  (mapcar (lambda (elt)
	    (setq elt (cdr elt))
	    (let ((num (aref elt 0))
		  (head (aref elt 1))
		  (pos (aref elt 2)))
	      (cons (if num
			(concat num " " head)
		      head)
		    pos)))
	  rfcview-local-heading-alist))

(defun rfcview-link-add-headlink (start end pos)
  (let ((overlay (rfcview-add-overlay start end 'rfcview-headlink-ovlcat)))
    (overlay-put overlay 'rfcview-link (list 'head pos))
    overlay))

(defun rfcview-link-add-headlink-for (start end key)
  (let ((vec (cdr (assoc (downcase key) rfcview-local-heading-alist))))
    (when vec
      (rfcview-link-add-headlink start end (aref vec 2)))))

(defun rfcview-hyperlink-contents ()
  "Find table of contents and hyperlink the entries to headers."
  (let* ((elt (assoc "table of contents" rfcview-local-heading-alist))
	 (start (if elt (aref (cdr elt) 3)))
	 (next (cadr (member elt rfcview-local-heading-alist)))
	 (end (if next (aref (cdr next) 2)))
	 (case-fold-search t))
    (when (and start end)
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (dolist (elt rfcview-local-heading-alist)
	    (let ((key (car elt)))
	      (when (re-search-forward (concat "^ *\\(" (regexp-quote key)
					       "\\) ")
				       nil t)
		(rfcview-link-add-headlink-for (match-beginning 1)
					       (line-end-position)
					       key)
		(end-of-line)))))))))

(defvar rfcview-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'rfcview-goto-link-mouse)
    map)
  "Keymap for use on link overlays.")

(defvar rfcview-overriding-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'rfcview-maybe-goto-link)
    (set-keymap-parent map view-mode-map)
    map)
  "Keymap binding RET to override the View mode binding.")

(defun rfcview-maybe-goto-link ()
  "Follow link if on one, else use normal binding of RET.
Push mark if on a link."
  (interactive)
  (or (rfcview-goto-link)
      (rfcview-find-location-of-rfc)
      (if (get-char-property (point) 'goto-address) ; URL found by goto-addr
	  (goto-address-at-point))
      ;; Use the binding that's presumably from View mode:
      (let ((minor-mode-overriding-map-alist nil))
	(call-interactively (key-binding [?\C-m])))))

(defun rfcview-goto-link ()
  "If on a link, go to target, push mark, and return non-nil.
Else return nil."
  (interactive)
  (let ((pos (cadr (get-char-property (point) 'rfcview-link))))
    (when pos
      (push-mark)
      (goto-char pos))))

(defun rfcview-goto-link-mouse (event)
  "Follow a link selected with the mouse EVENT and push mark."
  (interactive "e")
  (mouse-set-point event)
  (rfcview-goto-link))

(defun rfcview-hyperlink-refs ()
  "Find references in appropriate sections and hyperlink them from elsewhere."
  (save-excursion
    ;; Find the references sections, including `Normative
    ;; references' &c.
    (dolist (elt rfcview-local-heading-alist)
      (when (let ((case-fold-search t))
	      (string-match "\\<\\(?:references\\|bibliography\\)\\'"
			    (aref (cdr elt) 1)))
	(let* ((start (aref (cdr elt) 3))
	       (next (cadr (member elt rfcview-local-heading-alist)))
	       (end (if next
			(aref (cdr next) 2)
		      (point-max)))
	       (case-fold-search nil))
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char (point-min))
	    ;; Look for plausible-looking tags (with uppercase
	    ;; letters, numbers or hyphens within brackets).
	    (while (re-search-forward "^ *\\([[][-A-Z0-9]+]\\) " nil t)
	      (push (cons (match-string 1) (match-beginning 1))
		    rfcview-ref-alist)
	      ;; If it looks like an RFC reference, hyperlink it.
	      (let ((start (match-beginning 1))
		    (end (match-end 1))
		    (string (match-string 1)))
		(when (string-match "[[]\\(RFC\\|STD\\)\\([0-9]+\\)]" string)
		  (let ((overlay (make-overlay start end)))
		    (overlay-put overlay 'category 'rfcview-rfcurl-ovlcat)
		    (overlay-put overlay 'location
				 (if (equal "RFC" (match-string 1 string))
				     (mapcar
				      (lambda (x)
					(format x (match-string 2 string)))
				      (if (listp rfcview-rfc-location-pattern)
					  rfcview-rfc-location-pattern
					(list rfcview-rfc-location-pattern)))
				   (mapcar
				    (lambda (x)
				      (format x (match-string 2 string)))
				    (if (listp rfcview-std-location-pattern)
					rfcview-std-location-pattern
				      (list rfcview-std-location-pattern)))))))))))))
    ;; Find and activate references in the body.  Skip if it's at the
    ;; position of a target.
    (goto-char (point-min))
    (while (re-search-forward "\\([[][-A-Z0-9]+]\\)" nil t)
      (let ((elt (assoc (match-string 1) rfcview-ref-alist)))
	(when (and elt (/= (match-beginning 1) (cdr elt)))
	  (overlay-put (rfcview-add-overlay (match-beginning 1) (match-end 1)
					    'rfcview-reflink-ovlcat)
		       'rfcview-link (list 'ref (cdr elt))))))))

(defun rfcview-find-internal (files &optional sort mode)
  "Find the first of FILES which exists.
FILES may be a list or a single file."
  (catch 'found
    (dolist (file files)
      (when (file-exists-p file)
	(let (text-mode-hook)		; don't run Flyspell etc.
	  (find-file file))
	(throw 'found t)))
    (error "%s not found: %s" (or sort "RFC")
	   (mapconcat #'identity files ", ")))
  (if (and mode (not (eq major-mode 'rfcview-mode)))
    (rfcview-mode)))

;;;###autoload
(defun rfcview-find-rfc (number)
  "Find RFC NUMBER and view it in RFcview mode.
Interactively, prompt for the number.
See `rfcview-rfc-location-pattern' for where to search."
  (interactive  (rfcview-prompt-number))
  (rfcview-find-internal (mapcar
			  (lambda (x)
			    (format x number))
			  (if (listp rfcview-rfc-location-pattern)
			      rfcview-rfc-location-pattern
			    (list rfcview-rfc-location-pattern)))
			 nil t))

(defun rfcview-prompt-number ()
  (let* ((n (number-at-point))
	 (val (read-string "RFC number: " (if n (number-to-string n)) nil n)))
    (if (> (length val) 0)
	(list (string-to-number val))
      (error "Missing number"))))

(defun rfcview-find-location-of-rfc ()
  "Browse to the LOCATION of any RFC referenced at point."
  (interactive)
  (rfcview-find-internal (get-char-property (point) 'location) nil t))

(defun rfcview-find-location-of-rfc-mouse (event)
  "Browse to the LOCATION of the RFC reference at the mouse EVENT."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (rfcview-find-location-of-rfc)))

(defvar rfcview-rfc-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'rfcview-find-location-of-rfc-mouse)
    (define-key map [?\C-m] #'rfcview-find-location-of-rfc)
    map)
  "Keymap for links to RFC locations.")

;;;###autoload
(defun rfcview-find-index ()
  "Find the RFC index and hyperlink it."
  (interactive)
  (rfcview-find-internal (if (listp rfcview-index-location)
			     rfcview-index-location
			   (list rfcview-index-location))
			 "RFC index")
  (view-mode)
  (save-excursion
    (goto-char (point-min))
    (when (= (point-max)
	     (next-single-char-property-change (point) 'rfcview-rfcnum-ovlcat))
      (let ((pattern (if (listp rfcview-rfc-location-pattern)
			 rfcview-rfc-location-pattern
		       (list rfcview-rfc-location-pattern))))
      (while (re-search-forward "^\\([0-9]\\{4\\}\\) " nil t)
	(let ((start (match-beginning 1)))
	  (let ((overlay (make-overlay start (line-end-position))))
	    (overlay-put overlay 'category 'rfcview-rfcurl-ovlcat)
	    (overlay-put overlay 'location
			 (mapcar (lambda (x)
				   (format x (match-string 1)))
				 pattern)))
	  (rfcview-add-overlay start (match-end 1)
			       'rfcview-rfcnum-ovlcat)))))))

;; Major mode

;;;###autoload
(defun rfcview-mode ()
  "Major mode for viewing Internet RFCs.

http://www.loveshack.ukfsn.org/emacs/rfcview.el
http://www.neilvandyke.org/rfcview/

Key bindings:
\\{rfcview-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rfcview-mode)
  (setq mode-name "RFCview")
  (use-local-map rfcview-mode-map)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'rfcview-local-heading-alist)
  (setq font-lock-defaults nil)
  ;; Arrange to lose the C-m binding from View mode:
  (push (cons 'view-mode rfcview-overriding-map)
	minor-mode-overriding-map-alist)
  (set (make-local-variable 'imenu-create-index-function)
       'rfcview-imenu-index-function)
  (set (make-local-variable 'imenu-sort-function) nil)
  (make-local-variable 'rfcview-ref-alist)
  (when rfcview-use-view-mode-p
    (view-mode-enter nil #'rfcview-quit))
  (rfcview-grok-buffer)
  ;; This is easier and probably better than inserting contents in the
  ;; mode menu.
  (imenu-add-to-menubar "Contents")
  (run-hooks 'rfcview-mode-hook))

(defun rfcview-put-alist (symbol alist)
  (mapcar (function (lambda (cell)
                      (put symbol (nth 0 cell) (cdr cell))))
          alist))

(defun rfcview-quit (&optional buffer)
  "Kill the RFCview buffer.
Arg BUFFER is ignored."
  (interactive)
  (kill-buffer (current-buffer)))

(defun rfcview-remove-all-overlays ()
  (mapcar (function (lambda (lst)
                      (while lst
                        (delete-overlay (car lst))
                        (setq lst (cdr lst)))))
          (let ((lists (overlay-lists)))
            (list (car lists) (cdr lists)))))

(defun rfcview-textmode ()
  "Remove overlays from the buffer and put it into Text mode."
  (interactive)
  (rfcview-remove-all-overlays)
  (text-mode))

(defun rfcview-next-button ()
  "Move point to the next \"button\" (active link)."
  (interactive)
  (if (get-char-property (point) 'keymap) ; move off it
      (goto-char (next-single-char-property-change (point) 'keymap)))
  (goto-char (next-single-char-property-change (point) 'keymap)))

;; Overlay Categories:

(rfcview-put-alist 'rfcview-hide-ovlcat
                   (if rfcview-debug-show-hidden-p
                       '((face       . region)
                         (intangible . nil)
                         (invisible  . nil))
                     '((face       . default)
                       (intangible . t)
                       (invisible  . t))))

(rfcview-put-alist 'rfcview-headname-ovlcat '((face . rfcview-headname-face)))
(rfcview-put-alist 'rfcview-headnum-ovlcat  '((face . rfcview-headnum-face)))
(rfcview-put-alist 'rfcview-rfcnum-ovlcat   '((face . rfcview-rfcnum-face)))
(rfcview-put-alist 'rfcview-stdnum-ovlcat   '((face . rfcview-stdnum-face)))
(rfcview-put-alist 'rfcview-title-ovlcat    '((face . rfcview-title-face)))

(rfcview-put-alist 'rfcview-headlink-ovlcat
                   `((face       . rfcview-headlink-face)
                     (mouse-face . rfcview-mouseover-face)
		     (keymap . ,rfcview-link-map)
		     (help-echo . "mouse-2, C-m: go to section")))
(rfcview-put-alist 'rfcview-reflink-ovlcat
                   `((face       . rfcview-headlink-face)
                     (mouse-face . rfcview-mouseover-face)
		     (keymap . ,rfcview-link-map)
		     (help-echo . "mouse-2, C-m: follow reference")))

(rfcview-put-alist 'rfcview-rfcurl-ovlcat
		   `((face . ,goto-address-url-face)
		     (mouse-face . ,goto-address-url-mouse-face)
		     (help-echo . "mouse-2, C-m: browse RFC's location")
		     (keymap . ,rfcview-rfc-keymap)))

;; This persuades speedbar to use Imenu with RRCs.
(eval-after-load "speedbar"
  '(speedbar-add-supported-extension '("rfc[0-9]+\.txt\.gz" "rfc[0-9]+\.txt")))

;; End:

(provide 'rfcview)

;;; rfcview.el ends here
