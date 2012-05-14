;;; mutt-alias.el --- Lookup/insert mutt mail aliases.
;; Copyright 1999-2008 by Dave Pearson <davep@davep.org>
;; $Revision: 1.3 $

;; mutt-alias is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; mutt-alias allows you to lookup and insert the expansion of mutt mail
;; aliases. This is only handy if you use mutt <URL:http://www.mutt.org/>.

;;; TODO:
;;
;; o No attempt is made to handle aliases in aliases.
;; o No attempt is made to handle line continuation.

;;; Code:

;; Things we need:

(require 'cl)

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup mutt-alias nil
  "Lookup mutt mail aliases."
  :group  'mail
  :prefix "mutt-alias-")

(defcustom mutt-alias-file-list '("~/.mutt/aliases")
  "*List of files that contain your mutt aliases."
  :type  '(repeat (file :must-exist t))
  :group 'mutt-alias)

(defcustom mutt-alias-cache t
  "*Should we cache the aliases?"
  :type  '(choice (const :tag "Always cache the alias list" t)
                  (const :tag "Always re-load the alias list" nil))
  :group 'mutt-alias)

;; Non-customize variables.

(defvar mutt-alias-aliases nil
  "\"Cache\" of aliases.")

;; Main code:

(defun mutt-alias-load-aliases ()
  "Load aliases from files defined in `mutt-alias-file-list'.

The resulting list is an `assoc' list where the `car' is a string
representation of the alias and the `cdr' is the expansion of the alias.
Note that no attempt is made to handle aliases-in-expansions or continued
lines."
  (unless (and mutt-alias-aliases mutt-alias-cache)
    (with-temp-buffer
      (loop for file in mutt-alias-file-list do (insert-file-contents file))
      (setf (point) (point-min))
      (setq mutt-alias-aliases
            (loop while (search-forward-regexp "^[ \t]*alias +" nil t)
                  collect (mutt-alias-grab-alias)))))
  mutt-alias-aliases)

(defun mutt-alias-grab-alias ()
  "Convert an alias line into a cons.

The resulting `cons' has a `car' that is the alias and the `cdr' is the
expansion. Note that no attempt is made to handle continued lines."
  (let ((old-point (point))
        (end-point)
        (alias)
        (expansion))
    (end-of-line)
    (setq end-point (point))
    (setf (point) old-point)
    (search-forward-regexp "[ \t]" nil t)
    (setq alias (buffer-substring-no-properties old-point (1- (point))))
    (search-forward-regexp "[^ \t]" nil t)
    (setq expansion (buffer-substring-no-properties (1- (point)) end-point))
    (setf (point) old-point)
    (cons alias expansion)))

(defun mutt-alias-expand (alias)
  "Attempt to expand an alias."
  (let ((expansion (assoc alias (mutt-alias-load-aliases))))
    (when expansion
      (cdr expansion))))

(put 'mutt-alias-interactive 'lisp-indent-function 3)

(defmacro mutt-alias-interactive (name alias expansion doc &rest body)
  "Generate a function that asks for an alias.

The alias is placed into variable named by ALIAS and places it into the
variable named by EXPANSION. If there is an expansion BODY will be evaluated
otherwise an error is reported. The function will be given a doc string of
DOC."
  `(defun ,name (,alias) ,doc
     (interactive (list (completing-read "Alias: " (mutt-alias-load-aliases))))
     (let ((,expansion (mutt-alias-expand ,alias)))
       (if ,expansion
           (progn
             ,@body)
         (error "Unknown alias \"%s\"" ,alias)))))

(mutt-alias-interactive mutt-alias-insert alias expansion
  "Insert the expansion for ALIAS into the current buffer."
  (insert expansion))

(mutt-alias-interactive mutt-alias-lookup alias expansion
  "Lookup and display the expansion for ALIAS."
  (message "%s: %s" alias expansion))

(provide 'mutt-alias)

;;; mutt-alias.el ends here
