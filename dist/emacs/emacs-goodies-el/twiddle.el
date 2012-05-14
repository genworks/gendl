;;; twiddle.el --- mode-line display hack

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Status: Works in Emacs 19 and XEmacs.
;; Created: 1997-03-12

;; $Id: twiddle.el,v 1.1.1.1 2003/04/04 20:16:17 lolando Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Inspired by a similar hack by Jim Blandy <jimb@cyclic.com>.

;; There are two user commands of interest: twiddle-start and twiddle-compile.

;; If you write new twiddles, try to minimize or avoid consing, since those
;; functions are called constantly.

;;; Code:

(eval-and-compile

(defconst twiddle-xemacs-p
  (save-match-data (string-match "XEmacs" (emacs-version))))

(if twiddle-xemacs-p
    (require 'itimer)
  (require 'timer))

) ;; end eval-and-compile

(defvar twiddle-properties nil
  "*Text properties to put on the twiddle text.")

(defconst twiddle-delay 1
  "*Default amount of time between mode line updates, in seconds.
This can be overridden for specific hacks in `twiddle-hacks'.")

(defconst twiddle-default-hack "twiddle"
  "*Default twiddle to run.")

(defconst twiddle-hacks
  '(("twiddle"  twiddle-frob-twiddle   0 " - ")
    ("roll"     twiddle-frob-roll      5 ?\ )
    ("asterisk" twiddle-frob-asterisk 10 ?-))
  "*Twiddle hacks.
This is an alist of hacks, where each member contains the following elts:
  0. A name used for completion by `twiddle-start' and `twiddle-compile'.
  1. A twiddle function.
  2. The length of the twiddle string in the mode line.
  3. The initial char in each position of the twiddle string.
     This may also be a string, in which case the contents of this string
     is used as the initial value and the length parameter is ignored.
  4. (optional) A time delay between mode line updates, in seconds.
     If not specified, the value of `twiddle-delay' is used.
  5. (optional) This and any remaining arguments are passed to the function
     specified in field 1 each time it's called.")

;; Internal twiddle data
(defvar twiddle-mode-string nil)
(defvar twiddle-timer nil)
(defvar twiddle-current-pos 0)
(defvar twiddle-current-saved-char nil)
(defvar twiddle-direction 'identity)
(defvar twiddle-temp nil)

(defconst twiddle-rotate-chars '(?| ?/ ?- ?\\))


;;;###autoload
(defun twiddle-start (&optional hack)
  "Start a mode line display hack.
If called interactively with a prefix argument, prompt for the name of
a hack to run.  If called from lisp, optional argument HACK is the name of
a hack to run.
Named hacks are defined in the table `twiddle-hacks'."
  (interactive (list (and current-prefix-arg (twiddle-read-hack-complete))))
  (or hack (setq hack twiddle-default-hack))
  (let ((hack-data (assoc hack twiddle-hacks)))
    (cond ((null hack-data)
           (if hack
               (error "Unknown twiddle hack: %s" hack)
             (error "No twiddle hack specified.")))
          (t (apply 'twiddle-start-twiddling (cdr hack-data))))))

;;;###autoload
(defun twiddle-compile (&rest compile-args)
  "Like \\[compile], but run a twiddle hack during compilation.
If called with a prefix argument, prompt for a specific hack to run."
  (interactive)
  (let* ((hack (if current-prefix-arg
                   (twiddle-read-hack-complete)
                 twiddle-default-hack))
         (hack-data (assoc hack twiddle-hacks)))
    (and (null hack-data)
         (if hack
             (error "Unknown twiddle hack: %s" hack)
           (error "No twiddle hack specified.")))
    (setq hack-data (copy-alist (cdr hack-data)))
    (twiddle-insert hack-data (car hack-data) 3)
    (twiddle-insert hack-data nil 3)
    (setcar hack-data 'twiddle-frob-compile)
    (if (interactive-p)
        (call-interactively 'compile)
      (apply 'compile compile-args))
    ;; Start twiddle after compilation begins, to insure that
    ;; compilation-in-progress has been set.
    (apply 'twiddle-start-twiddling hack-data)))

(defun twiddle-stop ()
  "Stop twiddling."
  (interactive)
  (twiddle-timer-stop twiddle-timer)
  (setq twiddle-timer nil)
  (twiddle-unfrob-mode-line-format)
  (setq twiddle-mode-string nil)
  (twiddle-mode-line-update))

(defun twiddle-start-twiddling (fn len char &optional delay &rest fn-args)
  (twiddle-stop)
  (twiddle-initialize-data len char)
  (twiddle-frob-mode-line-format)
  (setq twiddle-timer
        (apply 'twiddle-timer-start 0 (or delay twiddle-delay) fn fn-args)))

(defun twiddle-initialize-data (len init-char)
  (if (stringp init-char)
      (setq twiddle-mode-string (copy-sequence init-char))
    (setq twiddle-mode-string (make-string len init-char)))
  (setq twiddle-current-pos 0)
  (setq twiddle-current-saved-char
        (aref twiddle-mode-string twiddle-current-pos))
  (setq twiddle-direction '1+)
  (and twiddle-properties
       (boundp 'add-text-properties)
       (add-text-properties 0 len twiddle-properties twiddle-mode-string)))

;; Edit global mode-line-format to include the twiddles.
;; "Destructively" modifies the global mode-line-format list, since XEmacs
;; makes the symbol local in every buffer.
(defun twiddle-frob-mode-line-format ()
  (let* ((format (default-value 'mode-line-format))
         ;; XEmacs 19.14 has "-%-" as the last elt by default.
         (end (or (member "%-" format)
                  (member "-%-" format))))
    (cond (end
           (setcdr end (cons (car end) (cdr end)))
           (setcar end 'twiddle-mode-string))
          (t (nconc format 'twiddle-mode-string)))))

(defun twiddle-unfrob-mode-line-format ()
  (setq-default mode-line-format
		(delq 'twiddle-mode-string
		      (default-value 'mode-line-format))))

;; Insert NEW-ELT in the INDEX position of LIST.
;; LIST is destructively modified.
(defun twiddle-insert (list new-elt index)
  (let ((inspoint (nthcdr index list)))
    (cond ((consp inspoint)
           (setcdr inspoint (cons (car inspoint) (cdr inspoint)))
           (setcar inspoint new-elt))
          ((> index (length list))
           (signal 'error (list "List too short" list new-elt index)))
          (t (nconc list (cons new-elt nil)))))
  list)

(defun twiddle-read-hack-complete ()
  (completing-read "Twiddle hack: " twiddle-hacks nil t twiddle-default-hack))

;; Return a function of no arguments which calls fn with args.
;; The args are quoted to avoid double-evaluation: they are evaluated
;; when passed to twiddle-make-thunk, never afterward.
(defun twiddle-make-thunk (fn args)
  (and (symbolp fn) (setq fn (list 'quote fn)))
  (list 'lambda '() (list 'apply fn (list 'quote args))))

;; Returns the timer object.
(defun twiddle-timer-start (secs repeat function &rest args)
  (cond (twiddle-xemacs-p
         ;; The initial timeout must be greater than zero.
         (and (zerop secs) (setq secs (1+ secs)))
         ;; The XEmacs timer interface doesn't allow one to specify
         ;; arguments to the function to call, but we can work around this
         ;; by wrapping the call in a thunk.
         (start-itimer "twiddle"
                       (if args
                           (twiddle-make-thunk function args)
                         function)
                       secs repeat))
        (t
         (apply 'run-with-timer secs repeat function args))))

(defun twiddle-timer-stop (timer)
  (cond (twiddle-xemacs-p
         (and (itimerp timer)
              (delete-itimer timer)))
        ((timerp timer)
         ;; If this function is called from the timer itself, the timer
         ;; object isn't present on timer-list so cancel-timer won't do
         ;; anything useful.  To work around this case, disable the timer
         ;; repeat so it will expire on its own.
         (timer-set-time timer '(0 0) 0)
         (cancel-timer timer))))

;; Subroutine of twiddle-compile.
(defun twiddle-frob-compile (&optional twiddle-fn &rest args)
  (if compilation-in-progress
      (apply (or twiddle-fn
                 (nth 1 (assoc twiddle-default-hack twiddle-hacks)))
             args)
    (twiddle-stop)))

(defalias 'twiddle-mode-line-update
  (if twiddle-xemacs-p
      'redraw-modeline
    'force-mode-line-update))


;;; Hacks.

(defun twiddle-frob-twiddle ()
  (setq twiddle-current-pos 0)
  (while (< twiddle-current-pos (length twiddle-mode-string))
    (setq twiddle-current-saved-char
          (memq (aref twiddle-mode-string twiddle-current-pos)
                twiddle-rotate-chars))
    (and twiddle-current-saved-char
         (aset twiddle-mode-string twiddle-current-pos
               (or (car (cdr twiddle-current-saved-char))
                   (car twiddle-rotate-chars))))
    (setq twiddle-current-pos (1+ twiddle-current-pos)))
  (twiddle-mode-line-update))

(defun twiddle-frob-roll ()
  (setq twiddle-temp
        (or (car (cdr (memq (aref twiddle-mode-string twiddle-current-pos)
                            twiddle-rotate-chars)))
            (car twiddle-rotate-chars)))
  (cond ((= twiddle-current-pos 0)
         (setq twiddle-direction '1+))
        ((= twiddle-current-pos (1- (length twiddle-mode-string)))
         (setq twiddle-direction '1-)))
  (aset twiddle-mode-string twiddle-current-pos twiddle-current-saved-char)
  (setq twiddle-current-pos (funcall twiddle-direction twiddle-current-pos))
  (setq twiddle-current-saved-char
        (aref twiddle-mode-string twiddle-current-pos))
  (aset twiddle-mode-string twiddle-current-pos twiddle-temp)
  (twiddle-mode-line-update))

(defun twiddle-frob-asterisk ()
  (aset twiddle-mode-string twiddle-current-pos twiddle-current-saved-char)
  (cond ((= twiddle-current-pos 0)
         (setq twiddle-direction '1+))
        ((= twiddle-current-pos (1- (length twiddle-mode-string)))
         (setq twiddle-direction '1-)))
  (setq twiddle-current-pos (funcall twiddle-direction twiddle-current-pos))
  (setq twiddle-current-saved-char
        (aref twiddle-mode-string twiddle-current-pos))
  (aset twiddle-mode-string twiddle-current-pos ?*)
  (twiddle-mode-line-update))

(provide 'twiddle)

;;; twiddle.el ends here.
