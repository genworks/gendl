;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is code for looking up entries in the Allegro Common Lisp
;;; documentation.  It is based on hyperspec.el, which was created by
;;; Erik Naggum and others. Kevin Layer @ Franz, Inc. got this code from
;;; Robert P. Goldman.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copright 2006 by SIFT, LLC and Robert P. Goldman.  License terms
;;; as for the original hyperspec.el (GNU GPL).

;;; ORIGINALLY:
;;; hyperspec.el --- Browse documentation from the Common Lisp HyperSpec

;; Copyright 1997 Naggum Software

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: lisp

;; This file is not part of GNU Emacs, but distributed under the same
;; conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'browse-url)                   ;you need the Emacs 20 version
(require 'thingatpt)
(fi::load "fi-manual-data")

(defvar fi::manual-base-url
    "http://www.franz.com/support/documentation/current/"
  "The root URL of the ACL documentation.
If you copy the ACL manual to your local system, set this variable to
something like \"file:/usr/local/acl/acl81/doc/\".")

(defvar fi:manual-history nil
  "History of symbols looked up in the ACL manual.")

(defvar fi::manual-symbols nil
  ;; An obarray of the symbols, their value being the relative locations in
  ;; the documentation.
  )

(defun fi::symbol-sans-package (symbol-name)
  "Remove the package prefix from a symbol name, if present."
  (cond
   ((string-match ":+" symbol-name)
    (substring symbol-name (match-end 0)))
   (t symbol-name)))

(defun fi:manual (symbol-name)
  "View the documentation on SYMBOL-NAME from the Allegro Common Lisp manual.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions, or it may be that the pages
open in different tabs."
  (interactive (list (let ((symbol-at-point (thing-at-point 'symbol)))
		       (when symbol-at-point
			 (setq symbol-at-point
			   (fi::symbol-sans-package symbol-at-point)))
                       (if (and symbol-at-point
                                (intern-soft (downcase symbol-at-point))
				fi::manual-symbols)
                           symbol-at-point
                         (completing-read
                          "Allegro CL documentation lookup: "
                          fi::manual-symbols #'boundp
                          t symbol-at-point
                          'fi:manual-history)))))
  (maplist (lambda (entry)
             (browse-url (concat fi::manual-base-url (car entry)))
             (when (cdr entry)
	       ;; How was this determined, and for what browser?
	       ;; Originally was 1.5 seconds.  Trying .5.
	       (sleep-for 0.5)))
           (let ((symbol (intern-soft (downcase symbol-name)
                                      fi::manual-symbols)))
             (if (and symbol (boundp symbol))
                 (symbol-value symbol)
               (error "The symbol `%s' is not documented in the ACL manual set"
                      symbol-name)))))

(progn
  (setq fi::manual-symbols (make-vector (length +table-from-franz+) 0))
  (mapc (lambda (entry)
	  (let ((symbol (intern (car entry) fi::manual-symbols)))
	    (cond
	     ((boundp symbol)
	      (push (cadr entry) (symbol-value symbol)))
	     (t (set symbol (cdr entry))))))
	+table-from-franz+)
  fi::manual-symbols)
		
(provide 'fi-manual)
