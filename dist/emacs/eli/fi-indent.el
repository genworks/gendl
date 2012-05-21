;; This file has its (distant) roots in lisp/lisp-mode.el, so:
;;
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; This file is derived from part of GNU Emacs.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defvar fi:lisp-electric-semicolon nil
  "*If non-nil, semicolons that begin comments are indented as they are
inserted into the buffer.")

(make-variable-buffer-local 'fi:lisp-electric-semicolon)

(defvar fi:lisp-comment-indent-specification '(40 t nil 0)
  "*Specification list for indentations of semicolon comments.
The nth element of the list specifies the indentation for a comment beginning
with n semicolons (e.g. the first element of the list is the indentation for
comments beginning with one semicolon).  Each element of the list may be one
of `t' (indent comment just like an s-expression), `nil' (don't change the
indentation of the comment, i.e. leave the semicolon where it is), a non-
negative integer (specifying the absolute column to which the comment is to
be indented), or a negative integer (specifying a negative offset for the
comment relative to the current column).

NOTE: if the buffer local variable comment-column is changed, then the
first element of fi:lisp-comment-indent-specification is changed to contain
the value of comment-column.")

(make-variable-buffer-local 'fi:lisp-comment-indent-specification)

(defvar fi:lisp-body-indent 2
  "*The indentation of a list continued on another line.")

(make-variable-buffer-local 'fi:lisp-body-indent)

(defvar fi:lisp-indent-offset nil
  "*If non-nil, then indent by a constant amount of the column in which the
sexp starts.")

(make-variable-buffer-local 'fi:lisp-indent-offset)

(defvar fi:lisp-indent-hook-property 'fi:lisp-indent-hook
  "*The indicator for the lisp-indentation hook property of a symbol.
This variable is buffer-local.")

(make-variable-buffer-local 'fi:lisp-indent-hook-property)

(defvar fi:lisp-tag-indentation 1
  "*Indentation of tags relative to containing list.
This variable is used by the function `fi:lisp-indent-tagbody' to indent tags
that occur within special forms whose symbols have a 'fi:lisp-indent-hook
property of 'tag or 'tagbody.  The indentation is relative to the
indentation of the parenthesis enclosing the special form.")

(make-variable-buffer-local 'fi:lisp-tag-indentation)

(defvar fi:lisp-tag-body-indentation 2
  "*Indentation of non-tagged lines relative to containing list.
This variable is used by the function `fi:lisp-indent-tagbody' to indent normal
lines (lines without tags) that occur within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'tag or 'tagbody.  The indentation is
relative to the indentation of the parenthesis enclosing the special form.
If the value is T, the body of tags will be indented as a block at the same
indentation as the first s-expression following the tag.  In this case, the
s-expressions before the first tag are indented as an undistinguished
form.")

(make-variable-buffer-local 'fi:lisp-tag-body-indentation)

(defvar fi:lisp-tag-indentation-hook nil
  "*Name of function to apply to return indentation of tag.
This variable may be bound to the name of a function to be applied (to
three arguments: the character position of the beginning of the tag,
the last parse state, and the indent point) to return the appropriate
indentation for tags occurring within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'tag or 'tagbody.  The indentation
returned is absolute.")

(make-variable-buffer-local 'fi:lisp-tag-indentation-hook)

(defvar fi:lisp-keyword-indentation 1
  "*Indentation of keywords relative to containing list.
This variable is used by the function `fi:lisp-indent-keyword-list' to indent
keywords that occur within special forms whose symbols have a
'fi:lisp-indent-hook property of 'keyword or 'keyword-list.  The
indentation is relative to the indentation of the parenthesis enclosing the
special form.")

(make-variable-buffer-local 'fi:lisp-keyword-indentation)

(defvar fi:lisp-keyword-argument-indentation t
  "*Indentation of keyword argument lines relative to containing list.
This variable is used by the function `fi:lisp-indent-keyword-list' to indent
keyword-argument lines that occur within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'keyword or 'keyword-list.  The indentation
is relative to the indentation of the parenthesis enclosing the special form.
If the value is T, the argument(s) of keywords will be indented as a block
at the same indentation as the first s-expression following the tag.  See
the documentation for the function `fi:lisp-indent-keyword-list'.")

(make-variable-buffer-local 'fi:lisp-keyword-argument-indentation)

(defvar fi:lisp-keyword-indentation-hook nil
  "*Name of function to apply to return indentation of a keyword.
This variable may be bound to the name of a function to be applied (to
three arguments: the character position of the beginning of the keyword,
the last parse state, and the indent point) to return the appropriate
indentation for keywords occurring within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'keyword or 'keyword-list.  The inden-
tation returned is absolute.")

(make-variable-buffer-local 'fi:lisp-keyword-indentation-hook)

(defvar fi:lisp-maximum-indent-struct-depth 3
  "*Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is NIL, no backtracking will occur and lists whose `car'
is a symbol with a 'fi:lisp-indent-hook property of 'label, 'labels, 'flet,
'macrolet, 'defun, or a list may not be indented properly.  In addition,
quoted lists will not be treated specially.  If this variable is T, there
is no limit placed on backtracking.  A numeric value specifies the maximum
depth to backtrack.  A reasonable value is 3.")

(make-variable-buffer-local 'fi:lisp-maximum-indent-struct-depth)

(defvar fi:indent-methods-case-sensitive t
  "*If non-nil, the code that is being edited is for a case-sensitive dialect
of Lisp.  This variable is buffer-local.  If a Lisp is case-insensitive,
indentation specifications should be placed on the Emacs Lisp symbol that
corresponds to the lowercase name of the function, macro, or special
form.")

(make-variable-buffer-local 'fi:indent-methods-case-sensitive)

(defvar fi:lisp-package t
  "*This variable may be NIL, T, or a symbol or string.
If the value is NIL, a package qualifier is ignored when getting the
indentation specification for a symbol.  If the value is T, the package
qualifier is not ignored.  If this variable is any other symbol or a string,
it names the package to be used for all unqualified symbols.  When this
variable is not NIL, the qualified symbol is first checked for an indentation
specification, then the unqualified symbol is checked.  This variable is
buffer-local.")

(make-variable-buffer-local 'fi:lisp-package)


(defconst fi:lisp-indent-hook 'fi:lisp-indent-hook
  "Function funcalled to calculate the indentation at a specific point.
Called with two arguments, indent-point and `state'.")

(make-variable-buffer-local 'fi:lisp-indent-hook)

(defvar fi::comment-indent-hook-values '(0 nil))

(make-variable-buffer-local 'fi::comment-indent-hook-values)

(defvar fi::lisp-most-recent-parse-result nil
  "Most recent parse result: point at parse end and parse state.
A list that is the `cons' of the point at which the most recent
parse ended and the parse state from `fi::parse-partial-sexp'.")

(make-variable-buffer-local 'fi::lisp-most-recent-parse-result)

(defvar fi::calculate-lisp-indent-state-temp nil
  "Used as the last argument to fi::parse-partial-sexp so we do as little
consing as is possible.")

(make-variable-buffer-local 'fi::calculate-lisp-indent-state-temp)

(defvar fi::lisp-indent-state-temp nil
  "Used as the last argument to fi::parse-partial-sexp so we can do as
little consing as possible.")

(make-variable-buffer-local 'fi::lisp-indent-state-temp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi:lisp-comment-indent (&optional addr)
  (let* ((begin (if addr addr (point)))
	 (comment-spec
	  (or (and fi:lisp-comment-indent-specification
		   (numberp (car fi:lisp-comment-indent-specification))
		   (progn
		     (if (not
			  (= comment-column
			     (car fi:lisp-comment-indent-specification)))
			 (rplaca fi:lisp-comment-indent-specification
				 comment-column))
		     fi:lisp-comment-indent-specification))
	      (or fi:lisp-comment-indent-specification
		  (list comment-column t nil 0))))
	 (spec-length (length comment-spec))
	 spec
	 count)
    (save-excursion
      (goto-char begin)
      (skip-chars-forward ";")
      (setq count (- (point) begin)))
    (setq spec
      (if (> count spec-length)
	  (nth (1- spec-length) comment-spec)
	(nth (max 0 (1- count)) comment-spec)))
    (car
     (setq fi::comment-indent-hook-values
       (cond
	((eq spec nil) (list (current-column) nil))
	((eq spec t) (let ((tem (fi::calculate-lisp-indent)))
		       (list (if (listp tem) (car tem) tem) t)))
	((and (integerp spec) (>= spec 0)) (list spec nil))
	((integerp spec) (list (- (current-column) spec) nil))
	(t (error "Bad comment indentation specification for count %d."
		  count)))))))

(defvar fi::lisp-doing-electric-semicolon nil)

(defconst fi::semi-char 59)

(defun fi:lisp-semicolon (&optional count)
  "Lisp semicolon hook.  Prefix argument is number of semicolons to
insert.  The default value is 1."
  (interactive "p")
  (insert-char fi::semi-char (or count 1))
  (if fi:lisp-electric-semicolon
      (save-excursion
	(skip-chars-backward ";")
	(let ((fi::lisp-doing-electric-semicolon t))
	  (fi::indent-lisp-semicolon)))))

;; I can't prove these don't need to be dynamic, so for now just do this:
(defvar comment-at)
(defvar end-sexp)
(defvar normal-indent)
(defvar last-depth)
(defvar innerloop-done)

(defun fi::indent-lisp-semicolon (&optional at last-state)
  "Indent Lisp semicolon at point.
The optional parameters specify the point at which the last partial
s-expression parse (using `fi::parse-partial-sexp') terminated and the
status of that parse."
  (save-excursion
    (let ((new-point (point))
	  (old-point (if at at (point)))
	  (parse-state (if last-state last-state (list 0 0 0 nil nil nil 0))))
      (if (and last-state (< old-point (point)))
	  (setq parse-state
	    (fi::parse-partial-sexp
	     old-point (point) nil nil last-state last-state))
	(progn
	  ;; Find beginning of the top-level list.
	  (save-excursion
	    (if (beginning-of-defun)
		(setq old-point (point))))
	  ;; Find beginning of outermost enclosing list if we could not
	  ;;   find the beginning of a top-level list.  We do this second
	  ;;   because `scan-lists' is much more expensive than applying
	  ;;   `beginning-of-defun'.
	  (if (= old-point (point))
	      (while (setq new-point
		       (condition-case nil
			   (scan-lists old-point -1 1)
			 (error nil)))
		(setq old-point new-point)))
	  (if (= old-point (point))
	      (setq old-point 1))
	  (setq parse-state
	    (fi::parse-partial-sexp
	     old-point (point) nil nil nil parse-state))))
      (if (not (or (nth 3 parse-state)
		   (nth 4 parse-state)
		   (nth 5 parse-state)))
	  (if (and (boundp 'comment-indent-function) comment-indent-function)
	      (let ((to-column (funcall comment-indent-function (point))))
		(if (and (second fi::comment-indent-hook-values)
			 (null fi::lisp-doing-electric-semicolon)
			 (save-excursion
			   (skip-chars-backward "\t ")
			   (not (bolp))))
		    nil
		  ;; Function `indent-to' only inserts characters.
		  (delete-horizontal-space)
		  (if (and (not (or (= (preceding-char) ?\ )
				    (= (preceding-char) ?\t)
				    (= (preceding-char) ?\n)
				    (= (preceding-char) ?\f)))
			   (>= (current-column) to-column))
		      ;; Insert space if not rigid comment or if rigid comment
		      ;;   and we are at or beyond the comment column.
		      (if (not (bobp)) (just-one-space)))
		  (indent-to to-column 0))))
	(if (nth 4 parse-state)
	    (let (comment-at)
	      (beginning-of-line)
	      (if (setq comment-at (fi::find-line-comment))
		  (if (and (boundp 'comment-indent-function)
			   comment-indent-function)
		      (let (to-column)
			(goto-char comment-at)
			(setq to-column (funcall comment-indent-function
						 (point)))
			;; Function `indent-to' only inserts characters.
			(delete-horizontal-space)
			(if (and (= (preceding-char) ?\))
				 (or (not
				      (second fi::comment-indent-hook-values))
				     (>= (current-column) to-column)))
			    ;; Insert space if not rigid comment or if rigid
			    ;;   comment and we are at or beyond the comment
			    ;;   column.
			    (if (not (bobp)) (just-one-space)))
			(indent-to to-column 0)))
		(error "sexp parse anomaly: no comment where expected"))))))))

(defun fi:lisp-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((saved-point (point))
	(indent (fi::calculate-lisp-indent))
	shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")

    (when (looking-at "[;]+")
      (let* ((start (point))
	     (end (save-excursion
		    (skip-chars-forward ";")
		    (point)))
	     (ind (nth (- end start 1)
		       fi:lisp-comment-indent-specification)))
	(if (eq t ind)
	    (current-column)
	  (if (null ind)
	      (setq indent nil)
	    (setq indent ind)))))

    (cond
     (indent
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (point))
	(indent-to indent))
      (if (setq comment-at (fi::find-line-comment))
	  (save-excursion
	    (goto-char comment-at)
	    (fi::indent-lisp-semicolon
	     (car fi::lisp-most-recent-parse-result)
	     (cdr fi::lisp-most-recent-parse-result))))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (fi:indent-code-rigidly beg end shift-amt)))
     (t (goto-char saved-point)))))

(defun fi::calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (state fi::calculate-lisp-indent-state-temp)
	  paren-depth
	  desired-indent
	  (retry t)
	  last-sexp
	  containing-sexp
	  temp
	  comment-hack
	  sharpm-hack)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))

      ;; Find outermost containing sexp

      (while (< (point) indent-point)
	(setq state (fi::parse-partial-sexp
		     (point) indent-point 0 nil nil state)))

      (rplaca fi::lisp-most-recent-parse-result (point))
      (rplacd fi::lisp-most-recent-parse-result (copy-sequence state))

      (when (and (setq temp (nth 3 state)) (= ?| temp))
	;; in a #||# comment
	(indent-relative t)
	(setq desired-indent (current-column))
	(setq comment-hack t))

      ;; Find innermost containing sexp
      (while (and (null comment-hack)
		  retry
		  (setq paren-depth (car state))
		  (> paren-depth 0))
	(setq retry nil)
	(setq last-sexp (nth 2 state))
	(setq containing-sexp (car (cdr state)))
	;; Position following last unclosed open.
	(goto-char (1+ containing-sexp))

	(when (= ?# (char-after (point)))
	  ;; Do special checking for #+/#-:
	  ;;   (#+foo x
	  ;;    #-foo y
	  ;;    xxx ...
	  ;; and
	  ;;   (#+foo x #-foo y
	  ;;    xxx ...
	  ;; but, don't change this:
	  ;; (#+foo x #-foo y yyy
	  ;;        xxx ...
	  
	  (when (save-excursion
		  (let ((max (save-excursion (forward-line 1) (point))))
		    (condition-case ()
			;; If there are 2 or 4 elements on the first line
			;; after the opening paren, then indent at the
			;; current column, otherwise, do nothing.  This
			;; works with comments on the same line, too.
			(or (and (progn
				   (forward-sexp 2)
				   (< (point) max))
				 (progn 
				   (forward-sexp 1)
				   (> (point) max)))
			    (and (progn
				   (forward-sexp 1)
				   (< (point) max))
				 (progn
				   (forward-sexp 1)
				   (> (point) max))))
		      (error () nil))))
	    ;; Short circuit most of the rest of this function's
	    ;; calculations, then set desired-indent to current column.
	    (setq retry t)
	    (setq desired-indent (current-column))
	    (setq sharpm-hack t)))
	
	;; Is there a complete sexp since then?
	(if (and last-sexp (> last-sexp (point)))
	    ;; Yes, but is there a containing sexp after that?
	    (let ((peek (fi::parse-partial-sexp last-sexp indent-point 0)))
	      (if (setq retry (car (cdr peek)))
		  (setq state peek))))
	(rplaca fi::lisp-most-recent-parse-result (point))
	(rplacd fi::lisp-most-recent-parse-result (copy-sequence state))

	(if retry
	    nil
	  ;; Innermost containing sexp found
	  (goto-char (1+ containing-sexp))
	  (if (not last-sexp)
	      ;; indent-point immediately follows open paren.
	      ;; Don't call hook.
	      (setq desired-indent (current-column))
	    ;; Move to first sexp after containing open paren
	    (fi::parse-partial-sexp (point) last-sexp 0 t nil
				;; the result goes here:
				(cdr fi::lisp-most-recent-parse-result))
	    (rplaca fi::lisp-most-recent-parse-result (point))
	    (cond
	     ((and nil			; 10/13/91, dkl: this
					; optimization screws things like
					; (defmethod foo :around ((a b)
					; &key c d) d), if you type RET
					; after the form `c'
		   (looking-at "\\s("))
	      ;; Looking at a list.  Don't call hook.
	      (if (not (> (save-excursion (forward-line 1) (point))
			  last-sexp))
		  (progn (goto-char last-sexp)
			 (beginning-of-line)
			 (fi::parse-partial-sexp
			  (point) last-sexp 0 t nil
			  ;; the result goes here:
			  (cdr fi::lisp-most-recent-parse-result))
			 (rplaca fi::lisp-most-recent-parse-result (point))))
	      ;; Indent under the list or under the first sexp on the
	      ;; same line as last-sexp.  Note that first thing on that
	      ;; line has to be complete sexp since we are inside the
	      ;; innermost containing sexp.
	      (backward-prefix-chars)
	      (setq desired-indent (current-column)))
	     ((> (save-excursion (forward-line 1) (point))
		 last-sexp)
	      ;; Last sexp is on same line as containing sexp.
	      ;; It's almost certainly a function call.
	      (fi::parse-partial-sexp (point) last-sexp 0 t nil
				      ;; the result goes here:
				      (cdr fi::lisp-most-recent-parse-result))
	      (rplaca fi::lisp-most-recent-parse-result (point))
	      (if (/= (point) last-sexp)
		  ;; Indent beneath first argument or, if only one sexp
		  ;; on line, indent beneath that.
		  (progn (forward-sexp 1)
			 (fi::parse-partial-sexp
			  (point) last-sexp 0 t nil
			  ;; the result goes here:
			  (cdr fi::lisp-most-recent-parse-result))
			 (rplaca fi::lisp-most-recent-parse-result (point))))
	      (backward-prefix-chars))
	     (t
	      ;; Indent beneath first sexp on same line as last-sexp.
	      ;; Again, it's almost certainly a function call.
	      (goto-char last-sexp)
	      (beginning-of-line)
	      (fi::parse-partial-sexp (point) last-sexp 0 t nil
				  ;; the result goes here:
				  (cdr fi::lisp-most-recent-parse-result))
	      (rplaca fi::lisp-most-recent-parse-result (point))
	      (backward-prefix-chars))))))

      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by fi:lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond (comment-hack)
	    (sharpm-hack)
	    ((nth 3 state)
	     ;; Inside a string, don't change indentation.
             (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (setq desired-indent (current-column)))
	    ((and (integerp fi:lisp-indent-offset) containing-sexp)
	     ;; Indent by constant offset
	     (goto-char containing-sexp)
	     (setq desired-indent (+ fi:lisp-indent-offset (current-column))))
	    (desired-indent)
	    ((and (boundp 'fi:lisp-indent-hook)
		  fi:lisp-indent-hook
		  (not retry))
	     (cond ((setq desired-indent
		      (catch 'fi:lisp-indent-hook-escape
			(funcall fi:lisp-indent-hook indent-point state))))
		   (t
		    ;; Use default indentation if not computed yet
		    (setq desired-indent (current-column)))))
	    (t (setq desired-indent (current-column))))

      desired-indent)))

(defun fi:lisp-indent-hook (indent-point state)
  (let ((normal-indent (current-column))
	(calculated-indent nil))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (when (and (looking-at "\\s(")
		 (not (looking-at "\\s()"))
		 ;; only return if there is an indent method for the thing
		 ;; following the open paren.
		 (save-excursion	;added 2aug94 - smh
		   (let ((function (buffer-substring
				    (progn (forward-char 1) (point))
				    (progn (forward-sexp 1) (point)))))
		     (fi::lisp-get-method function))))
	(throw 'fi:lisp-indent-hook-escape nil))
      (re-search-forward "\\sw\\|\\s_" nil t)
      (when (/= (point) (car (cdr state)))
	(let ((function (buffer-substring (progn (forward-char -1) (point))
					  (progn (forward-sexp 1) (point))))
	      (count (fi::calc-count (car (cdr state)) indent-point)))
	  (setq calculated-indent
	    (fi::lisp-invoke-method
	     (nth 1 state)
	     (fi::lisp-get-method function)
	     0 count state indent-point))))
      (when (and (null calculated-indent)
		 fi:lisp-maximum-indent-struct-depth)
	(let ((depth 0)
	      (maximum-depth
	       (if (integerp fi:lisp-maximum-indent-struct-depth)
		   fi:lisp-maximum-indent-struct-depth
		 99999))
	      last-start function sexp-beginning)
	  (goto-char (car (cdr state)))
	  (while (and (null calculated-indent)
		      (< depth maximum-depth)
		      (condition-case nil
			  (progn
			    (setq last-start (point))
			    (backward-up-list 1)
			    t)
			(error nil)))
	    (save-excursion
	      (setq depth (1+ depth))
	      (setq sexp-beginning (point))
	      (forward-char 1)
	      (re-search-forward "\\sw\\|\\s_")
	      (if (< (point) last-start)
		  (let ((count 1))
		    (setq function (buffer-substring
				    (progn (forward-char -1) (point))
				    (progn (forward-sexp 1) (point))))
		    (fi::parse-partial-sexp (point) indent-point 1 t nil
					    fi::lisp-indent-state-temp)
		    (while (and (condition-case nil
				    (progn
				      (forward-sexp 1)
				      (fi::parse-partial-sexp
				       (point) indent-point 1 t nil
				       fi::lisp-indent-state-temp))
				  (error nil))
				(< (point) indent-point))
		      (setq count (1+ count)))
		    (setq calculated-indent
		      (fi::lisp-invoke-method
		       sexp-beginning
		       (fi::lisp-get-method function)
		       depth count state
		       indent-point))))))))
      calculated-indent)))

(defvar fi::lisp-count-max 10
  "The max count in an indent hook.")

(defun fi::calc-count (from to)
  (save-excursion
    (let ((count 1))
      (goto-char from)
      (forward-char 1)
      (while (and (< count fi::lisp-count-max)
		  (condition-case nil
		      (progn
			(forward-sexp 2)
			(backward-sexp 1)
			t)
		    (error nil))
		  (< (point) to))
	(setq count (1+ count)))
      count)))

(defun fi::lisp-get-method (name)
  (let ((method
	 (let ((last-colon (fi::lisp-find-char ?: name t))
	       (first-colon (fi::lisp-find-char ?: name)))
	   (if last-colon
	       (cond
		((null fi:lisp-package)
		 (fi::lisp-get-method-aux (substring name (1+ last-colon))))
		(t
		 (or (fi::lisp-get-method-aux name)
		     (if (= first-colon (1- last-colon))
			 (fi::lisp-get-method-aux
			  (concat (substring name 0 first-colon) ":"
				  (substring name (1+ last-colon))))
		       nil)
		     (fi::lisp-get-method-aux
		      (substring name (1+ last-colon))))))
	     (cond
	      ((and fi:lisp-package (not (eq fi:lisp-package t)))
	       (or (fi::lisp-get-method-aux (concat fi:lisp-package "::" name))
		   (fi::lisp-get-method-aux (concat fi:lisp-package ":" name))
		   (fi::lisp-get-method-aux name)))
	      (t
	       (fi::lisp-get-method-aux name)))))))
    (if (eq method t)
	nil
      method)))

(defun fi::lisp-get-method-aux (name)
  (or (and (boundp 'fi:lisp-indent-hook-property)
	   fi:lisp-indent-hook-property
	   (get (intern-soft (if fi:indent-methods-case-sensitive
				 name
			       (downcase name)))
		fi:lisp-indent-hook-property))
      (get (intern-soft (if fi:indent-methods-case-sensitive
			    name
			  (downcase name)))
	   'fi:lisp-indent-hook)
      (and (or (string-match "^with-" name)
	       (string-match "^do-" name))
	   1)
      (and (string-match "^def" name) 2)))

(defun fi::lisp-invoke-method (form-start method depth count state
			       indent-point)
  (cond ((consp method)
	 (cond ((eq 'like (car method))
		(setq method
		  (fi::lisp-get-method (symbol-name (car (cdr method))))))
	       ((eq 'if (car method))
		(setq method
		  (apply 'fi::lisp-if-indent depth count state indent-point
			 (cdr method)))))))
  (cond ((and form-start
	      (or (eq (char-after (- form-start 1)) ?\#) ;; Vectors.
		  (and (eq (char-after (- form-start 1)) ?\') ;; Quoted lists.
		       (not (eq (char-after (- form-start 2)) ?\#)))))
	 (fi::lisp-indent-quoted-list depth count state indent-point))
	((integerp method)
	 (fi::lisp-indent-specform method depth count state indent-point))
	((consp method)
	 (cond ((eq 'funcall (car method))
		(funcall (car (cdr method)) depth count state indent-point))
	       ((eq (car method) 'recursive)
		(fi::lisp-invoke-method form-start (nth 1 method) 0 count state
					indent-point))
	       ((and (symbolp (car method)) (fboundp (car method)))
		(apply (car method) depth count state indent-point
		       (cdr method)))
	       (t (fi::lisp-indent-struct
		   method depth count state indent-point))))
	((eq method 'quote)
	 (fi::lisp-indent-quoted-list depth count state indent-point))
	((memq method '(tag tagbody))
	 (fi:lisp-indent-tagbody depth count state indent-point))
	((memq method '(keyword keyword-list))
	 (fi:lisp-indent-keyword-list
	  depth count state indent-point t nil t))
	((memq method '(lambda lambda-list))
	 (fi:lisp-indent-keyword-list depth count state indent-point t nil t
				      nil nil nil "&optional" "&rest" "&key"
				      "&allow-other-keys" "&aux" "&body"
				      "&whole" "&environment"))
	(method (error "can't handle method %s" method))))

(defun fi::lisp-indent-struct (methods depth count state indent-point)
  "Function to indent Lisp special forms that have substructure.
The METHODS is a list of triples, (Depth Count Method), where Depth and
Count specify when special treatment is required of a sublist in a form,
and Method is the method of indentation to apply to such a sublist.  If
Depth or Count is t or nil, Method is used for any Depth or Count.  The
Count may be a list of two elements, in which case it specifies an inclusive
range of subexpressions, either bound of which may be t nor nil.  The
Method will be applied to the form itself if the Depth is zero.  The Method
is interpreted identically to the 'fi:lisp-indent-hook property of symbols and
thus may be recursive (that is, itself a list of triples).  If the Method
is recursive, it is applied if DEPTH is greater or equal to Depth; this
implies that triples should be ordered greatest depth first in lists.  In
recursive applications of this function to a Method, DEPTH is set to the
depth of the current sublist relative to sublist associated with Method.
The METHODS list should be the value of the 'fi:lisp-indent-hook property of
a symbol that is a Lisp special form having substructure.  The DEPTH is
the depth of the current sublist relative to the form.  The COUNT argument
is the number of the current s-expression in the form.

As an example, giving LABELS the following property

  (put 'labels 'fi:lisp-indent-hook
       '((2 1 ((1 1 lambda-list) (0 t 1))) (0 t 1)))

will have the effect that s-expressions of the LABELS form itself are
indented with 1 distinguished form using `fi::lisp-indent-specform' (this is
specified by '(0 t 1)).  In addition, all sublists of the first s-expression,
which is a list, of the LABELS special form (denoted by '(2 1 ...)) will be
treated just like a LAMBDA (whose method is '((1 1 lambda-list) (0 t 1)))."
  (let ((calculated-indent nil)
	method)
    (while (and (null calculated-indent)
		(setq method (car methods)))
      (setq methods (cdr methods))
      (let ((method-depth (car method))
	    (method-count (car (cdr method)))
	    (method-method (car (cdr (cdr method)))))
	(if (and (or (equal depth method-depth)
		     (memq method-depth '(t nil))
		     (and (consp method-method)
			  (not (and (symbolp (car method-method))
				    (fboundp (car method-method))))
			  (> depth method-depth)))
		 (if (consp method-count)
		     (and (or (memq (car method-count) '(t nil))
			      (>= count (car method-count)))
			  (or (memq (car (cdr method-count)) '(t nil))
			      (<= count (car (cdr method-count)))))
		   (or (equal count method-count)
		       (memq method-count '(t nil)))))
	    (let ((inner-depth (if (integerp method-depth)
				   (- depth method-depth)
				 99999))
		  inner-count
		  (depth 0)
		  (count 0))
	      (goto-char (car (cdr state)))
	      (while (and (< depth inner-depth)
			  (condition-case nil
			      (progn
				(backward-up-list 1)
				t)
			    (error nil)))
		(setq depth (1+ depth)))
	      (condition-case nil
		  (progn
		    (forward-char 1)
		    (fi::parse-partial-sexp (point) indent-point 1 t nil
					fi::lisp-indent-state-temp)
		    (while (and (condition-case nil
				    (progn
				      (forward-sexp 1)
				      (fi::parse-partial-sexp
				       (point) indent-point 1 t nil
				       fi::lisp-indent-state-temp))
				  (error nil))
				(< (point) indent-point))
		      (setq count (1+ count))))
		(error nil))
	      (setq inner-count count)
	      (setq calculated-indent
		(fi::lisp-invoke-method
		 nil method-method inner-depth inner-count
		 state indent-point))))))
    (cond
     ((consp calculated-indent)
      calculated-indent)
     (calculated-indent
      (list calculated-indent (nth 1 state)))
     (t
      nil))))

(defun fi::lisp-indent-quoted-list (depth count state indent-point)
  "Function for indenting quoted lists."
  (goto-char (1+ (car (cdr state))))
  (current-column))

(defvar fi::lisp-if*-hack nil)

(defun fi:lisp-indent-keyword-list (depth count state indent-point
				    quotedp keyword-arg-pairs-p
				    &optional keyword-count
					      special-keyword-count
					      special-count
					      ignore-after-count
				    &rest keywords)
  "Function for indenting a form with keywords.
This function is useful for indenting lambda lists and special forms that
have keywords.  The argument QUOTEDP indicates that the form is a quoted
list (such as a lambda list) if it is non-NIL.

KEYWORD-ARG-PAIRS-P if non-NIL indicates that keywords and their arguments
come in pairs, i.e. there is a single s-expression associated with each
keyword.  If this is NIL, all s-expressions that follow a keyword, up to
the next keyword, are considered as the `arguments' to the keyword.  (Note
that the first s-expression following the keyword is always treated as the
argument to the keyword even if it is also a keyword.)

Optional argument KEYWORD-COUNT specifies the number of keyword that are
recognized as such.  Only the specified number of keywords (and their
associated argument s-expressions) will be indented distinctly: any further
keywords are treated simply as atomic expressions in the body of the form.
If this is T, all keywords found in the form are recognized.

Optional argument SPECIAL-KEYWORD-COUNT specifies the number of
distinguished keywords.  Distinguished keywords are indented twice the
value of `fi:lisp-body-indent', just as distinguished forms are indented by
`fi::lisp-indent-specform'.  If the argument to a distinguished keyword does
not appear on the same line as the keyword, the argument is indented thrice
the value of `fi:lisp-body-indent'.  If KEYWORD-ARG-PAIRS-P is NIL, variable
`fi:lisp-keyword-argument-indentation' determines the indentation of any
second and subsequent arguments to distinguished keywords.  If
`fi:lisp-keyword-argument-indentation' is not T, arguments are indented thrice
the value of `fi:lisp-body-indent'.  If SPECIAL-KEYWORD-COUNT is T, all
keywords and their arguments are treated as distinguished.  The keywords
encountered in the form are counted in parallel to satisfy both
SPECIAL-KEYWORD-COUNT and KEYWORD-COUNT.

Optional argument SPECIAL-COUNT specifies the number of distinguished
s-expressions in the form.  Any potential keywords and their arguments that
appear as distinguished s-expressions of the form are not counted toward
SPECIAL-KEYWORD-COUNT or KEYWORD-COUNT.

Optional argument IGNORE-AFTER-COUNT specifies the number of initial
non-keyword s-expressions in the form (after satisfying SPECIAL-COUNT)
after which keywords will no longer be recognized.  If this is T, no
keywords will be recognized after the first non-keyword, non-special
s-expression encountered.  (Note that specifying both KEYWORD-ARG-PAIRS-P
to be NIL and IGNORE-AFTER-COUNT to be T means only that no keywords will
be recognized if the first s-expression following any distinguished
s-expressions is not a keyword.)

Any further arguments to this function constitute the specific keywords to
be recognized.  If no keywords are explicitly specified, all keywords (atoms
beginning with a colon) are recognized."
  (if (> depth 0)
      nil
    (save-excursion
      (if (eq keyword-count t) (setq keyword-count 99999))
      (if (eq special-keyword-count t) (setq special-keyword-count 99999))
      (goto-char indent-point)
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (let* ((keyword-at (point))
	     (keyword
	      (condition-case nil
		  (save-excursion
		    (buffer-substring
		     keyword-at (progn (forward-sexp 1) (point))))
		(error nil)))
	     (special-keys nil)
	     (keywords (if keywords
			   (mapcar '(lambda (key)
				     (if (consp key)
					 (progn
					   (setq special-keys
					     (cons key special-keys))
					   (car key))
				       key))
				   keywords)))
	     (is-keyword
	      (or (and (null keywords)
		       (and (not (eobp)) (eq (following-char) ?:)))
		  (and keywords
		       keyword
		       (fi::lisp-find-keyword keyword keywords))))
	     (sexp-at (nth 1 state))
	     (sexp-column (save-excursion
			    (goto-char sexp-at)
			    (current-column))))
	(list
	 (let* ((keyword-info (fi::lisp-scan-sexp-for-keywords
			       special-count ignore-after-count
			       (not quotedp) keyword-arg-pairs-p
			       keywords state indent-point))
		(last-keyword-began (nth 0 keyword-info))
		(last-keyword-arg-began (nth 1 keyword-info))
		(keywords-found (nth 3 keyword-info))
		(nonkeywords-found (nth 4 keyword-info))
		(special-indent (if (integerp special-count)
				    (fi::lisp-indent-specform
				     special-count
				     depth count state indent-point)
				  nil)))
	   (if (consp special-indent)
	       (car special-indent)
	     (cond
	      ((and is-keyword
		    (integerp nonkeywords-found)
		    keyword-count
		    (< keywords-found keyword-count))
	       (cond
		((and special-keys
		      (+ sexp-column
			 (car (fi::lisp-find-special-keyword-indent
			       keyword special-keys)))))
		((and special-keyword-count
		      (< keywords-found special-keyword-count))
		 (+ sexp-column (* 2 fi:lisp-body-indent)))
		((and (boundp 'fi:lisp-keyword-indentation-hook)
		      fi:lisp-keyword-indentation-hook)
		 (funcall fi:lisp-keyword-indentation-hook
			  keyword-at state indent-point))
		(t
		 (+ sexp-column fi:lisp-keyword-indentation))))
	      ((integerp fi::lisp-if*-hack)
	       (+ sexp-column fi::lisp-if*-hack))
	      ((and special-keyword-count
		    (<= keywords-found special-keyword-count)
		    (integerp nonkeywords-found)
		    last-keyword-began
		    (or (null last-keyword-arg-began)
			(and (not (eq fi:lisp-keyword-argument-indentation t))
			     (not keyword-arg-pairs-p))))
	       (+ sexp-column (* 3 fi:lisp-body-indent)))
	      ((or (null last-keyword-began)
		   (and keyword-arg-pairs-p last-keyword-arg-began)
		   (null keyword-count)
		   (> keywords-found keyword-count)
		   (not (integerp nonkeywords-found)))
	       (if quotedp
		   (+ sexp-column 1)
		 (+ sexp-column fi:lisp-body-indent)))
	      ((integerp fi:lisp-keyword-argument-indentation)
	       (+ sexp-column fi:lisp-keyword-argument-indentation))
	      ((eq fi:lisp-keyword-argument-indentation t)
	       (condition-case nil
		   (progn (if last-keyword-arg-began
			      (goto-char last-keyword-arg-began)
			    (backward-sexp 1))
			  (current-column))
		 (error (+ sexp-column 1))))
	      (t
	       (+ sexp-column fi:lisp-body-indent)))))
	 sexp-at)))))

(defun fi::lisp-find-special-keyword-indent (keyword alist)
  (let ((keys alist)
	(key nil)
	(matched nil)
	(test-function
	 (if fi:indent-methods-case-sensitive
	     'string-equal
	   'fi::string-equal-nocase)))
    (while (and keys (not matched))
      (setq key (car keys))
      (if (funcall test-function (car key) keyword)
	  (setq matched (cdr key)))
      (setq keys (cdr keys)))
    matched))

(defun fi::lisp-find-keyword (keyword keywords)
  (let ((keys keywords)
	(matched nil)
	(test-function
	 (if fi:indent-methods-case-sensitive
	     'string-equal
	   'fi::string-equal-nocase)))
    (while (and keys (not matched))
      (if (funcall test-function
		   (if (stringp (car keys))
		       (car keys)
		     (cdr (car keys)))
		   keyword)
	  (setq matched t))
      (setq keys (cdr keys)))
    matched))

(defun fi::lisp-scan-sexp-for-keywords (special-count ignore-after-count
					ignore-car keyword-arg-pairs-p
					keywords state indent-point)
  "Scan an s-expression for keywords.
Returns a list of five elements: point where last keyword starts (or NIL
if no keyword was found), point where last keyword's argument starts (or
NIL if no keyword was found or if the last s-expression parsed was a keyword),
point where last s-expression parsed starts (or NIL if the current form
contains no s-expressions), count (possibly zero) of keywords found, and
the number (possibly zero) of non-keyword s-expressions (excluding keyword
arguments) that were found or T if IGNORE-AFTER-COUNT was non-NIL and keyword
scanning was curtailed because of it.  A keyword and its argument constitute
a keyword pair.  The argument to a keyword, even if it is itself a keyword,
is not counted as a keyword.  The argument to a keyword is not counted as a
non-keyword s-expression."
  (let ((containing-form-start (car (cdr state)))
	(form-count 0)
	(nonkeyword-count 0)
	(ignore-keywords nil)
	(count 0)
	(last-keyword-start nil)
	(last-keyword-arg-start nil)
	(last-sexp-start nil)
	(last-was-keyword nil)
	end-sexp)
    (if (null special-count) (setq special-count 0))
    (if (eq special-count t) (setq special-count 99999))
    (goto-char containing-form-start)
    (forward-char 1)
    (if ignore-car
	(progn (forward-sexp 1)
	       (fi::parse-partial-sexp (point) indent-point 1 t nil
				   fi::lisp-indent-state-temp)))
    (while (and
	    (< (point) indent-point)
	    (not ignore-keywords)
	    (condition-case nil
		(progn
		  (forward-sexp 1)
		  (setq end-sexp (point))
		  (backward-sexp 1)
		  (setq last-sexp-start (point))
		  (setq form-count (1+ form-count))
		  (if (> form-count special-count)
		      (if (not last-was-keyword)
			  (progn
			    (if (and (null keywords)
				     (eq (following-char) ?:))
				(progn
				  (setq count (1+ count))
				  (setq last-was-keyword t)
				  (setq last-keyword-arg-start nil)
				  (setq last-keyword-start (point)))
			      (let ((keyword
				     (buffer-substring (point) end-sexp))
				    (keys keywords)
				    (matched nil)
				    (test-function
				     (if fi:indent-methods-case-sensitive
					 'string-equal
				       'fi::string-equal-nocase)))
				(while (and keys (not matched))
				  (if (funcall test-function
					       (car keys) keyword)
				      (progn
					(setq count (1+ count))
					(setq last-was-keyword t)
					(setq matched t)
					(setq last-keyword-arg-start nil)
					(setq last-keyword-start (point))))
				  (setq keys (cdr keys)))))
			    (if (and (not last-was-keyword)
				     (or keyword-arg-pairs-p (zerop count)))
				(setq nonkeyword-count (1+ nonkeyword-count)))
			    (if (and ignore-after-count
				     (if (integerp ignore-after-count)
					 (>= nonkeyword-count
					     ignore-after-count)
				       (not (zerop nonkeyword-count))))
				(setq ignore-keywords t)))
			(progn
			  (setq last-keyword-arg-start (point))
			  (setq last-was-keyword nil))))
		  (goto-char end-sexp)
		  (fi::parse-partial-sexp (point) indent-point 1 t nil
				      fi::lisp-indent-state-temp))
	      (error nil))))
    (list last-keyword-start last-keyword-arg-start last-sexp-start
	  count (or ignore-keywords nonkeyword-count))))

(defun fi:lisp-indent-if* (depth count state indent-point)
  "Function for indenting the IF* special form of Allegro CL."
  (let ((fi:lisp-keyword-argument-indentation t)
	(fi::lisp-if*-hack 8))
    (fi:lisp-indent-keyword-list
     depth count state indent-point
     nil				; quotedp
     nil				; keyword-arg-pairs-p
     t					; keyword-count
     t					; special-keyword-count
     1					; special-count
     nil				; ignore-after-count
     ;; the keywords:
     '("then" 3)
     '("thenret" 3)
     '("else" 3)
     '("elseif" 1))))

(defun fi:lisp-indent-tagbody (depth count state indent-point
			       &optional spec-count
			       &rest keywords)
  "Function for indenting TAGBODY and related forms.
This function indents special forms that have `tags' or `keywords' that
should be treated specially.  For example, TAGBODY forms have `tags' for
GOTO.  An optional argument SPEC-COUNT is accepted, specifying the number
of distinguished s-expressions in the form.  Any further arguments
constitute the `keywords' or `tags' that are to be recognized.  In the
absence of an explicit list, any atomic expression is considered a
keyword."
  (if (> depth 0)
      nil
    (save-excursion
      (goto-char indent-point)
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (let* ((tag-at (point))
	     (tag (condition-case nil
		      (save-excursion
			(buffer-substring tag-at
					  (progn (forward-sexp 1) (point))))
		    (error nil)))
	     (is-tag (or (and (null keywords)
			      (and (not (eobp))
				   (memq (char-syntax (following-char))
					 '(?w ?_))))
			 (and keywords
			      tag
			      (let ((keys keywords)
				    (matched nil)
				    (test-function
				     (if fi:indent-methods-case-sensitive
					 'string-equal
				       'fi::string-equal-nocase)))
				(while (and keys (not matched))
				  (if (funcall test-function (car keys) tag)
				      (setq matched t))
				  (setq keys (cdr keys)))
				matched))))
	     (sexp-at (nth 1 state))
	     (sexp-column (save-excursion
			    (goto-char sexp-at)
			    (current-column))))
	(list
	 (if is-tag
	     (if (and (boundp 'fi:lisp-tag-indentation-hook)
		      fi:lisp-tag-indentation-hook)
		 (funcall fi:lisp-tag-indentation-hook
			  tag-at state indent-point)
	       (+ sexp-column fi:lisp-tag-indentation))
	   (let ((spec-indent (if (integerp spec-count)
				  (fi::lisp-indent-specform
				   spec-count depth count state indent-point)
				nil)))
	     (if (consp spec-indent)
		 (car spec-indent)
	       (cond
		((integerp fi:lisp-tag-body-indentation)
		 (+ sexp-column fi:lisp-tag-body-indentation))
		((eq fi:lisp-tag-body-indentation t)
		 (condition-case nil
		     (progn (backward-sexp 1)
			    (current-column))
		   (error (+ sexp-column 1))))
		(t (+ sexp-column fi:lisp-body-indent))))))
	 sexp-at)))))

(defun fi::lisp-if-indent (depth count state indent-point test then-spec
			   else-spec)
  "Indent a form conditionally.  The TEST form is used to generate a Lisp
form that is evaluated.  The form evaluated will be:
	(apply (car TEST) depth count state indent-point (cdr TEST))
If this application returns non-NIL, the indentation specification given by
THEN-SPEC is used, otherwise the indentation specification given by
ELSE-SPEC is used."
  (if (apply (car test) depth count state indent-point
	     (cdr test))
      then-spec
    else-spec))

(defun fi:lisp-atom-p (depth count state indent-point element)
  "Predicate to test whether the specified ELEMENT of the current s-expression
being parsed is atomic.  Returns T if the element is atomic, returns NIL if
the element is not atomic or if the s-expression does not contain enough
elements."
  (car (fi::lisp-atom-info depth count state indent-point element)))

(defun fi::lisp-atom-info (depth count state indent-point element)
  "Return information about the atomicity of the specified ELEMENT
of the current s-expression being parsed.
Returns a list of two elements.  The first is non-NIL if the element
is atomic.  It will be NIL if the element of the s-expression was not
atomic or if the s-expression is not a list or if the s-expression
is a list but does not contain enough elements.  The second element of
the returned list will be non-NIL if the specified element was found,
NIL otherwise.  The ELEMENT is the number of the element, zero indexed."
  (let ((atomic nil)
	(found nil)
	(count 0))
    ;; find the beginning of the form that has the method which triggered
    ;; us:
    (condition-case ()
	;; up-list will take us there, whether or not we error!  [I hereby
	;; dedicate this bug fix to Joe Satriani--the bug was found during
	;; a particularly loud playing of "Ice 9" on 7/17/90. -dkl]
	(up-list (- (- depth) 1))
      (error nil))
    (forward-char 1)			; get inside the sexp
    ;; now look for our sexp and see if it is atomic:
    (while (and
	    (not found)
	    (< (point) indent-point)
	    (condition-case nil
		(progn
		  (forward-sexp 1)
		  (setq end-sexp (point))
		  (backward-sexp 1)
		  (if (= count element)
		      (progn
			(setq found t)
			(setq atomic (not (eq (following-char) ?\()))))
		  (goto-char end-sexp)
		  (setq count (1+ count))
		  (fi::parse-partial-sexp (point) indent-point 1 t nil
				      fi::lisp-indent-state-temp))
	      (error nil))))
    (list atomic found)))

(defun fi::lisp-indent-predicated-special (depth count state indent-point
					   &optional spec-count predicate)
  "Function for indenting forms whose distinguished forms are predicated.
An example is the Common Lisp LOCALLY special form.  All initial subforms
that are declarations are distinguished forms, and any remaining forms are
body forms.  An optional SPEC-COUNT is accepted, indicating that there will
be at least that many distinguished forms.  If PREDICATE is not supplied,
this funciton uses a predicate that returns non-NIL for subforms that are
declarations (i.e., forms whose car is DECLARE).  The PREDICATE must name a
function of one argument, the buffer position of the subform to be
examined.  The predicate must return non-NIL if that subform is to be
distinguished."
  (or spec-count (setq spec-count 0))
  (or predicate (setq predicate 'fi::lisp-form-declare-p))
  (if (> depth 0)
      nil
    (or (save-excursion
	  (let ((containing-form-start (nth 1 state))
		(count 0)
		containing-form-column)
	    (goto-char containing-form-start)
	    (setq containing-form-column (current-column))
	    (forward-char 1)
	    (while (and (< (point) indent-point)
			(condition-case nil
			    (progn
			      (forward-sexp 1)
			      (if (> count spec-count)
				  (progn
				    (backward-sexp 1)
				    (if (funcall predicate (point))
					(setq spec-count (1+ spec-count)))
				    (forward-sexp 1)))
			      (setq count (1+ count))
			      (fi::parse-partial-sexp
			       (point) indent-point 1 t nil
			       fi::lisp-indent-state-temp))
			  (error nil))))
	    (if (<= count spec-count)
		nil
	      (progn
		(goto-char indent-point)
		(beginning-of-line 1)
		(skip-chars-forward " \t")
		(if (funcall predicate (point))
		    (list
		     (max normal-indent
			  (+ containing-form-column (* 2 fi:lisp-body-indent)))
		     containing-form-start)
		  nil)))))
	(fi::lisp-indent-specform spec-count depth count state indent-point))))

(defun fi::lisp-form-declare-p (sexp)
  (let ((car (fi::lisp-form-car sexp)))
    (and car
	 (funcall (if fi:indent-methods-case-sensitive
		      'string-equal
		    'fi::string-equal-nocase)
		  "declare"
		  car))))

(defun fi::lisp-form-car (at)
  "Return a string that is the car of the form at point AT."
  (save-excursion
    (goto-char at)
    (if (= (following-char) ?\()
	(forward-char 1))
    (condition-case nil
	(let (end)
	  (forward-sexp 1)
	  (setq end (point))
	  (backward-sexp 1)
	  (buffer-substring (point) end))
      (error nil))))

(defun fi::lisp-indent-specform (spec-count depth count state indent-point)
  (if (> depth 0)
      nil
    (let ((containing-form-start (car (cdr state))) (i spec-count)
	  body-indent containing-form-column)
      ;; Move to the start of containing form, calculate indentation
      ;; to use for non-distinguished forms (> spec-count), and move past the
      ;; function symbol.  fi:lisp-indent-hook guarantees that there is at
      ;; least one word or symbol character following open paren of containing
      ;; form.
      (goto-char containing-form-start)
      (setq containing-form-column (current-column))
      (setq body-indent (+ fi:lisp-body-indent containing-form-column))
      (forward-char 1)
      (forward-sexp 1)
      ;; Now find the start of the last form.
      (fi::parse-partial-sexp (point) indent-point 1 t nil
			  fi::lisp-indent-state-temp)
      (while (and (< (point) indent-point)
		  (condition-case nil
		      (progn
			(setq spec-count (1- spec-count))
			(forward-sexp 1)
			(fi::parse-partial-sexp (point) indent-point 1 t nil
					    fi::lisp-indent-state-temp))
		    (error nil))))
      ;; Point is sitting on first character of last (or spec-count) sexp.
      (if (> spec-count 0)
	  ;; A distinguished form.  If it is the first or second form use
	  ;; double fi:lisp-body-indent, else normal indent.  With
	  ;; fi:lisp-body-indent bound to 2 (the default), this just happens
	  ;; to work the same with if as the older code, but it makes
	  ;; unwind-protect, condition-case, with-output-to-temp-buffer,
	  ;; et. al. much more tasteful.  The older, less hacked, behavior
	  ;; can be obtained by replacing below with `(list normal-indent
	  ;; containing-form-start)'.
	  (if (<= (- i spec-count) 1)
	      (list (+ containing-form-column (* 2 fi:lisp-body-indent))
		    containing-form-start)
	    (list normal-indent containing-form-start))
	;; Non-distinguished form. Use body-indent if there are no
	;; distinguished forms and this is the first undistinguished form,
	;; or if this is the first undistinguished form and the preceding
	;; distinguished form has indentation at least as great as
	;; body-indent.
	(if (or (and (= i 0) (= spec-count 0))
		(and (= spec-count 0) (<= body-indent normal-indent)))
	    body-indent
	  normal-indent)))))

(defun fi:indent-sexp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) bol
	outer-loop-done state this-indent
	(current-parse-result nil)
	(previous-parse-result nil))
    ;; Get error now if we don't have a complete sexp after point.
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
	(setq last-depth next-depth
	      innerloop-done nil)
	;; Parse this line so we can learn the state
	;; to indent the next line.
	;; This inner loop goes through only once
	;; unless a line ends inside a string.
	(while (and (not innerloop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq previous-parse-result current-parse-result)
	  (setq state
	    (fi::parse-partial-sexp (point) (progn (end-of-line) (point))
				    nil nil state))
	  (setq current-parse-result (cons (point) state))
	  (setq next-depth (car state))
	  (if (nth 4 state)
	      (let (comment-at)
		(beginning-of-line 1)
		(if (setq comment-at (fi::find-line-comment))
		    (save-excursion
		      (goto-char comment-at)
		      (if previous-parse-result
			  (fi::indent-lisp-semicolon
			   (car previous-parse-result)
			   (cdr previous-parse-result))
			(fi::indent-lisp-semicolon))))
		(end-of-line)
		(setcar (nthcdr 4 state) nil)))
	  (if (nth 3 state)
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq innerloop-done t)))
	(if (setq outer-loop-done (<= next-depth 0))
	    nil
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  ;; Now go to the next line and indent it according
	  ;; to what we learned from parsing the previous one.
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  ;; But not if the line is blank, or just a comment
	  ;; (except for double-semi comments; indent them as usual).
	  (if (or (eobp) (looking-at "[;\n]"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (fi::calculate-lisp-indent
			  (if (car indent-stack) (- (car indent-stack))))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))))))

(defun fi:indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them. Called from a program, takes
args START, END, COLUMNS and NOCHANGE-REGEXP. The last is a regexp which,
if matched at the beginning of a line, means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state
	    (fi::parse-partial-sexp (point) (progn (forward-line 1) (point)))))
      (if (null state) (setq state '(nil nil nil nil nil nil nil)))
      (while (< (point) end)
	(or (nth 3 state)		; in string?
	    (nth 4 state)		; in comment?
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string or comment, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point)
			     (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (fi::parse-partial-sexp
		     (point) (progn (forward-line 1) (point))
		     nil nil state state))))))

(defun fi::find-line-comment ()
  "Return point of first comment character on this line, or nil."
  (save-excursion
    (let ((end-of-line-point (progn (end-of-line nil) (point))))
      (beginning-of-line nil)
      (if (re-search-forward comment-start-skip end-of-line-point t 1)
	  (match-beginning 0)
	nil))))

(defun fi::string-equal-nocase (a b)
  "Returns NIL if the two string arguments are not equal, case ignored."
  (string-equal (upcase a) (upcase b)))

;;; Lisp form indentation specifications.
;;; Note that `t' specifies that the form is not special and `shadows'
;;;   any indentation specified with the property stored under the
;;;   indicator `fi:lisp-indent-hook'.

;;A note on indenting methods:
;;
;; Triples are: depth, count and method.  In the following example, the
;; indentation method is a list of two lists, (1 (2 t)((1 0 quote)(0 t nil)))
;; and (0 t 1).  The first triple is for sexps at level 1 (ie, the arguments
;; to CASE).  The second triple is for the CASE sexp itself (ie, the
;; indentation applies to the elements of the CASE expression, not the
;; indentation of the individual elements within the CASE).  Note also that
;; triples for deeper sexps come first (ie, ordered depending on descending
;; depth).  For depth 1, the count is (2 t), which means apply the method
;; to all but the elements 0 and 1 of the CASE (0 is CASE and 1 is the
;; first argument to the CASE).  The method, ((1 0 quote) (0 t nil))
;; recursively defines what happens inside each of these elements in the
;; CASE (ie, refered to by a count of (2 t)): at depth 1 and count 0, the
;; elements are indented as quoted lists (aligned under the CAR); at depth
;; 0 for any element the standard indentation applies.
;;
;;(put 'case 'fi:lisp-indent-hook
;;     '((1 (2 t) ((1 0 quote)
;;		   (0 t nil)))
;;       (0 t 1)))

(let ((tag 'fi:lisp-indent-hook))
  (put 'assert tag '((1 2 quote) (0 t 2)))
  (put 'block tag 1)
  (put 'case tag '((1 (2 t) ((1 0 quote) (0 t nil))) (0 t 1)))
  (put 'catch tag 1)
  (put 'ccase tag '(like case))
  (put 'check-type tag 2)
  (put 'concatenate tag 1)
  (put 'ctypecase tag '(like case))
  (put 'defconstant tag '(like defvar))
  (put 'define-compiler-macro tag '(like defmacro))
  (put 'define-modify-macro tag '((1 2 lambda-list) (0 t 2)))
  (put 'define-setf-method tag '((1 2 lambda-list) (0 t 2)))
  (put 'defmacro tag '((1 2 lambda-list) (0 t 2)))
  (put 'defparameter tag '(like defvar))
  (put 'defsetf tag '((1 2 lambda-list) (0 t 3)))
  (put 'defstruct tag '((1 1 quote) (0 t 1)))
  (put 'deftype tag '((1 2 lambda-list) (0 t 2)))
  (put 'defun tag '((1 2 lambda-list) (0 t 2)))
  (put 'defvar tag 2)
  (put 'do tag '((1 1 quote) (1 2 quote) (0 t (fi:lisp-indent-tagbody 2))))
  (put 'do* tag '(like do))
  (put 'do-all-symbols tag '(like dolist))
  (put 'do-external-symbols tag '(like dolist))
  (put 'do-symbols tag '(like dolist))
  (put 'dolist tag '((1 1 1) (0 t (fi:lisp-indent-tagbody 1))))
  (put 'dotimes tag '(like dolist))
  (put 'ecase tag '(like case))
  (put 'etypecase tag '(like case))
  (put 'eval-when tag 1)
  (put 'flet tag '((2 1 ((1 1 lambda-list) (0 t 1))) (0 t 1)))
  (put 'if tag 2)
  (put 'labels tag '(like flet))
  (put 'lambda tag '((1 1 lambda-list) (0 t 1)))
  (put 'let tag '((1 1 quote) (0 t 1)))
  (put 'let* tag '(like let))
  (put 'map tag 1)
  (put 'multiple-value-bind tag '((1 1 quote) (0 t 2)))
  (put 'multiple-value-call tag 1)
  (put 'multiple-value-prog1 tag 1)
  (put 'multiple-value-setq tag '((1 1 quote) (0 t 1)))
  (put 'prog tag '((0 1 1) (0 t tagbody)))
  (put 'prog* tag '(like prog))
  (put 'prog1 tag 1)
  (put 'prog2 tag 2)
  (put 'progn tag 0)
  (put 'progv tag 2)
  (put 'psetf tag 1)
  (put 'psetq tag 1)
  (put 'return tag 0)
  (put 'return-from tag 1)
  (put 'setf tag 1)
  (put 'setq tag 1)
  (put 'tagbody tag 'tagbody)
  (put 'the tag 1)
  (put 'throw tag 1)
  (put 'typecase tag '(like case))
  (put 'unless tag 1)
  (put 'unwind-protect tag 1)
  (put 'when tag 1)
  (put 'while tag 1)
  (put 'until tag 1)
  )

(let ((tag 'fi:common-lisp-indent-hook))

  ;; generic Common Lisp

  (put 'compiler-let tag '((1 1 quote) (0 t 1)))
  (put 'defclass tag '((1 2 quote) (0 t 2)))
  (put 'defgeneric tag
       (quote (if (fi:lisp-atom-p 2)
		  ((1 3 lambda-list) (0 t 3))
		((1 2 lambda-list) (0 t 2)))))
  (put 'define-condition tag '((1 1 quote) (0 t 2)))
  (put 'defmacro tag '((1 2 (recursive lambda-list)) (0 t 2)))
  (put 'defmethod tag
       (quote (if (fi:lisp-atom-p 2)
		  ((1 3 lambda-list) (0 t 3))
		((1 2 lambda-list) (0 t 2)))))
  (put 'defpackage tag 1)
  (put 'defsetf tag '(if (fi:lisp-atom-p 2) 2 ((1 2 lambda-list) (0 t 3))))
  (put 'destructuring-bind tag '((1 1 lambda-list) (0 t 2)))
  (put 'handler-bind tag '(like let))
  (put 'handler-case tag '(like restart-case))
  (put 'locally tag '(funcall fi::lisp-indent-predicated-special))
  (put 'loop tag
       '((0 t (fi:lisp-indent-keyword-list
	       nil			; quotedp
	       nil			; keyword-arg-pairs-p
	       t			; keyword-count
	       t			; special-keyword-count
	       0			; special-count
	       nil			; ignore-after-count
	       ;; keywords recognized:
;;;; ANSI loop
	       "for" "as" "and"
	       "from" "downfrom" "upfrom" "to" "downto" "upto" "below"
	       "above" "by" "in" "on" "=" "then" "across" "being" "each"
	       "the" "hash-key" "hash-keys" "hash-value" "hash-values" "of"
	       "symbol" "present-symbol" "external-symbol"
	       "symbols" "present-symbols" "external-symbols"
	       "repeat" "while" "until" "always" "never" "thereis"
	       "collect" "collecting" "nconc" "nconcing"
	       "append" "appending" "sum" "summing"
	       "maximizing" "minimizing"
	       "maximize" "minimize"
	       "count" "counting"
	       "if" "unless" "when" "else" "end"
	       "do" "doing" "return"
	       "initially" "finally"
	       "with" "into" "it" "of-type" "using"

;;;; extended loop
	       "named"
;;;; old loop
	       "first"
;;;; ?
	       "nodeclare"))))
  (put 'macrolet tag '(like flet))
  (put 'make-instance tag 1)
  (put 'make-condition tag '(like make-instance))
  (put 'named-function tag 1)
  (put 'named-lambda tag '(like defun))
  (put 'pprint-logical-block tag 1)
  (put 'print-unreadable-object tag '(like with-open-file))
  (put 'restart-bind tag
       '((2 1 ((0 t (fi:lisp-indent-keyword-list
		     nil		; quotedp
		     t			; keyword-arg-pairs-p
		     2			; keyword-count
		     2			; special-keyword-count
		     1			; special-count
		     t			; ignore-after-count
		     ;; keywords recognized:
		     ":interactive-function" ":report-function"
		     ":test-function"))))
	 (0 t 1)))
  (put 'restart-case tag
       '((1 (2 t) ((1 1 lambda-list)
		   (0 t (fi:lisp-indent-keyword-list
			 nil		; quotedp
			 t		; keyword-arg-pairs-p
			 2		; keyword-count
			 2		; special-keyword-count
			 1		; special-count
			 t		; ignore-after-count
			 ;; keywords recognized:
			 ":report" ":interactive" ":test"))))
	 (0 t 1)))
  (put 'symbol-macrolet tag '(like flet))
  (put 'with-accessors tag '(like with-slots))
  (put 'with-condition-restarts tag 2)
  (put 'with-hash-table-iterator tag 1)
  (put 'with-input-from-string tag '((1 1 quote) (0 t 1)))
  (put 'with-open-file tag '((1 1 quote) (0 t 1)))
  (put 'with-open-stream tag '((1 1 quote) (0 t 1)))
  (put 'with-output-to-string tag '((1 1 quote) (0 t 1)))
  (put 'with-package-iterator tag 1)
  (put 'with-simple-restart tag '(like when))
  (put 'with-slots tag '((1 1 quote) (0 t 2)))
  (put 'with-standard-io-syntax tag '(like progn))

  ;; Allegro CL

  (put 'alias tag 2)
  (put 'def-c-type tag 1)
  (put 'def-c-typedef tag 1)
  (put 'def-format-parser tag '(like defun))
  (put 'def-format-runtime tag '(like defun))
  (put 'def-function-spec-handler tag 2)
  (put 'def-tr tag 3)
  (put 'defadvice tag 2)
  (put 'defcmacro tag '(like defmacro))
  (put 'defcstruct tag 1)
  (put 'defforeign tag 2)
  (put 'defresource tag 1)
  (put 'defsystem tag '((1 2 quote) (0 t 2)))
  (put 'defun-c-callable tag '(like defun))
  (put 'ensuring-compiled-body tag '(like progn))
  (put 'if* tag '(funcall fi:lisp-indent-if*))
  (put 'process-run-function tag 1)
  (put 'setq-default tag 1)
  (put 'with-process-lock tag 1)
  (put 'with-profiling tag '((1 1 quote) (0 t 1)))
  (put 'with-resource tag 1)
  (put 'with-timeout tag 1)
  (put 'without-interrupts tag 0)
  (put 'without-scheduling tag 0)

  ;; no SMP macros
  (put 'critical-section tag 1)
  (put 'defvar-nonbindable tag 2)
  (put 'fast-and-clean tag 0)
  (put 'with-delayed-interrupts tag 0)
  (put 'with-locked-object tag 1)
  (put 'with-locked-object-released tag 1)
  (put 'with-locked-stream tag 1)
  (put 'with-locked-structure tag 1)
  (put 'with-pinned-objects tag 1)

  ;; for clim 2.x
  (put 'dolist-noting-progress tag '(like dolist))
  (put 'dotimes-noting-progress tag '(like dolist))
  (put 'indenting-output tag 1)
  (put 'filling-output tag 1)
  (put 'dragging-output tag 1)
  (put 'noting-progress tag 1)
  (put 'vertically tag 1)
  (put 'bulletin-board tag 1)
  (put 'scrolling tag 1)
  (put 'horizontally tag 1)
  (put 'bordering tag 1)
  (put 'spacing tag 1)
  (put 'outlining tag 1)
  (put 'tabling tag 1)
  (put 'labelling tag 1)
  (put 'updating-output tag 1)
  (put 'surrounding-output-with-border tag 1)
  (put 'formatting-item-list tag 1)
  (put 'formatting-table tag 1)
  (put 'formatting-row tag 1)
  (put 'formatting-column tag 1)
  (put 'formatting-cell tag 1)
  (put 'tracking-pointer tag 1)
  (put 'with-bounding-rectangle* tag 2)
  (put 'with-scaling tag 1)
  (put 'with-rotation tag 1)
  (put 'with-presentation-type-decoded tag 1)
  (put 'letf-globally tag 1)
  (put 'horizontally tag 1)
  (put 'catch-abort-gestures tag 1)
  (put 'accepting-values tag 1)
  (put 'accept-values-command-button tag 2)
  (put 'changing-space-requirements tag 1)
  (put 'define-application-frame tag '(like defclass))

  ;; for AllegroStore:
  (put 'for-each tag '(like let))
  (put 'for-each-class tag 1)
  (put 'for-each-scratch-list tag '(like dolist))
  
  ;; AllegroCache
  (put 'doclass tag '(like dolist))
  (put 'doclass* tag '(like dolist))
  (put 'doset tag '(like dolist))

  ;; AllegroGraph
  (put 'select tag '(like dolist))
  (put 'select0 tag '(like dolist))
  (put 'select-distinct tag '(like dolist))
  (put 'select0-distinct tag '(like dolist))
  (put '<-- tag '(like dolist))
  (put '<- tag '(like dolist))
  (put 'select/callback tag '(like defun))
  (put 'select0/callback tag '(like defun))
  (put 'select-distinct/callback tag '(like defun))
  (put 'select0-distinct/callback tag '(like defun))
  )

(let ((tag 'fi:franz-lisp-indent-hook))
  (put '*catch tag 1)
  (put '*throw tag 1)
  (put 'c-declare tag '((1 t 1) (0 t 0)))
  (put 'caseq tag '(like case))
  (put 'def tag '((1 2 ((1 1 lambda-list) (0 t 1))) (0 t 1)))
  (put 'defcmacro tag '((1 2 lambda-list) (0 t 2)))
  (put 'defflavor tag '((1 2 quote) (1 3 quote) (0 t 3)))
  (put 'defmethod tag '((1 1 quote) (1 2 lambda-list) (0 t 2)))
  (put 'defsubst tag '((1 2 lambda-list) (0 t 2)))
  (put 'defwhopper tag '((1 1 quote) (1 2 lambda-list) (0 t 2)))
  (put 'defwrapper tag '((1 1 quote) (1 2 lambda-list) (0 t 2)))
  (put 'errset tag 1)
  (put 'fexpr tag '((1 1 lambda-list) (0 t 1)))
  ;;(put 'if tag '(like if*))
  ;;(put 'label tag '((1 2 ((1 1 lambda-list) (0 t 1))) (0 t 1)))
  (put 'let-closed tag '((1 1 quote) (0 t 1)))
  (put 'lexpr tag '((1 1 lambda-list) (0 t 1)))
  (put 'macro tag '((1 1 lambda-list) (0 t 1)))
  (put 'nlambda tag '((1 1 lambda-list) (0 t 1)))
  )

(let ((tag 'fi:emacs-lisp-indent-hook))
  (put 'condition-case tag 2)
  (put 'defconst tag 2)
  (put 'fi::make-request tag
       '((1 (2 t)
	    ((1 (0 1) quote)
	     (0 t quote)))
	 (0 t 1)))
  (put 'save-excursion tag 0)
  (put 'save-restriction tag 0)
  (put 'save-window-excursion tag 0)
  (put 'setq-default tag 1)
  (put 'while tag 1)
  (put 'with-keywords tag 2)
  (put 'with-output-to-temp-buffer tag 1)
  (put 'if* tag '(funcall fi:lisp-indent-if*))
  )
