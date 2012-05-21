;; Date: Wed, 13 Aug 2003 11:29:42 -0600
;; From: "Clementson, Bill"
;;
;; I frequently use the following function in place of the special-purpose
;; eval-compile functions. You might want to consider something similar for
;; inclusion in ELI (it could be bound to "C-c C-d"):
;; $Id: eval.el,v 3.0 2003/12/15 22:52:58 layer Exp $

(defun fi:lisp-eval-or-compile-dwim (compilep)
  "Send the appropriate forms to the Lisp subprocess associated with
this buffer (Do What I Mean).  If a region is selected, evaluate the
region.  If the cursor is on or immediately after a ')', evaluate the
last sexp.  If the cursor is on or immediately before a '(', evaluate
the next sexp. If the cursor is inside a defun, evaluate the defun. If
the cursor is inside a top-level sexp, evaluate the top-level
sexp. Tests are done in the order specified in these comments, so if
there is any ambiguity, make certain that the cursor is either on a
parenthesis (for the eval last/next commands or not directly
before/after/on a parenthesis for the eval defun/top-level commands.
See the documentation for fi:lisp-evals-always-compile."
  (interactive (fi::decode-prefix-argument-for-eval-or-compile))
  (save-excursion
    (cond 
      ;;Region selected - evaluate region
      ((not (equal mark-active nil))
       (fi:lisp-eval-or-compile-region compilep))
      ;; At/after sexp - evaluate last sexp
      ((or (looking-at "\\s\)")
	   (save-excursion
	     (backward-char 1)
	     (looking-at "\\s\)")))
       (if (looking-at "\\s\)")
	   (forward-char 1)) 
       (fi:lisp-eval-or-compile-last-sexp compilep))
      ;; At/before sexp - evaluate next sexp
      ((or (looking-at "\\s\(")
	   (save-excursion
	     (forward-char 1)
	     (looking-at "\\s\(")))
       (if (looking-at "\\s\(")
	   (forward-list 1)) 
       (fi:lisp-eval-or-compile-last-sexp compilep))
      ;; Default - evaluate enclosing defun/sexp
      (t (fi:lisp-eval-or-compile-defun compilep)))))
