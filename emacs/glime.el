(require 'cl)
(require 'slime)
(require 'advice)

;; TODO: this should be a gendl-swank version number and should be set from lisp side...
(defvar *enable-glime* t)

(defadvice slime-contextual-completions (around glime-completion (beg end) activate)
  (if *enable-glime*
    (setq ad-return-value (glime-contextual-completions beg end))
    ad-do-it))

;; Do this instead of slime-contextual-completions in order to enable message name
;; completion of non-keyword symbols.
(defun glime-contextual-completions (beg end)
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (if (and (>= (length token) 2)
             (string= (cl-subseq token 0 2) "#\\"))
      (slime-completions-for-character token)
      (glime-completions token
                         (save-excursion 
                           (goto-char beg)
                           (slime-parse-form-upto-point))))))


(defun glime-completions (prefix buffer-form)
  (slime-eval `(gendl-swank:glime-completions ,prefix ',(slime-current-package) ',buffer-form)))
