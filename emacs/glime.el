(require 'cl)
(require 'slime)
(require 'advice)

(defvar *enable-glime* t)

(defun enable-glime (&optional process)
  (and *enable-glime*
       (member :glime (slime-lisp-features (or process (slime-connection))))))

(defun glime-form-at-pos (pos)
  (save-excursion (goto-char pos) (slime-parse-form-upto-point)))

(defun glime-documentation ()
  "Display documentation for symbol at point"
  (interactive)
  (let* ((bounds (slime-bounds-of-symbol-at-point)))
    (if bounds
      (glime-contextual-documentation (car bounds) (cdr bounds))
      (let ((symbol-name (slime-read-from-minibuffer "Documentation for symbol: ")))
        (when symbol-name
          (slime-documentation symbol-name))))))

(defun glime-contextual-documentation (beg end)
  (let* ((token (buffer-substring-no-properties beg end)))
    (if (enable-glime)
      (slime-eval-describe
       `(gendl-swank:glime-documentation ,token ',(slime-current-package) ',(glime-form-at-pos beg)))
      (slime-documentation token))))


;; Patched to enable message name completion of non-keyword symbols.
(defadvice slime-contextual-completions (around glime-completion (beg end) activate)
  (if (enable-glime)
    (setq ad-return-value (glime-contextual-completions beg end))
    ad-do-it))

(defun glime-contextual-completions (beg end)
  "Return a list of completions of the token from BEG to END in the current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (if (and (>= (length token) 2)
             (string= (cl-subseq token 0 2) "#\\"))
      (slime-completions-for-character token)
      (slime-eval `(gendl-swank:glime-completions ,token ',(slime-current-package) ',(glime-form-at-pos beg))))))


(let ((entry (list ?s 'glime-documentation)))
  (let ((old-entry (find (cadr entry) slime-doc-bindings :key 'cadr)))
    (when old-entry (setq slime-doc-bindings (remove old-entry slime-doc-bindings))))
  (push entry slime-doc-bindings)
  (slime-bind-keys slime-doc-map t (list entry)))
