(in-package :user)

(defun file-size (pathname)
  (with-open-file (in pathname)
    (do ((line (read-line in nil nil) (read-line in nil nil)))
	((null line)(file-position in)))))

(defun read-large (pathname starting-at &key from-end?)
  "List of strings. Returns a list with one string for each line in ascii file,
starting from position given by starting-at."
  (with-open-file (in pathname)
    (file-position in (if from-end? (- (file-length in) starting-at)
			starting-at))
    (let (result)
      (do ((line (read-line in nil nil)(read-line in nil nil)))
	  ((null line) (nreverse result))
	(push line result)))))
