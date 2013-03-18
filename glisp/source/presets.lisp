(in-package :com.genworks.lisp)

;;
;; FLAG --- need to use asdf around-compile method (or something like
;; it) to set these only temporarily.
;;
(setq cl-who:*prologue* "<!doctype HTML>")
(setq cl-who:*attribute-quote-char* #\")
(setq cl-who:*downcase-tokens-p* nil)
