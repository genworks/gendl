(in-package :gdl-user)

(define-object-macro-toplevel :super-inputs (messages)
  `(:input-slots ,messages))


(define-object toplevel ()
  :super-inputs (hey now))


(define-object broken ()
  :silly-inputs (hey now))
