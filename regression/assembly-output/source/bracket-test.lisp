(in-package :bracket)

(define-object bracket-test (base-object)

  :objects
  ((bracket :type 'simple-bracket))

  :functions
  ((step-out!
    ()
    (with-format (step "/tmp/try.stp" :assembly? t) (write-the bracket cad-output-tree)))))