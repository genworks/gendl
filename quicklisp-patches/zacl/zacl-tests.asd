;;;; zacl-tests.asd

(asdf:defsystem #:zacl-tests
  :serial t
  :depends-on (#:zacl #:fiveam #:aserve)
  :components ((:file "tests")))
