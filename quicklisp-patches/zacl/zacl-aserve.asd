;;;; zacl-aserve.asd

(asdf:defsystem #:zacl-aserve
  :description "A shim system to load ZACL before loading
  AllegroServe."
  :license "BSD"
  :author "Zach Beane <xach@xach.com>"
  :depends-on (#:zacl #:aserve))
