

     Personal Accounting Ledger GDL/GWL Sample Application
     =====================================================

This is the original Ledger example and uses the old-style user
interface approach. It is not recommended to continue with this
approach; the preferred approach is to use form-control objects for
the form fields, as will be in other newer examples soon to be added
to the src/ directory distributed with GDL.

We hope to update it to use form-controls and AJAX at some
point. Currently it uses simple :settable computed-slots along with
explicit inline HTML forms and form fields (input tags), instead of
the more modern and preferred form-control objects
(e.g. text-form-control, menu-form-control, etc).

 

To compile the app
==================

1. Set yourself up with quicklisp and asdf, possibly with 

 (load (merge-pathnames "quicklisp/setup.lisp" glisp:*gdl-home*))

2. (ql:quickload :ledger)


Once the application is compiled:

 1. Make sure allegroserve is started with 

       (gendl:start-gendl!)   ;; or 
       (gwl:start-gwl :port 9000)  ;; or whatever port number you choose

 2. Visit:

      http://<your host>:<port number>/ledger

      e.g. http://localhost:9000/ledger





