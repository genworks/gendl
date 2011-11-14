

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

Use cl-lite on the ledger/ directory. See bootstrap.txt in the
<gdl-home>/gdl-doc/ directory for more details on cl-lite.

Once the application is compiled:

 1. Make sure allegroserve is started with 

       (net.aserve:start :port 9000)  ;; or whatever number you choose

 2. Visit:

      http://<your host>:<port number>/ledger

      e.g. http://localhost:9000/ledger



