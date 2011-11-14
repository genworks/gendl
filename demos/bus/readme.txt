
           School Bus GDL/GWL Sample Application
           =====================================


This is the original School Bus example and uses the old-style user
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

Use cl-lite on the bus/ directory.

Once the application is compiled:

 1. Make sure allegroserve is started with 

       (gwl:start-gwl)

 2. Visit:

      http://localhost:<port number>/demos/bus


