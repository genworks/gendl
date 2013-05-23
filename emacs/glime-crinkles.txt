

1. Reference-chains entered into Slime REPL throw an "error in process
   filter" e.g.:

   GDL-USER> 
   ;; Error: Odd length keyword list: (SWANK::%CURSOR-MARKER%)
   ;; Error: Odd length keyword list: (SWANK::%CURSOR-MARKER%)
   ;; Error: Odd length keyword list: (SWANK::%CURSOR-MARKER%)
   ; No value
   GDL-USER> (the foo ...

 
   This is fixed temporarily with an ignore-errors around body of
   this-the-from-form in emacs/glime.lisp genworks' current
   slime-devel branch. Ignore-errors should probably still be there
   just to catch any unforeseen situations, but probably with a
   multiple-value-bind to catch the error and handle it somehow or at
   least print a warning message about it.

2. the-child is not picking up the type of current child -- it still
   appears to be working based off the enclosing define-object type. 

3. When placing the "(" after "(the (" it does toggle to display
   available GDL Functions, but it should also suppress display of
   non-Function slots, and it does not appear to be doing this.

4. slime-autodoc-use-multiline-p does not appear to be activated.

5. Sequence (quantified) objects are not behaving like GDL Functions
   for "(the (" as they should. 

6. Reference-chains do not appear to pick up the inferred type of each
   element along the chain, as theoretically should (at least
   sometimes) be possible, using e.g. (the (slot-source ...))  and
   pulling out the :type for messages which name :objects
   or :hidden-objects.

7. Functions-in-this-form (which are not compiled into object yet) are
   not being picked up.
     
8. Things to start thinking about for future: What are some good
   places/contexts to start pulling up and presenting the
   docstrings (i.e. the message-remarks) for
   input-slots/computed-slots/functions etc?  (so far we haven't done
   anything with message-remarks). And how about best ways to
   specify/interrogate/present documentation for individual arguments
   to GDL Functions? (currently they are documented using a tortured
   convention within the doc-string, which has to be kept manually in
   sync with the actual lambda list --- it would be nice to be able to
   have the documentation embedded right there in the lambda list
   along with the args --- borrowing some meta-data ideas from Clojure
   perhaps?)


