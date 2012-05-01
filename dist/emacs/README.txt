

            Welcome to Emacs with Genworks GDL

            Genworks(R) --- Generate Tomorrow!



Learning Emacs
==============

If you are new to Emacs, you can get an Emacs Tutorial under the Help
menu above. After completing the Tutorial, try to practice what you
learned and not use the mouse too much in Emacs.


Learning GDL
============

Documentation:

  The main entry point to GDL documentation


     http://www.genworks.com/downloads/customer-documentation

  For the latest Reference documentation, ask your locally running GDL
  instance:

     http://localhost:9000/yadd  

     (substitute the webserver port you use in your gdlinit.cl here)



Startup
=======

Now, you can start GDL itself by typing:

    M-x gdl (that's hold down the Meta (or Alt) key, and type x, then
            type 'gdl' and hit Enter.


After starting GDL, you will probably want to get the latest updates
and extra modules, and start the webserver and possibly load the
geometry kernel, with:

       (update-gdl)


See the yadd documentation on update-gdl or gdl-updater for many more
options.


Quitting
========

When you have had enough of using GDL and Emacs, you should do two
things:

 1. type :exit (and hit Return) at the command prompt in the 
    *gdl toplevel* buffer to kill the GDL process.

 2. type C-x C-c (that's Control-x, Control-c) to kill the Emacs
    process and exit the window.



Emacs Reference
===============

The following is a reference for the Emacs keychords which are
commonly used when working with GDL. They will be understandable after
you completed the Emacs Tutorial mentioned above. Don't be scared,
after a few days of practice, these will become like old friends.

It is also an excellent idea (if you have admin priveleges on your
machine) to map your Caps Lock key to be a second Control key. There
is a Windows registry hack which we can recommend (don't worry, it's
safe) here (there's a .reg for undoing the mapping too, if need be):
      
  http://johnhaller.com/jh/useful_stuff/disable_caps_lock/
  http://johnhaller.com/jh/useful_stuff/disable_caps_lock/caps_lock_to_control.reg

Linux users should already know how to do that mapping, ask the Google
if you don't.


;; Backing out from a minibuffer command C-g quit the active command



;;
;; Navigating Through the Buffer
;;
C-n next line
C-p previous line
C-f forward char
M-f forward word
C-M-f forward s-expression
C-b backward char
M-b backward word
C-M-b backward s-expression
C-a begin of line
C-e end of line

;;
;; Auto-completion
;;
M-/   complete word

C-c ]  "super-bracket" -- close parens to enclosing toplevel.

;;
;; Copying, Cutting, Pasting, working with Lisp S-expressions
;;
C-d   delete character
M-d   delete word
C-M-k kill s-expression
C-<space> begins a selection
C-M-<space> selects current sexpression
M-w copies selection
C-w cuts selection
C-y yanks (pastes) the selection
M-y yanks previous 
C-k kills current line
C-x u  Undo!
M-<space> removes all but one whitespace under point.
M-\ removes all whitespace under point.

;;
;; Indenting
;;
C-M-q indents entire expression

C-q indents and word-wraps a paragraph of text to reasonable line
    length.

;;
;; Working with Files & Buffers
;;
C-x C-b get list of open buffers
C-x k   kills current buffer
C-x b   visit other buffer (default is previous)
C-x C-s   saves current buffer to file
C-x C-w   saves current buffer to a new file
C-x C-f   find file (i.e. file open)

;;
;; Dired (Directory editor) mode
;;
C-x C-f  to visit a directory, then <Enter> for Directory Editor (Dired)
Within Dired: 

  d  marks file for deletion
  f  visits the file or directory
  ~  marks all backup files for deletion
  x  executes pending commands (usually deletions)

;;
;; Windows and Frames
;;
C-x 2 split screen into two windows
C-x 1 make current window be the only window
C-x 0 close current window
C-x o change to next window
C-x y change to previous window
C-x 2 5 Open a new Frame (a new desktop window)
C-x & Jump to *gdl-devo* buffer

;;
;; Command-line Shortcuts
;;
M-p   bring back previous command
M-n   bring back next command



;;
;; Compiling and Loading from Buffer
;;
C-c C-b Compile Buffer
C-c C-f Compile Form
