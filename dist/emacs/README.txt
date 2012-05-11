

            Welcome to the Gendl™ Environment


===============
Emacs and Gendl
===============

Although you are free to use other editors or IDEs, spending some time
to get familiar with Emacs is the best small investment you can make
for working with a Lisp-based system like Gendl. Genworks supports two
distinct emacs-based IDEs for Gendl development: Franz ELI and SLIME
(Superior Lisp Interaction Mode for Emacs). Slime works across all
major OS platforms and CL implementations, is well-supported by the
Common Lisp community. Genworks plans to continue adding specialized
Gendl support to Slime.

If you are new to Emacs, you can get a general Emacs Tutorial under
the Help menu above. After completing the Tutorial, try to practice
what you learned by forcing yourself not to use the mouse too much in
Emacs.

Touch-typing ability is definitely an asset for Gendl development, but
with auto-completion of symbols (possible with M-/ -- you'll know what
that means after looking at the tutorial!) it is possible to be an
extremely effective Gendl developer without having world-class typing
speed.

It is strongly recommended to map your Caps Lock key to be a Control
key, if you have sufficient priveleges on your machine. This will give
you a much more comfortable experience, allowing you to use all the
Emacs navigation keystrokes without constantly curling your left pinky
finger down to the Control key.  Here are some resources which can
help you to this:

      
  http://emacswiki.org/emacs/MovingTheCtrlKey
  http://www.howtogeek.com/howto/windows-vista/disable-caps-lock-key-in-windows-vista/


Finally, please see the Genworks Documentation (URL below) for an
introductory video on using Emacs in the Gendl environment.



===============
Learning Gendl
===============

Absolutely the best way to learn Gendl is to dive in and start using
it for small exercises and projects.

Documentation:

  The main entry point to GDL documentation is here:

     http://www.genworks.com/newsite

  Then click the link for Documentation.


 There are videos and a tutorial manual. We hope to update this site
 frequently so please come round often.



Startup
=======

There are two ways to start the Gendl environment. The first is Slime,
the Superior Lisp Interaction Mode for Emacs. Slime enjoys wide
community support and works across all major OS platforms and CL
implementations. The second is Franz ELI (Emacs Lisp Interface). ELI
works with Allegro CL-based Gendl environments (currently supported by
Genworks on Linux and Windows) and receives commercial support from
Franz Inc.


1. Slime
=========

Starting:

   M-x slime  (that is, hold down the Meta (or Alt) key, and type x,
               then type 'slime' and hit Enter.)


Quitting: 

  When you have had enough of using Gendl with Slime, you should do
  two things:

 1. type M-x slime-quit-lisp (and hit Return) from anywhere in Emacs,
    to kill the GDL process.

 2. (optional) type C-x C-c (that's Control-x, Control-c) to kill the Emacs
    process and exit the window.
    
 3. If you kept Emacs up and running, it is possible to restart the
    Gendl session with either ELI or Slime, by following the
    instructions under "Starting."



2. ELI
=======

Starting:


    M-x gdl (that is, hold down the Meta (or Alt) key, and type x,
             then type 'gdl' and hit Enter.)


Quitting:


  When you have had enough of using Gendl with ELI, you should do two
  things:

 1. type :exit (and hit Return) at the command prompt in the *gdl
    toplevel* buffer to kill the GDL process.

 2. (optional) type C-x C-c (that's Control-x, Control-c) to kill the
    Emacs process and exit the window.

 3. If you kept Emacs up and running, it is possible to restart the
    Gendl session with either ELI or Slime, by following the
    instructions under "Starting."



Emacs Reference
===============

The following is a reference for the Emacs keychords which are
commonly used when working with GDL. They will be understandable after
you completed the Emacs Tutorial mentioned above. If you force
yourself to use these for a few days, they will soon become like old
friends.

;;
;; Backing out of a command (important! use this whenever you are lost)
;;

C-g  : quit the active command (use this if you ever get confused)

;;
;; Navigating Through the Buffer
;;

C-n   : next line
C-p   : previous line
C-f   : forward char
M-f   : forward word
C-M-f : forward s-expression
C-b   : backward char
M-b   : backward word
C-M-b : backward s-expression
C-a   : begin of line
C-e   : end of line

;;
;; Auto-completion
;;

M-/    : complete word

C-c ]  : "super-bracket" -- close parens to enclosing toplevel.


;;
;; Copying, Cutting, Pasting, working with Lisp S-expressions
;;

C-d         : delete character
M-d         : delete word
C-M-k       : kill s-expression
C-<space>   : begins a selection
C-M-<space> : selects current sexpression
M-w         : copies selection
C-w         : cuts selection
C-y         : yanks (pastes) the selection
M-y         : yanks previous 
C-k         : kills current line
C-x u       : Undo!
M-<space>   : removes all but one whitespace under point.
M-\         : removes all whitespace under point.

;;
;; Indenting
;;

C-M-q indents entire expression  (with cursor on opening parenthesis)

C-q indents and word-wraps a paragraph of text to reasonable line
    length.

;;
;; Working with Files & Buffers
;;

C-x C-b   : get list of open buffers
C-x k     : kills current buffer
C-x b     : visit other buffer (default is previous)
C-x C-s   : saves current buffer to file
C-x C-w   : saves current buffer to a new file
C-x C-f   : find file (i.e. file open)

;;
;; Dired (Directory editor) mode
;;

C-x C-f  to visit a directory, then <Enter> for Directory Editor (Dired)
Within Dired: 

  d  : marks file for deletion
  f  : visits the file or directory
  ~  : marks all backup files for deletion
  x  : executes pending commands (usually deletions)

;;
;; Windows and Frames
;;

C-x 2    : split screen into two windows
C-x 1    : make current window be the only window
C-x 0    : close current window
C-x o    : change to next window
C-x y    : change to previous window
C-x 2 5  : Open a new Frame (a new desktop window)
C-x &    : Jump to *gdl-devo* buffer

;;
;; Command-line Shortcuts
;;

M-p   : bring back previous command
M-n   : bring back next command


;;
;; Compiling and Loading from Buffer
;;

C-c C-b  : Compile Buffer  (in ELI)
C-c C-k  : Compile Buffer  (in Slime)
C-c C-f  : Compile Form   (in ELI)
C-x C-e  : Compile Form   (in Slime, with cursor after closing parenthesis)

;;
;; Mental Health Services 
;;

M-x doctor  : Get mental help from a pseudo psycho-therapist

