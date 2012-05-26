

            Welcome to the GenDL™ Environment


=============
Quick Startup
=============

To start the GenDL environment, type 

 M-x gendl

(that is, hold down Alt, press X, then type "gendl" in the small area
at the bottom of the window). 

NOTE: The first time it may take several minutes to start up, since it
      may be building some or most of GenDL from source code.

After some time, you should see a "gdl-user>" command prompt and start
a web server which allows you to visit, for example:

  http://localhost:9000/tasty

and

  http://localhost:9000/yadd


See "Troubleshooting" below if you experience trouble starting up.


===============
Emacs and GenDL
===============

Although you are free to use other editors or IDEs, spending some time
to get familiar with Emacs is the best small investment you can make
for working with a Lisp-based system like GenDL. Genworks supports two
distinct emacs-based IDEs for GenDL development: GLIME (Genworks Lisp
Interaction Mode for Emacs) and Franz ELI. Glime is a thin
customization layer on top of Slime (Superior Lisp Interaction Mode
for Emacs), which works across all major OS platforms and CL
implementations and is well-supported by the Common Lisp
community. Genworks plans to continue adding specialized GenDL support
to Glime. Franz ELI is provided mainly for back-compatibility to
previous releases.

If you are new to Emacs, you can get a general Emacs Tutorial under
the Help menu above. After completing the Tutorial, try to practice
what you learned by forcing yourself not to use the mouse too much in
Emacs.

Touch-typing ability is definitely an asset for GenDL development, but
with auto-completion of symbols (possible with M-/ -- you'll know what
that means after looking at the tutorial) it is possible to be an
extremely effective GenDL developer without having world-class typing
speed.

It is strongly recommended to map your Caps Lock key to be a Control
key. This will give you a much more comfortable experience, allowing
you to use all the Emacs navigation keystrokes without constantly
curling your left little finger down to the Control key.  For Linux
and Mac, this is easy to do in the System Preferences. For Windows,
here are some resources which can help you do this (the first one
appears not to require Administrator access on your machine):

  http://dropline.net/2009/05/mapping-caps-lock-to-control-without-admin-access/
  http://emacswiki.org/emacs/MovingTheCtrlKey

Note that if you are running Windows in a Virtual Machine hosted under
MacOS or Linux, it is sufficient to map the Caps Lock to to Control in
the host system.

Finally, please see the Genworks Documentation (URL below) for an
introductory video on using Emacs in the GenDL environment.



===============
Learning GenDL
===============

Absolutely the best way to learn GenDL is to dive in and start using
it for small exercises and projects.

Documentation:

  The main entry point to GDL documentation is here:

     http://www.genworks.com/newsite

  Then click the link for Documentation.


 There are videos and a tutorial manual. We hope to update this site
 frequently so please come round often.


=======
Startup
=======

There are two ways to start the GenDL environment. The first is Glime,
the Genworks Lisp Interaction Mode for Emacs. Glime is based on Slime,
the Superior Lisp Interation Mode for Emacs. Slime enjoys wide
community support and works across all major OS platforms and CL
implementations. The second is Franz ELI (Emacs Lisp Interface). ELI
is provided mainly for compatiblity with legacy GDL installations,
works with Allegro CL-based GenDL environments (currently supported by
Genworks on Linux and Windows), and receives commercial support from
Franz Inc.


1. Glime
=========

Starting:

   M-x gendl  (that is, hold down the Meta (or Alt) key, and type x,
               then type 'gendl' and press <Return>).

NOTE: "M-x glime" will work exactly the same as "M-x gendl"


Quitting: 

  When you have had enough of using GenDL with Glime, you should do
  two things:

 1. type 

        M-x quit-gendl (and press <Return>) 

    from anywhere in Emacs, to kill the GenDL process.

 2. (optional) type 

        C-x C-c (that's Control-x, Control-c) 

    to kill the Emacs process and exit the window.

    
 3. If you kept Emacs up and running, it is possible to restart the
    GenDL session again with "M-x gendl"



2. ELI
=======

Starting:


    M-x gdl (that is, hold down the Meta (or Alt) key, and type x,
             then type 'gdl' and press <Return>.)


Quitting:


  When you have had enough of using GenDL with ELI, you should do two
  things:

 1. type :exit (and press <Return>) at the command prompt in the *gdl
    toplevel* buffer to kill the GDL process.

 2. (optional) type C-x C-c (that's Control-x, Control-c) to kill the
    Emacs process and exit the window.

 3. If you kept Emacs up and running, it is possible to restart the
    GenDL session with "M-x gdl"



=================
Customizing Emacs
=================

If a file exists in your home directory called ".emacs-gendl" this
will be loaded upon startup. You can put any custom emacs-lisp code in
this file to customize your startup (see below for an example to
customize the color theme).

You can always find out where Emacs thinks is your home directory's
location by typing C-x d ~/ <RET>. This should present the list of
files in the home directory, and show its full name on the first
line. Likewise, to visit your init file, type C-x C-f ~/.emacs-gendl <RET>.


Color Themes
============

Starting now, we are releasing GenDL with the color-theme:

  http://emacswiki.org/emacs/ColorTheme

and solarized:

  http://ethanschoonover.com/solarized

packages for Gnu emacs.

You can select any of the themes from color-theme with 

  M-x color-theme-select

(Click on the color theme you want to select then press <Return>).

The solarized themes are called color-theme-solarized-light and
color-theme-solarized-dark. The default is
color-theme-solarized-light.

To set a color theme automatically, put something like the following
at the end of your ~/.emacs-gendl :


  (color-theme-solarized-dark)  ;; for solarized dark theme, or 


To revert to the legacy GDL "blue background" colors, edit your
emacs/gdl.el file to look as follows starting at Line 235::

;;
;; Set up color-theme and solarized color-themes:
;;

;;(add-to-list 'load-path "emacs/emacs-goodies-el")
;;(add-to-list 'load-path "emacs/solarized")

;;(require 'color-theme)
;;(color-theme-initialize)
;;(require 'color-theme-solarized)
;;(color-theme-solarized-light)

(gdl:set-colors)



=========================
Emacs Keychord Reference
=========================

The following is a reference for the Emacs keychords which are
commonly used when working with GenDL. They will be understandable
after you completed the Emacs Tutorial mentioned above. If you force
yourself to use these for a few days, they will quickly feel natural.

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
;; Compiling and Loading from Buffer (Gendl/Glime/Slime)
;;

C-c C-k  : Compile Buffer  (in Glime)
C-c C-c  : Compile current form (e.g. define-object or defun)
C-x C-e  : Evaluate Form   (in Glime, with cursor after closing parenthesis)

;;
;; Compiling and Loading from Buffer (legacy GDL/Franz ELI):
;;

C-c C-b  : Compile Buffer  (in ELI)
C-c C-f  : Compile Form   (in ELI)


;;
;; Mental Health Services 
;;

M-x doctor  : Get mental help from a pseudo psycho-therapist




Troubleshooting
===============


Missing or Invalid License File

  The gdl.lic or devel.lic file must be placed in the program/ folder
  of your Gendl installation (gdl1581-.../ folder). If this file is
  missing, has been modified from its original form, or is not
  compatible with the installed Gendl version, the Gendl process will
  immediately quit. If the console is quitting immediately without any
  visible message, then a missing or invalid license file is the
  likely cause. 

  Make sure that you have a current license file from the Downloads
  section of http://genworks.com/newsite, and copy it into the
  program/ folder of your installation. 
   
  Try directly starting the "gdl.exe" (Windows) or "gdl" executable
  and you should be able to see the message being displayed in the
  console, however briefly.


Webserver Fails to serve on Port 9000

  First of all, you may receive a message from your Firewall asking
  for permission to unblock services from the gdl.exe executable. You
  should grant this permission.

  If you are unable to see the following URL:

    http://localhost:9000/color-map

  then it is possible your machine already has another process running
  on Port 9000. For example, some online PC gaming software will set
  up a proxy service on this port. 

  If this is the case, then you can start Gendl serving on a different
  port. To do this, try the following at the "gdl-user>" command
  prompt (using port 8888 as an example):

    (net.aserve:shutdown)
    (gwl:start-gwl :port 8888)

 Now try visiting 


     http://localhost:8888/color-map

 If this works, then you can put these commands into a file gdlinit.cl
 in your Gendl application directory.




