;;
;; Copyright 2002, 2009, 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :gendl-doc)

(defparameter *basic-operation*
    `((:chapter :title "Basic Operation of the Gendl Environment")
      "This chapter will step you through all the basic steps of
operating a typical Gendl environment. We will not go into any depth
about the additional features of the environment or language syntax in
this section --- this is just for getting familiar and practicing with
the mechanics of operating the environment with a keyboard."

      ((:section :title "What is Different about Gendl?")
       "Gendl is  a dynamic language environment with incremental compiling and in-memory 
definitions. That means that as long as the system is running, you can "
       (:emph "compile")
       " new " 
       (:emph "definitions") 
       " of functions, objects, etc, and they will immediately become available as part of the running system,
and you can begin testing them immediately or update an existing set of objects to observe their new behavior.

In many other programming language systems, you have to start the
system from the beginning and reload all the files in order to test
new functionality. 
 
In Gendl, if you simply shut down the system after having compiled and
loaded a set of files with new definitions, then when you restart the
system you will have to recompile and/or reload those definitions in
order to bring the system back into the same state. This is typically
done automatically, using commands placed into the "
       (:texttt "gdlinit.cl")
       " initialization file, as introduced in Section "
       (:ref "sec:customizingyourenvironment")
       ". Alternatively, you can compile and load definitions into your Gendl
session, then save the ``world'' in that state. That way, it is
possible to start a new Gendl ``world'' which already has all your
application's definitions loaded and ready for use, without having to
procedurally reload any files. You can then begin to make and test new
definitions (and re-definitions) starting from this new ``world.''")
       
      ((:section :title "Startup, ``Hello, World!'' and Shutdown")
       (:p
	"The typical Gendl environment consists of three programs: Gnu
Emacs (the editor), a Common Lisp engine with Gendl system loaded or built into it (e.g. the "
	(:texttt "gdl.exe")
	" executable in your "
	(:texttt "program/")
	" directory), and (optionally) a web browser
such as Firefox, Google Chrome, Safari, Opera, or Internet
Explorer. Emacs runs as the main "
	(:emph "process")
	", and this in turn starts the CL engine with Gendl as a "
	(:emph "sub-process")
	". The CL engine typically runs an embedded "
	(:emph "webserver")
	", enabling you to access your application through a standard web browser.")
       (:p "As introduced in Chapter "
	   (:ref "chap:installation")
	   ", the typical way to start a pre-packaged Gendl environment is with the "
	   (:texttt "run-gdl.bat") 
	   " (Windows), or "
	   (:texttt "run-gdl") 
	   " (MacOS, Linux) script files. Invoke this script file
from your computer's file manager, or from a desktop shortcut if you
have created one as outlined in section "
	   (:ref "subsec:makeadesktopshortcut")
	   ". Your installation executable may also have created a
Windows ``Start'' menu item for Genworks Gendl. Of course you can also 
invoke "
	   (:texttt "run-gdl.bat")
	   " from the Windows ``cmd'' command-line, or from another command shell such as Cygwin."
	   (:footnote "Cygwin is also useful as a command-line tool on Windows
for interacting with a version control system like Subversion (svn)."))


       ((:subsection :title "Startup")
	" Startup of a typical Gendl development session consists of
two fundamental steps: (1) starting the Emacs editing environment,
and (2) starting the actual Gendl process as a ``sub-process'' or ``inferior'' process 
within Emacs. The Gendl process should automatically establish a network connection
back to Emacs, allowing you to interact directly with the Gendl process from within Emacs."
	((:list :style :enumerate)
	 (:item "Invoke the " (:texttt "run-gdl.bat") " or " (:texttt "run-gdl.bat") " startup script.")
	 (:item "You should see a blue emacs window as in Figure "
	   (:ref "fig:emacs-startup")
	   ". (alternative colors are also possible).")
	 (:item "Press M-x (Alt-x), and type " (:texttt "gendl") " in the mini-buffer, as seen in Figure "
		(:ref "fig:mini-buffer")
		".")
	 (:item "(MS Windows): Look for the Genworks Gendl Console
window, or (Linux, Mac) use the Emacs ``Buffer'' menu to visit the
``*inferior-lisp*'' buffer. Note that the Genworks Gendl Console
window might start as a minimized icon; click or double-click it to
un-minimize.")
	 (:item "Watch the Genworks GDL Console window for any
errors. Depending on your specific installation, it may take from a
few seconds to several minutes for the Genworks Gendl Console (or
*inferior-lisp* buffer) to settle down and give you a "
	   (:texttt "gdl-user(): ")
	   " prompt. This window is where you will see most of your program's textual output, any 
error messages, warnings, etc.")
	 
	 (:item "In Emacs, type: "
	   (:texttt "C-x \\&")
	   " (or select Emacs menu item "
	   "Buffers&dollar;\\rightarrow&dollar;*slime-repl...*"
	   ") to visit the ``*slime-repl ...*'' buffer. The full name
of this buffer depends on the specific CL/Gendl platform which you are
running. This buffer contains an interactive prompt, labeled "
	   (:texttt "gdl-user>")
	   ", where you will enter most of your commands to interact with your running Gendl session
for testing, debugging, etc. There is also a web-based graphical interactive environment called "
	   (:emph "tasty") 
	   " which will will cover in Chapter "
	   (:ref "chapter:tasty"))

	 (:item "To ensure that the Gendl interpreter is up and running, type: "
	   (:texttt "(+ 2 3)")
	   " and press [Enter].")
	 (:item "You should see the result "
	   (:texttt "5")
	   " echoed back to you below the prompt.")))
	 
       ((:subsection :title "Developing and Testing a Gendl ``Hello World'' application")
	" "
	((:list :style :enumerate)
	 (:item "type C-x (Control-x) 2, or C-x 3, or use the ``Split
Screen'' option of the File menu to split the Emacs frame into two
``windows'' (``windows'' in Emacs are non-overlapping panels, or
rectangular areas within the main Emacs window).")
	
	 (:item "type C-x o several times to move from one window to
the other, or move the mouse cursor and click in each window. Notice
how the blinking insertion point moves from one window to the other.")

	 (:item "In the top (or left) window, type C-x C-f (or select Emacs menu item
``File&dollar;\\rightarrow&dollar;Open File'') to get the ``Find file'' prompt in the
mini-buffer.")

	 (:item "Type C-a to move the point to the beginning of the mini-buffer line.")

	 (:item "Type C-k to delete from the point to the end of the mini-buffer.")

	 (:item "Type "
	   (:texttt "\\textasciitilde/hello.gdl")
	   " and press [Enter]")

	 (:item "You are now editing a (presumably new) file of Gendl
	 code, located in your HOME directory, called "
	   (:texttt "hello.gdl"))
	
	 (:item "Enter the text from Figure "
	   (:ref "fig:simpleobjectdefinition")
	   " into the "
	   (:texttt "hello.gdl")
	   " buffer. You do not have to match the line breaks and whitespace as shown in the example.
You can auto-indent each new line by pressing [TAB] after pressing [Enter] for the newline."
	   (:p (:emph "Protip:") "You can also try using "
	       (:texttt "C-j")
	       " instead of [Enter], which will automatically give a newline and auto-indent."))

	 ((:boxed-figure :caption "Example of Simple Object Definition"
			 :label "fig:simpleobjectdefinition")
	  (:verbatim "
 (in-package :gdl-user)

 (define-object hello ()

   :computed-slots 
   ((greeting \"Hello, World!\")))
"))
	 

	 (:item "type " (:texttt "C-x C-s") " (or choose Emacs menu item "
		(:emph "File&dollar;\\rightarrow&dollar;Save")
		") to save the contents of the buffer (i.e. the window) 
to the file in your HOME directory.")
	 
	 (:item "type " (:texttt "C-c C-k") " (or choose Emacs menu item "
		(:emph "SLIME&dollar;\\rightarrow&dollar;Compilation&dollar;\\rightarrow&dollar;Compile/Load File")
		") to compile \\& load the code from this file.")

	 (:item "type " (:texttt "C-c o") " (or move and click the mouse)  to switch to the bottom window.")

	 (:item "In the bottom window, type " (:texttt "C-x \\&") " (or choose Emacs menu item "
		(:emph "Buffers&dollar;\\rightarrow&dollar;*slime-repl...*")
		") to get the "
		(:texttt "*slime-repl ...*") " buffer, which should contain a "
		(:texttt "gdl-user>")
		" prompt. This is where you normally type interactive Gendl commands.")

	 (:item "If necessary, type "
	   (:texttt "M \\textgreater")
	   " (that is, hold down Meta (Alt), Shift, and the ``\\textgreater'' key) to
move the insertion point to the end of this buffer.")

	 (:item "At the "
	   (:texttt "gdl-user>")
	   " prompt, type "
	   (:verbatim "(make-self 'hello)")
	   " and press [Enter].")

	 (:item "At the "
	   (:texttt "gdl-user>")
	   " prompt, type "
	   (:verbatim "(the greeting)")
	   " and press [Enter].")

	 (:item "You should see the words "
	   (:texttt "Hello, World!")
	   " echoed back to you below the prompt.")
	 ))

       ((:subsection :title "Shutdown")
	" To shut down a development session gracefully, you should first shut down the Gendl process,
then shut down your Emacs."
	((:list :style :itemize)
	 (:item "Type "
	   (:texttt "M-x quit-gendl")
	   " (that is, hold Alt and press X, then release both while you type "
	   (:texttt "quit-gendl")
	   " in the mini-buffer), then press [Enter]")

	 (:item "Type "
	   (:texttt "C-x C-c")
	   " to quit from Emacs. Emacs will prompt you to save any modified buffers before exiting."))))

      ((:section :title "Working with Projects")
	"Gendl contains utilities which allow you to treat your
application as a ``project,'' with the ability to compile,
incrementally compile, and load a ``project'' from a directory tree of
source files representing your project. In this section we give an
overview of the expected directory structure and available control
files, followed by a reference for each of the functions included in
the bootstrap module."
       ((:subsection :title "Directory Structure")

	

	(:p "You should structure your applications in a modular fashion, with the
directories containing actual Lisp sources called \"source.\"")

	(:p "You may have subdirectories which themselves contain \"source\"
directories.")

	(:p "We recommend keeping your codebase directories relatively flat,
however.")

	(:p "In Figure "
	    (:ref "fig:yoyodyne-base")
	    " is an example application directory, with four source files.")

	((:boxed-figure :caption "Example project directory with four source files"
			:label "fig:yoyodyne-base")
	 (:verbatim "
  apps/yoyodyne/booster-rocket/source/assembly.gdl
  apps/yoyodyne/booster-rocket/source/package.gdl
  apps/yoyodyne/booster-rocket/source/parameters.gdl
  apps/yoyodyne/booster-rocket/source/rules.gdl
"))

	)

       
       ((:subsection :title "Source Files within a source/ subdirectory")

	((:subsubsection :title "Enforcing Ordering")
	 (:p "Within a source subdirectory, you may have a file called "
	     (:texttt "file-ordering.isc")
	     (:footnote (:texttt "isc") " stands for ``Intelligent Source Configuration''")
	     " to enforce a certain ordering on the files. Here is the contents of an example for the 
above application:")
	 (:p (:texttt "(\"package\" \"parameters\")"))

	 (:p "This will force package.lisp to be compiled/loaded first, and
parameters.lisp to be compiled/loaded next. The ordering on the rest
of the files should not matter (although it will default to
lexigraphical ordering).")


	(:p "Now our sample application directory looks like Figure "
	    (:ref "fig:yoyodyne-with-file-ordering-isc")
	    " is an example application directory, with four source files.")

	 )
	
	((:boxed-figure :caption "Example project directory with file ordering configuration file"
			:label "fig:yoyodyne-with-file-ordering-isc")
	 (:verbatim "
  apps/yoyodyne/booster-rocket/source/assembly.gdl
  apps/yoyodyne/booster-rocket/source/file-ordering.isc
  apps/yoyodyne/booster-rocket/source/package.gdl
  apps/yoyodyne/booster-rocket/source/parameters.gdl
  apps/yoyodyne/booster-rocket/source/rules.gdl"))

	))



      
      ((:section :title "Customizing your Environment")
	" You may customize your environment in several different ways,
for example by loading definitions and settings into your Gendl
``world'' automatically when the system starts, and by specifying
fonts, colors, and default buffers (to name a few) for your emacs
editing environment."

	)


      ((:section :title "Saving the World")
	" Saving the world refers to a technique of saving a complete
binary image of your Gendl ``world'' which contains all the currently
compiled and loaded definitions and settings.  This allows you to
start up a saved world almost instantly, without having to reload all
the definitions. You can then incrementally compile and load just the
particular definitions which you are working on for your development
session.

To save a world, follow these steps:"

       ((:list :style :enumerate)
	(:item "Load the base Gendl code and (optionally) code for Gendl
modules (e.g. gdl-yadd, gdl-tasty) you want to be in your saved image. For example:"
	  (:verbatim "
 (ql:quickload :gdl-yadd) 
 (ql:quickload :gdl-tasty)"))
	
	:item (:verbatim "(ff:unload-foreign-library (merge-pathnames \"smlib.dll\" \"sys:smlib;\"))")

	:item (:verbatim "(net.aserve:shutdown)")

	:item (:verbatim "(setq excl:*restart-init-function* '(gdl:start-gdl :edition :trial))")

	(:item  " (to save an image named yoyodyne.dxl) Invoke the command "
	  (:verbatim "(dumplisp :name \"yoyodyne.dxl\")")
	  "Note that the standard extension for Allegro CL images is "
	  (:texttt ".dxl")
	  ". Prepend the file name with path information, to write the image to a specific location.")))


      ((:section :title "Starting up a Saved World")
       "In order to start up Gendl using a custom saved image, or ``world,'' follow these steps"
       ((:list :style :enumerate)
	(:item "Exit Gendl")
	(:item "Copy the supplied "
	  (:texttt "gdl.dxl")
	  " to "
	  (:texttt "gdl-orig.dxl")
	  ".")
	(:item "Move the custom saved dxl image to "
	  (:texttt "gdl.dxl")
	  " in the Gendl application "
	  (:textt "\"program/\"")
	  " directory.")
	(:item "Start Gendl as usual.")))))




      
      


