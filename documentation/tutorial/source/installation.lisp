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

(defparameter *installation*
    `((:chapter :title "Installation")
      
      "This section will take you through the installation of Gendl
from a prepackaged distribution with the Allegro CL Common Lisp engine
and the Slime IDE (based on Gnu Emacs). Gendl is also available as
open-source software; if you want to use that version, then you are
expected to have your own Common Lisp engine installed and set up, and
your own editing environment. See the README file with the open-source
distribution"
      (:footnote "http://github.com/genworks/Genworks-GDL/")
      " for more details on using it."
      
      ((:section :title "Installation of pre-packaged Gendl")
       ((:subsection :title "Download the Software and retrieve a license key")
	((:list :style :enumerate)
	 (:item "Visit the Downloads section of the \\href{http://genworks.com/newsite}{Genworks Newsite}"
	   (:footnote "http://genworks.com/newsite"))
	 (:item "Enter your email address" 
	   (:footnote "if your address is not on file, send mail to licensing@genworks.com")
	   ".")
	 (:item "Download the latest Payload and gpl.zip for Windows.")
	 (:item "Click to receive license key file by email.")))
       
       ((:subsection :title "Unpack the Distribution")
	"GenDL is currently distributed for all the platforms as a
self-contained ``zip'' file which does not require official
administrator installation.  What follows are general instructions; more up-to-date details
may be found in the email which accompanies the license key file. A five-minute installation video
is also available in the Documentation section of the \href{http://genworks.com/newsite}{Genworks Newsite}."
	((:list :style :enumerate)
	 (:item "Unzip the gdl1581... zipfile into a location where you have write permissions")
	 (:item "Unzip the gpl.zip file at the same level as the gdl payload")
	 (:item "Copy the license key file as gdl.lic (for Trial,
	 Student, Professional editions), or devel.lic (for Enterprise edition) into the "
	   (:texttt "program/")
	   " directory within the gdl1581.../ directory."))

	((:image-figure :image-file "gendl-installation.png"
			:caption "Several Gendl versions and one GPL "
			:label "fig:gendl-installation"))


	"So you now should have two directories at the same level: one named "
	(:texttt "gdl1581.../") 
	"(the rest of the name will contain the specific dated build stamp), and a "
	(:texttt "gdl/")  
	"directory at the same level. Note that as seen in Figure "
	(:ref "fig:gendl-installation")
	", it is possible to have several Gendl versions installed, but just a single common "
	(:texttt "gpl/")
	" folder.")
       
       ((:subsection :title "Test your Installation")
	"Test your installation according to the following checklist:"
	((:list :style :enumerate)
	 (:item "Invoke the "
	   (:texttt "run-gdl.bat")
	   " (Windows), or "
	   (:texttt "run-gdl")
	   " (Linux, MacOS) startup script. This should launch Gnu Emacs with a 
README file displayed by default. Take the time to look through this README file. 
Especially the later part of the file contains information about Emacs keyboard
shortcuts available.")
	 
	 (:item  "In emacs, enter: "
	   (:texttt "M-x glime")
	   ". That is, hold down the ``Meta'' (or ``Alt'') key, and press the ``M'' key. 
You will see this command shown in the "
	   (:emph "mini-buffer")
	   " at the bottom of the Emacs window, as shown in Figure "
	   (:ref "fig:mini-buffer"))

	 (:item "press the ``Enter'' key")

	 (:item "On Windows, you will get a new window, named the Allegro CL Console or the Gendl Console. 
Watch this console for any errors or warnings. 

The first time you start up, you may see messages in this console for
several minutes while the system is being built (or if you received a
completely pre-built system, the messages may last only a few
seconds).

On Linux or MacOS, there will be a separate Emacs buffer (available
through Emacs' ``Buffers'' menu) where you will see these messages.

The messages will consist of compiling and loading information, followed by copyright and welcome information
for Gendl. After these messages have finished, you should see the following command prompt:"
	   (:texttt "gdl-user(1): "))

	 (:item "In your Web Browser (e.g. Google Chrome, Firefox, Safari, Opera, Internet Explorer), 
perform the following steps:"
	   ((:list :style :enumerate)
	    (:item "visit http://localhost:9000/tasty.")
	    (:item "accept default robot:assembly.")
	    (:item "Select ``Add Leaves'' from the Tree menu.")
	    (:item "Click on the top node in the tree.")
	    (:item "Observe the wireframe graphics for the robot.")
	    (:item "Click on the robot to zoom in.")
	    (:item "Select ``Clear View!'' from the View menu.")
	    (:item "Select ``X3DOM'' from the View menu.")
	    (:item "Click on the top node in the tree.")
	    (:item "``Refresh'' or ``Reload'' your browser window (may not be necessary)."
	    (:item "If your browser supports WebGL, you will see the robot in shaded dynamic view.")
	    (:item "Select ``PNG'' from the View menu.")
	    (:item "You will see the wireframe view of the robot as a PNG image."))))


	 ))


       ((:subsection :title "Make a Desktop Shortcut")
	"")
       
       ((:subsection :title "Populate your Initialization File")
	"The default initialization file for Gendl is called "
	(:tt "gdlinit.cl")
	", "))

      ((:section :title "Installation of open-source Gendl")
       ((:subsection :title "Install and Configure your Common Lisp environment")
	"Gendl is currently tested to build on the following Common Lisp engines:"
	((:list :style :itemize)
	 (:item "Allegro CL (commercial product from Franz, free Express Edition available)")
	 (:item "LispWorks (commercial product from LispWorks Ltd, free Personal Edition available)")
	 (:item "Steel Bank Common Lisp (SBCL) (free open-source project with permissive license)"))
	"Please refer to the documentation for each of these systems for full information on installing 
and configuring the environment. Typically this will include a text editor, either Gnu Emacs with Superior
Lisp Interaction Mode for Emacs (Slime), or a built-in text editing and development environment which 
comes with the Common Lisp system."))))
       

