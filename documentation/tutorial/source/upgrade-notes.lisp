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

(defparameter *upgrade-notes*
    `((:chapter* :title "Upgrade Notes")

      "GDL 1580 marked the end of a major branch of GDL development,
and 1581 was actually a major new version. Together with 1581, an
open-source version was released under the name ``The Gendl Project.''

This addendum lists the typical modifications you will want to
consider for upgrading from GDL 1580 to GDL 1582 or later."

      ((:list :style :itemize)
       
       
       (:item "(make-gdl-app ..) is now available for 1582. We have
made available an Enterprise Edition of 1582 which includes the
make-gdl-app function, which creates Runtime applications without the
compiler or GDL development facilities.  If you are an Enterprise
licensee, are ready to release Runtime applications on 1582, and you
have not received information on the Enterprise Edition, please
contact support@genworks.com")
       
       (:item "(register-asdf-systems) and the "
	 (:texttt "\"3rdpty/\"")
	 " directory are no longer needed or available. Instead, we depend on the Quicklisp
system. Details of Quicklisp are available at "
	 (:href "http://www.quicklisp.org")
	 ". See Section "
	 (:ref "subsec:compilingandloadingasystem")
	 " for information about how to use Quicklisp with GDL.")
       
       (:item "There is a system-wide "
	 (:texttt "gdlinit.cl")
	 " in the application directory, and depending on the
       particular release you have, this may have some default
       information which ships with GDL. There is a personal "
	 (:texttt "gdlinit.cl")
	 " in home directory, which you should modify if you want to
       customize anything.")

       (:item "Slime debugging is different from the ELI emacs debugger. The main thing to know is 
to press ``a'' or ``q'' to pop out of the current error. Full documentation for the Slime debug mode
is available with the "
	 (:href "http://common-lisp.net/project/slime/doc/html/Debugger.html" "Slime documentation")
	 ".")

       (:item "color-themes -- GDL now ships with the Emacs
       color-theme package. You can select a different color theme with "
	 (:texttt "M-x color-theme-select")
	 ". Press [Enter] or middle-mouse on a color theme to apply it.")

       (:item "GDL files can now end with "
	 (:texttt ".lisp")
	 " or "
	 (:texttt ".gdl")
	 ". The new "
	 (:texttt ".gdl")
	 " extension will work for emacs Lisp mode and will work with
	 cl-lite, ASDF, and Quicklisp for including source files in application systems. We recommend migrating
to the new "
	 (:texttt ".gdl")
	 " extension for files containing "
	 (:texttt "define-object")
	 ", "
	 (:texttt "define-format")
	 ", and "
	 (:texttt "define-lens")
	 " forms, and any other future toplevel defining forms introduced by GDL, in order to distinguish 
from files containing raw Common Lisp code.")

       (:item "in gdlAjax, HTML for a sheet-section is given in the slot called "
	 (:texttt "inner-html")
	 " instead of "
	 (:texttt "main-view")
	 ". This name change was made to clarify what exactly is
	 expected in this slot -- it is the innerHTML of the page
	 division represented by the current sheet-section. If you
	 want to make your code back-compatible with GDL 1580, you can
	 use the following form in place of old occurences of "
	 (:texttt "main-view")
	 ": "
	 (:verbatim "... #+allegro-v8.1 main-view #-allegro-v8.1 inner-html ..."))
       
       (:item "(update-gdl ..) is not yet available for 1582. Instead
of updating incrementally with patches, the intention starting with
GDL 1582 is for full GDL releases to be made available approximately
monthly. Less frequent Long Term Maintenance (``LTS'') releases will
also be made available along with a new simpler maintenance patch
system.")

       )))
	 


