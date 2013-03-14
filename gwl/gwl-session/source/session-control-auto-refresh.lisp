(in-package :gwl)

;;--------------------------------------------------
;; Author : Brian Sorg, Liberating Insight
;;
;; Date : created Nov 30, 2004
;;
;; Copyright November 2004 Liberating Insight LLC
;;
;;  License:  Users of this file are granted the rights to distribute
;;            and use this software as governed by the terms of the
;;            Lisp Lesser GNU Public License
;;           (http://opensource.franz.com/preamble.html), also known
;;            as the LLGPL.
;;-------------------------------------------------


(defun session-control-auto-refresh (timeout &optional (html-stream *html-stream*))
  "Adding this javascript function into the header of a web page will cause the page to timeout and reload repeatedly. This is intended to be used such that when 
an instance is open in an active browser the page will automatically update the expires-at function even if the operator takes an extended break from the application. 
It works by checking if any forms exist on this page. If they do it will submit the first form on the page when the timeout value is reached. This is done to avoid 
the Post Data confirmation warning that most browser present. If no forms are found it will use the reload(true) function to reload the page.
:arguments
(timeout \"Time in seconds between page reloads\")
:&optional 
(html-stream \"Stream which the output should be sent to. Default is *html-stream*\")
"
  
  (format html-stream "
  <script language=\"Javascript\" type=\"text/javascript\">
   <!--
    sessionAutoRefreshID = window.setTimeout(\"autoRefresh() ;\",~a);

    function autoRefresh () {
     var pageForms = document.forms.length ;
     if (pageForms == 0) {
      window.location.reload(true)	;
     }
     else {
      document.forms[0].submit()		;
     }
    }
						       
   // -->
  </script> 
"
	  (* timeout 1000)))


