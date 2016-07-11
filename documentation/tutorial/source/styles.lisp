(in-package :gendl-doc)

(defparameter *styles* 
  '(
    (body 
     (font-family "\'Arial\'")
     (margin-left "10%")
     (margin-right "10%"))
    (a 
     (color "#666")
     (text-decoration "none"))
    ("a:hover" 
     (color "#ccc"))
    (pre 
     (background-color "#eee")
     (padding "10px")
     (border-radius "5px")
     (font-family "monospace"))
    (tt 
     (background-color "#eee"))
    (div>ol 
     (border-left "6px solid #eee")
     (margin "20px"))
    (li 
     (margin "3px"))
    ("li li"
     (font-size "90%"))
    (dl 
     (margin-left "35px"))
    (dt 
     (font-weight "bold"))))

     