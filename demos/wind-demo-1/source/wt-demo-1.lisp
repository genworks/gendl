(in-package :wind)

(define-object demo-1 (base-ajax-sheet)
  ;;base-ajax-sheet)
 
  :input-slots 
  
  ((css-style  "/css-demo-1/style-wind-1.css"))
  
  :computed-slots 
  ((additional-header-content 
    (with-cl-who-string ()
      (:p (when *developing?* (str (the development-links))))
      (:style :type "text/css" :media "all"
              (str (format nil " @import url(~a);" (the css-style))))))
   
   (title "WT Demo 1")
   (main-sheet-body
    (with-cl-who-string ()
      (:div :id "wrapper"
            (:div :id "header"(:h1 "Wind Turbine Geometry Demo"))
            (:div :id "left"
                  (:div :id "logo"
                        (:h1 "KE-Works S.R.L. & Genworks International")
                        (:p "It's all possible"))
                  (:div :id "news"
                        (:h2 "General inputs")
                        (:div :id "end-input-head")
                        (str (the general-inputs main-div)))
                  (:div :id "end-input")
                  (:div :id "disp-comp"   
                        (:h2 "Displayed component")        
                        (:div :id "end-input-head")     
                        (:p)
                        (str (the displayed-component main-div))))
            (:div :id "data-export"
                  (:div :id "left-export"
                        (:div :id "data-export-t"
                              (:h2 "Data Export")
                              (:div :id "end-input-head")
                              (str (the export-section main-div))))
                  (:div :id "left-export"
                        (:div :id "data-export-t"
                              (:h2 "Turbine characteristics")
                              (:div :id "end-input-head")
                              (str (the turbine-characteristics-1 main-div))))
                  (:div :id "left-export"
                        (:div :id "data-export-t"
                              (:h2 "Turbine characteristics")
                              (:div :id "end-input-head")
                              (str (the turbine-characteristics-2  main-div))))
                  (:div :id "right"
                        (:h2 
                         (:form " WT reference input data file"
                                (:input  :size "25" :type "file" :name "ref-input" :accept="data/")))
                        (str (the  graphics graphics main-div))
                        (:h2 "Description")
                        (:div :class "scroll"
                              (:p "This quantification was made based on the next assumption: price of the wind turbine 1 mill/MW and 451/t price of raw steel plates. In the case of transport and installation, an equal value was allocated arbitrary to serve as input for the model implementation. Even though the assumptions reflects the reality with a ± 10% error, the assumed cost structure serves as a reference to develop a parametric model in which the identified manufacturing transport and installation processes are deeply investigated. The presented cost structure is valid just for the existing tower concepts. To account for the new concept, the identified cost structure has to be breaked in its constituent sub-parts (human resources, machining time, machining energy consumption, etc.). The model should be able to substitute processes like longitudinal welding with drilling and bolt-assembly."))))
            (:div :class"clear")
            (:div :id "footer"
                  (:div :id "copyright"
                        "Copyright &copy; 2011 Copyright KE-Works S.R.L Romania and Genworks International All right reserved.")
                  (:div :id "footerline")
                  )))))     
  
  
  :objects
  
  ((general-inputs :type 'general-inputs)
   (displayed-component :type 'displayed-component)
   (export-section :type  'export-section)
   (turbine-characteristics-1 :type  'turbine-characteristics-1)
   (turbine-characteristics-2 :type  'turbine-characteristics-2)
   (graphics :type 'graphics
             :blade-concepts (the displayed-component blade-concepts value)
             :number-of-blades (the general-inputs number-of-blades value)
             :geometry (the displayed-component component-display-control value)
             )))

(define-object general-inputs (sheet-section)
  :computed-slots
  ((main-view (with-cl-who-string ()
                ((:table :class "input_area")
                 ((:col  :width "90%")
                  (:tr (:td) (:td))
                  (:tr
                   ((:td) "Number of blades")
                   ((:td :align "right") (str (the number-of-blades html-string))))
                  (:tr 
                   (:td "Pitch angle")
                   ((:td :align "right") (str (the hub-hight html-string))))
                  (:tr 
                   (:td "Tilt angle")
                   ((:td :align "right") (str (the tilt-angle html-string))))
                  (:tr 
                   (:td "Precone angle")
                   ((:td :align "right") (str (the precone-angle html-string)))))))))
     
  :objects
  ((number-of-blades :type 'text-form-control
                     :default 3
                     :prompt nil
                     :size 1
                     :ajax-submit-on-change? t)
   
   (hub-hight :type 'text-form-control
              :default 0
              :prompt nil
              :size 1
              :ajax-submit-on-change? t)               
   
   (tilt-angle :type 'text-form-control
               :prompt nil
               :default 13
               :size 1
               :ajax-submit-on-change? t)                          
   
   (precone-angle :type 'text-form-control
                  :prompt nil
                  :default 13
                  :size 1
                  :ajax-submit-on-change? t)))
   
(define-object export-section (sheet-section)
  
  :computed-slots
  
  ((main-view (with-cl-who-string ()
                ((:table :class "input_area")
                 ((:col  :width "80%")
                  (:tr (:td) (:td))
                  (:tr
                   ((:td) "IGES file")
                   ((:td :align "right") (:button  :onClick "www.ke-works.ro" (:i "Download"))))
                  (:tr 
                   (:td "STL file")
                   ((:td :align "right") (:button  :onClick "www.ke-works.ro" (:i "Download"))))
                  (:tr 
                   (:td "VRML file")
                   ((:td :align "right") (:button  :onClick "www.ke-works.ro" (:i "Download"))))
                  (:tr 
                   (:td "PNG")
                   ((:td :align "right") (:button  :onClick "www.ke-works.ro" (:i "Download"))))))))))

(define-object turbine-characteristics-1 (sheet-section)
  
  :computed-slots
    
  ((main-view (with-cl-who-string ()
                ((:table :class "input_area")
                 ((:col  :width "80%")
                  (:tr (:td) (:td))
                  (:tr
                   ((:td) "Total mass")
                   ((:td :align "right") (:a (str (format nil "~at"(the total-mass))))))
                  (:tr 
                   (:td "Rotor mass")
                   ((:td :align "right")  (:a (str (format nil "~at"(the rotor-mass))))))
                  (:tr 
                   (:td "Blade mass")
                   ((:td :align "right") (:a (str (format nil "~at"(the blade-mass))))))
                  (:tr 
                   (:td "Nacelle mass")
                   ((:td :align "right") (:a (str (format nil "~at"(the nacelle-mass))))))
                  (:tr 
                   (:td "Power output")
                   ((:td :align "right") (:a (str (format nil "~aMW"(the power-output))))))))))

 
    (total-mass 1200)
    (rotor-mass 300)
    (blade-mass 100)
    (nacelle-mass 400)
    (power-output 5)
   ))
 
(define-object turbine-characteristics-2 (sheet-section)
  
  :computed-slots
  
  ((main-view (with-cl-who-string ()
                ((:table :class "input_area")
                 ((:col  :width "80%")
                  (:tr (:td) (:td))
                  (:tr
                   ((:td) "Data 1 ...etc")
                   ((:td :align "right") (:a (str (format nil "~a"(the total-mass))))))
                  (:tr 
                   (:td "Data 2 ...etc")
                   ((:td :align "right")  (:a (str (format nil "~a"(the rotor-mass))))))
                  (:tr 
                   (:td "Data 3 ...etc")
                   ((:td :align "right") (:a (str (format nil "~a"(the blade-mass))))))
                  (:tr 
                   (:td "Data 4 ...etc")
                   ((:td :align "right") (:a (str (format nil "~a"(the nacelle-mass))))))
                  (:tr 
                   (:td "Data 5 ...etc")
                   ((:td :align "right") (:a (str (format nil "~a"(the power-output))))))))))
                 
   
   (total-mass 88888)
   (rotor-mass 88888)
   (blade-mass 88888)
   (nacelle-mass 88888)
   (power-output 88888)
   ))

(define-object graphics (base-ajax-sheet base-object)
  
  :input-slots (geometry number-of-blades blade-concepts)
  :computed-slots ((displayed-geometry 
                    (list 
                     :wt-assembly
                     (flatten (list (list-elements (the-object (the rotor rotor-assembly) hub))
                                    (mapcar #'(lambda(leaves) (list-elements leaves))
                                            (list-elements (the-object (the rotor rotor-assembly) blade)))
                                    (the rotor tower)
                                    (list-elements (the-object (the rotor rotor-assembly) turbine-nacelle))))
                                   
                     :rotor-assembly
                     (flatten (list 
                               (list-elements (the-object (the rotor rotor-assembly) hub))
                               (mapcar #'(lambda(leaves) (list-elements leaves))
                                       (list-elements (the-object (the rotor rotor-assembly) blade)))))
                     
                     :hub-assembly (list-elements (the-object (the rotor rotor-assembly) hub))
                     :nacell-assembly (list-elements (the-object (the rotor rotor-assembly) turbine-nacelle ))
                     :blade (list-elements (the-object (the rotor rotor-assembly) rotor-hidded))
                     :tower (list (the rotor tower))
                     
                     )))
  
  :objects               
  ((rotor :type 'assembly
          :blade-concepts (the blade-concepts)
          :nr-blades-data (the number-of-blades))
   
   (box :type 'surf::test-b-spline-surface
        :sequence (:size 30))
   
    (graphics :type 'base-ajax-graphics-sheet
              :image-format-default :png
              :view-direction-default :trimetric
              :viewport-border-default 0
              :display-list-objects (getf (the displayed-geometry) (the geometry ))
              :length 702
              :width 690)))
    
  
 (define-object displayed-component (sheet-section)
  :computed-slots
  ((main-view (with-cl-who-string ()
                (str (the component-display-control html-string))
                (:h3 (str (if (eql (the component-display-control value) :rotor-assembly)
                             "Blade concepts"       
                            "" )))
                
                (str (if (eql (the component-display-control value) :rotor-assembly)
                         (the blade-concepts html-string) ""))
                )))
               
   :objects               
   ((component-display-control :type 'menu-form-control
                              :choice-plist (list  :wt-assembly "Turbine assembly all comp."
                                                   :rotor-assembly "Rotor assembly"
                                                   :hub-assembly "Hub assembly"
                                                   :nacell-assembly "Nacell assembly"
                                                   :blade "Blade"
                                                   :tower "Tower") 
                                                   :default :wt-assembly
                                                   :size 1
                                                   :prompt nil
                                                   :ajax-submit-on-change? t)
    
    
    (blade-concepts :type 'menu-form-control
                              :choice-plist (list  :classic-blade "Classic blade"
                                                   :blade-with-flaps "Blade with flaps"
                                                   :blade-with-morfin-tip "Blade with morfin tip"
                                                   :blade-with-m-tip-flaps "Blade with M-tip-flaps")
                                                   :default  :classic-blade
                                                   :size 1
                                                   :prompt nil
                                                   :ajax-submit-on-change? t)
    
    
    
    ))



(publish-gwl-app "/demo-1"
                 "wind::demo-1")

(publish-directory :prefix "/css-demo-1"
                   :destination (format nil "~a" (merge-pathnames "css-demo-1" *data-pathname*)))
