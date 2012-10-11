(in-package :gdl-tutorial)

(defparameter *example-3*
    `((:chapter :title "Example 3: School Bus")
      
      "In this chapter we leave you with another example of a geometric
GDL/GWL application, heavy on code examples, with a bit of explanation
sprinkled in between. This School Bus example introduces the use of
the "
      (:texttt (:indexed "node-mixin"))
      " primitive object, which is similar to "
      (:texttt (:indexed "application-mixin"))
      " which we met in the last chapter. But "
      (:texttt "node-mixin")
      " is used to "
      (:emph "contain")
      " other instances of either "
      (:texttt "node-mixin")
      " or "
      (:texttt "application-mixin")
      ", and automatically collects up any "
      (:texttt (:indexed "ui-display-list-objects"))
      " or rule objects (i.e.\\ objects of type "
      (:texttt (:indexed "gwl-rule-object"))
      ") from its descendants."

      ((:section :title "Toplevel Assembly")
       "Figure "
       (:ref "code:school-bus")
       " defines the toplevel assembly consisting of a chassis, body, and interior. 

The toplevel mixes in "
       (:texttt "node-mixin")
       ", and the chassis, body, and interior each mix in "
       (:texttt "application-mixin")
       ". This results in a high-level user-visible hierarchy, shown as 
the ``Assembly Tree'' in the lower-left of Figure "
       (:ref "fig:school-bus")
       ". Note that there is no need to specify a "
       (:texttt "ui-display-list-objects")
       " slot in the "
       (:texttt "assembly")
       ". This is because "
       (:texttt "node-mixin")
       " automatically defines this slot, which appends together the "
       (:texttt "ui-display-list-objects")
       " from any child objects of appropriate types.
       
Three toplevel "
       (:texttt ":settable")
       " computed-slots are also specified, affecting the overall dimensions of the vehicle. 

Figure "
       (:ref "code:school-bus-model-inputs")
       " defines the "
       (:texttt "model-inputs")
       " for the toplevel, corresponding to the three "
       (:texttt ":settable")
       " computed-slots in the "
       (:texttt "assembly")
       ". 

The complete code for the School Bus example is provided on the GDL (and Trial Edition) CD;
in this tutorial we provide only the major portion of the interior. The chassis and body are 
defined similarly, although the chassis in particular contains several interesting examples,
which are beyond the scope of this tutorial, of solving somewhat ``heavier'' engineering 
problems with GDL/GWL."
       
      
       ((:boxed-figure :caption "Toplevel Assembly for School Bus"
		       :label "code:school-bus")
	(:verbatim  "


 (define-object assembly (node-mixin)

   :computed-slots
   ((frame-datum (let ((datum
                        (read-safe-string (string-append
                                           \"(\"
                                           (the frame-datum-m)
                                           \")\"))))
                   (translate (the center) :right (first datum) :rear
                              (second datum) :top (third datum))))
    (strings-for-display \"School Bus\")
    (wheelbase 300 :settable)
    (track 96 :settable)
    (height 80 :settable)
    (frame-datum-m \"2000 500 0\" :settable))

   :objects
   ((chassis :type 'chassis
             :pass-down (:wheelbase :track)
             :datum (the frame-datum)
             :height 20)
    (body :type 'body
          :pass-down (:wheelbase :track)
          :frame-width (the chassis frame-width)
          :frame-overhang (- (the-child front-overhang) 
                             (the chassis front-overhang))
          :firewall-base (translate 
                          (the frame-datum) :up
                          (half (the chassis frame-height)) :right
                          (- (the-child cab-width)
                             (the-child frame-overhang))))
    (interior :type 'interior
              :firewall-base (the body firewall-base)
              :width (- (the body width) (the body cab-width))
              :length (the body length)
              :height (the body height))))
"))
      
      
       ((:boxed-figure :caption "HTML format View of Model Inputs for School Bus Toplevel"
		       :label "code:school-bus-model-inputs")
	(:verbatim  "


 (define-view (html-format assembly) nil
  
   :output-functions
   ((model-inputs
     nil
     (html 
      (:p
       (:table
        (:tr ((:td :bgcolor :yellow) \"Wheelbase\")
             (:td
              ((:input :type :text :size 5 :name :wheelbase :value
                       (the :wheelbase)))))
        (:tr ((:td :bgcolor :yellow) \"Track\")
             (:td ((:input :type :text :size 5 
                           :name :track :value (the :track)))))
        (:tr ((:td :bgcolor :yellow) \"Height\")
             (:td
              ((:input :type :text :size 5 
                       :name :height :value (the :height)))))))
      (:p ((:input :type :submit :name :submit :value \" OK \")))))))
"))
      
      
       ((:image-figure :image-file "school-bus.png"
                       :caption "Toplevel Sheet of School Bus App"
                       :label "fig:school-bus")))

      ((:section :title "Interior of School Bus")

       "Figures "
       (:ref "code:school-bus-interior")
       " and "
       (:ref "code:school-bus-seating-section")
       " define the major components making up the interior of the bus, which for our
current purposes consists of the bench seats and a few rules regarding their 
spacing.

In particular, Figure "
       (:ref "code:school-bus-seating-section")
       " defines the "
       (:texttt "seating-section")
       " which contains the two actual columns of bench seats. This object also contains
two "
       (:emph "rule objects")
       " which compute certain key pieces of information:"
       ((:list :style :description)
	((:item :word "inter-seat-spacing-computation")
	 "computes the exact spacing from the front of one seat to the front of the next,
given the available cabin width (i.e.\\ coach length), the maximumm allowed recline angle
of the seat backs, and the number of rows to be fit into the bus. (The actual seat 
dimensions are taken from defaults in the seat object definition, not listed here).

This spacing value is crucial for two reasons: first, this value is used in order to 
generate the actual geometric objects that you see in the graphical output. Second,
this value is used in the "
	 (:texttt "inter-seat-clearance-check")
	 " to compare with the overall length of one seat, to compute how much is 
left over to be considered ``legroom.''")
	(:index "rules!diagnostic")
	(:index "rules!generative")
	(:index "rules!violating")
	(:index "rules!model!tight integration with")
	(:index "rules")
	
	((:item :word "inter-seat-clearance-check")
	 "is a purely diagnostic rule which uses values from the spacing computation
rule in order to compute the effective ``legroom'' between seats. This legroom
is compared with the rule's specified "
	 (:texttt "value")
	 " (in this case representing the allowed minimum value), to determine whether the rule
has violated its condition or not."))
       
       "It is typical in a KB model to have this kind of tight integration between
``rules'' and the model itself --- when an input to the model is changed, any
rules and other objects which directly or indirectly depend on that input will 
automatically re-evaluate themselves. Rules and objects which are not affected 
by a given change will avoid the computational work of re-evaluating themselves. 

Maintaining this kind of dependency management in a traditional
procedural language environment becomes extremely burdensome on the
application developer. In a KB environment, however, this dependency
management ``just happens'' as a matter of course."
       (:index "dependency management")
       ((:boxed-figure :caption "Interior Assembly Component School Bus"
		       :label "code:school-bus-interior")
	(:verbatim  "


 (define-object interior (application-mixin)

   :input-slots
   (firewall-base
    length
    width
    height)

   :computed-slots
   ((ui-display-list-objects (the :sections))
    (number-of-rows 10 :settable)
    (reclined-angle 20 :settable)
    (max-reclined-angle 30 :settable)
    (minimum-inter-seat-clearance 7 :settable))

   :objects
   ((sections :type 'seating-section
              :body-reference-points 
              (list :left
                    (translate (the :firewall-base) :front
                               (half (the :length)) :right
                               (the :width))
                    :right
                    (translate (the :firewall-base) :rear
                               (half (the :length)) :right
                               (the :width)))
              :usable-cabin-width (the :width)
              :pass-down (:number-of-rows 
                          :reclined-angle :max-reclined-angle
                          :minimum-inter-seat-clearance))))      
      "))
      
       ((:boxed-figure :caption "HTML Format Model Inputs of School Bus Interior"
		       :label "code:school-bus-interior-model-inputs")
	(:verbatim  "


 (define-view (html-format interior) nil

   :output-functions
   ((model-inputs
     nil
     (html 
      (:p
       (:table
        (:tr 
         ((:td :bgcolor :yellow) \"Rows\")
         (:td
          ((:input :type :text :size 5 
                   :name :number-of-rows :value
                   (the :number-of-rows)))))
        (:tr 
         ((:td :bgcolor :yellow) \"Seat Recline\")
         (:td
          ((:input :type :text :size 5 
                   :name :reclined-angle :value
                   (the :reclined-angle)))))
        (:tr 
         ((:td :bgcolor :yellow) \"Max Recline\")
         (:td
          ((:input :type :text :size 5 
                   :name :max-reclined-angle :value
                   (the :max-reclined-angle)))))
        (:tr 
         ((:td :bgcolor :yellow) \"Req'd Clearance\")
         (:td
          ((:input :type :text :size 5 
                   :name :minimum-inter-seat-clearance
                   :value (the :minimum-inter-seat-clearance)))))))
      (:p ((:input :type :submit :name :submit :value \" OK \")))))))      
      "))

      
       ((:image-figure :image-file "school-bus-interior.png"
                       :caption "Interior of School Bus"
                       :label "fig:school-bus-interior"))
      
      
       ((:boxed-figure :caption "Object Definition for Seating Columns"
		       :label "code:school-bus-seating-section")
	(:verbatim  "


 (define-object seating-section (base-object)

   :input-slots
   (fare-class
    usable-cabin-width
    body-reference-points
    max-reclined-angle
    number-of-rows
    minimum-inter-seat-clearance)

   :objects
   ((inter-seat-spacing-computation 
     :type 'inter-seat-spacing
     :pass-down (:max-reclined-angle 
                 :usable-cabin-width :number-of-rows))
    (inter-seat-clearance-check 
     :type 'inter-seat-clearance-check
     :inter-seat-spacing (the inter-seat-spacing-computation result)
     :clearance-extent-typical (the inter-seat-spacing-computation
                                 clearance-extent-typical)
     :value (the minimum-inter-seat-clearance))
    (sides :type 'seating-side
           :fare-class (the fare-class)
           :sequence (:size 2)
           :side (ecase (the-child index) (0 :left) (1 :right))
           :display-controls (list :color :green)
           :body-reference-point (getf (the body-reference-points) 
                                       (the-child side))
           :pass-down (:number-of-rows :reclined-angle)
           :inter-seat-spacing 
           (the inter-seat-spacing-computation result)
           :x-max-typical 
           (the inter-seat-spacing-computation x-max-typical)
           :x-vector (the (face-normal-vector :right)))))
      "))

       ((:image-figure :image-file "school-bus-interior-front.png"
                       :caption "Front View of School Bus Interior"
                       :label "fig:school-bus-interior-front")))
      
      :newpage
      
      ((:section :title "Causing a Rule Violation")
       "Figure "
       (:ref "fig:school-bus-violated")
       " shows the state of the interior after a user has changed the number of seating
rows to eleven, from the default ten. The user has also changed the displayed recline 
angle of the seat backs to match the maximum allowed value (30 degrees). 

In this state, the seats have redistributed themselves so that they
still are spaced evenly in the available length of the coach. However,
this has caused a violation in the legroom, defined as the horizontal
distance from the rear-center of one seat back to the front of the
seat bottom aft of it. The allowed value for this legroom (``Req'd
Clearance'') is 7, and the current legroom value (as computed by
the rule object) is now less than this.

Therefore we have a violation, and the link to the rule shows up in
the ``Violations'' section of the user interface.

For a typical example such as this School Bus, one can imagine
dozens or hundreds of other rules. Many such rules can be computed
based on information we already have in our model, and others will
result in the model being augmented incrementally with new information
as needed.

The main point is that we now have a stable, user-friendly, and
readily scalable framework in which to represent and grow 
our ``knowledge.''

"
      
       ((:image-figure :image-file "school-bus-interior-violated.png"
                       :caption "Interior of School Bus with 11 Rows (Legroom Violation)"
                       :label "fig:school-bus-violated")))))
