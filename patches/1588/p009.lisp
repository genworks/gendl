(in-package :gdl)

(defparameter *1588p009-doc* 
  "Makes web-drawing and tasty respect background and foreground in *colors-default*.")


(#+allegro 
 excl:without-package-locks #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings
  #-allegro progn

  (defmethod lookup-color ((color null) &key (format :decimal) (ground :foreground))
    (unless (and (eql ground :foreground)
		 (eql (getf *colors-default* :foreground) :black))
      (values (lookup-color (getf *colors-default* ground) :format format) nil)))))

(in-package :geom-base)

(#+allegro 
 excl:without-package-locks #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings
  #-allegro progn

  (define-lens  (pdf base-object) ()
    :amend? t
    :output-functions
    ((rgb-stroke-setting
      ()
      (let* ((display-controls (find-in-hash self *display-controls*))
	     (color (or (getf display-controls :color)
			(the color-decimal)))
	     (color-decimal (if (consp color) color 
				(coerce (lookup-color (or color (format-slot foreground-color))) 'list)))
	     (fill-color-decimal (coerce (lookup-color (getf (the display-controls) :fill-color)) 'list)))
      
	(apply #'pdf:set-rgb-stroke color-decimal)
	(apply #'pdf:set-rgb-fill (or fill-color-decimal color-decimal))))))


  (define-object-amendment base-object ()
    :computed-slots
    ((color-decimal (lookup-color (getf (the display-controls) :color)))))))
   
	   

(in-package :raphael)


(#+allegro 
 excl:without-package-locks #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings
  #-allegro progn

  (define-lens (raphael base-drawing) ()
    :amend? t
    :output-functions
    ((raphael-paper-def
      (&key width length)
      (format *stream* "var paper = Raphael('~a', ~a, ~a);

               paper.canvas.style.backgroundColor = '~a';


               if (typeof start === 'undefined') {

                var start = function () {
                    this.lastdx ? this.odx += this.lastdx : this.odx = 0;
                    this.lastdy ? this.ody += this.lastdy : this.ody = 0;
                    this.animate({opacity: .5}, 500, \">\");
                },


                move_cb = function (dx, dy) {
                    this.transform(\"T\"+(dx+this.odx)+\",\"+(dy+this.ody));
                    this.lastdx = dx;
                    this.lastdy = dy;
                    this.animate({opacity: .5}, 500, \">\");
                    ~a 
                },

                move = function (dx, dy) {
                    this.transform(\"T\"+(dx+this.odx)+\",\"+(dy+this.ody));
                    this.lastdx = dx;
                    this.lastdy = dy;
                    this.animate({opacity: .5}, 500, \">\");
                },

                up = function () {
                    this.animate({opacity: 1.0}, 500, \">\");
                    ~a 
                },

                  touchcoords = function () {~a}};

"
	      (the raphael-canvas-id) width length

	      (lookup-color (format-slot background-color) :format :hex)

	      ;;
	      ;; FLAG -- pass in the containing
	      ;; base-ajax-graphics-sheet and refer
	      ;; to that, instead of referring to
	      ;; the parent here.
	      ;;
	      (the parent (gdl-sjax-call :null-event? t :js-vals? t :function-key :on-drag))
	      (the parent (gdl-sjax-call :null-event? t :js-vals? t :function-key :on-drop))

	      (the parent (gdl-sjax-call :null-event? t :js-vals? t :function-key :on-touchmove))))))))

  
