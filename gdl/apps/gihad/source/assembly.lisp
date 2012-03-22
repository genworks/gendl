(in-package :gihad)

(define-object assembly (base-ajax-sheet)

  

  :computed-slots
  ((use-jquery? t)

   (additional-header-content 
    (with-cl-who-string ()

      ((:link :type "text/css" :rel "stylesheet" :href "/static/3rdpty/jquery/css/layout-default-latest.css"))

      ((:style :type "text/css")
       "
	p {
		font-size:		1em;
		margin:			1ex 0;
	}
	p.buttons {
		text-align:		center;
		line-height:	2.5em;
	}
	button {
		line-height:	normal;
	}
	.hidden {
		display:		none;
	}

	/*
	 *	Rules for simulated drop-down/pop-up lists
	 */
	ul {
		/* rules common to BOTH inner and outer UL */
		z-index:	100000;
		margin:		1ex 0;
		padding:	0;
		list-style:	none;
		cursor:		pointer;
		border:		1px solid Black;
		/* rules for outer UL only */
		width:		15ex;
		position:	relative;
	}
	ul li {
		background-color: #EEE;
		padding: 0.15em 1em 0.3em 5px;
	}
	ul ul {
		display:	none;
		position:	absolute;
		width:		100%;
		left:		-1px;
		/* Pop-Up */
		bottom:		0;
		margin:		0;
		margin-bottom: 1.55em;
	}
	.ui-layout-north ul ul {
		/* Drop-Down */
		bottom:		auto;
		margin:		0;
		margin-top:	1.45em;
	}
	ul ul li		{ padding: 3px 1em 3px 5px; }
	ul ul li:hover	{ background-color: #FF9; }
	ul li:hover ul	{ display:	block; background-color: #EEE; }

")))


   (additional-header-js-content 
    (with-cl-who-string ()
      ((:script :type "text/javascript")
       "$(document).ready(function () {
  myLayout = $('body').layout({
                //      applyDefaultStyles: true
		//	enable showOverflow on west-pane so CSS popups will overlap north pane
                	west__showOverflowOnHover: true

		//	reference only - these options are NOT required because 'true' is the default
		,	closable:				true	// pane can open & close
		,	resizable:				true	// when open, pane can be resized 
		,	slidable:				true	// when closed, pane can 'slide' open over other panes - closes on mouse-out

		//	some resizing/toggling settings
		,	north__slidable:		false	// OVERRIDE the pane-default of 'slidable=true'
		,	north__togglerLength_closed: '100%'	// toggle-button is full-width of resizer-bar
		,	north__spacing_closed:	20		// big resizer-bar when open (zero height)
		,	south__resizable:		false	// OVERRIDE the pane-default of 'resizable=true'
		,	south__spacing_open:	0		// no resizer-bar when open (zero height)
		,	south__spacing_closed:	20		// big resizer-bar when open (zero height)
		//	some pane-size settings
		,	west__minSize:			100
		,	east__size:				300
		,	east__minSize:			200
		,	east__maxSize:			Math.floor(screen.availWidth / 2) // 1/2 screen width
		,	center__minWidth:		100

		,	useStateCookie:			true


});






});

")))

   (main-sheet-body
    
    (with-cl-who-string (:indent t)

      
      
      ((:div  :class "ui-layout-center")
       "This is the center pane")

      ((:div  :class "ui-layout-north" :onmouseover "myLayout.allowOverflow('north')" :onmouseout "myLayout.resetOverflow(this)")
       "This is the north pane"
       (str (the development-links)))
      ((:div  :class "ui-layout-east")
       "This is the east pane")
      ((:div  :class "ui-layout-west")
       "This is the west pane")
      ((:div  :class "ui-layout-south")
       "This is the south pane")
      


      ))))
