$(document).ready(function(){

// Code for allowing journal entries to be repeated easily. 
// Journal entry changes color on hover, 
// contents get placed into the form on click. 

    $(document).on("mouseenter", ".journal-entry", function(){ 
	$(this).css('background-color', 'rgba(255,255,255,0.5)'); 
	$(this).css('cursor', 'pointer'); 
    });

    $(document).on("mouseleave", ".journal-entry", function(){ 
	$(this).css('background-color', 'rgba(255,255,255,0)'); 
    }); 
    
    $(document).on("click", ".journal-entry", function(){ 
	$("#journal-entry").val($(this).find(".journal-descr").html());  
    }); 

// Code for disappearing default values in journal form fields onclick.

    $(document).on("click", "#user-name", function(){ 
	if($(this).val()=="Name") {
	    $(this).val(""); 
	}
    }); 

    $(document).on("click", "#user-email", function(){ 
        if($(this).val()=="Email") {
            $(this).val(""); 
        }
    }); 
    
    $(document).on("click", "#journal-entry", function(){
	if($(this).val()=="Description of task") {
	    $(this).val(""); 
	}
    }); 

// HideSeek code for implementing the live search.

//    $("#journal-entry").data("list", "#journal"); 
//    $("#journal-entry").hideseek(); 

}); 
