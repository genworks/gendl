var minutes, seconds; 
var interval; 
var timerStarted = false; 

var start_min, start_sec; 
var min_id, sec_id; 


function timerPause() {clearInterval(interval); reportToMother(); timerStarted = false;}

function timerStart(minid, secid) {

    if (timerStarted) {
	return; 
    }

    timerStarted = true; 

    minutes = parseInt(document.getElementById(minid).value); 
    seconds = parseInt(document.getElementById(secid).value); 

    min_id = minid; 
    sec_id = secid; 
    
    start_min = minutes; 
    start_sec = seconds;
    
    interval = setInterval(countDown, 1000); 

}

function countDown() {

    seconds--; 
    if (seconds==-1) {
	seconds = 59; 
	minutes--; 
    }

    document.getElementById(min_id).value = minutes; 
    document.getElementById(sec_id).value = seconds; 
    
    if (minutes==0 && seconds==0) {
        clearInterval(interval);
	reportToMother();
        timerStarted = false; 

	// Sorry, we have to keep these lines for now. 
        document.getElementById(min_id).value = start_min; 
        document.getElementById(sec_id).value = start_sec; 
        
        alert('Please record your journal entry for this time.'); 
        
        return;
    }


}

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
	$("#journal-descr").val($(this).find(".journal-descr").html()); 
    }); 

// Code for disappearing default values in journal form fields onclick.

    $(document).on("click", "#journal-name", function(){ 
	if($(this).val()=="Name") {
	    $(this).val(""); 
	}
    }); 

    $(document).on("click", "#journal-descr", function(){
	if($(this).val()=="Description of task") {
	    $(this).val(""); 
	}
    }); 

// HideSeek code for implementing the live search.

    $("#journal-descr").data("list", ".journal"); 
    $("#journal-descr").hideseek(); 

}); 


