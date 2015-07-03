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

	//
	//// Let user reset explicitly.
	//
        //document.getElementById(min_id).value = start_min; 
        //document.getElementById(sec_id).value = start_sec; 
        
        alert('Please record your journal entry for this time.'); 
        
        return;
    }


}

// Code for allowing journal entries to be repeated easily. 
// Journal entry changes color on hover, 
// contents get placed into the form on click. 

$(document).ready(function(){
    $(document).on("mouseenter", ".journal-entry", function(){ 
	$(this).css('color', 'blue'); 
	$(this).css('cursor', 'pointer'); 
    });

    $(document).on("mouseleave", ".journal-entry", function(){ 
	$(this).css('color', '#000'); 
    }); 
    
    $(document).on("click", ".journal-entry", function(){ 
	alert($("#journal-descr").val()); 
	$("#journal-descr").val($(this).find(".journal-descr").html()); 
    }); 

}); 
