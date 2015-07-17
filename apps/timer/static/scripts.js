var minutes, seconds; 
var interval; 
var timerStarted = false; 

function timerStart() {

    if (timerStarted)
        return; 

    timerStarted = true; 

    minutes = parseInt(document.getElementById("minutes").value); 
    seconds = parseInt(document.getElementById("seconds").value); 
    
    startTimerAjax(); 
    
    interval = setInterval(countDown, 1000); 

}

function timerPause() {
    clearInterval(interval); 
    timerStarted = false;
}

function timerReset() {
    resetTimerAjax(); 
    
    clearInterval(interval);
    timerStarted = false; 
    
    document.getElementById("minutes").value = "20"; 
    document.getElementById("seconds").value = "00"; 
}

function recordJournal() {
    recordJournalAjax(); 
}

function countDown() {
    seconds--; 
    if (seconds==-1) {
	seconds = 59; 
	minutes--; 
    }

    document.getElementById("minutes").value = minutes; 
    document.getElementById("seconds").value = seconds; 
    
    if (minutes==0 && seconds==0) {
        clearInterval(interval);
        timerStarted = false; 
        
        alert('Please record your journal entry for this time.'); 
        
        endTimerAjax(); 
        
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

    $("#journal-descr").data("list", ".journal"); 
    $("#journal-descr").hideseek(); 

}); 


