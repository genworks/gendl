var minutes, seconds; 
var interval; 
var timerStarted = false; 

function timerStart() {

    if (timerStarted) return; 
    timerStarted = true; 
    minutes = parseInt(document.getElementById("minutes").value); 
    seconds = parseInt(document.getElementById("seconds").value); 
    startTimerAjax(); 
    interval = setInterval(countDown, 1000); 

}


function timerPause() {
    
    pauseTimerAjax();
    
    clearInterval(interval); 
    timerStarted = false;

    //alert('Minutes = ' +  minutes + ' seconds = ' + seconds);
}

function timerReset() {
    resetTimerAjax(); 
    
    clearInterval(interval);
    timerStarted = false; 
    
    document.getElementById("minutes").value = document.getElementById("default-minutes").value; 
    document.getElementById("seconds").value = document.getElementById("default-seconds").value; 
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
        
        //alert('Please record your journal entry for this time.'); 
        
        endTimerAjax(); 
        
        return;
    }
}


