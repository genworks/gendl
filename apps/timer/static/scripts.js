var minutes, seconds; 
var interval; 
var timerStarted = false; 

var start_min, start_sec; 
var min_id, sec_id; 

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

    if (minutes==0 && seconds==0) {
        clearInterval(interval); 
        timerStarted = false; 
        
        document.getElementById(min_id).value = start_min; 
        document.getElementById(sec_id).value = start_sec; 
        
        alert('Please record your journal entry for this time.'); 
        
        return;
    }

    document.getElementById(min_id).value = minutes; 
    document.getElementById(sec_id).value = seconds; 

}