(in-package :ta2)

(defparameter *javascript-functions*
    "


function createRequest() {
  var request

  try {
    request = new XMLHttpRequest();
  } catch (trymicrosoft) {
    try {
      request = new ActiveXObject('Msxml2.XMLHTTP');
    } catch (othermicrosoft) {
      try {
        request = new ActiveXObject('Microsoft.XMLHTTP');
      } catch (failed) {
        request = false;
      }
    }
  }

  if (!request)
    alert('failed to create XMLHttpRequest');

  return request
}



function ta2operate (iid, rootPath) {



  var myreq = createRequest();
  var url = '/ta2operate';
  var params = 'iid' + '=' + iid + '&' +
               'rootPath' + '=' + rootPath;

  ta2ajax(myreq, url, params);

}


function ta2treetoggle(iid, rootPath) {

  var myreq = createRequest();
  var url = '/ta2treetoggle';
  var params = 'iid' + '=' + iid + '&' +
               'rootPath' + '=' + rootPath;

  ta2ajax(myreq, url, params);

}


function ta2clickbutton (iid, mode) {

  var myreq = createRequest();
  var url = '/ta2clickbutton';
  var params = 'iid' + '=' + iid + '&' +
               'mode' + '=' + mode;


  ta2ajax(myreq, url, params);

}


function ta2setexpandmode (iid, mode) {

  var myreq = createRequest();
  var url = '/ta2setexpandmode';
  var params = 'iid' + '=' + iid + '&' +
               'mode' + '=' + mode;
  
  ta2ajax(myreq, url, params);
}




function gdlsetslots (iid, rootPath, keys, values) {

  var params = 'iid' + '=' + iid + '&' +
               'rootPath' + '=' + rootPath + '&' +
               'keys' + '=' + keys + '&' +
               'values' + '=' + values;

  var myreq = createRequest();
  var url = '/gdlsetslots';
  ta2ajax(myreq, url, params);
}



function gdlsetcontrol (iid, rootPath, control, value) {

  
  var myreq = createRequest();
  var url = '/gdlsetcontrol';
  var params = 'iid' + '=' + iid + '&' +
               'rootPath' + '=' + rootPath + '&' +
               'control' + '=' + control + '&' +
               'value' + '=' + value;
  
  ta2ajax(myreq, url, params);
}



function ta2setcontrol (iid, control, value) {



  var myreq = createRequest();
  var url = '/ta2setcontrol';
  var params = 'iid' + '=' + iid + '&' +
               'control' + '=' + control + '&' +
               'value' + '=' + value;
  
  ta2ajax(myreq, url, params);
}




function ta2evaluate (iid, message) {
  
  var myreq = createRequest();
  var url = '/ta2evaluate';
  var params = 'iid' + '=' + iid + '&' +
               'message' + '=' + message;

  ta2ajax(myreq, url, params);
}



function ta2ajax (myreq, url, params)
{


  myreq.open('POST', url, true);
  myreq.setRequestHeader('content-type', 'application/x-www-form-urlencoded');
  myreq.setRequestHeader('content-length', params.length);
  myreq.setRequestHeader('user-agent', 'Firefox3-ajax');
  myreq.setRequestHeader('connection', 'close');

  myreq.onreadystatechange = function () {ta2update(myreq)};

  // alert(myreq);

  // alert(params + ' ' + params.length);

  myreq.send(params);

  }



function ta2update (myreq) {


 if (myreq.readyState == 1)
   document.getElementById('ta2status').innerHTML = 'Working...';

 if (myreq.readyState == 2)
   document.getElementById('ta2status').innerHTML = 'Still Working...';

 if (myreq.readyState == 3)
    document.getElementById('ta2status').innerHTML = 'Almost There...';

 if ((myreq.readyState == 4) && (myreq.status == 200))
  {
  


   var root = myreq.responseXML.documentElement;

   var children = root.childNodes;

   for (i=0; i< children.length; i++)
   {
    var child=children[i];
    var myid = child.getElementsByTagName('replaceId')[0].firstChild.data;
    var newHTML = child.getElementsByTagName('newHTML')[0].firstChild.nodeValue;
    document.getElementById(myid).innerHTML = newHTML;
   }

    document.getElementById('ta2status').innerHTML = 'Done.';

   }}




/**
* returns the absolute left location of an element.
* param: e: element
* return: an integer representing the offset from left.
*/
function getElementLeftPosition(e){
var x=0;
while(e){
x+=e.offsetLeft;
e=e.offsetParent;
}
return x;
}

/**
* returns the absolute top location of an element.
* param: e: element
* return: an integer representing the offset from top.
*/
function getElementTopPosition(e){
var y=0;
while(e){
y+=e.offsetTop;
e=e.offsetParent;
}
return y;
}


function scrolledClientX (evt)
{
 var x = evt.clientX-getElementLeftPosition(document.getElementById('myimage'));
 var scroll = 0;
 if (self.pageXOffset) 
   {scroll = self.pageXOffset;}
 else if (document.documentElement && document.documentElement.scrollLeft)
   {scroll = document.documentElement.scrollLeft;}
 else if (document.body)
   {scroll = document.body.scrollLeft;}
 return scroll + x;
}


function scrolledClientY (evt)
{
 var y = evt.clientY-getElementTopPosition(document.getElementById('myimage'));
 var scroll = 0;
 if (self.pageYOffset) 
   {scroll = self.pageYOffset;}
 else if (document.documentElement && document.documentElement.scrollTop)
   {scroll = document.documentElement.scrollTop;}
 else if (document.body)
   {scroll = document.body.scrollTop;}
 return scroll + y;
}



function digitizePoint(evt, iid, rootPath)
 {

  var myreq = createRequest();
  var url = '/digitizePoint';

  var x = scrolledClientX(evt);
  var y = scrolledClientY(evt);


  //var x = evt.clientX-getElementLeftPosition(document.getElementById('myimage'));
  //var y = evt.clientY-getElementTopPosition(document.getElementById('myimage'));


  //var x = evt.pageX-getElementLeftPosition(document.getElementById('myimage'));
  //var y = evt.pageY-getElementTopPosition(document.getElementById('myimage'));

  var params = 'iid' + '=' + iid + '&' +
               'rootPath' + '=' + rootPath + '&' +
               'x' + '=' + x + '&' +
               'y' + '=' + y;

  myreq.open('POST', url, true);
  myreq.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
  myreq.setRequestHeader('Content-length', params.length);
  myreq.setRequestHeader('Connection', 'close');

  myreq.onreadystatechange = function () {ta2update(myreq)};

  myreq.send(params);

 }


// This code was written by Tyler Akins and has been placed in the
// public domain.  It would be nice if you left this header intact.
// Base64 code from Tyler Akins -- http://rumkin.com

var keyStr = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-/_';

function encode64(input) {
   var output = '';
   var chr1, chr2, chr3;
   var enc1, enc2, enc3, enc4;
   var i = 0;

   do {
      chr1 = input.charCodeAt(i++);
      chr2 = input.charCodeAt(i++);
      chr3 = input.charCodeAt(i++);

      enc1 = chr1 >> 2;
      enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
      enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
      enc4 = chr3 & 63;

      if (isNaN(chr2)) {
         enc3 = enc4 = 64;
      } else if (isNaN(chr3)) {
         enc4 = 64;
      }

      output = output + keyStr.charAt(enc1) + keyStr.charAt(enc2) + 
         keyStr.charAt(enc3) + keyStr.charAt(enc4);
   } while (i < input.length);
   
   return output;
}

function decode64(input) {
   var output = '';
   var chr1, chr2, chr3;
   var enc1, enc2, enc3, enc4;
   var i = 0;

   // remove all characters that are not A-Z, a-z, 0-9, +, /, or =
   //input = input.replace(/[^A-Za-z0-9\+\/\=]/g, '');
   input = input.replace(/[^A-Za-z0-9\-\_\=]/g, '');

   do {
      enc1 = keyStr.indexOf(input.charAt(i++));
      enc2 = keyStr.indexOf(input.charAt(i++));
      enc3 = keyStr.indexOf(input.charAt(i++));
      enc4 = keyStr.indexOf(input.charAt(i++));

      chr1 = (enc1 << 2) | (enc2 >> 4);
      chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
      chr3 = ((enc3 & 3) << 6) | enc4;

      output = output + String.fromCharCode(chr1);

      if (enc3 != 64) {
         output = output + String.fromCharCode(chr2);
      }
      if (enc4 != 64) {
         output = output + String.fromCharCode(chr3);
      }
   } while (i < input.length);

   return output;
}




")
