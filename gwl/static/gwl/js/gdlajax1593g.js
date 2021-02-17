/*
;;
;; EXCEPT WHERE OTHERWISE NOTED BELOW:
;;
;; Copyright 2002-2011 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 
*/

var doublequote = '\"';

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


function gdlAjax1 (params, asynch)
{
    gdlAjax(null, params, async);
}

function gdlAjax (evt, params, asynch)
{


 if (evt)
 {
  var target;

  if (evt.target) target = evt.target;
  if (evt.srcElement) target = evt.srcElement;

  while ((target.tagName != 'DIV') && (target.tagName != 'BODY')){target = target.parentNode;}

  var x = evt.clientX-getElementLeftPosition(target);
  var y = evt.clientY-getElementTopPosition(target);
 }

  var request = createRequest();
  var url = "/gdlAjax";

  params = params + '&x=' + x + '&y=' + y;

  request.onreadystatechange = function () {gdlUpdate(request)};

  request.open('POST', url, asynch);
  request.setRequestHeader('content-type', 'application/x-www-form-urlencoded');

  request.send(params);

  }


function gdlUpdate (request) {

 if (request.readyState == 1)
   if (document.getElementById('gdlStatus'))
    document.getElementById('gdlStatus').innerHTML = 'Working...';

 if (request.readyState == 2)
   if (document.getElementById('gdlStatus'))
    document.getElementById('gdlStatus').innerHTML = 'Got Error!';

 if (request.readyState == 3)
   if (document.getElementById('gdlStatus'))
     document.getElementById('gdlStatus').innerHTML = 'Almost There...';

 if ((request.readyState == 4) && (request.status == 200))
    {

	var root = request.responseXML.documentElement;
	var children = root.childNodes;
	var myelem;
	var codes;

	for (i=0; i< children.length; i++)
	{
	    var child=children[i];
	    var myid = null;
	    if (child.getElementsByTagName('replaceId')[0].firstChild != null)
            {
		myid = child.getElementsByTagName('replaceId')[0].firstChild.data
            }

	    var newHTML = null;
	    
	    if (child.getElementsByTagName('newHTML')[0].firstChild != null)
            {newHTML = child.getElementsByTagName('newHTML')[0].firstChild.nodeValue}

	    var jsToEval = null;

	    if (child.getElementsByTagName('jsToEval')[0].firstChild != null)
	    {jsToEval = child.getElementsByTagName('jsToEval')[0].firstChild.nodeValue}

	    if (myid && (newHTML != null))
            {
		var myelem = document.getElementById(myid);

		myelem.innerHTML = newHTML;

		if (jsToEval && (jsToEval == 'parseme'))
		{
		    codes = myelem.getElementsByTagName("script");
		    
		    for (var j=0;j<codes.length;j++)
		    {
			var text = codes[j].text;
			if (text) eval(text);
		    }}
		
            }
	    
	    if (jsToEval && (jsToEval != 'parseme') && (jsToEval != ''))
	    eval(jsToEval);
	}


	if (document.getElementById('gdlStatus'))
	{
	    document.getElementById('gdlStatus').innerHTML = 'Done.';
	}
    }}


// This code was written by Tyler Akins and has been placed in the
// public domain.  It would be nice if you left this header intact.
// Base64 code from Tyler Akins -- http://rumkin.com

var keyStr = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=';

var ua = navigator.userAgent.toLowerCase();
if (ua.indexOf(" chrome/") >= 0 || ua.indexOf(" firefox/") >= 0 || ua.indexOf(' gecko/') >= 0) {
        var StringMaker = function () {
                this.str = "";
                this.length = 0;
                this.append = function (s) {
                        this.str += s;
                        this.length += s.length;
                }
                this.prepend = function (s) {
                        this.str = s + this.str;
                        this.length += s.length;
                }
                this.toString = function () {
                        return this.str;
                }
        }
} else {
        var StringMaker = function () {
                this.parts = [];
                this.length = 0;
                this.append = function (s) {
                        this.parts.push(s);
                        this.length += s.length;
                }
                this.prepend = function (s) {
                        this.parts.unshift(s);
                        this.length += s.length;
                }
                this.toString = function () {
                        return this.parts.join('');
                }
        }
}


function encode64(input) {
    var output = new StringMaker();
    var chr1, chr2, chr3;
    var enc1, enc2, enc3, enc4;
    var i = 0;
    
    while (i < input.length) {
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
	
        output.append(keyStr.charAt(enc1) + 
                      keyStr.charAt(enc2) + 
                      keyStr.charAt(enc3) + 
                      keyStr.charAt(enc4));
    }
    
    return output.toString().replace(/\=/g, '');
}


function decode64(input) {
        var output = new StringMaker();
        var chr1, chr2, chr3;
        var enc1, enc2, enc3, enc4;
        var i = 0;

        input = input.replace(/[^A-Za-z0-9\-\_\=]/g, '');

        while (i < input.length) {
                enc1 = keyStr.indexOf(input.charAt(i++));
                enc2 = keyStr.indexOf(input.charAt(i++));
                enc3 = keyStr.indexOf(input.charAt(i++));
                enc4 = keyStr.indexOf(input.charAt(i++));

                chr1 = (enc1 << 2) | (enc2 >> 4);
                chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
                chr3 = ((enc3 & 3) << 6) | enc4;

                output.append(String.fromCharCode(chr1));

                if (enc3 != 64) {
                        output.append(String.fromCharCode(chr2));
                }
                if (enc4 != 64) {
                        output.append(String.fromCharCode(chr3));
                }
        }

        return output.toString();
}


//
// debouncing technique from https://css-tricks.com/snippets/jquery/done-resizing-event/.
//
// More general debouncing function here: https://davidwalsh.name/javascript-debounce-function
//
var resizeTimer;

function gdlResize()
{

    // if  (document.getElementById('x3dom-1'))
    // {}
    //else
    //{
	      
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function () {
    
	gdlAjax(null, 'args=' + encode64('(:|iid| '+ doublequote + gdliid + doublequote + ' :|bashee| (:%rp% nil) :|function| :set-slot! :|arguments| (:viewport-dimensions (:width ' + (document.getElementById('viewport').getBoundingClientRect().width) +  ' :length ' + (document.getElementById('viewport').getBoundingClientRect().height) + ')))'), true );}, 250);

// }

}



function collectMenuSelections(select)
{
    var items = "";
    for (var i = 0; i < select.options.length; i++)
        if (select.options[i].selected)
            items = items + ':|' + select.name + '| ' + doublequote + encode64(select.options[i].value) + doublequote + ' ';

    if (items)
	return(items);
    else
	return(':|' + select.name | '|' + doublequote + encode64('nil') + doublequote + ' ');
}


function loadScript(url){

    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src = url;
    document.getElementsByTagName("head")[0].appendChild(script);
}



