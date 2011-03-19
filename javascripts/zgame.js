
function createXMLHttpRequest() {
  try { return new ActiveXObject("Msxml2.XMLHTTP");    } catch(e) {}
  try { return new ActiveXObject("Microsoft.XMLHTTP"); } catch(e) {}
  try { return new XMLHttpRequest();                   } catch(e) {}
  alert("XMLHttpRequest not supported");
  return null;
}

var xReq = createXMLHttpRequest();

$(document).ready(function() {
    setInterval(xReqCheck, 100);
    xReq.open('GET', 'http://localhost:8080/test2', true);
    xReq.onreadystatechange = function() {
      if (xReq.readyState==4) { alert("connection closed"); }
    }
    xReq.send();
});

function xReqCheck() {
  //var fullResponse = xhReq.responseText;
  //var responsePatt = I/$/;
  //if (fullResponse.match(responsePatt)) { // At least one full response so far
    //var mostRecentDigit = fullResponse.replace(responsePatt, "$2");

    $("#other").append('i');
    $("#other").append(xReq.responseText);
  //}
}
