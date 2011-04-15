
var keyPresses = [];
var command = null;
var timing = true;
//var queue_times = [];


// hotkey assignments:
$(document).bind("keydown", "return", function(e){ open_chat(); });
$(document).bind(
    "keydown", "w", function (evt) {
    pressKey("w");
    });
$(document).bind(
    "keydown", "a", function (evt) {
    pressKey("a");
    });
$(document).bind(
    "keydown", "s", function (evt) {
    pressKey("s");
    });
$(document).bind(
    "keydown", "d", function (evt) {
    pressKey("d");
    });
$(document).bind(
    "keydown", "j", function (evt) {
    command = "shoot";
    });
$(document).bind(
    "keydown", "k", function (evt) {
    command = "close";
    });
$(document).bind(
    "keydown", "l", function (evt) {
    command = "open";
    });

function pickDirection() {
  var direction;
  timing = true;
  key1 = keyPresses[0];
  key2 = keyPresses[1];
  if( (key1 == "w" && key2 == "a") || (key1 == "a" && key2 == "w") ) {
    direction = "northwest";
  } else if( (key1 == "w" && key2 == "d") || (key1 == "d" && key2 == "w") ) {
    direction = "northeast";
  } else if( (key1 == "s" && key2 == "d") || (key1 == "d" && key2 == "s") ) {
    direction = "southeast";
  } else if( (key1 == "s" && key2 == "a") || (key1 == "a" && key2 == "s") ) {
    direction = "southwest";
  } else if( key1 == "w") {
    direction = "north";
  } else if( key1 == "d") {
    direction = "east";
  } else if( key1 == "s") {
    direction = "south";
  } else if( key1 == "a") {
    direction = "west";
  }
  keyPresses = [];
  return direction;
}

function pressKey(Key) {
  keyPresses.push(Key);
  if( timing ){
    timing = false;
    t = setTimeout("doCommand(pickDirection())", 50);
  }
}

function doCommand(direction) {
  if (command != null) {
    post(command, direction);
  } else {
    post("walk", direction);
  }
  command = null;
}

/*
function update_queue() {
  $('#result').append( queue_times.length );
  if( queue_times.length >= 1) {
    t= setTimeout(tick_queue(), queue_times[0]);
  } else {
    t = setTimeout(update_queue(), 100);
  }
}

function tick_queue() {
  queue_times.shift();
  $("#queue:first-child").remove();
  update_queue();
}
*/

function update_stat(obj, value) {
  //alert(obj+" " +value);
  $("#"+obj).html(value);
  $("#"+obj).change();
}

function open_chat() {
  var input = $('#chat_input');
  input.bind("keydown", "return", function(e){ close_chat(); });
  if (input.css('display') == 'none') {
    input.css('display', 'inline');
  }
  input.trigger('focus');
}

function close_chat() {
  var input = $('#chat_input');
  input.css('display', 'none');
  if( input.val() != "" ) {
    post("say", input.val());
    input.val("");
  }
  input.trigger('blur');
}

function post(param, value) {
  $.ajax({  
    type: "POST",  
    url: "data",  
    data: param + '=' + value
  });
}


//      http://localhost/game/[object Object]
function get_data() {
  $.getJSON("data",
    function(data) {
     
      var d = new Date();
      start_time = d.getTime();
      
      get_data();
      update_map(data.map);
      append_log(data.msg);
      //update_flash(data.flash);
      update_data(data.data);

      var t = new Date();
      update_flash(t.getTime() - start_time);

    }
  );
};

function update_data(data) {
  if( data != "nil" ) {
    datas = data.split(";");
    for( d in datas ) {
      a = datas[d].split(",");
      update_stat(a[0], a[1]);
    }
  }
}

function append_log(msg) {
  if( msg != "nil" ) {
    $('#result').append( msg );
    $('#result').animate({scrollTop: $('#result')[0].scrollHeight});
  }
}

function update_flash( msg ) {
  $('#flash').html( msg );
}

function update_map(mapdata) {
  if( mapdata != "nil" ) {
    var map = mapdata.split(",");
     // alert(map);
    var i = 0
    for (r=25; r>=1; r--) {
      for(c = 1; c <= 25; c++) {
        position = map[i] * 16;
        $('#r'+r+'c'+c).css('background-position',"-"+position+"px 0px");
        i = i + 1;
      }
    }
  }
}

$(document).ready( function() {
    $("#map").append('<table></table>');
    get_data();
    for (r=1; r<=25; r++) {
      $("#map table").append('<tr id="r'+r+'"></tr>');
      for(c = 1; c <= 25; c++) {
        $("#r"+r).append('<td id="r'+r+'c'+c+'"></td>');
      }
    }
    $('#cooldown').change(function() {
      $('#action_bar .filling').css('left',$('#cooldown').html()*2);
    });
 //   update_queue();
});
