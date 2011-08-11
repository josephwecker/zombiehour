
var keyPresses = [];
var command = null;
var timing = true;
//var queue_times = [];

tileMap = new Array();
tileMap['unknown']        = "   0px 0px";
tileMap['blank']          = " -16px 0px";
tileMap['wall']           = " -32px 0px";
tileMap['closedDoor']     = " -48px 0px";
tileMap['repairedDoor']   = " -64px 0px";
tileMap['openDoor']       = " -80px 0px";
tileMap['brokenDoor']     = " -96px 0px";
tileMap['closedWindow']   = "-112px 0px";
tileMap['repairedWindow'] = "-128px 0px";
tileMap['openWindow']     = "-144px 0px";
tileMap['brokenWindow']   = "-160px 0px";
tileMap['obstacle']       = "-176px 0px";


// hotkey assignments:
$(document).bind("keydown", "return", function(e){ open_chat(); });
$(document).bind("keydown", "c", function(e){ post("cancel","cancel"); });
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
    "keydown", "space", function (evt) {
    pressKey("space");
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
$(document).bind(
    "keydown", "i", function (evt) {
    command = "repair";
    });
$(document).bind(
    "keydown", "p", function (evt) {
    command = "dress_wound";
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
  } else if( key1 == "space") {
    direction = "";
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
      show_anims(data.anims);
      //update_flash(data.flash);
      update_data(data.data);

      var t = new Date();
      update_flash(t.getTime() - start_time);

    }
  );
};

function update_data(data) {
  if( data != "nil" ) {
    //datas = data.split(";");
    //for( d in datas ) {
    for( d in data ) {
      update_stat(d, data[d]);
    }
  }
}

function show_anims(anim) {
  if( anim != "nil" ) {
    for( a in anim ) {
  //    shift_tiles(a);
  //    shift_tiles2(anim[a]);
    }
  }
}

function shift_tiles(direction, speed) {
  var obj = $("#map")
  var t = "-16px";
  var l = "-16px";
  switch( direction ) {
    case "walk_northwest":
      t = "-32px";
      l = "-32px";
      break;
    case "walk_north":
      t = "-32px";
      break;
    case "walk_northeast":
      t = "-32px";
      l = "0px";
      break;
    case "walk_west":
      l = "-32px";
      break;
    case "walk_east":
      l = "0px";
      break;
    case "walk_southwest":
      t = "0px";
      l = "-32px";
      break;
    case "walk_south":
      t = "0px";
      break;
    case "walk_southeast":
      t = "0px";
      l = "0px";
      break;
    default:
      break;
  }
  obj.css('left', l);
  obj.css('top',  t);
	obj.animate({left:"-16px",top:"-16px"}, speed*100, "linear");
}

function append_log(msg) {
  if( msg != "nil" ) {
    $('#result').append( msg );
    $('#result').animate({scrollTop: $('#result')[0].scrollHeight});
  }
}

function update_flash( msg ) {
  flash = $('#flash'); 
    if( flash.children().length >= 6 ){
      flash.children().first().remove();
    }
  $('#flash').append( '<p>'+msg+'</p>' );
}

function build_map() {
  var i = 0;
  var table = new Array();
  var table2 = new Array();
  table[i] = table2[i] = '<table>';
  for (y = 26; y > 0; y--) {
    table[i++] = table2[i++] = '<tr>';
    for (x = 0; x <= 25; x++) {
      table[i++] = '<td id="'+x+'n'+y+'"></td>';
      table2[i++] = '<td id="'+x+'n'+y+'shadow"></td>';
    }
    table[i++] = table2[i++] = '</tr>';
  }
  table[i++] = table2[i++] = '</table>';
  $("#map").html(table.join(''));
  $("#shadow").html(table2.join(''));
}

function update_map(mapdata) {
  if( mapdata != "nil" ) {
    for (tile in mapdata.changes) {
      newClass = mapdata.changes[tile].split('_');
      if ( newClass[1] == "clear" ) {
        $('#'+tile+'shadow').css('opacity','0');
      } else {
        $('#'+tile+'shadow').css('opacity','1');
      }
      $("#"+tile).css('background-position',tileMap[newClass[0]]);
      //table[tablePos] = '<td class="'+newClass[0]+'"></td>';
    }
      //alert(coords+", "+tablePos+", "+table[tablePos]);
    if (mapdata.moved != "false" ) {
      shift_tiles("walk_" + mapdata.moved, mapdata.speed);
    }
    //alert(table.join(''));
      //var d = new Date();
      //start_time = d.getTime();
      //var t = new Date();
      //alert(t.getTime() - start_time);
  }
}

$(document).ready( function() {
    build_map();
    get_data();
    cooldown = $('#cooldown');
    cooldown.change(function() {
      filling = $('#action_bar .filling');
      filling.css('left',cooldown.html()*2);
      filling.animate({left:'0px'}, cooldown.html()*100, 'linear');
    });
 //   update_queue();
});

function add_to_right() {
  map = $("#map");
  map.css("left", ((parseInt(map.css("left")) + 16) + "px"));
  x = parseInt( $("#map tr:first td:last").attr("id").slice(1) ) + 1;
  $("#map tr").each( function() {
    y = $(this).attr("id").slice(1);
    $(this).append('<td id="X'+x+'Y'+y+'"></td>');
    $(this).children().first().remove();
  });
}
function add_to_left() {
  map = $("#map");
  map.css("left", ((parseInt(map.css("left")) - 16) + "px"));
  x = parseInt( $("#map tr:first td:first").attr("id").slice(1) ) - 1;
  $("#map tr").each( function() {
    y = $(this).attr("id").slice(1);
    $(this).prepend('<td id="X'+x+'Y'+y+'"></td>');
    $(this).children().last().remove();
  });
}
function add_to_top() {
  map = $("#map");
  map.css("top", ((parseInt(map.css("top")) - 16) + "px"));
  x1 = parseInt( $("#map tr:first td:first").attr("id").slice(1) );
  x2 = parseInt( $("#map tr:first td:last").attr("id").slice(1) );
  y  = parseInt( $("#map tr:first").attr("id").slice(1) ) +1;
  $("#map").prepend('<tr></tr>');
  for ( x = x1; x <= x2; x++ ) {
    $("#map tr:first").append('<td id="X'+x+'Y'+y+'"></td>');
  }
	$("#map tr:last").remove();
}
function add_to_bottom() {
  map = $("#map");
  map.css("top", ((parseInt(map.css("top")) + 16) + "px"));
  x1 = parseInt( $("#map tr:first td:first").attr("id").slice(1) );
  x2 = parseInt( $("#map tr:first td:last").attr("id").slice(1) );
  y  = parseInt( $("#map tr:last").attr("id").slice(1) ) -1;
  $("#map").append('<tr></tr>');
  for ( x = x1; x <= x2; x++ ) {
    $("#map tr:last").append('<td id="X'+x+'Y'+y+'"></td>');
  }
	$("#map tr:first").remove();
}

function go_up(obj) {
	return (parseInt(obj.css("top")) - 16) + "px";
}

function go_down(obj) {
	return (parseInt(obj.css("top")) + 16) + "px";
}

function go_left(obj) {
	return (parseInt(obj.css("left")) - 16) + "px";
}

function go_right(obj) {
	return (parseInt(obj.css("left")) + 16) + "px";
}
