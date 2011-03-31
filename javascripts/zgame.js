
// hotkey assignments:
$(document).bind("keydown", "return", function(e){ open_chat(); });
$(document).bind(
    "keydown", "w", function (evt) {
    post("walk", "north");
    });
$(document).bind(
    "keydown", "a", function (evt) {
    post("walk", "west");
    });
$(document).bind(
    "keydown", "s", function (evt) {
    post("walk", "south");
    });
$(document).bind(
    "keydown", "d", function (evt) {
    post("walk", "east");
    });
//

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
      get_data();
      append_log(data.msg);
      update_flash(data.flash);
      update_map(data.map);
    }
  );
};

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
  var map = mapdata.split(",");
   // alert(map);
  var i = 0
  for (r=25; r>=1; r--) {
    for(c = 1; c <= 25; c++) {
      maptile = map[i];
      clientTile = $('#r'+r+'c'+c);
      clientTile.removeClass();
      if( maptile == "0" ) {
        clientTile.addClass('empty');
      } else if( maptile == "1" ) {
        clientTile.addClass('space');
      } else if( maptile == "2" ) {
        clientTile.addClass('wall');
      } else if( maptile == "3" ) {
        clientTile.addClass('rspace');
      } else if( maptile == "4" ) {
        clientTile.addClass('rwall');
      } else if( maptile == "5" ) {
        clientTile.addClass('player');
      }
      i = i + 1;
    }
  }
}
  


$(document).ready( function() {
    get_data();
    for (r=1; r<=25; r++) {
      for(c = 1; c <= 25; c++) {
        $("#map").append('<img src="/images/transparent.png" id="r'+r+'c'+c+'"/>');
      }
      $("#map").append('<br />');
    }
});
