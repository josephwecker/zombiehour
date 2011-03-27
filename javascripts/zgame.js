
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
}

function post(param, value) {
  $.ajax({  
    type: "POST",  
    url: "data",  
    data: param + '=' + value
  });
}

function recieve_data() {
  data = $('#data_stream').contents().find('#data').html();
  if( data != null ) {
    $('#result').append( data );
  }
  $('#data_stream').attr('src', 'data?'+(Math.random()*1000000));
}
