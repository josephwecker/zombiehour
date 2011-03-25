
// hotkey assignments:
$(document).bind("keydown", "return", toggle_chat);
//

function getUrlVars() {
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++) {
        hash = hashes[i].split('=');
        vars.push(hash[0]);
        vars[hash[0]] = hash[1];
    }
    return vars;
}

function toggle_chat() {
  var input = $('#chat_input');
  if (input.css('display') == 'none') {
    input.css('display', 'inline');
    input.trigger('focus');
  } else {
    input.css('display', 'none');
    if( input.val() != "" ) {
      chat_msg = "say " + input.val();
      $.ajax({  
        type: "POST",  
        url: "data",  
        data: 'input=' + chat_msg
      });  
      input.val("");
    }
  }
}

function recieve_data() {
  data = $('#data_stream').contents().find('#data').html();
  if( data != null ) {
    $('#result').append( data + '<br/>' );
  }
  $('#data_stream').attr('src', 'data?'+(Math.random()*1000000));
}
