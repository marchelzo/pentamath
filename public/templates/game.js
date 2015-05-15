'use strict';

function messageHandler(msg) {
  switch (msg.type) {

  case 'globalChatMessage':
    $('#messages').append($(
      '<li/>',
      {
        content: '[Global] ' + msg.from + ': ' + msg.message
      }
    ));
    break;

  case 'globalServerMessage':
    $('#messages').append($(
      '<li/>',
      {
        content: '[Server] ' + msg.from + ': ' + msg.message
      }
    ));
    break;

  case 'newQuestion':
    $('#question').html(msg.message);
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    break;

  }
}

function getCookie(name) {
  var value = "; " + document.cookie;
  var parts = value.split("; " + name + "=");
  if (parts.length == 2) return parts.pop().split(";").shift();
}

var connection = new WebSocket('ws://' + GameConfig.host + ':' + GameConfig.port);

connection.onopen = function () {
  connection.send(getCookie('pentamath-uid'));
}

connection.onmessage = function (msg) {
  try {
    handleMessage(JSON.parse(msg.data));
  } catch (e) {
    // Invalid JSON
  }
};

// message sending event listeners
var message = $('#message')

message.on('keydown', function (e) {
  if (e.which !== 13) return;
  if (e.shiftKey) { // global
    connection.send('globalChatMessage');
    connection.send(message.val());
  } else { // room
    connection.send(GameConfig.inRoom ? 'roomChatMessage' : 'globalChatMessage');
    connection.send(message.val());
  }
});
