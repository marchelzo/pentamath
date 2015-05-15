'use strict';

// use a monospaced font for chat messages
$('#messages').css('font-family', 'monospace');

function handleMessage(msg) {
  switch (msg.type) {

  case 'globalChatMessage':
    $('#messages').append($(
      '<li/>',
      {
        text: '[Global] ' + msg.from + ': ' + msg.message
      }
    ));
    if ($('#seperator')[0])
      $('#seperator')[0].scrollTop = $('#seperator')[0].scrollHeight;
    break;

  case 'roomChatMessage':
    $('#messages').append($(
      '<li/>',
      {
        text: '[Room]   ' + msg.from + ': ' + msg.message
      }
    ));
    if ($('#seperator')[0])
      $('#seperator')[0].scrollTop = $('#seperator')[0].scrollHeight;
    break;

  case 'globalServerMessage':
    $('#messages').append($(
      '<li/>',
      {
        text: '[Server] ' + msg.message
      }
    ));
    if ($('#seperator')[0])
      $('#seperator')[0].scrollTop = $('#seperator')[0].scrollHeight;
    break;

  case 'newQuestion':
    $('#question').html(msg.message);
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    break;

  case 'practiceProblems':
    problemList = msg.problems;
    answerList = msg.answers;
    startPractice();
    break;

  case 'newRoom':
    $('#messages').append(
      '<li>[Server] <a href="' + '/room/' + msg.message + '">' + msg.message + ' has created a new room!</a></li>'
    );
    break;

  case 'error':
    switch (msg.message) {
    case 'noRoom':
      window.location.replace('/');
      break;
    case 'noToken':
      alert('no token');
      window.location.replace('/login');
      break;
    case 'serverError':
      alert('The server is having problems. Please try again in a few minutes');
      break;
    }
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
  if (GameConfig.onOpen) GameConfig.onOpen();
}

connection.onmessage = function (msg) {
  console.log(msg.data);
  try {
    handleMessage(JSON.parse(msg.data));
  } catch (e) {
    // Invalid JSON
    console.log(e.stack);
  }
  if (GameConfig.onMessage) GameConfig.onMessage(msg.data);
};

// message sending event listeners
var message = $('#message');

message.on('keydown', function (e) {
  if (e.which !== 13 || !message.val()) return;
  
  var escapedMessage = message.val().replace(/"/g, "\\\"");

  if (e.shiftKey) { // global
    connection.send('globalChatMessage');
    connection.send(escapedMessage);
  } else if (GameConfig.inRoom) {
    connection.send('roomChatMessage');
    connection.send(GameConfig.roomOwner);
    connection.send(escapedMessage);
  } else {
    connection.send('globalChatMessage');
    connection.send(escapedMessage);
  }

  // clear message box
  message.val('');
});

var answer = $('#answer');

answer.on('keydown', function (e) {
  if (e.which !== 13) return;
  handleAnswer(answer.val());
  answer.val('');
});
