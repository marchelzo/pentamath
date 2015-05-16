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
    //if ($('#message-container')[0])
      //$('#message-container')[0].scrollTop = $('#message-container')[0].scrollHeight;
    break;

  case 'roomChatMessage':
    $('#messages').append($(
      '<li/>',
      {
        text: '[Room]   ' + msg.from + ': ' + msg.message
      }
    ));
    //if ($('#message-container')[0])
      //$('#message-container')[0].scrollTop = $('#message-container')[0].scrollHeight;
    break;

  case 'globalServerMessage':
    $('#messages').append($(
      '<li/>',
      {
        text: '[Server] ' + msg.message
      }
    ));
    //if ($('#message-container')[0])
      //$('#message-container')[0].scrollTop = $('#message-container')[0].scrollHeight;
    break;

  case 'newQuestion':

    questionNumber += 1;

    if (!roomStarted) {
      roomStarted = true;
      $('#time').css('display', 'block');
    }

    $('#question').html(msg.message);

    var offset = $('#c' + (questionNumber - 1)).position().left;
    $('#time').animate({
      left: offset
    });

    $('#time').TimeCircles().restart();
    $('#time').TimeCircles().start();
    $('#time').css('display', 'block');

    $('#answer').focus();

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

  case 'userJoinedRoom':
    $('#messages').append(
      '<li>[Room]   ' + msg.message + ' has joined the room!</li>'
    );
    addToScoreboard(msg.message, 0);
    break;

  case 'challengeBeginning':
    $('#messages').append(
      '<li>[Challenge] The 1v1 is beginning in 3 seconds!</li>'
    );
    break;

  case 'opponentQuestion':
    $('#their-question').html(msg.message);
    break;

  case 'newChallengeQuestion':
    $('#your-question').html(msg.message);
    if (challengeStarted === false) {
      challengeStarted = true;
      begin = +new Date();
      timerTimeoutID = window.setInterval(updateTimer, 50);
    }
    break;

  case 'opponentScore':
    opponentScore += 1;
    renderScores();
    break;

  case 'youScore':
    score += 1;
    renderScores();
    break;


  case 'userList':
    var users = msg.message;
    for (var k = 0; k < users.length; ++k) {
      addToScoreboard(users[k], 0);
    }
    break;

  case 'answer':
    checkAnswer(msg.message);
    break;

  case 'challengeRequest':
    $('#messages').append(
      '<li>[Server] <a href="/challenge/' + msg.message + '">' + msg.message + ' has challenged you to a 1v1!</a></li>'
    );
  break;

  case 'challengeAccepted':
    $('#messages').append(
      '<li>[Server] <a href="/challenge/' + msg.message + '">' + msg.message + ' has accepted your challenge!</a></li>'
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
    case 'noOpponent':
      $('#messages').append(
        '<li>[Server] That player is not currently online.</li>'
      );
      break;
    case 'serverError':
      alert('The server is having problems. Please try again in a few minutes');
      break;
    }
  break;


  case 'scoreboardUpdate':
    var scores = msg.message;
    for (var player in scores) {
      if (scores.hasOwnProperty(player)) {
        addToScoreboard(player, scores[player]);
      }
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
var connection2 = new WebSocket('ws://' + GameConfig.host + ':' + GameConfig.port);

connection.onopen = function () {
  connection.send(getCookie('pentamath-uid'));
  if (GameConfig.onOpen) GameConfig.onOpen();
}

connection2.onopen = function () {
  connection2.send(getCookie('pentamath-uid'));
}

connection.onmessage = function (msg) {
  console.log(msg.data);
  try {
    handleMessage(JSON.parse(msg.data));
  } catch (e) {
    // Invalid JSON
    console.log(e.stack);
    console.log('Invalid: ' + msg.data);
  }
  if (GameConfig.onMessage) GameConfig.onMessage(msg.data);
};

connection2.onmessage = function (msg) {
  console.log(msg.data);
  try {
    handleMessage(JSON.parse(msg.data));
  } catch (e) {
    // Invalid JSON
    console.log(e.stack);
    console.log('Invalid: ' + msg.data);
  }
  if (GameConfig.onMessage) GameConfig.onMessage(msg.data);
};

// message sending event listeners
var message = $('#message');

message.on('keypress', function (e) {
  if (e.which !== 13 || !message.val()) return;

  if (GameConfig.onSend) GameConfig.onSend(message.val());

  if (message.val() === '') return;
  
  var escapedMessage = message.val().replace(/"/g, "\\\"");

  if (e.shiftKey) { // global
    connection2.send('globalChatMessage');
    connection2.send(escapedMessage);
  } else if (GameConfig.inRoom) {
    connection2.send('roomChatMessage');
    connection2.send(GameConfig.roomOwner);
    connection2.send(escapedMessage);
  } else {
    connection2.send('globalChatMessage');
    connection2.send(escapedMessage);
  }

  // clear message box
  message.val('');
});

var answer = $('#answer');

answer.on('keypress', function (e) {
  if (e.which !== 13) return;
  handleAnswer(answer.val());
});
