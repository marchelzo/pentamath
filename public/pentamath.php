<?php

error_reporting(E_ALL);

require('../vendor/autoload.php');

echo 'yes';

map('GET', '/', function () {
  require('static/index.html');
});

map('GET', '/signup', function () {
  echo 'sign up today';
});

dispatch();

?>
