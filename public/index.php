<?php

error_reporting(E_ALL);

require('../vendor/autoload.php');

map('GET', '/', function () {
  redirect('static/index.html');
});

map('GET', '/signup', function () {
  echo 'sign up today';
});

dispatch();

?>
