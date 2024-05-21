<?php

//echo $argv[1];

$fh = fopen($argv[1], "r");

while (($line = fgets($fh)) !== false) {
  echo substr($line, 0, -2) . "\n";
}