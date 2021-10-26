<?php
$options = getopt("f:");

if ($file = fopen($options['f'], "r")) {
  while (($line = fgets($file)) !== false) {
    $line = str_replace("\n", '', $line);
    echo str_pad($line, 360) . "\n";
  }
}

