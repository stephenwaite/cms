<?php
if ($argc < 2) {
    fwrite(STDERR, "Usage: php {$argv[0]} <inputfile>\n");
    exit(1);
}

$in = fopen($argv[1], 'r');
if ($in === false) {
    fwrite(STDERR, "Cannot open {$argv[1]}\n");
    exit(1);
}
$out = fopen('output.csv', 'w');

while (($line = fgets($in)) !== false) {
    fwrite($out, strtoupper($line));
}

fclose($in);
fclose($out);