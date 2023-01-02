<?php

/*
filepas002 helper
*/

// filepas002
$lines = file($argv[1]);
// hosprri
$source = file($argv[2]);

$count = 0;

foreach($lines as $line) {

    $count += 1;
    if (stripos($line, 'NO BCBSVT AUTH') !== false) {
        $pieces = explode(' ', trim($line));
        $last_word = array_pop($pieces);
        foreach($source as $row) {
            if (stripos($row, $last_word)) {
                echo "look for reason from rrmc on missing auth for " .
                substr($row, 9, 24) .
                substr($row, 617, 10) . "\n";
            } 
        }

    }
}
