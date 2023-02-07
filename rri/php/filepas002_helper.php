<?php

/*
filepas002 helper
*/

$lines = file('/home/stee/Downloads/filepas002');
$source = file('/home/stee/Downloads/hosprri');

$count = 0;

foreach($lines as $line) {

    $count += 1;
    if (stripos($line, 'NO BCBSVT AUTH') !== false) {
        $pieces = explode(' ', trim($line));
        $last_word = array_pop($pieces);
        foreach($source as $row) {
            if (stripos($row, $last_word)) {
                echo $row . "\n";
            } 
        }

    }
}
