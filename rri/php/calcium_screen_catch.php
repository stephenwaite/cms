<?php

/*
calcium_screen_catch
*/

// filepas002
$lines = file($argv[1]);
$garno = '';
foreach($lines as $line) {

    if (substr($line, 37, 5) == "75571") {
        $garno = substr($line, 0, 8);
        echo "$garno 75571 might need GY mod";
    }
}
