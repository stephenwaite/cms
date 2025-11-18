<?php

/**
 *non patient facing analysis of rri procfile
 */

// output file from charcur_analysis.php
$trent = fopen("wsid2", "r") or die("unable to open file $trent");

while ($line = fgets($trent)) {
    $garno = substr($line, 0, 8);
    $trent_arr[$garno][] = $line;
}

// unload of garfile
$garfile = fopen("/home/stee/Downloads/wgar", "r") or die("unable to open file");

while ($line = fgets($garfile)) {
    $garno = substr($line, 0, 8);
    $insco = substr($line, 133, 3);
    if (array_key_exists($garno, $trent_arr)) {
        //if($insco == "003") {
        foreach ($trent_arr[$garno] as $key => $value) {
            $dos = substr($value, 15, 8);
            echo $dos . " " . $insco . "\n";
            $trent_03_arr[$garno . $dos] = $trent_arr[$garno];
        }
        //}
    }
}
//var_dump($trent_03_arr);
echo count($trent_03_arr) . "\n";
exit;
