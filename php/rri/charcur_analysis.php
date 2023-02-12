<?php

// wste.csv is list of pt-facing cpts
$myfile = fopen("wste.csv", "r") or die("Unable to open file!");
// Output one line until end-of-file

while($line = fgets($myfile)) {
  $pt_facing_codes_array[] = substr($line, 0, 5);
}

// wsid = unload of charcur
$rri_charges = fopen("/home/stee/Downloads/wsid", "r") or die("Unable to open file!");

$prev_cpt = '';
while ($line = fgets($rri_charges)){
    $cpt = substr($line, 37, 5);
    $dos = substr($line, 83, 8);
    $doc = substr($line, 59, 2);
    $gar = substr($line, 0, 8);
    $pos = substr($line, 81, 1);
    $ref = substr($line, 56, 3);
    //echo "cpt: " . $cpt . " dos:" . $dos . "\n";
    if ($cpt == $prev_cpt) {
        continue;
    }

    if ($dos < "20220101" || $dos > "20221231") {
        continue;
    }

    if (in_array($cpt, $pt_facing_codes_array)) {
        //if ($doc == "08") {
            //if ($ref == "S1X") {
                echo $gar. " " . $cpt . " " . $dos . " " . $doc . " " . $pos . " " . $ref . "\n";
            //}
        //}
        //$pt_facing[$gar] =  $cpt . " " . $dos . " " . $doc;
    }
    $prev_cpt = $cpt;
}
//var_dump($pt_facing);
fclose($myfile);
fclose($rri_charges);