<?php

$handle = fopen("/home/stee/Downloads/bilat_surg_1.csv", "r");
$bilat_array = array();
while ($line = fgetcsv($handle, null, "\t")) {
    $cpt = $line[0];
    $bilat_array[$cpt] = $cpt;
}

//var_dump($bilat_array);
ksort($bilat_array);
//var_dump($bilat_array);

$file = fopen("/home/stee/Downloads/wsid", "r");
$procs = array();
while (!feof($file)) {
    $line = fgets($file);

    $fee = (int) substr($line, 40, 10);
    if (empty($fee)) {
        continue;
    }

    $cdm = substr($line, 0, 4);
    $cpt = substr($line, 4, 5);
    if (!array_key_exists($cpt, $bilat_array)) {
        //echo "$cpt not found \n";
        continue;
    }

    $mod = substr($line, 9, 2);
    $desc = str_replace(",", " ", substr($line, 12, 28));

    $procs[$cdm] = $cdm;
}



ksort($procs);
//var_dump($procs);
//echo "\n" . count($procs) . " number of procs";
//exit;

//$fileout = fopen('2023_ratio', 'w');
foreach ($procs as $key => $item) {
    //$string = $key . "," . $value['mod'] . "," . $value['desc'] . "," . $value['fee'] . "," .
    //    $value['allow'] . "," . $value['ratio'] . "\n";
    //fwrite($fileout, $string);
    echo $item . "\n";
}

exit;
