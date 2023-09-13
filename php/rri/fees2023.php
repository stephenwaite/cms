<?php

$handle = fopen("/home/stee/Downloads/fee2023", "r");
$cms_cpt_array = array();
while ($line = fgetcsv($handle)) {
    if ($line[0] == 'C') {
        continue;
    }
    $cpt = substr($line[1], 0, 5) ?? '';
    
    $mod = $line[2];
    if ($mod == 'TC') {
        continue;
    }
    $allow = $line[3];
    $cms_cpt_array[$cpt]['cpt'] = $cpt;
    $cms_cpt_array[$cpt]['mod'] = $mod;
    $cms_cpt_array[$cpt]['allow'] = $allow;
}

$file = fopen("/home/stee/Downloads/wsid", "r");
$fees = array();
while (!feof($file)) {
    $line = fgets($file);
    $cpt = substr($line, 4, 5);
    if (!array_key_exists($cpt, $cms_cpt_array)) {
        //echo "$cpt not found \n";
        continue;
    }

    $mod = substr($line, 9, 2);
    $desc = str_replace(",", " ", substr($line, 12, 28));
    $fee = (int) substr($line, 40, 10);
    if (empty($fee)) {
        continue;
    }
    //if (array_key_exists($cpt, $fees)) {
//      echo "we have another $cpt \n";
    //  if ($fees[$cpt]['fee'] != $fee) {
        //echo "uh oh, fees not equal for $cpt " .
        //  $fees[$cpt]['fee'] . " $fee \n";
    //  }

    //}
    $fees[$cpt]['cpt'] = $cpt;
    $fees[$cpt]['mod'] = $mod;
    $fees[$cpt]['desc'] = $desc;
    $fees[$cpt]['fee'] = $fee/100;
    $fees[$cpt]['allow'] = $cms_cpt_array[$cpt]['allow'];
    $fees[$cpt]['ratio'] = round($fees[$cpt]['fee'] / $cms_cpt_array[$cpt]['allow'], 2);


//    if (empty($fees[$cpt]['fee'])) {
//      $fees[$cpt]['fee'] = $fee;
//      //echo $cpt . " " . $fee . "\n";
//    } elseif ($fees[$cpt]['fee'] != $fee) {
//      echo "uh oh, fees not equal for $cpt \n";
//    }
}

//var_dump($fees);
//exit;
//echo count($fees);

ksort($fees);

$fileout = fopen('2023_ratio', 'w');
foreach($fees as $key => $value) {
    $string = $key . "," . $value['mod'] . "," . $value['desc'] . "," . $value['fee'] . "," . 
        $value['allow'] . "," . $value['ratio'] . "\n";
    fwrite($fileout, $string);
}

exit;
