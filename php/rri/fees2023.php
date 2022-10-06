<?php

//$array = explode(PHP_EOL, file_get_contents('w1'));
$cms_cpt_file = fopen("/home/stee/Downloads/fee2022", "r");

$cms_cpt_array = array();
while (!feof($cms_cpt_file)) {
    $line = fgets($cms_cpt_file);
    if (substr($line, 0, 1) != '#') {
        $cpt = substr($line, 1, 5);
    } else {
        $cpt = substr($line, 2, 5);
    }
    $cms_cpt_array[$cpt] = $line;
}

$file = fopen("/home/stee/Downloads/w1", "r");
$fees = array();
while (!feof($file)) {
    $line = fgets($file);
    $cpt = substr($line, 0, 5);
    if (!array_key_exists($cpt, $cms_cpt_array)) {
        //echo "$cpt not found \n";
        continue;
    }
    $mod = substr($line, 5, 2);
    $desc = substr($line, 12, 28);
    $fee = substr($line, 41, 10);
    if (array_key_exists($cpt, $fees)) {
//      echo "we have another $cpt \n";
      if ($fees[$cpt]['fee'] != $fee) {
        //echo "uh oh, fees not equal for $cpt " .
        //  $fees[$cpt]['fee'] . " $fee \n";
      }

    }
    $fees[$cpt] = array(
        'mod' => $mod,
        'desc' => $desc,
        'fee' => $fee,
    );


//    if (empty($fees[$cpt]['fee'])) {
//      $fees[$cpt]['fee'] = $fee;
//      //echo $cpt . " " . $fee . "\n";
//    } elseif ($fees[$cpt]['fee'] != $fee) {
//      echo "uh oh, fees not equal for $cpt \n";
//    }
//    if ($cpt == '77003') {
//      var_dump($fees[$cpt]);
//      echo $cpt . " " . $fee . "\n";
//    }
}

    //var_dump($fees);
    //echo count($fees);

    ksort($fees);
    foreach($fees as $key => $value) {
        echo $key . " " . 
          $value['mod'] . " " .
          $value['desc'] . " " .
          $value['fee'];
    }

exit;
