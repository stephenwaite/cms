<?php
/**
 * Created by PhpStorm.
 * User: wiggins
 * Date: 12/4/20
 * Time: 4:17 PM
 */

$handle_apm = fopen("apm.csv", "r");
$handle_apl = fopen("ActiveProviderList.csv", "r");
$handle_ste = fopen("wste", "w");
$apm = array();
$apl = array();

$needle = '1174589097';
while (($line_apm = fgetcsv($handle_apm)) !== false) {
    //print_r($line_apm);
    array_push($apm, $line_apm);
}

//var_dump($apm);

while (($line_apl = fgetcsv($handle_apl)) !== false) {
    array_push($apl, $line_apl);
}    
    
if (array)
        //echo "apm npi is " . $line_apm[0] . " apl npi is " . $line_apl[1] . "\n";
       // if ($key = array_search($line_apl[1], array_column($line_apm, '0'))) {
       //     echo "key is $key \n";
       //     fwrite($handle_ste, $line_apl[1] . "\n");
       // }   




