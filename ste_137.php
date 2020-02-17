<?php
/**
 * Created by PhpStorm.
 * User: wiggins
 * Date: 2/16/20
 * Time: 4:17 PM
 */

$handle_dan = fopen("wdan", "r");
$handle_sid = fopen("wsid", "r");

//$handle_ste = fopen("wste", "a");
//$sid_match = false;
//$dan['dob'] = array();

//for ($i=0, $line_dan = fgets($handle_dan)) !== false, $i++) {
while (($line_dan = fgets($handle_dan)) !== false) {
    $dan[] = explode(',', $line_dan);
}

while (($line_sid = fgets($handle_sid)) !== false) {
    $sid_dob[] = substr($line_sid, 91, 10);
    $sid_ins[] = substr($line_sid, 208, 3);
}

//var_dump($sid_dob);

//ksort($dan);
foreach ($dan as $item) {
    //$dan_temp = str_replace('/', '', $item[4]);
    //$dan_dob[] = substr($dan_temp, 4,4) . substr($dan_temp, 0,2) . substr($dan_temp, 2, 2);
    $wsid_key = array_search($item[4], $sid_dob);
    if ($wsid_key) {
        echo "the dob " . $item[4] . " is in wsid as " . $sid_dob[$wsid_key] .
            " with insurance code " . $sid_ins[$wsid_key] . "\n";
    } else {
        echo "not in there \n";
    }
}

//$ste_dob = array_flip($dan_dob);
//$ste_dob[] = asort($dan_dob);



