<?php
/**
 * Created by PhpStorm.
 * User: wiggins
 * Date: 2/16/20
 * Time: 4:17 PM
 */

$handle_dan = fopen("wdan", "r");
$handle_dat = fopen("wdata.csv", "r");
$handle_sid = fopen("wsid", "r");
$handle_ste = fopen("wste", "w");
$dan = array();
$sid_dob = array();
$sid_ins = array();
$sid_fname = array();
$sid_lname = array();
$sid_sex = array();
$dan_npi = "1164512067";
$csv_string = '';
$garno = array();

//$handle_ste = fopen("wste", "a");
//$sid_match = false;
//$dan['dob'] = array();

//for ($i=0, $line_dan = fgets($handle_dan)) !== false, $i++) {
while (($line_dat = fgets($handle_dat)) !== false) {
    $dan[] = explode(',', $line_dat);
}

while (($line_sid = fgets($handle_sid)) !== false) {
    $garno[]     = substr($line_sid, 0, 8);
    $sid_fname[] = trim(substr($line_sid, 51, 20));
    $sid_lname[] = trim(substr($line_sid, 71, 20));
    $sid_dob[] = substr($line_sid, 91, 10);
    $sid_sex[] = substr($line_sid, 111, 1);
    $sid_ins[] = substr($line_sid, 208, 3);
}

//var_dump($sid_dob);

//ksort($dan);
$i = 0;
foreach ($dan as $item) {
    /*var_dump($item);
    if ($i == 3) exit;*/
    $i++;
    //$dan_temp = str_replace('/', '', $item[4]);
    //$dan_dob[] = substr($dan_temp, 4,4) . substr($dan_temp, 0,2) . substr($dan_temp, 2, 2);
    $wsid_key = array_search($item[4], $sid_dob);
    if ($wsid_key) {
        //echo "the dob " . $item[4] . " is in wsid as " . $sid_dob[$wsid_key] . " with insurance code " . $sid_ins[$wsid_key] . "\n";
        //print_r(array_keys($sid_dob, $item[4]));
        $csv_string = $i . "," . $dan_npi . "," . $garno[$wsid_key] . "," . trim(strtoupper($item[3])) . "," .
            trim(strtoupper($item[2])) . "," .
            $sid_fname[$wsid_key] . "," . $sid_lname[$wsid_key] . "," . $sid_sex[$wsid_key];
        switch ($sid_ins[$wsid_key]) {
            case 001:
                fwrite($handle_ste, $csv_string . ",Patient is payer\n");
                break;
            case 002:
                fwrite($handle_ste, $csv_string . ",Private pay insurance\n");
                break;
            case 006:
                fwrite($handle_ste, $csv_string . ",Other Payer\n");
                break;
            case 102:
                fwrite($handle_ste, $csv_string . ",Private pay insurance\n");
                break;
            case 256:
                $medicare_age = substr($item[4], 6,4);
                echo "mvp and $medicare_age\n";
                if ($medicare_age > 1955) {
                    fwrite($handle_ste, $csv_string . ",Private pay insurance\n");
                } else {
                    fwrite($handle_ste, $csv_string . ",Medicare Part B\n");
                }
                break;
            case 268:
                $medicare_age = substr($item[4], 6,4);
                echo "oos and $medicare_age\n";
                if ($medicare_age > 1955) {
                    fwrite($handle_ste, $csv_string . ",Private pay insurance\n");
                } else {
                    fwrite($handle_ste, $csv_string . ",Medicare Part B\n");
                }
                break;
            case 279:
                fwrite($handle_ste, $csv_string . ",Private pay insurance\n");
                break;
            case 605:
                fwrite($handle_ste, $csv_string . ",Private pay insurance\n");
                break;
            default:
                fwrite($handle_ste, $csv_string . ",Medicare Part B\n");
                break;
        }
    } else {
        $csv_string = $i . "," . $dan_npi . "," . "no garno" . "," . trim(strtoupper($item[3])) . "," .
            trim(strtoupper($item[2])) . "," . "," . ",";
        fwrite($handle_ste, $csv_string . ",unknown\n");
    }

}

//$ste_dob = array_flip($dan_dob);
//$ste_dob[] = asort($dan_dob);



