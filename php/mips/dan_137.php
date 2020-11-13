<?php
/**
 * Created by PhpStorm.
 * User: wiggins
 * Date: 2/16/20
 * Time: 4:17 PM
 */

$handle_dan = fopen("wdan", "r");
$handle_ste = fopen("wste", "a");
$sid_match = false;

while (($line_dan = fgets($handle_dan)) !== false) {
    $handle_sid = fopen("wsid", "r");
    if ($sid_match) {
        switch ($sid_ins) {
            case 003:
                fwrite($handle_ste, "Medicare Part B \n");
                break;

            default:
                fwrite($handle_ste, "other ins \n");
                break;
        }
        echo "we've got a match" . "\n";
    } else {
        fwrite($handle_ste, "no match \n");
    }
    $dan = explode(',', $line_dan);
    $dan_dob = str_replace('/', '', $dan[4]);
    $dan_dos = str_replace('/', '', $dan[5]);
    echo " outer while loop dan's dob is " . $dan_dob . " and dan's dos is " . $dan_dos .  "\n";
    while (($line_sid = fgets($handle_sid)) !== false) {
        $sid_dob = str_replace('/', '', substr($line_sid, 91, 10));
        $sid_dos = str_replace('/', '', substr($line_sid, 131, 10));
        $sid_ins = substr($line_sid, 208, 3);
        //echo "sid's dob is " . $sid_dob . " and sid's dos is " . $sid_dos .  "\n";
        // if (($sid_dob == $dan_dob) && ($sid_dos == $dan_dos)) {
        if ($sid_dob == $dan_dob) {
            echo "here's a match sid's ins is " . $sid_ins . " sid's dob is " .
                $sid_dob . " and dan's dob is " . $dan_dob .
                " sid's dos is " . $sid_dos .
                " dan's dos is " . $dan_dos . "\n";
            $sid_match = true;
            break;
        } else {
            echo "no match - dan's dob is " . $dan_dob; // . " and dan's dos is " . $dan_dos .  "\n";
            echo "sid's dob is " . $sid_dob . "\n"; // . " and sid's dos is " . $sid_dos .
            $sid_match = false;
        }
    }
}