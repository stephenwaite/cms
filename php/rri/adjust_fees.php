<?php

$handle = fopen("/home/stee/Downloads/fee2024", "r");
$cmsCptArray = array();
while ($line = fgetcsv($handle)) {
    if ($line[0] == 'C') {
        continue;
    }
    $code = substr($line[1], 0, 5) ?? '';

    $mod = $line[2] ?? '';
    if ($mod == 'TC') {
        continue;
    }
    $cmsCptArray[$code]['mod'] = $mod;
    $cmsCptArray[$code]['allow'] = floatval($line[3]) ?? 0;
}

$file = fopen("/home/stee/Downloads/wsid", "r");
$fees = array();
while (!feof($file)) {
    $line = fgets($file);
    $cdm = substr($line, 0, 4) ?? '';
    $code = substr($line, 4, 5) ?? '';
    $mod = substr($line, 9, 2) ?? '';
    $type = substr($line, 11, 1) ?? '';
    $desc = str_replace(",", " ", substr($line ?? '', 12, 28));
    $fee = floatval(substr($line, 40, 10) ?? 0);

    if (!empty($fee) && !empty($code)) {
        $fees[$code]['mod'] = $mod;
        $fees[$code]['desc'] = $desc;
        $fees[$code]['fee'] = $fee / 100;
    }
}

ksort($fees);

foreach ($fees as $key => $value) {
    if (!empty($cmsCptArray[$key])) {
        //echo "the allowable for $key is " . $cmsCptArray[$key]['allow'] . "\n";
        if (!empty($value['fee'])) {
            //echo "the fee for $key is " . $value['fee'] . "\n";
            $ratio = round($value['fee'] / $cmsCptArray[$key]['allow'], 2);
            //echo "the ratio for $key is " . $ratio . "\n";
            if ($ratio > 10) {
                $fee = round(10 * $cmsCptArray[$key]['allow']);
                $fees[$key]['fee'] = $fee;
                $format = "%s ratio %5.2f > 10, rad fee $%4d cms allows $%7.2f new fee is $%4d";
                echo sprintf($format, $key, $ratio, $value['fee'], $cmsCptArray[$key]['allow'], $fee) . "\n";
            }

            if ($ratio < 5) {
                $fee = round(5 * $cmsCptArray[$key]['allow']);
                $fees[$key]['fee'] = $fee;
                $format = "%s ratio %5.2f <  5, rad fee $%4d cms allows $%7.2f new fee is $%4d";
                echo sprintf($format, $key, $ratio, $value['fee'], $cmsCptArray[$key]['allow'], $fee) . "\n";
            }
        }
    }
}

$fileout = fopen('2025_fees', 'w');
foreach ($fees as $key => $value) {
    $string = $key . "," . ($value['mod'] ?? '') . "," . $value['desc'] . "," . round($value['fee'] * 1.03) . "," .
        ($value['allow'] ?? '') . "," . ($value['ratio'] ?? '') . "\n";
    fwrite($fileout, $string);
}

exit;
