<?php

require __DIR__ . '/../../vendor/autoload.php';

use PhpOffice\PhpSpreadsheet\IOFactory;

$fromDate = $argv[1];
$toDate = $argv[2];
$chcrrLoad = $argv[3];

$rrmcInputFileName = '/tmp/outreads.xlsx';
$rrmcInputFileType = 'Xlsx';
$rrmc_fp = fopen('/tmp/ris.csv', 'w');

$rrmc_reader = IOFactory::createReader($rrmcInputFileType);
$rrmc_spreadsheet = $rrmc_reader->load($rrmcInputFileName);
$rrmc_worksheet = $rrmc_spreadsheet->getActiveSheet();
$rrmc_rows = $rrmc_worksheet->toArray();

foreach ($rrmc_rows as $rrmc_key => $rrmc_value) {
    if ($rrmc_key == 0) {
        continue;
    }

    if (
        count(array_filter($rrmc_rows[$rrmc_key], function ($value) {
            return $value !== null;
        })) === 0
    ) {
        break;   // Ignore empty rows
    }

    //var_dump($rrmc_value);
    //exit;

    $outreadLocation = trim($rrmc_value[8]);

    switch ($outreadLocation) {
        case '34':
            $oPos = 'R';
            break;

        case '35':
            $oPos = 'C';
            break;

        case '36':
            $oPos = 'M';
            break;

        case '110':
            $oPos = 'N';
            break;

        default:
            $oPos = 'C';
            break;
    }

    //echo $outreadLocation . "\n";

    if (!empty($chcrrLoad)) {
        if ($oPos == 'N') {
            continue;
        }
    } else {
        if ($oPos != 'N') {
            continue;
        }
    }


    $renderingProvider = trim($rrmc_value[10]);
    switch ($renderingProvider) {
        case 'MITCHELL':
            $rPCode = '06';
            break;
        case 'SHELTON':
            $rPCode = '08';
            break;
        case 'HUMMEL':
            $rPCode = '09';
            break;
        case 'BOYER':
            $rPCode = '10';
            break;
    }


    $rrmcPtLName = $rrmc_value[2];
    $rrmcPtFName = $rrmc_value[3];
    $rrmcAccessionNumber = $rrmc_value[4];
    $dateTime = strtotime($rrmc_value[5]);
    //echo "Created date is " . date("Y-m-d h:i:sa", $dateTime);
    //echo "Created date is " . date("Ymd", $dateTime);
    //var_dump($dateTime);
    //exit;
    $rrmcPtDos = date("Ymd", $dateTime);
    //echo $rrmcPtDos . "\n";
    //exit;
    $rrmcCompareDos = $rrmcPtDos;
    if ($rrmcCompareDos < $fromDate || $rrmcCompareDos > $toDate) {
        //echo "skipping dos $rrmcCompareDos \n";
        //echo "fromDate " . $fromDate . " toDate " . $toDate . " rrmcPtDos " . $rrmcPtDos . " rrmc_compare_dos " . $rrmcCompareDos . "\n";
        continue;
    }
    $rrmcPtCpt = substr(trim($rrmc_value[6]), -5, 5);
    $firstFiveRisPtLastName = str_pad(substr($rrmcPtLName, 0, 5), 5);
    $risRrmcKey = $firstFiveRisPtLastName . $rrmcCompareDos . $rrmcPtCpt;
    $fields_rrmc[$risRrmcKey] = array($rrmcPtLName, $rrmcPtFName, $rrmcCompareDos, $rrmcPtCpt, $oPos, $rPCode);
}

foreach ($fields_rrmc as $rrmc_item) {
    fputcsv($rrmc_fp, $rrmc_item);
}
