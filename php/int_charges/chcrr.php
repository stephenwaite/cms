<?php

require __DIR__ . '/../../vendor/autoload.php';

use League\Csv\Reader;
use League\Csv\Writer;

// file missing charges
$reader_missing = Reader::createFromPath($argv[2], 'r');
$reader_missing->setHeaderOffset(0);
$header_missing = $reader_missing->getHeader();
//var_dump($header);
//exit;

// make an array of records from file that has missing records
// with the following global key
//global $chcrr_key;

$records_missing = $reader_missing->getRecords($header_missing);
foreach ($records_missing as $offset => $record) {
    $chcrr_key = $record['P Last Name'] .
        $record['P First Name'] .
        $record['P Date of Birth'] .
        $record['HA Service Date'] .
        $record['HA CPT_HCPCS Code'];

    $month_array[$chcrr_key] = $record;
    //}
}

// file with all october charges
$reader = Reader::createFromPath($argv[1], 'r');
$reader->setHeaderOffset(0);
$header = $reader->getHeader();

$records = $reader->getRecords($header);
//var_dump($records);
$csv = Writer::createFromPath('file.csv', 'w+');
$csv->insertOne($header);
foreach ($records as $offset => $record) {
    $chcrr_key = $record['P Last Name'] .
        $record['P First Name'] .
        $record['P Date of Birth'] .
        $record['HA Service Date'] .
        $record['HA CPT_HCPCS Code'];

    if (!array_key_exists($chcrr_key, $month_array)) {
        //var_dump($record);
        $csv->insertOne($record);
    }
}



