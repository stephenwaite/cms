<?php

require __DIR__ . '/vendor/autoload.php';

use League\Csv\Reader;
use League\Csv\Writer;

$csv = Reader::createFromPath('/tmp/test.csv', 'r');
$csv->setHeaderOffset(0);
$header = $csv->getHeader(); //returns the CSV header record

$csv_write = Writer::createFromString();


$records = $csv->getRecords();
foreach ($records as $offset => $record) {
    $record['First Name'] = strtoupper($record['First Name']);
    $record['Last Name'] = strtoupper($record['Last Name']);
    $csv_write->insertOne($record);
}

echo $csv_write->toString();





