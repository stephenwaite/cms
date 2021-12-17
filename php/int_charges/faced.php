<?php

require __DIR__ . '/../../vendor/autoload.php';

use League\Csv\Reader;

$reader = Reader::createFromPath('/tmp/test.csv', 'r');
$reader->setHeaderOffset(0);

$records = $reader->getRecords();
foreach ($records as $offset => $record) {
    //$offset : represents the record offset
    //var_export($record); // returns something like
    //var_dump($record);
    //if (in_array($record['Date'], $record)) {
    //echo $record['Date'] . "\n";
    //echo implode('', array_reverse(explode('/', $record['Date']))) . "\n";    
    $cdm = $record['CDM'];
    $cpt = $record['CPT'] ?: $record['HCPCS'];
    $dos = $record['DOS'];
    $mrn = $record['MRN'];
    if ($record['HCPCS'] == "G1004") {
      $cpt = "G1004";
    }
    echo $cdm . "," . $cpt . "," . $dos . "," . $mrn . "\n";
}


