<?php

require __DIR__ . '/../../vendor/autoload.php';

use League\Csv\Reader;

$reader = Reader::createFromPath($argv[1], 'r');
$reader->setHeaderOffset(0);
$header = $reader->getHeader();
//var_dump($header);
//exit;
$i = 1;
foreach ($header as $key => $value) {
    if (empty($value)) {
        $value = (string) $i++;
    }
    $new_header[$key] = trim($value);
}

//var_dump($new_header);

$records = $reader->getRecords($new_header);
foreach ($records as $offset => $record) {
    //$offset : represents the record offset
    //var_export($record); // returns something like
    //var_dump($offset);
    //var_export($record);
    //exit;
    //if (in_array($record['CDM'] ?? '', $record)) {
    //    echo $record['CDM'] ?? '' . "\n";
    //}
    //echo implode('', array_reverse(explode('/', $record['Date']))) . "\n";
    $cdm = $record['CDM'] ?? null;
    $cpt = $record['CPT'] ?? null ?: $record['HCPCS'] ?? null;
    $dos = $record['DOS'] ?? null;
    $mrn = $record['MRN'] ?? null;
    if ($record['HCPCS'] ?? '' == "G1004") {
        $cpt = "G1004";
    }
    echo $cdm . "," . $cpt . "," . $dos . "," . $mrn . "\n";
}
