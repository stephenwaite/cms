<?php

require __DIR__ . '/../../vendor/autoload.php';

use League\Csv\Reader;
use League\Csv\Writer;

$reader = Reader::createFromPath('/tmp/test.csv', 'r');
$reader->setHeaderOffset(0);

$csv = Writer::createFromString();

$records = $reader->getRecords();
foreach ($records as $offset => $record) {   
    var_dump($record);
    echo "," . "," . strtoupper($record["'First Name'"]) . "," . strtoupper($record["'Last Name'"]) . "," . "," . $record["'DOB'"] . "\n";    
}


