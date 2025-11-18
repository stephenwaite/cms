<?php

require __DIR__ . '/../vendor/autoload.php';

use PhpOffice\PhpSpreadsheet\IOFactory;
use PhpOffice\PhpSpreadsheet\Reader\IReadFilter;
use PhpOffice\PhpSpreadsheet\Reader\IReader;

$inputFileName = '/home/stee/Downloads/eis-pt-data.xlsx';
$inputFileType = 'Xlsx';

$spreadsheet = IOFactory::load($inputFileName, IReader::READ_DATA_ONLY);

$worksheet = $spreadsheet->getSheet(0);

$rowGenerator = $sheet->rangeToArrayYieldRows(
    $spreadsheet->getActiveSheet()->calculateWorksheetDataDimension(),
    null,
    false,
    false
);

foreach ($rowGenerator as $row) {
    echo '| ' . $row[0] . ' | ' . $row[1] . "|\n";
}



