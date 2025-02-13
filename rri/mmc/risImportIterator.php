<?php

require __DIR__ . '/../../vendor/autoload.php';

use PhpOffice\PhpSpreadsheet\IOFactory;
use PhpOffice\PhpSpreadsheet\Reader\IReadFilter;
class FirstRowFilter implements IReadFilter
{
    public function readCell(string $columnAddress, int $row, string $worksheetName = ''): bool {
        //  Return true for rows after first row
        //echo "row is $row" . "\n";
        if ($row > 1) {
            return true;
        } else {
            return false;
        }
    }
}
$fromDate = $argv[1];
$toDate = $argv[2];
$chcrrLoad = $argv[3];

$rrmcInputFileName = '/tmp/outreads.xlsx';
$rrmcInputFileType = 'Xlsx';
$rrmc_fp = fopen('/tmp/ris.csv', 'w');
$rrmc_reader = IOFactory::createReader($rrmcInputFileType);
$rrmc_reader->setReadEmptyCells(false);
//$rrmc_reader->setIgnoreRowsWithNoCells(true);
$filterRow = new FirstRowFilter();
$rrmc_reader->setReadFilter($filterRow);
$rrmc_spreadsheet = $rrmc_reader->load($rrmcInputFileName);
$rrmc_worksheet = $rrmc_spreadsheet->getActiveSheet();

foreach ($rrmc_worksheet->getRowIterator() as $row) {
    $cellIterator = $row->getCellIterator();
    foreach ($cellIterator as $key => $cell) {
        $cellValue = $cell->getFormattedValue();
        //var_dump($key);
        //var_dump($cellValue);
        switch ($key) {
            case 'C':
                $rrmcPtLName = $cellValue;
                break;
            case 'D':
                $rrmcPtFName = $cellValue;
                break;
            case 'E':
                $rrmcAccessionNumber = $cellValue;
                break;
            case 'F':
                $dateTimeString = $cellValue;
                $rrmcPtDos = (new DateTimeImmutable($dateTimeString))->format('Ymd');
                echo "rrmcptdos is $rrmcPtDos" . "\n";
                //var_dump($rrmcPtDos);
                //exit;
                $rrmcCompareDos = $rrmcPtDos;  
                break;             
            case 'G':
                $rrmcPtCpt = substr(trim($cellValue), -5, 5);
                break;
            case 'H':
                $rrmcRefProv = $cellValue;
                break;
            case 'I':
                $outreadLocation = $cellValue;
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
                break;
            case 'J':
                $outreadText = $cellValue;
                break;
            case 'K':
                $renderingProvider = $cellValue;
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
                break;
            
            
        }
        

        
    }
    if (!empty($chcrrLoad)) {
        if ($oPos == 'N') {
            continue;
        }
    } else {
        if ($oPos != 'N') {
            continue;
        }
    }






    if ($rrmcCompareDos < $fromDate || $rrmcCompareDos > $toDate) {
        echo "skipping dos $rrmc_compare_dos \n";
        echo "fromDate " . $fromDate . " toDate " . $toDate . " rrmcPtDos " . $rrmcPtDos . " rrmc_compare_dos " . $rrmc_compare_dos . "\n";
        continue;
    }
    $firstFiveRisPtLastName = str_pad(substr($rrmcPtLName, 0, 5), 5);
    $risRrmcKey = $firstFiveRisPtLastName . $rrmcCompareDos . $rrmcPtCpt;
    $fields_rrmc[$risRrmcKey] = array($rrmcPtLName, $rrmcPtFName, $rrmcCompareDos, $rrmcPtCpt, $oPos, $rPCode);

}


foreach ($fields_rrmc as $rrmc_item) {
    fputcsv($rrmc_fp, $rrmc_item);
}


