<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once 'risImport.php';

use PhpOffice\PhpSpreadsheet\IOFactory;
use PhpOffice\PhpSpreadsheet\Reader\IReadFilter;
use PhpOffice\PhpSpreadsheet\Shared\Date;

$inputFileName = '/tmp/test.xlsx';
$inputFileType = 'Xlsx';
$fp = fopen('/tmp/charges2.csv', 'w');

class MyReadFilter implements IReadFilter {
    public function readCell($columnAddress, $row, $worksheetName=''): bool {
        if($row > 100000) {
            return false;
        }
        return true;
    }
}

$filterSubset=new MyReadFilter();
$reader = IOFactory::createReader($inputFileType);
$reader->setReadFilter($filterSubset);
$spreadsheet = $reader->load($inputFileName);
foreach ($spreadsheet->getSheet(0)->getRowIterator() as $row) {
    if ($spreadsheet->getSheet(0)->getRowDimension($row->getRowIndex())->getVisible()) {

        if ($row->getRowIndex() == 0) {
            continue;
        }

        $key = $row->getRowIndex();

        $cpt_arr = explode(':', uC($spreadsheet->getSheet(0)->getCell('H' . $row->getRowIndex())));

        $cpt = $cpt_arr[0];
        if (strpos($cpt, ',')) {
            echo "there's a modifier " . $cpt . "\n";
        }
        $cpt = substr($cpt_arr[0], 0, 5);

        $dosValue = intval($spreadsheet->getSheet(0)->getCell('A' . $row->getRowIndex())->getValue());
        $dosDate = Date::excelToDateTimeObject($dosValue);
        $dos = $dosDate->format('m/d/y');
        $rrmcDos = $dosDate->format('Ymd');
        if ($rrmcDos < $fromDate || $rrmcDos > $toDate) {
            continue;
        }

        $rawPtLastName = ($spreadsheet->getSheet(0)->getCell('B' . $row->getRowIndex())->getValue());
        $ptLastName = uC($rawPtLastName); //lname
        // ignore suffixes
        $ptSuffixPos = strpos($rawPtLastName, ',');
        if (!empty($ptSuffixPos)) {
            $ptSuffix = substr($rawPtLastName, 0, $ptSuffixPos);
            $mmcPtLastName = preg_replace("/[^A-Z]/", '', $ptSuffix);
        } else {
            $mmcPtLastName = $ptLastName;
        }

        $ptFirstName = uC($spreadsheet->getSheet(0)->getCell('C' . $row->getRowIndex())->getValue());
        $risKey = str_pad(substr($mmcPtLastName, 0, 5), 5) . $rrmcDos . $cpt;
        $fields_mmc[$risKey] = array($mmcPtLastName, $ptFirstName, $cpt, $dos);

        $records[$risKey][0] = $ptLastName;
        $records[$risKey][1] = $ptFirstName;
        $addr1 = uC($spreadsheet->getSheet(0)->getCell('O' . $row->getRowIndex())->getValue()); // addr 1
        $records[$risKey][2] = $addr1;
        $addr2 = uC($spreadsheet->getSheet(0)->getCell('P' . $row->getRowIndex())->getValue()); // addr 2
        $records[$risKey][3] = $addr2;
        $city = uC($spreadsheet->getSheet(0)->getCell('Q' . $row->getRowIndex())->getValue()); // city
        $records[$risKey][4] = ($city == 'MANCHESTER CENTER') ? 'MANCHESTER CTR' : $city;
        $state = uC($spreadsheet->getSheet(0)->getCell('R' . $row->getRowIndex())->getValue()); // state
        $records[$risKey][5] = $state; // state
        $zip = uC($spreadsheet->getSheet(0)->getCell('S' . $row->getRowIndex())->getValue()); // zip
        $records[$risKey][6] = (strlen($zip) == 4) ? '0' . $zip : $zip;
        $dobValue = uC($spreadsheet->getSheet(0)->getCell('D' . $row->getRowIndex())->getValue()); // dob
        $dobDate = Date::excelToDateTimeObject($dobValue);
        $dob = $dobDate->format('m/d/Y');
        $records[$risKey][7] = $dob;
        $gender = uC($spreadsheet->getSheet(0)->getCell('E' . $row->getRowIndex())->getValue()); // gender
        $records[$risKey][8] = $gender;
        $primaryInsName = uC($spreadsheet->getSheet(0)->getCell('K' . $row->getRowIndex())->getValue()); // primary ins name
        $primaryPolicyValue = uC($spreadsheet->getSheet(0)->getCell('L' . $row->getRowIndex())->getValue()); // primary policy
        $records[$risKey][9] = chcrrInsCode($primaryInsName, $primaryPolicyValue); // chcrr ins code
        $records[$risKey][10] = ''; // chcrr ins name
        $records[$risKey][11] = ''; // chcrr ins addr
        $records[$risKey][12] = ''; // chcrr ins city
        $records[$risKey][13] = ''; // chcrr ins state
        $records[$risKey][14] = ''; // chcrr ins zip
        if (is_numeric($primaryPolicyValue)) {
            $records[$risKey][15] = $primaryPolicyValue;
        } else {
            $records[$risKey][15] = uC($primaryPolicyValue); // policy
        }
        $records[$risKey][16] = ''; // group number
        $records[$risKey][17] = $records[$risKey][0]; // hard code guarantor to be patient
        $records[$risKey][18] = $records[$risKey][1];
        $records[$risKey][19] = $records[$risKey][8];
        $records[$risKey][20] = 'Self';
        $secondaryInsName = uC($spreadsheet->getSheet(0)->getCell('M' . $row->getRowIndex())->getValue()); // secondary ins name
        $secondaryPolicyValue = uC($spreadsheet->getSheet(0)->getCell('N' . $row->getRowIndex())->getValue()); // secondary policy
        $records[$risKey][21] = chcrrInsCode($secondaryInsName, $secondaryPolicyValue);
        $records[$risKey][22] = '';
        $records[$risKey][23] = '';
        $records[$risKey][24] = '';
        $records[$risKey][25] = '';
        $records[$risKey][26] = '';
        $records[$risKey][27] = '';

        $records[$risKey][28] = $secondaryPolicyValue;
        $records[$risKey][29] = '';
        $records[$risKey][30] = '';

        $records[$risKey][31] = $gender;
        $records[$risKey][32] = 'S';
        $records[$risKey][33] = $cpt . "TC"; //chcrr sends TC mod
        $dx1Value = uC($spreadsheet->getSheet(0)->getCell('I' . $row->getRowIndex())->getValue()); // DX1
        $dx1 = formatDx($dx1Value);
        $records[$risKey][34] = $dx1;
        $dx2Value = uC($spreadsheet->getSheet(0)->getCell('J' . $row->getRowIndex())->getValue()); // DX2
        $dx2 = formatDx($dx2Value);
        $records[$risKey][35] = $dx2;
        $records[$risKey][36] = '';
        $records[$risKey][37] = '';
        $records[$risKey][38] = $dos;
        // skip rendering, use supervising for billing
        $npiValue = uC($spreadsheet->getSheet(0)->getCell('G' . $row->getRowIndex())->getValue()); // Rendering provider
        $records[$risKey][39] = getNpi($npiValue);
        $records[$risKey][40] = '';
        $records[$risKey][41] = '0';
        $records[$risKey][42] = '';
        $records[$risKey][43] = '';
        $records[$risKey][44] = '';

        if (!empty($fields_rrmc[$risKey])) {
            $matchedRecords[$risKey] = $records[$risKey]; 
            $matchedRecords[$risKey][45] = $fields_rrmc[$risKey][5];
            $matchedRecords[$risKey][46] = $fields_rrmc[$risKey][4];
        } else {
            $missingRecords[$risKey] = $records[$risKey];
        }

        /* echo $key . ' ' . $dos . " " . $ptLastName . ' ' .  $ptFirstName . ' ' . $cpt . ' ' .
            $addr1 . ' ' . $addr2 . ' ' . $city . ' ' . $state . ' ' . $zip . ' ' .
            $dob . ' ' . $gender . ' ' .
            $primaryInsName . ' ' . $primaryPolicyValue . ' ' .
            $secondaryInsName . ' ' . $secondaryPolicyValue . ' ' . 
            $dx1 . ' ' . $dx2 . ' ' . $npiValue . "\n";  */
    }
}

foreach ($fields_mmc as $mkey => $data) {
    if (!array_key_exists($mkey, $fields_rrmc)) {
        echo "this Manch key $mkey is not in rrmc data \n";
    }
}

foreach ($fields_rrmc as $rkey => $data) {
    if (!array_key_exists($rkey, $fields_mmc)) {
        echo "this RRMC key $rkey is not in Manch data \n";
    }
}

foreach ($missingRecords as $key => $item) {
    echo "adding $key to matched records array so we can edit the file \n";
    $matchedRecords[$key] = $item;
}

foreach ($records as $item) {
    fputcsv($fp, $item, "\t");
}

fclose($fp);

function uC($string)
{
    return trim(strToUpper(str_replace([',', '-'], '', $string ?? '')));
}

function getNpi($string)
{
    if (strpos($string, 'AYRE') !== false) {
        return '1548206428';
    } elseif (strpos($string, 'HOWARD') !== false) {
        return '1821222068';
    } else {
        return '1346453834'; // sterling
    }
}

function formatDx($diag)
{
    if (empty($diag)) {
        return '';
    }
    $tmp = explode(':', $diag);
    return substr($tmp[0], 0, 3) . '.' . substr($tmp[0], 3);
}

function chcrrInsCode($ins_string, $policy)
{
    switch ($ins_string) {
        case (strpos($ins_string, 'MEDICARE B') !== false):
            return '34P';
        break;

        case (strpos($ins_string, 'BCBSVT') !== false):
            return '33P';
        break;

        case (strpos($ins_string, 'BCBS') !== false):
            return '47P';
        break;

        case (strpos($ins_string, 'UNITED HEALTHCARE') !== false):
            return '74P';
        break;

        case (strpos($ins_string, 'MVP HEALTH CARE') !== false):
            return '81P';
        break;

        case (strpos($ins_string, 'MEDICAIDVT') !== false):
            return '82P';
        break;

        case (strpos($ins_string, 'ANTHEM BCBSNY') !== false):
            return '47P';
        break;
        case (strpos($ins_string, 'SELFPAY (CASH)') !== false):
            return '0';
        break;
        case (strpos($ins_string, 'BLUE CROSSCA') !== false):
            return '47P';
        break;
        case (strpos($ins_string, 'AETNA & AETNA/US HEALTHCARE') !== false):
            return '100P';
        break;
        case (strpos($ins_string, 'BLUE BENEFIT ADMIN OF MASS') !== false):
            return '47P';
        break;
        case (strpos($ins_string, 'BCBSMN') !== false):
            return '47P';
        break;
        case (strpos($ins_string, 'WC  CORVEL') !== false):
            return '47P';
        break;
        case (strpos($ins_string, 'S&S HEALTHCARE STRATEGIES') !== false):
            return '58P';
        break;
        case (strpos($ins_string, 'BANKERS') !== false):
            return '27P';
        break;
        case (strpos($ins_string, 'AARP') !== false):
            return '4P';
        break;
        case (strpos($ins_string, 'CIGNA') !== false):
            return '58P';
        break;
        case (strpos($ins_string, 'BCBS') !== false):
            return '47P';
        break;
        case (strpos($ins_string, 'OXFORD') !== false):
            return '109P';
        break;
        case (strpos($ins_string, 'UNITED MEDICAL') !== false):
            return '110P';
        break;
        case (strpos($ins_string, 'TRICARE EAST') !== false):
            return '41P';
        break;
        case (strpos($ins_string, 'MERCHANTS BENEFIT') !== false):
            return '163P';
        break;
        case (strpos($ins_string, 'HUMANA') !== false):
            return '86P';
        break;
        case (strpos($ins_string, 'WELLCARE') !== false):
            return '144P';
        break;
        case (strpos($ins_string, 'HARVARD PILGRIM') !== false):
            return '89P';
        break;
        case (strpos($ins_string, 'CHAMPVA') !== false):
            return '24P';
        break;
        case (strpos($ins_string, 'SOLIDARITY') !== false):
            return '164P';
        break;
        case (strpos($ins_string, 'BLUE BENEFIT') !== false):
            return '47P';
            break;
        case (strpos($ins_string, 'UNICARE') !== false):
            return '165P';
        break;

        case (strpos($ins_string, 'ALLEGIANCE') !== false):
            return '166P';
        break;

        case (strpos($ins_string, 'MERITAIN') !== false):
            return '87P';
        break;

        case (strpos($ins_string, 'FIDELIS') !== false):
            return '65P';
        break;

        case (strpos($ins_string, 'FOREIGN') !== false):
            return '95P';
        break;

        case (strpos($ins_string, 'EMBLEM') !== false):
            return '114P';
        break;

        case (strpos($ins_string, 'TUFTS') !== false):
            return '167P';
        break;

        case (strpos($ins_string, 'WELLMED') !== false):
            return '168P';
        break;

        case (strpos($ins_string, 'KAISER') !== false):
            return '169P';
        break;

        case (strpos($ins_string, 'USFHP') !== false):
            return '31P';
        break;

        case (strpos($ins_string, 'PRIORITY') !== false):
            return '170P';
        break;

        case (strpos($ins_string, 'AXA') !== false):
            return '171P';
        break;

        case (strpos($ins_string, 'MERCER') !== false):
            return '172P';
        break;

        case (strpos($ins_string, 'AETNA LIFE') !== false):
            return '173P';
        break;

        case (strpos($ins_string, 'UNITED AMERICAN') !== false):
            return '174P';
        break;

        case (strpos($ins_string, 'MUTUAL OF OMAHA') !== false):
            return '113P';
        break;

        case (strpos($ins_string, 'USAA LIFE') !== false):
            return '175P';
        break;

        case (strpos($ins_string, 'CONTINENTAL LIFE') !== false):
            return '173P';
        break;

        case (strpos($ins_string, 'WEB TPA') !== false):
            return '171P';
        break;

        case (strpos($ins_string, 'GENWORTH') !== false):
            return '176P';
        break;

        case (strpos($ins_string, 'NASSAU') !== false):
            return '177P';
        break;

        case (strpos($ins_string, 'TRICARE FOR LIFE') !== false):
            return '43P';
        break;

        default:
            return '***BAD INS***';
    }
}