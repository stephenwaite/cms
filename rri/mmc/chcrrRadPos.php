<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once 'risImport.php';

use PhpOffice\PhpSpreadsheet\Reader\Csv;

$inputFileName = '/tmp/test.csv';
$inputFileType = 'csv';
$fp = fopen('/tmp/charges.csv', 'w');

$reader = new Csv();
$spreadsheet = $reader->load($inputFileName);
$worksheet = $spreadsheet->getActiveSheet();
$rows = $worksheet->toArray();
$fields_chcrr = array();
$newFieldsChcrr = array();
foreach ($rows as $key => $value) {
    if ($key == 0) {
        //var_dump($value);
        continue;
    }

    //var_dump($value);
    //exit;

    $cpt = substr($value[33], 0, 5);
    if ($cpt == '72072') {
        $cpt = '72070';
    }

    /* if ($cpt == '73564') {
        $cpt = '73562';
    } */

    $dos = getDos($value[38]);
    $rrmc_dos = (new DateTime($dos))->format('Ymd');
    $compare_dos = (new DateTime($dos))->format('Ymd');
    if ($compare_dos < $fromDate || $compare_dos > $toDate) {
        continue;
    }
    $rawPtLastName = $value[0];
    $ptLastName = uC($rawPtLastName); //lname
    // ignore suffixes
    $ptSuffixPos = strpos($rawPtLastName, ',');
    if (!empty($ptSuffixPos)) {
        $ptSuffix = substr($rawPtLastName, 0, $ptSuffixPos);
        $chcrrPtLastName = preg_replace("/[^A-Z]/", '', $ptSuffix);
    } else {
        $chcrrPtLastName = $ptLastName;
    }
    $ptFirstName = uC($value[1]); //fname
    $risKey = $chcrrPtLastName . $rrmc_dos . $cpt;
    $fields_chcrr[$risKey] = array($chcrrPtLastName, $ptFirstName, $cpt, $dos);


    if (!empty($fields_rrmc[$risKey])) {
        $newFieldsChcrr[$key] = $value;
        array_push($newFieldsChcrr[$key], $fields_rrmc[$risKey][5]);
        array_push($newFieldsChcrr[$key], $fields_rrmc[$risKey][4]);
        //var_dump($newFieldsChcrr);
        //exit;
    }
}

foreach ($fields_chcrr as $okey => $data) {
    if (!array_key_exists($okey, $fields_rrmc)) {
        echo "this outread key $okey is not in rrmc data \n";
    }
}

/* foreach ($fields_rrmc as $rkey => $data) {
    if (!array_key_exists($rkey, $fields_chcrr)) {
        echo "this RRMC key $rkey is not in Manch data \n";
    }
} */

foreach ($newFieldsChcrr as $item) {
    fputcsv($fp, $item);
}

fclose($fp);
/* $fo = fopen('/tmp/test.csv', 'w');


foreach (file("/tmp/chcrrCharges.csv") as $line) {
    $line = str_replace('"', '', $line);
    fwrite($fo, $line);
} */



function formatDx($diag)
{
    if (empty($diag)) {
        return '';
    }
    $tmp = explode(':', $diag);
    return substr($tmp[0], 0, 3) . '.' . substr($tmp[0], 3);
}

function getDos($date)
{
    return date_format(date_create($date), "m/d/y");
}

/* array(45) {
    [0]=>
    string(11) "P Last Name"
    [1]=>
    string(12) "P First Name"
    [2]=>
    string(9) "P Address"
    [3]=>
    string(11) "P Address 2"
    [4]=>
    string(6) "P City"
    [5]=>
    string(7) "P State"
    [6]=>
    string(10) "P Zip Code"
    [7]=>
    string(15) "P Date of Birth"
    [8]=>
    string(13) "P Patient Sex"
    [9]=>
    string(21) "HA Insurance Company#"
    [10]=>
    string(25) "HA Insurance Company Name"
    [11]=>
    string(28) "HA Insurance Company Address"
    [12]=>
    string(25) "HA Insurance Company City"
    [13]=>
    string(26) "HA Insurance Company State"
    [14]=>
    string(24) "HA Insurance Company Zip"
    [15]=>
    string(26) "HA Insurance Policy Number"
    [16]=>
    string(16) "HA Date of Onset"
    [17]=>
    string(27) "P Primary Insured Last Name"
    [18]=>
    string(28) "P Primary Insured First Name"
    [19]=>
    string(21) "P Primary Insured Sex"
    [20]=>
    string(34) "P Primary Relation to Insured Desc"
    [21]=>
    string(25) "P Secondary Ins Co Number"
    [22]=>
    string(23) "P Secondary Ins Co Name"
    [23]=>
    string(27) "P Secondary Ins Co Address1"
    [24]=>
    string(23) "P Secondary Ins Co City"
    [25]=>
    string(24) "P Secondary Ins Co State"
    [26]=>
    string(22) "P Secondary Ins Co Zip"
    [27]=>
    string(25) "P Secondary Date of Onset"
    [28]=>
    string(25) "P Secondary Policy Number"
    [29]=>
    string(29) "P Secondary Insured Last Name"
    [30]=>
    string(30) "P Secondary Insured First Name"
    [31]=>
    string(23) "P Secondary Insured Sex"
    [32]=>
    string(31) "P Secondary Relation to Insured"
    [33]=>
    string(17) "HA CPT_HCPCS Code"
    [34]=>
    string(16) "HA 1st Diagnosis"
    [35]=>
    string(16) "HA 2nd Diagnosis"
    [36]=>
    string(16) "HA 3rd Diagnosis"
    [37]=>
    string(16) "HA 4th Diagnosis"
    [38]=>
    string(15) "HA Service Date"
    [39]=>
    string(14) "HA Doctor NPI#"
    [40]=>
    string(24) "P Tertiary Date of Onset"
    [41]=>
    string(24) "P Tertiary Ins Co Number"
    [42]=>
    string(24) "P Tertiary Policy Number"
    [43]=>
    string(22) "P Tertiary Ins Co Name"
    [44]=>
    string(22) "P Tertiary Ins Co City"
  } */

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

/* if (!array_key_exists($rrmc_chcrr_key, $rrmc_fields)) {
    echo "uh oh, rrmc doesn't have this exam $rrmc_chcrr_key \n";
    continue;
} else {
    echo "this is in the rrmc db $rrmc_chcrr_key \n";
} */
