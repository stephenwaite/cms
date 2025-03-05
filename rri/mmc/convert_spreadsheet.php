<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once 'risImport.php';

use PhpOffice\PhpSpreadsheet\IOFactory;

// php convert_spreadsheet 20240901 202040930 0
// fromdate todate chcrr load are params

$inputFileName = '/home/stee/Documents/test.xlsx';
$inputFileType = 'Xlsx';
$fp = fopen('/tmp/charges.csv', 'w');
$fpMissing = fopen('/tmp/missing_charges.csv', 'w');

$reader = IOFactory::createReader($inputFileType);
$spreadsheet = $reader->load($inputFileName);
$worksheet = $spreadsheet->getActiveSheet();
//$rows = $worksheet->rangeToArray("A1:V1110");
$rows = $worksheet->ToArray();
$fields_mmc = array();
foreach ($rows as $key => $value) {
    
    if ($key == 0) {
        //var_dump($value);
        continue;
    }

    //var_dump($value);
    //exit;

    $cpt_arr = explode(':', uC($value[8]));
    $cpt = trim(substr($cpt_arr[0], 0, 5));

    //echo $value[0] . "\n";
    //echo $value[1] . "\n";
    $dos = getDos($value[1]);
    //echo $dos;
    $rrmc_dos = (new DateTime($dos))->format('Ymd');
    //var_dump($rrmc_dos);
    $compare_dos = (new DateTime($dos))->format('Ymd');
    if ($compare_dos < $fromDate || $compare_dos > $toDate) {
        continue;
    }
    $rawPtLastName = $value[3];
    $ptLastName = uC($rawPtLastName); //lname
    // ignore suffixes
    $ptSuffixPos = strpos($rawPtLastName, ',');
    if (!empty($ptSuffixPos)) {
        $ptSuffix = substr($rawPtLastName, 0, $ptSuffixPos);
        $mmcPtLastName = preg_replace("/[^A-Z]/", '', $ptSuffix);
    } else {
        $mmcPtLastName = $ptLastName;
    }
    $ptFirstName = uC($value[2]); //fname
    $risKey = str_pad(substr($mmcPtLastName, 0, 5), 5) . $rrmc_dos . $cpt;
    $fields_mmc[$risKey] = array($mmcPtLastName, $ptFirstName, $cpt, $dos);

    $fields[$risKey][0] = $ptLastName;
    $fields[$risKey][1] = $ptFirstName;
    $fields[$risKey][2] = uC($value[15]); // addr 1
    if (!empty($value[16])) {
        $fields[$risKey][3] = uC($value[16]); // addr 2
    } else {
        $fields[$risKey][3] = '';
    }
    $city = uC($value[17]); // city
    //var_dump($city);
    $fields[$risKey][4] = ($city == 'MANCHESTER CENTER') ? 'MANCHESTER CTR' : $city;
    $fields[$risKey][5] = uC($value[18]); // state
    $zip = uC($value[19]); // zip
    $fields[$risKey][6] = (strlen($zip) == 4) ? '0' . $zip : $zip;
    $fields[$risKey][7] = uC($value[4]); //dob

    $gender = uC(substr($value[5], 0, 1)); //gender
    $fields[$risKey][8] = $gender;
    $fields[$risKey][9] = chcrrInsCode(($value[11]), $value[12]);
    $fields[$risKey][10] = ''; // chcrr ins name
    $fields[$risKey][11] = ''; // chcrr ins addr
    $fields[$risKey][12] = ''; // chcrr ins city
    $fields[$risKey][13] = ''; // chcrr ins state
    $fields[$risKey][14] = ''; // chcrr ins zip
    $rawPolicy = $value[12];
    //var_dump($rawPolicy);
    if (is_numeric($rawPolicy)) {
        $fields[$risKey][15] = $rawPolicy;
    } else {
        $fields[$risKey][15] = uC($value[12]); // policy
    }
    $fields[$risKey][16] = ''; // group
    $fields[$risKey][17] = $fields[$risKey][0]; // hard code guarnator to be patient
    $fields[$risKey][18] = $fields[$risKey][1];
    $fields[$risKey][19] = $fields[$risKey][8];
    $fields[$risKey][20] = 'Self';
    $fields[$risKey][21] = chcrrInsCode(uC($value[13]), $value[14]);
    $fields[$risKey][22] = '';
    $fields[$risKey][23] = '';
    $fields[$risKey][24] = '';
    $fields[$risKey][25] = '';
    $fields[$risKey][26] = '';
    $fields[$risKey][27] = '';
    if (!empty($value[14])) {
        $fields[$risKey][28] = $value[14];
    } else {
            $fields[$risKey][28] = '';
    }
    $fields[$risKey][29] = '';
    $fields[$risKey][30] = '';
    $fields[$risKey][31] = $gender;
    $fields[$risKey][32] = 'S';
    $fields[$risKey][33] = $cpt . "TC"; //chcrr sends TC mod
    $dx1 = formatDx(uC($value[9]));
    $fields[$risKey][34] = $dx1;
    $dx2 = formatDx(uC($value[10]));
    $fields[$risKey][35] = $dx2;
    $fields[$risKey][36] = '';
    $fields[$risKey][37] = '';
    $fields[$risKey][38] = $dos;
    // skip value 5 rendering, use value 6 supervising for billing
    $fields[$risKey][39] = getNpi(uC($value[7]));
    $fields[$risKey][40] = '';
    $fields[$risKey][41] = '0';
    $fields[$risKey][42] = '';
    $fields[$risKey][43] = '';
    $fields[$risKey][44] = '';

    //echo $risKey . " " . $fields_rrmc[$risKey][5] . "\n";
    if (!empty($fields_rrmc[$risKey])) {
        $fields[$risKey][45] = $fields_rrmc[$risKey][5];
        $fields[$risKey][46] = $fields_rrmc[$risKey][4];
    }
}
//var_dump($fields_rrmc);
//exit;
foreach ($fields_mmc as $mkey => $data) {
    if (!array_key_exists($mkey, $fields_rrmc)) {
        //var_dump($data);
        echo "this Manch key $mkey is not in rrmc data \n";
        //exit;
    }
}

foreach ($fields_rrmc as $rkey => $data) {
    if (!array_key_exists($rkey, $fields_mmc)) {
        echo "this RRMC read $rkey is not in Manch data \n";
        fputcsv($fpMissing, $data);
    }
}

foreach ($fields as $item) {
    fputcsv($fp, $item, "\t");
}

fclose($fp);
/* $fo = fopen('/tmp/test.csv', 'w');


foreach (file("/tmp/mmcCharges.csv") as $line) {
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

/*
  array(20) {
  [0]=>
  string(15) "Date of Service"
  [1]=>
  string(17) "Patient Last Name"
  [2]=>
  string(18) "Patient First Name"
  [3]=>
  string(7) "Pt  DOB"
  [4]=>
  string(6) "Gender"
  [5]=>
  string(17) "Ordering Provider"
  [6]=>
  string(21) "Supervising Physician"
  [7]=>
  string(14) "Exam/CPT  Code"
  [8]=>
  string(22) "Primary Diagnosis Code"
  [9]=>
  string(25) "Additional Diagnosis Code"
  [10]=>
  string(17) "Insurance Company"
  [11]=>
  string(17) "Pt Middle Initial"
  [12]=>
  string(17) "Pt Address Line 1"
  [13]=>
  string(17) "Pt Address Line 2"
  [14]=>
  string(4) "City"
  [15]=>
  string(5) "State"
  [16]=>
  string(3) "ZIP"
  [17]=>
  string(19) "Guarantor Last Name"
  [18]=>
  string(20) "Guarantor First Name"
  [19]=>
  string(16) "Policy ID Number"
*/

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
        case (strpos($ins_string, 'Medicare-VT') !== false):
            return '34P';
        break;

        case (stripos($ins_string, 'MedicareAR') !== false):
            return '34P';
        break;

        case (stripos($ins_string, 'MedicareVT') !== false):
            return '34P';
        break;

        case (strpos($ins_string, 'BCBS-VT') !== false):
            return '33P';
        break;

        case (stripos($ins_string, 'BCBS') !== false):
            return '47P';
        break;

        case (stripos($ins_string, 'UNITED HEALTHCARE') !== false):
            return '74P';
        break;

        case (stripos($ins_string, 'MVP HEALTH CARE') !== false):
            return '81P';
        break;

        case (stripos($ins_string, 'GREEN MOUNTAIN') !== false):
            return '82P';
        break;

        case (stripos($ins_string, 'ANTHEM') !== false):
            return '47P';
        break;

        case (stripos($ins_string, 'SELFPAY (CASH)') !== false):
            return '0';
        break;

        case (stripos($ins_string, 'SELMAN') !== false):
            return '0';
        break;

        case (stripos($ins_string, 'BLUE CROSSCA') !== false):
            return '47P';
        break;
        case (stripos($ins_string, 'AETNA & AETNA/US HEALTHCARE') !== false):
            return '100P';
        break;
        case (stripos($ins_string, 'BLUE BENEFIT ADMIN OF MASS') !== false):
            return '47P';
        break;
        case (stripos($ins_string, 'BCBSMN') !== false):
            return '47P';
        break;
        case (stripos($ins_string, 'WC  CORVEL') !== false):
            return '47P';
        break;
        case (stripos($ins_string, 'S&S HEALTHCARE STRATEGIES') !== false):
            return '58P';
        break;
        case (stripos($ins_string, 'BANKERS') !== false):
            return '27P';
        break;
        case (stripos($ins_string, 'AARP') !== false):
            return '4P';
        break;
        case (stripos($ins_string, 'CIGNA') !== false):
            return '58P';
        break;
        case (stripos($ins_string, 'BCBS') !== false):
            return '47P';
        break;
        case (stripos($ins_string, 'OXFORD') !== false):
            return '109P';
        break;
        case (stripos($ins_string, 'UNITED MEDICAL') !== false):
            return '110P';
        break;
        case (stripos($ins_string, 'TRICARE EAST') !== false):
            return '41P';
        break;
        case (stripos($ins_string, 'MERCHANTS BENEFIT') !== false):
            return '163P';
        break;
        case (stripos($ins_string, 'HUMANA') !== false):
            return '86P';
        break;
        case (stripos($ins_string, 'WELLCARE') !== false):
            return '144P';
        break;
        case (stripos($ins_string, 'HARVARD PILGRIM') !== false):
            return '89P';
        break;
        case (stripos($ins_string, 'CHAMPVA') !== false):
            return '24P';
        break;
        case (stripos($ins_string, 'SOLIDARITY') !== false):
            return '164P';
        break;
        case (stripos($ins_string, 'BLUE BENEFIT') !== false):
            return '47P';
            break;
        case (stripos($ins_string, 'UNICARE') !== false):
            return '165P';
        break;

        case (stripos($ins_string, 'ALLEGIANCE') !== false):
            return '166P';
        break;

        case (stripos($ins_string, 'MERITAIN') !== false):
            return '87P';
        break;

        case (stripos($ins_string, 'FIDELIS') !== false):
            return '65P';
        break;

        case (stripos($ins_string, 'FOREIGN') !== false):
            return '95P';
        break;

        case (stripos($ins_string, 'EMBLEM') !== false):
            return '114P';
        break;

        case (stripos($ins_string, 'TUFTS') !== false):
            return '167P';
        break;

        case (stripos($ins_string, 'WELLMED') !== false):
            return '168P';
        break;

        case (stripos($ins_string, 'KAISER') !== false):
            return '169P';
        break;

        case (stripos($ins_string, 'MARTIN') !== false):
            return '31P';
        break;

        case (stripos($ins_string, 'PRIORITY') !== false):
            return '170P';
        break;

        case (stripos($ins_string, 'AXA') !== false):
            return '171P';
        break;

        case (stripos($ins_string, 'MERCER') !== false):
            return '172P';
        break;

        case (stripos($ins_string, 'AETNA LIFE') !== false):
            return '173P';
        break;

        case (stripos($ins_string, 'AETNA') !== false):
            return '100P';
        break;

        case (stripos($ins_string, 'UNITED AMERICAN') !== false):
            return '174P';
        break;

        case (stripos($ins_string, 'MUTUAL OF OMAHA') !== false):
            return '113P';
        break;

        case (stripos($ins_string, 'USAA LIFE') !== false):
            return '175P';
        break;

        case (stripos($ins_string, 'CONTINENTAL LIFE') !== false):
            return '173P';
        break;

        case (stripos($ins_string, 'WEB TPA') !== false):
            return '171P';
        break;

        case (stripos($ins_string, 'GENWORTH') !== false):
            return '176P';
        break;

        case (stripos($ins_string, 'NASSAU') !== false):
            return '177P';
        break;

        case (stripos($ins_string, 'MAGNACARE') !== false):
            return '178P';
        break;

        case (stripos($ins_string, 'Palmetto') !== false):
            return '179P';
        break;

        case (stripos($ins_string, 'Lucent') !== false):
            return '180P';
        break;

        case (stripos($ins_string, 'TRICARE FOR LIFE') !== false):
            return '43P';
        break;

        case (stripos($ins_string, '*SELF PAY*') !== false):
            return '0P';
        break;

        default:
            echo $ins_string . "\n";
            return '***BAD INS***';
    }
}

/* if (!array_key_exists($rrmc_mmc_key, $rrmc_fields)) {
    echo "uh oh, rrmc doesn't have this exam $rrmc_mmc_key \n";
    continue;
} else {
    echo "this is in the rrmc db $rrmc_mmc_key \n";
} */
