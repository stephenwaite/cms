<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once 'mmc-rrmc.php';

use PhpOffice\PhpSpreadsheet\IOFactory;

$inputFileName = '/tmp/test.xlsx';
$inputFileType = 'Xlsx';
$fp = fopen('/tmp/mmcCharges.csv', 'w');

$reader = IOFactory::createReader($inputFileType);
$spreadsheet = $reader->load($inputFileName);
$worksheet = $spreadsheet->getActiveSheet();
$rows = $worksheet->toArray();
$cntr = 0;
foreach ($rows as $key => $value) {
    if ($key == 0) {
        //var_dump($value);
        continue;
    }

    //var_dump($value);
    //exit;

    $cntr++;

    $cpt_arr = explode(':', uC($value[7]));
    $cpt = trim($cpt_arr[0]);
    if ($cpt == '72072') {
        $cpt = '72070';
    }
    $dos = getDos($value[0]);
    $rrmc_dos = (new DateTime($dos))->format('mdy');
    $compare_dos = (new DateTime($dos))->format('ymd');
    if ($compare_dos < $fromDate || $compare_dos > $toDate) {
        continue;
    }
    $rawPtLastName = $value[1];
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
    $mmc_key = $mmcPtLastName . $rrmc_dos . $cpt;
    $fields_mmc[$mmc_key] = array($mmcPtLastName, $ptFirstName, $cpt);

    $fields[$cntr][0] = $ptLastName;
    $fields[$cntr][1] = $ptFirstName;
    $fields[$cntr][2] = uC($value[14]); // addr 1
    $fields[$cntr][3] = uC($value[15]); // addr 2
    $city = uC($value[16]); // city
    //var_dump($city);
    $fields[$cntr][4] = ($city == 'MANCHESTER CENTER') ? 'MANCHESTER CTR' : $city;
    $fields[$cntr][5] = uC($value[17]); // state
    $zip = uC($value[18]); // zip
    $fields[$cntr][6] = (strlen($zip) == 4) ? '0' . $zip : $zip;
    $fields[$cntr][7] = uC($value[3]); //dob

    $fields[$cntr][8] = uC($value[4]); //gender
    $fields[$cntr][9] = chcrrInsCode(uC($value[10]), $value[11]);
    $fields[$cntr][10] = ''; // chcrr ins name
    $fields[$cntr][11] = ''; // chcrr ins addr
    $fields[$cntr][12] = ''; // chcrr ins city
    $fields[$cntr][13] = ''; // chcrr ins state
    $fields[$cntr][14] = ''; // chcrr ins zip
    $fields[$cntr][15] = uC($value[11]); // policy
    $fields[$cntr][16] = ''; // group
    $fields[$cntr][17] = $fields[$cntr][0]; // hard code guarnator to be patient
    $fields[$cntr][18] = $fields[$cntr][1];
    $fields[$cntr][19] = $fields[$cntr][8];
    $fields[$cntr][20] = 'Self';
    $fields[$cntr][21] = chcrrInsCode(uC($value[12]), $value[13]);
    $fields[$cntr][22] = '';
    $fields[$cntr][23] = '';
    $fields[$cntr][24] = '';
    $fields[$cntr][25] = '';
    $fields[$cntr][26] = '';
    $fields[$cntr][27] = '';
    $fields[$cntr][28] = $value[13];
    $fields[$cntr][29] = '';
    $fields[$cntr][30] = '';
    $fields[$cntr][31] = uC($value[4]);
    $fields[$cntr][32] = 'S';
    $fields[$cntr][33] = $cpt . "TC"; //chcrr sends TC mod
    $dx1 = formatDx(uC($value[8]));
    $fields[$cntr][34] = $dx1;
    $dx2 = formatDx(uC($value[9]));
    $fields[$cntr][35] = $dx2;
    $fields[$cntr][36] = '';
    $fields[$cntr][37] = '';
    $fields[$cntr][38] = $dos;
    // skip value 5 rendering, use value 6 supervising for billing
    $fields[$cntr][39] = getNpi(uC($value[6]));
    $fields[$cntr][40] = '';
    $fields[$cntr][41] = '0';
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

/* if (!array_key_exists($rrmc_mmc_key, $rrmc_fields)) {
    echo "uh oh, rrmc doesn't have this exam $rrmc_mmc_key \n";
    continue;
} else {
    echo "this is in the rrmc db $rrmc_mmc_key \n";
} */
