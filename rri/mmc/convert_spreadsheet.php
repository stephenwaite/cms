<?php

require __DIR__ . '/../../vendor/autoload.php';

use PhpOffice\PhpSpreadsheet\IOFactory;

$inputFileName = '/tmp/test.xlsx';
$inputFileType = 'Xlsx';
$fp = fopen('/tmp/testing.csv', 'w');

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

    $cntr++;

    if (empty($value[0]) && empty($value[1]) && empty($value[7])) {
        continue;
    }

    if (empty($value[0]) && empty($value[1]) && !empty($value[7])) {
        $fields[$cntr] = $fields[$cntr - 1];
        $cpt_arr = explode(':', uC($value[7]));
        $fields[$cntr][33] = $cpt_arr[0] . "TC"; //chcrr sends TC mod
        continue;
    }
    // if this cell is empty it's the 2ndary ins
    if (empty($value[0])) {
        $fields[$cntr - 1][21] = chcrrInsCode(uC($value[10]), $value[9]);
        $fields[$cntr - 1][28] = uC($value[19]);
        $fields[$cntr - 1][29] = $value[17]; // hard code guar 2ndary ins
        $fields[$cntr - 1][30] = $value[18];
        continue;
    }
    //var_dump($value);

    $fields[$cntr][0] = uC($value[1]); //lname
    $fields[$cntr][1] = uC($value[2]); //fname
    $fields[$cntr][2] = uC($value[12]);
    $fields[$cntr][3] = uC($value[13]);
    $fields[$cntr][4] = uC($value[14]);
    $fields[$cntr][5] = uC($value[15]);
    $zip = uC($value[16]);
    $fields[$cntr][6] = (strlen($zip) == 4) ? '0' . $zip : $zip;
    $fields[$cntr][7] = uC($value[3]);

    $fields[$cntr][8] = uC($value[4]); //gender
    $fields[$cntr][9] = chcrrInsCode(uC($value[10]), $value[19]);
    $fields[$cntr][10] = '';
    $fields[$cntr][11] = '';
    $fields[$cntr][12] = '';
    $fields[$cntr][13] = '';
    $fields[$cntr][14] = '';
    $fields[$cntr][15] = uC($value[19]); // policy
    $fields[$cntr][16] = ''; // group
    $fields[$cntr][17] = $fields[$cntr][0]; // hard code guarnator to be patient
    $fields[$cntr][18] = $fields[$cntr][1];
    $fields[$cntr][19] = $fields[$cntr][8];
    $fields[$cntr][20] = 'Self';
    $fields[$cntr][21] = '';// secondary ins to [30]
    $fields[$cntr][22] = '';
    $fields[$cntr][23] = '';
    $fields[$cntr][24] = '';
    $fields[$cntr][25] = '';
    $fields[$cntr][26] = '';
    $fields[$cntr][27] = '';
    $fields[$cntr][28] = '';
    $fields[$cntr][29] = '';
    $fields[$cntr][30] = '';
    $fields[$cntr][31] = uC($value[4]);
    $fields[$cntr][32] = 'S';
    $cpt_arr = explode(':', uC($value[7]));
    $fields[$cntr][33] = $cpt_arr[0] . "TC"; //chcrr sends TC mod
    $dx1 = formatDx(uC($value[8]));
    $fields[$cntr][34] = $dx1;
    $dx2 = formatDx(uC($value[9]));
    $fields[$cntr][35] = $dx2;
    $fields[$cntr][36] = '';
    $fields[$cntr][37] = '';
    $dos = getDos($value[0]);
    $fields[$cntr][38] = $dos;
    $fields[$cntr][39] = getNpi(uC($value[6]));
    $fields[$cntr][40] = '';
    $fields[$cntr][41] = '0';
}

foreach ($fields as $item) {
    fputcsv($fp, $item, "\t");
}

fclose($fp);
$fo = fopen('/tmp/test.csv', 'w');
foreach (file("/tmp/testing.csv") as $line) {
    $line = str_replace('"', '', $line);
    fwrite($fo, $line);
}

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
    return trim(strToUpper(str_replace([',', '-'], '', $string)));
}

function getNpi($string)
{
    if (strpos($string, 'STERLING') !== false) {
        return '1346453834';
    } else {
        return '1548206428';
    }
}

function chcrrInsCode($ins_string, $policy)
{
    switch ($ins_string) {
        case (strpos($ins_string, 'MEDICARE BVT') !== false):
            return '34P';
            break;
        case (strpos($ins_string, 'BCBSVT') !== false):
            return '140';
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
        default:
            return '***BAD INS***';
    }
}
