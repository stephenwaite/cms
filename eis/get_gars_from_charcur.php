<?php

/* @package cms
* @link    http://www.cmsvt.com
* @author  s waite <stephen.waite@cmsvt.com>
* @copyright Copyright (c) 2024 cms <stephen.waite@cmsvt.com>
* @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
*/

require __DIR__ . '/../vendor/autoload.php';

use PhpOffice\PhpSpreadsheet\IOFactory;

$databaseHost   = 'localhost';
$port   = 8320;
$databaseUsername = 'root';
$databasePassword   = 'root';
$databaseName  = 'openemr';
$db_encoding = 'utf8mb4';

$mysqli = mysqli_connect('localhost:8320', 'root', 'root', 'openemr'); 

$inputFileName = '/home/stee/Documents/eis/ecg_unbilled.xlsx';
$inputFileType = 'Xlsx';
/* $fp = fopen('/tmp/charges.csv', 'w');
$fpMissing = fopen('/tmp/missing_charges.csv', 'w'); */

$reader = IOFactory::createReader($inputFileType);
$spreadsheet = $reader->load($inputFileName);
$worksheet = $spreadsheet->getActiveSheet();
foreach ($worksheet->getRowIterator() as $row) {
    //var_dump($value);
    $cellIterator = $row->getCellIterator();
    foreach($cellIterator as $key => $cell) {
        $cellValue = $cell->getFormattedValue();
        switch ($key) {
            case 'F':
                $name = explode(',', $cellValue);
                //var_dump($name);
                $lname = trim($name[0]);
                $fname = trim($name[1]);
                break;

            case 'G':
                $dob_raw = $cellValue;
                $dobDateTime = date_create_from_format('m/d/Y', $dob_raw);
                $dob = $dobDateTime->format('Y-m-d');
                //var_dump($dob);
                break;
            case 'I':
                $code = $cellValue;
                break;
            case 'J':
                $dx = $cellValue;
                break;        
            case 'K':
                    $dos_raw = $cellValue;
                    $dosDateTime = date_create_from_format('m/d/Y', $dos_raw);
                    $dos = $dosDateTime->format('Y-m-d');
                    //var_dump($dob);
                    break;    
        }
    }
    $query = "SELECT `pid`, `lname`, `fname` FROM `patient_data` WHERE `fname` like '%" . substr($fname, 0, 3) . "%' AND `lname` LIKE '%" .
        substr($lname, 0, 3) . "%' AND `DOB` = '" . $dob . "'";

    if ($row = mariaQuery($query, $mysqli)) {
        // pt exists, check for encounter
        $query = "SELECT `pid`, `encounter` FROM `form_encounter` WHERE `date` like '" . $dos . "%' AND `pid` = '" . $row[0] . "'";
        if (!($row = mariaQuery($query, $mysqli))) {
            var_dump($row);
        }
    }
        

     
}

function mariaQuery($query, $mysqli) {
    if ($result = $mysqli->query($query)) {
        while ($row = $result->fetch_row()) {
            //printf ("%s (%s)\n", $row[0], $row[1]);
            return $row;
        }
        $result->free_result();
    } else {
        return false;
    }
//var_dump($result->num_rows);

//exit;
}

exit;
$savedGarno = '';
$garnosWithCharges2024 = [];
// read the charges file and then grab a record from garfile
if ($charcurFile = fopen($argv[1], "r")) {
    while (($charcurLine = fgets($charcurFile)) !== false) {
        $garno = substr($charcurLine, 0, 8);
        //$code = substr($line, 37, 5);
        //$ins = substr($line, 29, 3);
      //echo $garno . "\n";
      //echo $code . "\n";
        if (empty($garnosWithCharges2024[$garno])) {
            $garnosWithCharges2024[$garno] = $garno;
        }

        $savedGarno = $garno;
    }
}

//var_dump($garnosWithCharges2024);
echo count($garnosWithCharges2024) . "\n";
exit;

foreach ($garnosWithCharges2024 as $key => $value) {
    if ($garfile = fopen($argv[2], "r")) {
        while (($garfileLine = fgets($garfile)) !== false) {
            $garno = substr($charcurLine, 0, 8);
            //$code = substr($line, 37, 5);
            //$ins = substr($line, 29, 3);
          //echo $garno . "\n";
          //echo $code . "\n";
            if (empty($garnosWithCharges2024[$garno])) {
                $garnosWithCharges2024[$garno] = $garno;
            }
    
            $savedGarno = $garno;
        }
    }
    
}


$createTable = "CREATE TABLE `cmsvt_garfile`
(
    `garno` CHAR(8) NOT NULL COMMENT 'foreign key to actfile',
    `garname` VARCHAR(24) COMMENT 'pt lname,fname mname',
    `billadd` VARCHAR(22) COMMENT 'billing address',
    `street` VARCHAR(22) COMMENT 'street',
    `city` VARCHAR(18) COMMENT 'city',
    `state` CHAR(2) COMMENT 'state',
    `zip` VARCHAR(9) COMMENT 'zip',
    `collt` CHAR(1) COMMENT 'collection indicator',
    `phone` VARCHAR(12) COMMENT 'phone',
    `sex` CHAR(1) COMMENT 'patient sex M/F',
    `relate` CHAR(1) COMMENT 'relate code',
    `mstat` CHAR(1) COMMENT 'marital status? unused?',
    `dob` CHAR(8) COMMENT 'pt dob',
    `dunning` CHAR(1) COMMENT 'dunning indicator',
    `acctstat` CHAR(1) COMMENT 'account status',
    `prmplr` CHAR(4) COMMENT 'primary ins employer code',
    `prins` VARCHAR(3) COMMENT 'primary ins code',
    `prassign` CHAR(1) COMMENT 'primary ins assignment indicator',
    `proffice` CHAR(4) COMMENT 'primary office code? unused',
    `prgroup` VARCHAR(10) COMMENT 'primary ins group number',
    `pripol` VARCHAR(16) COMMENT 'primary insurance policy number',
    `prname` VARCHAR(24) COMMENT 'primary ins subscriber name',
    `prrelate` CHAR(1) COMMENT 'primary insurance relate code',
    `addrcode` CHAR(4) COMMENT 'address code',
    `seins` VARCHAR(3) COMMENT 'secondary ins code',
    `seassign` CHAR(1) COMMENT 'secondary ins assignment indicator',
    `trinsind` CHAR(1) COMMENT 'trinsind',
    `trins` VARCHAR(3) COMMENT 'trins',
    `segroup` VARCHAR(10) COMMENT 'secondary ins group number',
    `secpol` VARCHAR(16) COMMENT 'secondary insurance policy number',
    `sename` VARCHAR(35) COMMENT 'secondary ins subscriber name',
    `serelate` CHAR(1) COMMENT 'secondary insurance relate code',
    `inspend` DECIMAL(7,2) COMMENT 'insurance pending',
    `lastbill` CHAR(8) COMMENT 'date of last bill',
    `assignm` CHAR(1) COMMENT 'patient assignment? unused',
    `private` CHAR(1) COMMENT 'private? unused',
    `billcycle` CHAR(1) COMMENT 'billing cycle, 1 thru 4',
    `delete` CHAR(1) COMMENT 'delete indicator, unused',
    `filler` CHAR(3) COMMENT 'filler',
    `acct` CHAR(8) COMMENT 'acct',
    `prgrpname` VARCHAR(15) COMMENT 'primary ins employer group name',
    `segrpname` VARCHAR(15) COMMENT 'secondary ins employer group name',
    UNIQUE KEY (`garno`),
    UNIQUE KEY (`actno`),
) ENGINE=InnoDB;";