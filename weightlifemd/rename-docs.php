<?php

/**
 * rename docs
 *
 * @author    stephen waite <stephen.waite@cmsvt.com>
 * @copyright Copyright (c) 2023 stephen waite <stephen.waite@cmsvt.com>
 * @license   https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
*/

// comment this out when using this script (and then uncomment it again when done using script)
//exit;

if (php_sapi_name() !== 'cli') {
    echo "Only php cli can execute command\n";
    echo "example use: php <script> filename start_record\n";
    die;
}

require_once dirname(__FILE__, 2) . "/vendor/autoload.php";

use League\Csv\Reader;

// setup a csv file with a header consiting of type, code and modifier
// at the specified location
$filename = DIRECTORY_SEPARATOR . $argv[1];
$filepath = "/tmp";
$reader = Reader::createFromPath($filepath . $filename);
$reader->setDelimiter("|");

$start_record = $argv[2];
$reader->setHeaderOffset($start_record);
$header = $reader->getHeader();

$records = $reader->getRecords($header);
foreach ($records as $offset => $record) {
    //var_dump($record);
    $old_name = $record['document_handler'];
    $new_name = str_replace(["/", " "], '-', $record['name']);
    //exit;
    if (!copy('/tmp/files/' . $old_name, '/tmp/files/newfiles/' . $new_name)) {
        echo "failed to copy " . $new_name;
    }
}
