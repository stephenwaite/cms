<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('MOVEIT_USERNAME');
$cms_pass = getenv('MOVEIT_PASSWORD');
$sftp = new SFTP('moveit.bcbsvt.com');
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

$path = '/Home/cms';

$rawlist = $sftp->rawlist($path, true);
if (!empty($rawlist)) {
    foreach ($rawlist as $file) {
        if (!empty($file)) {
            $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s a', $file->mtime));
            $date = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
            $fileName = $file->filename;
            echo "file: " . $fileName . " uploaded to 02 on " .
                $date->format('Y-m-d h:i:s a') . "\n";
            if (stripos($fileName, 'txt') !== false) {
                echo "we have to change this filename to 00711##.x12 \n";
                changeFileName($fileName);
            }
        }
    }
} else {
    // there's a test directory
}

function changeFileName($fileName) {
    $moveitFilename = '007111' . rand(20, 99) . '.x12';
    if (!checkFileNameExists($moveitFilename)) {
        echo "file name doesn't exist so we can rename it \n";
        //rename($fileName, $moveitFilename);
    } else {
        echo "file name does exists so we have to try changing the file name";
        //changeFileName($fileName);
    }
   
}

function checkFileNameExists($moveitFilename) {
    if (file_exists($moveitFilename)) {
        return true;
    }
    return false;
}