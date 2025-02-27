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
            // sftp needs to have the full dir
            $fileName = "/Home/cms/" . $file->filename;
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

function changeFileName($fileName)
{
    $moveitFilename = "/Home/cms/" . "007111" . rand(10, 99) . ".x12";
    if (!checkFileNameExists($moveitFilename)) {
        echo "file name $moveitFilename doesn't exist so we can rename it \n";
        if (rename($fileName, $moveitFilename)) {
            echo "changed name successfully \n";
        } else {
            echo "changed name unsuccessfully \n";
        }
    } else {
        echo "file name does exists so we have to try changing the file name";
        //changeFileName($fileName);
    }
}

function checkFileNameExists($moveitFilename)
{
    if (file_exists($moveitFilename)) {
        return true;
    }
    return false;
}
