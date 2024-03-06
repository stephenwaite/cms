<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('OA_USERNAME');
$cms_pass = getenv('OA_PASSWORD');
$sftp = new SFTP('ftp10.officeally.com');
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

// preserve date time
$sftp->enableDatePreservation();

$path = '/outbound';
//print_r($sftp->rawlist($path, true));

foreach (($sftp->rawlist($path, true)) as $file) {
    //var_dump($file);
    echo "downloading " . $file->filename . "\n";
    $sftp->get($path . "/" . $file->filename, $file->filename);
    if ($sftp->rename($path . "/" . $file->filename, '/tmp/' . $file->filename)) {
        echo "move file to oa's tmp dir " . $file->filename . "\n";
    } else {
        echo "huh, file already exists in oa server in tmp dir\n";
    }
}
