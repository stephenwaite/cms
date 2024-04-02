<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('IEDI_USERNAME');
$cms_pass = getenv('IEDI_PASSWORD');
$sftp = new SFTP('ecgpe.healthtechnologygroup.com');
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

// preserve date time
$sftp->enableDatePreservation();

$path = 'out/Care_Manag';
//print_r($sftp->rawlist($path, true));

foreach (($sftp->rawlist($path, true)) as $file) {
    //var_dump($file);
    echo "downloading " . $file->filename . "\n";
    $sftp->get($path . "/" . $file->filename, $file->filename);
    /* if ($sftp->rename($path . "/" . $file->filename, '/tmp/' . $file->filename)) {
        echo "move file to iedi's tmp dir " . $file->filename . "\n";
    } else {
        echo "huh, file already exists in iedi server in tmp dir\n";
    } */
}
