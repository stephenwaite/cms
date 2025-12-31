<?php

require_once(dirname(__FILE__) . '/../vendor/autoload.php');
use phpseclib3\Net\SFTP;


$cms_user = getenv('TRIZETTO_USERNAME');
$cms_pass = getenv('TRIZETTO_PASSWORD');
$sftpUrl = getenv('SFTP_URL');
$sftp = new SFTP($sftpUrl);
$sftp->setTimeout(300); // 5 minutes instead of default
$sftp->setKeepAlive(30); // Send keep-alive every 30 seconds
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

// preserve date time
$sftp->enableDatePreservation();

$path = '/997';
//print_r($sftp->rawlist($path, true));

$rawlist = $sftp->rawlist($path, true);
if (!empty($rawlist)) {
    foreach ($rawlist as $file) {
        //var_dump($file);
        $fileName = $file->filename;
        $destination = $fileName;
        echo "downloading " . $fileName . " to " . $destination . "\n";
        $sftp->get($path . "/" . $fileName, $destination);
        /* if ($sftp->rename($path . "/" . $file->filename, '/WorkedTrans/' . $file->filename)) {
            echo "move file to trizetto's worked transactions dir " . $file->filename . "\n";
        } else {
            echo "huh, file already exists in the worked transactions dir\n";
        } */
    }
}
