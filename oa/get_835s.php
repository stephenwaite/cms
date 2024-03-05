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

foreach(($sftp->rawlist($path, true)) as $file) {
    //var_dump($file);
    if (!file_exists($file->filename)) {
        echo "downloading " . $file->filename . "\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
    } else {
        echo "already grabbed " . $file->filename . " generated by oa on " .
            date('l jS \of F Y h:i:s A',$file->mtime) . "\n";
    }
}