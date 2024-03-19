<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('NGS_USERNAME');
$cms_pass = getenv('NGS_PASSWORD');
$sftp = new SFTP('edi.ngs.ahdsxhub.com', 10062);
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "failed to login to ngs, exiting...\n";
    exit;
}

try {
    $remote_file_path = "/" . $argv[1];
    $file_to_upload = $argv[1];
    $sftp->put($remote_file_path, $file_to_upload, SFTP::SOURCE_LOCAL_FILE);
    echo "file uploaded to ngs via sftp \n";
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}
