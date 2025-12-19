<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

try {
    $cms_user = getenv('MOVEIT_USERNAME');
    $cms_pass = getenv('MOVEIT_PASSWORD');
    $sftp = new SFTP('moveit.bcbsvt.com');
    $sftp->setTimeout(300); // 5 minutes instead of default
    $sftp->setKeepAlive(30); // Send keep-alive every 30 seconds
    if (!$sftp->login($cms_user, $cms_pass)) {
        echo "login failed" . "\n";
        exit;
    };
    
    $remote_file_path = "/Home/cms/" . $argv[2];
    $file_to_upload = $argv[1];
    $sftp->put($remote_file_path, $file_to_upload, SFTP::SOURCE_LOCAL_FILE);
    echo "file uploaded to moveit via sftp \n";
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}
