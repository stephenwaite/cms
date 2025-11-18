<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

try {
    $cms_user = getenv('IEDI_USERNAME_EIS');
    $cms_pass = getenv('IEDI_PASSWORD_EIS');
    $sftp = new SFTP('ecgpe.healthtechnologygroup.com');

    if (!$sftp->login($cms_user, $cms_pass)) {
        echo "login failed" . "\n";
        exit;
    };

    $remote_file_path = "MMIS/AllanEisemannERA_In/" . $argv[1];
    $file_to_upload = $argv[1];
    $sftp->put($remote_file_path, $file_to_upload, SFTP::SOURCE_LOCAL_FILE);
    echo "file uploaded to iedi via sftp \n";
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}
