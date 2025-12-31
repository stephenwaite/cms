<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

try {
    $cms_user = getenv('IEDI_USERNAME');
    $cms_pass = getenv('IEDI_PASSWORD');
    $sftp = new SFTP('ecgpe.healthtechnologygroup.com');
    $sftp->setTimeout(300); // 5 minutes instead of default
    $sftp->setKeepAlive(30); // Send keep-alive every 30 seconds

    if (!$sftp->login($cms_user, $cms_pass)) {
        echo "login failed" . "\n";
        exit;
    };

    $date = new DateTimeImmutable("now", new DateTimeZone("America/New_York"));
    $stamp = $date->format('YmdHis');
    $filename = $argv[1];
    $remote_filename = $filename . "-" . $stamp;
    copy($filename, $remote_filename);
    $remote_file_path = "E_ZCHC0409/In/" . $remote_filename;
    $file_to_upload = $filename;
    $sftp->put($remote_file_path, $file_to_upload, SFTP::SOURCE_LOCAL_FILE);
    echo "file uploaded to iedi via sftp \n";
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}
