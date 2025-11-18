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
$remote_path = "/Home/cms/";
$line = readline("really want to rename? Y or y \n" . $argv[1] . "? enter Y or y then");
$newFileName = readline("enter new name please \n");
if (trim($line) == "Y" || trim($line) == "y") {
    if ($sftp->rename($remote_path . $argv[1], $remote_path . $newFileName)) {
        echo "renamed \n";
        exit;
    } else {
        echo "not renamed then \n";
    }
}

