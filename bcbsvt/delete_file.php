<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('MOVEIT_USERNAME');
$cms_pass = getenv('MOVEIT_PASSWORD');
$sftp = new SFTP('moveit.bcbsvt.com');
$sftp->login($cms_user, $cms_pass);

$remote_path = "/Home/cms/";
$line = readline("really want to delete " . $argv[1] . "? enter Y or y then");
if (trim($line) == "Y" || trim($line) == "y") {
    if ($sftp->delete($remote_path . $argv[1])) {
        echo "deleted \n";
        exit;
    }
}

echo "not deleting then \n";
