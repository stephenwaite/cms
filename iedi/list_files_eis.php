<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('IEDI_USERNAME_EIS');
$cms_pass = getenv('IEDI_PASSWORD_EIS');
$sftp = new SFTP('ecgpe.healthtechnologygroup.com');
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

$path = 'MMIS/AllanEisemannERA_In';
//print_r($sftp->rawlist($path, true));

echo "listing inbound \n";
$rawlist = $sftp->rawlist($path, true);
//var_dump($rawlist);
if (!empty($rawlist)) {
    foreach ($rawlist as $file) {
        //var_dump($file);
        if (!empty($file)) {
            $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s a', $file->mtime));
            $date = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
            echo "file: " . $file->filename . " uploaded to iedi on " .
                $date->format('Y-m-d h:i:s a') . "\n";
        }
    }
} else {
    // there's a test directory
}

$path = 'MMIS/AllanEisemannERA_Out';
//print_r($sftp->rawlist($path, true));

echo "listing outbound \n";
$rawlist = $sftp->rawlist($path, true);
//var_dump($rawlist);
if (!empty($rawlist)) {
    foreach ($rawlist as $file) {
        //var_dump($file);
        if (!empty($file)) {
            $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s a', $file->mtime));
            $date = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
            echo "file: " . $file->filename . " created by iedi on " .
                $date->format('Y-m-d h:i:s a') . "\n";
        }
    }
} else {
    // there's a test directory
}

echo "end of list \n";
