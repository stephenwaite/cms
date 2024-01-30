<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('MOVEIT_USERNAME');
$cms_pass = getenv('MOVEIT_PASSWORD');
$sftp = new SFTP('moveit.bcbsvt.com');
$sftp->login($cms_user, $cms_pass);

$path = '/Distribution/cms-reports/Prod/ErrorReports';
//print_r($sftp->rawlist($path, true));

foreach (($sftp->rawlist($path, true)) as $file) {
    //var_dump($file);
    $date_utc = new DateTimeImmutable(date('Y-m-d H:i:s', $file->mtime));
    $date = $date_utc->setTimezone(new DateTimeZone('America/New_York'));
    //var_dump($date);
    if (!file_exists($file->filename)) {
        echo "*** downloading " . $file->filename . " generated by 02 on " .
            $date->format('Y-m-d h:i:s a') . " ***\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
    } else {
        echo "already grabbed " . $file->filename . " generated by 02 on " .
            $date->format('Y-m-d h:i:s a') . "\n";
    }
}
