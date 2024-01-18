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
    if (!file_exists($file->filename)) {
        $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s', $file->mtime));
        echo "downloading " . $file->filename . " generated by 02 on " .
            $dt_utc->format('Y-m-d H:i:s T') . "\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
    } else {
        echo "already grabbed " . $file->filename . " generated by 02 on " .
            $dt_utc->format('Y-m-d H:i:s T') . "\n";
    }
}
