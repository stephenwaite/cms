<?php
require_once(dirname(__FILE__) . '/../vendor/autoload.php');

use CMS\Utility\CmsSftp;

$cms_user = getenv('NGS_USERNAME');
$cms_pass = getenv('NGS_PASSWORD');
$sftp = new CmsSftp(
    'edi.ngs.ahdsxhub.com',
    10062,
    $cms_user,
    $cms_pass,
    '',
    'current'
);

var_dump($sftp);

if (!$sftp->login()) {
    echo "login failed, maybe wg0 is down or password expired?" . "\n";
    exit;
};

$rawlist = $sftp->rawlist();
var_dump($rawlist);
exit;
if (!empty($rawlist)) {
    foreach($rawlist as $file) {
        if (!empty($file)) {
            $dt_utc = new DateTimeImmutable(date('Y-m-d H:i:s', $file->mtime));
            $date = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
            echo "file: " . $file->filename . " created by ngs on " . $date->format('Y-m-d H:i:s') . "\n";
        //        $dt_nyc->format('Y-m-d H:i:s T'). "\n";
        }
    }
} else {
    echo "no files on ngs \n";
}