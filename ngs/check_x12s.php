<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('NGS_USERNAME');
$cms_pass = getenv('NGS_PASSWORD');
$sftp = new SFTP('edi.ngs.ahdsxhub.com', '10062');
$sftp->login($cms_user, $cms_pass);

$path = 'current';
//print_r($sftp->rawlist($path, true));

$rawlist = $sftp->rawlist($path, true);
//var_dump($rawlist);
if (!empty($rawlist)) {
    foreach($rawlist as $file) {
        //var_dump($file);
        if (!empty($file)) {
            $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s', $file->mtime));
            $dt_nyc = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
            echo "file: " . $file->filename . " uploaded to ngs on " .
                $dt_nyc->format('Y-m-d H:i:s T'). "\n";
        }
    }
} else {
    // there's a test directory 
}

echo "end of list \n";