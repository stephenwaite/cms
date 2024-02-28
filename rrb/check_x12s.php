<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$ans = readline("era, enter 1, or 277, enter anything else, ? \n");
$cms_user = ($ans == 1) ? getenv('RRBERN_USERNAME') : getenv('RRB_USERNAME');
$cms_pass = getenv('RRB_PASSWORD');
$sftp = new SFTP('edi.palmetto.ahdsxhub.com', '22');
$sftp->login($cms_user, $cms_pass);

$path = '/inbox/EZComm/BC/1.0/Notify';
//print_r($sftp->rawlist($path, true));

$rawlist = $sftp->rawlist($path, true);
//var_dump($rawlist);
if (!empty($rawlist)) {
    foreach($rawlist as $file) {
        //var_dump($file);
        if (!empty($file)) {
            $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s', $file->mtime));
            $dt_nyc = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
            echo "file: " . $file->filename . " uploaded to palmetto on " .
                $dt_nyc->format('Y-m-d h:i:s a'). "\n";
        }
    }
} else {
    // there's a test directory 
}

echo "end of list \n";