<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('NGS_USERNAME');
$cms_pass = getenv('NGS_PASSWORD');
$sftp = new SFTP('edi.ngs.ahdsxhub.com', '10062');
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "failed to login to ngs, exiting...\n";
    exit;
}

$path = 'current';

$rawlist = $sftp->rawlist($path, true);
if (!empty($rawlist)) {
    foreach($rawlist as $file) {
        if (!empty($file)) {
            echo "file: " . $file->filename . " created by ngs on " . $file->mtime . "\n";
        //        $dt_nyc->format('Y-m-d H:i:s T'). "\n";
        }
    }
} else {
    echo "no files on ngs \n";
}


try {
    $cms_user = getenv('NGS_RI_USERNAME');
    $cms_pass = getenv('NGS_RI_PASSWORD');
    $sftp = new SFTP('edi.ngs.ahdsxhub.com', '10062');
    if (!$sftp->login($cms_user, $cms_pass)) {
        echo "failed to login to ngsri, exiting... \n";
        exit;
    }
    $rawlist = $sftp->rawlist($path, true);
    if (!empty($rawlist)) {
        foreach($rawlist as $file) {
            if (!empty($file)) {
                echo "file: " . $file->filename . " created by ngsri on " . $file->mtime . "\n";
            //        $dt_nyc->format('Y-m-d H:i:s T'). "\n";
            }
        }
    } else {
        echo "no files on ngsri \n";
    }
} catch (Exception $e) {
        echo $e->getMessage() . "\n";
    }

echo "end of list \n";