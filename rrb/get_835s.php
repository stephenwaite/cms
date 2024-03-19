<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

// first create zip file to store downloaded files
if (file_exists('rrb.zip')) {
    echo "rrb.zip already exists, check it out\n";
    exit;
}

$zip = new ZipArchive();
$filename = "./rrb.zip";

if ($zip->open($filename, ZipArchive::CREATE) !== true) {
    exit("cannot create <$filename>\n");
}

try {
    echo "looking for rrb era" . "\n";
    $cms_user = getenv('RRBERN_USERNAME');
    $cms_pass = getenv('RRB_PASSWORD');
    $sftp = new SFTP('edi.palmetto.ahdsxhub.com', '22');

    if (!$sftp->login($cms_user, $cms_pass)) {
        echo "login failed, maybe wg0 is down or rrb password expired?" . "\n";
        exit;
    };

    $path = '/inbox/EZComm/BC/1.0/Notify';

    //print_r($sftp->rawlist($path, true));

    foreach (($sftp->rawlist($path, true)) as $file) {
        //var_dump($file);
        if (!file_exists($file->filename)) {
            echo "downloading " . $file->filename . "\n";
            $sftp->get($path . "/" . $file->filename, $file->filename);
            echo "adding " . $file->filename . " to rrb.zip\n";
            $zip->addFile($file->filename);
        }
    }
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}

try {
    echo "looking for rrb 277 999" . "\n";
    $cms_user = getenv('RRB_USERNAME');
    $cms_pass = getenv('RRB_PASSWORD');
    $sftp = new SFTP('edi.palmetto.ahdsxhub.com', '22');

    if (!$sftp->login($cms_user, $cms_pass)) {
        echo "login failed, maybe wg0 is down or rrb password expired?" . "\n";
        exit;
    };

    $path = '/inbox/EZComm/BC/1.0/Notify';

    //print_r($sftp->rawlist($path, true));

    foreach (($sftp->rawlist($path, true)) as $file) {
        //var_dump($file);
        if (!file_exists($file->filename)) {
            echo "downloading " . $file->filename . "\n";
            $sftp->get($path . "/" . $file->filename, $file->filename);
            echo "adding " . $file->filename . " to rrb.zip\n";
            $zip->addFile($file->filename);
        }
    }
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}

echo "numfiles: " . $zip->numFiles . "\n";
echo "status: " . $zip->status . "\n";
$zip->close();
