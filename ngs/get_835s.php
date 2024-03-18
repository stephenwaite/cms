<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('NGS_USERNAME');
$cms_pass = getenv('NGS_PASSWORD');
$sftp = new SFTP('edi.ngs.ahdsxhub.com', '10062');

if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed, maybe wg0 is down or password expired?" . "\n";
    exit;
};

$path = 'current';

if (file_exists('ngs.zip')) {
    echo "ngs.zip already exists, check it out\n";
    exit;
}

$zip = new ZipArchive();
$filename = "./ngs.zip";

if ($zip->open($filename, ZipArchive::CREATE)!==TRUE) {
    exit("cannot create <$filename>\n");
}

foreach(($sftp->rawlist($path, true)) as $file) {
        echo "downloading " . $file->filename . "\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
        echo "adding " . $file->filename . " to ngs.zip\n";
        $zip->addFile($file->filename);
}

$cms_user = getenv('NGS_RI_USERNAME');
$cms_pass = getenv('NGS_RI_PASSWORD');
$sftp = new SFTP('edi.ngs.ahdsxhub.com', '10062');

if (!$sftp->login($cms_user, $cms_pass)) {
    echo "ngs ri login failed, maybe wg0 is down or password expired?, exiting" . "\n";
    exit;
} else {
    foreach(($sftp->rawlist($path, true)) as $file) {
        echo "downloading " . $file->filename . "\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
        echo "adding " . $file->filename . " to ngs.zip\n";
        $zip->addFile($file->filename);
    }
};

echo "numfiles: " . $zip->numFiles . "\n";
echo "status: " . $zip->status . "\n";
$zip->close();