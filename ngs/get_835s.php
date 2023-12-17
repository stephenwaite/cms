<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('NGS_USERNAME');
$cms_pass = getenv('NGS_PASSWORD');
$sftp = new SFTP('edi.ngs.ahdsxhub.com', '10062');
$sftp->login($cms_user, $cms_pass);

$path = 'current';
//print_r($sftp->rawlist($path, true));

$zip = new ZipArchive();
$filename = "./ngs.zip";

if ($zip->open($filename, ZipArchive::OVERWRITE)!==TRUE) {
    exit("cannot overwrite <$filename>\n");
}

foreach(($sftp->rawlist($path, true)) as $file) {
        echo "downloading " . $file->filename . "\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
        echo "adding " . $file->filename . " to ngs.zip\n";
        $zip->addFile($file->filename);
}
echo "numfiles: " . $zip->numFiles . "\n";
echo "status: " . $zip->status . "\n";
$zip->close();