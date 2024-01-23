<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('RRBERN_USERNAME');
$cms_pass = getenv('RRB_PASSWORD');
$sftp = new SFTP('edi.palmetto.ahdsxhub.com', '22');
$sftp->login($cms_user, $cms_pass);

$path = '/inbox/EZComm/BC/1.0/Notify';

if (file_exists('rrb.zip')) {
    echo "rrb.zip already exists, check it out\n";
    exit;
}

$zip = new ZipArchive();
$filename = "./rrb.zip";

if ($zip->open($filename, ZipArchive::CREATE)!==TRUE) {
    exit("cannot create <$filename>\n");
}

foreach(($sftp->rawlist($path, true)) as $file) {
        echo "downloading " . $file->filename . "\n";
        $sftp->get($path . "/" . $file->filename, $file->filename);
        echo "adding " . $file->filename . " to rrb.zip\n";
        $zip->addFile($file->filename);
}
echo "numfiles: " . $zip->numFiles . "\n";
echo "status: " . $zip->status . "\n";
$zip->close();