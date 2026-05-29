<?php

use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');


$cms_user = getenv('MOVEIT_USERNAME');
$cms_pass = getenv('MOVEIT_PASSWORD');
$sftp = new SFTP('moveit.bcbsvt.com');
$sftp->setTimeout(300); // 5 minutes instead of default
$sftp->setKeepAlive(30); // Send keep-alive every 30 seconds
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

$path = '/Home/cms';
$files = $sftp->rawlist($path);   
foreach ($raw as $name => $entry) {
    var_dump($name, $entry);
    break;
}
if (!is_array($files)) {
    throw new RuntimeException("rawlist failed: " . $sftp->getLastError());
}

$files = array_filter($files, function ($f, $name) {
    return $name !== '.' && $name !== '..' && $f->type !== NET_SFTP_TYPE_DIRECTORY;
}, ARRAY_FILTER_USE_BOTH);

usort($files, fn($a, $b) => $a->mtime <=> $b->mtime);

foreach ($files as $file) {
    $dt_utc = new DateTimeImmutable('@' . $file->mtime);
    $date = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
    echo "file: {$file->filename} uploaded to 02 on "
       . $date->format('Y-m-d h:i:s a') . "\n";
}
