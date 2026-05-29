<?php
use phpseclib3\Net\SFTP;
require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_user = getenv('MOVEIT_USERNAME');
$cms_pass = getenv('MOVEIT_PASSWORD');

$sftp = new SFTP('moveit.bcbsvt.com');
$sftp->setTimeout(300);    // 5 minutes instead of default
$sftp->setKeepAlive(30);   // send keep-alive every 30 seconds

if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed\n";
    exit;
}

$path = '/Home/cms';
$raw = $sftp->rawlist($path);
if (!is_array($raw)) {
    throw new RuntimeException("rawlist failed: " . $sftp->getLastError());
}

// read a field whether the entry is an object or an associative array
$prop = function ($entry, $key) {
    if (is_object($entry)) return $entry->$key ?? null;
    if (is_array($entry))  return $entry[$key] ?? null;
    return null;
};

$files = [];
foreach ($raw as $name => $entry) {
    if ($name === '.' || $name === '..') continue;
    if ($prop($entry, 'type') === NET_SFTP_TYPE_DIRECTORY) continue;  // 2
    $mtime = $prop($entry, 'mtime');
    if (!$mtime) continue;
    $files[] = [
        'filename' => $prop($entry, 'filename') ?? $name,
        'mtime'    => $mtime,
    ];
}

usort($files, fn($a, $b) => $a['mtime'] <=> $b['mtime']);

foreach ($files as $file) {
    $dt_utc = new DateTimeImmutable('@' . $file['mtime']);
    $date = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
    echo "file: {$file['filename']} uploaded to 02 on "
       . $date->format('Y-m-d h:i:s a') . "\n";
}