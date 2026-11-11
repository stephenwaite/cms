<?php

require_once(dirname(__FILE__) . '/../vendor/autoload.php');
use phpseclib3\Net\SFTP;

// Enable this script via environment variable
if (!getenv('OPENEMR_ENABLE_TRIZETTO_SFTP')) {
    die('Set OPENEMR_ENABLE_TRIZETTO_SFTP=1 environment variable to enable this script');
}

$cms_user = getenv('TRIZETTO_USERNAME');
$cms_pass = getenv('TRIZETTO_PASSWORD');
$sftpUrl = getenv('SFTP_URL');
$sftp = new SFTP($sftpUrl);
$sftp->setTimeout(300); // 5 minutes instead of default
$sftp->setKeepAlive(30); // Send keep-alive every 30 seconds
if (!$sftp->login($cms_user, $cms_pass)) {
    echo "login failed" . "\n";
    exit;
};

// preserve date time
$sftp->enableDatePreservation();

$path = '/remits';
//print_r($sftp->rawlist($path, true));

// Only move a remit to WorkedTrans once it's here in full. A remit that didn't come down
// stays in /remits, so the next run tries it again.
$rawlist = $sftp->rawlist($path);
if ($rawlist === false) {
    echo "couldn't list " . $path . "\n";
    exit;
}
foreach ($rawlist as $fileName => $file) {
    // skip . and .. and anything else that isn't a plain file
    if (($file['type'] ?? null) !== NET_SFTP_TYPE_REGULAR) {
        continue;
    }
    $destination = $fileName;
    // download under a hidden temporary name, so a partial file never sits under the remit's name
    $partial = '.' . $fileName . '.part';
    echo "downloading " . $fileName . " to " . $destination . "\n";
    try {
        $downloaded = $sftp->get($path . "/" . $fileName, $partial);
    } catch (\RuntimeException $e) {
        echo "download error for " . $fileName . ": " . get_class($e) . "\n";
        $downloaded = false;
    }
    // get() can return true for a transfer the server cut short, so compare sizes too
    clearstatcache();
    $localSize = is_file($partial) ? filesize($partial) : false;
    $remoteSize = $file['size'] ?? null;
    if ($downloaded === false || $localSize === false || ($remoteSize !== null && $localSize !== $remoteSize)) {
        echo "NOT archived, didn't download in full: " . $fileName
            . " (got " . var_export($localSize, true) . " of " . var_export($remoteSize, true) . " bytes)\n";
        if (is_file($partial)) {
            unlink($partial);
        }
        continue;
    }
    if (!rename($partial, $destination)) {
        echo "NOT archived, couldn't save " . $fileName . " as " . $destination . "\n";
        unlink($partial);
        continue;
    }
    if ($sftp->rename($path . "/" . $fileName, '/WorkedTrans/' . $fileName)) {
        echo "move file to trizetto's worked transactions dir " . $fileName . "\n";
    } else {
        echo "huh, file already exists in the worked transactions dir\n";
    }
}
