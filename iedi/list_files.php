<?php

declare(strict_types=1);

use phpseclib3\Net\SFTP;

require_once dirname(__FILE__) . '/../vendor/autoload.php';

$cms_user = getenv('IEDI_USERNAME');
$cms_pass = getenv('IEDI_PASSWORD');

if (!$cms_user || !$cms_pass) {
    fwrite(STDERR, "Error: IEDI_USERNAME and/or IEDI_PASSWORD environment variables are not set.\n");
    exit(1);
}

$sftp = new SFTP('ecgpe.healthtechnologygroup.com');
$sftp->setTimeout(300);
$sftp->setKeepAlive(30);

if (!$sftp->login($cms_user, $cms_pass)) {
    fwrite(STDERR, "Error: login failed\n");
    exit(1);
}

function listFiles(SFTP $sftp, string $path, string $label): void
{
    echo "Listing {$label}: {$path}\n";

    $rawlist = $sftp->rawlist($path, false);

    if ($rawlist === false || empty($rawlist)) {
        echo "  (no files or path unreadable)\n";
        return;
    }

    foreach ($rawlist as $filename => $file) {
        if (in_array($filename, ['.', '..'], true)) {
            continue;
        }

        // Skip subdirectories — files only
        if ($file['type'] === NET_SFTP_TYPE_DIRECTORY) {
            continue;
        }

        $dt_utc = new DateTimeImmutable(date('Y-m-d H:i:s', $file['mtime']));
        $date   = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));

        echo "  file: {$filename}  size: {$file['size']}  modified: " . $date->format('Y-m-d h:i:s a') . "\n";
    }
}

listFiles($sftp, 'E_ZCHC0409/In', 'inbound');
listFiles($sftp, 'out/Care_Manag', 'outbound');

echo "Done.\n";