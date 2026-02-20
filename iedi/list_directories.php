<?php

declare(strict_types=1);

use phpseclib3\Net\SFTP;

require_once dirname(__FILE__) . '/../vendor/autoload.php';

// --- Configuration ---
const SFTP_HOST  = 'ecgpe.healthtechnologygroup.com';
const SFTP_ROOT  = '.';  // Start from home directory; change to '/' for absolute root

// --- Credentials ---
$cms_user = getenv('IEDI_USERNAME');
$cms_pass = getenv('IEDI_PASSWORD');

if (!$cms_user || !$cms_pass) {
    fwrite(STDERR, "Error: IEDI_USERNAME and/or IEDI_PASSWORD environment variables are not set.\n");
    exit(1);
}

// --- Connect & Authenticate ---
$sftp = new SFTP(SFTP_HOST);

if (!$sftp->login($cms_user, $cms_pass)) {
    fwrite(STDERR, "Error: SFTP login failed for user '{$cms_user}' on " . SFTP_HOST . "\n");
    exit(1);
}

// --- Allow an optional path argument from the command line ---
$startPath = $argv[1] ?? SFTP_ROOT;

echo "Listing directories on " . SFTP_HOST . " under: {$startPath}\n";
echo str_repeat('-', 60) . "\n";

// --- Recursive directory lister ---
function listRemoteDirs(SFTP $sftp, string $path, int $depth = 0): void
{
    $entries = $sftp->rawlist($path, true);

    if ($entries === false) {
        fwrite(STDERR, "Warning: Could not read path: {$path}\n");
        return;
    }

    foreach ($entries as $entry) {
    // phpseclib returns . and .. as plain arrays, not objects — skip them
        if (!is_object($entry)) {
            continue;
        }

        if ($entry->type === NET_SFTP_TYPE_DIRECTORY) {
            $indent   = str_repeat('  ', $depth);
            $fullPath = rtrim($path, '/') . '/' . $entry->filename;
            $modified = date('Y-m-d H:i', $entry->mtime);

            echo "{$indent}[DIR]  {$entry->filename}  (modified: {$modified})  {$fullPath}\n";

            listRemoteDirs($sftp, $fullPath, $depth + 1);
        }
    }
}

listRemoteDirs($sftp, $startPath);

echo str_repeat('-', 60) . "\n";
echo "Done.\n";
exit(0);
