<?php

/**
 * Uploads a claim file to Vermont Medicaid (04) through the Upload Files page of
 * vtmedicaid.com. Gainwell takes claim files there, not over SFTP.
 *
 * usage: php upl_004.php <file>      send the file
 *        php upl_004.php --check     sign in and find the upload page, send nothing
 *
 * The login comes from VT_MEDICAID_USER and VT_MEDICAID_PASS. On success the last line
 * is "SUCCESS - Tracking: <number>" and the exit status is 0; anything else exits 1.
 * Each run adds to vtmedicaid_upload_<yyyymmdd>.log in the current directory.
 *
 * This posts the file and reads the tracking number off the confirmation page. It
 * replaces python/vtmed_upl_from_server.py, which drove a browser to do it.
 *
 * "Uploaded successfully" means the portal has the file, not that it accepted the claims:
 * the acknowledgment arrives on the portal's download page later (dnl_004.php).
 */

use GuzzleHttp\Exception\GuzzleException;

require_once(dirname(__FILE__) . '/portal.php');

const UPLOAD_PATH = '/secure/translator.do';
const CONFIRMATION_MARK = 'Uploaded successfully';

// The portal's limit for one upload.
const MAX_BYTES = 16 * 1024 * 1024;

const CHECK_FIRST = 'Look for it on the portal before sending this file again.';

if ($argc !== 2) {
    fwrite(STDERR, "usage: php upl_004.php <file> | --check\n");
    exit(1);
}
$check_only = $argv[1] === '--check';
$file = $argv[1];

if ((string) getenv('VT_MEDICAID_USER') === '' || (string) getenv('VT_MEDICAID_PASS') === '') {
    fail('Missing login. Set VT_MEDICAID_USER and VT_MEDICAID_PASS.');
}
if (!$check_only) {
    if (!is_file($file) || !is_readable($file)) {
        fail('File not found or not readable: ' . $file);
    }
    $size = filesize($file);
    if ($size === 0 || $size === false) {
        fail('File is empty: ' . $file);
    }
    if ($size > MAX_BYTES) {
        fail('File is over the portal\'s 16 MB limit: ' . $file);
    }
    note('Starting upload of ' . realpath($file) . ' (' . $size . ' bytes)');
}

$http = portal_client();
sign_in($http, 'Nothing was sent.');
if ($check_only) {
    echo "Signed in; the Upload Files page is there. Nothing was sent.\n";
    exit(0);
}

try {
    $response = $http->request('POST', UPLOAD_PATH, [
        'multipart' => [
            ['name' => 'screen_id', 'contents' => 'Upload'],
            ['name' => 'directory', 'contents' => ''],
            ['name' => 'dispopts', 'contents' => 'html, gif, jpg, other;;*'],
            ['name' => 'userfile1', 'contents' => (string) file_get_contents($file), 'filename' => basename($file)],
            ['name' => 'action', 'contents' => 'Upload Files'],
            ['name' => 'op-upload', 'contents' => ''],
        ],
    ]);
} catch (GuzzleException $e) {
    // The file may or may not have arrived.
    fail('The connection to the portal failed during the upload (' . reason($e) . '). ' . CHECK_FIRST);
}
$page = (string) $response->getBody();
if (!str_contains($page, CONFIRMATION_MARK)) {
    fail('The portal did not confirm the upload (HTTP ' . $response->getStatusCode() . '). ' . CHECK_FIRST);
}
if (preg_match('/Tracking Number:\s*(\d+)/', strip_tags($page), $match) !== 1) {
    // The portal has the file; sending it again would duplicate it.
    note('Uploaded; the portal gave no tracking number.');
    echo "SUCCESS - uploaded, but the portal gave no tracking number. Do not send it again; check the portal.\n";
    exit(0);
}

note('SUCCESS - Tracking Number: ' . $match[1]);
echo 'SUCCESS - Tracking: ' . $match[1] . "\n";
