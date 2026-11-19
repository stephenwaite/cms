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
 * The portal's forms are plain posts (no script, token or one-time code), so this signs
 * in as a person does, posts the file and reads the tracking number off the confirmation
 * page. It is the same exchange OpenEMR's CMS module makes (Billing/VtMedicaidPortal),
 * and replaces python/vtmed_upl_from_server.py, which drove a browser to do it.
 *
 * "Uploaded successfully" means the portal has the file, not that it accepted the claims:
 * the acknowledgment arrives on the portal's download page later.
 */

use GuzzleHttp\Client;
use GuzzleHttp\Cookie\CookieJar;
use GuzzleHttp\Exception\GuzzleException;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

const LOGON_PATH = '/secure/logon.do';
const UPLOAD_FORM_PATH = '/secure/upload.do';
const UPLOAD_PATH = '/secure/translator.do';

// The upload page's form; the logon page comes back instead when the sign-in failed.
const UPLOAD_FORM_MARK = 'name="WebUploadForm"';
const CONFIRMATION_MARK = 'Uploaded successfully';

// The portal answers HTTP/1.1 and then drops the connection without closing it, which
// OpenSSL 3 reports as an error ("unexpected eof while reading"); over HTTP/2, as a
// browser speaks to it, it answers properly.
const HTTP_VERSION = '2.0';

// The portal's limit for one upload.
const MAX_BYTES = 16 * 1024 * 1024;

const CHECK_FIRST = 'Look for it on the portal before sending this file again.';

function note(string $message): void
{
    file_put_contents(
        'vtmedicaid_upload_' . date('Ymd') . '.log',
        date('Y-m-d H:i:s') . ' - ' . $message . "\n",
        FILE_APPEND
    );
}

function fail(string $message): never
{
    note('ERROR - ' . $message);
    fwrite(STDERR, $message . "\n");
    exit(1);
}

/**
 * What the HTTP client says went wrong, without the library's documentation link. The
 * login travels in the request body, never the URL, so the message holds no credentials.
 */
function reason(GuzzleException $e): string
{
    $message = preg_replace('/\s*\(see https?:[^)]*\)/', '', $e->getMessage()) ?? $e->getMessage();

    return substr(trim($message), 0, 200);
}

/**
 * Sign in and open the upload page. Exits when the page is not there.
 */
function sign_in(Client $http, CookieJar $cookies, string $logon_id, string $password): void
{
    try {
        $logon = $http->request('POST', LOGON_PATH, [
            'cookies' => $cookies,
            'form_params' => [
                'screen_id' => 'logon.jsp',
                'logon_id' => $logon_id,
                'password' => $password,
                'action' => 'Log In',
            ],
        ]);
        if ($logon->getStatusCode() >= 400) {
            fail('The portal answered the sign-in with HTTP ' . $logon->getStatusCode() . '. Nothing was sent.');
        }
        $form = $http->request('GET', UPLOAD_FORM_PATH, ['cookies' => $cookies]);
    } catch (GuzzleException $e) {
        fail('Could not reach the Vermont Medicaid portal (' . reason($e) . '). Nothing was sent.');
    }
    if (!str_contains((string) $form->getBody(), UPLOAD_FORM_MARK)) {
        fail('The portal did not show its Upload Files page after signing in (HTTP ' . $form->getStatusCode()
            . '): the login or password is wrong, or the portal has changed. Nothing was sent.');
    }
    note('Signed in; the Upload Files page is there.');
}

if ($argc !== 2) {
    fwrite(STDERR, "usage: php upl_004.php <file> | --check\n");
    exit(1);
}
$check_only = $argv[1] === '--check';
$file = $argv[1];

$logon_id = (string) getenv('VT_MEDICAID_USER');
$password = (string) getenv('VT_MEDICAID_PASS');
if ($logon_id === '' || $password === '') {
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

// VT_MEDICAID_URL is for testing against a stand-in; leave it unset in production.
$http = new Client([
    'base_uri' => getenv('VT_MEDICAID_URL') ?: 'https://www.vtmedicaid.com',
    'http_errors' => false,
    'version' => HTTP_VERSION,
    'connect_timeout' => 30,
    'timeout' => 120,
]);
$cookies = new CookieJar();

sign_in($http, $cookies, $logon_id, $password);
if ($check_only) {
    echo "Signed in; the Upload Files page is there. Nothing was sent.\n";
    exit(0);
}

try {
    $response = $http->request('POST', UPLOAD_PATH, [
        'cookies' => $cookies,
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
