<?php

/**
 * What upl_004.php and dnl_004.php share: the connection to vtmedicaid.com, the sign-in
 * and the dated log file.
 *
 * The portal's forms are plain posts (no script, token or one-time code), so these sign
 * in as a person does. It is the same exchange OpenEMR's CMS module makes
 * (Billing/VtMedicaidPortal).
 */

use GuzzleHttp\Client;
use GuzzleHttp\Cookie\CookieJar;
use GuzzleHttp\Exception\GuzzleException;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

const PORTAL_URL = 'https://www.vtmedicaid.com';
const LOGON_PATH = '/secure/logon.do';
const UPLOAD_FORM_PATH = '/secure/upload.do';

// The upload page's form; the logon page comes back instead when the sign-in failed.
const UPLOAD_FORM_MARK = 'name="WebUploadForm"';

// The portal answers HTTP/1.1 and then drops the connection without closing it, which
// OpenSSL 3 reports as an error ("unexpected eof while reading"); over HTTP/2, as a
// browser speaks to it, it answers properly.
const HTTP_VERSION = '2.0';

/**
 * Add a line to vtmedicaid_<upload|download>_<yyyymmdd>.log in the current directory.
 * The first call of a run names the log: note('...', 'download').
 */
function note(string $message, ?string $log = null): void
{
    static $name = 'upload';
    $name = $log ?? $name;
    file_put_contents(
        'vtmedicaid_' . $name . '_' . date('Ymd') . '.log',
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
 * VT_MEDICAID_URL is for testing against a stand-in; leave it unset in production.
 */
function portal_url(): string
{
    return getenv('VT_MEDICAID_URL') ?: PORTAL_URL;
}

/**
 * A client that keeps the session's cookies across requests.
 */
function portal_client(): Client
{
    return new Client([
        'base_uri' => portal_url(),
        'cookies' => new CookieJar(),
        'http_errors' => false,
        'version' => HTTP_VERSION,
        'connect_timeout' => 30,
        'timeout' => 120,
    ]);
}

/**
 * Sign in with VT_MEDICAID_USER / VT_MEDICAID_PASS and open the upload page, which only
 * a signed-in session gets. Exits when the page is not there. $nothing says what did not
 * happen, for the message: "Nothing was sent."
 */
function sign_in(Client $http, string $nothing): void
{
    $logon_id = (string) getenv('VT_MEDICAID_USER');
    $password = (string) getenv('VT_MEDICAID_PASS');
    if ($logon_id === '' || $password === '') {
        fail('Missing login. Set VT_MEDICAID_USER and VT_MEDICAID_PASS.');
    }
    try {
        $logon = $http->request('POST', LOGON_PATH, [
            'form_params' => [
                'screen_id' => 'logon.jsp',
                'logon_id' => $logon_id,
                'password' => $password,
                'action' => 'Log In',
            ],
        ]);
        if ($logon->getStatusCode() >= 400) {
            fail('The portal answered the sign-in with HTTP ' . $logon->getStatusCode() . '. ' . $nothing);
        }
        $form = $http->request('GET', UPLOAD_FORM_PATH);
    } catch (GuzzleException $e) {
        fail('Could not reach the Vermont Medicaid portal (' . reason($e) . '). ' . $nothing);
    }
    if (!str_contains((string) $form->getBody(), UPLOAD_FORM_MARK)) {
        fail('The portal did not show its Upload Files page after signing in (HTTP ' . $form->getStatusCode()
            . '): the login or password is wrong, or the portal has changed. ' . $nothing);
    }
    note('Signed in; the Upload Files page is there.');
}
