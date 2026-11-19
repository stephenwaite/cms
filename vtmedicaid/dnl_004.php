<?php

/**
 * Downloads the files Vermont Medicaid (04) has waiting on vtmedicaid.com: every file on
 * the portal's download page with no download date yet.
 *
 * usage: php dnl_004.php [directory]   download them
 *        php dnl_004.php --list        show what is waiting, download nothing
 *
 * The login comes from VT_MEDICAID_USER and VT_MEDICAID_PASS. Files go to the directory
 * given, else VT_MEDICAID_DOWNLOAD_DIR, else ./vtmedicaid_downloads. Exit status 0 when
 * every waiting file was saved (or none was waiting), 1 otherwise. Each run adds to
 * vtmedicaid_download_<yyyymmdd>.log in the current directory.
 *
 * The portal stamps a file as downloaded when it is asked for, so --list is the way to
 * look without changing anything, and a file that fails here has to be fetched by hand:
 * the next run will not see it as waiting.
 *
 * Replaces python/vtmed_dnl_to_server.py, which drove a browser to do it.
 */

use GuzzleHttp\Client;
use GuzzleHttp\Exception\GuzzleException;
use GuzzleHttp\Psr7\Uri;
use GuzzleHttp\Psr7\UriResolver;

require_once(dirname(__FILE__) . '/portal.php');

const SEARCH_PATH = '/secure/translator.do?action=Search';

// The page lists a few files unless asked for more; 50 is what the browser version chose.
const COUNT_FIELD = 'searchFileCount';
const COUNT = '50';

const BY_HAND = 'Get it from the portal by hand: it may now show as downloaded there.';

function load_page(string $html): DOMXPath
{
    $doc = new DOMDocument();
    @$doc->loadHTML($html);

    return new DOMXPath($doc);
}

function resolve(string $page_url, string $href): string
{
    return (string) UriResolver::resolve(new Uri($page_url), new Uri($href));
}

/**
 * The page again with 50 files listed: fills in the form that holds the count field as
 * a browser would send it and submits it. Null when the page has no such form.
 */
function list_fifty(Client $http, DOMXPath $page, string $page_url): ?string
{
    $form = $page->query('//form[.//select[@name="' . COUNT_FIELD . '"]]')->item(0);
    if (!$form instanceof DOMElement) {
        return null;
    }
    $fields = [];
    $submitted = false;
    foreach ($page->query('.//input[@name] | .//select[@name] | .//textarea[@name]', $form) as $field) {
        $name = $field->getAttribute('name');
        if ($field->hasAttribute('disabled')) {
            continue;
        }
        if ($field->nodeName === 'select') {
            $option = $page->query('.//option[@selected]', $field)->item(0) ?? $page->query('.//option', $field)->item(0);
            $fields[$name] = $option instanceof DOMElement
                ? ($option->hasAttribute('value') ? $option->getAttribute('value') : trim($option->textContent))
                : '';
            continue;
        }
        if ($field->nodeName === 'textarea') {
            $fields[$name] = $field->textContent;
            continue;
        }
        $type = strtolower($field->getAttribute('type'));
        if (in_array($type, ['button', 'reset', 'file', 'image'], true)) {
            continue;
        }
        if (in_array($type, ['checkbox', 'radio'], true) && !$field->hasAttribute('checked')) {
            continue;
        }
        if ($type === 'submit') {
            // Only the button that was pressed is sent: the first one, as the browser version clicked.
            if ($submitted) {
                continue;
            }
            $submitted = true;
        }
        $fields[$name] = $field->getAttribute('value');
    }
    $fields[COUNT_FIELD] = COUNT;

    $post = strtolower($form->getAttribute('method')) === 'post';
    $action = $form->getAttribute('action');
    $response = $http->request(
        $post ? 'POST' : 'GET',
        $action === '' ? $page_url : resolve($page_url, $action),
        [$post ? 'form_params' : 'query' => $fields]
    );

    return $response->getStatusCode() < 400 ? (string) $response->getBody() : null;
}

/**
 * The listed files with no download date: rows of file (a link), type, created,
 * downloaded, size.
 *
 * @return list<array{name: string, type: string, created: string, size: string, url: string}>
 */
function waiting_files(DOMXPath $page, string $page_url): array
{
    $waiting = [];
    foreach ($page->query('//table//tr') as $row) {
        $cells = $page->query('./td', $row);
        if ($cells->length < 5) {
            continue;
        }
        $link = $page->query('.//a[@href]', $cells->item(0))->item(0);
        if (!$link instanceof DOMElement) {
            continue;
        }
        $text = static fn(int $i): string => trim(str_replace("\u{a0}", ' ', $cells->item($i)->textContent));
        if ($text(3) !== '') {
            continue;
        }
        $waiting[] = [
            'name' => trim($link->textContent),
            'type' => $text(1),
            'created' => $text(2),
            'size' => $text(4),
            'url' => resolve($page_url, $link->getAttribute('href')),
        ];
    }

    return $waiting;
}

if ($argc > 2) {
    fwrite(STDERR, "usage: php dnl_004.php [directory] | --list\n");
    exit(1);
}
$list_only = ($argv[1] ?? '') === '--list';
$dir = $list_only ? '' : ($argv[1] ?? (getenv('VT_MEDICAID_DOWNLOAD_DIR') ?: './vtmedicaid_downloads'));

note('Starting download run' . ($list_only ? ' (list only)' : ''), 'download');
$http = portal_client();
sign_in($http, 'Nothing was downloaded.');

$page_url = resolve(portal_url(), SEARCH_PATH);
try {
    $response = $http->request('GET', $page_url);
    if ($response->getStatusCode() >= 400) {
        fail('The portal answered the download page with HTTP ' . $response->getStatusCode() . '. Nothing was downloaded.');
    }
    $page = load_page((string) $response->getBody());
    $fifty = list_fifty($http, $page, $page_url);
} catch (GuzzleException $e) {
    fail('The connection to the portal failed on the download page (' . reason($e) . '). Nothing was downloaded.');
}
if ($fifty === null) {
    // Still right, only possibly short: an older waiting file could be past the end of the list.
    note('WARNING - could not ask for ' . COUNT . ' files; using the page as it first came.');
    echo 'Could not ask the portal to list ' . COUNT . " files; going by its first page.\n";
} else {
    $page = load_page($fifty);
}
if ($page->query('//table//tr[td]')->length === 0) {
    fail('The portal\'s download page has no file table: the portal has changed. Nothing was downloaded.');
}

$waiting = waiting_files($page, $page_url);
note('Found ' . count($waiting) . ' files not yet downloaded');
echo "\nFound " . count($waiting) . " files not yet downloaded\n";
foreach ($waiting as $file) {
    note('Waiting: ' . $file['name'] . ' (' . $file['type'] . ', created ' . $file['created'] . ', ' . $file['size'] . ' bytes)');
    if ($list_only) {
        echo '  ' . $file['name'] . '  ' . $file['type'] . '  ' . $file['created'] . '  ' . $file['size'] . "\n";
    }
}
if ($list_only || $waiting === []) {
    echo $list_only ? "Nothing was downloaded.\n" : "No files to download!\n";
    exit(0);
}

if (!is_dir($dir) && !@mkdir($dir, 0775, true)) {
    fail('Could not make the download directory ' . $dir . '. Nothing was downloaded.');
}
if (!is_writable($dir)) {
    fail('Cannot write to the download directory ' . $dir . '. Nothing was downloaded.');
}
$dir = (string) realpath($dir);

$saved = [];
$failed = [];
foreach ($waiting as $i => $file) {
    // The name comes from the portal: keep it inside the download directory.
    $name = basename(str_replace('\\', '/', $file['name']));
    echo 'Downloading ' . ($i + 1) . '/' . count($waiting) . ': ' . $name . "\n";
    if ($name === '' || $name === '.' || $name === '..') {
        note('ERROR - unusable file name from the portal: ' . $file['name']);
        $failed[] = $file['name'];
        continue;
    }
    $path = $dir . '/' . $name;
    $part = $path . '.part';
    try {
        $response = $http->request('GET', $file['url'], ['sink' => $part]);
        $status = $response->getStatusCode();
        $error = $status === 200 ? null : 'HTTP ' . $status;
    } catch (GuzzleException $e) {
        $error = reason($e);
    }
    clearstatcache();
    if ($error === null && (!is_file($part) || filesize($part) === 0)) {
        $error = 'the file came empty';
    }
    if ($error === null && !rename($part, $path)) {
        $error = 'could not save it as ' . $path;
    }
    if ($error !== null) {
        @unlink($part);
        note('ERROR - failed to download ' . $name . ': ' . $error);
        $failed[] = $name;
    } else {
        note('Downloaded ' . $name . ' (listed ' . $file['size'] . ' bytes, saved ' . filesize($path) . ' bytes)');
        $saved[] = $name;
    }
    sleep(1);
}

$line = str_repeat('=', 80);
echo "\n$line\nDOWNLOAD SUMMARY\n$line\n";
echo 'Downloaded: ' . count($saved) . " files\n";
echo 'Failed: ' . count($failed) . " files\n";
echo 'Download directory: ' . $dir . "\n$line\n";
if ($saved !== []) {
    echo "\nDownloaded files:\n  " . implode("\n  ", $saved) . "\n";
}
if ($failed !== []) {
    echo "\nFailed downloads:\n  " . implode("\n  ", $failed) . "\n" . BY_HAND . "\n";
}
note('Download complete. Saved: ' . count($saved) . ', failed: ' . count($failed));

if ($failed !== []) {
    exit(1);
}
echo "\nSUCCESS - Downloaded " . count($saved) . ' files to ' . $dir . "\n";
