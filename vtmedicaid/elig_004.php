<?php

/**
 * Checks a member's Vermont Medicaid (04) eligibility on vtmedicaid.com.
 *
 * usage: php elig_004.php <member id> <date of service mm/dd/yyyy>
 *        php elig_004.php -f <input file> <output file>
 *
 * One member: prints the member information, eligibility records, important messages
 * and third party liability the portal shows; exit status 0 when it answered, 1 for an
 * error or no result.
 *
 * -f: one lookup per line of the input file (charcur key in columns 1-11, member id from
 * column 59, date of service mmddyyyy from column 108) and a tab-delimited output file:
 * charcur_key, carrier_code, member_id, dos, status, benefit_plan, tpl_coverage,
 * tpl_description, result. Each row is written as it is answered.
 *
 * The login comes from VT_MEDICAID_USER and VT_MEDICAID_PASS. Each run adds to
 * vtmedicaid_eligibility_<yyyymmdd>.log in the current directory (member id and date
 * only, never the answer).
 *
 * Needs PHP 8.4 with the dom extension (Dom\HTMLDocument). Replaces
 * python/vtmed_elig.py, which drove a browser to do it.
 */

use Dom\Element;
use Dom\HTMLDocument;
use GuzzleHttp\Client;
use GuzzleHttp\Exception\GuzzleException;
use GuzzleHttp\Psr7\Uri;
use GuzzleHttp\Psr7\UriResolver;

require_once(dirname(__FILE__) . '/portal.php');

const ELIGIBILITY_PATH = '/secure/eligibilityStatus.do';
const MEMBER_FIELD = 'memberId';
const DATE_FIELD = 'beginDate';

/*
 * Third party liability coverage labels, in the order a carrier code is taken from them:
 * the code is the first word after the label, "PHYSICIAN INPATIENT / OUTPATIENT SERVICES
 * 16 - AETNA" gives 16. Part C before Part B: a Medicare Advantage member still shows Part B.
 */
const CARRIER_LABELS = [
    'PHYSICIAN INPATIENT / OUTPATIENT SERVICES ',
    'MAJOR MEDICAL/PHYSICIAN (IV-D) NO DRUG COVERAGE FO ',
    'MEDICARE C - COMPREHENSIVE COVERAGE & DRUG ',
    'MEDICARE B ',
];

/**
 * Trim, counting &nbsp; as a space.
 */
function clean(string $text): string
{
    return preg_replace('/^[\s\x{a0}]+|[\s\x{a0}]+$/u', '', $text) ?? trim($text);
}

/**
 * All runs of white space as one space: what a tab-delimited column can hold.
 */
function one_line(string $text): string
{
    return clean(preg_replace('/[\s\x{a0}]+/u', ' ', $text) ?? $text);
}

/**
 * An element's text as the page shows it: a line per <br>, other white space as one space.
 */
function shown_text(Element $element): string
{
    $html = preg_replace('/\s+/', ' ', $element->innerHTML) ?? $element->innerHTML;
    $html = preg_replace('/<br\b[^>]*>/i', "\n", $html) ?? $html;
    $lines = array_map(clean(...), explode("\n", html_entity_decode(strip_tags($html), ENT_QUOTES | ENT_HTML5)));

    return clean(implode("\n", $lines));
}

/**
 * The fieldsets whose legend holds $legend, as "Eligibility" or "Third Party Liability".
 *
 * @return list<Element>
 */
function fieldsets(HTMLDocument $page, string $legend): array
{
    $found = [];
    foreach ($page->querySelectorAll('fieldset.scheduler-border') as $fieldset) {
        foreach ($fieldset->querySelectorAll('legend') as $candidate) {
            if (stripos($candidate->textContent, $legend) !== false) {
                $found[] = $fieldset;
                break;
            }
        }
    }

    return $found;
}

/**
 * The rows of td.elig-info cells under those fieldsets, a list of cells per row.
 *
 * @param list<Element> $fieldsets
 * @return list<list<Element>>
 */
function info_rows(array $fieldsets): array
{
    $rows = [];
    foreach ($fieldsets as $fieldset) {
        foreach ($fieldset->querySelectorAll('tr') as $row) {
            $cells = iterator_to_array($row->querySelectorAll('td.elig-info'), false);
            if ($cells !== []) {
                $rows[] = $cells;
            }
        }
    }

    return $rows;
}

/**
 * Ask the portal about one member on one date.
 *
 * @return array{error: true, messages: list<string>}|array{error: false, found: false, text: string}|array{error: false, found: true, member_info: array<string, string>, eligibility: list<array<string, string>>, tpl: list<array<string, string>>, important: list<string>, disclaimer: string}
 * @throws RuntimeException when the eligibility form is not there or the login page comes back (session ended, or the portal changed)
 * @throws GuzzleException
 */
function query_member(Client $http, string $member_id, string $dos): array
{
    note('Eligibility check for member ' . $member_id . ', date of service ' . $dos);
    $page_url = (string) UriResolver::resolve(new Uri(portal_url()), new Uri(ELIGIBILITY_PATH));
    $response = $http->request('GET', $page_url);
    $page = HTMLDocument::createFromString((string) $response->getBody(), LIBXML_NOERROR);

    $form = null;
    foreach ($page->querySelectorAll('form') as $candidate) {
        if ($candidate->querySelector('input[name="' . MEMBER_FIELD . '"]') !== null) {
            $form = $candidate;
            break;
        }
    }
    if ($form === null) {
        throw new RuntimeException('the portal did not show its eligibility form (HTTP ' . $response->getStatusCode() . ')');
    }

    // The form as a browser would send it: its hidden fields and the first submit button.
    $fields = [];
    $submitted = false;
    foreach ($form->querySelectorAll('input[name], select[name], textarea[name]') as $field) {
        $name = (string) $field->getAttribute('name');
        if ($name === '' || $field->hasAttribute('disabled')) {
            continue;
        }
        $kind = strtolower($field->localName);
        if ($kind === 'select') {
            $option = $field->querySelector('option[selected]') ?? $field->querySelector('option');
            $fields[$name] = $option === null ? '' : ($option->getAttribute('value') ?? clean($option->textContent));
            continue;
        }
        if ($kind === 'textarea') {
            $fields[$name] = $field->textContent;
            continue;
        }
        $type = strtolower((string) $field->getAttribute('type'));
        if (in_array($type, ['button', 'reset', 'file', 'image'], true)) {
            continue;
        }
        if (in_array($type, ['checkbox', 'radio'], true) && !$field->hasAttribute('checked')) {
            continue;
        }
        if ($type === 'submit') {
            if ($submitted) {
                continue;
            }
            $submitted = true;
        }
        $fields[$name] = (string) $field->getAttribute('value');
    }
    $fields[MEMBER_FIELD] = $member_id;
    $fields[DATE_FIELD] = $dos;

    $post = strtolower((string) $form->getAttribute('method')) === 'post';
    $action = (string) $form->getAttribute('action');
    $response = $http->request(
        $post ? 'POST' : 'GET',
        $action === '' ? $page_url : (string) UriResolver::resolve(new Uri($page_url), new Uri($action)),
        [$post ? 'form_params' : 'query' => $fields]
    );
    $page = HTMLDocument::createFromString((string) $response->getBody(), LIBXML_NOERROR);

    if ($page->querySelector('div.errors') !== null) {
        $messages = [];
        foreach ($page->querySelectorAll('div.errors li') as $item) {
            $messages[] = clean($item->textContent);
        }
        note('ERROR - the portal refused the check for member ' . $member_id . ': ' . implode('; ', $messages));

        return ['error' => true, 'messages' => $messages];
    }
    if ($page->querySelector('input[name="logon_id"]') !== null) {
        throw new RuntimeException('the portal asked for the login again');
    }
    if ($page->querySelector('fieldset.scheduler-border') === null) {
        note('ERROR - no results on the page for member ' . $member_id);

        return ['error' => false, 'found' => false, 'text' => clean($page->body?->textContent ?? '')];
    }

    $labels = $page->querySelectorAll('div.elig-member-info');
    $values = $page->querySelectorAll('div.elig-member-info-res');
    $member_info = [];
    for ($i = 0; $i < min($labels->length, $values->length); $i++) {
        $member_info[rtrim(clean($labels->item($i)->textContent), ':')] = clean($values->item($i)->textContent);
    }

    $eligibility = [];
    foreach (info_rows(fieldsets($page, 'Eligibility')) as $cells) {
        if (count($cells) >= 7) {
            $eligibility[] = [
                'start_date' => clean($cells[0]->textContent),
                'end_date' => clean($cells[1]->textContent),
                'status' => clean($cells[2]->textContent),
                'benefit_plan' => clean($cells[3]->textContent),
                'service_type' => shown_text($cells[4]),
                'max_copay' => clean($cells[5]->textContent),
                'aco' => clean($cells[6]->textContent),
            ];
        } elseif (count($cells) === 1 && clean($cells[0]->textContent) !== '') {
            $eligibility[] = ['message' => clean($cells[0]->textContent)];
        }
    }

    $tpl = [];
    foreach (info_rows(fieldsets($page, 'Third Party Liability')) as $cells) {
        if (count($cells) >= 3) {
            $tpl[] = [
                'start_date' => clean($cells[0]->textContent),
                'end_date' => clean($cells[1]->textContent),
                'coverage' => clean($cells[2]->textContent),
                'description' => isset($cells[3]) ? clean($cells[3]->textContent) : '',
            ];
        }
    }

    $important = [];
    foreach ($page->querySelectorAll('div.elig-member-info-important') as $message) {
        if (clean($message->textContent) !== '') {
            $important[] = clean($message->textContent);
        }
    }

    note('Eligibility check completed for ' . $member_id);

    return [
        'error' => false,
        'found' => true,
        'member_info' => $member_info,
        'eligibility' => $eligibility,
        'tpl' => $tpl,
        'important' => $important,
        'disclaimer' => clean($page->querySelector('p.elig-member-info-disclaimer')?->textContent ?? ''),
    ];
}

/**
 * @param array<string, mixed> $result a found result of query_member()
 */
function print_result(array $result, string $member_id, string $dos): void
{
    $wide = str_repeat('=', 80);
    $thin = str_repeat('-', 80);
    $short = str_repeat('-', 40);

    echo "\n$wide\nELIGIBILITY CHECK RESULTS\n$wide\n";
    echo "Query - Member ID: $member_id, Date of Service: $dos\n$wide\n";
    if ($result['disclaimer'] !== '') {
        echo "\nNOTE: {$result['disclaimer']}\n$thin\n";
    }

    echo "\nMEMBER INFORMATION:\n$thin\n";
    foreach ($result['member_info'] as $label => $value) {
        if (str_contains($value, "\n")) {
            // An address: one line each.
            echo "$label:\n";
            foreach (array_filter(array_map(clean(...), explode("\n", $value)), static fn(string $l): bool => $l !== '') as $line) {
                echo "  $line\n";
            }
        } else {
            echo "$label: $value\n";
        }
    }

    if ($result['eligibility'] !== []) {
        echo "\n$thin\nELIGIBILITY RECORDS:\n$thin\n";
        foreach ($result['eligibility'] as $record) {
            if (isset($record['message'])) {
                echo "Note: {$record['message']}\n$short\n";
                continue;
            }
            echo "Start Date:   {$record['start_date']}\n";
            echo "End Date:     {$record['end_date']}\n";
            echo "Status:       {$record['status']}\n";
            echo "Benefit Plan: {$record['benefit_plan']}\n";
            echo "Service Type: {$record['service_type']}\n";
            echo "Max Co-Pay:   {$record['max_copay']}\n";
            echo "ACO:          {$record['aco']}\n$short\n";
        }
    }

    if ($result['important'] !== []) {
        echo "\n$thin\nIMPORTANT MESSAGES:\n";
        foreach ($result['important'] as $message) {
            echo "  $message\n";
        }
    }

    if ($result['tpl'] !== []) {
        echo "\n$thin\nTHIRD PARTY LIABILITY:\n$thin\n";
        foreach ($result['tpl'] as $record) {
            echo "Start Date:   {$record['start_date']}\n";
            echo "End Date:     {$record['end_date']}\n";
            echo "Coverage:     {$record['coverage']}\n";
            echo "Description:  {$record['description']}\n$short\n";
        }
    }
    echo "$wide\n\n";
}

/**
 * @param ?array<string, mixed> $result of query_member(); null when the lookup failed
 */
function carrier_code(?array $result): string
{
    if ($result === null || $result['error'] || !$result['found']) {
        return '';
    }
    $coverages = array_map(static fn(array $r): string => one_line($r['coverage']) . ' ', $result['tpl']);
    foreach (CARRIER_LABELS as $label) {
        foreach ($coverages as $coverage) {
            if (str_starts_with($coverage, $label)) {
                $rest = one_line(substr($coverage, strlen($label)));
                if ($rest !== '') {
                    return explode(' ', $rest)[0];
                }
            }
        }
    }

    return '';
}

/**
 * The status, benefit plan, TPL coverage, TPL description and result columns.
 *
 * @param ?array<string, mixed> $result
 * @return list<string>
 */
function carrier_summary(?array $result): array
{
    if ($result === null || (!$result['error'] && !$result['found'])) {
        return ['', '', '', '', 'FAILED'];
    }
    if ($result['error']) {
        return ['', '', '', '', 'ERROR: ' . implode('; ', $result['messages'])];
    }
    $records = array_values(array_filter($result['eligibility'], static fn(array $r): bool => isset($r['status'])));

    return [
        implode(';', array_column($records, 'status')),
        implode(';', array_column($records, 'benefit_plan')),
        implode(';', array_column($result['tpl'], 'coverage')),
        implode(';', array_column($result['tpl'], 'description')),
        'OK',
    ];
}

/**
 * Charcur key, member id and date (mm/dd/yyyy) from a line of the input file; null when
 * the line has no member id or date where they belong.
 *
 * @return ?array{string, string, string}
 */
function parse_input_line(string $line): ?array
{
    $member_id = preg_split('/\s+/', trim(substr($line, 58)))[0] ?? '';
    $dos = substr($line, 107, 8);
    if ($member_id === '' || preg_match('/^\d{8}$/', $dos) !== 1) {
        return null;
    }

    return [trim(substr($line, 0, 11)), $member_id, substr($dos, 0, 2) . '/' . substr($dos, 2, 2) . '/' . substr($dos, 4, 4)];
}

/**
 * One lookup; when it fails for a reason other than the portal's own answer, sign in
 * again (the session may have timed out) and try once more. Null when that fails too.
 *
 * @return ?array<string, mixed>
 */
function query_with_retry(Client $http, string $member_id, string $dos, string $where): ?array
{
    try {
        return query_member($http, $member_id, $dos);
    } catch (RuntimeException | GuzzleException $e) {
        $why = $e instanceof GuzzleException ? reason($e) : $e->getMessage();
        note("WARNING - $where: $why; signing in again and retrying");
    }
    $error = sign_in_error($http);
    if ($error !== null) {
        note("ERROR - $where: $error");

        return null;
    }
    try {
        return query_member($http, $member_id, $dos);
    } catch (RuntimeException | GuzzleException $e) {
        note("ERROR - $where: retry failed: " . ($e instanceof GuzzleException ? reason($e) : $e->getMessage()));

        return null;
    }
}

$usage = "usage: php elig_004.php <member id> <date of service>\n"
    . "       php elig_004.php -f <input file> <output file>\n"
    . "example: php elig_004.php 1310414 01/15/2026\n"
    . "input file: member id from column 59, date of service mmddyyyy from column 108; output is tab-delimited\n";

if ($argc === 4 && $argv[1] === '-f') {
    [, , $input, $output] = $argv;
    note('Starting eligibility run from ' . $input, 'eligibility');
    $lines = is_readable($input) ? file($input, FILE_IGNORE_NEW_LINES) : false;
    if ($lines === false) {
        fail('Cannot read the input file ' . $input . '.');
    }
    $http = portal_client();
    sign_in($http, 'Nothing was checked.');
    $out = @fopen($output, 'w');
    if ($out === false) {
        fail('Cannot write the output file ' . $output . '.');
    }
    fwrite($out, implode("\t", ['charcur_key', 'carrier_code', 'member_id', 'dos', 'status', 'benefit_plan',
        'tpl_coverage', 'tpl_description', 'result']) . "\n");
    $counts = ['OK' => 0, 'ERROR' => 0, 'FAILED' => 0, 'skipped' => 0];
    foreach ($lines as $i => $line) {
        $n = $i + 1;
        $parsed = parse_input_line($line);
        if ($parsed === null) {
            note("WARNING - line $n: no member id/date found, skipped");
            $counts['skipped']++;
            continue;
        }
        [$key, $member_id, $dos] = $parsed;
        $result = query_with_retry($http, $member_id, $dos, "line $n");
        $row = array_merge([$key, carrier_code($result), $member_id, $dos], carrier_summary($result));
        fwrite($out, implode("\t", array_map(one_line(...), $row)) . "\n");
        fflush($out);
        $counts[explode(':', $row[8])[0]]++;
        note("Line $n of " . count($lines) . ' done');
        usleep(250000);
    }
    fclose($out);
    echo count($lines) . " lines: {$counts['OK']} answered, {$counts['ERROR']} refused by the portal, "
        . "{$counts['FAILED']} failed, {$counts['skipped']} skipped. Output: $output\n";
    exit(0);
}

if ($argc !== 3) {
    fwrite(STDERR, $usage);
    exit(1);
}
[, $member_id, $dos] = $argv;
note('Starting eligibility check', 'eligibility');
echo "Checking eligibility for Member ID: $member_id, DOS: $dos\n";

$http = portal_client();
sign_in($http, 'Nothing was checked.');
$result = query_with_retry($http, $member_id, $dos, 'member ' . $member_id);
if ($result === null) {
    fail('Eligibility check failed: the portal did not show its eligibility form or dropped the connection, twice.');
}

$wide = str_repeat('=', 80);
if ($result['error']) {
    echo "\n$wide\nELIGIBILITY CHECK ERROR\n$wide\nMember ID: $member_id\nDate of Service: $dos\n\nERROR:\n";
    foreach ($result['messages'] as $message) {
        echo "  - $message\n";
    }
    echo "$wide\n\n\nFAILED - Invalid member ID or search parameters\n";
    exit(1);
}
if (!$result['found']) {
    echo "\n$wide\nELIGIBILITY CHECK - NO RESULTS FOUND\n$wide\n" . mb_substr($result['text'], 0, 500) . "\n$wide\n\n";
    echo "\nFAILED - Eligibility check failed\n";
    exit(1);
}
print_result($result, $member_id, $dos);
echo "\nSUCCESS - Eligibility information retrieved\n";
