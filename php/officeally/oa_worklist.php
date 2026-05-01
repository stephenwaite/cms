<?php
/**
 * Office Ally worklist — accepted claims without attachments.
 *   php oa_worklist.php          # last 15 days (default)
 *   php oa_worklist.php 60       # last N days
 */

require_once(__DIR__ . '/../../vendor/autoload.php');

use GuzzleHttp\Client;

const OA_API_BASE = 'https://xpi.officeally.com/Claim.Attachments.Api/v1/';
const OA_FORM_TYPE_PROFESSIONAL = 2;
const OA_STATUS_ACCEPTED = 110;

$jwt = getenv('OA_JWT');
if (!$jwt) {
    fwrite(STDERR, "OA_JWT environment variable not set.\n");
    exit(1);
}

$daysBack = (int) ($argv[1] ?? 15);

$client = new Client([
    'base_uri' => OA_API_BASE,
    'timeout'  => 60,
    'headers'  => [
        'Authorization' => 'Bearer ' . $jwt,
        'Accept'        => 'application/json, text/plain, */*',
        'Origin'        => 'https://www.officeally.com',
        'Referer'       => 'https://www.officeally.com/',
    ],
]);

$toDate   = new DateTimeImmutable('now', new DateTimeZone('UTC'));
$fromDate = $toDate->modify("-{$daysBack} days");

$body = [
    'date' => [
        'fromDate' => $fromDate->format('Y-m-d\TH:i:s.v\Z'),
        'toDate'   => $toDate->format('Y-m-d\TH:i:s.v\Z'),
    ],
    'payerMemberId' => null,
    'memberId'      => null,
    'patient' => [
        'firstName'     => null,
        'lastName'      => null,
        'accountNumber' => null,
        'memberId'      => null,
    ],
    'providerTaxId' => null,
    'formType'      => OA_FORM_TYPE_PROFESSIONAL,
];

$res = $client->post('formtype/' . OA_FORM_TYPE_PROFESSIONAL . '/claims/search', [
    'json' => $body,
]);

$decoded = json_decode((string) $res->getBody(), true);
$rows = $decoded['results']
    ?? (is_array($decoded) && array_is_list($decoded) ? $decoded : []);

// Filter: accepted, no attachments
$worklist = array_values(array_filter($rows, function ($c) {
    return ($c['statusId'] ?? null) === OA_STATUS_ACCEPTED
        && (int) ($c['attachmentCount'] ?? 0) === 0;
}));

// Sort newest first by receivedDate
usort($worklist, function ($a, $b) {
    return strcmp($b['receivedDate'] ?? '', $a['receivedDate'] ?? '');
});

if (count($worklist) === 0) {
    echo "No claims without attachments in the last {$daysBack} days.\n";
    exit(0);
}

// Print table
printf("%-4s %-12s %-12s %-12s %-25s %-20s %-10s %10s\n",
    '#', 'Received', 'OA ClaimID', 'PCN', 'Patient', 'Payer', 'DOS', 'Amount');
echo str_repeat('-', 115) . "\n";

foreach ($worklist as $i => $c) {
    $patient = trim(($c['patientLastName'] ?? '') . ', ' . ($c['patientFirstName'] ?? ''));
    printf("%-4d %-12s %-12s %-12s %-25s %-20s %-10s %10s\n",
        $i + 1,
        substr($c['receivedDate'] ?? '', 0, 10),
        $c['claimId'] ?? '',
        $c['patientAcctNo'] ?? '',
        substr($patient, 0, 25),
        substr($c['payerName'] ?? '', 0, 20),
        substr($c['fromDos'] ?? '', 0, 10),
        '$' . ($c['totalCharge'] ?? '0.00')
    );
}

echo "\n" . count($worklist) . " claim(s) without attachments (of " . count($rows) . " total in window)\n";
