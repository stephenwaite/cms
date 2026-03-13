<?php

require_once(__DIR__ . '/../../vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Request;

$base_uri = 'https://apigw.changehealthcare.com/';
$guzzle   = new Client();
$response = $guzzle->post($base_uri . 'apip/auth/v2/token', [
    'form_params' => [
        'grant_type'    => 'client_credentials',
        'client_id'     => getenv('PROD_CHANGE_CLIENT_ID'),
        'client_secret' => getenv('PROD_CHANGE_CLIENT_SECRET'),
    ],
]);
$bearer = json_decode((string) $response->getBody(), true)['access_token'];

$client  = new Client();
$headers = [
    'Content-Type'  => 'application/json',
    'Authorization' => 'Bearer ' . $bearer,
];

$reportFile = '/home/rri/weligrpt';
$fo = fopen($reportFile, 'w');
if (!$fo) {
    die("Cannot open report file for writing: $reportFile\n");
}

$cntr         = 0;
$fh_wcomp_sid = fopen('/home/rri/clmstatin', 'r');
if (!$fh_wcomp_sid) {
    die("Cannot open input file\n");
}

while ($row = fgets($fh_wcomp_sid)) {
    $cntr++;

    // --- parse fixed-width input ---
    $charcur_key    = substr($row,   0, 11);
    $ins_neic       = trim(substr($row,  11,  5));
    $ins_name       = trim(substr($row,  16, 22));
    $g_pripol       = trim(substr($row,  88, 16));
    $g_garname      = substr($row, 104, 24);
    $g_garname_arr  = explode(';', $g_garname);
    $firstName      = trim($g_garname_arr[1] ?? '');
    $lastName       = trim($g_garname_arr[0] ?? '');
    $g_garno        = trim(substr($row, 128,  8));
    $cc_date_t      = substr($row, 136,  8);
    $g_group_number = trim(substr($row, 152, 10));
    $g_dob          = substr($row, 162,  8);
    $g_sex          = substr($row, 170,  1);

    // --- build request body ---
    $body = [
        'controlNumber'          => getControlNumber(),   // one call per row
        'tradingPartnerServiceId' => $ins_neic,
        'provider' => [
            'organizationName'       => 'RUTLAND RADIOLOGISTS',
            'npi'                    => '1700935780',
            'taxId'                  => '030238095',
            'providerCode'           => 'BI',
            'referenceIdentification'=> '2085R0202X',
        ],
        'subscriber' => [
            'memberId'    => $g_pripol,
            'firstName'   => $firstName,
            'lastName'    => $lastName,
            'gender'      => $g_sex,
            'dateOfBirth' => $g_dob,
        ],
        'encounter' => [
            'beginningDateOfService' => $cc_date_t,
            'endDateOfService'       => $cc_date_t,
            'serviceTypeCodes'       => ['52'],
        ],
    ];

    if ($g_group_number !== '') {
        $body['subscriber']['groupNumber'] = $g_group_number;
    }

    $request = new Request(
        'POST',
        $base_uri . 'medicalnetwork/eligibility/v3/',
        $headers,
        json_encode($body)
    );

    try {
        $res      = $client->sendAsync($request)->wait();
        $data     = json_decode((string) $res->getBody());
        $report   = formatBenefitsReport($data, $firstName, $lastName, $g_garno, $cc_date_t, $ins_name);
    } catch (Exception $e) {
        $errBody = method_exists($e, 'getResponse') && $e->getResponse()
            ? $e->getResponse()->getBody()->getContents()
            : $e->getMessage();
        $report  = "ERROR for $lastName, $firstName ($g_garno) dos $cc_date_t: $errBody\n";
    }

    echo $report;
    fwrite($fo, $report);
}

fclose($fh_wcomp_sid);
fclose($fo);

// ---------------------------------------------------------------------------

function formatBenefitsReport(object $d, string $firstName, string $lastName,
                               string $garno, string $dos, string $payerName): string
{
    $sep  = str_repeat('-', 72) . "\n";
    $out  = $sep;
    $out .= sprintf("PATIENT : %s, %s\n", strtoupper($lastName), strtoupper($firstName));
    $out .= sprintf("ACCT    : %s   DOS: %s   PAYER: %s\n", trim($garno), fmtDate($dos), $payerName);

    // plan status
    if (!empty($d->planStatus)) {
        foreach ($d->planStatus as $ps) {
            $status = strtoupper($ps->status ?? 'UNKNOWN');
            $out   .= sprintf("STATUS  : %s", $status);
            if (!empty($ps->planDetails)) {
                $out .= ' — ' . $ps->planDetails;
            }
            $out .= "\n";
        }
    }

    // plan dates
    if (!empty($d->planDateInformation)) {
        $pdi = $d->planDateInformation;
        if (!empty($pdi->plan))       $out .= sprintf("PLAN BEGIN : %s\n", fmtDate($pdi->plan));
        if (!empty($pdi->planEnd))    $out .= sprintf("PLAN END   : %s\n", fmtDate($pdi->planEnd));
        if (!empty($pdi->eligibilityBegin)) $out .= sprintf("ELIG BEGIN : %s\n", fmtDate($pdi->eligibilityBegin));
    }

    // payer / plan name
    if (!empty($d->payer->name))         $out .= sprintf("PAYER NAME : %s\n", $d->payer->name);
    if (!empty($d->subscriber->groupNumber)) $out .= sprintf("GROUP #    : %s\n", $d->subscriber->groupNumber);
    if (!empty($d->subscriber->groupName))   $out .= sprintf("GROUP NAME : %s\n", $d->subscriber->groupName);

    // benefits
    if (empty($d->benefitsInformation)) {
        $out .= "BENEFITS: none returned\n";
        $out .= $sep;
        return $out;
    }

    $out .= "\nBENEFITS:\n";
    $out .= sprintf("  %-32s %-16s %-10s %s\n", 'TYPE', 'COVERAGE LEVEL', 'NETWORK', 'AMOUNT/PCT');
    $out .= sprintf("  %s\n", str_repeat('-', 68));

    foreach ($d->benefitsInformation as $b) {
        $type     = $b->name ?? ($b->code ?? '?');
        $level    = $b->coverageLevel ?? '';
        $network  = '';
        if (!empty($b->inPlanNetworkIndicator)) {
            $network = $b->inPlanNetworkIndicatorCode === 'Y' ? 'In-Net'
                     : ($b->inPlanNetworkIndicatorCode === 'N' ? 'Out-Net' : '');
        }
        $amount = '';
        if (!empty($b->benefitAmount)) {
            $amount = '$' . number_format((float)$b->benefitAmount, 2);
        } elseif (!empty($b->benefitPercent)) {
            $amount = number_format((float)$b->benefitPercent * 100, 0) . '%';
        }
        $timeQ = !empty($b->timeQualifier) ? ' [' . $b->timeQualifier . ']' : '';

        $out .= sprintf("  %-32s %-16s %-10s %s%s\n",
            substr($type, 0, 32),
            substr($level, 0, 16),
            $network,
            $amount,
            $timeQ
        );

        // additional info lines (messages, limitations)
        if (!empty($b->additionalInformation)) {
            foreach ($b->additionalInformation as $ai) {
                if (!empty($ai->description)) {
                    $out .= sprintf("    NOTE: %s\n", $ai->description);
                }
            }
        }
    }

    $out .= $sep;
    return $out;
}

function fmtDate(string $d): string
{
    if (strlen($d) !== 8) return $d;
    return substr($d, 4, 2) . '/' . substr($d, 6, 2) . '/' . substr($d, 0, 4);
}

function getControlNumber(): string
{
    $fn  = '/home/rri/change_wc_control_number';
    $fh  = fopen($fn, 'r+');
    if (!$fh) {
        die("no control # file to read\n");
    }
    $cn = str_pad((int)fgets($fh) + 1, 9, '0', STR_PAD_LEFT);
    file_put_contents($fn, $cn);
    fclose($fh);
    return $cn;
}