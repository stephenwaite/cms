<?php
/**
 * Office Ally Claim Attachments uploader.
 *
 */

require_once('/../../vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Exception\RequestException;

// =====================================================================
// Configuration
// =====================================================================

const OA_API_BASE = 'https://xpi.officeally.com/Claim.Attachments.Api/v1/';

// X12 PWK01 = "OZ" (Support Data for Claim) — 
const OA_ATTACH_TYPE_OZ = '164';

// OA form types. Only Professional (2)
const OA_FORM_TYPE_PROFESSIONAL = 2;  // 837P / HCFA-1500 — confirmed

// statusId 
const OA_STATUS_ACCEPTED = 110;

// PDF directory
const PDF_DIR = '/home/rri/';

$jwt = getenv('OA_JWT');
if (!$jwt) {
    fwrite(STDERR, "OA_JWT environment variable not set.\n");
    exit(1);
}

// =====================================================================
// HTTP client
// =====================================================================

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

// =====================================================================
// API methods
// =====================================================================

function oa_search_by_pcn(Client $client, string $pcn, int $daysBack = 90): array {
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
            'accountNumber' => $pcn,
            'memberId'      => null,
        ],
        'providerTaxId' => null,
        'formType'      => OA_FORM_TYPE_PROFESSIONAL,
    ];

    $res = $client->post('formtype/' . OA_FORM_TYPE_PROFESSIONAL . '/claims/search', [
        'json' => $body,
    ]);

    $decoded = json_decode((string) $res->getBody(), true);

    if (isset($decoded['results']) && is_array($decoded['results'])) {
        return $decoded['results'];
    }
    if (is_array($decoded) && array_is_list($decoded)) {
        return $decoded;
    }
    if (isset($decoded['claimId'])) {
        return [$decoded];
    }
    return [];
}

function oa_get_claim(Client $client, string $claimId, int $formType): array {
    $res = $client->get("formtype/{$formType}/claims/{$claimId}");
    return json_decode((string) $res->getBody(), true) ?? [];
}

function oa_upload(Client $client, array $claim, string $filePath): string {
    if (!is_readable($filePath)) {
        throw new RuntimeException("Cannot read file: {$filePath}");
    }

    $filename = basename($filePath);
    $mimeType = mime_content_type($filePath) ?: 'application/pdf';
    $formType = (int) ($claim['formType'] ?? OA_FORM_TYPE_PROFESSIONAL);
    $claimId  = (string) $claim['claimId'];

    $metaData = [
        'claimId'       => (int) $claimId,
        'claimFormType' => $formType,
        'payerMemberId' => (int) $claim['payerMemberId'],
        'status'        => 2,
        'id'            => 0,
        'data' => [
            'attachmentType' => OA_ATTACH_TYPE_OZ,
            'fileType'       => $mimeType,
        ],
    ];

    $res = $client->post("formType/{$formType}/claims/{$claimId}/attachments", [
        'multipart' => [
            [
                'name'     => 'metaData',
                'contents' => json_encode($metaData),
            ],
            [
                'name'     => $filename,
                'filename' => $filename,
                'contents' => Utils::tryFopen($filePath, 'r'),
                'headers'  => ['Content-Type' => $mimeType],
            ],
        ],
    ]);

    return (string) $res->getBody();
}

// =====================================================================
// Batch driver — reads wcompin format, same as CHC script
// =====================================================================

$inputFile = $argv[1] ?? '/home/rri/wcompin';

$fh_wcomp_sid = fopen($inputFile, 'r');
if (!$fh_wcomp_sid) {
    fwrite(STDERR, "Cannot open input file: {$inputFile}\n");
    exit(1);
}

$cntr    = 0;
$succeed = 0;
$failed  = 0;
$logFile = '/home/rri/oa_attachments_log_' . date('Ymd_His') . '.jsonl';
$logFh   = fopen($logFile, 'w');

while ($row = fgets($fh_wcomp_sid)) {
    $cntr++;

    // Same fixed-width parse as CHC script. Most fields unused for OA
    // (it derives them from the claim) but kept so COBOL output format
    // doesn't have to change.
    $charcur_key = substr($row, 0, 11);
    $ins_neic    = substr($row, 11, 5);
    $ins_name    = trim(substr($row, 16, 22));
    $ins_street  = trim(substr($row, 38, 24));
    $ins_city    = trim(substr($row, 62, 15));
    $ins_state   = substr($row, 77, 2);
    $ins_zip     = trim(substr($row, 79, 9));
    if (strlen($ins_zip) == 5) {
        $ins_zip = $ins_zip . "9999";
    }
    $g_pripol         = trim(substr($row, 88, 16));
    $g_garname        = substr($row, 104, 24);
    $g_garname_array  = explode(';', $g_garname);
    $firstName        = trim($g_garname_array[1] ?? '');
    $lastName         = trim($g_garname_array[0] ?? '');
    $g_garno          = substr($row, 128, 8);
    $cc_date_t        = substr($row, 136, 8);
    $cc_date_a        = substr($row, 144, 8);

    // PCN for OA lookup. Same field CHC uses as the
    // providerAttachmentControlNumber and PDF filename basis.
    $pcn      = trim(substr($charcur_key, 0, 8));
    $pdfPath  = PDF_DIR . trim($charcur_key) . '.pdf';

    try {
        $matches = oa_search_by_pcn($client, $pcn);

        if (count($matches) === 0) {
            throw new RuntimeException("No OA claim found for PCN '{$pcn}'");
        }
        if (count($matches) > 1) {
            $ids = implode(', ', array_column($matches, 'claimId'));
            throw new RuntimeException("Multiple claims for PCN '{$pcn}': {$ids}");
        }

        $claim = $matches[0];

        if (($claim['statusId'] ?? null) !== OA_STATUS_ACCEPTED) {
            throw new RuntimeException(
                "Claim {$claim['claimId']} for PCN '{$pcn}' not in accepted state "
                . "(statusId={$claim['statusId']})"
            );
        }

        $beforeCount = (int) ($claim['attachmentCount'] ?? 0);

        $uploadResp = oa_upload($client, $claim, $pdfPath);

        // Verify by re-fetching and confirming count incremented
        $after      = oa_get_claim($client, (string) $claim['claimId'], (int) $claim['formType']);
        $afterCount = (int) ($after['attachmentCount'] ?? 0);

        if ($afterCount <= $beforeCount) {
            throw new RuntimeException(
                "Upload returned success but attachmentCount did not increment "
                . "({$beforeCount} -> {$afterCount})"
            );
        }

        $result = [
            'status'             => 'ok',
            'pcn'                => $pcn,
            'oa_claim_id'        => $claim['claimId'],
            'payer_name'         => $claim['payerName'] ?? null,
            'carrier_claim_no'   => $claim['patientId'] ?? null,
            'file_name'          => basename($pdfPath),
            'count_before'       => $beforeCount,
            'count_after'        => $afterCount,
            'upload_response'    => $uploadResp,
            'submitted_at'       => date('c'),
        ];

        $succeed++;
        echo "[{$cntr}] OK   {$pcn} -> claim {$claim['claimId']} ({$claim['payerName']}) "
           . "count {$beforeCount}->{$afterCount}\n";

    } catch (RequestException $e) {
        $resp   = $e->getResponse();
        $status = $resp ? $resp->getStatusCode() : 0;
        $bodySn = $resp ? substr((string) $resp->getBody(), 0, 500) : '';
        $result = [
            'status'        => 'error',
            'error_type'    => 'http',
            'http_status'   => $status,
            'error_message' => $e->getMessage(),
            'response_body' => $bodySn,
            'pcn'           => $pcn,
            'file_name'     => basename($pdfPath),
            'submitted_at'  => date('c'),
        ];
        $failed++;
        fwrite(STDERR, "[{$cntr}] FAIL {$pcn} HTTP {$status}: {$bodySn}\n");

        if ($status === 401) {
            fwrite(STDERR, "JWT rejected (401). Aborting batch — re-scrape OA_JWT.\n");
            fwrite($logFh, json_encode($result) . "\n");
            break;
        }
    } catch (Throwable $e) {
        $result = [
            'status'        => 'error',
            'error_type'    => 'app',
            'error_message' => $e->getMessage(),
            'pcn'           => $pcn ?? null,
            'file_name'     => isset($pdfPath) ? basename($pdfPath) : null,
            'submitted_at'  => date('c'),
        ];
        $failed++;
        fwrite(STDERR, "[{$cntr}] FAIL {$pcn}: {$e->getMessage()}\n");
    }

    fwrite($logFh, json_encode($result) . "\n");
}

fclose($fh_wcomp_sid);
fclose($logFh);

echo "\n";
echo "Processed:  {$cntr}\n";
echo "Succeeded:  {$succeed}\n";
echo "Failed:     {$failed}\n";
echo "Log:        {$logFile}\n";

exit($failed === 0 ? 0 : 1);
