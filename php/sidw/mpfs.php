<?php
/**
 * pfs_lookup.php — Medicare Physician Fee Schedule locality-adjusted allowable lookup
 *
 * Requires: composer require guzzlehttp/guzzle
 *
 * Locality-adjusted formula (CMS):
 *   Non-Fac = [(work_rvu * gpci_work) + (nfac_pe_rvu * gpci_pe) + (mp_rvu * gpci_mp)] * conv_fact
 *   Facility = [(work_rvu * gpci_work) + (fac_pe_rvu  * gpci_pe) + (mp_rvu * gpci_mp)] * conv_fact
 *
 * Usage:
 *   php pfs_lookup.php <hcpcs_code> <mac_locality> [year] [modifier]
 *
 * Examples:
 *   php pfs_lookup.php 72197 1420200 2025 26    # Vermont, prof component
 *   php pfs_lookup.php 99213 0000000 2025       # National rate
 *   php pfs_lookup.php 93000 1420200            # Current year
 *
 * Vermont locality: 1420200
 * National:         0000000
 */

require __DIR__ . '/../../vendor/autoload.php';

use GuzzleHttp\Client;
use GuzzleHttp\Exception\GuzzleException;

// ---------------------------------------------------------------------------
// CLI arguments
// ---------------------------------------------------------------------------
if ($argc < 3) {
    die("Usage: php pfs_lookup.php <hcpcs_code> <mac_locality> [year] [modifier]\n");
}

$hcpcs    = strtoupper(trim($argv[1]));
$locality = $argv[2];
$year     = $argv[3] ?? date('Y');
$modifier = strtoupper(trim($argv[4] ?? ''));

// ---------------------------------------------------------------------------
// Guzzle client
// ---------------------------------------------------------------------------
$client = new Client([
    'base_uri' => 'https://pfs.data.cms.gov/api/1/',
    'timeout'  => 15,
    'headers'  => ['Accept' => 'application/json'],
]);

/**
 * GET a JSON endpoint and return decoded array.
 */
function api_get(Client $client, string $path, array $query = []): array
{
    try {
        $response = $client->get($path, ['query' => $query]);
        return json_decode((string) $response->getBody(), true) ?? [];
    } catch (GuzzleException $e) {
        fwrite(STDERR, "HTTP error: " . $e->getMessage() . "\n");
        exit(1);
    }
}

/**
 * Build a DKAN conditions query array from a list of [property, value, operator].
 */
function build_conditions(array $conditions): array
{
    $query = [];
    foreach ($conditions as $i => $c) {
        foreach ($c as $k => $v) {
            $query["conditions[$i][$k]"] = $v;
        }
    }
    return $query;
}

// ---------------------------------------------------------------------------
// Step 1: discover dataset UUIDs from the catalog
//         Indicators  — RVUs, conversion factor, policy indicators
//         Localities  — GPCIs per locality
//         For years with A/B splits, prefer B over A.
// ---------------------------------------------------------------------------
fwrite(STDERR, "Fetching PFS dataset catalog...\n");

$meta = api_get($client, 'metastore/schemas/dataset/items');

$indicatorsByYear = [];
$localitiesByYear = [];

foreach ($meta as $d) {
    $title = $d['title'] ?? '';
    $id    = $d['identifier'];

    if (preg_match('/^(Indicators|Localities) for (20\d{2})(A|B)?$/i', $title, $m)) {
        $type   = strtolower($m[1]);
        $y      = $m[2];
        $suffix = strtoupper($m[3] ?? '');

        if ($type === 'indicators') {
            $existing = $indicatorsByYear[$y][1] ?? '';
            if ($suffix === 'B' || ($suffix === 'A' && $existing === '') || $existing === '') {
                $indicatorsByYear[$y] = [$id, $suffix];
            }
        } else {
            $existing = $localitiesByYear[$y][1] ?? '';
            if ($suffix === 'B' || ($suffix === 'A' && $existing === '') || $existing === '') {
                $localitiesByYear[$y] = [$id, $suffix];
            }
        }
    }
}

krsort($indicatorsByYear);
krsort($localitiesByYear);

// Walk back up to 3 years for both datasets
$indicatorsId = null;
$localitiesId = null;
$resolvedYear = null;

for ($y = (int)$year; $y >= (int)$year - 3; $y--) {
    $ys = (string)$y;
    if (isset($indicatorsByYear[$ys]) && isset($localitiesByYear[$ys])) {
        $indicatorsId = $indicatorsByYear[$ys][0];
        $localitiesId = $localitiesByYear[$ys][0];
        $resolvedYear = $ys;
        break;
    }
}

if (!$indicatorsId || !$localitiesId) {
    fwrite(STDERR, "ERROR: Could not find matching Indicators+Localities datasets for {$year}.\n");
    fwrite(STDERR, "       Indicators available: " . implode(', ', array_keys($indicatorsByYear)) . "\n");
    fwrite(STDERR, "       Localities available: " . implode(', ', array_keys($localitiesByYear)) . "\n");
    exit(1);
}

if ($resolvedYear !== $year) {
    fwrite(STDERR, "WARNING: No data for {$year} yet — using {$resolvedYear} instead.\n");
    $year = $resolvedYear;
}

fwrite(STDERR, "Indicators UUID: {$indicatorsId}\n");
fwrite(STDERR, "Localities UUID: {$localitiesId}\n");

// ---------------------------------------------------------------------------
// Step 2: fetch RVUs from Indicators dataset
// ---------------------------------------------------------------------------
$conditions = [
    ['property' => 'hcpc', 'value' => $hcpcs, 'operator' => '='],
];
if ($modifier !== '') {
    $conditions[] = ['property' => 'modifier', 'value' => $modifier, 'operator' => '='];
}

$rvu_result = api_get($client, "datastore/query/{$indicatorsId}/0", build_conditions($conditions));
$rvu_rows   = $rvu_result['results'] ?? [];

if (empty($rvu_rows)) {
    echo "No RVU data found for HCPCS {$hcpcs}" . ($modifier ? " modifier {$modifier}" : '') . ".\n";
    exit(1);
}

// Deduplicate — some years have duplicate rows; key by modifier
$rvu_by_mod = [];
foreach ($rvu_rows as $r) {
    $mod = trim($r['modifier'] ?? '');
    $rvu_by_mod[$mod] = $r; // last write wins, fine for dupes
}

// ---------------------------------------------------------------------------
// Step 3: fetch GPCIs from Localities dataset
// ---------------------------------------------------------------------------
$gpci_result = api_get($client, "datastore/query/{$localitiesId}/0", build_conditions([
    ['property' => 'locality', 'value' => $locality, 'operator' => '='],
]));
$gpci_rows = $gpci_result['results'] ?? [];

if (empty($gpci_rows)) {
    echo "No GPCI data found for locality {$locality}.\n";
    echo "Tip: use 0000000 for national rates.\n";
    exit(1);
}

$gpci       = $gpci_rows[0];
$gpci_work  = (float)($gpci['gpci_work'] ?? 1.0);
$gpci_pe    = (float)($gpci['gpci_pe']   ?? 1.0);
$gpci_mp    = (float)($gpci['gpci_mp']   ?? 1.0);
$loc_name   = $gpci['loc_description'] ?? $locality;

// ---------------------------------------------------------------------------
// Step 4: compute locality-adjusted allowables and display
// ---------------------------------------------------------------------------
echo str_pad('Year:',         16) . $year     . "\n";
echo str_pad('HCPCS Code:',   16) . $hcpcs    . "\n";
echo str_pad('Description:',  16) . ($rvu_rows[0]['sdesc'] ?? '') . "\n";
echo str_pad('Locality:',     16) . "{$locality} ({$loc_name})\n";
echo str_pad('GPCIs:',        16) . "Work={$gpci_work}  PE={$gpci_pe}  MP={$gpci_mp}\n";
echo "\n";

foreach ($rvu_by_mod as $mod => $r) {
    $modLabel = $mod !== '' ? "[{$mod}]" : '(none)';
    $cf       = (float)($r['conv_fact'] ?? 0);
    $status   = $r['proc_stat'] ?? '';

    $work_rvu    = (float)($r['rvu_work']       ?? 0);
    $nfac_pe_rvu = (float)($r['full_nfac_pe']   ?? 0);
    $fac_pe_rvu  = (float)($r['full_fac_pe']    ?? 0);
    $mp_rvu      = (float)($r['rvu_mp']         ?? 0);

    if ($cf > 0) {
        $nf_allow  = (($work_rvu * $gpci_work) + ($nfac_pe_rvu * $gpci_pe) + ($mp_rvu * $gpci_mp)) * $cf;
        $fac_allow = (($work_rvu * $gpci_work) + ($fac_pe_rvu  * $gpci_pe) + ($mp_rvu * $gpci_mp)) * $cf;
        $nf_str    = '$' . number_format($nf_allow,  2);
        $fac_str   = '$' . number_format($fac_allow, 2);
    } else {
        $nf_str = $fac_str = 'N/A';
    }

    printf(
        "  Modifier %-8s  Non-Facility: %8s   Facility: %8s   CF: \$%.4f   Status: %s\n",
        $modLabel, $nf_str, $fac_str, $cf, $status
    );
}