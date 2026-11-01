<?php
// rri_docp.php - find the reading radiologist in openemr for charnew
// charges that came in with CD-DOCP = "00" (the facility stopped sending it).
//
// usage: php rri_docp.php <rrr255 extract> <answers out>
//
// in:  rrr255 lines - key(11) mrn(8) visitno(7) dos(8) cpt(5)
// out: one line per charge for rrr256 -
//        key(11) docp(2) space src(4) space note
//      src FEED = the radiologist whose feed the read came in on
//          NONE = not found, docp stays 00 for the coder, note says why
//
// each radiologist has his own results feed into openemr (vitl filters it
// on his npi), so the feed the read came in on
// (DiagnosticReport.performer -> Organization) is the radiologist. on
// site 2400 every read in 30 days had a feed and 98.8% were signed by
// that feed's radiologist.
//
// prints counts only, no patient data.
require_once(__DIR__ . '/../../vendor/autoload.php');
use GuzzleHttp\Client;

// procedure_providers feeds, one per radiologist
const DOCP_BY_FEED_ORG = [
    '99a32cd3-c075-4306-80de-60f07957d3ed' => '06', // vitl-dan
    '99a20467-a142-4ebe-be10-97b01f4e28c8' => '08', // vhie-trent
    '99a35d7a-5da8-4384-a98d-171edee493c8' => '09', // vhie-jed
    '99a20467-a147-4412-928f-7305397f1293' => '10', // vhie-andy
];

$in_file  = $argv[1] ?? '';
$out_file = $argv[2] ?? '';
if (!is_readable($in_file) || empty($out_file)) {
    echo "usage: php rri_docp.php <rrr255 extract> <answers out>\n";
    exit(1);
}

$base_url = getenv('BASE_OEMR_URL');
$site_id  = getenv('OEMR_RRI_SITE_ID');
$fhir_url = $base_url . '/apis/' . $site_id . '/fhir/';

$client = new Client(['verify' => false, 'timeout' => 60, 'connect_timeout' => 10]);

try {
    $response = $client->post($base_url . '/oauth2/' . $site_id . '/token', [
        'form_params' => [
            'grant_type' => 'password',
            'client_id'  => getenv('OEMR_TEST_CLIENT_ID'),
            'scope'      => "openid fhirUser online_access user/Observation.read " .
                            "user/Patient.read user/DiagnosticReport.read",
            'user_role'  => 'users',
            'username'   => getenv('OEMR_RRI_USERNAME'),
            'password'   => getenv('OEMR_RRI_PASSWORD'),
        ],
    ]);
    $bearer = json_decode((string) $response->getBody(), true)['access_token'] ?? null;
} catch (\Exception $e) {
    $bearer = null;
}
if (empty($bearer)) {
    echo "could not log in to openemr, nothing done\n";
    exit(1);
}

$headers = [
    'Authorization' => 'Bearer ' . $bearer,
    'Accept'        => 'application/json',
];

// all entries of a fhir search, following the bundle's next links;
// null when the api call fails
function fhirSearch(Client $client, array $headers, string $url): ?array
{
    $entries = [];
    while (!empty($url)) {
        try {
            $res = $client->get($url, ['headers' => $headers]);
        } catch (\Exception $e) {
            error_log("fhir call failed: " . $e->getMessage());
            return null;
        }
        $bundle  = json_decode((string) $res->getBody(), true);
        $entries = array_merge($entries, $bundle['entry'] ?? []);
        $url     = null;
        foreach ($bundle['link'] ?? [] as $link) {
            if (($link['relation'] ?? '') === 'next') {
                $url = $link['url'];
            }
        }
    }
    return $entries;
}

// docp of the feed the read came in on, null when there is no report
// for it or the feed isn't one of the four
function feedDocp(array $reports, string $obs_id): ?string
{
    foreach ($reports as $entry) {
        $r = $entry['resource'] ?? [];
        $has_obs = false;
        foreach ($r['result'] ?? [] as $result) {
            if (str_ends_with($result['reference'] ?? '', 'Observation/' . $obs_id)) {
                $has_obs = true;
            }
        }
        if (!$has_obs) {
            continue;
        }
        foreach ($r['performer'] ?? [] as $performer) {
            $org = preg_replace('#^.*Organization/#', '', $performer['reference'] ?? '');
            if (isset(DOCP_BY_FEED_ORG[$org])) {
                return DOCP_BY_FEED_ORG[$org];
            }
        }
    }
    return null;
}

$pt_cache  = [];
$dr_cache  = [];
$obs_cache = [];
// api errors are counted apart from not found so a missing scope or an
// openemr outage stands out (openemr still issues the token without a
// scope the client isn't registered for, it just refuses those calls)
$counts    = ['FEED' => 0, 'NONE' => 0, 'ERROR' => 0];
$out       = [];

foreach (file($in_file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
    $line     = str_pad($line, 39);
    $key      = substr($line, 0, 11);
    $mrn      = ltrim(substr($line, 11, 8), '0');
    $visit_no = substr($line, 19, 7);
    $dos      = substr($line, 26, 8);
    $cpt      = substr($line, 34, 5);
    if (substr($visit_no, 0, 1) == '0') {
        $visit_no = "1" . $visit_no;
    }

    $answer = function (string $docp, string $src, string $note) use (&$out, &$counts, $key) {
        $out[] = $key . $docp . ' ' . $src . ' ' . $note;
        $counts[$note === 'OPENEMR API ERROR' ? 'ERROR' : $src]++;
    };

    if (!array_key_exists($mrn, $pt_cache)) {
        $pts = fhirSearch($client, $headers, $fhir_url . 'Patient?identifier=' . urlencode($mrn));
        if ($pts === null) {
            $pt_cache[$mrn] = 'ERROR';
        } elseif (count($pts) > 1) {
            $pt_cache[$mrn] = false;
        } else {
            $pt_cache[$mrn] = $pts[0]['resource']['id'] ?? null;
        }
    }
    $pt_uuid = $pt_cache[$mrn];
    if ($pt_uuid === 'ERROR') {
        $answer('00', 'NONE', 'OPENEMR API ERROR');
        continue;
    }
    if ($pt_uuid === false) {
        $answer('00', 'NONE', 'MORE THAN ONE PATIENT FOR MRN');
        continue;
    }
    if (empty($pt_uuid)) {
        $answer('00', 'NONE', 'NO PATIENT IN OPENEMR');
        continue;
    }

    $obs_key = $pt_uuid . '|' . $visit_no;
    if (!array_key_exists($obs_key, $obs_cache)) {
        $obs_cache[$obs_key] = fhirSearch(
            $client,
            $headers,
            $fhir_url . 'Observation?patient=' . $pt_uuid . '&external_id=' . urlencode($visit_no)
        );
    }
    if (!array_key_exists($pt_uuid, $dr_cache)) {
        $dr_cache[$pt_uuid] = fhirSearch($client, $headers, $fhir_url . 'DiagnosticReport?patient=' . $pt_uuid);
    }
    $all_obs = $obs_cache[$obs_key];
    $reports = $dr_cache[$pt_uuid];
    if ($all_obs === null || $reports === null) {
        $answer('00', 'NONE', 'OPENEMR API ERROR');
        continue;
    }

    // reads for this dos, then the ones whose procedure shows our cpt
    $dos_found = false;
    $cpt_obs   = [];
    foreach ($all_obs as $entry) {
        $obs = $entry['resource'] ?? [];
        $effective = $obs['effectiveDateTime'] ?? '';
        if (empty($effective) || (new DateTimeImmutable($effective))->format('Ymd') !== $dos) {
            continue;
        }
        $dos_found = true;
        if (str_contains($obs['code']['coding'][0]['display'] ?? '', $cpt)) {
            $cpt_obs[] = $obs;
        }
    }
    if (!$dos_found) {
        $answer('00', 'NONE', 'NO READ FOR DOS');
        continue;
    }
    if (empty($cpt_obs)) {
        $answer('00', 'NONE', 'NO READ SHOWS CPT ' . $cpt);
        continue;
    }

    // a re-sent read comes in on the same feed, so any copy will do as
    // long as the feeds agree
    $docps = [];
    foreach ($cpt_obs as $obs) {
        $docp = feedDocp($reports, $obs['id'] ?? '');
        if ($docp !== null) {
            $docps[$docp] = true;
        }
    }
    if (count($docps) === 1) {
        $answer((string) array_key_first($docps), 'FEED', '');
    } elseif (empty($docps)) {
        $answer('00', 'NONE', 'NO REPORT OR FEED FOR READ');
    } else {
        $answer('00', 'NONE', 'READS FOR CPT ' . $cpt . ' ON DIFFERENT FEEDS');
    }
}

$tmp = $out_file . '.tmp';
file_put_contents($tmp, implode("\n", $out) . (empty($out) ? '' : "\n"));
rename($tmp, $out_file);

echo "rri_docp: " . count($out) . " charges, feed " . $counts['FEED'] .
     ", not found " . $counts['NONE'] . ", api errors " . $counts['ERROR'] . "\n";
