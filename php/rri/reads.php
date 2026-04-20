<?php
require_once(__DIR__ . '/../../vendor/autoload.php');
use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

$cms_user = getenv('USER');

// first remove tmp courier cached file in case some other script put it there
$context = $argv[1] ?? null;
$ask_claude = strtoupper($argv[1] ?? '') === 'Y';

if (!empty($context) && $context == 'pdf') {
    // create temp dir for cached courier font
    $tmp_dir = '/tmp/reads' . $cms_user;
    putenv('TMPDIR=' . $tmp_dir);
    if (!is_dir($tmp_dir)) {
        mkdir($tmp_dir);
    }
    $pdf = new Cezpdf();
    $pdf->selectFont('Courier');
}

const CT_QUALIFYING_CPT = [
    '70490' => true, '70491' => true, '70492' => true,
    '75571' => true, '75572' => true, '75573' => true, '75574' => true,
    '70498' => true, '71250' => true, '71260' => true, '71270' => true,
    '71275' => true, '72125' => true, '72126' => true, '72127' => true,
    '72128' => true, '72129' => true, '72130' => true, '74150' => true,
    '74160' => true, '74170' => true, '74174' => true, '74175' => true,
    '74176' => true, '74177' => true, '74178' => true,
];

function isQualifyingCtCpt(string $coding_display): ?string
{
    foreach (CT_QUALIFYING_CPT as $code => $unused) {
        if (str_contains($coding_display, $code)) {
            return $code;
        }
    }
    return null;
}

function getQualifyingLungFindings(string $note): array
{
    $note_lower = strtolower($note);
    $no_nodule  = str_contains($note_lower, 'no pulmonary nodule');
    return [
        'no_pulmonary_nodule' => $no_nodule,
        'pulmonary_nodule'    => !$no_nodule && str_contains($note_lower, 'pulmonary nodule'),
        'includes_guidelines' => str_contains($note_lower, 'fleischner society 2017'),
    ];
}

function suggestIcd10Codes(Client $guzzle, string $interp, string $cpt): array
{
    $system = <<<PROMPT
You are a radiology ICD-10-CM coding assistant. Given a radiology report and the CPT code
for the procedure performed, return a JSON array of suggested diagnosis codes.

For each code include:
- code: ICD-10-CM code
- description: full code description
- confidence: high/medium/low
- rationale: one sentence citing the specific finding or indication

CODING HIERARCHY — follow in order:

1. Begin with IMPRESSION. If the impression documents a specific, confirmed finding
   or diagnosis, code that finding as primary using the most specific ICD-10-CM code
   available.

2. If the IMPRESSION is normal, unremarkable, or negative, look to CLINICAL INDICATION:
   a. If the indication contains a specific codeable condition (e.g. "cerebral aneurysm,
      nonruptured", "lung nodule"), code that condition — a normal result does not
      eliminate the underlying diagnosis.
   b. If the indication is a symptom or vague (e.g. "headache", "rule out PE"), code
      the symptom.

3. Never code "rule out," "suspected," "probable," "possible," or "questionable"
   conditions as confirmed — code the sign, symptom, or abnormal finding instead
   per ICD-10-CM outpatient guidelines.

4. Never return a Z51 aftercare code solely because findings are normal or the study
   is a follow-up. Z51 requires explicit documentation of an aftercare encounter.

5. If IMPRESSION reveals incidental pathology not mentioned in the indication, include
   it as a secondary suggestion at lower confidence.

Return ONLY a valid JSON array. No preamble, no markdown, no backticks.
PROMPT;

    $clean_interp = preg_replace('/(Please note:|Electronically Signed by:).*$/si', '', $interp);
    $user_message = "CPT: {$cpt}\n\nInterpretation:\n{$clean_interp}";

    try {
        $response = $guzzle->post('https://api.anthropic.com/v1/messages', [
            'headers' => [
                'x-api-key'         => getenv('ANTHROPIC_API_KEY'),
                'anthropic-version' => '2023-06-01',
                'Content-Type'      => 'application/json',
            ],
            'timeout'         => 30,
            'connect_timeout' => 10,
            'json' => [
                'model'      => 'claude-sonnet-4-20250514',
                'max_tokens' => 1024,
                'system'     => $system,
                'messages'   => [
                    ['role' => 'user', 'content' => $user_message]
                ],
            ],
        ]);
        $body = json_decode((string) $response->getBody(), true);
        $raw  = $body['content'][0]['text'] ?? '[]';
        return json_decode($raw, true) ?? [];
    } catch (\GuzzleHttp\Exception\ConnectException $e) {
        echo "Claude unavailable (connection timeout) \n";
    } catch (\GuzzleHttp\Exception\RequestException $e) {
        echo "Claude request failed: " . $e->getMessage() . " \n";
    } catch (\Exception $e) {
        echo "Claude error: " . $e->getMessage() . " \n";
    }
    return [];
}

$filename = getenv('HOME') . "/W2" . getenv('tid') . $cms_user;
$file = file_get_contents($filename);
$mrn = ltrim(substr($file, 0, 8), '0');
$visit_no = substr($file, 8, 7);
$rri_cpt = substr($file, 34, 5);

if (substr($visit_no, 0, 1) == '0') {
    $visit_no = "1" . $visit_no;
}

$charcur_key = substr($file, 15, 11);
$billing_tape_date_of_service = substr($file, 26, 8);

$base_url = getenv('BASE_OEMR_URL');
$site_id  = getenv('OEMR_RRI_SITE_ID');
$base_uri = $base_url . '/oauth2/' . $site_id . '/token';

$guzzle = new Client(['verify' => false]);

$response = $guzzle->post($base_uri, [
    'form_params' => [
        'grant_type' => 'password',
        'client_id'  => getenv('OEMR_TEST_CLIENT_ID'),
        'scope'      => "openid fhirUser online_access offline_access user/Observation.read user/Patient.read",
        'user_role'  => 'users',
        'username'   => getenv('OEMR_RRI_USERNAME'),
        'password'   => getenv('OEMR_RRI_PASSWORD'),
    ],
]);

$bearer = json_decode((string) $response->getBody(), true)['access_token'];

$client = new Client(['verify' => false]);

$headers = [
    'Authorization' => 'Bearer ' . $bearer,
    'Accept'        => 'application/json',
];

$request = new Request('GET', $base_url . '/apis/' . $site_id . '/fhir/Patient?identifier=' . $mrn, $headers);
$res     = $client->sendAsync($request)->wait();
$ptObj   = json_decode($res->getBody(), true);

$pt_uuid = $ptObj['entry'][0]['resource']['id'] ?? null;
if (empty($pt_uuid)) {
    echo "no patient uuid in the emr for some reason \n";
    exit;
}

$pt_name_array = $ptObj['entry'][0]['resource']['name'][0] ?? null;
$pt_name_text  = ($pt_name_array['family'] ?? '') . ", " . ($pt_name_array['given'][0] ?? '') .
                 " " . ($pt_name_array['given'][1] ?? '');
$pt_birthdate  = $ptObj['entry'][0]['resource']['birthDate'] ?? null;
$pt_dob        = new DateTimeImmutable($pt_birthdate);
$pt_dob_line   = "DOB: " . $pt_dob->format('m-d-Y');

$request = new Request(
    'GET',
    $base_url . '/apis/' . $site_id . '/fhir/Observation?patient=' . $pt_uuid . '&external_id=' . $visit_no,
    $headers
);
$res     = $client->sendAsync($request)->wait();
$jsonObj = json_decode($res->getBody(), true);

if (!empty($jsonObj['entry'])) {
    $note  = '';
    $count = count($jsonObj['entry']);
    $cntr  = 0;

    // in pdf context ask once whether to strip PROCEDURE headers
    // in console context always strip — coders don't need it
    if (!empty($context) && $context == 'pdf') {
        $strip_procedure = str_contains(
            strtoupper(readline("Strip PROCEDURE headers from reports? (y or Y) ")), 'Y'
        );
    } else {
        $strip_procedure = false;
    }

    $pdf_page_count = 0;

    foreach ($jsonObj['entry'] as $entry) {
        $cntr++;
        $coding_display = $entry['resource']['code']['coding'][0]['display'];
        $interp         = $entry['resource']['note'][0]['text'];

        // optionally strip PROCEDURE: header line
        if ($strip_procedure) {
            $interp = preg_replace('/^PROCEDURE:.*\n?(?![A-Z]{2,}:).*\n?/mi', '', $interp);
            $interp = ltrim($interp);
        }

        $coding_display_length = strlen($coding_display);
        $pt_name_text_length   = strlen($pt_name_text);

        if ($coding_display_length > 15 || $pt_name_text_length > 15) {
            $banner_length = ($coding_display_length > $pt_name_text_length)
                ? $coding_display_length
                : $pt_name_text_length;
        } else {
            $banner_length = 15;
        }

        $note  = str_pad('', $banner_length, '#') . "\n";
        $note .= $pt_name_text . "\n";
        $note .= $pt_dob_line . "\n";
        if (!$strip_procedure) {
            $note .= $coding_display . "\n";
        }

        $date_of_order             = $entry['resource']['effectiveDateTime'] ?? '';
        $date_of_order_utc         = new DateTimeImmutable($date_of_order);
        $date_of_order_utc_display = $date_of_order_utc->format('m-d-Y');
        $date_of_order_utc_compare = $date_of_order_utc->format('Ymd');

        if ($date_of_order_utc_compare != $billing_tape_date_of_service) {
            continue;
        }

        $pt_dos_line = 'DOS: ' . $date_of_order_utc_display;
        $note .= $pt_dos_line . "\n";
        $note .= str_pad('', $banner_length, '#') . "\n\n";
        $note .= $interp . "\n";

        if (!empty($context) && $context == 'pdf') {
            // per-report prompt for PDF inclusion
            $add_to_pdf = str_contains(
                strtoupper(readline("Add {$coding_display} to PDF? (y or Y) ")), 'Y'
            );
            if ($add_to_pdf) {
                if ($pdf_page_count > 0) {
                    $pdf->ezNewPage();
                }
                $pdf->ezText($note, 10);
                $pdf_page_count++;
            }
        } else {
            echo $note . "\n";
            if ($ask_claude && str_contains($coding_display, $rri_cpt)) {
                $icd10_suggestions = suggestIcd10Codes($guzzle, $interp, $rri_cpt);
                foreach ($icd10_suggestions as $s) {
                    echo sprintf("[%s] %s (%s) — \"%s\"\n",
                        $s['confidence'],
                        $s['code'],
                        $s['description'],
                        $s['rationale']
                    );
                }
            }
        }
    }
} else {
    echo "read not available for some reason, try mpages please \n";
}

if (!empty($context) && $context == 'pdf') {
    if ($pdf_page_count > 0) {
        $pdf_data = $pdf->ezOutput();
        file_put_contents($charcur_key . ".pdf", $pdf_data);
        file_put_contents('wcomp1', $charcur_key);
        echo "saved pdf under rri \n";
        $filename = exec('pwd') . "/" . $charcur_key . ".pdf";
        $tty      = exec('tty');
        echo "downloading $filename \n";
        echo " for $cms_user \n";
        if ($cms_user == 'lynda') {
            $cmd = "sz $filename > $tty < $tty";
            exec($cmd, $output);
        } else {
            echo "                not implemented for " . $cms_user . "\n";
        }
    } else {
        echo "No reports added to PDF, nothing saved.\n";
    }
}
