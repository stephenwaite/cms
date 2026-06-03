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

function suggestIcd10Codes(Client $guzzle, string $interp, string $cpt): ?array
{
    $system = <<<PROMPT
You are a radiology ICD-10-CM coding assistant. Given a radiology report and the CPT code
for the procedure performed, return a JSON array of suggested diagnosis codes.

For each code include:
- code: ICD-10-CM code
- description: full code description
- confidence: high/medium/low
- rationale: one sentence citing the specific finding or indication
- specificity_check: if the code contains "unspecified" in its description or ends in
  a 9, you MUST explain here why no specific code is supportable from the report text.
  If the code is specific, confirm the exact text that supports the specificity (e.g.
  "posterior horn documented in findings"). "n/a" is not acceptable — always cite text.

CODING HIERARCHY — follow in order:

1. Begin with IMPRESSION. If the impression documents a specific, confirmed finding
   or diagnosis, code that finding as primary using the most specific ICD-10-CM code
   available.
   a. SPECIFICITY RULE: Never select an "unspecified" code (typically ending in .9,
      or containing "unspecified" in the description) when the impression documents
      detail that supports a more specific code. In particular:
      - Laterality: if the report states left/right/bilateral, use the lateralized code.
      - Anatomic subsite: if the report specifies a substructure (e.g. "posterior horn
        of medial meniscus", "right middle lobe", "distal LAD"), select the code for
        that subsite, not the parent "unspecified site" code.
      - Acuity/chronicity: if the report distinguishes acute vs. chronic, old vs.
        current, traumatic vs. degenerative, pick the matching code family.
      - Type/morphology: if the report characterizes the lesion (complex vs. simple
        tear, displaced vs. nondisplaced, comminuted, etc.), reflect that in the code.
      Only fall back to an unspecified code if the report genuinely does not contain
      the distinguishing detail. If you do return an unspecified code, the rationale
      must state what detail was missing from the report.

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

6. Never suggest an unspecified code (codes ending in 9, or descriptions containing
   "unspecified") when a more specific code is supportable from the report text.
   If the text does not support a specific code, omit the finding rather than
   falling back to unspecified.

EXAMPLES:

EXAMPLE 1 — specific finding in impression:
INDICATION: Right knee pain after fall
IMPRESSION: Complex tear of the posterior horn of the medial meniscus, right knee.
Correct primary: S83.231A (Complex tear of medial meniscus, current injury, right knee, initial encounter)
Wrong: M23.92 (unspecified knee derangement) — fails specificity (laterality, morphology, subsite all documented)

EXAMPLE 2 — normal impression, specific indication:
INDICATION: Follow-up of known 4mm right MCA aneurysm
IMPRESSION: No change. No new aneurysm. No hemorrhage.
Correct primary: I67.1 (Cerebral aneurysm, nonruptured)
Wrong: Z09 / Z51 aftercare — normal result does not eliminate the underlying diagnosis

EXAMPLE 3 — rule-out only:
INDICATION: Chest pain, rule out PE
IMPRESSION: No pulmonary embolism. Lungs clear.
Correct primary: R07.9 (Chest pain, unspecified)
Wrong: I26.99 — never code rule-out as confirmed

EXAMPLE 4 — incidental finding:
INDICATION: Cough
IMPRESSION: No acute cardiopulmonary process. Incidental 6mm right lower lobe pulmonary nodule.
Correct: R05.9 (Cough) primary, R91.1 (Solitary pulmonary nodule) secondary, lower confidence

MENISCUS TEAR SPECIFICITY — ICD-10-CM M23.2xx:
- Posterior horn of medial meniscus → M23.221 (right) / M23.222 (left)
- Anterior horn of medial meniscus  → M23.211 (right) / M23.212 (left)
- Other medial meniscus (body/NOS)  → M23.201 (right) / M23.202 (left) ← last resort only
- Posterior horn of lateral meniscus → M23.261 (right) / M23.262 (left)
- Anterior horn of lateral meniscus  → M23.251 (right) / M23.252 (left)
- Other lateral meniscus (body/NOS) → M23.261 (right) / M23.262 (left)
When both posterior horn AND body are documented, code posterior horn as primary
(M23.221/M23.222) and note the body involvement in rationale. Do not use M23.20x
(unspecified) when a location is explicitly stated in the report.

MUCOID DEGENERATION (no discrete tear):
- Meniscus mucoid degeneration without tear → M23.892 (left) / M23.891 (right)
  Do NOT use M23.2xx — that family requires a documented tear.
- ACL/ligament mucoid degeneration → M67.862 (left) / M67.861 (right)

SYMPTOM CODES:
- Only code symptoms explicitly documented in the report or indication.
- Never infer a symptom (e.g. stiffness, swelling) that is not stated.

FINAL CHECKS before returning:
- Did I pick the most specific code the documentation supports? (laterality, subsite, acuity, morphology)
- Did I avoid coding any rule-out/probable/possible/suspected condition as confirmed?
- For a normal study, did I check the clinical indication for a codeable underlying condition?
- Did I include incidental findings as secondary, lower confidence?
- Did I avoid Z51 unless aftercare is explicitly documented?
PROMPT;

    $clean_interp = preg_replace('/(Please note:|Electronically Signed by:).*$/si', '', $interp);
    $user_message = "CPT: {$cpt}\n\nInterpretation:\n{$clean_interp}";

    $attempts = 0;
    $max_attempts = 3;

    while ($attempts < $max_attempts) {
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
                    'model'      => 'claude-sonnet-4-6',
                    'max_tokens' => 1024,
                    'system'     => [
                        [
                            'type'          => 'text',
                            'text'          => $system,
                            'cache_control' => ['type' => 'ephemeral'],
                        ],
                    ],
                    'tools' => [
                        [
                            'name'        => 'submit_diagnosis_codes',
                            'description' => 'Submit suggested ICD-10-CM diagnosis codes for the radiology report.',
                            'input_schema' => [
                                'type'       => 'object',
                                'properties' => [
                                    'codes' => [
                                        'type'  => 'array',
                                        'items' => [
                                            'type'       => 'object',
                                            'properties' => [
                                                'code'        => ['type' => 'string', 'description' => 'ICD-10-CM code'],
                                                'description' => ['type' => 'string', 'description' => 'Full code description'],
                                                'confidence'  => ['type' => 'string', 'enum' => ['high', 'medium', 'low']],
                                                'rationale'   => ['type' => 'string', 'description' => 'One sentence citing the specific finding or indication'],
                                            ],
                                            'required' => ['code', 'description', 'confidence', 'rationale'],
                                        ],
                                    ],
                                ],
                                'required' => ['codes'],
                            ],
                        ],
                    ],
                    'tool_choice' => ['type' => 'tool', 'name' => 'submit_diagnosis_codes'],
                    'messages'    => [
                        ['role' => 'user', 'content' => $user_message],
                    ],
                ],
            ]);

            $body = json_decode((string) $response->getBody(), true);

            // Cache verification — keep during rollout, remove later
            $usage = $body['usage'] ?? [];
            error_log(sprintf(
                "Claude usage: input=%d, output=%d, cache_write=%d, cache_read=%d",
                $usage['input_tokens'] ?? 0,
                $usage['output_tokens'] ?? 0,
                $usage['cache_creation_input_tokens'] ?? 0,
                $usage['cache_read_input_tokens'] ?? 0,
            ));

            if (($body['stop_reason'] ?? '') === 'max_tokens') {
                error_log("Claude: response truncated at max_tokens, consider raising limit");
            }

            foreach ($body['content'] ?? [] as $block) {
                if (($block['type'] ?? '') === 'tool_use'
                    && ($block['name'] ?? '') === 'submit_diagnosis_codes') {
                        return $block['input']['codes'] ?? [];
                }
            }

            error_log("Claude: no tool_use block found, stop_reason=" . ($body['stop_reason'] ?? 'null'));
            return [];

        } catch (\GuzzleHttp\Exception\ConnectException $e) {
            $attempts++;
            if ($attempts < $max_attempts) {
                sleep(2 ** $attempts);
                continue;
            }
            error_log("Claude unavailable (connection timeout): " . $e->getMessage());
            return null;

        } catch (\GuzzleHttp\Exception\RequestException $e) {
            $status = $e->hasResponse() ? $e->getResponse()->getStatusCode() : 0;
            $detail = $e->hasResponse()
                ? (string) $e->getResponse()->getBody()
                : $e->getMessage();

            if (in_array($status, [429, 529, 500, 502, 503, 504]) && $attempts < $max_attempts - 1) {
                $attempts++;
                sleep(2 ** $attempts);
                continue;
            }
            error_log("Claude request failed ($status): $detail");
            return null;

        } catch (\Exception $e) {
            error_log("Claude error: " . $e->getMessage());
            return null;
        }
    }

    return null;
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
    $strip_procedure = false;
    /* if (!empty($context) && $context == 'pdf') {
        $strip_procedure = str_contains(
            strtoupper(readline("Strip PROCEDURE headers from reports? (y or Y) ")), 'Y'
        );
    } else {
        $strip_procedure = false;
    } */

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
            // adding to pdf without prompting
            echo "Adding {$coding_display} to PDF\n";
            if ($pdf_page_count > 0) {
                $pdf->ezNewPage();
            }
            $pdf->ezText($note, 10);
            $pdf_page_count++;
        } else {
            if (!$ask_claude) {
                echo $note . "\n";
            }
            if ($ask_claude && str_contains($coding_display, $rri_cpt)) {
                $icd10_suggestions = suggestIcd10Codes($guzzle, $interp, $rri_cpt);
                if ($icd10_suggestions === null) {
                    echo "(Claude unavailable — code manually)\n";
                } elseif (empty($icd10_suggestions)) {
                    echo "(no ICD-10 suggestions returned)\n";
                } else {
                    foreach ($icd10_suggestions as $s) {
                        echo sprintf("[%s] %s (%s) — \"%s\"\n",
                            $s['confidence'], $s['code'], $s['description'], $s['rationale']
                        );
                    }
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
