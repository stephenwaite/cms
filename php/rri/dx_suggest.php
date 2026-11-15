<?php

/**
 * ICD-10 suggestions from a radiology report, and the checks made on them.
 *
 * Shared by dx_compare.php. reads.php still carries its own copy of the prompt; when it is
 * changed to use this file, icd10SystemPrompt() is the one place the prompt lives.
 *
 * Nothing here prints or stores patient data: the callers decide what is written where.
 */

declare(strict_types=1);

const DX_API_VERSION = '2023-06-01';

/** A claim line points to at most four diagnoses, so the coders' prompt asks for no more. */
const DX_MAX_CODES = 4;

/** The columns of Gifford's charge file when it has no header line, before the three report columns. */
const DX_SAMPLE_COLUMNS = [
        'encounter_num', 'pat_mrn', 'pat_last', 'pat_first', 'pat_addr1', 'pat_addr2', 'pat_city', 'pat_state',
        'pat_zip', 'pat_phone', 'pat_email', 'pat_sex', 'pat_dob', 'enc_admit_date', 'enc_admit_time',
        'enc_discharge_date', 'enc_workcomp', 'enc_accident_date', 'enc_accident_time', 'enc_auth_num',
        'enc_ref_ind', 'enc_dx_codes', 'prov_ref_name', 'prov_ref_npi', 'prov_rend_name', 'prov_rend_npi',
        'chg_dos', 'chg_cpt', 'chg_hcpcs', 'chg_mod1', 'chg_mod2', 'chg_mod3', 'chg_units', 'chg_pos',
        'chg_clinical', 'chg_accession', 'chg_location', 'chg_dept', 'chg_comment', 'guar_last', 'guar_first',
        'guar_addr1', 'guar_addr2', 'guar_city', 'guar_state', 'guar_zip', 'guar_ssn', 'emp_name', 'emp_addr1',
        'emp_addr2', 'emp_city', 'emp_state', 'emp_zip', 'ins1_rank', 'ins1_mnemonic', 'ins1_payer_name',
        'ins1_payer_addr1', 'ins1_payer_addr2', 'ins1_payer_city', 'ins1_payer_state', 'ins1_payer_zip',
        'ins1_payer_phone', 'ins1_payer_contact', 'ins1_member_id', 'ins1_cert_num', 'ins1_group_num',
        'ins1_group_name', 'ins1_sub_name', 'ins1_sub_dob', 'ins1_sub_gender', 'ins1_sub_ssn',
        'ins1_sub_employer', 'ins1_relate', 'ins2_rank', 'ins2_mnemonic', 'ins2_payer_name',
        'ins2_payer_addr1', 'ins2_payer_addr2', 'ins2_payer_city', 'ins2_payer_state', 'ins2_payer_zip',
        'ins2_payer_phone', 'ins2_payer_contact', 'ins2_member_id', 'ins2_cert_num', 'ins2_group_num',
        'ins2_group_name', 'ins2_sub_name', 'ins2_sub_dob', 'ins2_sub_gender', 'ins2_sub_ssn',
        'ins2_sub_employer', 'ins2_relate', 'ins3_rank', 'ins3_mnemonic', 'ins3_payer_name',
        'ins3_payer_addr1', 'ins3_payer_addr2', 'ins3_payer_city', 'ins3_payer_state', 'ins3_payer_zip',
        'ins3_payer_phone', 'ins3_payer_contact', 'ins3_member_id', 'ins3_cert_num', 'ins3_group_num',
        'ins3_group_name', 'ins3_sub_name', 'ins3_sub_dob', 'ins3_sub_gender', 'ins3_sub_ssn',
        'ins3_sub_employer', 'ins3_relate',
];

const DX_REPORT_COLUMNS = ['report_signed', 'report_status', 'report_text'];

const DX_CLASSES = [
    'primary exact', 'other exact', 'primary same category', 'other same category', 'no overlap',
    'no suggestion', 'gifford sent none',
];

/**
 * The coding instructions: reads.php's prompt, word for word.
 */
function icd10SystemPrompt(): string
{
    return <<<'PROMPT'
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
- For pain symptoms, use the most anatomically specific pain code available.
  Prefer site-specific codes (e.g. M25.562 Pain in left knee, M54.50 Low back pain)
  over generic R52 (Pain, unspecified). Only use R52 if no site-specific pain code exists.

INDETERMINATE / UNCHARACTERIZED MASSES:
- If a mass or lesion is described as "indeterminate," "uncharacterized," "cannot be
  further characterized," or explicitly does NOT meet criteria for benign or malignant,
  code it as uncertain behavior (D37-D48 family), NOT as benign (D10-D36) or malignant.
- Never assign a "benign neoplasm" code when the report states the lesion does not meet
  benign criteria or remains indeterminate.

FINAL CHECKS before returning:
- Did I pick the most specific code the documentation supports? (laterality, subsite, acuity, morphology)
- Did I avoid coding any rule-out/probable/possible/suspected condition as confirmed?
- For a normal study, did I check the clinical indication for a codeable underlying condition?
- Did I include incidental findings as secondary, lower confidence?
- Did I avoid Z51 unless aftercare is explicitly documented?
PROMPT;
}

/**
 * What the practice's coders add to the instructions: what the facility's clinical indication
 * is for when a request carries one (it does only on the second of two steps, see
 * icd10NeedsIndication()), what to code on films taken during surgery, and their quick-pick
 * list of preferred codes. The hierarchy itself is reads.php's and is not changed. Fixed text for a given list, so it caches with the rest of the prompt.
 *
 * @param list<array{string, string}> $quickPick [code as the coders write it, what it is for]
 */
function icd10CoderAdditions(array $quickPick): string
{
    $picks = implode("\n", array_map(static fn (array $p): string => sprintf('%-9s %s', $p[0], $p[1]), $quickPick));

    return <<<TEXT
ADDITIONS FROM THE PRACTICE'S CODERS. The coding hierarchy above stands as written; these
add to it.

CLINICAL INDICATION FROM THE FACILITY:
Most requests do not have one. A request carries a "Clinical indication from the facility"
only after the report alone gave no codeable reason for the exam. When it is there, it is
the CLINICAL INDICATION for step 2 of the hierarchy and for the films below.

FILMS TAKEN DURING OR RIGHT AFTER SURGERY (intraoperative and immediate postoperative
studies: hardware or implant placement, arthroplasty, fixation, stent or line checks):
- Code the surgical diagnosis, or the clinical indication the facility sent, as primary.
- Do not make an implant, device or postprocedural status code (Z96-Z98) the primary code
  because the report describes hardware in place. It may follow as a secondary code.
- If neither the report nor the facility's indication says why the surgery was done, return
  an empty list. Do not return a status code alone.
- This is only for films during or right after surgery. A study done before surgery
  (pre-operative planning or clearance) is coded by the hierarchy above like any other.

CODERS' NOTES FOR THE EXAM:
- A request may give "Coders' notes for this exam". Follow them.

THE ANSWER:
- Return at most four codes, in the order they belong on the claim: a claim line can point
  to four diagnoses. When more than four apply, keep the four that matter most for the claim
  and leave out the lesser incidental findings.
- The answer has no description field. For each code give the code, the confidence, the
  rationale and the specificity check; the description is looked up from the code.

PREFERRED CODES (the coders' quick-pick list):
When the condition you are coding is one of these, use the listed code. A trailing "-" means
the code still needs its last character or characters (side, encounter) from the report:
never return a code with "-" in it. The specificity rules still apply: when the report
supports a more specific code than the one listed, use the more specific code. The list is the
coders' memory aid and some entries are old: when a listed code is not a current ICD-10-CM
code, or is not the code for the condition named beside it, use the correct current code.
{$picks}
TEXT;
}

/**
 * The JSON the answer must be: a list of codes, each with its confidence and reasons, and its
 * description too when asked as reads.php asks (the coders' prompt leaves the description to
 * be looked up from the code).
 *
 * @return array<string, mixed>
 */
function icd10AnswerSchema(bool $withDescription = true): array
{
    $schema = icd10FullAnswerSchema();
    if (!$withDescription) {
        unset($schema['properties']['codes']['items']['properties']['description']);
        $schema['properties']['codes']['items']['required'] = ['code', 'confidence', 'rationale', 'specificity_check'];
    }

    return $schema;
}

/**
 * @return array<string, mixed>
 */
function icd10FullAnswerSchema(): array
{
    return [
        'type' => 'object',
        'properties' => [
            'codes' => [
                'type' => 'array',
                'items' => [
                    'type' => 'object',
                    'properties' => [
                        'code' => ['type' => 'string', 'description' => 'ICD-10-CM code'],
                        'description' => ['type' => 'string', 'description' => 'Full code description'],
                        'confidence' => ['type' => 'string', 'enum' => ['high', 'medium', 'low']],
                        'rationale' => ['type' => 'string', 'description' => 'One sentence citing the specific finding or indication'],
                        'specificity_check' => ['type' => 'string', 'description' => 'Text from report supporting code specificity'],
                    ],
                    'required' => ['code', 'description', 'confidence', 'rationale', 'specificity_check'],
                    'additionalProperties' => false,
                ],
            ],
        ],
        'required' => ['codes'],
        'additionalProperties' => false,
    ];
}

/**
 * The Messages API request for one study: the report and its CPT codes and, with the coders'
 * additions, the facility's clinical indication and the coders' notes for the exam. Nothing
 * else of the patient's.
 *
 * @param list<string> $cpts
 * @param ?string $additions icd10CoderAdditions()'s text, or null for reads.php's prompt alone
 * @param string $indication the clinical indication the facility sent, when there is one
 * @param list<string> $notes the coders' notes for this exam
 * @return array<string, mixed>
 */
function icd10Request(
    string $model,
    string $effort,
    array $cpts,
    string $report,
    ?string $additions = null,
    string $indication = '',
    array $notes = []
): array {
    // As reads.php sends it: the signature block and what follows it are left off.
    $clean = preg_replace('/(Please note:|Electronically Signed by:).*$/si', '', $report) ?? $report;
    $extra = '';
    if ($additions !== null && trim($indication) !== '') {
        $extra .= 'Clinical indication from the facility: ' . trim($indication) . "\n\n";
    }
    if ($additions !== null && $notes !== []) {
        $extra .= "Coders' notes for this exam:\n- " . implode("\n- ", $notes) . "\n\n";
    }

    return [
        'model' => $model,
        'max_tokens' => 16000,
        'output_config' => [
            'effort' => $effort,
            'format' => ['type' => 'json_schema', 'schema' => icd10AnswerSchema($additions === null)],
        ],
        'system' => [
            [
                'type' => 'text',
                'text' => icd10SystemPrompt() . "\n" . ($additions === null ? '' : "\n" . $additions . "\n"),
                'cache_control' => ['type' => 'ephemeral'],
            ],
        ],
        'messages' => [
            ['role' => 'user', 'content' => 'CPT: ' . implode(', ', $cpts) . "\n\n" . $extra . "Interpretation:\n" . $clean],
        ],
    ];
}

/**
 * Whether an answer shows the report alone gave no codeable reason for the exam, so the
 * study is worth asking about once more with the facility's clinical indication: no code at
 * all, or a first code that only says an examination was done or a device or status is
 * present (Z00-Z09, Z40-Z99). Screening (Z11-Z13) and pregnancy codes are reasons in
 * themselves and do not count.
 *
 * @param list<array{code: string}> $codes the answer's codes, in order
 */
function icd10NeedsIndication(array $codes): bool
{
    foreach ($codes as $c) {
        $code = dxNorm((string) ($c['code'] ?? ''));
        if ($code === '') {
            continue;
        }
        if ($code[0] !== 'Z') {
            return false;
        }
        $category = (int) substr($code, 1, 2);

        return $category <= 9 || $category >= 40;
    }

    return true;
}

/**
 * What a Messages API response says, as the plain record kept for each request.
 *
 * @param array<string, mixed> $body the decoded response
 * @param int $maxCodes keep only this many codes (0: all); the number returned goes in over_cap when cut
 * @return array<string, mixed> outcome, usage, and codes when it succeeded
 */
function icd10ParseResponse(array $body, int $maxCodes = 0): array
{
    $usage = [];
    foreach (['input_tokens', 'output_tokens', 'cache_creation_input_tokens', 'cache_read_input_tokens'] as $key) {
        $usage[$key] = (int) ($body['usage'][$key] ?? 0);
    }
    $stop = (string) ($body['stop_reason'] ?? 'null');
    if ($stop !== 'end_turn') {
        return ['outcome' => 'stopped: ' . $stop, 'usage' => $usage];
    }
    $text = '';
    foreach ($body['content'] ?? [] as $block) {
        if (($block['type'] ?? '') === 'text') {
            $text = (string) ($block['text'] ?? '');
            break;
        }
    }
    $answer = json_decode($text, true);
    if (!is_array($answer) || !is_array($answer['codes'] ?? null)) {
        return ['outcome' => 'answer was not JSON', 'usage' => $usage];
    }
    $codes = [];
    foreach ($answer['codes'] as $c) {
        if (!is_array($c)) {
            continue;
        }
        $codes[] = [
            'code' => (string) ($c['code'] ?? ''),
            'description' => (string) ($c['description'] ?? ''),
            'confidence' => (string) ($c['confidence'] ?? ''),
            'rationale' => (string) ($c['rationale'] ?? ''),
        ];
    }

    if ($maxCodes > 0 && count($codes) > $maxCodes) {
        return ['outcome' => 'succeeded', 'usage' => $usage, 'codes' => array_slice($codes, 0, $maxCodes), 'over_cap' => count($codes)];
    }

    return ['outcome' => 'succeeded', 'usage' => $usage, 'codes' => $codes];
}

/**
 * An ICD-10-CM code upper-cased with its dot (R05.9); '' when it isn't shaped like one.
 */
function dxNorm(string $code): string
{
    $c = preg_replace('/[^A-Z0-9]/', '', strtoupper($code)) ?? '';
    if (preg_match('/^[A-Z][0-9][0-9A-Z][0-9A-Z]{0,4}$/', $c) !== 1) {
        return '';
    }

    return strlen($c) === 3 ? $c : substr($c, 0, 3) . '.' . substr($c, 3);
}

/**
 * A report sent as a JSON string body (\n for a line break), decoded; as sent when it isn't one.
 */
function dxDecodeReport(string $sent): string
{
    if ($sent === '') {
        return '';
    }
    $body = strlen($sent) >= 2 && $sent[0] === '"' && substr($sent, -1) === '"' ? substr($sent, 1, -1) : $sent;
    $text = json_decode('"' . $body . '"');
    if (!is_string($text)) {
        return $sent;
    }

    return str_replace(["\r\n", "\r"], "\n", $text);
}

/**
 * A tab-separated charge file as rows of fields: UTF-8, or UTF-16 with a byte-order mark.
 *
 * @return list<list<string>>
 */
function dxReadTabFile(string $path): array
{
    $bytes = file_get_contents($path);
    if ($bytes === false) {
        fwrite(STDERR, "cannot read $path\n");
        exit(1);
    }
    if (str_starts_with($bytes, "\xFF\xFE")) {
        $text = mb_convert_encoding(substr($bytes, 2), 'UTF-8', 'UTF-16LE');
    } elseif (str_starts_with($bytes, "\xFE\xFF")) {
        $text = mb_convert_encoding(substr($bytes, 2), 'UTF-8', 'UTF-16BE');
    } elseif (str_starts_with($bytes, "\xEF\xBB\xBF")) {
        $text = substr($bytes, 3);
    } else {
        $text = $bytes;
    }
    $lines = preg_split('/\r\n|\n|\r/', $text) ?: [];
    if ($lines !== [] && end($lines) === '') {
        array_pop($lines);
    }

    return array_map(static fn (string $line): array => explode("\t", $line), $lines);
}

/**
 * The file's lines grouped by accession, in file order: codes, Gifford's dx, the report, the payer.
 *
 * @return list<array<string, mixed>>
 */
function dxReadAccessions(string $path): array
{
    $rows = dxReadTabFile($path);
    $names = array_map(
        static fn (string $h): string => strtolower(trim(trim(str_replace("\u{FEFF}", '', $h)), '"')),
        $rows[0] ?? []
    );
    if (in_array('pat_mrn', $names, true) && in_array('chg_cpt', $names, true)) {
        $header = $names;
        $data = array_slice($rows, 1);
    } else {
        $header = [...DX_SAMPLE_COLUMNS, ...DX_REPORT_COLUMNS];
        $data = $rows;
        foreach ($data as $n => $row) {
            if (count($row) !== count($header) && trim(implode('', $row)) !== '') {
                fwrite(STDERR, sprintf("no header line, and line %d has %d fields, not %d\n", $n + 1, count($row), count($header)));
                exit(1);
            }
        }
    }
    $col = array_flip(array_filter($header, static fn (string $h): bool => $h !== ''));
    foreach (['chg_accession', 'chg_cpt', 'enc_dx_codes', 'report_text'] as $needed) {
        if (!isset($col[$needed])) {
            fwrite(STDERR, "the file has no column $needed\n");
            exit(1);
        }
    }
    $get = static fn (array $row, string $name): string => isset($col[$name]) ? trim($row[$col[$name]] ?? '') : '';

    $accessions = [];
    foreach ($data as $n => $row) {
        if (trim(implode('', $row)) === '') {
            continue;
        }
        $key = $get($row, 'chg_accession');
        if ($key === '') {
            continue;
        }
        $k = 'k' . $key;
        $accessions[$k] ??= [
            'accession' => $key, 'lines' => [], 'cpts' => [], 'gifford' => [], 'report' => '',
            'location' => $get($row, 'chg_location'), 'pos' => $get($row, 'chg_pos'),
            'status' => $get($row, 'report_status'), 'payer' => strtoupper($get($row, 'ins1_mnemonic')),
            'clinical' => '',
        ];
        if ($accessions[$k]['clinical'] === '') {
            $accessions[$k]['clinical'] = $get($row, 'chg_clinical');
        }
        $accessions[$k]['lines'][] = $n + 1;
        $code = strtoupper($get($row, 'chg_cpt') !== '' ? $get($row, 'chg_cpt') : $get($row, 'chg_hcpcs'));
        if ($code !== '' && !in_array($code, $accessions[$k]['cpts'], true)) {
            $accessions[$k]['cpts'][] = $code;
        }
        foreach (preg_split('/[,; ]+/', $get($row, 'enc_dx_codes')) ?: [] as $dx) {
            $dx = dxNorm($dx);
            if ($dx !== '' && !in_array($dx, $accessions[$k]['gifford'], true)) {
                $accessions[$k]['gifford'][] = $dx;
            }
        }
        if ($accessions[$k]['report'] === '') {
            $accessions[$k]['report'] = dxDecodeReport($get($row, 'report_text'));
        }
    }

    return array_values($accessions);
}

/**
 * How the suggestions (best first) sit against Gifford's codes for the visit.
 *
 * @param list<string> $gifford
 * @param list<string> $suggested
 */
function dxClassify(array $gifford, array $suggested): string
{
    if ($suggested === []) {
        return 'no suggestion';
    }
    if ($gifford === []) {
        return 'gifford sent none';
    }
    $cats = array_map(static fn (string $g): string => substr($g, 0, 3), $gifford);
    if (in_array($suggested[0], $gifford, true)) {
        return 'primary exact';
    }
    if (array_intersect($suggested, $gifford) !== []) {
        return 'other exact';
    }
    if (in_array(substr($suggested[0], 0, 3), $cats, true)) {
        return 'primary same category';
    }
    foreach ($suggested as $s) {
        if (in_array(substr($s, 0, 3), $cats, true)) {
            return 'other same category';
        }
    }

    return 'no overlap';
}

/**
 * Gifford's primary payer mnemonic as medicaid, medicare, medicare advantage or other.
 */
function dxPayerClass(string $mnemonic): string
{
    $m = strtoupper($mnemonic);
    if (str_starts_with($m, 'MCD')) {
        return 'medicaid';
    }
    if ($m === 'MCR-AB') {
        return 'medicare';
    }

    return str_contains($m, 'MCR') ? 'medicare advantage' : 'other';
}

/**
 * The coders' workbook as gifford-dx-lists.py wrote it: VT Medicaid's cannot-be-primary and
 * unspecified codes, and the Medicare LCD diagnoses for each exam it covers.
 *
 * @return array{made: string, not_primary: array<string, true>, unspecified: array<string, true>, quick_pick: list<array{string, string}>, by_cpt: array<string, array<string, mixed>>}
 */
function dxLoadLists(string $path): array
{
    $data = json_decode((string) file_get_contents($path), true);
    if (!is_array($data) || !isset($data['not_primary'], $data['unspecified'], $data['exams'])) {
        fwrite(STDERR, "$path is not a lists file from gifford-dx-lists.py\n");
        exit(1);
    }
    $byCpt = [];
    foreach ($data['exams'] as $exam) {
        $exam['set'] = array_fill_keys(array_map('strval', $exam['codes']), true);
        foreach ($exam['cpts'] as $cpt) {
            $byCpt['c' . $cpt] ??= $exam;
        }
    }

    return [
        'made' => (string) ($data['made'] ?? ''),
        'not_primary' => array_fill_keys(array_map('strval', $data['not_primary']), true),
        'unspecified' => array_fill_keys(array_map('strval', $data['unspecified']), true),
        'quick_pick' => array_values(array_filter($data['quick_pick'] ?? [], static fn ($p): bool => is_array($p) && count($p) === 2)),
        'by_cpt' => $byCpt,
    ];
}

/**
 * The workbook's exam for these CPT codes, if it has one.
 *
 * @param array<string, mixed> $lists
 * @param list<string> $cpts
 * @return ?array<string, mixed>
 */
function dxExamFor(array $lists, array $cpts): ?array
{
    foreach ($cpts as $cpt) {
        if (isset($lists['by_cpt']['c' . $cpt])) {
            return $lists['by_cpt']['c' . $cpt];
        }
    }

    return null;
}

/**
 * Whether a code is on the exam's list. A range "E24.2 - E24.4" takes every code from the first
 * through any code under the last.
 *
 * @param array<string, mixed> $exam
 */
function dxSupports(array $exam, string $code): bool
{
    $c = str_replace('.', '', $code);
    if (isset($exam['set'][$c])) {
        return true;
    }
    foreach ($exam['ranges'] as [$lo, $hi]) {
        if (strcmp((string) $lo, $c) <= 0 && (strcmp($c, (string) $hi) <= 0 || str_starts_with($c, (string) $hi))) {
            return true;
        }
    }

    return false;
}
