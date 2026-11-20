<?php

/**
 * Answering the radiology quality measures from the report: which measures a study carries,
 * the instructions, the request, and reading the answer. For measure_compare.php and, later,
 * whatever else answers them.
 *
 * The measures, their code sets and gates are the billing system's (rri/load/mea003.cob) and
 * the answers are the coder's own numbers (CD-QP1..3 in rri/coding/mea004.cob), so an answer
 * means the same thing here, in the billing system and in OpenEMR's Gifford module
 * (src/Measure/Measure.php, MeasureRules.php). Keep the three alike.
 */

declare(strict_types=1);

require_once __DIR__ . '/dx_suggest.php';

/** A patient this old or older gets the measure. */
const MEAS_AGE = ['QMM26' => 50, '364' => 35];

/** Measures asked only when Medicare is the primary payer: their answers go on the claim as G-codes. */
const MEAS_ON_CLAIM = ['405', '406'];

/** mea004's A1-364-DX: with one of these on the charge, a 364 answer of 1, 2 or 3 is the exclusion (4). */
/** Added to the facility's indication when the ordering physician is a hematology-oncology one. */
const MEAS_HEMONC_NOTE = 'ordered by a hematology-oncology physician';

const MEAS_364_EXCLUSION_DX = '/^(C|D0|Z08|Z85|Z122)/';

/**
 * Each measure's answers: the coder's number, what it means, and the code it sends (the
 * G-codes of a claims measure, or the ACR numerator code; '' when nothing is sent).
 */
const MEAS_OPTIONS = [
    'QMM26' => [
        1 => ['AAA under 5.5 cm: follow-up recommended per guideline, source cited', 'PM002'],
        2 => ['AAA 5.5 cm or more: follow-up recommended and direct communication documented', 'PM102'],
        3 => ['Negative for AAA: states no future screening needed', 'PM202'],
        4 => ['Performance not met: recommendation, source, communication or statement missing', 'PNM02'],
        5 => ['Exception: under active surveillance by a vascular specialist, AAA unchanged', 'PE002'],
    ],
    '364' => [
        0 => ['No incidental pulmonary nodule: not eligible', ''],
        1 => ['Nodule: follow-up (or none) recommended per guideline, with interval, modality and source (performance met)', 'G9345'],
        2 => ['Nodule: medical reason for no recommendation (exception)', 'G9755'],
        3 => ['Nodule: recommendation not per guideline or missing, reason not given (performance not met)', 'G9347'],
        4 => ['Nodule, but cancer history, heavy smoker or lung cancer screening: excluded', 'M1018'],
    ],
    '405' => [
        0 => ['No incidental lesion', 'G9551'],
        1 => ['Lesion, no follow-up imaging recommended', 'G9548 G9547'],
        2 => ['Lesion, follow-up imaging indicated for a medical reason', 'G9549 G9547'],
        3 => ['Lesion, follow-up imaging recommended or not addressed (performance not met)', 'G9550 G9547'],
    ],
    '406' => [
        0 => ['No incidental thyroid nodule under 1.0 cm', 'G9557'],
        1 => ['Nodule, follow-up imaging recommended (performance not met)', 'G9554 G9552'],
        2 => ['Nodule, follow-up imaging recommended for a medical reason', 'G9555 G9552'],
        3 => ['Nodule, follow-up imaging not recommended', 'G9556 G9552'],
    ],
];

/**
 * The measures each procedure code carries, before the gates.
 *
 * @return array<string, list<string>>
 */
function measSets(): array
{
    $sets = [];
    $add = static function (array $measures, array $codes) use (&$sets): void {
        foreach ($codes as $code) {
            $sets[(string) $code] = $measures;
        }
    };
    $add(['QMM26'], [76706]);
    $add(['364'], [75571, 75572, 75573, 75574, 72128, 72129, 72130, 74174, 74175]);
    $add(['405'], [72131, 72191, 72192, 72193, 72194, 72195, 72196, 72197, 72198, 74181, 74182, 74183]);
    $add(['406'], [70471, 70486, 70487, 70488, 70540, 70542, 70543, 72141, 72142, 72156, 71550, 71551, 71552]);
    $add(['364', '405'], [71275, 74150, 74160, 74170, 74176, 74177, 74178]);
    $add(['364', '406'], [70490, 70491, 70492, 70498, 72125, 72126, 72127]);
    $add(['405', '406'], [71271, 71555]);
    $add(['364', '405', '406'], [71250, 71260, 71270]);

    return $sets;
}

/**
 * The measures a study carries: each measure of any of its codes whose gate the patient
 * passes. QMM26 at 50 and over and 364 at 35 and over, any payer; 405 and 406 when Medicare is
 * primary. An unknown age passes no age gate.
 *
 * @param list<string> $cpts
 * @return list<string> in the order QMM26, 364, 405, 406
 */
function measFor(array $cpts, ?int $age, bool $medicare): array
{
    $sets = measSets();
    $found = [];
    foreach ($cpts as $cpt) {
        foreach ($sets[$cpt] ?? [] as $measure) {
            $applies = in_array($measure, MEAS_ON_CLAIM, true) ? $medicare : ($age !== null && $age >= MEAS_AGE[$measure]);
            if ($applies) {
                $found[(string) $measure] = true;
            }
        }
    }

    // PHP keeps '364' as the integer key 364: hand the names back as strings.
    return array_values(array_filter(array_map('strval', array_keys(MEAS_OPTIONS)), static fn (string $m): bool => isset($found[$m])));
}

/**
 * Age in whole years on a date, both as Gifford writes them (m/d/Y); null when either is not a date.
 */
function measAge(string $born, string $on): ?int
{
    $b = \DateTimeImmutable::createFromFormat('!m/d/Y', trim($born));
    $d = \DateTimeImmutable::createFromFormat('!m/d/Y', trim($on));
    if ($b === false || $d === false || $b > $d) {
        return null;
    }

    return $b->diff($d)->y;
}

/**
 * The instructions: what each measure counts and what each answer means. Fixed text, so it
 * caches. From the 2026 MIPS CQM specifications for 364, 405 and 406, ACR's specification of
 * QMM26 (the copy on hand is from February 2024), and the coders' rulings on 364 of 2026-11-07.
 */
function measSystemPrompt(): string
{
    $options = '';
    foreach (MEAS_OPTIONS as $measure => $answers) {
        $options .= "Measure $measure:\n";
        foreach ($answers as $n => [$text]) {
            $options .= "  $n = $text\n";
        }
    }

    return <<<TEXT
You answer radiology quality measure questions from a final radiology report, as a certified
coder at a radiology practice does. For each measure named in the request, choose the one
answer the report supports. Answer from the report's own words. Do not infer a recommendation,
a guideline or a history the report does not state.

WHAT "INCIDENTAL" MEANS
An incidental finding is one the study was not done to look for and that was not already
known. A finding the study was ordered to evaluate or follow, or one the report describes as
known, stable from a prior study or previously characterized, is not incidental.

MEASURE 364: INCIDENTAL PULMONARY NODULE (CT in which any of the chest is seen)
Counted: a final report with a finding of an incidental pulmonary nodule.
Decide in this order: the exclusion (4) first, then whether the nodule is incidental, then
the recommendation.
- 4 (excluded), checked first: the report describes a pulmonary nodule, new, known, stable or
  being followed, and the patient has an active diagnosis or history of cancer (lymphoma and
  leukemia count; basal cell and squamous cell skin cancer do not; a suspected cancer without
  a diagnosis does not), is a heavy tobacco smoker (30 pack-years and smoking now or quit
  within 15 years: use it only when the report gives the pack-years), or the study is a lung
  cancer screening CT. The exclusion is wanted on the record, so with a nodule it is 4 and
  not 0 even when the nodule is not incidental. Read the cancer from any of: the request's
  "Clinical indication from the facility" line, the visit's diagnosis codes, and every part
  of the report (indication, history, comparison, findings, impression), not the impression
  alone. A study done for staging, restaging, surveillance or treatment response of a cancer
  ("RESTAGE", "restaging", "staging", "f/u lung ca", "mets", "s/p chemo" and the like) is a
  patient with cancer. With no pulmonary nodule the answer is 0, cancer or not.
- "ordered by a hematology-oncology physician" on the indication line is a reason to look
  harder for a cancer, not a cancer by itself: the patient may have a blood disorder that is
  not cancer (anemia, a clotting disorder, hemochromatosis). When the indication or the
  report states the cancer, a nodule is 4. When a nodule is described and no cancer is
  stated anywhere, answer by the rules below and give medium confidence at most, so a coder
  checks the history.
- Not incidental nodules, so answer 0 when 4 does not apply: granulomas, hamartomas, lesions
  with internal fat and other characteristically benign findings; a nodule that is being
  followed or was already known; a mass the report calls a suspected or known malignancy
  rather than a nodule. No nodule at all is also 0.
- 1 (performance met): the impression or conclusion gives, for the nodule, a recommended
  interval and modality for follow-up (for example "CT at 12 months"), or says no follow-up is
  needed, AND names the source of the recommendation (Fleischner Society, American College of
  Chest Physicians, Lung-RADS or another guideline). A conditional recommendation as the
  guideline states it ("no follow-up if low risk; CT at 12 months if high risk") meets it. All
  three are needed: what, when and the source.
- 2 (exception): the report documents a medical reason for giving no such recommendation, for
  example unexplained fever or an immunocompromised patient at risk for infection. The reason
  must be in the report.
- 3 (performance not met): there is an incidental nodule and the recommendation is missing,
  has no interval or modality, or names no source, with no reason given. A 3 is final: do not
  assume an addendum.

MEASURE 405: INCIDENTAL ABDOMINAL LESION (CT, CTA, MRI or MRA that includes the abdomen)
Counted: a final report that notes, incidentally, one or more of
  - a cystic renal lesion that is simple appearing (Bosniak I or II; a simple cyst),
  - an adrenal lesion of 1.0 cm or less,
  - an adrenal lesion over 1.0 cm and up to 4.0 cm classified as likely benign or diagnostic
    benign (for example an adenoma by unenhanced CT density, washout CT, or in- and
    opposed-phase MRI).
A complex or indeterminate renal lesion (Bosniak IIF or higher, solid, enhancing), an adrenal
lesion over 4.0 cm, an adrenal lesion over 1.0 cm that is not characterized as benign, and
liver, pancreatic or other lesions are not counted: answer 0 when those are all there is.
- 0: no such incidental lesion.
- 1 (performance met): the report states that no follow-up imaging is recommended for the
  lesion (for example "no follow-up imaging is recommended as the incidental lesion is likely
  benign").
- 2 (exception): the report documents a medical reason that follow-up imaging is indicated,
  for example lymphadenopathy, signs of metastasis, or an active diagnosis or history of
  cancer.
- 3 (performance not met): follow-up imaging is recommended for the lesion, or the report does
  not include a specific recommendation of no follow-up. A lesion that is only described, with
  nothing said about follow-up, is 3.

MEASURE 406: INCIDENTAL THYROID NODULE (CT, CTA, MRI or MRA of the chest or neck)
Counted: a final report that notes an incidental thyroid nodule smaller than 1.0 cm in a
patient with no known thyroid disease. A nodule of 1.0 cm or more, a nodule with no size given
and not described as subcentimeter, a goiter or known thyroid disease, and a thyroid study
done for the thyroid are not counted: answer 0.
- 0: no such nodule.
- 1: follow-up imaging (for example a thyroid ultrasound) is recommended for it, with no
  medical reason given. This is the poor-quality outcome of this inverse measure.
- 2 (exception): follow-up imaging is recommended and the report documents a medical reason,
  for example multiple endocrine neoplasia or cervical lymphadenopathy.
- 3: follow-up imaging is not recommended: the report says so, or it describes the nodule and
  recommends nothing.

MEASURE QMM26: SCREENING ULTRASOUND FOR ABDOMINAL AORTIC ANEURYSM (AAA), ACR registry measure
Counted: every final report of a screening ultrasound for AAA. There is no 0 answer.
- 1 (performance met): an AAA under 5.5 cm, and the report includes a recommendation for
  follow-up of the aneurysm (follow-up ultrasound, referral to a specialist) or a
  recommendation of no follow-up, according to the Society for Vascular Surgery practice
  guidelines or a similar published guideline, with the source cited.
- 2 (performance met): an AAA of 5.5 cm or more, the report includes such a recommendation
  with the source cited, AND it documents direct communication of the finding and the
  recommendation to the ordering provider.
- 3 (performance met): negative for AAA, and the report clearly states that no future
  screening is necessary or recommended.
- 4 (performance not met): a positive finding with no recommendation for follow-up (or of no
  follow-up), or with no source cited; or an AAA of 5.5 cm or more with no documented direct
  communication; or a negative screening with no clear statement that no future screening is
  needed.
- 5 (exception): the report documents that the patient is under active surveillance by a
  vascular specialist and the AAA is unchanged from the prior study.

THE ANSWERS
$options
THE REPLY
- One entry for each measure named in the request, and no others.
- answer: the number, from that measure's list.
- confidence: high when the report states what the answer rests on in plain words; medium when
  you had to interpret (an unclear size, a recommendation that may or may not be about this
  finding, a finding that may or may not be incidental); low when the report does not let you
  decide.
- evidence: the sentence or phrase of the report the answer rests on, copied word for word, at
  most 300 characters. For an answer of 0 or a missing statement, the phrase that describes
  the organ (for example "The thyroid is unremarkable"), or an empty string when the report
  says nothing about it.
- rationale: one sentence saying why this answer and not the nearest other one.
TEXT;
}

/**
 * @param list<string> $measures
 * @return array<string, mixed>
 */
function measAnswerSchema(array $measures): array
{
    return [
        'type' => 'object',
        'properties' => [
            'answers' => [
                'type' => 'array',
                'items' => [
                    'type' => 'object',
                    'properties' => [
                        'measure' => ['type' => 'string', 'enum' => array_map('strval', $measures)],
                        'answer' => ['type' => 'integer'],
                        'confidence' => ['type' => 'string', 'enum' => ['high', 'medium', 'low']],
                        'evidence' => ['type' => 'string', 'description' => 'Words copied from the report'],
                        'rationale' => ['type' => 'string', 'description' => 'One sentence'],
                    ],
                    'required' => ['measure', 'answer', 'confidence', 'evidence', 'rationale'],
                    'additionalProperties' => false,
                ],
            ],
        ],
        'required' => ['answers'],
        'additionalProperties' => false,
    ];
}

/**
 * The Messages API request for one study: its measures, CPT codes, the patient's age, the
 * visit's diagnosis codes, the facility's indication and the report. No names, numbers or dates.
 *
 * @param list<string> $measures
 * @param list<string> $cpts
 * @param list<string> $diagnoses the codes Gifford sent for the visit
 * @return array<string, mixed>
 */
function measRequest(string $model, string $effort, array $measures, array $cpts, ?int $age, array $diagnoses, string $indication, string $report): array
{
    $clean = preg_replace('/(Please note:|Electronically Signed by:).*$/si', '', $report) ?? $report;
    $content = 'Measures to answer: ' . implode(', ', $measures) . "\n"
        . 'CPT: ' . implode(', ', $cpts) . "\n"
        . 'Age: ' . ($age === null ? 'not given' : (string) $age) . "\n"
        . "Diagnosis codes on the visit: " . ($diagnoses === [] ? 'none sent' : implode(', ', $diagnoses)) . "\n"
        . 'Clinical indication from the facility: ' . (trim($indication) === '' ? 'none sent' : trim($indication)) . "\n\n"
        . "Report:\n" . $clean;

    return [
        'model' => $model,
        'max_tokens' => 16000,
        'output_config' => [
            'effort' => $effort,
            'format' => ['type' => 'json_schema', 'schema' => measAnswerSchema($measures)],
        ],
        'system' => [
            ['type' => 'text', 'text' => measSystemPrompt(), 'cache_control' => ['type' => 'ephemeral']],
        ],
        'messages' => [
            ['role' => 'user', 'content' => $content],
        ],
    ];
}

/**
 * What a Messages API response says, as the plain record kept for each request: one answer
 * per measure asked. An answer that is not on the measure's list, or a measure left out, makes
 * the whole reply "answer incomplete": nothing is guessed.
 *
 * @param array<string, mixed> $body the decoded response
 * @param list<string> $measures the measures asked
 * @return array<string, mixed> outcome, usage, and answers (by measure) when it succeeded
 */
function measParseResponse(array $body, array $measures): array
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
    $reply = json_decode($text, true);
    if (!is_array($reply) || !is_array($reply['answers'] ?? null)) {
        return ['outcome' => 'answer was not JSON', 'usage' => $usage];
    }
    $answers = [];
    foreach ($reply['answers'] as $a) {
        $measure = is_array($a) ? (string) ($a['measure'] ?? '') : '';
        $answer = is_array($a) && is_int($a['answer'] ?? null) ? $a['answer'] : null;
        if (!in_array($measure, $measures, true) || $answer === null || !isset(MEAS_OPTIONS[$measure][$answer]) || isset($answers[$measure])) {
            continue;
        }
        $answers[$measure] = [
            'answer' => $answer,
            'confidence' => (string) ($a['confidence'] ?? ''),
            'evidence' => (string) ($a['evidence'] ?? ''),
            'rationale' => (string) ($a['rationale'] ?? ''),
        ];
    }
    if (count($answers) !== count($measures)) {
        return ['outcome' => 'answer incomplete', 'usage' => $usage];
    }

    return ['outcome' => 'succeeded', 'usage' => $usage, 'answers' => $answers];
}

/**
 * The answer that stands for a measure and whether it stands without a coder.
 *
 * mea004's rule first: a 364 answer of 1, 2 or 3 on a visit with a cancer or lung-screening
 * diagnosis is the exclusion, 4. Then Stephen's rule of 2026-11-18: only a "no finding" (0)
 * with high confidence stands without a coder; every finding, and every answer less sure
 * than that, is the coder's to give (so QMM26, which has no 0, always is). The Gifford
 * module has the same rule (MeasureSuggestion::standsAlone()).
 *
 * @param array{answer: int, confidence: string} $answer
 * @param list<string> $diagnoses the visit's codes, with or without the dot
 * @return array{int, bool, string} the answer, whether it stands without a coder, and a note
 */
function measDecide(string $measure, array $answer, array $diagnoses): array
{
    $n = $answer['answer'];
    if ($measure === '364' && in_array($n, [1, 2, 3], true)) {
        foreach ($diagnoses as $dx) {
            if (preg_match(MEAS_364_EXCLUSION_DX, str_replace('.', '', strtoupper($dx))) === 1) {
                return [4, false, 'excluded by diagnosis'];
            }
        }
    }

    $sure = $answer['confidence'] === 'high';

    return [$n, $n === 0 && $sure, $n !== 0 ? 'a finding' : ($sure ? '' : 'not high confidence')];
}
