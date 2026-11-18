<?php

/**
 * Score the quality measure answers read from the report against the answers the coders gave,
 * on Rutland charges already coded (charcur).
 *
 * Runs on the billing server (it reads patient data and fetches the reports); only the summary
 * it writes is free of identifiers.
 *
 *   php measure_compare_rri.php run CHARCUR GARFILE --from YYYYMMDD --to YYYYMMDD
 *           [--answered [--blank N]] [--measure M] [--limit N] [--model M] [--effort E]
 *           [--workers 4] [--dry-run]
 *   php measure_compare_rri.php run CHARCUR GARFILE (same selection) --resume rri-measure-run-<time>.json
 *   php measure_compare_rri.php summary rri-measure-run-<time>.json
 *   php measure_compare_rri.php show rri-measure-run-<time>.json [--all]
 *
 * show prints, for the questions where coder and model differ (--all: every question), the two
 * answers, the words quoted and, of the report, only the exam, indication and impression, with
 * dates and long numbers masked and no charge key, date, age or diagnosis: text that can be
 * pasted elsewhere once it has been read for names.
 *
 * CHARCUR and GARFILE are the unloads (one fixed-width record per line, the copybooks'
 * layouts). A charge carries a measure by its CPT code, the patient's age on the date of
 * service and whether the primary payer is Medicare (003), as mea003 flags them; the coder's
 * answers are CC-QP1..3, in the order 364, 405, 406 (QMM26 alone in CC-QP1).
 *   --from, --to   dates of service to take, inclusive
 *   --answered     only charges with at least one answer keyed (a blank is the coder's Enter)
 *   --blank N      with --answered: also N charges with every answer blank, at random
 *                  (--seed picks which)
 *   --measure M    only charges that carry QMM26, 364, 405 or 406
 *   --limit N      only the first N charges after the selection
 *   --dry-run      select and count, fetch and send nothing
 * The report comes from the RRI site's FHIR API the way reads.php gets it (same environment:
 * BASE_OEMR_URL, OEMR_RRI_SITE_ID, OEMR_TEST_CLIENT_ID, OEMR_RRI_USERNAME, OEMR_RRI_PASSWORD),
 * by MRN (GARFILE's account number) and visit number; the Observation whose procedure names
 * the charge's CPT on its date of service. Then one request per charge through the Messages
 * API: the report, the CPT, the age and the charge's diagnosis codes; no names, numbers or
 * dates. Needs ANTHROPIC_API_KEY. It writes, in the current directory:
 *
 *   rri-measure-run-<time>.json           the charges asked, with their reports (PATIENT DATA)
 *   rri-measure-run-<time>.results.jsonl  each answer as it came (PATIENT DATA)
 *   rri-measure-run-<time>.detail.csv     one line per charge and measure (PATIENT DATA)
 *   rri-measure-run-<time>.summary.txt    counts only: the file to copy out
 *
 * The reports are fetched once and kept in the run file, so --resume and summary fetch nothing.
 */

declare(strict_types=1);

require_once __DIR__ . '/measure_run.php';
require_once __DIR__ . '/rri_reports.php';

/** Columns of the unloads (start, length), from copylib/rri/charcur.cpy and garfile.cpy. */
const RRI_CHARCUR = [
    'key' => [0, 11], 'garno' => [0, 8], 'dx1' => [26, 7], 'cpt' => [37, 5], 'paycode' => [61, 3], 'dos' => [83, 8],
    'dx2' => [108, 7], 'dx3' => [115, 7], 'dx4' => [133, 7], 'qp1' => [140, 2], 'qp2' => [142, 2], 'visit' => [144, 7],
    'qp3' => [151, 2],
];
const RRI_GARFILE = ['garno' => [0, 8], 'dob' => [119, 8], 'acct' => [277, 8]];

const RRI_MEDICARE = '003';

/**
 * @param array<string, array{int, int}> $layout
 * @return array<string, string>
 */
function rriFields(string $line, array $layout): array
{
    $fields = [];
    foreach ($layout as $name => [$start, $length]) {
        $fields[$name] = trim(substr($line, $start, $length));
    }

    return $fields;
}

/**
 * Whole years between two yyyymmdd dates; null when either is not a date.
 */
function rriAge(string $born, string $on): ?int
{
    $b = \DateTimeImmutable::createFromFormat('!Ymd', $born);
    $d = \DateTimeImmutable::createFromFormat('!Ymd', $on);
    if ($b === false || $d === false || $b->format('Ymd') !== $born || $d->format('Ymd') !== $on || $b > $d) {
        return null;
    }

    return $b->diff($d)->y;
}

/**
 * A diagnosis as the billing system keeps it (R911, no dot) with its dot; '' when blank.
 */
function rriDx(string $code): string
{
    return $code === '' ? '' : dxNorm($code);
}

/**
 * The coder's answer in a QP field as a number, or null when blank or not a number.
 */
function rriAnswer(string $qp): ?int
{
    return preg_match('/^\d$/', $qp) === 1 ? (int) $qp : null;
}

/**
 * Account number (MRN) and date of birth by guarantor number.
 *
 * @return array<string, array{acct: string, dob: string}>
 */
function rriGuarantors(string $path): array
{
    $handle = fopen($path, 'r');
    if ($handle === false) {
        measFail("cannot read $path");
    }
    $found = [];
    $lines = 0;
    $bad = 0;
    while (($line = fgets($handle)) !== false) {
        $lines++;
        $g = rriFields(rtrim($line, "\r\n"), RRI_GARFILE);
        if ($g['garno'] === '' || $g['acct'] === '' || preg_match('/^\d{8}$/', $g['dob']) !== 1) {
            $bad++;
            continue;
        }
        $found[$g['garno']] = ['acct' => $g['acct'], 'dob' => $g['dob']];
    }
    fclose($handle);
    fwrite(STDERR, sprintf("garfile: %d records, %d without an account number or a date of birth\n", $lines, $bad));
    if ($lines === 0 || $bad > $lines / 2) {
        measFail('that does not look like a garfile unload (see the layout at the top of this script)');
    }

    return $found;
}

/**
 * The charges of the unload that carry a measure, in the date range, with the coder's answers.
 *
 * @param array<string, array{acct: string, dob: string}> $guarantors
 * @return array{list<array<string, mixed>>, array<string, int>} the charges, and counts of what was left out
 */
function rriCharges(string $path, array $guarantors, string $from, string $to): array
{
    $handle = fopen($path, 'r');
    if ($handle === false) {
        measFail("cannot read $path");
    }
    $sets = measSets();
    $charges = [];
    $left = ['records' => 0, 'in the dates with a measure code' => 0, 'no garfile record' => 0, 'gate not passed' => 0, 'no visit number' => 0];
    while (($line = fgets($handle)) !== false) {
        $left['records']++;
        $c = rriFields(rtrim($line, "\r\n"), RRI_CHARCUR);
        if (!isset($sets[$c['cpt']]) || $c['dos'] < $from || $c['dos'] > $to || preg_match('/^\d{8}$/', $c['dos']) !== 1) {
            continue;
        }
        $left['in the dates with a measure code']++;
        $g = $guarantors[$c['garno']] ?? null;
        if ($g === null) {
            $left['no garfile record']++;
            continue;
        }
        $age = rriAge($g['dob'], $c['dos']);
        $measures = measFor([$c['cpt']], $age, $c['paycode'] === RRI_MEDICARE);
        if ($measures === []) {
            $left['gate not passed']++;
            continue;
        }
        if ($c['visit'] === '') {
            $left['no visit number']++;
            continue;
        }
        $coder = [];
        foreach ($measures as $i => $measure) {
            $coder[$measure] = rriAnswer($c['qp' . ($i + 1)]);
        }
        $charges[] = [
            'key' => $c['key'], 'acct' => $g['acct'], 'visit' => $c['visit'], 'dos' => $c['dos'], 'cpt' => $c['cpt'],
            'paycode' => $c['paycode'], 'age' => $age, 'measures' => $measures, 'coder' => $coder,
            'dx' => array_values(array_filter(array_map('rriDx', [$c['dx1'], $c['dx2'], $c['dx3'], $c['dx4']]))),
        ];
    }
    fclose($handle);
    if ($left['records'] > 0 && $left['in the dates with a measure code'] === 0) {
        fwrite(STDERR, "no charge in those dates has a measure code: check the dates and that this is a charcur unload\n");
    }

    return [$charges, $left];
}

/**
 * The answer that stands on a side once mea004's diagnosis rule is applied; a blank is 0.
 *
 * @param list<string> $dx
 */
function rriFinal(string $measure, ?int $answer, array $dx): int
{
    return measDecide($measure, ['answer' => $answer ?? 0, 'confidence' => 'high'], $dx)[0];
}

function rriWriteOutputs(string $runPath): void
{
    $meta = json_decode((string) file_get_contents($runPath), true);
    if (!is_array($meta) || !is_array($meta['charges'] ?? null)) {
        measFail("$runPath is not a run file of this script");
    }
    $base = preg_replace('/\.json$/', '', $runPath);
    $results = measLoadResults($base . '.results.jsonl');

    $outcomes = [];
    $usage = [];
    $rows = [];
    foreach ($meta['charges'] as $id => $c) {
        if (($c['report'] ?? '') === '') {
            $outcomes[] = 'no report';
            continue;
        }
        $r = $results[(string) $id] ?? ['outcome' => 'not asked'];
        $outcomes[] = (string) $r['outcome'];
        foreach ($r['usage'] ?? [] as $name => $n) {
            $usage[$name] = ($usage[$name] ?? 0) + (int) $n;
        }
        if ($r['outcome'] !== 'succeeded') {
            continue;
        }
        foreach ($c['measures'] as $measure) {
            $measure = (string) $measure;
            $answer = $r['answers'][$measure];
            $keyed = $c['coder'][$measure] ?? null;
            $rows[] = [
                'c' => $c, 'measure' => $measure, 'keyed' => $keyed, 'coder' => rriFinal($measure, $keyed, $c['dx']),
                'given' => $answer['answer'], 'model' => measDecide($measure, $answer, $c['dx'])[0],
                'confidence' => $answer['confidence'], 'evidence' => $answer['evidence'], 'rationale' => $answer['rationale'],
            ];
        }
    }

    $detail = measOpenPrivate($base . '.detail.csv', 'w');
    fputcsv($detail, ['charge', 'dos', 'cpt', 'paycode', 'age', 'dx', 'measure', 'coder_keyed', 'coder', 'model_answer', 'model',
        'agree', 'confidence', 'evidence', 'rationale'], ',', '"', '');
    foreach ($rows as $row) {
        $c = $row['c'];
        fputcsv($detail, [
            $c['key'], $c['dos'], $c['cpt'], $c['paycode'], $c['age'] ?? '', implode(' ', $c['dx']), $row['measure'],
            $row['keyed'] ?? 'blank', $row['coder'], $row['given'], $row['model'], $row['coder'] === $row['model'] ? 'yes' : 'no',
            $row['confidence'], $row['evidence'], $row['rationale'],
        ], ',', '"', '');
    }
    fclose($detail);

    $pct = static fn (int $n, int $of): string => $of > 0 ? sprintf('%d of %d (%.1f%%)', $n, $of, 100 * $n / $of) : '0 of 0';
    $same = static fn (array $set): int => count(array_filter($set, static fn (array $r): bool => $r['coder'] === $r['model']));
    $out = [
        sprintf('Rutland quality measures against the coders, %s; model %s, effort %s', $meta['created'], $meta['model'], $meta['effort']),
        sprintf('dates of service %s to %s; %s', $meta['from'], $meta['to'], $meta['selection']),
        sprintf('%d charges; results: %s', count($meta['charges']), measCounts($outcomes)),
        '',
        "An answer is compared after mea004's rule (a 364 answer of 1, 2 or 3 with a cancer or screening",
        'diagnosis on the charge is 4) on both sides. A blank from the coder counts as 0.',
        '',
    ];
    foreach (array_map('strval', array_keys(MEAS_OPTIONS)) as $measure) {
        $mine = array_values(array_filter($rows, static fn (array $r): bool => $r['measure'] === $measure));
        if ($mine === []) {
            continue;
        }
        $keyed = array_values(array_filter($mine, static fn (array $r): bool => $r['keyed'] !== null));
        $blank = array_values(array_filter($mine, static fn (array $r): bool => $r['keyed'] === null));
        $high = array_values(array_filter($mine, static fn (array $r): bool => $r['confidence'] === 'high'));
        $rest = array_values(array_filter($mine, static fn (array $r): bool => $r['confidence'] !== 'high'));
        $out[] = sprintf('Measure %s: %d questions', $measure, count($mine));
        $out[] = '  same answer               ' . $pct($same($mine), count($mine));
        $out[] = '    where the coder keyed one ' . $pct($same($keyed), count($keyed));
        $out[] = '    where the coder left it blank ' . $pct($same($blank), count($blank));
        $out[] = '    model high confidence     ' . $pct($same($high), count($high));
        $out[] = '    model not high confidence ' . $pct($same($rest), count($rest));
        $out[] = '  coder (down) against model (across):';
        $answers = array_keys(MEAS_OPTIONS[$measure]);
        $out[] = '          ' . implode('', array_map(static fn (int $n): string => sprintf('%6d', $n), $answers));
        // The coder's side can hold a number the measure has no answer for (and a blank is 0).
        $keyedValues = array_values(array_unique(array_column($mine, 'coder')));
        sort($keyedValues);
        foreach ($keyedValues as $coder) {
            $line = sprintf('    %4d  ', $coder);
            foreach ($answers as $model) {
                $n = count(array_filter($mine, static fn (array $r): bool => $r['coder'] === $coder && $r['model'] === $model));
                $line .= sprintf('%6s', $n === 0 ? '.' : (string) $n);
            }
            $out[] = $line . (isset(MEAS_OPTIONS[$measure][$coder]) ? '' : '   (not an answer of this measure)');
        }
        $out[] = '';
    }
    $out[] = 'Tokens: ' . ($usage === [] ? '-' : implode(', ', array_map(static fn (string $k, int $n): string => $k . ' ' . number_format($n), array_keys($usage), $usage)));
    $summary = implode("\n", $out) . "\n";
    file_put_contents($base . '.summary.txt', $summary);
    echo $summary, "\n";
    fwrite(STDERR, "wrote $base.detail.csv (patient data, keep it here) and $base.summary.txt (counts only)\n");
}

/**
 * A report cut down to what can leave the server: the exam, the indication or history and the
 * impression (and a recommendation or aorta section when the report has one), with dates and
 * long numbers masked and the signature left off. The rule is gifford-dx-show.py's --paste.
 */
function rriPasteable(string $report): string
{
    $report = preg_replace('/(Please note:|Electronically Signed by:).*$/si', '', $report) ?? $report;
    $kept = [];
    $keeping = false;
    foreach (preg_split('/\r\n|\n|\r/', $report) ?: [] as $line) {
        if (preg_match('/^\s*([A-Z][A-Z \/&]{2,40}):/', $line, $heading) === 1) {
            $keeping = preg_match('/PROCEDURE|EXAM|INDICATION|HISTORY|REASON|IMPRESSION|CONCLUSION|RECOMMENDATION|AORTA/', $heading[1]) === 1;
        }
        if ($keeping && trim($line) !== '' && preg_match('/^\s*(please note|voice recognition)/i', $line) !== 1) {
            $kept[] = rriMask(rtrim($line));
        }
    }

    return $kept === [] ? '(no indication or impression heading found: read this one on the server)' : implode("\n", $kept);
}

function rriMask(string $text): string
{
    $text = preg_replace('/\b\d{1,2}[\/-]\d{1,2}[\/-]\d{2,4}\b|\b\d{4}-\d{2}-\d{2}\b/', '[date]', $text) ?? $text;

    return preg_replace('/\d{5,}/', '[number]', $text) ?? $text;
}

/**
 * Print the questions of a run with what can leave the server: no charge key, date, age or
 * diagnosis; of the report only rriPasteable(). Without $all, only where coder and model differ.
 */
function rriShow(string $runPath, bool $all): void
{
    $meta = json_decode((string) file_get_contents($runPath), true);
    if (!is_array($meta) || !is_array($meta['charges'] ?? null)) {
        measFail("$runPath is not a run file of this script");
    }
    $results = measLoadResults(preg_replace('/\.json$/', '', $runPath) . '.results.jsonl');
    $n = 0;
    foreach ($meta['charges'] as $id => $c) {
        $r = $results[(string) $id] ?? null;
        if ($r === null || ($r['outcome'] ?? '') !== 'succeeded') {
            continue;
        }
        $lines = [];
        foreach ($c['measures'] as $measure) {
            $measure = (string) $measure;
            $answer = $r['answers'][$measure];
            $coder = rriFinal($measure, $c['coder'][$measure] ?? null, $c['dx']);
            $model = measDecide($measure, $answer, $c['dx'])[0];
            if (!$all && $coder === $model) {
                continue;
            }
            $lines[] = sprintf(
                "measure %s: coder %s, model %d (%s)%s\n  quoted: %s\n  why: %s",
                $measure,
                ($c['coder'][$measure] ?? null) === null ? 'blank' : (string) $coder,
                $model,
                $answer['confidence'],
                $coder === $model ? '' : '  DIFFER',
                rriMask($answer['evidence']),
                rriMask($answer['rationale'])
            );
        }
        if ($lines === []) {
            continue;
        }
        $n++;
        echo "=== $n === CPT {$c['cpt']}\n", implode("\n", $lines), "\n--- report (exam, indication, impression only)\n", rriPasteable((string) $c['report']), "\n\n";
    }
    fwrite(STDERR, "$n charges shown. Names can still be inside the text: read it before pasting.\n");
}

/**
 * @param array<string, string> $options
 */
function rriRun(string $charcur, string $garfile, array $options): void
{
    $from = $options['from'] ?? '';
    $to = $options['to'] ?? '';
    if (preg_match('/^\d{8}$/', $from) !== 1 || preg_match('/^\d{8}$/', $to) !== 1 || $from > $to) {
        measFail('--from and --to are dates of service, YYYYMMDD');
    }
    $resume = $options['resume'] ?? null;
    $meta = null;
    if ($resume !== null) {
        $meta = json_decode((string) file_get_contents($resume), true);
        if (!is_array($meta) || !is_array($meta['charges'] ?? null)) {
            measFail("$resume is not a run file of this script");
        }
    }
    [$charges, $left] = rriCharges($charcur, rriGuarantors($garfile), $from, $to);
    fwrite(STDERR, 'charcur: ' . implode(', ', array_map(static fn (string $k, int $n): string => "$k $n", array_keys($left), $left)) . "\n");

    $selection = [];
    if (isset($options['measure'])) {
        if (!isset(MEAS_OPTIONS[$options['measure']])) {
            measFail('--measure is one of ' . implode(', ', array_keys(MEAS_OPTIONS)));
        }
        $charges = array_values(array_filter($charges, static fn (array $c): bool => in_array($options['measure'], $c['measures'], true)));
        $selection[] = 'measure ' . $options['measure'];
    }
    if (isset($options['answered'])) {
        $isBlank = static fn (array $c): bool => array_filter($c['coder'], static fn (?int $a): bool => $a !== null) === [];
        $blank = array_values(array_filter($charges, $isBlank));
        $charges = array_values(array_filter($charges, static fn (array $c): bool => !$isBlank($c)));
        $selection[] = 'charges with an answer keyed';
        $want = (int) ($options['blank'] ?? 0);
        if ($want > 0) {
            mt_srand((int) ($options['seed'] ?? 20261118));
            $order = array_keys($blank);
            shuffle($order);
            $picked = array_slice($order, 0, $want);
            sort($picked);
            foreach ($picked as $i) {
                $charges[] = $blank[$i];
            }
            $selection[] = sprintf('plus %d of the %d all-blank ones at random', count($picked), count($blank));
        }
    } elseif (isset($options['blank'])) {
        measFail('--blank goes with --answered');
    }
    if (isset($options['limit'])) {
        $charges = array_slice($charges, 0, (int) $options['limit']);
        $selection[] = 'first ' . (int) $options['limit'];
    }
    $ids = array_map(static fn (int $i): string => sprintf('r%05d', $i), array_keys($charges));
    $answered = count(array_filter($charges, static fn (array $c): bool => array_filter($c['coder'], static fn (?int $a): bool => $a !== null) !== []));
    fwrite(STDERR, sprintf(
        "%d charges selected (%s); the coder keyed an answer on %d\n",
        count($charges),
        measCounts(array_merge(...array_column($charges, 'measures'), ...[[]])),
        $answered
    ));
    if (isset($options['dry-run'])) {
        fwrite(STDERR, "dry run: nothing fetched, nothing sent\n");

        return;
    }

    $model = $meta['model'] ?? $options['model'] ?? 'claude-opus-5-5';
    $effort = $meta['effort'] ?? $options['effort'] ?? 'high';
    if (!in_array($effort, ['low', 'medium', 'high', 'xhigh', 'max'], true)) {
        measFail('--effort is low, medium, high, xhigh or max');
    }
    if ($meta !== null) {
        $runPath = $resume;
        if (array_map('strval', array_keys($meta['charges'])) !== $ids || array_column($meta['charges'], 'key') !== array_column($charges, 'key')) {
            measFail('these files with this selection do not give the charges the run file was made from');
        }
    } else {
        $created = date('Ymd-His');
        $runPath = "rri-measure-run-$created.json";
        if (file_exists($runPath)) {
            measFail("$runPath exists already: wait a second and run again");
        }
        // The reports are fetched once, here, and kept with the run.
        $reports = new RriReports();
        $missing = 0;
        foreach ($charges as $i => $c) {
            $charges[$i]['report'] = $reports->report($c['acct'], $c['visit'], $c['dos'], $c['cpt']);
            $missing += $charges[$i]['report'] === '' ? 1 : 0;
            if (($i + 1) % 50 === 0) {
                fwrite(STDERR, ($i + 1) . ' of ' . count($charges) . " reports looked up\n");
            }
        }
        fwrite(STDERR, sprintf("reports: %d found, %d not found\n", count($charges) - $missing, $missing));
        $meta = [
            'created' => $created, 'model' => $model, 'effort' => $effort, 'from' => $from, 'to' => $to,
            'selection' => $selection === [] ? 'every charge that carries a measure' : implode('; ', $selection),
            'charges' => array_combine($ids, $charges),
        ];
        $handle = measOpenPrivate($runPath, 'w');
        fwrite($handle, json_encode($meta, JSON_THROW_ON_ERROR | JSON_INVALID_UTF8_SUBSTITUTE));
        fclose($handle);
    }
    fwrite(STDERR, "run file $runPath\n");

    $requests = [];
    $measures = [];
    foreach ($meta['charges'] as $id => $c) {
        if (($c['report'] ?? '') === '') {
            continue;
        }
        $id = (string) $id;
        $measures[$id] = array_map('strval', $c['measures']);
        $requests[$id] = measRequest($model, $effort, $measures[$id], [$c['cpt']], $c['age'], $c['dx'], '', $c['report']);
    }
    measSendAll($requests, $measures, array_keys($requests), preg_replace('/\.json$/', '', $runPath) . '.results.jsonl', max(1, (int) ($options['workers'] ?? 4)));
    rriWriteOutputs($runPath);
    fwrite(STDERR, "if some errored, ask only those again with: --resume $runPath\n");
}

$command = $argv[1] ?? '';
if ($command === 'run') {
    [$positional, $options] = measArgs(array_slice($argv, 2), ['dry-run', 'answered']);
    if (count($positional) !== 2) {
        measFail('run takes the charcur unload and the garfile unload; see the top of this script');
    }
    rriRun($positional[0], $positional[1], $options);
} elseif ($command === 'summary') {
    [$positional, $options] = measArgs(array_slice($argv, 2), []);
    if (count($positional) !== 1) {
        measFail('summary takes the run file; see the top of this script');
    }
    rriWriteOutputs($positional[0]);
} elseif ($command === 'show') {
    [$positional, $options] = measArgs(array_slice($argv, 2), ['all']);
    if (count($positional) !== 1) {
        measFail('show takes the run file; see the top of this script');
    }
    rriShow($positional[0], isset($options['all']));
} else {
    measFail('usage: php measure_compare_rri.php run CHARCUR GARFILE --from D --to D [options] | summary RUN.json; see the top of this script');
}
