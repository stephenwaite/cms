<?php

/**
 * Answer the radiology quality measures (QMM26, 364, 405, 406) from the reports in Gifford's
 * charge file, and count what came back.
 *
 * Runs where the live charge file is (it reads patient data); only the summary it writes is free
 * of identifiers.
 *
 *   php measure_compare.php run FILE [--measure QMM26] [--limit N] [--model M] [--effort E] [--workers 4] [--dry-run]
 *   php measure_compare.php run FILE [--measure QMM26] [--limit N] --resume gifford-measure-run-<time>.json
 *   php measure_compare.php summary gifford-measure-run-<time>.json
 *
 * run reads the charge file and asks once per accession that carries a measure (by its CPT
 * codes, the patient's age and whether Medicare is primary: measure_suggest.php): the report,
 * the CPT codes, the age, the visit's diagnosis codes and the facility's clinical indication;
 * no names, numbers or dates. Each answer is saved as it arrives, so a run that stops can be
 * finished with --resume (same file, --measure and --limit). --measure M keeps only the
 * accessions that carry that measure (QMM26, 364, 405 or 406); their other measures are still
 * asked. The first request is sent alone: a refused
 * key, model or setting stops the run there. It writes, in the current directory:
 *
 *   gifford-measure-run-<time>.json           the accessions asked (PATIENT DATA)
 *   gifford-measure-run-<time>.results.jsonl  each answer as it came (PATIENT DATA)
 *   gifford-measure-run-<time>.detail.csv     one line per accession and measure (PATIENT DATA)
 *   gifford-measure-run-<time>.summary.txt    counts only: the file to copy out
 *
 * summary writes the detail and summary files again from the saved answers, asking nothing.
 * Needs ANTHROPIC_API_KEY in the environment.
 */

declare(strict_types=1);

require_once __DIR__ . '/measure_run.php';

/**
 * The accessions of the file that carry a measure, each with its measures and the patient's
 * age on the date of service.
 *
 * @return array{list<array<string, mixed>>, int} the accessions, and how many carry a measure but have no report
 */
function measAccessions(string $file): array
{
    $found = [];
    $noReport = 0;
    foreach (dxReadAccessions($file) as $a) {
        $age = measAge((string) $a['dob'], (string) $a['dos']);
        $measures = measFor($a['cpts'], $age, dxPayerClass((string) $a['payer']) === 'medicare');
        if ($measures === []) {
            continue;
        }
        if (trim($a['report']) === '') {
            $noReport++;
            continue;
        }
        $found[] = $a + ['age' => $age, 'measures' => $measures];
    }

    return [$found, $noReport];
}

/**
 * The detail (patient data) and summary (counts only) files, from the saved answers.
 */
function measWriteOutputs(string $runPath): void
{
    $meta = json_decode((string) file_get_contents($runPath), true);
    if (!is_array($meta) || !is_array($meta['accessions'] ?? null)) {
        measFail("$runPath is not a run file");
    }
    $base = preg_replace('/\.json$/', '', $runPath);
    $results = measLoadResults($base . '.results.jsonl');

    $outcomes = [];
    $usage = [];
    $rows = [];       // one per accession and measure
    $studies = ['all' => 0, 'none' => 0, 'some' => 0];
    foreach ($meta['accessions'] as $id => $a) {
        $r = $results[(string) $id] ?? ['outcome' => 'not asked'];
        $outcomes[] = (string) $r['outcome'];
        foreach ($r['usage'] ?? [] as $name => $n) {
            $usage[$name] = ($usage[$name] ?? 0) + (int) $n;
        }
        if ($r['outcome'] !== 'succeeded') {
            continue;
        }
        $standing = 0;
        foreach ($a['measures'] as $measure) {
            $answer = $r['answers'][(string) $measure];
            [$final, $stands, $note] = measDecide((string) $measure, $answer, $a['gifford']);
            $standing += $stands ? 1 : 0;
            $rows[] = [
                'a' => $a, 'measure' => (string) $measure, 'given' => $answer['answer'], 'answer' => $final, 'stands' => $stands,
                'note' => $note, 'confidence' => $answer['confidence'], 'evidence' => $answer['evidence'], 'rationale' => $answer['rationale'],
            ];
        }
        $studies[$standing === count($a['measures']) ? 'all' : ($standing === 0 ? 'none' : 'some')]++;
    }

    $detail = measOpenPrivate($base . '.detail.csv', 'w');
    fputcsv($detail, ['accession', 'file_lines', 'cpt', 'location', 'payer', 'age', 'gifford_dx', 'measure', 'answer', 'code',
        'model_answer', 'confidence', 'coder_step', 'evidence', 'rationale'], ',', '"', '');
    foreach ($rows as $row) {
        $a = $row['a'];
        fputcsv($detail, [
            $a['accession'], implode(' ', $a['lines']), implode(' ', $a['cpts']), $a['location'], $a['payer'], $a['age'] ?? '',
            implode(' ', $a['gifford']), $row['measure'], $row['answer'], MEAS_OPTIONS[$row['measure']][$row['answer']][1],
            $row['given'], $row['confidence'], $row['stands'] ? 'stands' . ($row['note'] !== '' ? ': ' . $row['note'] : '') : $row['note'],
            $row['evidence'], $row['rationale'],
        ], ',', '"', '');
    }
    fclose($detail);

    $asked = count($meta['accessions']);
    $ok = count(array_filter($outcomes, static fn (string $o): bool => $o === 'succeeded'));
    $pct = static fn (int $n, int $of): string => $of > 0 ? sprintf('%d (%.1f%%)', $n, 100 * $n / $of) : (string) $n;
    $out = [
        sprintf('Gifford quality measures, %s; model %s, effort %s', $meta['created'], $meta['model'], $meta['effort']),
        sprintf('%d accessions sent; results: %s', $asked, measCounts($outcomes)),
        '',
        sprintf('Studies answered: %d', $ok),
        '  every measure stands without a coder   ' . $pct($studies['all'], $ok),
        '  some stand, some need a coder          ' . $pct($studies['some'], $ok),
        '  none stands                            ' . $pct($studies['none'], $ok),
        '  by location (answered): ' . measCounts(array_map(
            static fn (string $id): string => (string) $meta['accessions'][$id]['location'],
            array_values(array_filter(array_map('strval', array_keys($meta['accessions'])), static fn (string $id): bool => ($results[$id]['outcome'] ?? '') === 'succeeded'))
        )),
        '',
    ];
    foreach (array_keys(MEAS_OPTIONS) as $measure) {
        $measure = (string) $measure;
        $mine = array_values(array_filter($rows, static fn (array $r): bool => $r['measure'] === $measure));
        if ($mine === []) {
            continue;
        }
        $stand = array_values(array_filter($mine, static fn (array $r): bool => $r['stands']));
        $out[] = sprintf('Measure %s: %d questions; stand without a coder %s, coder confirms %d', $measure, count($mine), $pct(count($stand), count($mine)), count($mine) - count($stand));
        $out[] = '  confidence: ' . measCounts(array_column($mine, 'confidence'));
        foreach (MEAS_OPTIONS[$measure] as $n => [$text, $code]) {
            $with = array_values(array_filter($mine, static fn (array $r): bool => $r['answer'] === $n));
            if ($with === []) {
                continue;
            }
            $out[] = sprintf(
                '  %d %-9s %3d  (stand %d, coder %d)  %s',
                $n,
                $code === '' ? '-' : str_replace(' ', '+', $code),
                count($with),
                count(array_filter($with, static fn (array $r): bool => $r['stands'])),
                count(array_filter($with, static fn (array $r): bool => !$r['stands'])),
                $text
            );
        }
        $byDx = count(array_filter($mine, static fn (array $r): bool => $r['note'] === 'excluded by diagnosis'));
        if ($byDx > 0) {
            $out[] = sprintf(
                '  of the 4s, %d are by a cancer or screening diagnosis on the visit; the answer from the report was: %s',
                $byDx,
                measCounts(array_map(static fn (array $r): string => (string) $r['given'], array_values(array_filter($mine, static fn (array $r): bool => $r['note'] === 'excluded by diagnosis'))))
            );
        }
        $empty = count(array_filter($mine, static fn (array $r): bool => trim($r['evidence']) === ''));
        $out[] = sprintf('  answers with no words quoted from the report: %d', $empty);
        $out[] = '';
    }
    $out[] = 'Tokens: ' . ($usage === [] ? '-' : implode(', ', array_map(static fn (string $k, int $n): string => $k . ' ' . number_format($n), array_keys($usage), $usage)));
    $summary = implode("\n", $out) . "\n";
    file_put_contents($base . '.summary.txt', $summary);
    echo $summary, "\n";
    fwrite(STDERR, "wrote $base.detail.csv (patient data, keep it here) and $base.summary.txt (counts only)\n");
}

/**
 * @param array<string, string> $options
 */
function measRun(string $file, array $options): void
{
    $resume = $options['resume'] ?? null;
    $meta = null;
    if ($resume !== null) {
        $meta = json_decode((string) file_get_contents($resume), true);
        if (!is_array($meta) || !is_array($meta['accessions'] ?? null)) {
            measFail("$resume is not a run file");
        }
    }
    [$usable, $noReport] = measAccessions($file);
    if (isset($options['measure'])) {
        if (!isset(MEAS_OPTIONS[$options['measure']])) {
            measFail('--measure is one of ' . implode(', ', array_keys(MEAS_OPTIONS)));
        }
        $usable = array_values(array_filter($usable, static fn (array $a): bool => in_array($options['measure'], $a['measures'], true)));
    }
    if (isset($options['limit'])) {
        $usable = array_slice($usable, 0, (int) $options['limit']);
    }
    $ids = array_map(static fn (int $i): string => sprintf('m%05d', $i), array_keys($usable));
    fwrite(STDERR, sprintf(
        "%d accessions carry a measure (%s); %d more left out for having no report\n",
        count($usable),
        measCounts(array_merge(...array_column($usable, 'measures'), ...[[]])),
        $noReport
    ));
    $model = $meta['model'] ?? $options['model'] ?? 'claude-opus-5-5';
    $effort = $meta['effort'] ?? $options['effort'] ?? 'high';
    if (!in_array($effort, ['low', 'medium', 'high', 'xhigh', 'max'], true)) {
        measFail('--effort is low, medium, high, xhigh or max');
    }
    $requests = [];
    foreach ($usable as $i => $a) {
        $requests[$ids[$i]] = measRequest($model, $effort, $a['measures'], $a['cpts'], $a['age'], $a['gifford'], (string) $a['clinical'], $a['report']);
    }
    if (isset($options['dry-run'])) {
        $chars = array_sum(array_map(static fn (array $r): int => mb_strlen($r['messages'][0]['content']), $requests));
        fwrite(STDERR, sprintf(
            "dry run, nothing sent: %d requests, %s characters of reports, system prompt %s characters\n",
            count($requests),
            number_format($chars),
            number_format(mb_strlen(measSystemPrompt()))
        ));

        return;
    }

    $kept = ['accession', 'lines', 'cpts', 'gifford', 'location', 'pos', 'status', 'payer', 'age', 'measures'];
    if ($meta !== null) {
        $runPath = $resume;
        $then = array_map(static fn (array $a): string => (string) $a['accession'], $meta['accessions']);
        if (array_values($then) !== array_column($usable, 'accession') || array_map('strval', array_keys($then)) !== $ids) {
            measFail('this file with this selection does not give the accessions the run file was made from');
        }
    } else {
        $created = date('Ymd-His');
        $runPath = "gifford-measure-run-$created.json";
        if (file_exists($runPath)) {
            measFail("$runPath exists already: wait a second and run again");
        }
        $meta = ['created' => $created, 'model' => $model, 'effort' => $effort, 'file' => basename($file), 'accessions' => []];
        foreach ($usable as $i => $a) {
            $meta['accessions'][$ids[$i]] = array_intersect_key($a, array_flip($kept));
        }
        $handle = measOpenPrivate($runPath, 'w');
        fwrite($handle, json_encode($meta, JSON_THROW_ON_ERROR));
        fclose($handle);
    }
    $resultsPath = preg_replace('/\.json$/', '', $runPath) . '.results.jsonl';
    fwrite(STDERR, "run file $runPath\n");

    $measures = [];
    foreach ($usable as $i => $a) {
        $measures[$ids[$i]] = $a['measures'];
    }
    measSendAll($requests, $measures, $ids, $resultsPath, max(1, (int) ($options['workers'] ?? 4)));
    measWriteOutputs($runPath);
    fwrite(STDERR, "if some errored, ask only those again with: --resume $runPath\n");
}

$command = $argv[1] ?? '';
if ($command === 'run') {
    [$positional, $options] = measArgs(array_slice($argv, 2), ['dry-run']);
    if (count($positional) !== 1) {
        measFail('run takes the charge file; see the top of this script');
    }
    measRun($positional[0], $options);
} elseif ($command === 'summary') {
    [$positional, $options] = measArgs(array_slice($argv, 2), []);
    if (count($positional) !== 1) {
        measFail('summary takes the run file; see the top of this script');
    }
    measWriteOutputs($positional[0]);
} else {
    measFail('usage: php measure_compare.php run FILE [options] | summary RUN.json; see the top of this script');
}
