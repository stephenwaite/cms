<?php

/**
 * Compare Claude's ICD-10 suggestions, read from the report, with the diagnoses Gifford sent.
 *
 * Runs where the live charge file is (it reads patient data); only the summary it writes is free
 * of identifiers.
 *
 *   php dx_compare.php run FILE [--limit 25 | --sample 150 | --from-run RUN.json | --exams]
 *                               [--prompt coders|reads] [--model M] [--effort E] [--workers 4]
 *                               [--valid icd10_valid.txt] [--lists gifford-dx-lists.json] [--dry-run]
 *   php dx_compare.php run FILE [same selection] --resume gifford-dx-run-<time>.json
 *   php dx_compare.php summary gifford-dx-run-<time>.json [--valid F] [--lists F [--file FILE]]
 *
 * run reads the charge file (Gifford's 116-column billing feed, or a file with a header that has
 * the report columns) and asks once per accession through the Messages API: the report and the
 * accession's CPT codes; no names, numbers or dates from the other columns.
 *   --limit N      only the first N accessions
 *   --sample N     every accession Gifford sent no diagnosis for, plus N of the others at random
 *                  across the locations (--seed picks which)
 *   --from-run R   the accessions of an earlier run file, in its order: to put the same sample
 *                  to another --model (also the way to repeat a sample the Python script drew)
 *   --exams        only the accessions on an exam the coders' workbook has a Medicare LCD list
 *                  for (needs --lists)
 *   --prompt P     coders (the default; needs --lists): reads.php's prompt and hierarchy plus the
 *                  coders' additions (what to code on films taken during surgery, their quick-pick
 *                  list) and the coders' notes for the exam. Asked in two steps: first the report
 *                  and CPT alone, as reads.php asks; then, only when that answer shows the report
 *                  gave no codeable reason (no code, or a first code that is an examination or
 *                  status Z code), once more with the facility's clinical indication
 *                  (chg_clinical, or "none sent"). The second answer is the one kept. The answer is at most four
 *                  codes (a claim line points to four) and carries no description. reads: reads.php's prompt alone, report
 *                  and CPT only, as the first runs were made.
 *   --dry-run      read and count, send nothing
 * Each answer is saved as it arrives, so a run that stops can be finished with --resume (same
 * file and selection). The first request is sent alone: a refused key, model or setting stops
 * the run there. It writes, in the current directory:
 *
 *   gifford-dx-run-<time>.json           the accessions asked (PATIENT DATA)
 *   gifford-dx-run-<time>.results.jsonl  each answer as it came (PATIENT DATA)
 *   gifford-dx-run-<time>.detail.csv     one line per accession with the suggestions (PATIENT DATA)
 *   gifford-dx-run-<time>.summary.txt    counts only: the file to copy out
 *
 * summary writes the detail and summary files again from the saved answers, asking nothing.
 * --lists adds the checks from the coders' workbook (gifford-dx-lists.py makes the file):
 * VT Medicaid's cannot-be-primary and unspecified lists, and the Medicare LCD list for the exam.
 * --file gives the charge file for a run made before the payer was kept in the run file.
 *
 * The run and results files are the same as the Python script's (gifford-dx-compare.py), so
 * either can summarize the other's runs. Needs ANTHROPIC_API_KEY in the environment.
 */

declare(strict_types=1);

require_once __DIR__ . '/../../vendor/autoload.php';
require_once __DIR__ . '/dx_suggest.php';

use GuzzleHttp\Client;
use GuzzleHttp\Exception\ConnectException;
use GuzzleHttp\Pool;
use GuzzleHttp\Promise\PromiseInterface;
use Psr\Http\Message\ResponseInterface;

const DX_MAX_RETRIES = 5;
const DX_RETRY_STATUS = [429, 500, 502, 503, 504, 529];
const DX_FATAL_STATUS = [400, 401, 403, 404];

/**
 * Says why on stderr and stops. (No `never` return type: this also has to run on PHP 8.0.)
 */
function dxFail(string $message): void
{
    fwrite(STDERR, $message . "\n");
    exit(1);
}

/**
 * @param list<string> $argv after the command
 * @param list<string> $flags options that take no value
 * @return array{list<string>, array<string, string>} positional arguments and --options
 */
function dxArgs(array $argv, array $flags): array
{
    $positional = [];
    $options = [];
    for ($i = 0; $i < count($argv); $i++) {
        $arg = $argv[$i];
        if (!str_starts_with($arg, '--')) {
            $positional[] = $arg;
            continue;
        }
        $name = substr($arg, 2);
        if (in_array($name, $flags, true)) {
            $options[$name] = '1';
            continue;
        }
        if (!isset($argv[$i + 1]) || str_starts_with($argv[$i + 1], '--')) {
            // An unset shell variable (--file $F) leaves the option with nothing after it.
            dxFail("--$name needs a value");
        }
        $options[$name] = $argv[++$i];
    }

    return [$positional, $options];
}

/**
 * Counts of each value, most common first, ties in the order first seen.
 *
 * @param iterable<string> $values
 * @return list<array{string, int}>
 */
function dxCount(iterable $values): array
{
    $counts = [];
    foreach ($values as $value) {
        $counts['_' . $value] = ($counts['_' . $value] ?? 0) + 1;
    }
    arsort($counts, SORT_NUMERIC);
    $pairs = [];
    foreach ($counts as $key => $n) {
        $pairs[] = [substr((string) $key, 1), $n];
    }

    return $pairs;
}

/**
 * @param list<array{string, int}> $pairs
 */
function dxJoin(array $pairs, string $between = ' '): string
{
    return implode(', ', array_map(static fn (array $p): string => $p[0] . $between . $p[1], $pairs));
}

/**
 * @param list<int> $values
 */
function dxSpread(array $values): string
{
    if ($values === []) {
        return 'none';
    }
    sort($values);

    return 'median ' . $values[intdiv(count($values), 2)] . ', most ' . $values[count($values) - 1];
}

/**
 * @param list<array<string, mixed>> $rows
 */
function dxTally(array $rows, string $class): int
{
    return count(array_filter($rows, static fn (array $r): bool => $r['class'] === $class));
}

/**
 * The summary's section from the coders' workbook; adds the per-row flags to each row.
 *
 * @param list<array<string, mixed>> $done by reference: payer_class, flags, exam are added
 * @param array<string, mixed> $lists
 * @return list<string>
 */
function dxWorkbookChecks(array &$done, array $lists): array
{
    $plain = static fn (string $c): string => str_replace('.', '', $c);
    foreach ($done as &$r) {
        $r['payer_class'] = dxPayerClass((string) ($r['payer'] ?? ''));
        $suggested = array_column($r['suggested'], 0);
        $r['flags'] = [];
        foreach ([['suggested', $suggested], ['gifford', $r['gifford']]] as [$who, $codes]) {
            if ($codes !== [] && isset($lists['not_primary'][$plain($codes[0])])) {
                $r['flags'][] = "$who first code cannot be primary";
            }
            if ($codes !== [] && isset($lists['unspecified'][$plain($codes[0])])) {
                $r['flags'][] = "$who first code unspecified";
            }
        }
        $exam = dxExamFor($lists, $r['cpts']);
        $r['exam'] = $exam === null ? '' : (string) $exam['name'];
        if ($exam !== null) {
            $supports = static fn (array $codes): bool => array_filter($codes, static fn (string $c): bool => dxSupports($exam, $c)) !== [];
            $r['exam_gifford'] = $supports($r['gifford']);
            $r['exam_suggested'] = $supports($suggested);
            if (!$r['exam_suggested']) {
                $r['flags'][] = 'no suggested code on the exam list';
            }
            if ($r['gifford'] !== [] && !$r['exam_gifford']) {
                $r['flags'][] = 'no gifford code on the exam list';
            }
        }
    }
    unset($r);

    $out = ["Checks from the coders' workbook (lists made {$lists['made']})", ''];
    $out[] = '  VT Medicaid lists, first code only (all payers / Medicaid primary):';
    $line = static function (string $label, string $flag) use ($done): string {
        $rows = array_values(array_filter($done, static fn (array $r): bool => in_array($flag, $r['flags'], true)));
        $codes = dxCount(array_map(
            static fn (array $r): string => str_starts_with($flag, 'suggested') ? $r['suggested'][0][0] : $r['gifford'][0],
            $rows
        ));
        $medicaid = count(array_filter($rows, static fn (array $r): bool => $r['payer_class'] === 'medicaid'));

        return sprintf('    %-44s %4d / %3d   ', $label, count($rows), $medicaid) . dxJoin(array_slice($codes, 0, 8), ' x');
    };
    $out[] = $line('suggested first code cannot be primary', 'suggested first code cannot be primary');
    $out[] = $line("Gifford's first code cannot be primary", 'gifford first code cannot be primary');
    $out[] = $line('suggested first code is unspecified', 'suggested first code unspecified');
    $out[] = $line("Gifford's first code is unspecified", 'gifford first code unspecified');
    $out[] = sprintf(
        '    (of %d accessions, %d Medicaid primary; %d with a Gifford code)',
        count($done),
        count(array_filter($done, static fn (array $r): bool => $r['payer_class'] === 'medicaid')),
        count(array_filter($done, static fn (array $r): bool => $r['gifford'] !== []))
    );
    $out[] = '';
    $out[] = '  Medicare LCD lists: does a code support the exam? (accessions / Gifford yes / suggestion yes / neither)';
    $scopes = [
        'all payers' => null,
        'Medicare primary' => 'medicare',
        'Medicare Advantage primary' => 'medicare advantage',
    ];
    foreach ($scopes as $scope => $class) {
        $rows = array_values(array_filter(
            $done,
            static fn (array $r): bool => $r['exam'] !== '' && ($class === null || $r['payer_class'] === $class)
        ));
        $out[] = sprintf('    %s: %d accessions on an exam the workbook covers', $scope, count($rows));
        foreach (dxCount(array_column($rows, 'exam')) as [$name, $n]) {
            $g = array_filter($rows, static fn (array $r): bool => $r['exam'] === $name);
            $out[] = sprintf(
                '      %-14s %4d %4d %4d %4d',
                $name,
                $n,
                count(array_filter($g, static fn (array $r): bool => $r['exam_gifford'])),
                count(array_filter($g, static fn (array $r): bool => $r['exam_suggested'])),
                count(array_filter($g, static fn (array $r): bool => !$r['exam_gifford'] && !$r['exam_suggested']))
            );
        }
    }
    $covered = array_filter($done, static fn (array $r): bool => $r['exam'] !== '' && $r['gifford'] === []);
    $out[] = sprintf(
        '    of those, Gifford sent no diagnosis on %d; the suggestion supports the exam on %d',
        count($covered),
        count(array_filter($covered, static fn (array $r): bool => $r['exam_suggested']))
    );

    return $out;
}

/**
 * The summary's section on who confirms the diagnoses; adds coder_reasons to each row.
 *
 * @param list<array<string, mixed>> $done by reference; dxWorkbookChecks() has added payer_class
 * @param ?array<string, true> $valid
 * @param array<string, mixed> $lists
 * @return list<string>
 */
function dxCoderStep(array &$done, ?array $valid, array $lists): array
{
    foreach ($done as &$r) {
        $r['coder_reasons'] = dxCoderReasons($r['suggested'], $r['gifford'], $r['cpts'], $r['payer_class'], $valid, $lists);
    }
    unset($r);
    $posts = array_values(array_filter($done, static fn (array $r): bool => $r['coder_reasons'] === []));
    $pct = static fn (int $n): string => count($done) > 0 ? sprintf('%d (%.1f%%)', $n, 100 * $n / count($done)) : (string) $n;
    $out = ['Who confirms the diagnoses (first code high confidence, valid, passes the checks, among Gifford\'s codes)', ''];
    $out[] = sprintf('  of %d accessions: posts without a coder %s, coder confirms %s', count($done), $pct(count($posts)), $pct(count($done) - count($posts)));
    if ($valid === null) {
        $out[] = '  (no valid-code list given: that check was skipped)';
    }
    $out[] = '  posting without a coder, by location: ' . dxJoin(dxCount(array_map(static fn (array $r): string => (string) $r['location'], $posts)));
    $out[] = '  why a coder confirms (an accession can have several reasons):';
    foreach (dxCount(array_merge(...array_column($done, 'coder_reasons'), ...[[]])) as [$reason, $n]) {
        $out[] = sprintf('    %-46s %4d', $reason, $n);
    }
    $only = array_filter($done, static fn (array $r): bool => $r['coder_reasons'] === ["first code is not among Gifford's"]);
    $out[] = sprintf("  held only because the first code is not among Gifford's: %d", count($only));
    $out[] = '    of those, the first code shares a category with one of Gifford\'s: ' . count(array_filter(
        $only,
        static fn (array $r): bool => in_array(
            substr($r['suggested'][0][0], 0, 3),
            array_map(static fn (string $g): string => substr($g, 0, 3), $r['gifford']),
            true
        )
    ));

    return $out;
}

/**
 * The counts-only summary. Its text is the Python script's, line for line.
 *
 * @param list<array<string, mixed>> $rows by reference: the workbook's flags are added to the rows
 * @param array<string, int> $usage
 * @param array<string, mixed> $meta
 * @param ?array<string, true> $valid
 * @param ?array<string, mixed> $lists
 */
function dxSummarize(array &$rows, array $usage, array $meta, ?array $valid, ?array $lists): string
{
    $out = [];
    $out[] = "Gifford dx comparison, {$meta['created']}; model {$meta['model']}, effort {$meta['effort']}"
        . (isset($meta['prompt']) ? ", prompt {$meta['prompt']}" : '');
    $outcomes = [];
    foreach ($rows as $r) {
        $outcomes[$r['outcome']] = ($outcomes[$r['outcome']] ?? 0) + 1;
    }
    ksort($outcomes, SORT_STRING);
    $out[] = count($rows) . ' accessions sent; results: '
        . implode(', ', array_map(static fn (string $k, int $v): string => "$k $v", array_keys($outcomes), $outcomes));

    $doneKeys = array_keys(array_filter($rows, static fn (array $r): bool => $r['outcome'] === 'succeeded'));
    $done = array_map(static fn (int $k): array => $rows[$k], $doneKeys);
    $withDx = array_values(array_filter($done, static fn (array $r): bool => $r['gifford'] !== []));
    $without = array_values(array_filter($done, static fn (array $r): bool => $r['gifford'] === []));
    $out[] = '';
    $out[] = 'Accessions where Gifford sent diagnoses: ' . count($withDx);
    $pct = static fn (int $n, int $d): string => $d > 0 ? sprintf('%5d  %5.1f%%', $n, 100 * $n / $d) : sprintf('%5d', $n);
    foreach (array_slice(DX_CLASSES, 0, 6) as $class) {
        $out[] = sprintf('  %-24s %s', $class, $pct(dxTally($withDx, $class), count($withDx)));
    }
    $agree = dxTally($withDx, 'primary exact') + dxTally($withDx, 'other exact');
    $near = dxTally($withDx, 'primary same category') + dxTally($withDx, 'other same category');
    $out[] = sprintf('  %-24s %s', 'any exact match', $pct($agree, count($withDx)));
    $out[] = sprintf('  %-24s %s', 'exact or same category', $pct($agree + $near, count($withDx)));
    $out[] = '';
    $out[] = '  by the confidence of the first suggestion (exact match / same category / none / accessions):';
    $breakdown = static fn (array $g): string => sprintf(
        '%5d %5d %5d %5d',
        dxTally($g, 'primary exact') + dxTally($g, 'other exact'),
        dxTally($g, 'primary same category') + dxTally($g, 'other same category'),
        dxTally($g, 'no overlap'),
        count($g)
    );
    foreach (['high', 'medium', 'low'] as $confidence) {
        $g = array_values(array_filter(
            $withDx,
            static fn (array $r): bool => $r['suggested'] !== [] && $r['suggested'][0][1] === $confidence
        ));
        $out[] = sprintf('    %-7s %s', $confidence, $breakdown($g));
    }
    foreach (['location' => 'location', 'place of service' => 'pos'] as $label => $key) {
        $out[] = '';
        $out[] = "  by $label (exact match / same category / none / accessions):";
        foreach (dxCount(array_map(static fn (array $r): string => (string) $r[$key], $withDx)) as [$value, $n]) {
            $g = array_values(array_filter($withDx, static fn (array $r): bool => (string) $r[$key] === $value));
            $out[] = sprintf('    %-12s %s', $value, $breakdown($g));
        }
    }
    $out[] = '';
    $out[] = "  how many codes: Gifford's per visit "
        . dxSpread(array_map(static fn (array $r): int => count($r['gifford']), $withDx))
        . '; suggested per accession '
        . dxSpread(array_map(static fn (array $r): int => count($r['suggested']), $withDx));
    $out[] = '';
    $out[] = "  no overlap, first suggestion vs Gifford's first code, by category (5 or more):";
    $pairs = dxCount(array_map(
        static fn (array $r): string => substr($r['suggested'][0][0], 0, 3) . '|' . substr($r['gifford'][0], 0, 3),
        array_filter($withDx, static fn (array $r): bool => $r['class'] === 'no overlap')
    ));
    foreach (array_slice($pairs, 0, 25) as [$pair, $n]) {
        if ($n >= 5) {
            [$s, $g] = explode('|', $pair);
            $out[] = "    suggested $s  Gifford $g  x$n";
        }
    }
    $out[] = '';
    $out[] = 'Accessions where Gifford sent no diagnosis: ' . count($without);
    $out[] = '  first suggestion confidence: ' . dxJoin(dxCount(array_map(
        static fn (array $r): string => $r['suggested'] !== [] ? $r['suggested'][0][1] : 'none',
        $without
    )));
    $firsts = dxCount(array_map(
        static fn (array $r): string => $r['suggested'][0][0],
        array_filter($without, static fn (array $r): bool => $r['suggested'] !== [])
    ));
    $out[] = '  first suggestions (3 or more): '
        . dxJoin(array_values(array_filter(array_slice($firsts, 0, 20), static fn (array $p): bool => $p[1] >= 3)), ' x');
    $out[] = '';
    $every = [];
    $misshapen = 0;
    foreach ($done as $r) {
        foreach ($r['suggested'] as [$code]) {
            $every[] = $code;
        }
        foreach ($r['raw_codes'] as $raw) {
            $misshapen += dxNorm($raw) === '' ? 1 : 0;
        }
    }
    $out[] = 'Suggestions: ' . count($every) . ' codes; not shaped like ICD-10-CM: ' . $misshapen;
    if ($valid !== null) {
        $bad = dxCount(array_filter($every, static fn (string $c): bool => !isset($valid[str_replace('.', '', $c)])));
        $out[] = sprintf('  not in the valid-code list: %d (%d different): ', array_sum(array_column($bad, 1)), count($bad))
            . dxJoin(array_slice($bad, 0, 15), ' x');
    }
    $out[] = '  ending in 9 or with nothing after the category: '
        . count(array_filter($every, static fn (string $c): bool => str_ends_with($c, '9') || strlen($c) === 3));
    if ($lists !== null) {
        $out[] = '';
        array_push($out, ...dxWorkbookChecks($done, $lists));
        $out[] = '';
        array_push($out, ...dxCoderStep($done, $valid, $lists));
        foreach ($doneKeys as $i => $k) {
            $rows[$k] = $done[$i];
        }
    }
    if (($meta['prompt'] ?? '') === 'coders-2step') {
        $again = array_values(array_filter($rows, static fn (array $r): bool => $r['asked_again'] !== ''));
        $first = static fn (array $codes): string => $codes[0] ?? '-';
        $out[] = '';
        $out[] = sprintf(
            "Asked again with the facility's indication: %d of %d (second answer kept on %d)",
            count($again),
            count($rows),
            count(array_filter($again, static fn (array $r): bool => $r['asked_again'] === 'succeeded'))
        );
        $out[] = '  first code, first answer -> second: ' . dxJoin(dxCount(array_map(
            static fn (array $r): string => $first($r['first_answer']) . ' -> ' . $first(array_column($r['suggested'], 0)),
            $again
        )), ' x');
        $out[] = '  by location: ' . dxJoin(dxCount(array_map(static fn (array $r): string => (string) $r['location'], $again)));
        $over = array_values(array_filter(array_column($rows, 'over_cap')));
        $out[] = sprintf(
            'Answers with more than %d codes, cut to %d: %d%s',
            DX_MAX_CODES,
            DX_MAX_CODES,
            count($over),
            $over === [] ? '' : ' (the longest had ' . max($over) . ')'
        );
    }
    $out[] = '';
    $out[] = 'Tokens: ' . implode(', ', array_map(
        static fn (string $k, int $v): string => $k . ' ' . number_format($v),
        array_keys($usage),
        $usage
    ));

    return implode("\n", $out) . "\n";
}

/**
 * The last result recorded for each request id.
 *
 * @return array<string, array<string, mixed>>
 */
function dxLoadResults(string $path): array
{
    $results = [];
    foreach (is_file($path) ? (file($path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) ?: []) : [] as $line) {
        $r = json_decode($line, true);
        if (is_array($r) && isset($r['id'])) {
            $results[(string) $r['id']] = $r;
        }
    }

    return $results;
}

/**
 * A file only its owner can read: the run, results and detail files hold patient data.
 *
 * @return resource
 */
function dxOpenPrivate(string $path, string $mode)
{
    if (!file_exists($path)) {
        touch($path);
    }
    chmod($path, 0600);
    $handle = fopen($path, $mode);
    if ($handle === false) {
        dxFail("cannot write $path");
    }

    return $handle;
}

function dxWriteOutputs(string $runPath, ?string $validPath, ?string $listsPath, ?string $filePath): void
{
    $meta = json_decode((string) file_get_contents($runPath), true);
    if (!is_array($meta) || !is_array($meta['accessions'] ?? null)) {
        dxFail("$runPath is not a run file");
    }
    $lists = $listsPath === null ? null : dxLoadLists($listsPath);
    if ($filePath !== null) {
        // A run made before the payer was kept: take it from the charge file, by accession.
        $payers = [];
        foreach (dxReadAccessions($filePath) as $a) {
            $payers['k' . $a['accession']] = $a['payer'];
        }
        foreach ($meta['accessions'] as &$a) {
            $a['payer'] ??= $payers['k' . $a['accession']] ?? '';
        }
        unset($a);
    }
    if ($lists !== null) {
        foreach ($meta['accessions'] as $a) {
            if (!isset($a['payer'])) {
                dxFail('this run has no payer on its accessions: add --file with the charge file it was made from');
            }
        }
    }
    $valid = null;
    if ($validPath !== null) {
        $valid = [];
        foreach (file($validPath, FILE_IGNORE_NEW_LINES) ?: [] as $line) {
            $code = str_replace('.', '', strtoupper(trim($line)));
            if ($code !== '') {
                $valid[$code] = true;
            }
        }
    }

    $base = preg_replace('/\.json$/', '', $runPath);
    $results = dxLoadResults($base . '.results.jsonl');
    $usage = [];
    $rows = [];
    $keys = array_map('strval', array_keys($meta['accessions']));
    sort($keys, SORT_STRING);
    foreach ($keys as $key) {
        $r = $results[$key] ?? ['outcome' => 'not asked'];
        $row = $meta['accessions'][$key] + [
            'outcome' => (string) $r['outcome'], 'note' => (string) ($r['note'] ?? ''),
            'suggested' => [], 'raw_codes' => [], 'details' => [],
            'asked_again' => (string) ($r['asked_again'] ?? ''),
            'over_cap' => (int) ($r['over_cap'] ?? 0),
            'first_answer' => array_values(array_filter(array_map(
                static fn (array $c): string => dxNorm((string) $c['code']),
                $r['first_codes'] ?? []
            ))),
        ];
        foreach ($r['usage'] ?? [] as $name => $n) {
            $usage[$name] = ($usage[$name] ?? 0) + (int) $n;
        }
        foreach ($r['codes'] ?? [] as $c) {
            $row['raw_codes'][] = (string) $c['code'];
            $code = dxNorm((string) $c['code']);
            if ($code !== '') {
                $row['suggested'][] = [$code, (string) $c['confidence']];
                $description = (string) ($c['description'] ?? '');
                $row['details'][] = "[{$c['confidence']}] $code" . ($description === '' ? '' : " $description") . ": {$c['rationale']}";
            }
        }
        $row['class'] = dxClassify($row['gifford'], array_column($row['suggested'], 0));
        $rows[] = $row;
    }

    $summary = dxSummarize($rows, $usage, $meta, $valid, $lists);
    $detail = dxOpenPrivate($base . '.detail.csv', 'w');
    fputcsv($detail, ['accession', 'file_lines', 'cpt', 'location', 'pos', 'payer', 'report_status', 'gifford_dx',
        'suggested_dx', 'class', 'outcome', 'workbook_flags', 'suggestions', 'asked_again', 'first_answer_dx', 'coder_step'], ',', '"', '');
    foreach ($rows as $r) {
        fputcsv($detail, [
            $r['accession'], implode(' ', $r['lines']), implode(' ', $r['cpts']), $r['location'], $r['pos'], $r['payer'] ?? '',
            $r['status'], implode(' ', $r['gifford']), implode(' ', array_column($r['suggested'], 0)), $r['class'],
            $r['outcome'] . ($r['note'] !== '' ? ' ' . $r['note'] : ''), implode('; ', $r['flags'] ?? []), implode(' | ', $r['details']),
            $r['asked_again'], implode(' ', $r['first_answer']),
            isset($r['coder_reasons']) ? ($r['coder_reasons'] === [] ? 'posts' : implode('; ', $r['coder_reasons'])) : '',
        ], ',', '"', '');
    }
    fclose($detail);
    file_put_contents($base . '.summary.txt', $summary);
    echo $summary, "\n";
    fwrite(STDERR, "wrote $base.detail.csv (patient data, keep it here) and $base.summary.txt (counts only)\n");
}

/**
 * Every accession Gifford sent no diagnosis for, plus $size of the others at random, spread over
 * the locations in proportion (at least one from each). The same file, size and seed give the
 * same accessions; not the ones the Python script's --sample picks (use --from-run for those).
 *
 * @param list<array<string, mixed>> $accessions
 * @return list<array<string, mixed>>
 */
function dxSample(array $accessions, int $size, int $seed): array
{
    mt_srand($seed);
    $chosen = [];
    $byLocation = [];
    foreach ($accessions as $i => $a) {
        if ($a['gifford'] === []) {
            $chosen[$i] = true;
        } else {
            $byLocation['_' . $a['location']][] = $i;
        }
    }
    $total = array_sum(array_map('count', $byLocation));
    $size = min($size, $total);
    $quota = [];
    foreach ($byLocation as $location => $indexes) {
        $quota[$location] = $size > 0 ? max(1, (int) round($size * count($indexes) / $total)) : 0;
    }
    $order = array_keys($byLocation);
    usort($order, static fn (string $a, string $b): int => count($byLocation[$b]) <=> count($byLocation[$a]));
    while (array_sum($quota) > $size && max($quota) > 1) {
        $quota[array_search(max($quota), $quota, true)]--;
    }
    while (array_sum($quota) < $size) {
        $room = array_values(array_filter($order, static fn (string $l): bool => $quota[$l] < count($byLocation[$l])));
        if ($room === []) {
            break;
        }
        $quota[$room[0]]++;
    }
    foreach ($byLocation as $location => $indexes) {
        for ($n = 0; $n < $quota[$location] && $indexes !== []; $n++) {
            $pick = mt_rand(0, count($indexes) - 1);
            $chosen[$indexes[$pick]] = true;
            array_splice($indexes, $pick, 1);
        }
    }
    $picked = array_values(array_filter($accessions, static fn (int $i): bool => isset($chosen[$i]), ARRAY_FILTER_USE_KEY));
    fwrite(STDERR, 'sample: ' . dxJoin(dxCount(array_column($picked, 'location'))) . "\n");

    return $picked;
}

/**
 * One accession's request, retried on rate limits, overloads and failed connections. Resolves to
 * the plain record kept for it; it never rejects.
 *
 * @param array<string, mixed> $request
 */
function dxAsk(Client $client, string $apiKey, array $request, int $attempt = 0, int $delayMs = 0, int $maxCodes = 0): PromiseInterface
{
    $retry = static fn (int $seconds): PromiseInterface => dxAsk($client, $apiKey, $request, $attempt + 1, $seconds * 1000, $maxCodes);

    return $client->postAsync('/v1/messages', [
        'headers' => ['x-api-key' => $apiKey, 'anthropic-version' => DX_API_VERSION, 'content-type' => 'application/json'],
        'json' => $request,
        'delay' => $delayMs,
    ])->then(
        static function (ResponseInterface $response) use ($retry, $attempt, $maxCodes) {
            $status = $response->getStatusCode();
            $body = json_decode((string) $response->getBody(), true);
            if ($status === 200) {
                return is_array($body) ? icd10ParseResponse($body, $maxCodes) : ['outcome' => 'errored', 'note' => 'the response was not JSON'];
            }
            if (in_array($status, DX_RETRY_STATUS, true)) {
                if ($attempt < DX_MAX_RETRIES) {
                    $after = (int) $response->getHeaderLine('retry-after');

                    return $retry($after > 0 ? min($after, 120) : 2 ** ($attempt + 1));
                }

                return ['outcome' => 'errored', 'note' => $status === 429 ? 'rate limited after retries' : "HTTP $status after retries"];
            }

            return [
                'outcome' => 'errored', 'note' => "HTTP $status", 'fatal' => in_array($status, DX_FATAL_STATUS, true),
                'message' => substr(is_array($body) ? (string) ($body['error']['message'] ?? '') : '', 0, 300),
            ];
        },
        static function (\Throwable $e) use ($retry, $attempt) {
            if ($e instanceof ConnectException && $attempt < DX_MAX_RETRIES) {
                return $retry(2 ** ($attempt + 1));
            }

            return ['outcome' => 'errored', 'note' => $e instanceof ConnectException ? 'connection failed after retries' : 'request failed'];
        }
    );
}

/**
 * The coders' two steps: the request without the facility's indication, and, when that answer
 * shows the report gave no codeable reason, the one with it. The second answer is kept (the
 * first one's codes go along as first_codes); a second request that fails keeps the first.
 *
 * @param array<string, mixed> $request
 * @param ?array<string, mixed> $withIndication null under reads.php's prompt, which has one step
 * @param int $maxCodes the most codes kept from an answer (0: all)
 */
function dxAskTwoSteps(Client $client, string $apiKey, array $request, ?array $withIndication, int $maxCodes = 0): PromiseInterface
{
    return dxAsk($client, $apiKey, $request, 0, 0, $maxCodes)->then(
        static function (array $first) use ($client, $apiKey, $withIndication, $maxCodes) {
            if ($withIndication === null || $first['outcome'] !== 'succeeded' || !icd10NeedsIndication($first['codes'])) {
                return $first;
            }

            return dxAsk($client, $apiKey, $withIndication, 0, 0, $maxCodes)->then(static function (array $second) use ($first): array {
                $usage = $first['usage'];
                foreach ($second['usage'] ?? [] as $name => $n) {
                    $usage[$name] = ($usage[$name] ?? 0) + $n;
                }
                $kept = $second['outcome'] === 'succeeded' ? $second : $first;

                return ['usage' => $usage, 'asked_again' => $second['outcome'], 'first_codes' => $first['codes']] + $kept;
            });
        }
    );
}

/**
 * @param array<string, string> $options
 */
function dxRun(string $file, array $options): void
{
    $resume = $options['resume'] ?? null;
    $meta = null;
    if ($resume !== null) {
        $meta = json_decode((string) file_get_contents($resume), true);
        if (!is_array($meta) || !is_array($meta['accessions'] ?? null)) {
            dxFail("$resume is not a run file");
        }
    }
    $all = dxReadAccessions($file);
    $usable = array_values(array_filter($all, static fn (array $a): bool => trim($a['report']) !== '' && $a['cpts'] !== []));
    $skipped = count($all) - count($usable);
    $selections = array_intersect(['limit', 'sample', 'from-run', 'exams'], array_keys($options));
    if (count($selections) > 1) {
        dxFail('use one of --limit, --sample, --from-run and --exams');
    }
    // A run made before there was a choice used reads.php's prompt alone.
    $prompt = $meta !== null ? (string) ($meta['prompt'] ?? 'reads') : ($options['prompt'] ?? 'coders');
    if ($meta !== null && $prompt === 'coders') {
        dxFail("that run was made with an earlier version of the coders' prompt and cannot be resumed");
    }
    // The run file names the two-step version, so its runs are not mistaken for the earlier ones.
    $prompt = $prompt === 'coders' ? 'coders-2step' : $prompt;
    if (!in_array($prompt, ['coders-2step', 'reads'], true)) {
        dxFail('--prompt is coders or reads');
    }
    $lists = isset($options['lists']) ? dxLoadLists($options['lists']) : null;
    if ($lists === null && ($prompt !== 'reads' || isset($options['exams']))) {
        dxFail("--lists gifford-dx-lists.json is needed for the coders' prompt and for --exams");
    }
    if ($prompt !== 'reads' && $lists['quick_pick'] === []) {
        dxFail('that lists file has no quick-pick list: make it again with gifford-dx-lists.py');
    }
    if (isset($options['limit'])) {
        $usable = array_slice($usable, 0, (int) $options['limit']);
    } elseif (isset($options['sample'])) {
        $usable = dxSample($usable, (int) $options['sample'], (int) ($options['seed'] ?? 20261114));
    } elseif (isset($options['exams'])) {
        $usable = array_values(array_filter($usable, static fn (array $a): bool => dxExamFor($lists, $a['cpts']) !== null));
    } elseif (isset($options['from-run'])) {
        $earlier = json_decode((string) file_get_contents($options['from-run']), true);
        if (!is_array($earlier) || !is_array($earlier['accessions'] ?? null)) {
            dxFail("{$options['from-run']} is not a run file");
        }
        $byNumber = [];
        foreach ($usable as $a) {
            $byNumber['k' . $a['accession']] = $a;
        }
        $keys = array_map('strval', array_keys($earlier['accessions']));
        sort($keys, SORT_STRING);
        $usable = [];
        foreach ($keys as $key) {
            $number = 'k' . $earlier['accessions'][$key]['accession'];
            if (!isset($byNumber[$number])) {
                dxFail('an accession of that run is not in this file (or has no report now)');
            }
            $usable[] = $byNumber[$number];
        }
    }
    $ids = array_map(static fn (int $i): string => sprintf('a%05d', $i), array_keys($usable));
    $noDx = count(array_filter($usable, static fn (array $a): bool => $a['gifford'] === []));
    fwrite(STDERR, sprintf(
        "%d accessions (%d with no dx from Gifford); %d left out for having no report or no code\n",
        count($usable),
        $noDx,
        $skipped
    ));
    $model = $meta['model'] ?? $options['model'] ?? 'claude-opus-5-5';
    $effort = $meta['effort'] ?? $options['effort'] ?? 'high';
    if (!in_array($effort, ['low', 'medium', 'high', 'xhigh', 'max'], true)) {
        dxFail('--effort is low, medium, high, xhigh or max');
    }
    $requests = [];
    $second = [];
    $additions = $prompt === 'reads' ? null : icd10CoderAdditions($lists['quick_pick']);
    foreach ($usable as $i => $a) {
        $exam = $additions === null ? null : dxExamFor($lists, $a['cpts']);
        $requests[$ids[$i]] = icd10Request($model, $effort, $a['cpts'], $a['report'], $additions, '', $exam['notes'] ?? []);
        // The second step's request, sent only if the first answer calls for it.
        // It says so when the facility sent no indication: the coders' ruling for a study
        // with no symptom from either source applies only once both have been looked at.
        $second[$ids[$i]] = $additions === null
            ? null
            : icd10Request($model, $effort, $a['cpts'], $a['report'], $additions, trim($a['clinical']) === '' ? DX_NO_INDICATION : $a['clinical'], $exam['notes'] ?? []);
    }
    if (isset($options['dry-run'])) {
        $chars = array_sum(array_map(static fn (array $r): int => mb_strlen($r['messages'][0]['content']), $requests));
        $first = reset($requests);
        fwrite(STDERR, sprintf(
            "dry run, nothing sent: %d requests, %s characters of reports, system prompt %s characters (prompt %s)\n",
            count($requests),
            number_format($chars),
            number_format($first === false ? 0 : mb_strlen($first['system'][0]['text'])),
            $prompt
        ));

        return;
    }

    $kept = ['accession', 'lines', 'cpts', 'gifford', 'location', 'pos', 'status', 'payer'];
    if ($meta !== null) {
        $runPath = $resume;
        $then = array_map(static fn (array $a): string => (string) $a['accession'], $meta['accessions']);
        ksort($then, SORT_STRING);
        if (array_values($then) !== array_column($usable, 'accession') || array_map('strval', array_keys($then)) !== $ids) {
            dxFail('this file with this selection does not give the accessions the run file was made from');
        }
    } else {
        $created = date('Ymd-His');
        $runPath = "gifford-dx-run-$created.json";
        if (file_exists($runPath)) {
            dxFail("$runPath exists already: wait a second and run again");
        }
        $meta = [
            'created' => $created, 'model' => $model, 'effort' => $effort, 'prompt' => $prompt,
            'file' => basename($file), 'accessions' => [],
        ];
        foreach ($usable as $i => $a) {
            $meta['accessions'][$ids[$i]] = array_intersect_key($a, array_flip($kept));
        }
        $handle = dxOpenPrivate($runPath, 'w');
        fwrite($handle, json_encode($meta, JSON_THROW_ON_ERROR));
        fclose($handle);
    }
    $resultsPath = preg_replace('/\.json$/', '', $runPath) . '.results.jsonl';
    $done = dxLoadResults($resultsPath);
    $todo = array_values(array_filter($ids, static fn (string $id): bool => ($done[$id]['outcome'] ?? '') !== 'succeeded'));
    fwrite(STDERR, sprintf("%d to ask, %d already answered; run file %s\n", count($todo), count($ids) - count($todo), $runPath));

    $apiKey = (string) getenv('ANTHROPIC_API_KEY');
    if ($apiKey === '' && $todo !== []) {
        dxFail('ANTHROPIC_API_KEY is not set');
    }
    $client = new Client([
        'base_uri' => getenv('ANTHROPIC_BASE_URL') ?: 'https://api.anthropic.com',
        'http_errors' => false, 'timeout' => 600, 'connect_timeout' => 10,
    ]);
    $results = dxOpenPrivate($resultsPath, 'a');
    $record = static function (string $id, array $result) use ($results): void {
        fwrite($results, json_encode($result + ['id' => $id], JSON_THROW_ON_ERROR) . "\n");
        fflush($results);
    };

    if ($todo !== []) {
        // The first request alone: a refused key, model or setting stops here, not 1,300 times.
        $cap = $prompt === 'reads' ? 0 : DX_MAX_CODES;
        $first = dxAskTwoSteps($client, $apiKey, $requests[$todo[0]], $second[$todo[0]], $cap)->wait();
        $record($todo[0], $first);
        if ($first['fatal'] ?? false) {
            fclose($results);
            dxFail("stopped at the first request: {$first['note']}: " . ($first['message'] ?? ''));
        }
        $rest = array_slice($todo, 1);
        $asked = 1;
        $pool = new Pool($client, (static function () use ($client, $apiKey, $requests, $second, $rest, $cap) {
            foreach ($rest as $id) {
                yield $id => static fn (): PromiseInterface => dxAskTwoSteps($client, $apiKey, $requests[$id], $second[$id], $cap);
            }
        })(), [
            'concurrency' => max(1, (int) ($options['workers'] ?? 4)),
            'fulfilled' => static function (array $result, string $id) use ($record, &$asked, $todo): void {
                $record($id, $result);
                if (++$asked % 50 === 0) {
                    fwrite(STDERR, "$asked of " . count($todo) . " asked\n");
                }
            },
        ]);
        $pool->promise()->wait();
    }
    fclose($results);
    dxWriteOutputs($runPath, $options['valid'] ?? null, $options['lists'] ?? null, null);
    fwrite(STDERR, "if some errored, ask only those again with: --resume $runPath\n");
}

$command = $argv[1] ?? '';
if ($command === 'run') {
    [$positional, $options] = dxArgs(array_slice($argv, 2), ['dry-run', 'exams']);
    if (count($positional) !== 1) {
        dxFail('run takes the charge file; see the top of this script');
    }
    dxRun($positional[0], $options);
} elseif ($command === 'summary') {
    [$positional, $options] = dxArgs(array_slice($argv, 2), []);
    if (count($positional) !== 1) {
        dxFail('summary takes the run file; see the top of this script');
    }
    dxWriteOutputs($positional[0], $options['valid'] ?? null, $options['lists'] ?? null, $options['file'] ?? null);
} else {
    dxFail('usage: php dx_compare.php run FILE [options] | summary RUN.json [options]; see the top of this script');
}
