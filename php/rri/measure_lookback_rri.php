<?php

/**
 * Look back at the quality measure suggestions the coders were shown on Rutland charges,
 * beside what they keyed, without asking anything again.
 *
 * Runs on the billing server. It reads the suggestions rri-measure-suggest saved
 * (measure-suggested.jsonl, written by measure_charnew.php) and a charcur unload, and matches
 * them by visit number, date of service and CPT. Nothing is fetched and nothing is sent.
 *
 *   php measure_lookback_rri.php CHARCUR [--saved measure-suggested.jsonl]
 *           [--asked YYYYMMDD] [--from YYYYMMDD --to YYYYMMDD] [--list]
 *   php measure_lookback_rri.php CHARNEW --charnew [--from YYYYMMDD --to YYYYMMDD] [--list]
 *
 * --charnew: the file is a charnew unload, and nothing else is read. While a charge is still in
 * charnew its record holds both the suggestion (CD-QS1..3) and the coder's answer (CD-QP1..3),
 * so the day's load can be looked back at from charnew alone. A charge counts as coded once
 * it has a diagnosis (CD-DIAG not blank or zeros). Once charges have moved on to charcur, use
 * the first form: charcur keeps the coder's answers but not the suggestions.
 *
 *   --asked D      only suggestions made on that day (the day of the load)
 *   --from, --to   only dates of service in that range, inclusive
 *   --list         also one line per question where the two differ: the charge key, date of
 *                  service, CPT, measure, suggestion and coder's answer (no name, no account)
 *
 * A blank from the coder is counted as 0, as mea004 bills it. A charge still in charnew is not
 * in charcur yet and is counted as "not coded yet". The output is counts only, unless --list.
 */

declare(strict_types=1);

require_once __DIR__ . '/measure_run.php';

/** Columns of the charcur unload (start, length), from copylib/rri/charcur.cpy. */
const LOOKBACK_CHARCUR = [
    'key' => [0, 11], 'cpt' => [37, 5], 'dos' => [83, 8], 'qp1' => [140, 2], 'qp2' => [142, 2], 'visit' => [144, 7],
    'qp3' => [151, 2],
];

/** Columns of the charnew unload (start, length), from copylib/rri/charnew.cpy. */
const LOOKBACK_CHARNEW = [
    'key' => [0, 11], 'diag' => [26, 7], 'cpt' => [37, 5], 'paycode' => [61, 3], 'dos' => [107, 8],
    'qp1' => [169, 2], 'qp2' => [171, 2], 'qp3' => [173, 2], 'qs1' => [175, 2], 'qs2' => [177, 2], 'qs3' => [179, 2],
];
/** The measures behind each flag paycode, in the order of CD-QP1..3 (as measure_charnew.php). */
const LOOKBACK_FLAGS = [
    '009' => ['QMM26'], '010' => ['364'], '012' => ['405'], '013' => ['406'], '014' => ['364', '405'],
    '015' => ['364', '406'], '016' => ['405', '406'], '017' => ['364', '405', '406'],
];

[$positional, $options] = measArgs(array_slice($argv, 1), ['list', 'charnew']);
if (count($positional) !== 1) {
    measFail('usage: php measure_lookback_rri.php CHARCUR [--saved FILE] [--asked YYYYMMDD] [--from D --to D] [--list]');
}
$savedPath = $options['saved'] ?? 'measure-suggested.jsonl';
$asked = $options['asked'] ?? null;
$from = $options['from'] ?? '00000000';
$to = $options['to'] ?? '99999999';
foreach (array_filter([$asked, $options['from'] ?? null, $options['to'] ?? null], static fn (?string $d): bool => $d !== null) as $d) {
    if (preg_match('/^\d{8}$/', $d) !== 1) {
        measFail('--asked, --from and --to are dates, YYYYMMDD');
    }
}

// The last suggestion saved for each charge (a --refresh run saves a newer one).
$suggested = [];
$coded = [];
if (isset($options['charnew'])) {
    // Suggestion and answer from the same record; no saved file, no charcur.
    $letters = ['H' => 'high', 'M' => 'medium', 'L' => 'low'];
    $lines = @file($positional[0], FILE_IGNORE_NEW_LINES);
    if ($lines === false) {
        measFail('cannot read ' . $positional[0]);
    }
    foreach ($lines as $n => $line) {
        $c = [];
        foreach (LOOKBACK_CHARNEW as $name => [$start, $length]) {
            $c[$name] = trim(substr($line, $start, $length));
        }
        $measures = LOOKBACK_FLAGS[$c['paycode']] ?? null;
        if ($measures === null || $c['dos'] < $from || $c['dos'] > $to) {
            continue;
        }
        $answers = [];
        foreach ($measures as $i => $measure) {
            $qs = $c['qs' . ($i + 1)];
            if (preg_match('/^(\d)([HML])$/', $qs, $m) === 1) {
                $answers[$measure] = ['answer' => (int) $m[1], 'confidence' => $letters[$m[2]]];
            }
        }
        if ($answers === []) {
            continue;
        }
        $id = 'line ' . $n;
        $suggested[$id] = ['measures' => $measures, 'answers' => $answers];
        if ($c['diag'] !== '' && trim($c['diag'], '0') !== '') {
            $coded[$id] = $c;
        }
    }
}
foreach (isset($options['charnew']) ? [] : (is_file($savedPath) ? (file($savedPath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) ?: []) : []) as $line) {
    $r = json_decode($line, true);
    if (!is_array($r) || !isset($r['visit'], $r['dos'], $r['cpt']) || !is_array($r['measures'] ?? null) || !is_array($r['answers'] ?? null)) {
        continue;
    }
    if ($asked !== null && str_replace('-', '', substr((string) ($r['asked'] ?? ''), 0, 10)) !== $asked) {
        continue;
    }
    if ($r['dos'] < $from || $r['dos'] > $to) {
        continue;
    }
    $suggested[$r['visit'] . '|' . $r['dos'] . '|' . $r['cpt']] = $r;
}
if ($suggested === []) {
    measFail(isset($options['charnew']) ? 'no charge with a suggestion in that charnew unload' : "no saved suggestion in $savedPath for that selection");
}

$handle = isset($options['charnew']) ? false : fopen($positional[0], 'r');
if ($handle === false && !isset($options['charnew'])) {
    measFail('cannot read ' . $positional[0]);
}
while ($handle !== false && ($line = fgets($handle)) !== false) {
    $line = rtrim($line, "\r\n");
    $c = [];
    foreach (LOOKBACK_CHARCUR as $name => [$start, $length]) {
        $c[$name] = trim(substr($line, $start, $length));
    }
    $id = $c['visit'] . '|' . $c['dos'] . '|' . $c['cpt'];
    if (isset($suggested[$id])) {
        $coded[$id] = $c;
    }
}
if ($handle !== false) {
    fclose($handle);
}

$table = [];
$differ = [];
$charges = ['coded' => 0, 'not coded yet' => 0, 'every answer as suggested' => 0];
foreach ($suggested as $id => $s) {
    $c = $coded[$id] ?? null;
    if ($c === null) {
        $charges['not coded yet']++;
        continue;
    }
    $charges['coded']++;
    $same = true;
    foreach (array_values($s['measures']) as $i => $measure) {
        $a = $s['answers'][$measure] ?? null;
        if (!is_array($a) || !is_int($a['answer'] ?? null)) {
            continue;
        }
        $qp = $c['qp' . ($i + 1)] ?? '';
        $keyed = preg_match('/^\d$/', $qp) === 1 ? (int) $qp : null;
        $sure = ['high' => 'H', 'medium' => 'M', 'low' => 'L'][$a['confidence'] ?? ''] ?? 'L';
        $row = sprintf('suggested %d%s, coder %s', $a['answer'], $sure, $keyed === null ? 'blank' : (string) $keyed);
        $table[$measure][$row] = ($table[$measure][$row] ?? 0) + 1;
        $agree = ($keyed ?? 0) === $a['answer'];
        $table[$measure][$agree ? '= same' : '= different'] = ($table[$measure][$agree ? '= same' : '= different'] ?? 0) + 1;
        if (!$agree) {
            $same = false;
            $differ[] = sprintf('%s %s %s %s: suggested %d%s, coder %s', $c['key'], $c['dos'], $c['cpt'], $measure, $a['answer'], $sure, $keyed === null ? 'blank' : (string) $keyed);
        }
    }
    if ($same) {
        $charges['every answer as suggested']++;
    }
}

echo sprintf(
    "Suggestions %s%s: %d charges; coded since %d, not coded yet %d\n",
    isset($options['charnew']) ? 'in charnew' : 'saved',
    $asked === null ? '' : " on $asked",
    count($suggested),
    $charges['coded'],
    $charges['not coded yet']
);
if ($charges['coded'] > 0) {
    echo sprintf(
        "Coded charges where every answer is as suggested (a blank counts as 0): %d of %d (%.1f%%)\n",
        $charges['every answer as suggested'],
        $charges['coded'],
        100 * $charges['every answer as suggested'] / $charges['coded']
    );
}
ksort($table);
foreach ($table as $measure => $rows) {
    $same = $rows['= same'] ?? 0;
    $different = $rows['= different'] ?? 0;
    unset($rows['= same'], $rows['= different']);
    echo sprintf("\nMeasure %s: %d questions, same %d, different %d\n", $measure, $same + $different, $same, $different);
    ksort($rows);
    foreach ($rows as $row => $n) {
        echo sprintf("  %-28s %4d\n", $row, $n);
    }
}
if (isset($options['list']) && $differ !== []) {
    echo "\nDifferent (charge key, date of service, CPT, measure):\n";
    sort($differ);
    foreach ($differ as $row) {
        echo "  $row\n";
    }
}
