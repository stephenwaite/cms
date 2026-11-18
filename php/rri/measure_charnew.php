<?php

/**
 * Read the quality measure answers from the reports of the charnew charges waiting to be
 * coded, for xxx003c to show the coder beside each question (rri-measure-suggest runs it
 * between rrr258 and rrr259).
 *
 *   php measure_charnew.php <rrr258 extract> <answers out> [--model M] [--effort E] [--workers 4]
 *
 * in:  rrr258 lines - key(11) acct(8) visitno(7) dos(8) cpt(5) paycode(3) dob(8)
 *      clinical(40) admit diag(23)
 * out: one line per charge answered, for rrr259 - key(11) qs1(2) qs2(2) qs3(2), a
 *      suggestion being the answer digit and H, M or L (how sure), in the order of the
 *      coder's CD-QP1..3: 364, then 405, then 406 (QMM26 alone in the first).
 *
 * The report comes from the RRI site as reads.php gets it; then one request per charge
 * through the Messages API (the report, the CPT, the age and the clinical line; no names,
 * numbers or dates). A charge with no report yet, or whose request failed, is left out: its
 * question shows no suggestion and the next run tries again. Nothing is answered for the
 * coder: these are suggestions only.
 *
 * Each answer is also kept, with the words of the report it rests on, in
 * measure-charnew-<time>.results.jsonl in the current directory (PATIENT DATA). Counts go to
 * stdout. Needs ANTHROPIC_API_KEY and reads.php's environment.
 */

declare(strict_types=1);

require_once __DIR__ . '/rri_reports.php';

/** The measures behind each flag paycode, in the order of CD-QP1..3 (mea003, mea004). */
const CHARNEW_MEASURES = [
    '009' => ['QMM26'], '010' => ['364'], '012' => ['405'], '013' => ['406'], '014' => ['364', '405'],
    '015' => ['364', '406'], '016' => ['405', '406'], '017' => ['364', '405', '406'],
];

[$positional, $options] = measArgs(array_slice($argv, 1), []);
if (count($positional) !== 2) {
    measFail('usage: php measure_charnew.php <rrr258 extract> <answers out> [--model M] [--effort E] [--workers 4]');
}
[$extractPath, $answersPath] = $positional;
$lines = file($extractPath, FILE_IGNORE_NEW_LINES);
if ($lines === false) {
    measFail("cannot read $extractPath");
}
$model = $options['model'] ?? 'claude-opus-5-5';
$effort = $options['effort'] ?? 'high';
if (!in_array($effort, ['low', 'medium', 'high', 'xhigh', 'max'], true)) {
    measFail('--effort is low, medium, high, xhigh or max');
}

$charges = [];
$bad = 0;
foreach ($lines as $line) {
    if (trim($line) === '') {
        continue;
    }
    $line = str_pad($line, 113);
    $c = [
        'key' => substr($line, 0, 11), 'acct' => trim(substr($line, 11, 8)), 'visit' => trim(substr($line, 19, 7)),
        'dos' => substr($line, 26, 8), 'cpt' => trim(substr($line, 34, 5)), 'paycode' => substr($line, 39, 3),
        'dob' => substr($line, 42, 8), 'clinical' => trim(substr($line, 50, 40)), 'admit' => trim(substr($line, 90, 23)),
    ];
    if (!isset(CHARNEW_MEASURES[$c['paycode']]) || $c['acct'] === '' || $c['visit'] === '' || preg_match('/^\d{8}$/', $c['dos']) !== 1) {
        $bad++;
        continue;
    }
    $charges[] = $c;
}
echo sprintf("measure_charnew: %d charges to read, %d lines not usable\n", count($charges), $bad);
if ($charges === []) {
    file_put_contents($answersPath, '');
    exit(0);
}

$reports = new RriReports();
$requests = [];
$measures = [];
$noReport = 0;
foreach ($charges as $i => $c) {
    $report = $reports->report($c['acct'], $c['visit'], $c['dos'], $c['cpt']);
    if ($report === '') {
        $noReport++;
        continue;
    }
    $id = sprintf('c%05d', $i);
    $born = \DateTimeImmutable::createFromFormat('!Ymd', $c['dob']);
    $on = \DateTimeImmutable::createFromFormat('!Ymd', $c['dos']);
    $age = $born === false || $on === false || $born > $on ? null : $born->diff($on)->y;
    $indication = trim($c['clinical'] . ($c['admit'] === '' ? '' : '; admitting diagnosis: ' . $c['admit']));
    $measures[$id] = CHARNEW_MEASURES[$c['paycode']];
    $requests[$id] = measRequest($model, $effort, $measures[$id], [$c['cpt']], $age, [], $indication, $report);
}
echo sprintf("measure_charnew: %d reports found, %d not there yet\n", count($requests), $noReport);

$resultsPath = 'measure-charnew-' . date('Ymd-His') . '.results.jsonl';
if ($requests !== []) {
    measSendAll($requests, $measures, array_keys($requests), $resultsPath, max(1, (int) ($options['workers'] ?? 4)));
}
$results = measLoadResults($resultsPath);

$out = '';
$written = 0;
$failed = 0;
$tally = [];
foreach ($charges as $i => $c) {
    $id = sprintf('c%05d', $i);
    if (!isset($requests[$id])) {
        continue;
    }
    $r = $results[$id] ?? null;
    if ($r === null || ($r['outcome'] ?? '') !== 'succeeded') {
        $failed++;
        continue;
    }
    $fields = '';
    foreach ($measures[$id] as $measure) {
        $a = $r['answers'][$measure];
        $sure = ['high' => 'H', 'medium' => 'M', 'low' => 'L'][$a['confidence']] ?? 'L';
        $fields .= $a['answer'] . $sure;
        $kind = $a['answer'] === 0 ? ($sure === 'H' ? 'no finding, sure' : 'no finding, not sure') : 'a finding';
        $tally[$measure][$kind] = ($tally[$measure][$kind] ?? 0) + 1;
    }
    $out .= $c['key'] . str_pad($fields, 6) . "\n";
    $written++;
}
file_put_contents($answersPath, $out);
echo sprintf("measure_charnew: %d charges answered, %d requests failed (tried again next run)\n", $written, $failed);
foreach ($tally as $measure => $kinds) {
    ksort($kinds);
    echo sprintf("  %s: %s\n", $measure, implode(', ', array_map(static fn (string $k, int $n): string => "$k $n", array_keys($kinds), $kinds)));
}
if ($failed > 0 && $written === 0) {
    exit(1);
}
