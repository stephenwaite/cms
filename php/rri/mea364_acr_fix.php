<?php

/**
 * Rewrite measure 364 rows written before mea004 learned the M1018 exclusion.
 *
 * Reads the year's mea004acr-MMDDYY-HHMMSS files (the ACR CSV mea004 writes on
 * each run) and writes them out as one CSV for mea004acr.php, with two changes
 * to measure 364 rows from files dated before the cut:
 *
 *   1. a row answered 1, 2 or 3 (G9345, G9755, G9347) whose charge carries an
 *      exclusion diagnosis gets numerator M1018, the same rule mea004 applies
 *      since #371 (C, D0, Z08, Z85, Z122 on any of the four dx; smoking codes
 *      are left to the coder, the exclusion needs a heavy smoker);
 *      dx 2-4 come from the charcur unload when one is given, else dx 1 only;
 *   2. a row listed in the review file with an answer gets that answer's code
 *      (2 = G9755, 3 = G9347, 4 = M1018), which wins over rule 1.
 *
 * Rows from files dated on or after the cut were written by the new mea004 and
 * are copied unchanged. Counts go to stderr; nothing else is printed.
 *
 * usage: php mea364_acr_fix.php <cut MMDDYY> <out csv> [charcur unload|-] [review file]
 *        [MEA004_DIR=/home/rri] [YEAR=26]
 *
 * review file: the lines of answer2-to-review.txt (date acct proc dx visit)
 * with the coder's answer (2, 3 or 4) added at the end of each line.
 */

const EXCLUSION_DX = '/^(C|D0|Z08|Z85|Z122)/';
const ANSWER_CODE = ['2' => 'G9755', '3' => 'G9347', '4' => 'M1018'];
const FIXABLE = ['G9345', 'G9755', 'G9347'];

if ($argc < 3) {
    fwrite(STDERR, "usage: php mea364_acr_fix.php <cut MMDDYY> <out csv> [charcur unload|-] [review file]\n");
    exit(2);
}
[, $cut, $out] = $argv;
$charcurPath = $argv[3] ?? '-';
$reviewPath = $argv[4] ?? '';
$dir = getenv('MEA004_DIR') ?: '/home/rri';
$year = getenv('YEAR') ?: '26';

if (!preg_match('/^\d{6}$/', $cut)) {
    fwrite(STDERR, "cut must be MMDDYY\n");
    exit(2);
}
$cutKey = substr($cut, 4, 2) . substr($cut, 0, 4);

// Charge diagnoses from the charcur unload, keyed like mea004acr.php matches:
// account|proc|dos. Fixed columns from CharCurParser in mea004acr.php.
$dx = [];
if ($charcurPath !== '-') {
    $h = fopen($charcurPath, 'r');
    if (!$h) {
        fwrite(STDERR, "cannot open charcur unload $charcurPath\n");
        exit(1);
    }
    while (($line = fgets($h)) !== false) {
        $key = substr($line, 0, 8) . '|' . substr($line, 37, 5) . '|' . substr($line, 83, 8);
        $dx[$key] = [trim(substr($line, 26, 7)), trim(substr($line, 108, 7)), trim(substr($line, 115, 7)), trim(substr($line, 133, 7))];
    }
    fclose($h);
}

// Coder answers: date acct proc dx visit answer.
$review = [];
$reviewUnanswered = 0;
if ($reviewPath !== '') {
    foreach (file($reviewPath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
        $f = preg_split('/\s+/', trim($line));
        if (count($f) < 6 || !isset(ANSWER_CODE[$f[5]])) {
            $reviewUnanswered++;
            continue;
        }
        $review[$f[1] . '|' . substr($f[2], 0, 5) . '|' . $f[0]] = ANSWER_CODE[$f[5]];
    }
}

$files = glob("$dir/mea004acr-[0-9][0-9][0-9][0-9]$year-*");
if ($files === false || $files === []) {
    fwrite(STDERR, "no mea004acr files for year $year in $dir\n");
    exit(1);
}
usort($files, static function (string $a, string $b): int {
    return strcmp(fileKey($a), fileKey($b));
});

$o = fopen($out, 'w');
if (!$o) {
    fwrite(STDERR, "cannot write $out\n");
    exit(1);
}
$n = ['files_before' => 0, 'files_after' => 0, 'rows' => 0, 'rows_364_before' => 0,
    'by_dx' => ['G9345' => 0, 'G9755' => 0, 'G9347' => 0], 'by_review' => 0, 'dx_from_charcur' => 0, 'dx1_only' => 0];
$reviewUsed = [];
foreach ($files as $file) {
    $before = strcmp(fileKey($file), $cutKey) < 0;
    $n[$before ? 'files_before' : 'files_after']++;
    foreach (file($file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
        $n['rows']++;
        $f = explode(',', $line);
        if ($before && count($f) >= 10 && trim($f[6]) === '364' && in_array(trim($f[9]), FIXABLE, true)) {
            $n['rows_364_before']++;
            $key = trim($f[1]) . '|' . substr(trim($f[7]), 0, 5) . '|' . trim($f[0]);
            $was = trim($f[9]);
            if (isset($review[$key])) {
                $f[9] = $review[$key];
                $reviewUsed[$key] = true;
                $n['by_review']++;
            } else {
                if (isset($dx[$key])) {
                    $codes = $dx[$key];
                    $n['dx_from_charcur']++;
                } else {
                    $codes = [trim($f[8])];
                    $n['dx1_only']++;
                }
                foreach ($codes as $code) {
                    if ($code !== '' && preg_match(EXCLUSION_DX, $code)) {
                        $f[9] = 'M1018';
                        $n['by_dx'][$was]++;
                        break;
                    }
                }
            }
            $line = implode(',', $f);
        }
        fwrite($o, $line . "\n");
    }
}
fclose($o);

fwrite(STDERR, sprintf(
    "files before cut %d, from cut on %d; rows written %d\n" .
    "364 rows before the cut with answers 1-3: %d\n" .
    "  -> M1018 by dx: %d (was G9345 %d, G9755 %d, G9347 %d); dx from charcur %d, dx 1 only %d\n" .
    "  -> set from the review file: %d of %d answered lines (%d lines had no answer, %d answered keys not found)\n",
    $n['files_before'],
    $n['files_after'],
    $n['rows'],
    $n['rows_364_before'],
    array_sum($n['by_dx']),
    $n['by_dx']['G9345'],
    $n['by_dx']['G9755'],
    $n['by_dx']['G9347'],
    $n['dx_from_charcur'],
    $n['dx1_only'],
    $n['by_review'],
    count($review),
    $reviewUnanswered,
    count($review) - count($reviewUsed)
));

function fileKey(string $path): string
{
    // mea004acr-MMDDYY-HHMMSS -> YYMMDDHHMMSS so string order is date order
    if (!preg_match('/mea004acr-(\d{2})(\d{2})(\d{2})-(\d{6})/', basename($path), $m)) {
        return '';
    }
    return $m[3] . $m[1] . $m[2] . $m[4];
}
