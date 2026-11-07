<?php

/**
 * Turn the coder's measure 364 notes file into review lines for
 * mea364_acr_fix.php.
 *
 * The notes file is free text with the xxx003c screen pasted per case:
 *
 *   MRN <acct> DOS <yyyymmdd> ...
 *   <mm-dd-yyyy> <proc 7> ...            the charge line
 *   <exam name>
 *   <the coder's note, any lines until the next MRN line>
 *
 * Each case becomes one line: date acct proc | note, with the note's words
 * joined by spaces. The coder adds the answer (1, 2, 3 or 4) as the last
 * word of the line; the fix script skips lines without one. Lines before
 * the first MRN line (the file's own header) are ignored.
 *
 * usage: php mea364_notes_to_review.php <notes file> > <review file>
 * Counts go to stderr. Blocks whose charge line has no 7-digit procedure
 * are listed there by line number and left out.
 */

if ($argc < 2) {
    fwrite(STDERR, "usage: php mea364_notes_to_review.php <notes file>\n");
    exit(2);
}
$lines = file($argv[1], FILE_IGNORE_NEW_LINES);
if ($lines === false) {
    fwrite(STDERR, "cannot read {$argv[1]}\n");
    exit(1);
}

$cases = [];
$current = null;
foreach ($lines as $i => $line) {
    if (preg_match('/^\s*MRN\s+(\S+)\s+DOS\s+(\d{8})\b/', $line, $m)) {
        if ($current !== null) {
            $cases[] = $current;
        }
        $current = ['line' => $i + 1, 'acct' => $m[1], 'dos' => $m[2], 'proc' => '', 'note' => []];
        continue;
    }
    if ($current === null) {
        continue;
    }
    if ($current['proc'] === '' && count($current['note']) === 0 && preg_match('/^\s*\d{2}-\d{2}-\d{4}\s+(\d{5}[0-9A-Z ]{0,2})/', $line, $m)) {
        $current['proc'] = rtrim($m[1]);
        continue;
    }
    if (trim($line) !== '' && !preg_match('/^\*+$/', trim($line))) {
        $current['note'][] = trim($line);
    }
}
if ($current !== null) {
    $cases[] = $current;
}

$written = 0;
$skipped = [];
foreach ($cases as $c) {
    if ($c['proc'] === '') {
        $skipped[] = $c['line'];
        continue;
    }
    $note = preg_replace('/\s+/', ' ', implode(' ', $c['note']));
    echo $c['dos'], ' ', $c['acct'], ' ', $c['proc'], ' | ', $note, "\n";
    $written++;
}
fwrite(STDERR, sprintf("cases %d, written %d, no charge line %d%s\n", count($cases), $written, count($skipped), $skipped ? ' (notes lines ' . implode(', ', $skipped) . ')' : ''));
