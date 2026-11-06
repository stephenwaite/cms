<?php
// rri_docp_file.php - take the reading radiologist for charnew charges still
// at CD-DOCP = "00" from a corrected RRMC charge file (one re-sent with the
// radiologist filled in), for rrr256 to write.
//
// usage: php rri_docp_file.php <rrr255 extract> <rrmc charge file> <answers out>
//
// in:  rrr255 lines - key(11) mrn(8) visitno(7) dos(8) cpt(5)
//      the rrmc charge file, raw (one long string, records introduced by
//      ##, ++ and $$) or as str009 leaves it (one record per line):
//        ## cols 3-10 the visit, $$ cols 13-18 the date of service mmddyy,
//        cols 90-93 the radiologist (rrmc006 keys on these four letters),
//        cols 112-116 the cpt
// out: one line per charge found in the file, for rrr256 -
//        key(11) docp(2) space src(4) space note
//      src FILE = the file names one of our radiologists for that visit,
//                 dos and cpt
//          NONE = the file has the charge but no usable radiologist; docp
//                 stays 00 for the coder, note says why
//      charges not in the file are not written (rrr256 has nothing to do)
//
// charnew's visit is the file's 8 digit visit without its first digit
// (rrmc006: R1-PATNUM(2:7)). the file's date is mmddyy; charnew's is
// yyyymmdd. the file is read as a whole; nothing is written to it.
//
// prints counts only, no patient data.

// rrmc006's mapping of the file's radiologist to CD-DOCP
const DOCP_BY_NAME = [
    'MITC' => '06',
    'SHEL' => '08',
    'HUMM' => '09',
    'BOYE' => '10',
];

$in_file   = $argv[1] ?? '';
$rrmc_file = $argv[2] ?? '';
$out_file  = $argv[3] ?? '';
if (!is_readable($in_file) || !is_readable($rrmc_file) || empty($out_file)) {
    echo "usage: php rri_docp_file.php <rrr255 extract> <rrmc charge file> <answers out>\n";
    exit(1);
}

// the file's records, in order, each starting with its ## ++ or $$
function rrmcRecords(string $raw): array
{
    if (str_contains($raw, "\n") || str_contains($raw, "\r")) {
        return explode("\n", rtrim(str_replace(["\r\n", "\r"], "\n", $raw), "\n"));
    }
    $parts   = preg_split('/(##|\+\+|\$\$)/', $raw, -1, PREG_SPLIT_DELIM_CAPTURE | PREG_SPLIT_NO_EMPTY) ?: [];
    $records = [];
    $marker  = null;
    foreach ($parts as $part) {
        if ($part === '##' || $part === '++' || $part === '$$') {
            if ($marker !== null) {
                $records[] = $marker;
            }
            $marker = $part;
        } elseif ($marker !== null) {
            $records[] = $marker . $part;
            $marker    = null;
        }
    }
    if ($marker !== null) {
        $records[] = $marker;
    }
    return $records;
}

// visit(7) . dos(yyyymmdd) . cpt(5) => list of the radiologists the file
// names for it (the 4 letters, '' when blank), one per $$ record
$named    = [];
$file_cnt = ['records' => 0, 'charges' => 0, 'blank' => 0, 'unknown' => 0];
$visit    = '';
foreach (rrmcRecords((string) file_get_contents($rrmc_file)) as $record) {
    $file_cnt['records']++;
    $type = substr($record, 0, 2);
    if ($type === '##') {
        $visit = trim(substr($record, 2, 8));
        continue;
    }
    if ($type !== '$$') {
        continue;
    }
    $file_cnt['charges']++;
    $record = str_pad($record, 120);
    $d      = substr($record, 12, 6);
    $dos    = ctype_digit($d) ? '20' . substr($d, 4, 2) . substr($d, 0, 4) : '';
    $name   = strtoupper(trim(substr($record, 89, 4)));
    $cpt    = trim(substr($record, 111, 5));
    if ($name === '') {
        $file_cnt['blank']++;
    } elseif (!isset(DOCP_BY_NAME[$name])) {
        $file_cnt['unknown']++;
    }
    $named[substr($visit, 1, 7) . $dos . $cpt][] = $name;
}

$counts = ['FILE' => 0, 'NONE' => 0, 'not in file' => 0];
$out    = [];
foreach (file($in_file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
    $line  = str_pad($line, 39);
    $key   = substr($line, 0, 11);
    $names = $named[trim(substr($line, 19, 7)) . substr($line, 26, 8) . trim(substr($line, 34, 5))] ?? null;
    if ($names === null) {
        $counts['not in file']++;
        continue;
    }
    $names = array_values(array_unique(array_filter($names, fn($n) => $n !== '')));
    if (count($names) === 0) {
        [$docp, $src, $note] = ['00', 'NONE', 'CHARGE FILE HAS NO RADIOLOGIST'];
    } elseif (count($names) > 1) {
        [$docp, $src, $note] = ['00', 'NONE', 'CHARGE FILE NAMES MORE THAN ONE RADIOLOGIST'];
    } elseif (!isset(DOCP_BY_NAME[$names[0]])) {
        [$docp, $src, $note] = ['00', 'NONE', 'CHARGE FILE RADIOLOGIST NOT ONE OF OURS ' . $names[0]];
    } else {
        [$docp, $src, $note] = [DOCP_BY_NAME[$names[0]], 'FILE', 'charge file ' . basename($rrmc_file)];
    }
    $counts[$src]++;
    $out[] = $key . $docp . ' ' . $src . ' ' . $note;
}

$tmp = $out_file . '.tmp';
file_put_contents($tmp, implode("\n", $out) . (empty($out) ? '' : "\n"));
rename($tmp, $out_file);

echo "rri_docp_file: charge file " . $file_cnt['records'] . " records, " . $file_cnt['charges'] . " charges, " .
     "radiologist blank " . $file_cnt['blank'] . ", not one of ours " . $file_cnt['unknown'] . "\n";
echo "rri_docp_file: " . (count($out) + $counts['not in file']) . " charges at 00, in the file " . count($out) .
     " (set " . $counts['FILE'] . ", left at 00 " . $counts['NONE'] . "), not in the file " . $counts['not in file'] . "\n";
