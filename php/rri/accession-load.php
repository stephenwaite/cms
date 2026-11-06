<?php
// accession-load.php - keep the accession RRMC sends with each charge, so
// php/rri/rri_docp.php can find the read in openemr by it.
//
// usage: php accession-load.php <hosprri> <accession file>
//
// in:  hosprri, str009's output (and up1070's): one record per line, "##"
//      visit header (visit number cols 3-10), "$$" charge (DOS cols 13-18
//      MMDDYY, CPT 112-116, HCPCS 120-124, accession from col 221). run it
//      before rrmc008's output is copied over hosprri: rrmc008 writes the
//      charge back through a 7 character accession field, which cuts the
//      10 digit one RRMC sends since the MedQ RIS (10/29/26).
// out: one line per charge appended to <accession file> -
//        visit(8) dos(8, yyyymmdd) cpt(5) accession(10)
//      the CPT is R3-CPT, else R3-HCPCS, as rrmc008 picks it, which is what
//      ends up in CD-CPT. a line already in the file is not appended again,
//      so a re-run of the load is harmless. charges whose accession isn't
//      10 digits are counted and left out.
//
// the accession is the number the VITL results carry in OBR-2, which
// openemr stores as procedure_order.control_id (<accession>-<mnemonic>).
// checked on the 11/01/26 file: 125 of 129 charges found their read by it.
//
// prints counts only, no patient data. exits 1 when nothing could be read
// or written, so the load script stops.

$in_file  = $argv[1] ?? '';
$out_file = $argv[2] ?? '';
if (!is_readable($in_file) || empty($out_file)) {
    echo "usage: php accession-load.php <hosprri> <accession file>\n";
    exit(1);
}

$have = [];
if (is_file($out_file)) {
    if (!is_readable($out_file) || !is_writable($out_file)) {
        echo "can't read and write $out_file, nothing done\n";
        exit(1);
    }
    foreach (file($out_file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
        $have[$line] = true;
    }
}

$visit   = '';
$charges = 0;
$no_acc  = 0;
$dupes   = 0;
$new     = [];
foreach (file($in_file, FILE_IGNORE_NEW_LINES) as $line) {
    $type = substr($line, 0, 2);
    if ($type === '##') {
        $visit = trim(substr($line, 2, 8));
        continue;
    }
    if ($type !== '$$') {
        continue;
    }
    $charges++;
    $line = str_pad($line, 230);
    $d    = substr($line, 12, 6);
    $dos  = ctype_digit($d) ? '20' . substr($d, 4, 2) . substr($d, 0, 2) . substr($d, 2, 2) : '';
    $cpt  = trim(substr($line, 111, 5));
    if ($cpt === '') {
        $cpt = trim(substr($line, 119, 5));
    }
    $accession = trim(substr($line, 220));
    if ($visit === '' || $dos === '' || $cpt === '' || !preg_match('/^\d{10}$/', $accession)) {
        $no_acc++;
        continue;
    }
    $out = sprintf('%-8s %s %-5s %s', $visit, $dos, $cpt, $accession);
    if (isset($have[$out])) {
        $dupes++;
        continue;
    }
    $have[$out] = true;
    $new[]      = $out;
}

if ($new !== []) {
    $written = file_put_contents($out_file, implode("\n", $new) . "\n", FILE_APPEND | LOCK_EX);
    if ($written === false) {
        echo "can't write $out_file, nothing done\n";
        exit(1);
    }
}

echo "accession-load: $charges charges, " . count($new) . " added to $out_file, $dupes already there, " .
     "$no_acc without a 10 digit accession\n";
