<?php

/**
 * lvam_fill: put the lvam rows cptfind writes (its S65 file) in the
 * Medicare contractor's LVAM request sheet, the spreadsheet that goes with a
 * Part B reopening request for 25 or more claims with the same correction.
 *
 *   php lvam_fill.php <lvam rows> <blank LVAM sheet> <sheet to write> "<correction>"
 *
 * lvam rows, "|" delimited:
 *   1 name as garfile holds it (LAST;FIRST M)   2 Medicare id   3 DOS CCYYMMDD
 *   4 ICN   5 CPT and mods   6 paid   7 garno   8 account
 *   9 allowed   10 deductible (not in rows from an older cptfind)
 *
 * The sheet wants, from row 3 down: A patient's first and last name,
 * B Medicare id, C date of service, D the 13 digit ICN, E procedure code and
 * mods, F the correction needed. The same correction text goes on every row.
 * A claim line (ICN + procedure) listed twice in the rows is written once.
 * The blank sheet is only read; the filled copy is a new file.
 *
 * The amounts do not go in the sheet. They are for the totals printed at the
 * end: what Medicare paid on the lines, and for the lines it paid nothing on,
 * how many went to the deductible and how many it denied (with their sheet
 * rows, to look at before the sheet is sent).
 */

require __DIR__ . '/../../vendor/autoload.php';

use PhpOffice\PhpSpreadsheet\Cell\DataType;
use PhpOffice\PhpSpreadsheet\IOFactory;
use PhpOffice\PhpSpreadsheet\Shared\Date;

const LVAM_HEADERS = [
    'A' => "Patient's name",
    'B' => "Patient's HICN/MBI",
    'C' => 'Date of service',
    'D' => 'ICN',
    'E' => 'Procedure Code',
    'F' => 'Explain Correction Needed',
];
const LVAM_HEADER_ROW = 2;
const LVAM_MIN_CLAIMS = 25;

function lvamFail(string $message): never
{
    fwrite(STDERR, "lvam_fill: $message\n");
    exit(1);
}

/** LAST;FIRST M -> FIRST M LAST. A name with no semicolon is left alone. */
function lvamName(string $garName): string
{
    $parts = explode(';', $garName, 2);
    if (count($parts) < 2) {
        return trim($garName);
    }
    return trim(trim($parts[1]) . ' ' . trim($parts[0]));
}

if ($argc !== 5) {
    lvamFail('usage: php lvam_fill.php <lvam rows> <blank LVAM sheet> <sheet to write> "<correction>"');
}
[, $rowsFile, $blankSheet, $outSheet, $correction] = $argv;
$correction = trim($correction);
if ($correction === '') {
    lvamFail('the correction text is empty');
}
if (!is_readable($rowsFile)) {
    lvamFail("cannot read $rowsFile");
}
if (!is_readable($blankSheet)) {
    lvamFail("cannot read $blankSheet");
}
if (file_exists($outSheet)) {
    lvamFail("$outSheet is already there, not overwriting it");
}

$claims = [];
$duplicates = 0;
$lineNo = 0;
foreach (file($rowsFile, FILE_IGNORE_NEW_LINES) as $line) {
    $lineNo++;
    if (trim($line) === '') {
        continue;
    }
    $f = array_map('trim', explode('|', $line));
    if (count($f) < 6) {
        lvamFail("$rowsFile line $lineNo: " . count($f) . ' columns, expected at least 6');
    }
    [$name, $medicareId, $dos, $icn, $proc, $paid] = $f;
    $proc = preg_replace('/\s+/', ' ', $proc);
    if (!preg_match('/^\d{13}$/', $icn)) {
        lvamFail("$rowsFile line $lineNo: ICN [$icn] is not 13 digits");
    }
    if (
        !preg_match('/^(\d{4})(\d{2})(\d{2})$/', $dos, $d)
        || !checkdate((int) $d[2], (int) $d[3], (int) $d[1])
    ) {
        lvamFail("$rowsFile line $lineNo: date of service [$dos] is not CCYYMMDD");
    }
    if ($name === '' || $medicareId === '' || $proc === '') {
        lvamFail("$rowsFile line $lineNo: name, Medicare id or procedure is blank");
    }
    if (!is_numeric($paid)) {
        lvamFail("$rowsFile line $lineNo: paid [$paid] is not a number");
    }
    $allowed = $f[8] ?? null;
    $deduct = $f[9] ?? null;
    if ($allowed !== null && (!is_numeric($allowed) || !is_numeric($deduct))) {
        lvamFail("$rowsFile line $lineNo: allowed [$allowed] or deductible [$deduct] is not a number");
    }
    $key = "$icn $proc";
    if (isset($claims[$key])) {
        $duplicates++;
        continue;
    }
    $claims[$key] = [
        'name' => lvamName($name),
        'id' => $medicareId,
        'dos' => Date::formattedPHPToExcel((int) $d[1], (int) $d[2], (int) $d[3]),
        'icn' => $icn,
        'proc' => $proc,
        'paid' => (float) $paid,
        'allowed' => $allowed === null ? null : (float) $allowed,
        'deduct' => $allowed === null ? null : (float) $deduct,
    ];
}
if ($claims === []) {
    lvamFail("no rows in $rowsFile, no sheet written");
}

$spreadsheet = IOFactory::load($blankSheet);
$sheet = $spreadsheet->getActiveSheet();
foreach (LVAM_HEADERS as $col => $header) {
    $found = trim((string) $sheet->getCell($col . LVAM_HEADER_ROW)->getValue());
    if ($found !== $header) {
        lvamFail("$blankSheet: " . $col . LVAM_HEADER_ROW . " is [$found], expected [$header]: not the LVAM sheet this was written for");
    }
}
$row = LVAM_HEADER_ROW + 1;
foreach (array_keys(LVAM_HEADERS) as $col) {
    if (trim((string) $sheet->getCell($col . $row)->getValue()) !== '') {
        lvamFail("$blankSheet: row $row is already filled in, give the blank sheet");
    }
}

$paidTotal = 0.0;
$noPayment = 0;
$toDeductible = 0;
$allowedNotPaid = 0;
$deniedRows = [];
$noAmounts = 0;
foreach ($claims as $claim) {
    $sheet->setCellValueExplicit("A$row", $claim['name'], DataType::TYPE_STRING);
    $sheet->setCellValueExplicit("B$row", $claim['id'], DataType::TYPE_STRING);
    // a real date, as typing it in the sheet gives; the column shows m/d/yy
    $sheet->setCellValueExplicit("C$row", $claim['dos'], DataType::TYPE_NUMERIC);
    $sheet->setCellValueExplicit("D$row", $claim['icn'], DataType::TYPE_STRING);
    $sheet->setCellValueExplicit("E$row", $claim['proc'], DataType::TYPE_STRING);
    $sheet->setCellValueExplicit("F$row", $correction, DataType::TYPE_STRING);
    $paidTotal += $claim['paid'];
    if ($claim['paid'] == 0.0) {
        $noPayment++;
        if ($claim['allowed'] === null) {
            $noAmounts++;
        } elseif ($claim['deduct'] > 0.0) {
            $toDeductible++;
        } elseif ($claim['allowed'] > 0.0) {
            $allowedNotPaid++;
        } else {
            $deniedRows[] = $row;
        }
    }
    $row++;
}

IOFactory::createWriter($spreadsheet, 'Xlsx')->save($outSheet);

$count = count($claims);
printf("%d claim lines written to %s\n", $count, $outSheet);
printf("%d duplicate rows left out\n", $duplicates);
printf("%d of the lines show no Medicare payment\n", $noPayment);
if ($noPayment > 0 && $noAmounts === 0) {
    printf("  %d went to the deductible\n", $toDeductible);
    printf("  %d allowed, nothing paid and no deductible\n", $allowedNotPaid);
    printf("  %d denied (nothing allowed)\n", count($deniedRows));
    if ($deniedRows !== []) {
        printf("  sheet rows of the denied lines: %s\n", implode(' ', $deniedRows));
    }
}
printf("Medicare paid on these lines: %.2f\n", $paidTotal);
if ($count < LVAM_MIN_CLAIMS) {
    printf("note: the LVAM sheet is for %d or more claims, this one has %d\n", LVAM_MIN_CLAIMS, $count);
}
