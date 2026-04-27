#!/usr/bin/env php
<?php
/**
 * reconcile_835.php
 *
 * Reads an 835 EDI file, sums all CLP04 (claim payment amounts),
 * and compares the total against the BPR02 check/EFT amount.
 *
 * Usage:  php reconcile_835.php <835-file> [--detail] [--debug]
 *    or:  php reconcile_835.php < <835-file>
 *
 *  --detail  always show per-claim table (default: only on mismatch)
 *  --debug   dump segment parsing info to stderr
 */

// ---------------------------------------------------------------------------
// 1. Parse args
// ---------------------------------------------------------------------------
$flags      = array_flip(array_filter($argv ?? [], fn($a) => str_starts_with($a, '--')));
$showDetail = isset($flags['--detail']);
$debug      = isset($flags['--debug']);

$file = null;
foreach (array_slice($argv ?? [], 1) as $a) {
    if (!str_starts_with($a, '--')) { $file = $a; break; }
}

// ---------------------------------------------------------------------------
// 2. Load raw EDI
// ---------------------------------------------------------------------------
if ($file !== null) {
    if (!is_readable($file)) { fwrite(STDERR, "ERROR: Cannot read: $file\n"); exit(1); }
    $raw = file_get_contents($file);
} else {
    $raw = stream_get_contents(STDIN);
}
if (!$raw) { fwrite(STDERR, "ERROR: Empty input.\n"); exit(1); }

// ---------------------------------------------------------------------------
// 3. Locate the ISA header (skip any leading BOM / CR / LF / whitespace)
// ---------------------------------------------------------------------------
$isaPos = strpos($raw, 'ISA');
if ($isaPos === false) {
    fwrite(STDERR, "ERROR: No ISA header found.\n"); exit(1);
}

// Element separator is the character immediately after "ISA"
$elSep = $raw[$isaPos + 3];

// ---------------------------------------------------------------------------
// 4. Detect segment terminator ROBUSTLY
//
//    The ISA segment has exactly 16 elements (ISA01–ISA16).  We count
//    16 occurrences of the element separator starting from isaPos+3,
//    then the very next character is the segment terminator.
//
//    This is far more reliable than the fixed-offset trick (position 105),
//    which breaks whenever there is a BOM, a preamble line, or when the
//    ISA element separator happens to appear in the padding of ISA02/ISA04.
// ---------------------------------------------------------------------------
$pos = $isaPos + 3;   // position of first element separator
$found = 0;
$len   = strlen($raw);
while ($pos < $len && $found < 16) {
    if ($raw[$pos] === $elSep) $found++;
    $pos++;
}
// $pos now points at the first character of ISA16's value; advance past it
// to find the terminator character that ends the ISA segment.
// ISA16 is exactly 1 character (the component separator), so skip 1 char.
// After the loop, $pos points at ISA16's value (the component separator, e.g. ':').
// The segment terminator is the character immediately after ISA16.
$segTerm = $raw[$pos + 1] ?? '~';

if ($debug) {
    fprintf(STDERR, "DEBUG isaPos=%d  elSep=0x%02X(%s)  segTerm=0x%02X(%s)\n",
        $isaPos, ord($elSep), $elSep, ord($segTerm), $segTerm);
}

// Sanity check: if segTerm is alphanumeric or * something went wrong; fall back to ~
if (ctype_alnum($segTerm) || $segTerm === $elSep) {
    fwrite(STDERR, "WARNING: Detected terminator 0x" . sprintf('%02X', ord($segTerm))
        . " looks wrong; falling back to '~'\n");
    $segTerm = '~';
}

// Strip CR/LF that some systems append after each terminator
$raw = str_replace(["\r\n", "\r", "\n"], '', $raw);

// ---------------------------------------------------------------------------
// 5. Split into segments and parse
//
//    Key BPR and CLP records by sequential $tsIndex (incremented on each ST)
//    rather than by the ST control-number string.  This prevents mismatches
//    caused by address/name tokens that look like "ST" when the wrong
//    terminator is used, and is robust to any ST02 value.
// ---------------------------------------------------------------------------
$segments = explode($segTerm, $raw);
$tsIndex  = -1;
$bprRecs  = [];
$clpByTS  = [];
$allClps  = [];

foreach ($segments as $rawSeg) {
    $seg = trim($rawSeg);
    if ($seg === '') continue;

    $els = explode($elSep, $seg);
    $id  = strtoupper($els[0] ?? '');

    // Guard: only treat as ST if element[1] looks like a transaction-set ID
    // (numeric, 3 digits).  This prevents address words like "ST" from
    // triggering a false transaction-set boundary.
    if ($id === 'ST' && !preg_match('/^\d{3}$/', $els[1] ?? '')) {
        if ($debug) fprintf(STDERR, "DEBUG skipping false ST: %s\n", substr($seg,0,60));
        continue;
    }

    if ($debug) {
        fprintf(STDERR, "DEBUG [ts=%d] %s\n", $tsIndex, substr($seg, 0, 80));
    }

    switch ($id) {

        case 'ST':
            $tsIndex++;
            if ($debug) {
                fprintf(STDERR, "DEBUG ST: tsIndex now %d  ST01=%s ST02=%s\n",
                    $tsIndex, $els[1] ?? '', $els[2] ?? '');
            }
            break;

        // BPR — Financial Information
        //  BPR02  total payment amount
        //  BPR05  payment method: ACH, CHK, NON, CCP, …
        //  BPR10  check/trace number (1-based element 10 → index 10)
        //  BPR16  effective date CCYYMMDD
        case 'BPR':
            $trace = $els[10] ?? '';
            if ($trace === '') $trace = $els[9] ?? '';
            $bprRecs[$tsIndex] = [
                'bpr02'    => isset($els[2]) ? (float)$els[2] : 0.0,
                'method'   => $els[5]  ?? '',
                'trace'    => $trace,
                'eff_date' => $els[16] ?? '',
            ];
            break;

        // CLP — Claim Payment Information
        //  CLP04  claim payment amount ← sum these
        case 'CLP':
            $rec = [
                'patient_ctrl' => $els[1] ?? '',
                'status'       => $els[2] ?? '',
                'charge'       => isset($els[3]) ? (float)$els[3] : 0.0,
                'payment'      => isset($els[4]) ? (float)$els[4] : 0.0,
                'pt_resp'      => isset($els[5]) ? (float)$els[5] : 0.0,
                'icn'          => $els[7] ?? '',
            ];
            $clpByTS[$tsIndex][] = $rec;
            $allClps[]           = $rec;
            break;
    }
}

if ($debug) {
    fprintf(STDERR, "DEBUG done: tsIndex max=%d  BPR keys=[%s]  CLP keys=[%s]\n",
        $tsIndex,
        implode(',', array_keys($bprRecs)),
        implode(',', array_keys($clpByTS))
    );
}

// ---------------------------------------------------------------------------
// 6. Report
// ---------------------------------------------------------------------------
$fmt  = fn(float $v): string => number_format($v, 2);
$hr   = str_repeat('-', 72);

echo "\n835 PAYMENT RECONCILIATION REPORT\n";
echo "File      : " . ($file ? realpath($file) : '(stdin)') . "\n";
echo "Generated : " . date('Y-m-d H:i:s') . "\n";
echo "Delimiters: el=" . json_encode($elSep) . "  seg=" . json_encode($segTerm) . "\n";
echo "$hr\n";

if (empty($bprRecs)) {
    echo "\nERROR: No BPR segment found — is this a valid 835?\n\n"; exit(1);
}

$grandBPR  = 0.0;
$grandCLP  = 0.0;
$allMatch  = true;
$tolerance = 0.005;

foreach ($bprRecs as $idx => $bpr) {

    $bprAmt   = $bpr['bpr02'];
    $claims   = $clpByTS[$idx] ?? [];
    $clpSum   = array_sum(array_column($claims, 'payment'));
    $clpCount = count($claims);
    $diff     = round($clpSum - $bprAmt, 2);
    $match    = abs($diff) <= $tolerance;
    $status   = $match ? '✓ MATCH' : '✗ MISMATCH';

    if (!$match) $allMatch = false;
    $grandBPR += $bprAmt;
    $grandCLP += $clpSum;

    echo "\nTransaction Set #" . ($idx + 1) . "\n";
    if ($bpr['eff_date']) {
        $d = $bpr['eff_date'];
        echo "  Check Date   : " . substr($d,4,2).'/'.substr($d,6,2).'/'.substr($d,0,4) . "\n";
    }
    if ($bpr['trace'])  echo "  Check/Trace# : {$bpr['trace']}\n";
    if ($bpr['method']) echo "  Method       : {$bpr['method']}\n";
    echo "  BPR02 Amount : \$" . $fmt($bprAmt)  . "\n";
    echo "  CLP Sum      : \$" . $fmt($clpSum)  . "  ($clpCount claim(s))\n";
    echo "  Difference   : \$" . $fmt($diff)    . "\n";
    echo "  Status       : $status\n";

    if ($showDetail || !$match) {
        $claims_charge  = array_sum(array_column($claims, 'charge'));
        $claims_ptresp  = array_sum(array_column($claims, 'pt_resp'));

        echo "\n  Claim detail:\n";
        echo "  " . str_repeat('-', 69) . "\n";
        printf("  %-22s %-4s %11s %11s %11s\n", 'PatCtrlNum', 'Stat', 'Charge', 'Payment', 'PtResp');
        echo "  " . str_repeat('-', 69) . "\n";

        if (empty($claims)) {
            echo "  (no CLP segments found under this transaction set)\n";
        } else {
            foreach ($claims as $c) {
                printf("  %-22s %-4s %11s %11s %11s\n",
                    substr($c['patient_ctrl'], 0, 22),
                    $c['status'],
                    $fmt($c['charge']),
                    $fmt($c['payment']),
                    $fmt($c['pt_resp'])
                );
            }
        }

        echo "  " . str_repeat('-', 69) . "\n";
        printf("  %-22s %-4s %11s %11s %11s\n",
            'TOTAL', '',
            $fmt(array_sum(array_column($claims, 'charge'))),
            $fmt($clpSum),
            $fmt(array_sum(array_column($claims, 'pt_resp')))
        );
        echo "  " . str_repeat('-', 69) . "\n";
    }
}

if (count($bprRecs) > 1) {
    $grandDiff = round($grandCLP - $grandBPR, 2);
    echo "\n$hr\n";
    echo "GRAND TOTALS\n";
    echo "  Total BPR02  : \$" . $fmt($grandBPR)  . "\n";
    echo "  Total CLP Sum: \$" . $fmt($grandCLP)  . "\n";
    echo "  Difference   : \$" . $fmt($grandDiff) . "\n";
    echo "  Overall      : " . ($allMatch ? '✓ ALL MATCH' : '✗ DISCREPANCIES FOUND') . "\n";
}

echo "\n$hr\n";
printf("SUMMARY\n");
printf("  Transaction Sets : %d\n", count($bprRecs));
printf("  Total Claims     : %d\n", count($allClps));
printf("  Total Billed     : \$%s\n", $fmt(array_sum(array_column($allClps, 'charge'))));
printf("  Total Paid       : \$%s\n", $fmt(array_sum(array_column($allClps, 'payment'))));
printf("  Total Pt. Resp.  : \$%s\n", $fmt(array_sum(array_column($allClps, 'pt_resp'))));
echo "\n";

exit($allMatch ? 0 : 1);
