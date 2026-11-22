<?php

/**
 * The same list mea006 makes, from unloads instead of the live files: the charnew charges
 * whose quality measure questions can be read from the report. For trying
 * measure_charnew.php without the compiled programs; nothing here touches charnew.
 *
 *   php measure_extract_unload.php <charnew unload> <garfile unload> <extract out>
 *
 * The unloads are `vutil -unl -t` files (charnew 253 bytes, garfile 315). A charge is listed
 * when mea003 flagged it (paycode 009, 010, 012-017), a radiologist has read it (CD-DOCP not
 * 00 and not 02), the coder has not answered (CD-QP1..3 blank) and no suggestion is on it
 * (CD-QS1..3 blank).
 *
 * out: key(11) acct(8) visitno(7) dos(8) cpt(5) paycode(3) dob(8) clinical(40) admit diag(23) docr(3)
 */

declare(strict_types=1);

if ($argc !== 4) {
    fwrite(STDERR, "usage: php measure_extract_unload.php <charnew unload> <garfile unload> <extract out>\n");
    exit(2);
}
$gar = [];
foreach (file($argv[2], FILE_IGNORE_NEW_LINES) ?: [] as $line) {
    $gar[substr($line, 0, 8)] = [substr(str_pad($line, 285), 277, 8), substr(str_pad($line, 127), 119, 8)];
}
$n = ['read' => 0, 'flagged' => 0, 'unread' => 0, 'answered or suggested' => 0, 'no garfile' => 0, 'listed' => 0];
$out = '';
foreach (file($argv[1], FILE_IGNORE_NEW_LINES) ?: [] as $line) {
    $line = str_pad($line, 253);
    $n['read']++;
    $paycode = substr($line, 61, 3);
    if (!in_array($paycode, ['009', '010', '012', '013', '014', '015', '016', '017'], true)) {
        continue;
    }
    $n['flagged']++;
    if (in_array(substr($line, 59, 2), ['00', '02'], true)) {
        $n['unread']++;
        continue;
    }
    if (trim(substr($line, 169, 12)) !== '') {
        $n['answered or suggested']++;
        continue;
    }
    $g = $gar[substr($line, 0, 8)] ?? null;
    if ($g === null) {
        $n['no garfile']++;
        continue;
    }
    $out .= substr($line, 0, 11) . $g[0] . substr($line, 246, 7) . substr($line, 107, 8) . substr($line, 37, 5) . $paycode
        . $g[1] . substr($line, 183, 40) . substr($line, 223, 23) . substr($line, 56, 3) . "\n";
    $n['listed']++;
}
file_put_contents($argv[3], $out);
echo 'measure_extract_unload: ' . implode(', ', array_map(static fn (string $k, int $v): string => "$k $v", array_keys($n), $n)) . "\n";
