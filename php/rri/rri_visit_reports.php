<?php

/**
 * What reports the RRI site holds for one visit, as reads.php (the coder's G) and the measure
 * batch see them: a line per report with the procedure it is filed under, whether its date is
 * the charge's date of service, and how long the text is. No names, numbers or report text
 * are printed.
 *
 *   php rri_visit_reports.php <acct (MRN)> <visit number> <date of service YYYYMMDD> [cpt]
 *
 * reads.php shows the reports dated the date of service; the measure batch takes the one of
 * those whose procedure names the charge's CPT. Needs reads.php's environment.
 */

declare(strict_types=1);

require_once __DIR__ . '/rri_reports.php';

if ($argc < 4) {
    measFail('usage: php rri_visit_reports.php <acct> <visit number> <date of service YYYYMMDD> [cpt]');
}
[, $acct, $visit, $dos] = $argv;
$cpt = $argv[4] ?? '';
$found = (new RriReports())->visit($acct, $visit);
if ($found === []) {
    echo "nothing found for that account and visit number\n";
    exit(0);
}
echo count($found) . " reports on the visit:\n";
foreach ($found as $r) {
    echo sprintf(
        "  %-60s %s  %6d characters%s\n",
        substr(preg_replace('/\d{6,}/', '[number]', $r['display']) ?? '', 0, 60),
        $r['date'] === $dos ? 'on the date of service' : 'ANOTHER DATE          ',
        strlen($r['text']),
        $cpt !== '' && str_contains($r['display'], $cpt) ? '  <- names ' . $cpt : ''
    );
}
