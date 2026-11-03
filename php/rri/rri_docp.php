<?php
// rri_docp.php - find the reading radiologist in openemr for charnew
// charges that came in with CD-DOCP = "00" (the facility stopped sending it).
//
// usage: php rri_docp.php <rrr255 extract> <answers out>
//
// in:  rrr255 lines - key(11) mrn(8) visitno(7) dos(8) cpt(5)
// out: one line per charge for rrr256 -
//        key(11) docp(2) space src(4) space note
//      src FEED = the radiologist whose feed the visit's reads came in on
//          NONE = not found, docp stays 00 for the coder, note says why
//
// each radiologist has his own results feed into openemr (vitl filters it
// on his npi), and procedure_order.lab_id is the feed. the read is found
// by the visit number vitl sends (procedure_order.external_id) and the
// exam date (procedure_report.date_collected, obr-7):
//   1. reads for the visit on the dos, all on one feed -> that radiologist
//   2. none on the dos, but all the visit's reads on one feed -> that one
//   3. anything else stays 00
// (openemr's fhir DiagnosticReport doesn't return these reads, so this
// reads the two tables directly with a select-only account.)
//
// the database settings come from the environment (scripts/rri/rri-docp
// loads /etc/cms/rri-docp.env). on a database error no answers file is
// written and it exits 1, so rri-docp stops before rrr256.
//
// prints counts only, no patient data.

// procedure_order.lab_id, one feed per radiologist
const DOCP_BY_LAB_ID = [
    4 => '06', // vitl-dan, Mitchell
    2 => '08', // vhie-trent, Shelton
    5 => '09', // vhie-jed, Hummel
    3 => '10', // vhie-andy, Boyer
];

$in_file  = $argv[1] ?? '';
$out_file = $argv[2] ?? '';
if (!is_readable($in_file) || empty($out_file)) {
    echo "usage: php rri_docp.php <rrr255 extract> <answers out>\n";
    exit(1);
}

$db = [];
foreach (['HOST', 'PORT', 'NAME', 'USER', 'PASS'] as $setting) {
    $db[$setting] = getenv('OEMR_RRI_DB_' . $setting);
    if ($db[$setting] === false || $db[$setting] === '') {
        echo "OEMR_RRI_DB_$setting is not set (see /etc/cms/rri-docp.env), nothing done\n";
        exit(1);
    }
}

// charnew's 7 digit visit -> the 8 digit one vitl sends; visits starting
// with 9 are from before rrmc's switch and go as they are
function oemrVisit(string $visit_no): string
{
    $visit_no = trim($visit_no);
    if (substr($visit_no, 0, 1) == '0') {
        $visit_no = "1" . $visit_no;
    }
    return $visit_no;
}

$charges = [];
foreach (file($in_file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
    $line      = str_pad($line, 39);
    $charges[] = [
        'key'   => substr($line, 0, 11),
        'visit' => oemrVisit(substr($line, 19, 7)),
        'dos'   => substr($line, 26, 8),
    ];
}

// external_id has no index, so one query for all the visits rather than
// a scan of procedure_order per charge
$visits = array_values(array_unique(array_filter(array_column($charges, 'visit'))));
$reads  = [];   // visit => list of [lab_id, exam yyyymmdd or null]
try {
    $pdo = new PDO(
        'mysql:host=' . $db['HOST'] . ';port=' . $db['PORT'] . ';dbname=' . $db['NAME'] . ';charset=utf8mb4',
        $db['USER'],
        $db['PASS'],
        [PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION, PDO::ATTR_TIMEOUT => 30]
    );
    foreach (array_chunk($visits, 500) as $chunk) {
        $stmt = $pdo->prepare(
            "SELECT DISTINCT po.external_id, po.lab_id, DATE_FORMAT(prp.date_collected, '%Y%m%d') AS exam
               FROM procedure_order po
               LEFT JOIN procedure_report prp ON prp.procedure_order_id = po.procedure_order_id
              WHERE po.lab_id IN (2, 3, 4, 5)
                AND po.external_id IN (" . implode(',', array_fill(0, count($chunk), '?')) . ")"
        );
        $stmt->execute($chunk);
        foreach ($stmt->fetchAll(PDO::FETCH_ASSOC) as $row) {
            $reads[$row['external_id']][] = [(int) $row['lab_id'], $row['exam']];
        }
    }
} catch (\PDOException $e) {
    // the message can carry the host and user, never the password
    echo "openemr database error, nothing done: " . $e->getCode() . "\n";
    exit(1);
}

$counts = ['FEED' => 0, 'NONE' => 0];
$out    = [];
foreach ($charges as $charge) {
    $visit_reads = $reads[$charge['visit']] ?? [];
    $dos_labs    = [];
    $all_labs    = [];
    foreach ($visit_reads as [$lab_id, $exam]) {
        $all_labs[$lab_id] = true;
        if ($exam === $charge['dos']) {
            $dos_labs[$lab_id] = true;
        }
    }

    if (count($dos_labs) === 1) {
        [$docp, $src, $note] = [DOCP_BY_LAB_ID[array_key_first($dos_labs)], 'FEED', 'visit + DOS'];
    } elseif (count($dos_labs) > 1) {
        [$docp, $src, $note] = ['00', 'NONE', 'VISIT + DOS READS ON MORE THAN ONE FEED'];
    } elseif (count($all_labs) === 1) {
        [$docp, $src, $note] = [DOCP_BY_LAB_ID[array_key_first($all_labs)], 'FEED', 'visit only (no read on the DOS)'];
    } elseif (count($all_labs) > 1) {
        [$docp, $src, $note] = ['00', 'NONE', 'VISIT READS ON MORE THAN ONE FEED, NONE ON THE DOS'];
    } else {
        [$docp, $src, $note] = ['00', 'NONE', 'NO READ FOR THE VISIT'];
    }

    $out[] = $charge['key'] . $docp . ' ' . $src . ' ' . $note;
    $counts[$src]++;
}

$tmp = $out_file . '.tmp';
file_put_contents($tmp, implode("\n", $out) . (empty($out) ? '' : "\n"));
rename($tmp, $out_file);

echo "rri_docp: " . count($out) . " charges, feed " . $counts['FEED'] .
     ", not found " . $counts['NONE'] . ", db errors 0\n";
