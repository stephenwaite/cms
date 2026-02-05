<?php

// Configuration: How many characters to use for matching
$FNAME_LENGTH = 3;  // Use first 3 chars of first name
$LNAME_LENGTH = 3;  // Use first 3 chars of last name

// Read and parse CSV1 (OpenEMR patient data)
function parseCSV1($filename)
{
    $records = [];
    if (($handle = fopen($filename, "r")) !== false) {
        while (($data = fgetcsv($handle)) !== false) {
            $records[] = [
                'id' => $data[0],
                'fname' => trim($data[1]),
                'lname' => trim($data[2]),
                'dob' => $data[3],
                'raw' => $data
            ];
        }
        fclose($handle);
    }
    return $records;
}

// Read and parse CSV2 (fixed-length COBOL guarantor file)
function parseCSV2($filename)
{
    $records = [];

    if (($handle = fopen($filename, "r")) !== false) {
        while (($line = fgets($handle)) !== false) {
            // Fixed-length positions based on COBOL layout
            $garno = substr($line, 0, 8);           // G-GARNO (8 chars)
            $garname = substr($line, 8, 24);        // G-GARNAME (24 chars)
            $billadd = substr($line, 32, 22);       // G-BILLADD (22 chars)
            $street = substr($line, 54, 22);        // G-STREET (22 chars)
            $city = substr($line, 76, 18);          // G-CITY (18 chars)
            $state = substr($line, 94, 2);          // G-STATE (2 chars)
            $zip = substr($line, 96, 9);            // G-ZIP (9 chars)
            $collt = substr($line, 105, 1);         // G-COLLT (1 char)
            $phone = substr($line, 106, 10);        // G-PHONE (10 chars)
            $sex = substr($line, 116, 1);           // G-SEX (1 char)
            $relate = substr($line, 117, 1);        // G-RELATE (1 char)
            $mstat = substr($line, 118, 1);         // G-MSTAT (1 char)
            $dob = substr($line, 119, 8);           // G-DOB (8 chars YYYYMMDD)

            // Split G-GARNAME on semicolon to get last name and first name
            $nameParts = explode(';', trim($garname));
            $lname = isset($nameParts[0]) ? trim($nameParts[0]) : '';
            $fname = isset($nameParts[1]) ? trim($nameParts[1]) : '';

            $records[] = [
                'garno' => trim($garno),
                'fname' => $fname,
                'lname' => $lname,
                'dob' => trim($dob),
                'billadd' => trim($billadd),
                'street' => trim($street),
                'city' => trim($city),
                'state' => trim($state),
                'zip' => trim($zip),
                'phone' => trim($phone),
                'sex' => trim($sex),
                'raw' => rtrim($line)
            ];
        }
        fclose($handle);
    }

    return $records;
}

// Read and parse filter_77080 output (pipe-delimited)
function parseFilter77080($filename)
{
    $records = [];

    if (($handle = fopen($filename, "r")) !== false) {
        $header = fgets($handle); // Skip header line

        while (($line = fgets($handle)) !== false) {
            $fields = explode('|', trim($line));
            if (count($fields) >= 9) {
                $records[] = [
                    'cc_key8' => trim($fields[0]),
                    'cc_key3' => trim($fields[1]),
                    'garno' => trim($fields[0]), // GARNO is just cc_key8 (8 chars)
                    'cc_patid' => trim($fields[2]),
                    'cc_claim' => trim($fields[3]),
                    'cc_proc1' => trim($fields[4]),
                    'cc_date_t' => trim($fields[5]),
                    'cc_amount' => floatval($fields[6]),
                    'cc_docp' => trim($fields[7]),
                    'cc_visitno' => trim($fields[8]),
                    'raw' => trim($line)
                ];
            }
        }
        fclose($handle);
    }

    return $records;
}

// Create lookup key for matching
function createKey($fname, $lname, $dob, $fnameLen = null, $lnameLen = null)
{
    // Normalize DOB to YYYYMMDD format
    $normalizedDOB = preg_replace('/[^0-9]/', '', $dob);

    // Apply substring limits if specified
    $fnameKey = strtoupper($fname);
    $lnameKey = strtoupper($lname);

    if ($fnameLen !== null) {
        $fnameKey = substr($fnameKey, 0, $fnameLen);
    }
    if ($lnameLen !== null) {
        $lnameKey = substr($lnameKey, 0, $lnameLen);
    }

    return $fnameKey . '|' . $lnameKey . '|' . $normalizedDOB;
}

// Write matched records to CSV
function writeMatchedCSV($filename, $matches)
{
    $handle = fopen($filename, 'w');
    if (!$handle) {
        echo "Error: Cannot write to $filename\n";
        return;
    }

    // Write header
    fputcsv($handle, [
        'csv1_id',
        'csv1_fname',
        'csv1_lname',
        'csv1_dob',
        'garno',
        'csv2_fname',
        'csv2_lname',
        'csv2_dob',
        'csv2_street',
        'csv2_city',
        'csv2_state',
        'csv2_zip',
        'csv2_phone',
        'charge_count',
        'total_amount',
        'first_charge_date',
        'last_charge_date',
        'match_key'
    ]);

    // Write data
    foreach ($matches as $match) {
        fputcsv($handle, [
            $match['csv1']['id'],
            $match['csv1']['fname'],
            $match['csv1']['lname'],
            $match['csv1']['dob'],
            $match['csv2']['garno'],
            $match['csv2']['fname'],
            $match['csv2']['lname'],
            $match['csv2']['dob'],
            $match['csv2']['street'],
            $match['csv2']['city'],
            $match['csv2']['state'],
            $match['csv2']['zip'],
            $match['csv2']['phone'],
            $match['charge_count'],
            $match['total_amount'],
            $match['first_charge_date'],
            $match['last_charge_date'],
            $match['match_key']
        ]);
    }

    fclose($handle);
}

// Write unmatched records to CSV
function writeUnmatchedCSV($filename, $unmatched, $fnameLen, $lnameLen)
{
    $handle = fopen($filename, 'w');
    if (!$handle) {
        echo "Error: Cannot write to $filename\n";
        return;
    }

    // Write header
    fputcsv($handle, [
        'csv1_id',
        'csv1_fname',
        'csv1_lname',
        'csv1_dob',
        'match_key'
    ]);

    // Write data
    foreach ($unmatched as $record) {
        $key = createKey(
            $record['fname'],
            $record['lname'],
            $record['dob'],
            $fnameLen,
            $lnameLen
        );
        fputcsv($handle, [
            $record['id'],
            $record['fname'],
            $record['lname'],
            $record['dob'],
            $key
        ]);
    }

    fclose($handle);
}

// ============================================
// MAIN MATCHING LOGIC
// ============================================

if ($argc < 4) {
    echo "Usage: php match_all.php <csv1_file> <csv2_guarantor_file> <filter77080_output>\n";
    exit(1);
}

$csv1File = $argv[1];
$csv2File = $argv[2];
$filter77080File = $argv[3];

echo "Loading files...\n";
$csv1Records = parseCSV1($csv1File);
$csv2Records = parseCSV2($csv2File);
$filter77080Records = parseFilter77080($filter77080File);

echo "CSV1 records: " . count($csv1Records) . "\n";
echo "CSV2 records: " . count($csv2Records) . "\n";
echo "Filter 77080 records: " . count($filter77080Records) . "\n";
echo "Matching using first $FNAME_LENGTH chars of first name and first $LNAME_LENGTH chars of last name\n\n";

// Step 1: Index CSV2 by name+DOB key
echo "Building CSV2 name+DOB index...\n";
$csv2ByNameDOB = [];
foreach ($csv2Records as $record) {
    $key = createKey(
        $record['fname'],
        $record['lname'],
        $record['dob'],
        $FNAME_LENGTH,
        $LNAME_LENGTH
    );
    if (!isset($csv2ByNameDOB[$key])) {
        $csv2ByNameDOB[$key] = [];
    }
    $csv2ByNameDOB[$key][] = $record;
}

// Step 2: Index filter records by GARNO
echo "Building filter GARNO index...\n";
$filterByGarno = [];
foreach ($filter77080Records as $record) {
    $garno = $record['garno'];
    if (!isset($filterByGarno[$garno])) {
        $filterByGarno[$garno] = [];
    }
    $filterByGarno[$garno][] = $record;
}

// Step 3: For each CSV1 record, try to find matches (deduplicated)
echo "Matching CSV1 records...\n";
$matched = [];
$unmatched = [];
$matchedCSV1Ids = []; // Track which CSV1 IDs we've already matched

foreach ($csv1Records as $csv1Record) {
    $key = createKey(
        $csv1Record['fname'],
        $csv1Record['lname'],
        $csv1Record['dob'],
        $FNAME_LENGTH,
        $LNAME_LENGTH
    );

    // Skip if we've already matched this CSV1 record
    if (isset($matchedCSV1Ids[$csv1Record['id']])) {
        continue;
    }

    $foundMatch = false;

    // Look for CSV2 records with matching name+DOB
    if (isset($csv2ByNameDOB[$key])) {
        foreach ($csv2ByNameDOB[$key] as $csv2Record) {
            $garno = $csv2Record['garno'];

            // Check if this GARNO has any 77080 charges
            if (isset($filterByGarno[$garno])) {
                // Aggregate charge information
                $charges = $filterByGarno[$garno];
                $chargeCount = count($charges);
                $totalAmount = 0;
                $dates = [];

                foreach ($charges as $charge) {
                    $totalAmount += $charge['cc_amount'];
                    $dates[] = $charge['cc_date_t'];
                }

                sort($dates);
                $firstDate = reset($dates);
                $lastDate = end($dates);

                $matched[] = [
                    'csv1' => $csv1Record,
                    'csv2' => $csv2Record,
                    'charge_count' => $chargeCount,
                    'total_amount' => $totalAmount,
                    'first_charge_date' => $firstDate,
                    'last_charge_date' => $lastDate,
                    'match_key' => $key
                ];

                $matchedCSV1Ids[$csv1Record['id']] = true;
                $foundMatch = true;
                break; // Only take the first matching CSV2 record
            }
        }
    }

    if (!$foundMatch) {
        $unmatched[] = $csv1Record;
    }
}

// ============================================
// OUTPUT RESULTS
// ============================================

echo "\n=== SUMMARY ===\n";
echo "Total CSV1 records: " . count($csv1Records) . "\n";
echo "Matched (have 77080 charges): " . count($matched) . "\n";
echo "Unmatched (no 77080 charges): " . count($unmatched) . "\n\n";

// Write CSV files
$matchedFile = 'matched_77080.csv';
$unmatchedFile = 'unmatched_77080.csv';

echo "Writing matched records to $matchedFile...\n";
writeMatchedCSV($matchedFile, $matched);

echo "Writing unmatched records to $unmatchedFile...\n";
writeUnmatchedCSV($unmatchedFile, $unmatched, $FNAME_LENGTH, $LNAME_LENGTH);

echo "\nDone! Output files:\n";
echo "  - $matchedFile (" . count($matched) . " records)\n";
echo "  - $unmatchedFile (" . count($unmatched) . " records)\n";
