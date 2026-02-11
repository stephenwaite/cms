<?php

// Parse the guarantor file and build an index by GARNO
function buildGarnoIndex($garfile) {
    $index = [];
    
    if (($handle = fopen($garfile, "r")) !== FALSE) {
        while (($line = fgets($handle)) !== FALSE) {
            // Fixed-length positions based on COBOL layout
            $garno = trim(substr($line, 0, 8));     // G-GARNO (8 chars)
            $sex = trim(substr($line, 116, 1));     // G-SEX (1 char)
            $dob = trim(substr($line, 119, 8));     // G-DOB (8 chars YYYYMMDD)
            
            $index[$garno] = [
                'garno' => $garno,
                'sex' => $sex,
                'dob' => $dob
            ];
        }
        fclose($handle);
    }
    
    return $index;
}

// Parse charcur file and build index by key
function buildCharcurIndex($charcurFile) {
    $index = [];
    
    if (($handle = fopen($charcurFile, "r")) !== FALSE) {
        while (($line = fgets($handle)) !== FALSE) {
            // Parse charcur fields
            $cc_key8 = trim(substr($line, 0, 8));
            $cc_proc1 = trim(substr($line, 37, 5));
            $cc_date_t = trim(substr($line, 83, 8));
            $cc_docp = trim(substr($line, 59, 2));  // Rendering provider
            
            // Create composite key
            $key = $cc_key8 . '|' . $cc_proc1 . '|' . $cc_date_t;
            
            // Store (take first match if duplicates)
            if (!isset($index[$key])) {
                $index[$key] = [
                    'cc_docp' => $cc_docp
                ];
            }
        }
        fclose($handle);
    }
    
    return $index;
}

// Calculate age from DOB and service date (both in YYYYMMDD format)
function calculateAge($dob, $serviceDate) {
    if (empty($dob) || empty($serviceDate) || strlen($dob) != 8 || strlen($serviceDate) != 8) {
        return '';
    }
    
    $dobYear = intval(substr($dob, 0, 4));
    $dobMonth = intval(substr($dob, 4, 2));
    $dobDay = intval(substr($dob, 6, 2));
    
    $serviceYear = intval(substr($serviceDate, 0, 4));
    $serviceMonth = intval(substr($serviceDate, 4, 2));
    $serviceDay = intval(substr($serviceDate, 6, 2));
    
    $age = $serviceYear - $dobYear;
    
    // Adjust if birthday hasn't occurred yet this year
    if ($serviceMonth < $dobMonth || ($serviceMonth == $dobMonth && $serviceDay < $dobDay)) {
        $age--;
    }
    
    return $age;
}

// Map provider code to NPI
function getProviderNPI($providerCode) {
    return match(trim($providerCode)) {
        '06' => '1194737833',
        '08' => '1407002355',
        '09' => '1174889182',
        '10' => '1487884953',
        default => ''
    };
}

// Parse the input CSV (space-delimited)
function parseInputCSV($filename) {
    $records = [];
    
    if (($handle = fopen($filename, "r")) !== FALSE) {
        while (($line = fgets($handle)) !== FALSE) {
            $fields = preg_split('/\s+/', trim($line));
            
            if (count($fields) >= 5) {
                $records[] = [
                    'col1' => $fields[0],      // 145
                    'col2' => $fields[1],      // 77002
                    'col3' => $fields[2],      // 20250516
                    'garno' => $fields[3],     // LAV0651G
                    'col5' => $fields[4],      // G9500
                    'raw' => trim($line)
                ];
            }
        }
        fclose($handle);
    }
    
    return $records;
}

// Write output CSV
function writeOutputCSV($filename, $records) {
    $handle = fopen($filename, 'w');
    if (!$handle) {
        echo "Error: Cannot write to $filename\n";
        return;
    }
    
    // Write header
    fputcsv($handle, [
        'col1',
        'col2',
        'col3',
        'garno',
        'col5',
        'g_dob',
        'g_sex',
        'age',
        'provider_code',
        'provider_npi'
    ]);
    
    // Write data
    foreach ($records as $record) {
        fputcsv($handle, [
        '1.1',
        '107816',
        formatDate($record['col3']),
        '030238095',
        $record['provider_npi'],
        'garno' => $record['garno'],
        $record['age'],
        $record['g_sex'],
        '145',
        $record['col2'],
        '',
        'G9501',
        ]);
    }
    
    fclose($handle);
}

// ============================================
// MAIN
// ============================================

if ($argc < 4) {
    echo "Usage: php lookup_garno.php <input_csv> <guarantor_file> <charcur_file>\n";
    exit(1);
}

$inputCSV = $argv[1];
$garfile = $argv[2];
$charcurFile = $argv[3];

echo "Building GARNO index from guarantor file...\n";
$garnoIndex = buildGarnoIndex($garfile);
echo "Indexed " . count($garnoIndex) . " guarantor records\n\n";

echo "Building charcur index...\n";
$charcurIndex = buildCharcurIndex($charcurFile);
echo "Indexed " . count($charcurIndex) . " charcur records\n\n";

echo "Reading input CSV...\n";
$inputRecords = parseInputCSV($inputCSV);
echo "Read " . count($inputRecords) . " input records\n\n";

// Lookup and enrich
$outputRecords = [];
$foundGarno = 0;
$foundCharcur = 0;
$notFoundGarno = 0;
$notFoundCharcur = 0;

foreach ($inputRecords as $record) {
    $garno = $record['garno'];
    $procCode = $record['col2'];
    $serviceDate = $record['col3'];
    
    // Create lookup key for charcur
    $charcurKey = $garno . '|' . $procCode . '|' . $serviceDate;
    
    $outputRec = [
        '1.1',
        '107816',
        'col1' => $record['col1'],
        'col2' => $record['col2'],
        'col3' => $record['col3'],
        'garno' => $record['garno'],
        'col5' => $record['col5'],
        'g_dob' => '',
        'g_sex' => '',
        'age' => '',
        'provider_code' => '',
        'provider_npi' => ''
    ];
    
    // Lookup guarantor info
    if (isset($garnoIndex[$garno])) {
        $outputRec['g_dob'] = $garnoIndex[$garno]['dob'];
        $outputRec['g_sex'] = $garnoIndex[$garno]['sex'];
        
        // Calculate age
        $outputRec['age'] = calculateAge($garnoIndex[$garno]['dob'], $serviceDate);
        
        $foundGarno++;
    } else {
        $notFoundGarno++;
        echo "Warning: GARNO '$garno' not found in guarantor file\n";
    }
    
    // Lookup charcur info for provider
    if (isset($charcurIndex[$charcurKey])) {
        $providerCode = $charcurIndex[$charcurKey]['cc_docp'];
        $outputRec['provider_code'] = $providerCode;
        $outputRec['provider_npi'] = getProviderNPI($providerCode);
        $foundCharcur++;
    } else {
        $notFoundCharcur++;
        echo "Warning: Charcur record not found for key '$charcurKey'\n";
    }
    
    $outputRecords[] = $outputRec;
}

echo "\n=== SUMMARY ===\n";
echo "Total records: " . count($inputRecords) . "\n";
echo "Found in guarantor file: $foundGarno\n";
echo "Not found in guarantor: $notFoundGarno\n";
echo "Found in charcur file: $foundCharcur\n";
echo "Not found in charcur: $notFoundCharcur\n\n";

// Write output
$outputFile = 'output_with_garno_info.csv';
echo "Writing output to $outputFile...\n";
writeOutputCSV($outputFile, $outputRecords);

echo "\nDone! Output file: $outputFile\n";

function formatDate($dateStr) {
    if (empty($dateStr) || strlen($dateStr) != 8) {
        return '';
    }
    
    $year = substr($dateStr, 0, 4);
    $month = substr($dateStr, 4, 2);
    $day = substr($dateStr, 6, 2);
    
    return $month . '/' . $day . '/' . $year;
}