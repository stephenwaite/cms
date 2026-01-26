<?php

class CharCurParser {
    
    private static $fieldDefinitions = [
        // CHARCUR-KEY
        'cc_key8'        => ['start' => 0,   'length' => 8,  'type' => 'string'],
        'cc_key3'        => ['start' => 8,   'length' => 3,  'type' => 'string'],
        
        'cc_patid'       => ['start' => 11,  'length' => 8,  'type' => 'string'],
        'cc_claim'       => ['start' => 19,  'length' => 6,  'type' => 'string'],
        'cc_service'     => ['start' => 25,  'length' => 1,  'type' => 'string'],
        'cc_diag'        => ['start' => 26,  'length' => 7,  'type' => 'string'],
        
        // CC-PROC components
        'cc_proc0'       => ['start' => 33,  'length' => 4,  'type' => 'string'],
        'cc_proc1'       => ['start' => 37,  'length' => 5,  'type' => 'string'],
        'cc_proc2'       => ['start' => 42,  'length' => 2,  'type' => 'string'],
        
        'cc_mod2'        => ['start' => 44,  'length' => 2,  'type' => 'string'],
        'cc_mod3'        => ['start' => 46,  'length' => 2,  'type' => 'string'],
        'cc_mod4'        => ['start' => 48,  'length' => 2,  'type' => 'string'],
        
        'cc_amount'      => ['start' => 50,  'length' => 6,  'type' => 'decimal'], // S9(4)V99
        'cc_docr'        => ['start' => 56,  'length' => 3,  'type' => 'string'],
        'cc_docp'        => ['start' => 59,  'length' => 2,  'type' => 'string'],
        'cc_paycode'     => ['start' => 61,  'length' => 3,  'type' => 'integer'],
        'cc_stud'        => ['start' => 64,  'length' => 1,  'type' => 'string'],
        
        'cc_work'        => ['start' => 65,  'length' => 2,  'type' => 'string'],
        'cc_dat1'        => ['start' => 67,  'length' => 8,  'type' => 'date'],
        'cc_result'      => ['start' => 75,  'length' => 1,  'type' => 'string'],
        'cc_act'         => ['start' => 76,  'length' => 1,  'type' => 'string'],
        'cc_sorcref'     => ['start' => 77,  'length' => 1,  'type' => 'string'],
        'cc_collt'       => ['start' => 78,  'length' => 1,  'type' => 'string'],
        'cc_auth'        => ['start' => 79,  'length' => 1,  'type' => 'string'],
        'cc_paper'       => ['start' => 80,  'length' => 1,  'type' => 'string'],
        
        'cc_place'       => ['start' => 81,  'length' => 1,  'type' => 'string'],
        'cc_epsdt'       => ['start' => 82,  'length' => 1,  'type' => 'string'],
        'cc_date_t'      => ['start' => 83,  'length' => 8,  'type' => 'date'],
        'cc_date_a'      => ['start' => 91,  'length' => 8,  'type' => 'date'],
        'cc_date_p'      => ['start' => 99,  'length' => 8,  'type' => 'date'],
        'cc_rec_stat'    => ['start' => 107, 'length' => 1,  'type' => 'string'],
        
        'cc_dx2'         => ['start' => 108, 'length' => 7,  'type' => 'string'],
        'cc_dx3'         => ['start' => 115, 'length' => 7,  'type' => 'string'],
        
        'cc_acc_type'    => ['start' => 122, 'length' => 1,  'type' => 'string'],
        'cc_date_m'      => ['start' => 123, 'length' => 8,  'type' => 'date'],
        'cc_assign'      => ['start' => 131, 'length' => 1,  'type' => 'string'],
        'cc_neic_assign' => ['start' => 132, 'length' => 1,  'type' => 'string'],
        'cc_dx4'         => ['start' => 133, 'length' => 7,  'type' => 'string'],
        
        'cc_qp1'         => ['start' => 140, 'length' => 2,  'type' => 'string'],
        'cc_qp2'         => ['start' => 142, 'length' => 2,  'type' => 'string'],
        'cc_visitno'     => ['start' => 144, 'length' => 7,  'type' => 'string'],
        'cc_future'      => ['start' => 151, 'length' => 9,  'type' => 'string'],
    ];
    
    public static function parseFile($filename) {
        $handle = fopen($filename, 'r');
        if (!$handle) {
            throw new Exception("Cannot open file: $filename");
        }
        
        $lineNum = 0;
        while (($line = fgets($handle)) !== false) {
            $lineNum++;
            $line = rtrim($line, "\r\n");
            
            yield $lineNum => self::parseLine($line);
        }
        
        fclose($handle);
    }
    
    public static function parseLine($line) {
        $record = [];
        
        foreach (self::$fieldDefinitions as $fieldName => $def) {
            $value = substr($line, $def['start'], $def['length']);
            
            // Type conversion
            switch ($def['type']) {
                case 'decimal':
                    // S9(4)V99 - 6 digits total, implied decimal after 4th digit
                    $value = trim($value);
                    if ($value !== '' && $value !== '000000') {
                        // Handle sign if present
                        $sign = 1;
                        if (substr($value, 0, 1) === '-') {
                            $sign = -1;
                            $value = substr($value, 1);
                        }
                        $value = $sign * (floatval($value) / 100);
                    } else {
                        $value = 0.00;
                    }
                    break;
                    
                case 'integer':
                    $value = trim($value);
                    $value = $value === '' ? 0 : intval($value);
                    break;
                    
                case 'date':
                    // YYYYMMDD format - convert to readable or keep as-is
                    $value = trim($value);
                    if ($value && $value !== '00000000' && strlen($value) === 8) {
                        // Optional: convert to Y-m-d format
                        // $value = substr($value,0,4).'-'.substr($value,4,2).'-'.substr($value,6,2);
                    }
                    break;
                    
                case 'string':
                default:
                    $value = trim($value);
                    break;
            }
            
            $record[$fieldName] = $value;
        }
        
        // Add computed fields
        $record['cc_proc_full'] = trim($record['cc_proc0'] . $record['cc_proc1'] . $record['cc_proc2']);
        $record['cc_key_full'] = $record['cc_key8'] . $record['cc_key3'];
        
        return $record;
    }
}

class CsvMatcher {
    
    // Define CSV field positions (0-indexed)
    private static $csvFields = [
        'date'       => 0,  // 20250102
        'key'        => 1,  // BUR0182G (cc_key8)
        'dob'        => 2,  // 19730226
        'gender'     => 3,  // F
        'field4'     => 4,  // 268
        'field5'     => 5,  // S4R183W03605
        'field6'     => 6,  // MSN15
        'proc_code'  => 7,  // 76536 (cc_proc1 = 5 chars offset of 4 chars)
        'field8'     => 8,  // E041
        'field9'     => 9,  // PM004
        'field10'    => 10, // 005915
    ];
    
    /**
     * Build indexed hash of CHARCUR file for fast lookups
     * Key format: "cc_key8|cc_proc1|cc_date_t"
     */
    public static function buildCharCurIndex($charcurFile) {
        echo "Building index from CHARCUR file...\n";
        $index = [];
        $lineNum = 0;
        
        foreach (CharCurParser::parseFile($charcurFile) as $num => $record) {
            $lineNum = $num;
            
            // Create lookup key
            $key = sprintf("%s|%s|%s", 
                $record['cc_key8'],
                $record['cc_proc1'],
                $record['cc_date_t']
            );
            
            // Store the record (or just line number if memory is tight)
            $index[$key] = $record;
            
            if ($lineNum % 100000 == 0) {
                echo "Indexed $lineNum records...\n";
            }
        }
        
        echo "Index complete: $lineNum records\n";
        return $index;
    }
    
    /**
     * Process CSV and find matches in CHARCUR index
     */
    public static function findMatches($csvFile, $charcurIndex) {
        $handle = fopen($csvFile, 'r');
        if (!$handle) {
            throw new Exception("Cannot open CSV file: $csvFile");
        }
        
        $lineNum = 0;
        $matchCount = 0;
        $noMatchCount = 0;
        
        while (($line = fgets($handle)) !== false) {
            $lineNum++;
            $fields = str_getcsv(trim($line));
            
            if (count($fields) < 8) {
                echo "Line $lineNum: Invalid CSV format\n";
                continue;
            }
            
            // Extract matching fields from CSV
            $csvKey8 = trim($fields[self::$csvFields['key']]);
            $csvProc1 = substr(trim($fields[self::$csvFields['proc_code']]), 0, 5); // 5 chars offset of 4 chars
            $csvDateT = trim($fields[self::$csvFields['date']]);
            
            // Build lookup key
            $lookupKey = sprintf("%s|%s|%s", $csvKey8, $csvProc1, $csvDateT);
            
            // Search for match
            if (isset($charcurIndex[$lookupKey])) {
                $matchCount++;
                $match = $charcurIndex[$lookupKey];
                
                yield [
                    'csv_line' => $lineNum,
                    'csv_data' => $fields,
                    'charcur_match' => $match,
                    'match_key' => $lookupKey
                ];
            } else {
                $noMatchCount++;
                yield [
                    'csv_line' => $lineNum,
                    'csv_data' => $fields,
                    'charcur_match' => null,
                    'match_key' => $lookupKey,
                    'no_match' => true
                ];
            }
            
            if ($lineNum % 1000 == 0) {
                echo "Processed $lineNum CSV records ($matchCount matches, $noMatchCount no match)\n";
            }
        }
        
        fclose($handle);
        
        echo "\nFinal: $lineNum CSV records processed\n";
        echo "Matches: $matchCount\n";
        echo "No Match: $noMatchCount\n";
    }
}

// ============================================
// USAGE EXAMPLE
// ============================================

// Step 1: Build index from CHARCUR file (2M records)
$charcurIndex = CsvMatcher::buildCharCurIndex($argv[1]);

// Step 2: Process CSV and find matches
foreach (CsvMatcher::findMatches($argv[2], $charcurIndex) as $result) {
    if (isset($result['no_match'])) {
        // No match found
        echo "CSV Line {$result['csv_line']}: NO MATCH for key {$result['match_key']}\n";
        
        // Log to error file or handle as needed
        // file_put_contents('no_match.log', json_encode($result['csv_data'])."\n", FILE_APPEND);
        
    } else {
        // Match found!
        //echo "CSV Line {$result['csv_line']}: MATCH FOUND\n";
        $result_npi = match(trim($result['charcur_match']['cc_docp'])) {
            '06' => '1194737833',
            '08' => '1407002355',
            '09' => '1174889182',
            '10' => '1487884953'
        };
        echo implode(',', $result['csv_data']) . ",{$result_npi},1{$result['charcur_match']['cc_visitno']}\n";
        //echo "  CHARCUR Patient ID: {$result['charcur_match']['cc_patid']}\n";
        //echo "  CHARCUR Claim: {$result['charcur_match']['cc_claim']}\n";
        //echo "  CHARCUR Amount: {$result['charcur_match']['cc_amount']}\n";
        //echo "\n";
        
        // Do something with the match
        // - Update database
        // - Write to output file
        // - Process payment, etc.
    }
}