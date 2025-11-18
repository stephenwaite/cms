<?php
/**
 * Parse fixed-length record file and split by date criteria
 * 
 * Splits records into two files based on TODAY'S DATE:
 * - File 1: Claims where EITHER date is less than 30 days old from TODAY OR is 00000000
 * - File 2: All other claims (BOTH dates are 30+ days old from TODAY and not 00000000)
 */

// Configuration
$inputFile = 'input_claims.txt';
$outputFile1 = 'claims_recent.txt';  // Either date recent or 00000000
$outputFile2 = 'claims_older.txt';   // Both dates are old (30+ days)

// Date column positions (0-indexed, so subtract 1 from column number)
$dateColumn1 = 107;  // Column 108 (0-indexed)
$dateColumn2 = 119;  // Column 120 (0-indexed)
$dateLength = 8;     // MMDDYYYY format

// Days threshold
$daysThreshold = 30;

// Get today's date for comparison
$today = new DateTime();
$todayFormatted = $today->format('m/d/Y');

/**
 * Parse MMDDYYYY date string to DateTime
 * 
 * @param string $dateStr Date in MMDDYYYY format
 * @return DateTime|null DateTime object or null if invalid
 */
function parseDate($dateStr) {
    if (strlen($dateStr) !== 8 || !ctype_digit($dateStr)) {
        return null;
    }
    
    $month = substr($dateStr, 0, 2);
    $day = substr($dateStr, 2, 2);
    $year = substr($dateStr, 4, 4);
    
    // Validate date components
    if (!checkdate((int)$month, (int)$day, (int)$year)) {
        return null;
    }
    
    return DateTime::createFromFormat('m-d-Y', "$month-$day-$year");
}

/**
 * Check if date is less than specified days from TODAY
 * 
 * @param string $dateStr Date in MMDDYYYY format
 * @param int $days Number of days threshold
 * @param DateTime $referenceDate Reference date (today)
 * @return bool True if date is less than $days old from reference date
 */
function isLessThanDaysOld($dateStr, $days, $referenceDate) {
    $date = parseDate($dateStr);
    if ($date === null) {
        return false;
    }
    
    // Calculate difference from today
    $interval = $referenceDate->diff($date);
    $daysDiff = (int)$interval->format('%r%a'); // Signed days difference
    
    // Positive means date is in the past, negative means future
    // Recent = date is within the last $days days (0 to $days-1 days ago)
    return $daysDiff >= 0 && $daysDiff < $days;
}

/**
 * Check if date is blank/zero (00000000)
 * 
 * @param string $dateStr Date string to check
 * @return bool True if date is 00000000
 */
function isBlankDate($dateStr) {
    return $dateStr === '00000000';
}

/**
 * Check if date qualifies as OLD (not recent and not blank)
 * A date is OLD if:
 * - It's NOT 00000000 AND
 * - It's 30+ days old from TODAY
 * 
 * @param string $dateStr Date in MMDDYYYY format
 * @param int $days Number of days threshold
 * @param DateTime $referenceDate Reference date (today)
 * @return bool True if date is old (30+ days and not blank)
 */
function isOldDate($dateStr, $days, $referenceDate) {
    // If blank, it's not considered "old"
    if (isBlankDate($dateStr)) {
        return false;
    }
    
    // If recent (< 30 days), it's not old
    if (isLessThanDaysOld($dateStr, $days, $referenceDate)) {
        return false;
    }
    
    // Check if it's a valid date that's 30+ days old
    $date = parseDate($dateStr);
    if ($date === null) {
        // Invalid dates are considered "old" for safety
        return true;
    }
    
    // It's a valid date and >= 30 days old
    return true;
}

/**
 * Extract date from fixed position in record
 * 
 * @param string $record The record line
 * @param int $position Starting position (0-indexed)
 * @param int $length Length of date field
 * @return string The extracted date string
 */
function extractDate($record, $position, $length) {
    if (strlen($record) < $position + $length) {
        return str_repeat('0', $length); // Return blank date if record too short
    }
    return substr($record, $position, $length);
}

/**
 * Format date string for display (MMDDYYYY -> MM/DD/YYYY)
 * 
 * @param string $dateStr Date in MMDDYYYY format
 * @return string Formatted date string
 */
function formatDateForDisplay($dateStr) {
    if (strlen($dateStr) !== 8) {
        return $dateStr;
    }
    return substr($dateStr, 0, 2) . '/' . substr($dateStr, 2, 2) . '/' . substr($dateStr, 4, 4);
}

// Main processing
try {
    // Check if input file exists
    if (!file_exists($inputFile)) {
        throw new Exception("Input file '$inputFile' not found.");
    }
    
    // Open files
    $input = fopen($inputFile, 'r');
    $output1 = fopen($outputFile1, 'w');
    $output2 = fopen($outputFile2, 'w');
    
    if (!$input || !$output1 || !$output2) {
        throw new Exception("Failed to open files.");
    }
    
    $lineNumber = 0;
    $count1 = 0; // Recent or blank claims
    $count2 = 0; // Both dates old
    
    echo "Processing file: $inputFile\n";
    echo "Today's date: $todayFormatted\n";
    echo "Date columns: 108 and 120 (MMDDYYYY format)\n";
    echo "Threshold: Less than $daysThreshold days old from TODAY OR 00000000\n";
    echo "\nLOGIC:\n";
    echo "  File 1: EITHER date is recent (< 30 days) OR 00000000\n";
    echo "  File 2: BOTH dates are old (>= 30 days) AND NOT 00000000\n";
    echo str_repeat('-', 60) . "\n";
    
    // Process each line
    while (($line = fgets($input)) !== false) {
        $lineNumber++;
        
        // Remove line ending but preserve the rest of the line
        $record = rtrim($line, "\r\n");
        
        // Skip empty lines
        if (empty($record)) {
            continue;
        }
        
        // Extract dates from specified columns
        $date1 = extractDate($record, $dateColumn1, $dateLength);
        $date2 = extractDate($record, $dateColumn2, $dateLength);
        
        // Check if BOTH dates are OLD (30+ days and not 00000000)
        $date1IsOld = isOldDate($date1, $daysThreshold, $today);
        $date2IsOld = isOldDate($date2, $daysThreshold, $today);
        $bothDatesOld = $date1IsOld && $date2IsOld;
        
        // Calculate days difference for display
        $date1Obj = parseDate($date1);
        $date2Obj = parseDate($date2);
        $date1Days = $date1Obj ? (int)$today->diff($date1Obj)->format('%r%a') : 'N/A';
        $date2Days = $date2Obj ? (int)$today->diff($date2Obj)->format('%r%a') : 'N/A';
        
        // Determine status for each date
        $date1Status = isBlankDate($date1) ? 'Blank (00000000)' : 
                       (isLessThanDaysOld($date1, $daysThreshold, $today) ? "Recent ($date1Days days)" : "Old ($date1Days days)");
        $date2Status = isBlankDate($date2) ? 'Blank (00000000)' : 
                       (isLessThanDaysOld($date2, $daysThreshold, $today) ? "Recent ($date2Days days)" : "Old ($date2Days days)");
        
        // Debug info for first few records
        if ($lineNumber <= 10) {
            echo "Line $lineNumber:\n";
            echo "  Date 1 (col 108): " . formatDateForDisplay($date1) . " - $date1Status\n";
            echo "  Date 2 (col 120): " . formatDateForDisplay($date2) . " - $date2Status\n";
            echo "  Both dates old? " . ($bothDatesOld ? "YES" : "NO") . "\n";
            echo "  -> File: " . ($bothDatesOld ? "2 (Both Old)" : "1 (At least one recent/blank)") . "\n\n";
        }
        
        // Write to appropriate file
        // File 2: BOTH dates are old (>= 30 days from TODAY and not 00000000)
        // File 1: Everything else (at least one date is recent or blank)
        if ($bothDatesOld) {
            // Both dates are old - File 2
            fwrite($output2, $line);
            $count2++;
        } else {
            // At least one date is recent or blank - File 1
            fwrite($output1, $line);
            $count1++;
        }
    }
    
    // Close all files
    fclose($input);
    fclose($output1);
    fclose($output2);
    
    // Calculate cutoff date (30 days ago from today)
    $cutoffDate = clone $today;
    $cutoffDate->sub(new DateInterval('P30D'));
    $cutoffFormatted = $cutoffDate->format('m/d/Y');
    
    // Summary
    echo str_repeat('-', 60) . "\n";
    echo "Processing complete!\n\n";
    echo "Reference date (TODAY): $todayFormatted\n";
    echo "Cutoff date (30 days ago): $cutoffFormatted\n";
    echo "Dates on or after cutoff are considered 'recent'\n\n";
    echo "Total records processed: " . ($count1 + $count2) . "\n";
    echo "File 1 (Recent/Blank): $count1 records -> $outputFile1\n";
    echo "  (At least one date < $daysThreshold days from TODAY or 00000000)\n";
    echo "File 2 (Both Old): $count2 records -> $outputFile2\n";
    echo "  (Both dates >= $daysThreshold days from TODAY and not 00000000)\n";
    
} catch (Exception $e) {
    echo "ERROR: " . $e->getMessage() . "\n";
    exit(1);
}

echo "\nDone!\n";
