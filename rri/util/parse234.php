<?php

/**
 * Parse fixed-length record file and filter by date criteria
 * 
 * Echoes lines where:
 * - Date in column 108 (MMDDYYYY) is older than 60 days from today
 * - Date in column 108 is older than 60 days from date in column 120
 * - Date in column 120 is NOT 00000000
 */

$filename = 'input.txt'; // Change to your file path

// Check if file exists
if (!file_exists($filename)) {
    die("Error: File '$filename' not found.\n");
}

// Open the file
$handle = fopen($filename, 'r');
if (!$handle) {
    die("Error: Could not open file '$filename'.\n");
}

// Get today's date
$today = new DateTime();
$sixtyDaysAgo = (clone $today)->modify('-60 days');

// Process each line
$lineNumber = 0;
while (($line = fgets($handle)) !== false) {
    $lineNumber++;
    
    // Extract dates from fixed positions (adjust indices if 0-based vs 1-based)
    // Assuming column 108 means character position 107 (0-indexed)
    // and column 120 means character position 119 (0-indexed)
    
    // Column 108 (positions 107-114 for MMDDYYYY = 8 characters)
    $date108 = substr($line, 107, 8);

    //echo $date108 . " date108\n";
    
    // Column 120 (positions 119-126 for MMDDYYYY = 8 characters)
    $date120 = substr($line, 119, 8);
    
    //echo $date120 . " date120\n";
    //exit;
    // Skip if date in column 120 is 00000000
    if ($date120 === '00000000') {
        continue;
    }
    
    // Parse date from column 108 (MMDDYYYY format)
    $dateTime108 = parseMMDDYYYY($date108);
    if (!$dateTime108) {
        // Skip invalid dates
        continue;
    }
    
    // Parse date from column 120 (MMDDYYYY format)
    $dateTime120 = parseMMDDYYYY($date120);
    if (!$dateTime120) {
        // Skip invalid dates
        continue;
    }
    
    // Check if date in column 108 is older than 60 days from today
    $olderThan60DaysFromToday = $dateTime108 < $sixtyDaysAgo;
    
    $charcurKey = substr($line, 0, 11);
    /* if ($olderThan60DaysFromToday) {
        echo $charcurKey . " dos is older than 60 days from today\n";
    } */
    
    // Check if date in column 120 is older than 60 days from today
    $olderThan60DaysFromDate120 = $dateTime120 < $sixtyDaysAgo;
    
    // Echo line if both conditions are met
    if ($olderThan60DaysFromToday && $olderThan60DaysFromDate120) {
        echo $line;
    } else {
        //echo $line;
    } 
}

fclose($handle);

/**
 * Parse MMDDYYYY date format to DateTime object
 * 
 * @param string $dateStr Date string in MMDDYYYY format
 * @return DateTime|null DateTime object or null if invalid
 */
function parseMMDDYYYY($dateStr) {
    // Validate length
    if (strlen($dateStr) !== 8) {
        return null;
    }
    
    // Extract components
    $month = substr($dateStr, 0, 2);
    $day = substr($dateStr, 2, 2);
    $year = substr($dateStr, 4, 4);
    
    // Validate numeric
    if (!is_numeric($month) || !is_numeric($day) || !is_numeric($year)) {
        return null;
    }
    
    // Create date string in format that DateTime can parse
    $formattedDate = "$year-$month-$day";
    
    // Try to create DateTime object
    try {
        $date = new DateTime($formattedDate);
        
        // Verify the date is valid (handles things like 02/30/2024)
        if ($date->format('Y-m-d') !== $formattedDate) {
            return null;
        }
        
        return $date;
    } catch (Exception $e) {
        echo "couldn't create datetime object\n";
        return null;
    }
}
