<?php
// Configuration
$inputFile = 'input.txt';           // Input file name
$outputNot003 = 'output_not_003.txt'; // Lines without 3-digit key of 003
$output003Criteria = 'output_003_criteria.txt'; // Lines with 003 matching date criteria

// Read the file
$lines = file($inputFile, FILE_IGNORE_NEW_LINES);

if ($lines === false) {
    die("Error: Could not read file '$inputFile'\n");
}

// Sort the lines
$sortedLines = $lines;
usort($sortedLines, function($a, $b) {
    $num_a = substr($a, 31, 3);
    $num_b = substr($b, 31, 3);
    
    $date_a = substr($a, 107, 8);
    $date_b = substr($b, 107, 8);
    
    $num_a = (int)$num_a;
    $num_b = (int)$num_b;
    $date_a = (int)$date_a;
    $date_b = (int)$date_b;
    
    if ($num_a !== $num_b) {
        return $num_a - $num_b;
    }
    
    return $date_a - $date_b;
});

// Filter and process lines
$not003 = [];
$criteria003 = [];
$today = time();

foreach ($sortedLines as $line) {
    $threeDigit = substr($line, 31, 3);
    $date120 = substr($line, 119, 8); // MMDDYYYY format
    
    // Parse date at column 120
    $month120 = substr($date120, 0, 2);
    $day120 = substr($date120, 2, 2);
    $year120 = substr($date120, 4, 4);
    $timestamp120 = strtotime("$year120-$month120-$day120");
    
    // Check if column 120 date is within past 30 days - if so, skip this line entirely
    if ($timestamp120 !== false) {
        $daysDiff120 = ($today - $timestamp120) / 86400; // Positive if in past
        
        if ($daysDiff120 >= 0 && $daysDiff120 <= 30) {
            continue; // Skip this line
        }
    }
    
    // Process lines that aren't 003
    if ($threeDigit !== '003') {
        $not003[] = $line;
        continue;
    }
    
    // Process lines that ARE 003
    if ($threeDigit === '003') {
        $date108 = substr($line, 107, 8); // MMDDYYYY format
        $date135 = substr($line, 134, 8); // YYYYMMDD format
        
        // Parse date at column 108 (MMDDYYYY)
        $month108 = substr($date108, 0, 2);
        $day108 = substr($date108, 2, 2);
        $year108 = substr($date108, 4, 4);
        $timestamp108 = strtotime("$year108-$month108-$day108");
        
        // Parse date at column 135 (YYYYMMDD)
        $year135 = substr($date135, 0, 4);
        $month135 = substr($date135, 4, 2);
        $day135 = substr($date135, 6, 2);
        $timestamp135 = strtotime("$year135-$month135-$day135");
        
        if ($timestamp108 !== false && $timestamp135 !== false) {
            // Check if column 108 date is more than 60 days ago
            $daysDiff108 = ($today - $timestamp108) / 86400;
            
            // Check if column 135 date is more than 30 days in future (older than 30 days from today)
            $daysDiff135 = ($today - $timestamp135) / 86400;
            
            // Include if: column 108 is > 60 days ago AND column 135 is > 30 days old
            if ($daysDiff108 > 60 && $daysDiff135 > 30) {
                $criteria003[] = $line;
            }
        }
    }
}

// Write filtered files
file_put_contents($outputNot003, implode(PHP_EOL, $not003) . PHP_EOL);
file_put_contents($output003Criteria, implode(PHP_EOL, $criteria003) . PHP_EOL);

echo "Processing complete!\n";
echo "NOT 003 (excluding those with column 120 within past 30 days): $outputNot003 (" . count($not003) . " lines)\n";
echo "003 with criteria (column 108 > 60 days ago AND column 135 > 30 days old, excluding those with column 120 within past 30 days): $output003Criteria (" . count($criteria003) . " lines)\n";
