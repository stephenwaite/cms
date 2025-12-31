<?php

// Directory path
$archiveDirectoryPath = 'archive' . DIRECTORY_SEPARATOR;

// Prompt the user for a MMDDYY date
echo "Please enter a date in MMDDYY format: ";
$dateInput = trim(fgets(STDIN)); // Get input from the user

// Validate the date format (MMDDYY)
if (!preg_match('/^\d{2}\d{2}\d{2}$/', $dateInput)) {
    echo "Invalid date format. Please use MMDDYY.\n";
    exit; // Exit if the format is incorrect
}

// Format the output filename
$outputFile = 'remittz' . $dateInput . ".edi";

// Check if the output directory exists, otherwise create it
if (!file_exists($archiveDirectoryPath)) {
    mkdir($archiveDirectoryPath, 0777, true);  // Create archive directory if it doesn't exist
}

// Open the output file in write mode (create if not exists)
$outputHandle = fopen($outputFile, 'w');

// Check if the directory is valid
    // Loop through the files in the directory
$dir = opendir('.');
while (false !== ($file = readdir($dir))) {
    // Check if the file ends with .RMT extension
    if (pathinfo($file, PATHINFO_EXTENSION) === 'RMT') {
        $filePath = $directoryPath . $file;

        // Read the content of the RMT file
        $fileContents = file_get_contents($filePath);

        // Concatenate the contents to the output file
        fwrite($outputHandle, $fileContents);

        // Move the processed file to the archive directory
        $archivePath = $archiveDirectoryPath . $file;
        if (rename($filePath, $archivePath)) {
            echo "File $file moved to archive successfully.\n";
        } else {
            echo "Failed to move $file to archive.\n";
        }
    }
}


// Close the output file handle
fclose($outputHandle);

echo "Files concatenated successfully to '$outputFile'.\n";
