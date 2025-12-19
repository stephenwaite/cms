#!/usr/bin/env php
<?php
/**
 * HIPAA 999 Implementation Acknowledgment Error Checker
 *
 * Parses 999 transaction files and reports any rejections or errors found.
 * The 999 acknowledges receipt and reports on the syntactical correctness
 * of the received functional group.
 *
 * Usage:
 *   php check_999_errors.php <999_file_or_directory>
 *   php check_999_errors.php <directory> --verbose
 *   php check_999_errors.php <directory> --json
 *   php check_999_errors.php <directory> --summary
 *   php check_999_errors.php <directory> --errors-only
 *   php check_999_errors.php <directory> -r  (recursive)
 */

// AK9 Functional Group Acknowledgment Codes
const AK9_CODES = [
    'A' => 'Accepted',
    'E' => 'Accepted, But Errors Were Noted',
    'M' => 'Rejected, Message Authentication Code (MAC) Failed',
    'P' => 'Partially Accepted',
    'R' => 'Rejected',
    'W' => 'Rejected, Assurance Failed Validity Tests',
    'X' => 'Rejected, Content After Decryption Could Not Be Analyzed',
];

// IK5 Transaction Set Acknowledgment Codes
const IK5_CODES = [
    'A' => 'Accepted',
    'E' => 'Accepted But Errors Were Noted',
    'M' => 'Rejected, Message Authentication Code (MAC) Failed',
    'R' => 'Rejected',
    'W' => 'Rejected, Assurance Failed Validity Tests',
    'X' => 'Rejected, Content After Decryption Could Not Be Analyzed',
];

// AK9/IK5 Error Codes (positions 2-6)
const ACKNOWLEDGMENT_ERROR_CODES = [
    '1' => 'Functional Group Not Supported',
    '2' => 'Functional Group Version Not Supported',
    '3' => 'Functional Group Trailer Missing',
    '4' => 'Group Control Number in Header and Trailer Do Not Agree',
    '5' => 'Number of Included Transaction Sets Does Not Match Actual Count',
    '6' => 'Group Control Number Violates Syntax',
    '7' => 'Transaction Set Not in Functional Group',
    '10' => 'Authentication Key Name Unknown',
    '11' => 'Encryption Key Name Unknown',
    '12' => 'Requested Service (Authentication or Encryption) Not Available',
    '13' => 'Unknown Security Recipient',
    '14' => 'Unknown Security Originator',
    '15' => 'Syntax Error in Decrypted Text',
    '16' => 'Security Not Supported',
    '17' => 'Incorrect Message Length (Encryption)',
    '18' => 'Message Authentication Code Failed',
    '19' => 'Transaction Set Not Supported',
    '23' => 'Transaction Set Trailer Missing',
    '24' => 'Transaction Set Control Number in Header and Trailer Do Not Match',
    '25' => 'Number of Included Segments Does Not Match Actual Count',
    '26' => 'Message Authentication Code Failed',
    '27' => 'Transaction Set Control Number Not Unique within Functional Group',
];

// IK3 Segment Syntax Error Codes
const IK3_SEGMENT_ERROR_CODES = [
    '1' => 'Unrecognized segment ID',
    '2' => 'Unexpected segment',
    '3' => 'Required segment missing',
    '4' => 'Loop Occurs Over Maximum Times',
    '5' => 'Segment Exceeds Maximum Use',
    '6' => 'Segment Not in Defined Transaction Set',
    '7' => 'Segment Not in Proper Sequence',
    '8' => 'Segment Has Data Element Errors',
    'I4' => 'Implementation "Not Used" Segment Present',
    'I6' => 'Implementation Dependent Segment Missing',
    'I7' => 'Implementation Loop Occurs Under Minimum Times',
    'I8' => 'Implementation Segment Below Minimum Use',
    'I9' => 'Implementation Dependent "Not Used" Segment Present',
];

// IK4 Element Syntax Error Codes
const IK4_ELEMENT_ERROR_CODES = [
    '1' => 'Required Data Element Missing',
    '2' => 'Conditional Required Data Element Missing',
    '3' => 'Too Many Data Elements',
    '4' => 'Data Element Too Short',
    '5' => 'Data Element Too Long',
    '6' => 'Invalid Character in Data Element',
    '7' => 'Invalid Code Value',
    '8' => 'Invalid Date',
    '9' => 'Invalid Time',
    '10' => 'Exclusion Condition Violated',
    '12' => 'Too Many Repetitions',
    '13' => 'Too Many Components',
    'I10' => 'Implementation "Not Used" Data Element Present',
    'I11' => 'Implementation Too Few Repetitions',
    'I12' => 'Implementation Pattern Match Failure',
    'I13' => 'Implementation Dependent "Not Used" Data Element Present',
    'I6' => 'Code Value Not Used in Implementation',
    'I9' => 'Implementation Dependent Data Element Missing',
];


class ElementError
{
    public string $elementPosition = '';
    public ?string $componentPosition = null;
    public ?string $repeatPosition = null;
    public ?string $elementReference = null;
    public string $errorCode = '';
    public string $errorDescription = '';
    public ?string $copyOfBadElement = null;

    public function toArray(): array
    {
        return [
            'element_position' => $this->elementPosition,
            'component_position' => $this->componentPosition,
            'repeat_position' => $this->repeatPosition,
            'element_reference' => $this->elementReference,
            'error_code' => $this->errorCode,
            'error_description' => $this->errorDescription,
            'copy_of_bad_element' => $this->copyOfBadElement,
        ];
    }
}

class SegmentError
{
    public string $segmentId = '';
    public string $segmentPosition = '';
    public ?string $loopId = null;
    public ?string $errorCode = null;
    public ?string $errorDescription = null;
    /** @var ElementError[] */
    public array $elementErrors = [];

    public function toArray(): array
    {
        return [
            'segment_id' => $this->segmentId,
            'segment_position' => $this->segmentPosition,
            'loop_id' => $this->loopId,
            'error_code' => $this->errorCode,
            'error_description' => $this->errorDescription,
            'element_errors' => array_map(fn($e) => $e->toArray(), $this->elementErrors),
        ];
    }
}

class TransactionSetResponse
{
    public string $transactionSetId = '';
    public string $controlNumber = '';
    public ?string $implementationConvention = null;
    public string $acknowledgmentCode = '';
    public string $acknowledgmentDescription = '';
    public array $errorCodes = [];
    /** @var SegmentError[] */
    public array $segmentErrors = [];

    public function hasErrors(): bool
    {
        return in_array($this->acknowledgmentCode, ['E', 'R', 'M', 'W', 'X']);
    }

    public function isRejected(): bool
    {
        return in_array($this->acknowledgmentCode, ['R', 'M', 'W', 'X']);
    }

    public function toArray(): array
    {
        return [
            'transaction_set_id' => $this->transactionSetId,
            'control_number' => $this->controlNumber,
            'implementation_convention' => $this->implementationConvention,
            'acknowledgment_code' => $this->acknowledgmentCode,
            'acknowledgment_description' => $this->acknowledgmentDescription,
            'has_errors' => $this->hasErrors(),
            'is_rejected' => $this->isRejected(),
            'error_codes' => $this->errorCodes,
            'segment_errors' => array_map(fn($s) => $s->toArray(), $this->segmentErrors),
        ];
    }
}

class FunctionalGroupResponse
{
    public string $groupControlNumber = '';
    public ?string $transactionSetId = null;
    public string $acknowledgmentCode = '';
    public string $acknowledgmentDescription = '';
    public int $includedTsCount = 0;
    public int $receivedTsCount = 0;
    public int $acceptedTsCount = 0;
    public array $errorCodes = [];
    /** @var TransactionSetResponse[] */
    public array $transactionSets = [];

    public function hasErrors(): bool
    {
        return in_array($this->acknowledgmentCode, ['E', 'R', 'M', 'P', 'W', 'X']);
    }

    public function isRejected(): bool
    {
        return in_array($this->acknowledgmentCode, ['R', 'M', 'W', 'X']);
    }

    public function toArray(): array
    {
        return [
            'group_control_number' => $this->groupControlNumber,
            'transaction_set_id' => $this->transactionSetId,
            'acknowledgment_code' => $this->acknowledgmentCode,
            'acknowledgment_description' => $this->acknowledgmentDescription,
            'has_errors' => $this->hasErrors(),
            'is_rejected' => $this->isRejected(),
            'included_ts_count' => $this->includedTsCount,
            'received_ts_count' => $this->receivedTsCount,
            'accepted_ts_count' => $this->acceptedTsCount,
            'error_codes' => $this->errorCodes,
            'transaction_sets' => array_map(fn($t) => $t->toArray(), $this->transactionSets),
        ];
    }
}

class ParseResult
{
    public string $filename = '';
    public ?string $interchangeSender = null;
    public ?string $interchangeReceiver = null;
    public ?string $interchangeControlNumber = null;
    public ?string $interchangeDate = null;
    /** @var FunctionalGroupResponse[] */
    public array $functionalGroups = [];
    public array $parseErrors = [];

    public function hasAnyErrors(): bool
    {
        foreach ($this->functionalGroups as $fg) {
            if ($fg->hasErrors()) {
                return true;
            }
        }
        return false;
    }

    public function hasAnyRejections(): bool
    {
        foreach ($this->functionalGroups as $fg) {
            if ($fg->isRejected()) {
                return true;
            }
            foreach ($fg->transactionSets as $ts) {
                if ($ts->isRejected()) {
                    return true;
                }
            }
        }
        return false;
    }

    public function getTotalRejectedTransactions(): int
    {
        $count = 0;
        foreach ($this->functionalGroups as $fg) {
            foreach ($fg->transactionSets as $ts) {
                if ($ts->isRejected()) {
                    $count++;
                }
            }
        }
        return $count;
    }

    public function getTotalTransactions(): int
    {
        $count = 0;
        foreach ($this->functionalGroups as $fg) {
            $count += count($fg->transactionSets);
        }
        return $count;
    }

    public function toArray(): array
    {
        return [
            'filename' => $this->filename,
            'interchange_sender' => $this->interchangeSender,
            'interchange_receiver' => $this->interchangeReceiver,
            'interchange_control_number' => $this->interchangeControlNumber,
            'interchange_date' => $this->interchangeDate,
            'has_any_errors' => $this->hasAnyErrors(),
            'has_any_rejections' => $this->hasAnyRejections(),
            'total_transactions' => $this->getTotalTransactions(),
            'total_rejected_transactions' => $this->getTotalRejectedTransactions(),
            'functional_groups' => array_map(fn($fg) => $fg->toArray(), $this->functionalGroups),
            'parse_errors' => $this->parseErrors,
        ];
    }
}

class BatchResult
{
    /** @var ParseResult[] */
    public array $results = [];
    public string $directory = '';
    public int $totalFiles = 0;
    public int $acceptedFiles = 0;
    public int $errorFiles = 0;
    public int $rejectedFiles = 0;
    public int $totalTransactions = 0;
    public int $rejectedTransactions = 0;

    public function addResult(ParseResult $result): void
    {
        $this->results[] = $result;
        $this->totalFiles++;
        $this->totalTransactions += $result->getTotalTransactions();
        $this->rejectedTransactions += $result->getTotalRejectedTransactions();

        if ($result->hasAnyRejections()) {
            $this->rejectedFiles++;
        } elseif ($result->hasAnyErrors()) {
            $this->errorFiles++;
        } else {
            $this->acceptedFiles++;
        }
    }

    public function hasAnyProblems(): bool
    {
        return $this->errorFiles > 0 || $this->rejectedFiles > 0;
    }

    public function toArray(): array
    {
        return [
            'directory' => $this->directory,
            'summary' => [
                'total_files' => $this->totalFiles,
                'accepted_files' => $this->acceptedFiles,
                'error_files' => $this->errorFiles,
                'rejected_files' => $this->rejectedFiles,
                'total_transactions' => $this->totalTransactions,
                'rejected_transactions' => $this->rejectedTransactions,
            ],
            'has_any_problems' => $this->hasAnyProblems(),
            'files' => array_map(fn($r) => $r->toArray(), $this->results),
        ];
    }
}

// ============================================================================
// PARSING FUNCTIONS
// ============================================================================

function detectDelimiters(string $content): array
{
    if (substr($content, 0, 3) !== 'ISA') {
        throw new Exception("File does not start with ISA segment");
    }

    $elementDelim = $content[3];
    $subelementDelim = $content[104];
    $segmentDelim = $content[105];

    if ($segmentDelim === "\r" && isset($content[106]) && $content[106] === "\n") {
        $segmentDelim = "\r\n";
    }

    return [$elementDelim, $subelementDelim, $segmentDelim];
}

function parseSegment(string $segment, string $elementDelim, string $subelementDelim): array
{
    $elements = explode($elementDelim, $segment);
    $result = [];

    foreach ($elements as $element) {
        if (strpos($element, $subelementDelim) !== false) {
            $result[] = explode($subelementDelim, $element);
        } else {
            $result[] = $element;
        }
    }

    return $result;
}

function parse999(string $filepath): ParseResult
{
    $result = new ParseResult();
    $result->filename = basename($filepath);

    if (!file_exists($filepath)) {
        $result->parseErrors[] = "File not found: $filepath";
        return $result;
    }

    $content = file_get_contents($filepath);
    if ($content === false) {
        $result->parseErrors[] = "Unable to read file: $filepath";
        return $result;
    }

    $content = preg_replace('/^\xEF\xBB\xBF/', '', $content);
    $content = trim($content);

    try {
        [$elementDelim, $subelementDelim, $segmentDelim] = detectDelimiters($content);
    } catch (Exception $e) {
        $result->parseErrors[] = "Failed to detect delimiters: " . $e->getMessage();
        return $result;
    }

    $segments = explode($segmentDelim, $content);
    $segments = array_filter(array_map('trim', $segments));

    $currentGroup = null;
    $currentTransaction = null;
    $currentSegmentError = null;

    foreach ($segments as $segStr) {
        $seg = parseSegment($segStr, $elementDelim, $subelementDelim);
        $segId = $seg[0] ?? '';

        switch ($segId) {
            case 'ISA':
                $result->interchangeSender = isset($seg[6]) ? trim($seg[6]) : null;
                $result->interchangeReceiver = isset($seg[8]) ? trim($seg[8]) : null;
                $result->interchangeDate = $seg[9] ?? null;
                $result->interchangeControlNumber = $seg[13] ?? null;
                break;

            case 'AK1':
                $currentGroup = new FunctionalGroupResponse();
                $currentGroup->transactionSetId = $seg[1] ?? '';
                $currentGroup->groupControlNumber = $seg[2] ?? '';
                $result->functionalGroups[] = $currentGroup;
                break;

            case 'AK2':
                if ($currentGroup) {
                    $currentTransaction = new TransactionSetResponse();
                    $currentTransaction->transactionSetId = $seg[1] ?? '';
                    $currentTransaction->controlNumber = $seg[2] ?? '';
                    $currentTransaction->implementationConvention = $seg[3] ?? null;
                    $currentGroup->transactionSets[] = $currentTransaction;
                }
                break;

            case 'IK3':
                if ($currentTransaction) {
                    $currentSegmentError = new SegmentError();
                    $currentSegmentError->segmentId = $seg[1] ?? '';
                    $currentSegmentError->segmentPosition = $seg[2] ?? '';
                    $currentSegmentError->loopId = $seg[3] ?? null;
                    $currentSegmentError->errorCode = $seg[4] ?? null;

                    if ($currentSegmentError->errorCode) {
                        $currentSegmentError->errorDescription =
                            IK3_SEGMENT_ERROR_CODES[$currentSegmentError->errorCode] ?? 'Unknown Error';
                    }

                    $currentTransaction->segmentErrors[] = $currentSegmentError;
                }
                break;

            case 'CTX':
                // Context segment - can expand parsing if needed
                break;

            case 'IK4':
                if ($currentSegmentError) {
                    $elementError = new ElementError();

                    $elemPos = $seg[1] ?? '';
                    if (is_array($elemPos)) {
                        $elementError->elementPosition = $elemPos[0] ?? '';
                        $elementError->componentPosition = $elemPos[1] ?? null;
                        $elementError->repeatPosition = $elemPos[2] ?? null;
                    } else {
                        $elementError->elementPosition = $elemPos;
                    }

                    $elementError->elementReference = $seg[2] ?? null;
                    $elementError->errorCode = $seg[3] ?? '';
                    $elementError->errorDescription =
                        IK4_ELEMENT_ERROR_CODES[$elementError->errorCode] ?? 'Unknown Error';
                    $elementError->copyOfBadElement = $seg[4] ?? null;

                    $currentSegmentError->elementErrors[] = $elementError;
                }
                break;

            case 'IK5':
                if ($currentTransaction && isset($seg[1])) {
                    $currentTransaction->acknowledgmentCode = $seg[1];
                    $currentTransaction->acknowledgmentDescription =
                        IK5_CODES[$seg[1]] ?? 'Unknown Status';

                    for ($i = 2; $i < min(7, count($seg)); $i++) {
                        if (!empty($seg[$i])) {
                            $currentTransaction->errorCodes[] = [
                                'code' => $seg[$i],
                                'description' => ACKNOWLEDGMENT_ERROR_CODES[$seg[$i]] ?? 'Unknown Error',
                            ];
                        }
                    }
                }
                $currentTransaction = null;
                $currentSegmentError = null;
                break;

            case 'AK9':
                if ($currentGroup && isset($seg[1])) {
                    $currentGroup->acknowledgmentCode = $seg[1];
                    $currentGroup->acknowledgmentDescription =
                        AK9_CODES[$seg[1]] ?? 'Unknown Status';

                    $currentGroup->includedTsCount = (int)($seg[2] ?? 0);
                    $currentGroup->receivedTsCount = (int)($seg[3] ?? 0);
                    $currentGroup->acceptedTsCount = (int)($seg[4] ?? 0);

                    for ($i = 5; $i < min(10, count($seg)); $i++) {
                        if (!empty($seg[$i])) {
                            $currentGroup->errorCodes[] = [
                                'code' => $seg[$i],
                                'description' => ACKNOWLEDGMENT_ERROR_CODES[$seg[$i]] ?? 'Unknown Error',
                            ];
                        }
                    }
                }
                $currentGroup = null;
                break;
        }
    }

    return $result;
}

// ============================================================================
// DIRECTORY PROCESSING
// ============================================================================

function find999Files(string $directory, bool $recursive = false): array
{
    $files = [];
    $extensions = ['edi', 'x12', '999', 'txt'];

    if ($recursive) {
        $iterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($directory, RecursiveDirectoryIterator::SKIP_DOTS)
        );
    } else {
        $iterator = new DirectoryIterator($directory);
    }

    foreach ($iterator as $file) {
        if ($file->isFile()) {
            $ext = strtolower($file->getExtension());
            $filename = strtolower($file->getFilename());

            if (in_array($ext, $extensions) || strpos($filename, '999') !== false) {
                $files[] = $file->getPathname();
            }
        }
    }

    sort($files);
    return $files;
}

function processDirectory(string $directory, bool $recursive = false): BatchResult
{
    $batch = new BatchResult();
    $batch->directory = realpath($directory) ?: $directory;

    $files = find999Files($directory, $recursive);

    foreach ($files as $filepath) {
        $result = parse999($filepath);
        $batch->addResult($result);
    }

    return $batch;
}

// ============================================================================
// OUTPUT FUNCTIONS
// ============================================================================

function printSummary(ParseResult $result): void
{
    echo str_repeat("=", 70) . "\n";
    echo "999 ACKNOWLEDGMENT: {$result->filename}\n";
    echo str_repeat("=", 70) . "\n";

    if (!empty($result->parseErrors)) {
        echo "\n⚠️  PARSE ERRORS:\n";
        foreach ($result->parseErrors as $err) {
            echo "   $err\n";
        }
    }

    echo "Interchange Control #: " . ($result->interchangeControlNumber ?? 'N/A') . "\n";
    echo "From: " . ($result->interchangeSender ?? 'N/A');
    echo " → To: " . ($result->interchangeReceiver ?? 'N/A') . "\n";
    echo "Date: " . ($result->interchangeDate ?? 'N/A') . "\n";

    if ($result->hasAnyRejections()) {
        echo "🔴 STATUS: REJECTED\n";
    } elseif ($result->hasAnyErrors()) {
        echo "🟡 STATUS: ACCEPTED WITH ERRORS\n";
    } else {
        echo "🟢 STATUS: ACCEPTED\n";
    }

    echo "Transactions: {$result->getTotalTransactions()}";
    if ($result->getTotalRejectedTransactions() > 0) {
        echo " ({$result->getTotalRejectedTransactions()} rejected)";
    }
    echo "\n";
}

function printDetailed(ParseResult $result): void
{
    foreach ($result->functionalGroups as $fgIdx => $fg) {
        echo "\n" . str_repeat("─", 70) . "\n";
        echo "FUNCTIONAL GROUP " . ($fgIdx + 1) . "\n";
        echo str_repeat("─", 70) . "\n";
        echo "  Group Control #: {$fg->groupControlNumber}\n";
        echo "  Transaction Set Type: {$fg->transactionSetId}\n";
        echo "  Status: {$fg->acknowledgmentCode} - {$fg->acknowledgmentDescription}\n";
        echo "  Included: {$fg->includedTsCount} | Received: {$fg->receivedTsCount} | Accepted: {$fg->acceptedTsCount}\n";

        if (!empty($fg->errorCodes)) {
            echo "\n  Group-Level Errors:\n";
            foreach ($fg->errorCodes as $err) {
                echo "    • [{$err['code']}] {$err['description']}\n";
            }
        }

        foreach ($fg->transactionSets as $tsIdx => $ts) {
            $statusIcon = $ts->isRejected() ? "🔴" : ($ts->hasErrors() ? "🟡" : "🟢");
            echo "\n  Transaction Set " . ($tsIdx + 1) . ": {$ts->transactionSetId}-{$ts->controlNumber}\n";
            echo "    $statusIcon {$ts->acknowledgmentCode} - {$ts->acknowledgmentDescription}\n";

            if (!empty($ts->errorCodes)) {
                echo "    Transaction-Level Errors:\n";
                foreach ($ts->errorCodes as $err) {
                    echo "      • [{$err['code']}] {$err['description']}\n";
                }
            }

            foreach ($ts->segmentErrors as $segErr) {
                echo "\n    Segment Error: {$segErr->segmentId} at position {$segErr->segmentPosition}\n";
                if ($segErr->loopId) {
                    echo "      Loop: {$segErr->loopId}\n";
                }
                if ($segErr->errorCode) {
                    echo "      [{$segErr->errorCode}] {$segErr->errorDescription}\n";
                }

                foreach ($segErr->elementErrors as $elemErr) {
                    $posStr = "Element {$elemErr->elementPosition}";
                    if ($elemErr->componentPosition) {
                        $posStr .= ", Component {$elemErr->componentPosition}";
                    }
                    if ($elemErr->repeatPosition) {
                        $posStr .= ", Repeat {$elemErr->repeatPosition}";
                    }

                    echo "      Element Error: $posStr\n";
                    if ($elemErr->elementReference) {
                        echo "        Reference: {$elemErr->elementReference}\n";
                    }
                    echo "        [{$elemErr->errorCode}] {$elemErr->errorDescription}\n";
                    if ($elemErr->copyOfBadElement) {
                        echo "        Bad Value: '{$elemErr->copyOfBadElement}'\n";
                    }
                }
            }
        }
    }
}

function printBatchSummary(BatchResult $batch): void
{
    echo "\n" . str_repeat("═", 70) . "\n";
    echo "DIRECTORY SUMMARY\n";
    echo str_repeat("═", 70) . "\n";
    echo "Directory: {$batch->directory}\n";
    echo "\nFiles Processed: {$batch->totalFiles}\n";
    echo "  🟢 Accepted:           {$batch->acceptedFiles}\n";
    echo "  🟡 Accepted w/Errors:  {$batch->errorFiles}\n";
    echo "  🔴 Rejected:           {$batch->rejectedFiles}\n";
    echo "\nTotal Transactions: {$batch->totalTransactions}\n";
    echo "Rejected Transactions: {$batch->rejectedTransactions}\n";

    if ($batch->totalFiles === 0) {
        echo "\n⚠️  No 999 files found.\n";
        echo "   (Searched for: .edi, .x12, .999, .txt or filenames containing '999')\n";
    } elseif ($batch->hasAnyProblems()) {
        echo "\n" . str_repeat("─", 70) . "\n";
        echo "FILES WITH ISSUES:\n";
        echo str_repeat("─", 70) . "\n";

        foreach ($batch->results as $result) {
            if ($result->hasAnyRejections()) {
                echo "  🔴 {$result->filename} - REJECTED";
                echo " ({$result->getTotalRejectedTransactions()}/{$result->getTotalTransactions()} transactions)\n";
            } elseif ($result->hasAnyErrors()) {
                echo "  🟡 {$result->filename} - ERRORS\n";
            }
        }
    } else {
        echo "\n✅ All files accepted without errors.\n";
    }
}

// ============================================================================
// MAIN
// ============================================================================

function main(array $argv): int
{
    $options = getopt('vjqhsreR', ['verbose', 'json', 'quiet', 'help', 'summary', 'recursive', 'errors-only'], $optind);
    $args = array_slice($argv, $optind);

    $verbose = array_key_exists('v', $options) || array_key_exists('verbose', $options);
    $json = array_key_exists('j', $options) || array_key_exists('json', $options);
    $quiet = array_key_exists('q', $options) || array_key_exists('quiet', $options);
    $help = array_key_exists('h', $options) || array_key_exists('help', $options);
    $summaryOnly = array_key_exists('s', $options) || array_key_exists('summary', $options);
    $recursive = array_key_exists('r', $options) || array_key_exists('R', $options) || array_key_exists('recursive', $options);
    $errorsOnly = array_key_exists('e', $options) || array_key_exists('errors-only', $options);

    if ($help || empty($args)) {
        echo "HIPAA 999 Implementation Acknowledgment Error Checker\n\n";
        echo "Usage: php check_999_errors.php [options] <file_or_directory>\n\n";
        echo "Options:\n";
        echo "  -v, --verbose      Show detailed error information for all files\n";
        echo "  -e, --errors-only  Show details only for files with errors\n";
        echo "  -s, --summary      Show only directory summary (skip individual files)\n";
        echo "  -r, -R, --recursive  Process subdirectories recursively\n";
        echo "  -j, --json         Output results as JSON\n";
        echo "  -q, --quiet        Only output if errors found\n";
        echo "  -h, --help         Show this help message\n\n";
        echo "File Detection:\n";
        echo "  Searches for: .edi, .x12, .999, .txt or filenames containing '999'\n\n";
        echo "Examples:\n";
        echo "  php check_999_errors.php file.edi              # Single file\n";
        echo "  php check_999_errors.php /path/to/999s/        # All 999s in directory\n";
        echo "  php check_999_errors.php -r /path/to/999s/     # Include subdirectories\n";
        echo "  php check_999_errors.php -e /path/to/999s/     # Only show errors\n";
        echo "  php check_999_errors.php -s /path/to/999s/     # Summary only\n";
        echo "  php check_999_errors.php --json /path/         # JSON output\n\n";
        echo "Exit codes:\n";
        echo "  0 - All accepted (no errors)\n";
        echo "  1 - Some accepted with errors\n";
        echo "  2 - Some rejected\n";
        return $help ? 0 : 1;
    }

    $path = $args[0];

    if (!file_exists($path)) {
        fwrite(STDERR, "Error: Path not found: $path\n");
        return 1;
    }

    // Directory mode
    if (is_dir($path)) {
        $batch = processDirectory($path, $recursive);

        if ($json) {
            echo json_encode($batch->toArray(), JSON_PRETTY_PRINT) . "\n";
            return $batch->rejectedFiles > 0 ? 2 : ($batch->errorFiles > 0 ? 1 : 0);
        }

        if ($quiet && !$batch->hasAnyProblems()) {
            return 0;
        }

        if (!$summaryOnly) {
            foreach ($batch->results as $result) {
                // Skip accepted files in errors-only mode
                if ($errorsOnly && !$result->hasAnyErrors() && !$result->hasAnyRejections()) {
                    continue;
                }

                printSummary($result);

                if ($verbose || $result->hasAnyErrors() || $result->hasAnyRejections()) {
                    printDetailed($result);
                }
                echo "\n";
            }
        }

        printBatchSummary($batch);

        return $batch->rejectedFiles > 0 ? 2 : ($batch->errorFiles > 0 ? 1 : 0);
    }

    // Single file mode
    $result = parse999($path);

    if ($json) {
        echo json_encode($result->toArray(), JSON_PRETTY_PRINT) . "\n";
    } elseif ($quiet) {
        if ($result->hasAnyErrors() || $result->hasAnyRejections()) {
            printSummary($result);
            if ($verbose) {
                printDetailed($result);
            }
        }
    } else {
        printSummary($result);
        if ($verbose || $result->hasAnyErrors()) {
            printDetailed($result);
        }
    }

    if ($result->hasAnyRejections()) {
        return 2;
    } elseif ($result->hasAnyErrors()) {
        return 1;
    }
    return 0;
}

exit(main($argv));
