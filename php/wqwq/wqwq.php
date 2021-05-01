<?php
include "../../vendor/autoload.php";

$pdf = new Cezpdf('LETTER');
$pdf->ezSetMargins(170, 0, 10, 0);
$pdf->selectFont('Courier');
$page_count = 0;
$continued = false;
$is_continued = false;
$was_continued = false;
$body_count = 0;
$old_body = '';

$content = file_get_contents($argv[1]);
$pages = explode("\014", $content); // form feeds separate pages
foreach ($pages as $page) {
    $body_count = 0;    
    
    $page_lines = explode("\012", $page);
    $page_lines_count = count($page_lines);

    $was_continued = $is_continued;

    if (!strpos($page, "CONTINUED")) {         
        $is_continued = false;
    } else {        
        $is_continued = true;
    }

    $header = '';
    for ($i = 0; $i < 5; $i++) {
        $header .= $page_lines[$i];
    }

    $body = '';
    for ($i = 5; $i < ($page_lines_count - 4); $i++) {        
        $body .= $page_lines[$i];
        $body_count++;
    }

    $footer = '';
    for ($i = ($page_lines_count - 3); $i < $page_lines_count; $i++) {
        if ($page_lines[$i] == '') {
            $footer .= $page_lines[$i] . "\r";
        }
        $footer .= $page_lines[$i];
    }    

    if (!$is_continued && !$was_continued) {
        printHeader($header, $pdf);
        printBody($body, $pdf);
        printFooter($footer, $pdf);
        $total_body_count = 0;
    }

    if ($is_continued && !$was_continued) {
        $old_body .= $body;
        $total_body_count += $body_count;

    }

    if (!$is_continued && $was_continued) {
        $total_body_count += $body_count;
        if ($total_body_count < 35) {
            $old_body .= $body;
            printHeader($header, $pdf);
            printBody($old_body, $pdf);
            printFooter($footer, $pdf);
            $old_body = '';
            $total_body_count = 0;
        } else {
            printHeader($header, $pdf);
            printBody($old_body, $pdf);
            printFooter($footer, $pdf);
            printHeader($header, $pdf);
            $body = "\r" . $body ;
            printBody($body, $pdf);
            printFooter($footer, $pdf);
            $old_body = '';
            $total_body_count = 0;
        }    
    }

    if ($is_continued && $was_continued) {
        $total_body_count += $body_count;
        if ($total_body_count < 41) {
            $old_body .= $body;
        } else {
            printHeader($header, $pdf);
            printBody($old_body, $pdf);
            printFooter($footer, $pdf);
            $old_body = "\r" . $body ;
            $total_body_count = $body_count;
        }
    }
}

function printHeader($header, $pdf) {
    global $argv;
    $pdf->ezNewPage();        
    $pdf->ezSetY($pdf->ez['pageHeight'] - $pdf->ez['topMargin']);
    $pdf->addPngFromFile($argv[2], 0, 0, 612, 792);
    $pdf->ezText($header, 12, array(
        'justification' => 'left',
        'leading' => 12
    ));
}

function printBody($content, $pdf) {
    $pdf->ezSetY($pdf->ez['pageHeight'] - $pdf->ez['topMargin'] - 130);
    $pdf->ezText($content, 12, array(
        'justification' => 'left',
        'leading' => 12
    ));
}

function printFooter($footer, $pdf) { 
    $pdf->ezSetY($pdf->ez['pageHeight'] - $pdf->ez['topMargin'] - 560);
    $pdf->ezText($footer, 12, array(
        'justification' => 'left',
        'leading' => 12
    ));
}

$fname = tempnam('/tmp', 'PDF');
file_put_contents($fname, $pdf->ezOutput());
// Send the content for view.
header("Pragma: public");
header("Expires: 0");
header("Cache-Control: must-revalidate, post-check=0, pre-check=0");
header('Content-type: application/pdf');
header('Content-Disposition: inline; filename="new_wqwq"');
header('Content-Transfer-Encoding: binary');
header('Content-Length: ' . filesize($fname));
ob_end_clean();
@readfile($fname);
unlink($fname);
exit();
