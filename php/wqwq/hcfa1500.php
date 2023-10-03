<?php
$USER = getenv("USER");
$filepath = "/home/" . $USER;
include $filepath . "/src/cms/vendor/autoload.php";

$pdf = new Cezpdf('LETTER');
$pdf->ezSetMargins(0, 0, 25, 0);
$pdf->ez['topMargin'] = '32';
$pdf->selectFont('Courier');
$page_count = 0;
$continued = false;
$is_continued = false;
$was_continued = false;
$line_count = 0;
$old_body = '';
$total_line_count = 0;
$hcfa_image = "/home/stee/src/cms/php/wqwq/cms1500.png";


$content = file_get_contents($argv[1]);
//var_dump($content);
//exit;

$pages = explode("\014", $content); // form feeds separate pages
foreach ($pages as $page) {
    //echo $page;
    $pdf->ezSetY($pdf->ez['pageHeight'] - $pdf->ez['topMargin']);
    $pdf->addPngFromFile("$hcfa_image", 0, 0, 612, 792);
    $pdf->ezText($page, 12, array(
        'justification' => 'left',
        'leading' => 12
    ));
}

$fname = './wsid.pdf';
file_put_contents($fname, $pdf->ezOutput());
//$command = "cp $fname ./bill.pdf";
//exec($command);

exit();

