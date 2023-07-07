<?php

require __DIR__ . '/../vendor/autoload.php';

// Parse PDF file and build necessary objects.
$parser = new \Smalot\PdfParser\Parser();
$pdf = $parser->parseFile('/home/stee/Downloads/test.pdf');

$text = $pdf->getText();
unlink("/home/stee/Downloads/text.txt");
$file = fopen("/home/stee/Downloads/text.txt","a");


foreach(preg_split("/((\r?\n)|(\r\n?))/", $text) as $line){
    // do stuff with $line
    if (strpos($line, '(') !== false) {
        continue;
    }
    $words = preg_split('/\s+/', $line);
    $word_count = count($words);
    
    if (strpos($words[0], '.') !== false) {
        //var_dump($words);
        echo "word count " . $word_count . "\n";
        $cntr = 0;
        foreach($words as $word) {
            var_dump($word);

        }
        //echo "ari comm is " . $words[0];
        $ari_comm = $words[0] ?? '';
        //echo "ari paid is " . $words[1];
        $ari_paid = $words[1] ?? '';
        //echo "bal is " . $words[3];
        $bal = $words[3] ?? '';
        //echo "paid agency " . $words[4];
        $pmt_amount = $words[4] ?? '';
        //echo "name is " . $words[5] . $words[6];
        $name = '"' . ($words[5] ?? '') . ($words[6] ?? '') . '"';

        if (strpos($words[7] ?? '', '.') !== false) {
            //echo "paid date is " . $words[7];
            //echo "garno is " . $words[8];
            $garno = $words[9] ?? '';
            $status = $words[10] ?? '';
            $due_client = $words[11] ?? '';

        } else {
            //echo "paid date is " . $words[7];
            //echo "garno is " . $words[8];
            $garno = $words[8] ?? '';
            $status = $words[9] ?? '';
            $due_client = $words[10] ?? '';
        }
        $paid_date = '28-Apr';

        $csv = $paid_date . "," . $name . "," . $garno . "," .
          $pmt_amount . "," . $status . "," . "T" . ",". $bal . "," . $ari_comm . "," .
          $due_client . "\n";
        //exit;
        fwrite($file, $csv);
    }
    echo "\n";
} 
fclose($file);
