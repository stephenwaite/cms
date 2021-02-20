<?php

require __DIR__ . '/vendor/autoload.php';

use Egulias\EmailValidator\EmailValidator;
use Egulias\EmailValidator\Validation\RFCValidation;

$validator = new EmailValidator();

$handle_sid = fopen("wsteve", "r");
//$handle_ste = fopen("wste", "w");
$row = array();
$i=0;
while (($line_sid = fgets($handle_sid)) !== false) {
  $row[$i]['key'] = substr($line_sid, 0, 6);
  $row[$i]['mrn'] = substr($line_sid, 6, 8);
  $row[$i]['name'] = substr($line_sid, 14, 24);
  $row[$i]['email'] = substr($line_sid, 38, 30);
  $row[$i]['auth'] = substr($line_sid, 68, 20);
  $row[$i]['date'] = substr($line_sid, 88, 8);
  $row[$i]['date'] = substr($line_sid, 96, 9);

  $i++;
}

foreach ($row as $line){
  var_dump($line);
  if ($validator->isValid(trim($line['email']), new RFCValidation())) {
    echo $line['email'] . " is valid \n";
  } else {
    echo $line['email'] . " is not valid \n";
  }

//fwrite($handle_ste, $value . "\n");
}



