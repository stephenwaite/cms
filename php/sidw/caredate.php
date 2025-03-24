<?php
$mystring = file($argv[1]);
$findme = 'TRN';
$pos = strpos($mystring[0], $findme);
$strDate = trim(substr($mystring[0], ($pos - 9), 8) . "\n");
$dateTime = DateTimeImmutable::createFromFormat("Ymd", $strDate);
echo $dateTime->format('mdy') . "\n";

