<?php

$lines = file('/home/stee/Downloads/hosprri');

$load = [];
$visitno = '';
foreach($lines as $line) {
    if (substr($line, 0, 2) == '##') {
        $visitno = substr($line, 2, 7);
        //echo "$line\n";
        //echo "visitno $visitno \n";
    }

    if (substr($line, 0, 2) == '++') {
        $mrn = (substr($line, 849, 8));
        //echo "key is $mrn\n";
        if (array_key_exists($mrn, $load)) {
            //echo "$mrn already in load \n";
            if ($load[$mrn] != $visitno) {
                echo "*** that's odd, visitno $visitno is different from prior record in this file for mrn $mrn\n";
            }
        }
        $load[$mrn] = $visitno;
        
    }
    
}
//var_dump($load);