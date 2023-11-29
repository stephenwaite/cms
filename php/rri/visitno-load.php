<?php

$lines = file($argv[1]);

$load = [];
$visitno = '';
foreach($lines as $line) {
    if (substr($line, 0, 2) == '##') {
        $visitno = substr($line, 2, 7);
        //echo "$line\n";
        //echo "visitno $visitno \n";
        $rrmc_ins_code = trim(substr($line, 295, 5));
        //echo "rrmc ins code $rrmc_ins_code\n";
        //exit;
    }

    if (substr($line, 0, 2) == '++') {
        $mrn = (substr($line, 849, 8));
        //echo "key is $mrn\n";
        if (array_key_exists($mrn, $load)) {
            //echo "$mrn already in load \n";
            if ($load[$mrn]['visitno'] != $visitno) {
                if ($load[$mrn]['rrmc_ins_code'] != $rrmc_ins_code) {
                    echo "*** uh oh, rrmc ins code $rrmc_ins_code is different from prior record ins code " . $load[$mrn]['rrmc_ins_code'] . " in this file for mrn $mrn\n";
                    echo "*** which means we were given different insurances for distinct charges for the same person and should be remedied.\n";
                }
            }
        }
        $load[$mrn]['visitno'] = $visitno;
        $load[$mrn]['rrmc_ins_code'] = $rrmc_ins_code;
        
    }
    
}
//var_dump($load);