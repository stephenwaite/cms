<?php
class CmsFeeSchedItem {
    public string $note = '';
    public string $code = '';
    public float $allow = 0;
    public float $nonParAmount = 0;
    public float $limitingCharge = 0;
    public string $effectiveDate = '';
}

class CmsFeeSched {
    public function set(string $key, $value) {
        $this->value[$key] = $value;
    }
    public function get(string $key) {
        return $this->value[$key] ?? 0;
    }
}

class RriProcItem {
    public function __construct(
        public string $cdm,
        public string $cpt,
        public string $mod,
        public string $type = '',
        public string $desc = '',
        public int $fee)
    {}

    public function setFee(string $key, $value) {
        $this->fee[$key] = $value;
    }
    
}

class CmsFeeSchedArray extends \ArrayObject {
    public CmsFeeSchedItem $item;
}

$handle = fopen("/home/stee/Downloads/fee2024", "r");
$cms_cpt_array = array();
while ($line = fgetcsv($handle)) {
    if ($line[0] == 'C') {
        continue;
    }
    $CmsFeeSchedItem = new CmsFeeSchedItem;
    $CmsFeeSchedItem->code = substr($line[1], 0, 5) ?? '';

    if ($line[2] == 'TC') {
        continue;
    }
    $CmsFeeSchedItem->mod = $line[2];
    $CmsFeeSchedItem->allow = $line[3];
    $cms_cpt_array[] = $CmsFeeSchedItem;
}

$file = fopen("/home/stee/Downloads/wsid", "r");
$fees = array();
while (!feof($file)) {
    $line = fgets($file);
    $RriProcItem = new $RriProcItem;
    $RriProcItem->cdm = substr($line, 0, 4);
    $RriProcItem->cpt = substr($line, 4, 5);
    $RriProcItem->mod = substr($line, 9, 2);
    $RriProcItem->type = substr($line, 11, 1);
    $RriProcItem->desc = str_replace(",", " ", substr($line, 12, 28));
    $RriProcItem->fee = (int) substr($line, 40, 10);
    $RriProcItem->allow = $cms_cpt_array

    //if (array_key_exists($cpt, $fees)) {
//      echo "we have another $cpt \n";
    //  if ($fees[$cpt]['fee'] != $fee) {
        //echo "uh oh, fees not equal for $cpt " .
        //  $fees[$cpt]['fee'] . " $fee \n";
    //  }

    //}
    //$fees[$cpt]['cdm'] = $cdm;
    if (!empty($RriProcItem->fee) && !empty($RriProcItem->cpt)) {
        $fees[] = $RriProcItem;
        
    }
}
/* $fees[$cpt]['mod'] = $mod;
        $fees[$cpt]['desc'] = $desc;
        // update 2025 fees by 3%
        $fees[$cpt]['fee'] = round($fee * 1.03 / 100, 0);

        //var_dump($cpt);
        //echo "fee " . $fee . " cpt " . $cpt . "\n";
        $fees[$cpt]['allow'] = $cms_cpt_array[$cpt]['allow'] ?? '';
        if (array_key_exists($cpt, $cms_cpt_array)) {
            $fees[$cpt]['ratio'] = round($fees[$cpt]['fee'] / $cms_cpt_array[$cpt]['allow'], 2);
        }
    if (!empty($fees[$cpt])) {
        $cms_allow = $fees[$cpt]['allow'];
    }
    $ratio = $fees[$cpt]['ratio'] ?? ''; */
    /* if (!empty($fees[$cpt]) && !empty($ratio)) {
        if ($ratio > 10) {
            echo "ratio $ratio greater than 10, fee " . $fee . " cpt " . $cpt . " cms allow " . $cms_allow;
            $fee =  round(10 * $cms_allow);
            echo " suggest new fee of " . $fee . "\n";
        }

        if ($ratio < 5) {
            echo "ratio $ratio less than 5, fee " . $fee . " cpt " . $cpt . " cms allow " . $cms_allow;
            $fee = 100 * round(5 * $cms_allow);
            echo " suggest new fee of " . $fee . "\n";
        }

        //if ($fees[$cpt]['cpt'] == '77067') {
        //    $fee = 217;
        //}
    } */

    //echo $cdm . $cpt . $mod . $type .
    //    sprintf("%-28s", $desc) . sprintf("%06s",$fee, ' '). "\n";


//    if (empty($fees[$cpt]['fee'])) {
//      $fees[$cpt]['fee'] = $fee;
//      //echo $cpt . " " . $fee . "\n";
//    } elseif ($fees[$cpt]['fee'] != $fee) {
//      echo "uh oh, fees not equal for $cpt \n";
//    }


//var_dump($fees);
//echo count($fees);
//exit;
//echo count($fees);

//ksort($fees);

$fileout = fopen('2024_ratio', 'w');
foreach ($fees as $key => $value) {
    $ratio = $value['ratio'] ?? '';
    //echo $key . " " . $value . "\n";
    //var_dump($value);
    //exit;
    $string = $key . "," . $value['mod'] . "," . $value['desc'] . "," . $value['fee'] . "," .
        $value['allow'] . "," . ($value['ratio'] ?? '') . "\n";
    fwrite($fileout, $string);

    if (!empty($value['fee']) && !empty($value['allow'])) {
        if ($ratio > 10) {
            //echo "ratio " . $ratio . " greater than 10, fee " . $value['fee'] . " cpt " . $value['cpt'] . " cms allow " . $value['allow'];
            $fee =  round(10 * floatval($value['allow'] ?? ''));
            echo ( " suggest 2025 fee of " . round($fee * 1.03)) " instead of ". "\n";
        }

        if ($ratio < 5) {
            echo "ratio " . $ratio  . " less than 5, fee " . $value['fee'] . " cpt " . $value['cpt'] . " cms allow " . $value['allow'];
            $fee = 100 * round(5 * floatval($value['allow'] ?? ''));
            echo " suggest new fee of " . $fee . "\n";
        }

    }
}

exit;
