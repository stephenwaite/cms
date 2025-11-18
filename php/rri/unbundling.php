<?php

/* @package cms
* @link    http://www.cmsvt.com
* @author  s waite <stephen.waite@cmsvt.com>
* @copyright Copyright (c) 2024 cms <stephen.waite@cmsvt.com>
* @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
*/

$savedGarno = '';
$garnosWithStudies = [];
if ($file = fopen($argv[1], "r")) {
    while (($line = fgets($file)) !== false) {
        $garno = substr($line, 83, 8);
        $code = substr($line, 37, 5);
        $ins = substr($line, 29, 3);
      //echo $garno . "\n";
      //echo $code . "\n";
        if (empty($garnosWithStudies[$garno])) {
            $garnosWithStudies[$garno] = [];
            $garnosWithStudies[$garno]['line'] = $line;
            $garnosWithStudies[$garno]['ins'] = $ins;
        }
        array_push($garnosWithStudies[$garno], $code);
        $savedGarno = $garno;
    }
}

//var_dump($garnosWithStudies);
//exit;

foreach ($garnosWithStudies as $key => $value) {
    if ((count($garnosWithStudies[$key]) != 1) && in_array('70450', $value) && in_array('70496', $value)) {
        //echo $key . "\n";
        echo substr($garnosWithStudies[$key]['line'], 0, 32) . ' ' . $key . "\n";
    }
}
