<?php
/**
 * Created by PhpStorm.
 * User: stee
 * Date: 2/25/20
 * Time: 1:37 PM
 */

$handle_dat = fopen("mea358", "r");
$handle_sid = fopen("w1", "r");
$handle_ste = fopen("wste", "w");
$mea = array();

while (($line_sid = fgets($handle_sid)) !== false) {
    $proc[]     = substr($line_sid, 0, 5);
    $proc_title[] = substr($line_sid, 19, 28);
}

while (($line_dat = fgets($handle_dat)) !== false) {
    $mea[] = explode(',', $line_dat);
    //$new_array = array_keys($mea);
    //var_dump($mea);
    //var_dump($new_array);
    //exit;
    foreach ($mea as $item){
        foreach ($item as $value) {
            $value = trim(preg_replace('/[\x00-\x1F\x7F]/u', '', $value));
            if ($value !== '' && in_array($value, $proc)) {
                fwrite($handle_ste, $value . "\n");
            } else {
                //var_dump($new_array[$item[0]][0]);
            }        }
        $value = array();
    }
    $mea = array();
}



