<?php

require __DIR__ . '/vendor/autoload.php';

use League\Csv\Reader;

$reader = Reader::createFromPath('/tmp/test.csv', 'r');
$reader->setHeaderOffset(0);

$records = $reader->getRecords();
$prev_npi = '';

foreach ($records as $offset => $record) {    
        $prov_id = $record["VT Prov ID"];
        $prov_nm = $record["Provider Name"];
        $provTaxo = $record["Taxonomy"];
        $prov_npi = $record["NPI"];
        $request = "https://npiregistry.cms.hhs.gov/api/?version=2.1&number=" . $prov_npi . "&enumeration_type=NPI-1";
        $response = file_get_contents($request);
        $jsonobj = json_decode($response, true);
        
        if ($jsonobj['result_count'] != 0 && $prov_npi != $prev_npi) {          
          $prov_nm = preg_replace('/[^A-Z, ]/', '', $prov_nm);
          $prov_pieces = explode(",", $prov_nm);
          $prov_nm = preg_replace('/[^A-Z, ]/', '', $prov_nm);
          $prov_nm = str_replace(',', ';', $prov_nm);
          $prov_pieces = explode(";", $prov_nm);
          $prov_sid = $prov_pieces[0] . ";" . substr($prov_pieces[1], 1);         
          $prov_nm = str_pad($prov_sid, 24);
          echo $prov_id . $prov_nm . $prov_npi . $provTaxo . "\n";
          $prev_npi = $prov_npi;
        }
}


