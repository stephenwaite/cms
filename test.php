<?php

require_once('vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

$file = file_get_contents(getenv('HOME') . "/W2" . getenv('tid') . getenv('USER'));
//echo (getenv('HOME') . "/W2" . getenv('tid') . getenv('USER'));
//exit;
$mrn = ltrim(substr($file, 0, 8), '0');
$visit_no = substr($file, 8, 7);
$charcur_key = substr($file, 15, 11);
// 000121359669901BUC0891G016
$base_uri = 'https://cmsvt.com/openemr/oauth2/2400/token';
$guzzle = new Client(
    ['verify' => false],
    ['debug' => true]
);
$response = $guzzle->post($base_uri, [
    'form_params' => [
        'grant_type' => 'password',
        'client_id' => getenv('OEMR_TEST_CLIENT_ID'),
        'scope' => "openid fhirUser online_access offline_access user/Observation.read user/Patient.read",
        'user_role' => 'users',
        'username' => getenv('OEMR_RRI_USERNAME'),
        'password' => getenv('OEMR_RRI_PASSWORD')
    ],
]);
$bearer = json_decode((string) $response->getBody(), true)['access_token'];
//echo $bearer . "\n";
//exit;

$client = new Client(['verify' => false],
['debug' => true]
);
$headers = [
  'Authorization' => 'Bearer ' . $bearer,
  'Accept' => 'application/json',
];
$request = new Request('GET', 'https://cmsvt.com/openemr/apis/2400/fhir/Patient?identifier=' . $mrn, $headers);
$res = $client->sendAsync($request)->wait();
$jsonObj = json_decode($res->getBody(), true);
$pt_uuid = $jsonObj['entry'][0]['resource']['id'] ?? null;

if (empty($pt_uuid)) {
  echo "no patient uuid in the emr for some reason \n";
}

//echo $pt_uuid . "\n";

$request = new Request('GET', 'https://cmsvt.com/openemr/apis/2400/fhir/Observation?patient=' . $pt_uuid . '&external_id=' . $visit_no, $headers);
$res = $client->sendAsync($request)->wait();

$jsonObj = json_decode($res->getBody(), true);
//var_dump($jsonObj);
//exit;
$pdf = new Cezpdf();
$pdf->selectFont('Helvetica');
if (!empty($jsonObj['entry'])) {
    //$note = '';
    $count = count($jsonObj['entry']);
    $cntr = 0;
    foreach($jsonObj['entry'] as $entry) {
        $cntr++;
        //$note .= $entry['resource']['code']['coding'][0]['display'] . "\n";

        $note = "MRN: " . $mrn . " Visit No: " . $visit_no .
            " Time: " . $entry['resource']['effectiveDateTime'] . "\n";
        $note .= "Reference No: " . $charcur_key . "\n";
        $note .= $entry['resource']['note'][0]['text'];
        $pdf->ezText($note,10);
        if ($cntr != $count) {
            $pdf->ezNewPage();
        }
    }
    //echo $note . "\n";
} else {
    echo "read not available for some reason, try mpages please \n";
}
//exit;
//var_dump(json_decode($res->getBody()));

$pdf_data = $pdf->ezOutput();
file_put_contents($charcur_key . ".pdf", $pdf_data);
file_put_contents('wcomp1', $charcur_key);
//chgrp('wcomp1', 'cms');
//chmod('wcomp1', 0664);
//echo $charcur_key . ".pdf" . "\n";
$cmd = "chc-wcomp";
exec($cmd, $output);
var_dump($output);
unlink('/tmp/cachedHelvetica.php');
