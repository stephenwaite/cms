<?php

require_once(__DIR__ . '/../../vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

$context = $argv[1] ?? null;
if (!empty($context) && $context == 'pdf') {
    $pdf = new Cezpdf();
    $pdf->selectFont('Helvetica');
}
$file = file_get_contents(getenv('HOME') . "/W2" . getenv('tid') . getenv('USER'));

$mrn = ltrim(substr($file, 0, 8), '0');
$visit_no = substr($file, 8, 7);
$charcur_key = substr($file, 15, 11);
$base_url = getenv($BASE_OEMR_URL);
$site_id = getenv($OEMR_RRI_SITE_ID);
$base_uri = $base_url . '/oauth2/' . $site_id . '/token';
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

$client = new Client(['verify' => false],
['debug' => true]
);
$headers = [
  'Authorization' => 'Bearer ' . $bearer,
  'Accept' => 'application/json',
];
$request = new Request('GET', $base_url . '/apis/' . $site_id . '/fhir/Patient?identifier=' . $mrn, $headers);
$res = $client->sendAsync($request)->wait();
$jsonObj = json_decode($res->getBody(), true);
$pt_uuid = $jsonObj['entry'][0]['resource']['id'] ?? null;

if (empty($pt_uuid)) {
  echo "no patient uuid in the emr for some reason \n";
}

$request = new Request('GET', $base_url . '/apis/' . $site_id . '/fhir/Observation?patient=' . $pt_uuid . '&external_id=' . $visit_no, $headers);
$res = $client->sendAsync($request)->wait();

$jsonObj = json_decode($res->getBody(), true);


if (!empty($jsonObj['entry'])) {
    $note = '';
    $count = count($jsonObj['entry']);
    $cntr = 0;
    foreach($jsonObj['entry'] as $entry) {
        $cntr++;

        $note .= $entry['resource']['code']['coding'][0]['display'] . "\n";
        $note .= $entry['resource']['effectiveDateTime'] . "\n";
        $note .= $entry['resource']['note'][0]['text'];
        
        if (!empty($context) && $context == 'pdf') {
            $pdf->ezText($note,10);
            if ($cntr != $count) {
                $pdf->ezNewPage();
            }
        } else {
            echo $note . "\n";
        }
    }
} else {
    echo "read not available for some reason, try mpages please \n";
}

if (!empty($context) && $context == 'pdf') {

    $pdf_data = $pdf->ezOutput();
    file_put_contents($charcur_key . ".pdf", $pdf_data);
    file_put_contents('wcomp1', $charcur_key);
    
    $cmd = "chc-wcomp";
    exec($cmd, $output);
    var_dump($output);
    unlink('/tmp/cachedHelvetica.php');
}
exit;