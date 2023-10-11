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
$cms_user = getenv('USER');
$file = file_get_contents(getenv('HOME') . "/W2" . getenv('tid') . $cms_user);

$mrn = ltrim(substr($file, 0, 8), '0');
$visit_no = substr($file, 8, 7);
$charcur_key = substr($file, 15, 11);
$raw_date_of_service = substr($file, 26, 8);
$date_of_service = substr($file, 30, 2) . "-" .
  substr($file, 32, 2) . "-" . substr($file, 26, 4);
$base_url = getenv('BASE_OEMR_URL');
$site_id = getenv('OEMR_RRI_SITE_ID');
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

$client = new Client(
    ['verify' => false],
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
$pt_name_array = $jsonObj['entry'][0]['resource']['name'] ?? null;
$pt_name_text = $pt_name_array[0]['family'] . ", " . $pt_name_array[0]['given'][0];
$pt_birthdate = $jsonObj['entry'][0]['resource']['birthDate'] ?? null;
$pt_dob = new DateTimeImmutable($pt_birthdate);
$pt_line = $pt_name_text . " DOB: " . $pt_dob->format('m-d-Y');


if (empty($pt_uuid)) {
    echo "no patient uuid in the emr for some reason \n";
}

$request = new Request(
    'GET',
    $base_url . '/apis/' . $site_id . '/fhir/Observation?patient=' . $pt_uuid . '&external_id=' . $visit_no,
    $headers
);

$res = $client->sendAsync($request)->wait();

$jsonObj = json_decode($res->getBody(), true);

if (!empty($jsonObj['entry'])) {
    $note = '';
    $count = count($jsonObj['entry']);
    $cntr = 0;
    foreach ($jsonObj['entry'] as $entry) {
        $cntr++;
        $banner_length = strlen($pt_line);
        $note = str_pad('', $banner_length, '#') . "\n";
        $note .= $pt_line . "\n";
        $note .= $entry['resource']['code']['coding'][0]['display'] . "\n";
        $note .= 'Date of Srvc: ' . $date_of_service . "\n";
        $note .= str_pad('', $banner_length, '#') . "\n\n";
        $date_of_read = $entry['resource']['effectiveDateTime'];
        $date_of_read_utc = new DateTimeImmutable($date_of_read);
        $date_of_read_nyc = $date_of_read_utc->setTimezone(new DateTimeZone('America/New_York'));
        $date_of_read_nyc_display = $date_of_read_nyc->format('m-d-Y');
        $note .= $entry['resource']['note'][0]['text'] . "\n";
        $date_dos = new DateTime($raw_date_of_service);

        if ($date_of_read_nyc < $date_dos) {
            echo "*** Date read " .  $date_read_nyc_display .
                " is before DOS " . $date_dos->format('Y-m-d H:i:s') . " *** \n";
        }

        if (!empty($context) && $context == 'pdf') {
            $pdf->ezText($note, 10);
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

    // just save file to rri dir if need a pdf of all reports for uhc for instance
    $line = strtoupper(readline("Upload " . $charcur_key . ".pdf to change wcomp? (y or Y) "));
    if (strpos($line, "Y") !== false) {
        echo "uploading to change\n";
        $cmd = "chc-wcomp";
        exec($cmd, $output);
        var_dump($output);
    } else {
        echo "saved pdf under rri but not uploading \n";
        $line = strtoupper(readline("Download " . $charcur_key . ".pdf to your comp? (y or Y) "));
        if (strpos($line, "Y") !== false) {
            $filename = exec('pwd') . "/" . $charcur_key . ".pdf";
            $tty = exec('tty');
            echo "downloading $filename \n";
            echo " for $cms_user \n";
            if ($cms_user == 'lynda') {
                $cmd = "sz $filename > $tty < $tty";
                exec($cmd, $output);
            } else {
                echo "                not implemented for " . $cms_user . "\n";
            }
        }
    }
    unlink('/tmp/cachedHelvetica.php');
}
