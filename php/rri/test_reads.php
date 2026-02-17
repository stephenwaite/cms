<?php

require_once(__DIR__ . '/../../vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

ini_set("xdebug.var_display_max_children", '-1');
ini_set("xdebug.var_display_max_data", '-1');
ini_set("xdebug.var_display_max_depth", '-1');

// first remove tmp courier cached file in case some other script put it there
$cache_font_file = "/tmp/cachedCourier.php";
if (is_file($cache_font_file) && !is_writable($cache_font_file)) {
    unlink($cache_font_file);
}

$context = $argv[1] ?? null;
if (!empty($context) && $context == 'pdf') {
    $pdf = new Cezpdf();
    $pdf->selectFont('Courier');
}


const CT_QUALIFYING_CPT = [
    '70490' => true, '70491' => true, '70492' => true,
    '75571' => true, '75572' => true, '75573' => true, '75574' => true,
    '70498' => true, '71250' => true, '71260' => true, '71270' => true,
    '71275' => true, '72125' => true, '72126' => true, '72127' => true,
    '72128' => true, '72129' => true, '72130' => true, '74150' => true,
    '74160' => true, '74170' => true, '74174' => true, '74175' => true,
    '74176' => true, '74177' => true, '74178' => true,
];

function isQualifyingCtCpt(string $coding_display): ?string
{
    foreach (CT_QUALIFYING_CPT as $code => $unused) {
        if (str_contains($coding_display, $code)) {
            return $code;
        }
    }
    return false;
}

function getQualifyingLungFindings(string $note): array
{
    $note_lower = strtolower($note);

    return [
        'pulmonary_nodule'          => str_contains($note_lower, 'pulmonary nodule'),
        'includes_guidelines'       => str_contains($note_lower, 'fleischner society 2017'),
    ];
}

$cms_user = getenv('USER');
$file = file_get_contents(getenv('HOME') . "/W2" . getenv('tid') . $cms_user);

$mrn = ltrim(substr($file, 0, 8), '0');
$visit_no = substr($file, 8, 7);
if (substr($visit_no, 0, 1) == '0') {
    $visit_no = "1" . $visit_no;
}
//echo $visit_no . " visit no \n";
$charcur_key = substr($file, 15, 11);
$billing_tape_date_of_service = substr($file, 26, 8);
//$date_of_service = substr($file, 30, 2) . "-" .  substr($file, 32, 2) . "-" . substr($file, 26, 4);
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
$ptObj = json_decode($res->getBody(), true);
$pt_uuid = $ptObj['entry'][0]['resource']['id'] ?? null;
//echo $pt_uuid . " pt uuid\n";
if (empty($pt_uuid)) {
    echo "no patient uuid in the emr for some reason \n";
    exit;
}

$pt_name_array = $ptObj['entry'][0]['resource']['name'][0] ?? null;
$pt_name_text = ($pt_name_array['family'] ?? '') . ", " . ($pt_name_array['given'][0] ?? '') .
    " " . ($pt_name_array['given'][1] ?? '');
$pt_birthdate = $ptObj['entry'][0]['resource']['birthDate'] ?? null;
$pt_dob = new DateTimeImmutable($pt_birthdate);
$pt_dob_line = "DOB: " . $pt_dob->format('m-d-Y');

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
        $coding_display = $entry['resource']['code']['coding'][0]['display'];
        //echo "CODING DISPLAY:" . $coding_display . "\n";
        $interp = $entry['resource']['note'][0]['text'];
        //var_dump($interp);
        $lung_findings = getQualifyingLungFindings($interp);
        if ($cpt = isQualifyingCtCpt($coding_display)) {
            if ($lung_findings['pulmonary_nodule']) {
                if ($lung_findings['includes_guidelines']) {
                    echo "\n*** Pulmonary nodule and guidelines mentioned for {$cpt} Yay! ***\n";
                    readline("Press ENTER to continue...");
                } else {
                    echo "\n*** ALERT: Pulmonary nodule but NO guidelines mentioned: {$cpt} ***\n";
                    readline("Press ENTER to continue...");
                }
            }
        }
        $coding_display_length = strlen($coding_display);
        $pt_name_text_length = strlen($pt_name_text);
        if ($coding_display_length > 15 || $pt_name_text_length > 15) {
            $banner_length = ($coding_display_length > $pt_name_text_length) ?
                $coding_display_length : $pt_name_text_length;
        } else {
            $banner_length = 15;
        }
        $note = str_pad('', $banner_length, '#') . "\n";
        $note .= $pt_name_text . "\n";
        $note .= $pt_dob_line . "\n";
        $note .= $coding_display . "\n";
        $date_of_read = $entry['resource']['effectiveDateTime'] ?? '';
        $date_of_read_utc = new DateTimeImmutable($date_of_read);
        $date_of_read_nyc = $date_of_read_utc->setTimezone(new DateTimeZone('America/New_York'));
        $date_of_read_nyc_display = $date_of_read_nyc->format('m-d-Y');
        $pt_dos_line = 'DOS: ' . $date_of_read_nyc_display;
        $note .= $pt_dos_line . "\n";
        $note .= str_pad('', $banner_length, '#') . "\n\n";
        $note .= $interp . "\n";

        if (!empty($context) && $context == 'pdf') {
            $pdf->ezText($note, 10);
            if ($cntr != $count) {
                $pdf->ezNewPage();
            }
        } else {
            //echo $note . "\n";
            //readline("one line at a time?");
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
    unlink('/tmp/cachedCourier.php');
}
