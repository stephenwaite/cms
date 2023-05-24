<?php

require_once('vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

function getControlNumber() {
    $fn = '/home/rri/change_wc_control_number';
    $fh_wcomp_cntrl_no = fopen($fn, 'r+');
    if (!$fh_wcomp_cntrl_no) {
        die("no control # file to read\n");
    }
    $control_number = str_pad((int) fgets($fh_wcomp_cntrl_no) + 1, 9, '0', STR_PAD_LEFT);
    file_put_contents($fn, $control_number);
    return $control_number;
}

$base_uri = 'https://apigw.changehealthcare.com/';
$guzzle = new Client();
$response = $guzzle->post($base_uri . 'apip/auth/v2/token', [
    'form_params' => [
        'grant_type' => 'client_credentials',
        'client_id' => getenv('PROD_CHANGE_CLIENT_ID'),
        'client_secret' => getenv('PROD_CHANGE_CLIENT_SECRET'),
    ],
]);
$bearer = json_decode((string) $response->getBody(), true)['access_token'];
//echo $bearer;
//exit;

$client = new Client();
$headers = [
    'Content-Type' => 'application/json',
    'Authorization' => 'Bearer ' . $bearer,
    'http_errors' => false
];

$cntr = 0;
$fh_wcomp_sid = fopen('/home/rri/clmstatin', 'r');
if ($fh_wcomp_sid) {
    while ($row = fgets($fh_wcomp_sid)) {
        $cntr++;
        $charcur_key = substr($row, 0, 11);
        $ins_neic = substr($row, 11, 5);
        $ins_name = trim(substr($row, 16, 22));
        $ins_street = trim(substr($row, 38, 24));
        $ins_city = trim(substr($row, 62, 15));
        $ins_state = substr($row, 77, 2);
        $ins_zip = trim(substr($row, 79, 9));
        if (strlen($ins_zip) == 5) {
            $ins_zip = $ins_zip . "9999";
        }
        $g_pripol = trim(substr($row, 88, 16));
        $g_garname = substr($row, 104, 24);
        $g_garname_array = explode(';', $g_garname);
        $firstName = trim($g_garname_array[1]);
        $lastName = trim($g_garname_array[0]);
        $g_garno = substr($row, 128, 8);
        $cc_date_t = substr($row, 136, 8);
        $cc_date_a = substr($row, 144, 8);
        $g_group_number = trim(substr($row, 152, 10));
        $g_dob = substr($row, 162, 8);
        $g_sex = substr($row, 170, 1);

        $control_number = getControlNumber();
        $trading_partner_service_id = $ins_neic;
        $trading_partner_name = $ins_name;       

        $control_array = array(
            'controlNumber' => getControlNumber(),
            'tradingPartnerServiceId' => $trading_partner_service_id,
        );

        $datum = new stdClass();
        $datum->organization_name = 'RUTLAND RADIOLOGISTS';
        $datum->taxId = '030238095';
        $datum->providerType = 'BillingProvider';
        $datum_array = array($datum);
        $providers = array(
            'providers' => $datum_array
        );

        $subscriber = array(
            'subscriber' => array(
                'memberId' => $g_pripol,
                'firstName' => $firstName,
                'lastName' => $lastName,
                'gender' => $g_sex,
                'dateOfBirth' => $g_dob,
                'groupNumber' => $g_group_number
            )
        );

        $encounter = array(
            'encounter' => array(
                'beginningDateOfService' => $cc_date_t,
                'endDateOfService'       => $cc_date_t,
                'trackingNumber'         => getControlNumber()
            )
        );
    }
}

$body = array_merge(
    $control_array,
    $providers,
    $subscriber,
    $encounter
);

//echo json_encode($body, JSON_PRETTY_PRINT);
//exit;


$request = new Request(
    'POST', 
    $base_uri . 'medicalnetwork/claimstatus/v2',
    $headers,
    json_encode($body)
);

try {
    $res = $client->sendAsync($request)->wait();
} catch (Exception $e) {
    throw new Exception($e->getResponse()->getBody()->getContents());
    exit;
} 
echo json_encode(json_decode($res->getBody()), JSON_PRETTY_PRINT);

