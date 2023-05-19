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

$base_uri = 'https://sandbox.apigw.changehealthcare.com/';
$guzzle = new Client();
$response = $guzzle->post($base_uri . 'apip/auth/v2/token', [
    'form_params' => [
        'grant_type' => 'client_credentials',
        'client_id' => getenv('TEST_CHANGE_CLIENT_ID'),
        'client_secret' => getenv('TEST_CHANGE_CLIENT_SECRET'),
    ],
]);
$bearer = json_decode((string) $response->getBody(), true)['access_token'];
//echo $bearer;
//exit;

$client = new Client();
$headers = [
    'Content-Type' => 'application/json',
    'Authorization' => 'Bearer ' . $bearer,
];


$body = '{
  "controlNumber": "000000001",
  "tradingPartnerServiceId": "serviceId",
  "providers": [
    {
      "organizationName": "TestProvider",
      "taxId": "0123456789",
      "providerType": "BillingProvider"
    },
    {
      "organizationName": "happy doctors group",
      "npi": "1760854442",
      "providerType": "ServiceProvider"
    }
  ],
  "subscriber": {
    "memberId": "0000000000",
    "firstName": "johnone",
    "lastName": "doeone",
    "gender": "M",
    "dateOfBirth": "18800102",
    "groupNumber": "0000000000"
  },
  "dependent": {
    "firstName": "janeone",
    "lastName": "doeone",
    "gender": "F",
    "dateOfBirth": "18800101",
    "groupNumber": "0000000000"
  },
  "encounter": {
    "beginningDateOfService": "20100101",
    "endDateOfService": "20100102",
    "trackingNumber": "ABCD"
  }
}';
$request = new Request('POST', $base_uri . 'medicalnetwork/claimstatus/v2', $headers, $body);
$res = $client->sendAsync($request)->wait();
echo json_encode(json_decode($res->getBody()), JSON_PRETTY_PRINT);

