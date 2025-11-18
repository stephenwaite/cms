<?php

require_once(__DIR__ . '/../../vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

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

$fo = fopen('/home/rri/wclaimstat', 'w');

$cntr = 0;
$fh_wcomp_sid = fopen('/home/rri/clmstatin', 'r');
if ($fh_wcomp_sid) {
    while ($row = fgets($fh_wcomp_sid)) {
        $cntr++;
        $charcur_key = substr($row, 0, 11);
        $ins_neic = trim(substr($row, 11, 5));
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

        $controlNumber = getControlNumber();
        $tradingPartnerServiceId = $ins_neic;
        $tradingPartnerName = $ins_name;       

        /* $control_array = array(
            'controlNumber' => '123456789',
            'tradingPartnerServiceId' => 'CMSMED',
        ); */

        $control_array = array(
            'controlNumber' => getControlNumber(),
            'tradingPartnerServiceId' => $tradingPartnerServiceId,
        );

        $providerObj = new stdClass();
        $providerObj->organizationName = 'RUTLAND RADIOLOGISTS';
        $providerObj->npi = '1700935780';
        $providerObj->taxId = "030238095";
        $providerObj->providerCode = "BI";
        $providerObj->referenceIdentification = "2085R0202X";
        $provider = array(
            'provider' => $providerObj
        );
        
        $subscriberObj = new stdClass();
        $subscriberObj->memberId = $g_pripol;
        $subscriberObj->firstName = $firstName;
        $subscriberObj->lastName = $lastName;
        $subscriberObj->gender = $g_sex;
        $subscriberObj->dateOfBirth = $g_dob;
        //$subscriberObj->ssn = "555443333";
        //$subscriberObj->idCard = "card123";
        //$subscriberObjArray = array($subscriberObj);
        $subscriber = array(
            'subscriber' => $subscriberObj
        );


        $encounterObj = new stdClass();
        $encounterObj->beginningDateOfService = $cc_date_t;
        $encounterObj->endDateOfService       = $cc_date_t;
        $encounterObj->serviceTypeCodes = [
            "52"
        ];
        $encounter = array(
            'encounter' => $encounterObj
        );
    }
}

$body = array_merge(
    $control_array,
    $provider,
    $subscriber,
//    $dependents,
    $encounter
);

echo json_encode($body, JSON_PRETTY_PRINT);
//exit;

/* $body = '{
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
  "encounter": {
    "beginningDateOfService": "20100101",
    "endDateOfService": "20100102",
    "trackingNumber": "ABCD"
  }
}'; */
$request = new Request(
    'POST', 
    $base_uri . 'medicalnetwork/eligibility/v3/',
    $headers,
    json_encode($body)
);

try {
    $res = $client->sendAsync($request)->wait();
} catch (Exception $e) {
    throw new Exception($e->getResponse()->getBody()->getContents());
} 

$responseJson = json_decode($res->getBody());
echo json_encode($responseJson, JSON_PRETTY_PRINT);
file_put_contents($fo, $responseJson);


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