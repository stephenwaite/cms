<?php

require_once('vendor/autoload.php');

use GuzzleHttp\Client;

$base_uri = 'https://sandbox.apigw.changehealthcare.com/apip/auth/v2/token';

$guzzle = new Client();

$response = $guzzle->post($base_uri, [
    'form_params' => [
        'grant_type' => 'client_credentials',
        'client_id' => getenv('TEST_CHANGE_CLIENT_ID'),
        'client_secret' => getenv('TEST_CHANGE_CLIENT_SECRET'),
    ],
]);

$bearer = json_decode((string) $response->getBody(), true)['access_token'];
echo $bearer;
exit;
$bearer_auth = 'Bearer ' . $bearer;

$payer_address = array(
    'payerAddress'   => array(
        'address1'   => "123 address1",
        'city'       => 'city1',
        'state'      => 'wa',
        'postalCode' => '981010000'
    )
);

$control_number = '123459999';
$trading_partner_service_id = 'TESTEP51';
$trading_partner_name = 'test';

$data = array(
    'controlNumber' => $control_number,
    'tradingPartnerServiceId' => $trading_partner_service_id,
    'tradingPartnerName' => $trading_partner_name
);

$submitter = array(
    'submitter' => array(
        'organizationName' => 'happy doctors grouppractice',
        'etin'             => '1942788757'
    )
);

$provider = array(
    'provider' => array(
        "organizationName" => "happy doctors group",
        "npi" => "1760854442",
        "address" => array(
            "address1" => "123 address1",
            "city" => "city1",
            "state" => "wa",
            "postalCode" => "981010000"
        ),
        'phoneNumber' => '123456789',
        'faxNumber'   => '123456789'
    )
);

$subscriber = array(
    'subscriber' => array(
        'memberId' => '0000000001',
        'firstName' => 'johnone',
        'lastName' => 'doeone'
    )
);

$name = 'rightarm.jpg';
$claim_information = array(
    'claimInformation' => array(
        'patientControlNumber' => '12345',
        'payerControlNumber' => '00001',
        'beginClaimServiceDate' => '20050513',
        'endClaimServiceDate'   => '20050514',
        'serviceLines' => array(
            array(
                'providerAttachmentControlNumber' => '123456789',
                'serviceLineDateInformation' => array(
                    'submissionDate' => '20050514'
                ),
                'attachmentDetails' => array(
                    'name' => $name
                )
            )
        )
    )
);

$data = array_merge(
    $data,
    $payer_address,
    $submitter,
    $provider,
    $subscriber,
    $claim_information
);

echo json_encode($data);

$url = 'https://sandbox.apigw.changehealthcare.com/medicalnetwork/attachments/submission/v1/uploads';

$curl = curl_init($url);
curl_setopt($curl, CURLOPT_HTTPHEADER, array(
    "Content-type: multipart/form-data",
    $bearer_auth)
);
curl_setopt($curl, CURLOPT_POST, true);
curl_setopt($curl, CURLOPT_POSTFIELDS, [
    'files' => new CURLFile('base64.jpg', 'image/jpeg', $name),
    'request' => json_encode($data)
]);

$resp = curl_exec($curl);

//echo $resp."<br /><br />".$http_status;

