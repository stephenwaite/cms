<?php
require_once('vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Request;

$base_uri = 'https://sandbox.apigw.changehealthcare.com/apip/auth/v2/token';

$guzzle = new Client;

$response = $guzzle->post($base_uri, [
    'form_params' => [
        'grant_type' => 'client_credentials',
        'client_id' => getenv('TEST_CHANGE_CLIENT_ID'),
        'client_secret' => getenv('TEST_CHANGE_CLIENT_SECRET'),
    ],
]);

$bearer = json_decode((string) $response->getBody(), true)['access_token'];
//echo $bearer;
//exit;

// This is the normal Guzzle client that you use in your application
$client = new Client;
$headers = [
    'Authorization' => 'Bearer ' . $bearer,
    'Accept' => '*/*'
];

$url = 'https://sandbox.apigw.changehealthcare.com/medicalnetwork/attachments/status/v1/';
$request = new Request('GET', $url . $argv[1] . '?fieldset=summary', $headers);
$res = $client->sendAsync($request)->wait();
echo $res->getBody();

exit;
