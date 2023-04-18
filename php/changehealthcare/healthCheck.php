<?php

require_once('vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Request;

$guzzle = new Client();
$base_uri = 'https://sandbox.apigw.changehealthcare.com/apip/auth/v2/token';

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

$headers = [
    'Authorization' => 'Bearer ' . $bearer,
    'Accept' => '*/*'
];
$url = 'https://sandbox.apigw.changehealthcare.com/medicalnetwork/attachments/submission/v1/healthcheck';
// This is the normal Guzzle client that you use in your application

$client = new Client();

$request = new Request('GET', $url, $headers);
$res = $client->sendAsync($request)->wait();
echo $res->getBody();

exit;
