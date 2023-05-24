<?php

require_once('vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

$base_uri = 'https://moveit.bcbsvt.com/api/v1/token';
$guzzle = new Client();
$response = $guzzle->post($base_uri, [
    'form_params' => [
        'grant_type' => 'password',
        'username' => getenv('MOVEIT_USERNAME'),
        'password' => getenv('MOVEIT_PASSWORD'),
    ],
]);
$bearer = json_decode((string) $response->getBody(), true)['access_token'];
//echo $bearer;
//exit;

$client = new Client();
$headers = [
    'Authorization' => 'Bearer ' . $bearer,
    'Accept' => '*/*',
];

/* $list_of_folders_url = 'https://moveit.bcbsvt.com/api/v1/folders/';
$request = new Request('GET', $list_of_folders_url, $headers);
$res = $client->sendAsync($request)->wait();
echo json_encode(json_decode($res->getBody()), JSON_PRETTY_PRINT);
exit;
 */
//$folder = '793491394';
// 835
//$subfolderId = '793491394';
// ErrorReports
$parent_id = '774903675';
$subfolderId = '776168668';

/* $list_of_subfolders_url = 'https://moveit.bcbsvt.com/api/v1/folders/' . $folder . '/subfolders/';
$request = new Request('GET', $list_of_subfolders_url, $headers);
$res = $client->sendAsync($request)->wait();
echo json_encode(json_decode($res->getBody()), JSON_PRETTY_PRINT);
exit;
 */
$url = 'https://moveit.bcbsvt.com/api/v1/folders/' . $subfolderId . '/files';

$request = new Request('GET', $url, $headers);
$res = $client->sendAsync($request)->wait();

//echo json_encode(json_decode($res->getBody()), JSON_PRETTY_PRINT);
$jsonobj = json_decode($res->getBody());
//var_dump($jsonobj);
//exit;

foreach($jsonobj->items as $item) {
    if ($item->isNew) {
        echo "*** NEW ERROR *** " . $item->path . " " . $item->uploadStamp . "\n";
    } else {
        //var_dump($item);
        echo "old error " . $item->path . " " . $item->uploadStamp . "\n";
    }
}




