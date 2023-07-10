<?php

require_once('vendor/autoload.php');

use GuzzleHttp\Client;
use GuzzleHttp\Psr7\Utils;
use GuzzleHttp\Psr7\Request;

$base_uri = 'https://apigw.changehealthcare.com/apip/auth/v2/token';
$guzzle = new Client();
$response = $guzzle->post($base_uri, [
    'form_params' => [
        'grant_type' => 'client_credentials',
        'client_id' => getenv('PROD_CHANGE_CLIENT_ID'),
        'client_secret' => getenv('PROD_CHANGE_CLIENT_SECRET'),
    ],
]);
$bearer = json_decode((string) $response->getBody(), true)['access_token'];
//echo $bearer;
//exit;

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

$url = 'https://apigw.changehealthcare.com/medicalnetwork/attachments/submission/v1/';

$client = new Client();
$headers = [
    'Authorization' => 'Bearer ' . $bearer,
    'Accept' => '*/*',
];

$parm39 = file('parm39');

$organizationName = trim($parm39[7]);
if (strpos($organizationName, ';') !== false) {
    $parts = explode(';', $organizationName);
    $organizationName = trim($parts[1]) . ' ' . trim($parts[0]);
}  
  
$ein = trim($parm39[0]);
$npi = substr($parm39[5], 0, 10);
$address1 = trim($parm39[8]);
$city = trim($parm39[9]);
$state = trim($parm39[10]);
$zip = trim($parm39[11]);

$submitter = array(
    'submitter' => array(
        'organizationName' => $organizationName,
        'etin'             => $ein
    )    
);

$provider = array(
    'provider' => array(
        "organizationName" => $organizationName,
        "npi" => $npi,
        "address" => array(
            "address1" => $address1,
            "city" => $city,
            "state" => $state,
            "postalCode" => $zip
        ),
        'phoneNumber' => '8003718685',
        'faxNumber'   => '8027705175'
    )
);

$cntr = 0;
$fh_wcomp_sid = fopen('wcompin', 'r');
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

        $control_number = getControlNumber();
        $trading_partner_service_id = $ins_neic;
        $trading_partner_name = $ins_name;       

        $req = array(
            'controlNumber' => getControlNumber(),
            'tradingPartnerServiceId' => $trading_partner_service_id,
            'tradingPartnerName' => $trading_partner_name
        );

        $payer_address = array(
            'payerAddress'   => array(
                'address1'   => $ins_street,
                'city'       => $ins_city,
                'state'      => $ins_state,
                'postalCode' => $ins_zip
            )
        );

        $subscriber = array(
            'subscriber' => array(
                'memberId' => $g_pripol,
                'firstName' => $firstName,
                'lastName' => $lastName
            )
        );

        $claim_information = array(
            'claimInformation' => array(
                'patientControlNumber' => $g_garno,
                'claimServiceDate' => $cc_date_t,
                'serviceLines' => array(
                        array(
                        'providerAttachmentControlNumber' => $charcur_key,
                        'serviceLineDateInformation' => array(
                            'serviceDate' => $cc_date_t,
                            'submissionDate' => $cc_date_a
                        ),
                        'attachmentDetails' => array(
                            'name' => $charcur_key . '.pdf'
                        )
                    )
                )
            )
        );

        $data = array_merge(
            $req,
            $payer_address,
            $submitter,
            $provider,
            $subscriber, 
            $claim_information
        );
        //var_dump($req);
        //exit;
        $options = [
            'multipart' => [
                [
                    'name' => 'files',
                    'filename' => $charcur_key . '.pdf',
                    'contents' => Utils::tryFopen($charcur_key . '.pdf', 'r'),
                ],
                [
                    'name' => 'request',
                    'contents' => json_encode($data),
                ]
            ]
        ];
    

        $url = 'https://apigw.changehealthcare.com/medicalnetwork/attachments/submission/v1/uploads';

        $request = new Request('POST', $url, $headers);
        $res = $client->sendAsync($request, $options)->wait();
        echo $res->getBody();
    }
}
