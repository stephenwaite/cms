<?php

$soapClient = new SoapClient("https://apps.cortexedi.com/AppsWebService.asmx?WSDL");

// Prepare SoapHeader parameters
$sh_param = array(
            'username'    =>    $argv[1],
            'password'    =>    $argv[2]);
//$headers = new SoapHeader('https://cortexedi.com/apps/webservices', 'UserCredentials', $sh_param);

// Prepare Soap Client
//$soapClient->__setSoapHeaders(array($headers));

// Setup the RemoteFunction parameters
//$ap_param = array(
//            'amount'     =>    $irow['total_price']);

// Call RemoteFunction ()
$error = 0;
try {
    $response = $soapClient->NGSDownloadReports($sh_param);
} catch (SoapFault $fault) {
    $error = 1;
    echo "Sorry, blah returned the following ERROR: " . $fault->faultcode . "-" .
        $fault->faultstring . ". Wassup with stee's script?";
}

file_put_contents($argv[3], $response->NGSDownloadReportsResult);


