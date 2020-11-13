<?php

        $soapClient = new SoapClient("https://apps.cortexedi.com/AppsWebService.asmx?WSDL");

        $file = file_get_contents($argv[1]);
   
        // Prepare SoapHeader parameters
        $sh_param = array(
                    'username'    =>    'cmswest',
                    'password'    =>    'CH4GgoY8',
                    'buffer'      =>    $file,
                    'filename'    =>    $argv[1]
                );
        //$headers = new SoapHeader('https://cortexedi.com/apps/webservices', 'UserCredentials', $sh_param);
   
        // Prepare Soap Client
        //$soapClient->__setSoapHeaders(array($headers));
   
        // Setup the RemoteFunction parameters
        //$ap_param = array(
        //            'amount'     =>    $irow['total_price']);
                   
        // Call RemoteFunction ()
        //var_dump($sh_param);
        echo "sending " . $argv[1] . " to ngs";
        $error = 0;
        try {
            $info = $soapClient->NGSUploadFile($sh_param);
        } catch (SoapFault $fault) {
            $error = 1;
            print("
            alert('Sorry, blah returned the following ERROR: ".$fault->faultcode."-".$fault->faultstring.". We will now take you back to our home page.');
            window.location = 'main.php';
            ");
        }

        var_dump($info->NGSUploadFileResult);


    