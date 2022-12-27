<?php

echo getAccessToken();

function getAccessToken() {
    $old_timestamp = file_get_contents('moveit_access_timestamp');
    $test_time = time() - $old_timestamp;
    if ($test_time > 1199) {
        echo "using refresh grant with $test_time \n";
        $bearer = getTokenWithRefreshGrant();
    } else {
        echo "using stored access token with $test_time \n";
        $bearer = file_get_contents('moveit_access_token');
    }

    return $bearer;
}

function getTokenWithRefreshGrant()
{
    $curl = curl_init();

    $params = array(
        CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/token',
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_MAXREDIRS => 10,
        CURLOPT_TIMEOUT => 30,
        CURLOPT_POST => 1,
        CURLOPT_NOBODY => false,
        CURLOPT_HTTPHEADER => array(
        "Content-type: application/x-www-form-urlencoded",
        ),
        CURLOPT_POSTFIELDS => http_build_query(array
        (
            'refresh_token' => file_get_contents('moveit_refresh_token'),
            'grant_type' => 'refresh_token'
        ))
    );

    curl_setopt_array($curl, $params);

    $response = curl_exec($curl);
    $err = curl_error($curl);

    curl_close($curl);

    if ($err) {
        echo "cURL Error #01: " . $err;
    } else {
        $resp = json_decode($response, true);
        //var_dump($resp);
        //return json_decode($response, true);
        $bearer = file_put_contents('moveit_access_token', $resp['access_token']);
        file_put_contents('moveit_timestamp', time());
        return $bearer;
    }
}

function getTokenWithPasswordGrant()
{

    $curl = curl_init();

    $params = array(
      CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/token',
      CURLOPT_RETURNTRANSFER => true,
      CURLOPT_MAXREDIRS => 10,
      CURLOPT_TIMEOUT => 30,
      CURLOPT_POST => 1,
      CURLOPT_NOBODY => false,
      CURLOPT_HTTPHEADER => array(
        "Content-type: application/x-www-form-urlencoded",
      ),
      CURLOPT_POSTFIELDS => http_build_query(array
      (
          'username' => getenv('MOVEIT_USERNAME'),
          'password' => getenv('MOVEIT_PASSWORD'),
          'grant_type' => 'password'
      ))
    );

    curl_setopt_array($curl, $params);

    $response = curl_exec($curl);
    $err = curl_error($curl);

    curl_close($curl);

    if ($err) {
        echo "cURL Error #01: " . $err;
    } else {
        $resp = json_decode($response, true);
        //var_dump($resp);
        //return json_decode($response, true);
        file_put_contents('moveit_access_token', $resp['access_token']);
        file_put_contents('moveit_access_timestamp', time());
        file_put_contents('moveit_refresh_token', $resp['refresh_token']);
        file_put_contents('moveit_refresh_timestamp', time());
    }
}

function sendFile($resp)
{
    if (array_key_exists("access_token", $resp)) {
        $auth = "Authorization: Bearer " . $resp['access_token'];
        // echo $auth . "\n";
        $curl = curl_init();
        $real_file = realpath($argv[1]);
        $cFile = new CURLFile(
            $real_file,
            '',
            '00711104.x12'
        );
        $hash = hash_file('sha1', $real_file);
        $data = array(
            'hashtype' => 'sha-1',
            'hash' => $hash,
            'file' => $cFile,
        );
        $params = array(
            CURLOPT_HTTPHEADER => array(
                'Content-type: multipart/form-data',
                $auth
            ),
            CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/folders/357858290/files',
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_POST => 1,
            CURLOPT_POSTFIELDS => $data,
        );
        curl_setopt_array($curl, $params);

        $response = curl_exec($curl);
        $err = curl_error($curl);

        curl_close($curl);

        var_dump($response);
    }
}

function get835FileList($resp): string
{
    $bearer = getBearerToken();
    $auth = "Authorization: Bearer " . $bearer;
    // echo $auth . "\n";
    $curl = curl_init();

    $params = array(
        CURLOPT_HTTPHEADER => array(
            'Content-type: multipart/form-data',
            $auth
        ),
        CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/folders/793491394/files',
        CURLOPT_RETURNTRANSFER => true,
    );

    curl_setopt_array($curl, $params);

    $response = curl_exec($curl);

    curl_close($curl);

    return $response;
}

function download835($id, $name)
{
    global $resp;
    if (array_key_exists("access_token", $resp)) {
        $auth = "Authorization: Bearer " . $resp['access_token'];
        // echo $auth . "\n";
        $fh = fopen($name, "w") or exit("Error opening $name");
        $curl = curl_init();

        $params = array(
            CURLOPT_HTTPHEADER => array(
                'Content-type: multipart/form-data',
                $auth
            ),
            CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/files/' . $id . '/download',
            CURLOPT_FILE => $fh,
        );

        curl_setopt_array($curl, $params);

        curl_exec($curl);
        if (curl_errno($curl)) {
            echo curl_error($curl);
        } else {
            $status = curl_getinfo($curl);
            // print_r($status);
            echo $status["http_code"] == 200 ? "OK" : "ERROR - " . $status["http_code"] ;
        }

            curl_close($curl);
            fclose($fh);
    }
}

function patchFile($id)
{
    global $resp;
    if (array_key_exists("access_token", $resp)) {
        $auth = "Authorization: Bearer " . $resp['access_token'];
        // echo $auth . "\n";
        $curl = curl_init();
        $data = "{'isNew': 'false'}";
        $params = array(
            CURLOPT_HTTPHEADER => array(
                'Content-type: application/json',
                $auth
            ),
            CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/files/' . $id,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_CUSTOMREQUEST => 'PATCH',
            CURLOPT_POSTFIELDS => $data,
        );
        curl_setopt_array($curl, $params);

        $response = curl_exec($curl);
        curl_close($curl);

        var_dump($response);
    }
}

function viewFiles() {
    $files835 = json_decode(get835FileList($resp));

    //var_dump($files835);
    //exit;
    
    foreach ($files835 as $key => $value) {
        if ($key == 'items') {
            foreach ($value as $val => $item) {
                if ($item->isNew == true) {
                    echo "downloading New file: " . $item->name . " uploaded by moveit on " . $item->uploadStamp . " with size " . $item->size . "\n";
                    //download835($item->id, $item->name);
                    //patchFile($item->id);
                } else {
                    echo "old file: " . $item->name . " uploaded by moveit on " . $item->uploadStamp . " with size " . $item->size . "\n";
                }
            }
        }
        //var_dump($value);
       // echo $key . " => " . $value . "\n";
                /* if ($key == 'isNew' and $value == true) {
                    echo "we have new file " . $file['name'] . "\n";
                } else {
                    echo "we have old file " . $file['name'] . "\n";
                } */
    }    
}
