<?php

//var_dump(getTokenWithRefreshGrant());
//exit;
//echo getAccessToken();
//getTokenWithPasswordGrant();
//var_dump(get835FileList());
//viewFiles();
//sendFile();

// 835 download folder 793491394
//var_dump(getFiles('793491394'));
// list of files in upload folder
//var_dump(getFiles('357858290'));

viewFiles(getFiles('793491394'));

function getAccessToken() {
    $old_timestamp = file_get_contents('moveit_access_timestamp');
    //echo $old_timestamp . " old timestamp \n";
    $test_time = time() - $old_timestamp;
    //echo $test_time . " test_time \n";
    if ($test_time > 1199) {
        //echo "have to refresh grant with $test_time \n";
        getTokenWithPasswordGrant();
    } else {
        //echo "using stored access token with $test_time \n";
    }
    $bearer = file_get_contents('moveit_access_token');
    return $bearer;
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
        if ($resp['error'] ?? '') {
            var_dump($resp);
        } else {
            //var_dump($resp);
            //return json_decode($response, true);
            file_put_contents('moveit_access_token', $resp['access_token']);
            file_put_contents('moveit_access_timestamp', time());
            file_put_contents('moveit_refresh_token', $resp['refresh_token']);
            file_put_contents('moveit_refresh_timestamp', time());
            return true;
        }
    }
    return false;
}

function getTokenWithRefreshGrant()
{
    $curl = curl_init();
    $refresh_token = file_get_contents('moveit_refresh_token');
    //echo $refresh_token . "\n";

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
            'grant_type' => 'refresh_token',
            'refresh_token' => $refresh_token
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
        if ($resp['error'] ?? '') {
            var_dump($resp);
        } else {
            //var_dump($resp);
            //return json_decode($response, true);
            file_put_contents('moveit_access_token', $resp['access_token']);
            file_put_contents('moveit_access_timestamp', time());
            file_put_contents('moveit_refresh_token', $resp['refresh_token']);
            file_put_contents('moveit_refresh_timestamp', time());
            return true;
        }
    }
    return false;
}

function sendFile()
// uses 2 command line args
// 1) raw filename like void37
// 2) id like 02 as in 00711102.x12
{
    global $argv;
    if ($files = getFiles('357858290')) {
        if (checkFiles($files, $argv[2])) {
            die("try to send file again with another number \n");
        } else {
            echo("should be ok to upload 007111" . $argv[2] . ".x12");
        }
    } else {
        echo "no files in upload directory, proceeding ... \n";
    }
    $bearer = getAccessToken();
    $auth = "Authorization: Bearer " . $bearer;
    $curl = curl_init();
    $real_file = realpath($argv[1]);
    $cFile = new CURLFile(
        $real_file,
        '',
        '007111' . $argv[2] . '.x12'
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

    if ($err) {
        echo "cURL Error #01: " . $err;
    } else {
        $resp = json_decode($response, true);
        if ($resp['error'] ?? '') {
            echo "uh oh, errored \n";
            var_dump($resp);
        } else {
            echo $resp['currentFileType'] . "\n";
        }
    }

    //var_dump($response);
}

function getFiles($folder)
{
    $bearer = getAccessToken();
    $auth = "Authorization: Bearer " . $bearer;
    // echo $auth . "\n";
    $curl = curl_init();

    $params = array(
        CURLOPT_HTTPHEADER => array(
            'Content-type: multipart/form-data',
            $auth
        ),
        CURLOPT_URL => 'https://moveit.bcbsvt.com/api/v1/folders/'. $folder . '/files',
        CURLOPT_RETURNTRANSFER => true,
    );

    curl_setopt_array($curl, $params);

    $response = curl_exec($curl);

    if (curl_errno($curl)) {
        echo curl_error($curl);
    } else {
        $status = curl_getinfo($curl);
        // print_r($status);
        echo $status["http_code"] == 200 ? "got files OK \n" : "ERROR - " . $status["http_code"] ;
    }

    curl_close($curl);

    return json_decode($response);
}

function download835($id, $name)
{

    $bearer = getAccessToken();
    $auth = "Authorization: Bearer " . $bearer;
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

function viewFiles($files) {
    //var_dump($files);
    //exit;
    
        foreach ($files as $key => $value) {
            //var_dump($key);
            if ($key == 'items') {
                foreach ($value as $val => $item) {
                    if ($item->isNew == true) {
                        echo "downloading New file: " . $item->name . " uploaded by moveit on " . $item->uploadStamp . " with size " . $item->size . "\n";
                        download835($item->id, $item->name);
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

function checkFiles($files, $id) {
    //var_dump($files);
    //exit;
    foreach ($files as $key => $value) {
        //var_dump($key);
        if ($key == 'items') {
            foreach ($value as $val => $item) {
                $existing_id = substr($item->name, 6, 2);
                echo "existing id $existing_id and new id $id \n";
                if ( $existing_id == $id) {
                    echo "aborting upload of file: " . $item->name . 
                      " already uploaded to moveit at " . $item->uploadStamp .
                      " with size " . $item->size . "\n";
                    return true;
                }
                return false;
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
