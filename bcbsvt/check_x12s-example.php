    <?php

    use phpseclib3\Net\SFTP;

    require_once(dirname(__FILE__) . '/../vendor/autoload.php');

    $remote_user = getenv('REMOTE_USERNAME');
    $remote_pass = getenv('REMOTE_PASSWORD');
    $sftp = new SFTP('remote_server.example.com');
    $sftp->login($user, $pass);

    $path = '/somepath_to_files';

    $rawlist = $sftp->rawlist($path, true);

    if (!empty($rawlist)) {
        foreach ($rawlist as $file) {
            if (!empty($file)) {
                $dt_utc = new DateTimeImmutable(date('Y-m-d h:i:s', $file->mtime));
                $dt_nyc = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
                echo "file: " . $file->filename . " with create time of " .
                    $dt_nyc->format('Y-m-d H:i:s T') . "\n";
            }
        }
    } else {
        // there's an empty directory
    }

        echo "end of list \n";
