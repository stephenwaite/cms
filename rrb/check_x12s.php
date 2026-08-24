<?php
use phpseclib3\Net\SFTP;

require_once(dirname(__FILE__) . '/../vendor/autoload.php');

$cms_creds = [
    ['user' => getenv('RRBERN_USERNAME'), 'pass' => getenv('RRBERN_PASSWORD')],
    ['user' => getenv('RRB_USERNAME'),    'pass' => getenv('RRB_PASSWORD')],
];

foreach ($cms_creds as $cred) {
    $cms_user = $cred['user'];
    $cms_pass = $cred['pass'];

    if (!is_string($cms_user) || $cms_user === '' || !is_string($cms_pass) || $cms_pass === '') {
        fwrite(STDERR, "Skipping: missing credentials (user=" . var_export($cms_user, true) . ")\n");
        continue;
    }

    $sftp = new SFTP('edi.palmetto.ahdsxhub.com', '22');
    if (!$sftp->login($cms_user, $cms_pass)) {
        fwrite(STDERR, "Login failed for $cms_user\n");
        continue;
    }

    $path = '/inbox/EZComm/BC/1.0/Notify';
    $rawlist = $sftp->rawlist($path, true);
    if (!empty($rawlist)) {
        foreach ($rawlist as $file) {
            if (!empty($file)) {
                $dt_utc = new DateTimeImmutable(date('Y-m-d H:i:s', $file->mtime));
                $dt_nyc = $dt_utc->setTimezone(new DateTimeZone('America/New_York'));
                echo "file: " . $file->filename . " uploaded to palmetto on " .
                    $dt_nyc->format('Y-m-d h:i:s a') . "\n";
            }
        }
    } else {
        // there's a test directory
    }
    echo "end of list \n";
}