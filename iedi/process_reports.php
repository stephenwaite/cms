<?php
$user = getenv('USER');
$batch_file = '/tmp/w22' . $user;
$remit_file = '/tmp/w33' . $user;
if (file_exists($batch_file)) {
    unlink($batch_file);
}

if (!touch($batch_file)) {
    echo "batch file creation failed, exiting..." . "\n";
    exit;
}

if (file_exists($remit_file)) {
    unlink($remit_file);
}

if (!touch($remit_file)) {
    echo "remit file creation failed, exiting..." . "\n";
    exit;
}

foreach (new DirectoryIterator('.') as $file) {
    if ($file->isDot()) {
        continue;
    }
    $fileName =  $file->getFilename();
    $ext = pathinfo($fileName, PATHINFO_EXTENSION);
    $za = new ZipArchive();
    switch ($ext) {
        case 'TXT':
        print $fileName . "\n";
        if (preg_match('/(IK5|AK9)\*[RE]/', file_get_contents($fileName))) {
            echo "uh oh, have to check out 999 error or reject $fileName\n";
        }
        readline('enter to continue');
        break;
        case 'txt':
            print $fileName . "\n";
            $contents = file_get_contents($fileName);
            echo $contents;
            readline('enter to continue');
            break;
        case '277':
            file_put_contents($batch_file, '/home/sidw/iedi/' . $fileName . "\n", FILE_APPEND);
            break;
        case '835':
            file_put_contents($remit_file, '/home/sidw/iedi/' . $fileName . "\n", FILE_APPEND);
            break;
        case 'zip':
            if ($za->open($fileName) !== true) { echo "bad zip: $fileName\n"; break; }
            for ($i = 0; $i < $za->numFiles; $i++) {
                $name = $za->statIndex($i)['name'];
                if (pathinfo($name, PATHINFO_EXTENSION) !== '277') continue;
                $dest = '/tmp/' . basename($name);
                copy("zip://$fileName#$name", $dest);
                file_put_contents($batch_file, $dest . "\n", FILE_APPEND);
            }
            $za->close();
            break;
    }
}

exit;
