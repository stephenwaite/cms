<?php
$user = getenv('USER');
$batch_file = '/tmp/w22' . $user;
if (file_exists($batch_file)) {
    unlink($batch_file);
}

if (!touch($batch_file)) {
    echo "batch file creation failed, exiting..." . "\n";
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
        case '999':
            print $fileName . "\n";
            $flag = false;
            foreach (file($fileName) as $line) {
                if ((stripos($line, 'R') != false) || (stripos($line, 'E') != false)) {
                    $flag = true;
                    if ((stripos($line, 'SE') != false) || (stripos($line, 'GE') != false) || (stripos($line, 'IEA') != false)) {
                        $flag = false;
                    }
                }
            }
            if (!empty($flag)) {
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
            file_put_contents($batch_file, '/home/sidw/oa' . $fileName . "\n", FILE_APPEND);
            break;
        case 'zip':
            $za->open($fileName);
            for ($i = 0; $i < $za->numFiles; $i++) {
                $stat = $za->statIndex($i);
                $fileName277 = basename($stat['name']) . PHP_EOL;
                $za->extractTo('/tmp');
                file_put_contents($batch_file, '/tmp/' . $fileName277, FILE_APPEND);
            }
            $za->close();
            break;
    }
}

exit;
