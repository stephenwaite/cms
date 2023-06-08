<?php

require 'vendor/autoload.php';

use PHPMailer\PHPMailer\PHPMailer;
use PHPMailer\PHPMailer\SMTP;

$mail = new PHPMailer();

$mail->isSMTP();

$mail->SMTPDebug = SMTP::DEBUG_SERVER;

$mail->Host = 'smtp.gmail.com';

//Set the SMTP port number:
// - 465 for SMTP with implicit TLS, a.k.a. RFC8314 SMTPS or
// - 587 for SMTP+STARTTLS
//$mail->Port = 465;
$mail->Port = 587;

//$mail->SMTPSecure = PHPMailer::ENCRYPTION_SMTPS;
$mail->SMTPSecure = PHPMailer::ENCRYPTION_STARTTLS;

$mail->SMTPAuth = true;
$mail->Username = getenv('STI_EMAIL_USERNAME');
$mail->Password = getenv('STI_EMAIL_PASSWORD');

$mail->setFrom('markstickneymd@gmail.com', 'Mark Stickney MD');
//$mail->addReplyTo('markstickneymd@gmail.com', 'Mark Stickney MD');

$mail->addAddress('stephen.waite@cmsvt.com', 'Steve');

$mail->Subject = 'Test Email via gmail SMTP using PHPMailer';

$mail->Body = 'hi Steve';

if($mail->send()){
    echo 'Message has been sent';
}else{
    echo 'Message could not be sent.';
    echo 'Mailer Error: ' . $mail->ErrorInfo;
}