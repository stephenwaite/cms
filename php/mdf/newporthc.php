<?php

require __DIR__ . '/vendor/autoload.php';

// Parse PDF file and build necessary objects.

function x12Clean($str)
    {
        return trim(preg_replace('/[^A-Z0-9!"\\&\'()+,\\-.\\/;?=@ ]/', '', strtoupper($str)));
    }

function x12Zip($zip)
    {
        $zip = x12Clean($zip);
        // this will take out dashes and pad with trailing 9s if not 9 digits
        return preg_replace('/[^0-9]/', '', $zip);
        /* return str_pad(
            preg_replace('/[^0-9]/', '', $zip),
            9,
            9,
            STR_PAD_RIGHT
        ); */
    }

//$config = new Smalot\PdfParser\Config();
//$config->setDataTmFontInfoHasToBeIncluded(true);
$parser = new \Smalot\PdfParser\Parser();
$path_to_mdf = '/home/stee/Documents/mdf';
$directory_of_facesheets = $path_to_mdf . '/newport_facesheets';
$array_of_facesheets = scandir($directory_of_facesheets);
$bad_dirs = array('.', '..');

foreach ($array_of_facesheets as $key => $facesheet) {
    if (in_array($facesheet, $bad_dirs)) {
        continue;
    }
    $filename = $directory_of_facesheets . '/' . $facesheet;
    //echo $facesheet . "\n";
    //echo $filename;
    $pdf = $parser->parseFile($directory_of_facesheets . '/' . $facesheet);
    $facesheet_text = $pdf->getPages()[0]->getText();
    createPerson($facesheet_text);
}

function createPerson($text) {
    $arr = explode("\n", $text);
    //$ptName = getPatientName($arr['5']);
    
    foreach ($arr as $key => $value) {
        //echo $value . "\n";
        if (trim($key) == 'ADMISSION RECORD') {
            $line_no = 0;
        } else {
            $line_no = $key;
        }
        if (trim($prior_key ?? '') == 'Resident Name') {
            //echo $line_no . "\n";
            $mrn = getMRN($value);
            $data[$mrn] = array();
            $name = getPatientName($value);
            $offset = 1;
            $address = getPhoneAndAddress($arr[$key + $offset]);
            $offset = 2;
            if (checkTextForDOB($arr[$key + $offset])) {
                $dob = getDOB($arr[$key + $offset]);
            } elseif (checkTextForDOB($arr[$key + $offset + 1])) {
                $offset++;
                $dob = getDOB($arr[$key + $offset]);
            } else {
                $offset = $offset + 2;
                $dob = getDOB($arr[$key + $offset]);
            }
            
            $medicare_primary = false;
            $medicaid_primary = false;
            $offset++;
            $mbi = null;
            $gmc_id = null;
            if (checkTextForMedicare($arr[$key + $offset])) {
                if (!checkTextForMedicaid($arr[$key + $offset])) {
                    $offset++;
                    $medicare_primary = true;
                    $mbi = getMBI($arr[$key + $offset]);
                } else {
                    $offset++;
                    $gmc_id = getPolicyNo($arr[$key + $offset]);
                    $offset++;
                    if (checkTextForSSN($arr[$key + $offset])) {
                        $offset++;
                        $ssn = getSSN($arr[$key + $offset]);
                        if (checkTextForInsuranceName($arr[$key + $offset])) {
                            $offset++;
                            $ins_code = getInsuranceCode(x12Clean($arr[$key + $offset]));
                            $offset++;
                            $offset++;
                            $ins_pol = getPolicyNo($arr[$key + $offset]);
                            $secins = "04";
                            $secpol = $gmc_id;
                        } else {
                            $medicaid_primary = true;
                        }
                    } else {
                        $offset++;
                        $secins = "04";
                        $secpol = getPolicyNo($arr[$key + $offset]);
                    }
                }
            } else {
                if (checkTextForAdmitted($arr[$key + $offset])) {
                    $offset++;
                    if (!checkTextForMedicaid($arr[$key + $offset])) {
                        $offset++;
                        $medicare_primary = true;
                        $mbi = getMBI($arr[$key + $offset]);
                    } else {
                        $offset++;
                        $medicaid_primary = true;
                        $gmc_id = getPolicyNo($arr[$key + $offset]);
                    }
                }
            }
            
            if ($medicare_primary) {
                $prins = "03";
                $pripol = $mbi;
            }

            if ($medicaid_primary) {
                $prins = "04";
                $pripol = $gmc_id;
            }

            
            // determine if 03/04
            $offset++;
            //echo $arr[$key + $offset] . "\n";

            if (checkTextForMedicaid($arr[$key + $offset])) {
                $offset++;
                if ($medicare_primary) {
                    //echo "2ndary medicaid policy ";
                    $secins = "04";
                    $secpol = getPolicyNo($arr[$key + $offset]);
                    $offset++;
                    if (checkTextForSSN($arr[$key + $offset])) {
                        $offset++;
                        $ssn = getSSN($arr[$key + $offset]);
                        if (checkTextForInsuranceName($arr[$key + $offset])) {
                            $offset++;
                            $ins_code = getInsuranceCode(x12Clean($arr[$key + $offset]));
                            $offset++;
                            $offset++;
                            $ins_pol = getPolicyNo($arr[$key + $offset]);
                        }
                    }
                } else {
                    //echo "not 2ndary 04 ";
                    //echo $arr[$key + $offset] . "\n";

                }
                //echo $arr[$key + $offset] . "\n";
            } elseif (checkTextForMBI($arr[$key + $offset])) {
                $offset++;
                // skip over repeat of MBI
                $offset++;
                // check for medicaid
                if (checkTextForMedicaid($arr[$key + $offset])) {
                    if (checkTextForSSN($arr[$key + $offset])) {
                        $offset++;
                        $ssn = getSSN($arr[$key + $offset]);
                        if (checkTextForInsuranceName($arr[$key + $offset])) {
                            $offset++;
                            $ins_code = getInsuranceCode(x12Clean($arr[$key + $offset]));
                            $offset++;
                            $offset++;
                            $ins_pol = getPolicyNo($arr[$key + $offset]);
                        }
                    } else {
                        $offset++;
                        $secins = "04";
                        $secpol = getPolicyNo($arr[$key + $offset]);
                    }
                }
                $offset++;
                if (checkTextForSSN($arr[$key + $offset])) {
                    $offset++;
                    $ssn = getSSN($arr[$key + $offset]);
                }    
            } elseif (checkTextForSSN($arr[$key + $offset])) {
                $offset++;
                $ssn = getSSN($arr[$key + $offset]);
                //echo "mbi ";
                //echo $arr[$key + $offset] . "\n";
                //echo $arr[$key + 4 + $dob_offset + $med_offset + 1] . "\n";
            }
            $ins = array(
                'primary' => $prins,
                'pripol' => $pripol,
                'secondary' => $secins ?? null,
                'secpol' => $secpol ?? null,
                'ssn' => $ssn ?? null,
                'ins_code' => $ins_code ?? null,
                'ins_pol' => $ins_pol ?? null
            );
            
            $data[$mrn] = array_merge($name, $address, $dob, $ins);
            var_dump($data);
        }
        $prior_key = substr($value, 0, 14);
    }
}
function getMRN($text) {
    $lineArr = explode(",", $text);
    $parts =  preg_split('/\s+/', $lineArr[1]);
    return trim($parts[6]);
}
function getPatientName($text) {
    $lineArr = explode(",", $text);
    $parts = preg_split('/\s+/', $lineArr[0]);
    $count = count($parts);
    if ($count == 3) {
        $last_name = $parts[2];
    }
    elseif ($count == 4) {
        $last_name = $parts[3];
    } elseif ($count == 5) {
        if (strpos($parts[4], '.')) {
            $last_name = $parts[3];

        } else {
            $last_name = $parts[4];
        }
    }

    $parts = preg_split('/\s+/', $lineArr[1]);
    $first_name = $parts[1];
    $mid_init = $parts[2];
    return array(
        'fname' => x12Clean($first_name),
        'mname' => x12Clean($mid_init),
        'lname' => x12Clean($last_name)
    );
}

function getPhoneAndAddress($text) {
    $address_position = strpos($text, 'Address') + 7;
    $sex_position =  strpos($text, 'Sex');
    if (!empty($address_position)) {
        $address_text = trim(substr($text, $address_position, ($sex_position - $address_position)));
        //echo $address_text . "\n";
        $parts = explode(',', $address_text);
        //echo count($parts) . "\n";
        if (count($parts) == '4') {
            $line1 = x12Clean($parts[0]);
            $line2 = '';
            $city = x12Clean($parts[1]);
            $state = x12Clean($parts[2]);
            $zip = x12Zip($parts[3]);
        } elseif (count($parts) == '5') {
            $line1 = x12Clean($parts[0]);
            $line2 = x12Clean($parts[1]);
            $city = x12Clean($parts[2]);
            $state = x12Clean($parts[3]);
            $zip = x12Zip($parts[4]);
        }
        if ($address_position != 7) 
        {
            //echo $line1 . " line1 " . $line2 . " line2 " . $city . " city " . $state . " st " . $zip . " zip\n";
        } else {
            //echo "resident is at 148 Prouty \n";
            $line1 = "148 PROUTY DR";
            $line2 = '';
            $city = 'NEWPORT';
            $state = 'VT';
            $zip = '05855';
        }
    }
    $parts = preg_split('/\s+/', $text);
    //var_dump($parts);
    if (strpos($parts[0], '(') !== false) {
        //echo "pt phone is " . $parts[0] . $parts[1] . "\n";        
        $phone = $parts[0] . $parts[1];
    } else {
        $phone = '';
    }
    return array(
        'street' => x12Clean($line1),
        'street_line_2' => x12Clean($line2),
        'city' => x12Clean($city),
        'state' => x12Clean($state),
        'postal_code' => x12Zip($zip),
        'phone_home' => x12Clean($phone)
    );
}

function checkTextForDOB($text) {
    if (strpos($text, 'English') !== false) {
        return true;
    }
    
    return false;
}

function getDOB($text) {
    $english_position = strpos($text, 'English') + 7;
    $raw_date = substr($text, $english_position + 1, 10) . "\n";
    $YYYY = substr($raw_date, 6, 4);
    $MM = substr($raw_date, 0, 2);
    $DD = substr($raw_date, 3, 2);
    return array('DOB' => $YYYY . '-' . $MM . '-' . $DD);
}

function checkTextForMedicare($text) {
    if (strpos($text, 'Medicare (HIC) #') !== false) {
        return true;
    }

    return false;
}

function getMBI($text) {
    global $filename;
    if (strlen(trim($text)) == 11) {
        return x12Clean($text);
        //echo trim($text) . "\n";
    } else {
        echo "bad mbi " . $filename . "\n";
        return false;
    }
}

function checkTextForMedicaid($text) {
    if (strpos($text, 'Medicaid #') !== false) {
        return true;
    }

    return false;
}

function getPolicyNo($text) {
    return x12Clean($text);
}

function checkTextForSSN($text) {
    if (strpos($text, 'Social Security #') !== false) {
        return true;
    }

    return false;
}

function getSSN($text) {
    return x12Clean(str_replace('-', '', substr($text, 0, 11)));
}

function checkTextForAdmitted($text) {
    if (strpos($text, 'Admitted From') !== false) {
        return true;
    }

    return false;   
}

function checkTextForInsuranceName($text) {
    if (strpos($text, 'Insurance Name') !== false) {
        return true;
    }

    return false;   
}

function getInsuranceCode($text) {
    if ($text == 'BCBS') {
        $code = "074";
    }
    if ($text == 'BCBSFEDERAL') {
        $code = "006";
    }
    if ($text == 'AARP') {
        $code = "932";
    }
    if ($text == 'BANKERSLIFE') {
        $code = "062";
    }
    if ($text == 'UNITEDHEALTHCARE') {
        $code = "062";
    }
    if ($text == 'CHAMPVA') {
        $code = "409";
    }
    if ($text == 'BLUECROSSBLUESHIELD') {
        $code = "006";
    }
    if ($text == 'WELLCARE') {
        $code = "270";
    }

    return x12Clean($code ?? '');
}

function checkTextForMBI($text) {
    if (strpos($text, 'Medicare Beneficiary ID') !== false) {
        return true;
    }

    return false;   
}