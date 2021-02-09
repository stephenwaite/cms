<?php

/* measure 137 melanoma: continuity of care
*/


ini_set('max_execution_time', '0');
$ignoreAuth = true;
$_GET['site'] = 'default';
//$argv = $_GET['argv'];
require_once(dirname(__FILE__) . "/../interface/globals.php");
  
$query = "SELECT b.pid, b.encounter, fe.date, b.code FROM billing AS b INNER JOIN form_encounter AS fe USING (encounter) WHERE fe.date > '2019-12-31' AND fe.date < '2021-01-01' AND b.code in ('99201', '99202', '99203', '99204', '99205', '99211', '99212', '99213', '99214', '99215') ORDER BY b.pid";

$result = sqlStatement($query);

while ($row = sqlFetchArray($result)) {
    $query2 = "SELECT b.pid, b.encounter, b.code, fe.date, fe.encounter ".
           "FROM billing AS b " .
           "INNER JOIN form_encounter as fe USING (encounter) " .
           "WHERE b.pid  = ? AND " .
           "code in ('C43.0', 'C43.10', " .
           "'C43.111', 'C43.112', 'C43.121', 'C43.122', 'C43.20', 'C43.21', 'C43.22', 'C43.30', " .
           "'C43.31', 'C43.39', 'C43.4', 'C43.51', 'C43.52', 'C43.59', " .
           "'C43.60', 'C43.61', 'C43.62', 'C43.70', 'C43.71', 'C43.72', " .
           "'C43.8', 'C43.9', 'D03.0', 'D03.10', 'D03.111', 'D03.112', 'D03.121', 'D03.122', " .
           "'D03.20', 'D03.21', 'D03.22', 'D03.30', 'D03.39', 'D03.4', " .
           "'D03.51', 'D03.52', 'D03.59', 'D03.60', 'D03.61', 'D03.62', " .
           "'D03.70', 'D03.71', 'D03.72', 'D03.8', 'D03.9', " .
           "'Z85.820')";

    $result2 = sqlStatement($query2, array($row['pid']));

    while ($row2 = sqlFetchArray($result2)) {     
        echo $row2['pid'] . ", " . $row2['encounter'] . ", " . $row2['code'] . ", " . $row2['date'] . "\n";   
           
        }
        $last_pt  = $row[0];
        $last_enc = $row[1];
    }
