      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri224.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ORDFILE ASSIGN TO   "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ORDNO
               ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES.
       
           SELECT ORDFILEBK ASSIGN TO "S35" ORGANIZATION LINE
               SEQUENTIAL.
       
       DATA DIVISION.

       FILE SECTION.

       FD ORDFILE
           DATA RECORD IS ORDFILE01.
       01 ORDFILE01.
           02 ORDNO.
             03 ORD8 PIC X(8).
             03 ORD3 PIC XXX.
           02 CHARGE1 PIC X(4).
           02 CHARGE2 PIC X.
           02 C-REF PIC XXX.
           02 C-IOPAT PIC X.
           02 C-DATE-A PIC X(8).
           02 C-DATE-T PIC X(8).
           02 C-DATE-ADMIT PIC X(8).
           02 C-ORDER PIC XXXX.
           02 C-CLINICAL PIC X(40).
           02 C-ADMIT-DIAG PIC X(30).
           02 C-DATE-E PIC X(8).
           02 C-CPT PIC X(5).

       FD ORDFILEBK
           DATA RECORD IS ORDFILEBK01.
       01 ORDFILEBK01.
           02 ORDNOBK.
             03 ORD8BK PIC X(8).
             03 ORD3BK PIC XXX.
           02 CHARGEBK1 PIC X(4).
           02 CHARGEBK2 PIC X.
           02 C-REFBK PIC XXX.
           02 C-IOPATBK PIC X.
           02 C-DATE-ABK PIC X(8).
           02 C-DATE-TBK PIC X(8).
           02 C-DATE-ADMITBK PIC X(8).
           02 C-ORDERBK PIC XXXX.
           02 C-CLINICALBK PIC X(40).
           02 C-ADMIT-DIAGBK PIC X(30).
           02 C-DATEBK-E PIC X(8).
           02 C-CPTBK PIC X(5).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT ORDFILE
           OPEN OUTPUT ORDFILEBK.
           MOVE SPACE TO ORDNO
           
           START ORDFILE KEY > ORDNO
             INVALID
               GO TO P2
           END-START.

       P1. 
           READ ORDFILE NEXT
             AT END
               GO TO P2
           END-READ
    
           IF CHARGE1 = "1131" OR "1321" OR "1838"
           OR "1911" OR "1950" OR "1951" OR "1956" OR "1958" OR "1959"
           OR "1985"  OR "4052" OR "0700" OR "0502" OR "0518" 
           OR "1408" OR "1853"  OR "1400" OR "1401" OR "0901"
           OR "0531" OR "0532" OR "0533" OR "0534" OR "0544" OR "8818"
           OR "0749" OR "0750" OR "0751" OR "0752"
           GO TO A1.

           IF CHARGE1 = "0535" OR
           "0902" OR "0501" OR "0505" OR "0508" OR "0509" OR "0507"
           OR "0503" OR "0701" OR "0702" OR "0504" OR "0512" OR "0519"
           OR "0500" OR "0600" OR "0514" OR "0515" OR "0506" OR "0525"
           OR "0516" OR "6002" OR "0513" OR "0517" OR "0706" OR "0809"
           OR "0802" OR "0707" OR "0900" OR "0800" OR "0510" OR "0810"
           OR "0705" OR "0522" OR "0511" OR "5101" OR "0903" OR "0528"
           OR "0709" OR "0808" OR "0813" OR "0805" OR "0530" OR "0804"
           OR "0812" OR "0803" OR "0807" OR "4051" OR "0811" OR "0818"
           GO TO A1.

           IF CHARGE1 = "7405" OR "7406" OR "7601" OR "7602" OR "1616"
           OR "4622" OR "4623" OR "4470" OR "4472" OR "7003"
           OR "4510" OR "4421" OR "4626" OR "4627" OR "2310" OR "7001"
           OR "7506" OR "7513"
           GO TO A1.

           IF CHARGE1 = "8284" OR "8296" OR "8303" OR "8304" 
           GO TO A1.

           GO TO P1.
       A1.
           WRITE ORDFILEBK01 FROM ORDFILE01
           
           GO TO P1.

       P2.
           CLOSE ORDFILEBK ORDFILE.
           STOP RUN.
