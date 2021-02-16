      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. one803.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT AGEDATE ASSIGN TO "S25"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO  "S35" ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION LINE SEQUENTIAL.

           SELECT INSFILE ASSIGN TO "S45" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
               ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
             LOCK MODE MANUAL.    

           SELECT MIPSOUT ASSIGN TO "S55" ORGANIZATION LINE SEQUENTIAL.
  

       DATA DIVISION.
       FILE SECTION.
       
       FD  CHARCUR.
       copy charcur.cpy in "c:\users\sid\cms\copylib".
       
       FD  INSFILE.
       copy insfile.cpy in "c:\users\sid\cms\copylib".
           
       FD  FILEIN.
       01  FILEIN01. 
           02 FI-1 PIC X(5).      

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(156).
           02 FO-2 PIC X(5).

       FD  AGEDATE.
       01  AGEDATE01.
           02 DATE-LOW PIC X(8).
           02 DATE-HIGH PIC X(8).      

       FD  GARFILE.
       copy garfile.cpy in "c:\users\sid\cms\copylib".

       FD  MIPSOUT.
       01  MIPSOUT01.       
           02 KEY-OUT PIC X(11).
           02 PROV-FNAME PIC X(20).
           02 PROV-LNAME PIC X(20).
           02 PAT-FNAME PIC X(20).
           02 PAT-LNAME PIC X(20).
           02 PAT-DOB PIC X(20).
           02 PAT-SEX PIC X(20).
           02 DATE1 PIC X(20).
           02 DESC  PIC X(20).
           02 F-ICD PIC X(11).
           02 ICD10 PIC X(20).
           02 F-PROC PIC X(7).

       WORKING-STORAGE SECTION. 

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT AGEDATE CHARCUR FILEIN INSFILE garfile
           OPEN OUTPUT FILEOUT MIPSOUT.

           READ AGEDATE.

       P1. 
           MOVE SPACE TO FILEIN01
           
           READ FILEIN 
             AT END
               GO TO P9
           END-READ. 

           MOVE LOW-VALUES TO CHARCUR-KEY
           START CHARCUR KEY > CHARCUR-KEY
             INVALID 
               CONTINUE.      

       P2.               
           READ CHARCUR NEXT             
             AT END  
               GO TO P1
           END-READ

           IF CC-DATE-T < DATE-LOW OR > DATE-HIGH
              GO TO P2.

           IF CC-PROC(1:5) NOT = FI-1(1:5)
             GO TO P2.   

           MOVE CC-PAYCODE TO INS-KEY

           READ INSFILE
             INVALID
               DISPLAY "PAYCODE NOT IN INSFILE? " CC-PAYCODE " "
                   CHARCUR01 
               ACCEPT OMITTED
               CONTINUE
           END-READ

           MOVE CHARCUR01 TO FO-1
           MOVE INS-NEIC TO FO-2.

           WRITE FILEOUT01

           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE 
             INVALID 
               DISPLAY G-GARNO " HAS NO ACCT"
               ACCEPT ALF1
               GO TO P2
           END-READ.

           MOVE SPACE TO PROV-FNAME PROV-LNAME
           MOVE "DANIEL" TO PROV-FNAME
           MOVE "MCCAULIFFE" TO PROV-LNAME
           MOVE SPACE TO PAT-LNAME PAT-FNAME

           IF CC-DOCP = "02"
             MOVE "KERRY" TO PROV-FNAME
             MOVE "LANE" TO PROV-LNAME.

           UNSTRING G-GARNAME DELIMITED BY ";"
             INTO PAT-LNAME PAT-FNAME ALF1
           
           MOVE SPACE TO PAT-DOB
           STRING G-DOB(5:2) "/" G-DOB(7:2) "/" G-DOB(1:4)
           DELIMITED BY SIZE INTO PAT-DOB
           MOVE SPACE TO PAT-SEX
           MOVE "MALE  " TO PAT-SEX
           IF G-SEX = "F"
           MOVE "FEMALE" TO PAT-SEX.
           MOVE DX-0 TO DIAG-KEY
           READ DIAGFILE INVALID 
             MOVE DX-0 TO ICD10
            NOT INVALID
             MOVE DX-0 TO ICD10
           END-READ
           MOVE SPACE TO DATE1
           STRING CC-DATE-T(5:2) "/" CC-DATE-T(7:2) "/" CC-DATE-T(1:4)
           DELIMITED BY SIZE INTO DATE1
           MOVE CHARCUR-KEY TO KEY-OUT
           MOVE "ICD10" TO DESC
           MOVE CC-PROC TO F-PROC
           MOVE "5050F  " TO F-ICD
           WRITE MIPSOUT01

           GO TO P2.
           
       P9.
           CLOSE AGEDATE CHARCUR FILEOUT FILEIN INSFILE GARFILE MIPSOUT
           STOP RUN.
