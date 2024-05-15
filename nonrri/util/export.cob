      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. export.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARDATE ASSIGN TO "S25"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT PAYDATE ASSIGN TO "S30"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CCPROCIN ASSIGN TO "S45"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT DOCFILE ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO.

           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.

           SELECT INSFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
               ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
               LOCK MODE MANUAL.  

       DATA DIVISION.

       FILE SECTION.

       FD  PAYCUR.
           COPY PAYCUR.CPY  IN "C:\Users\sid\cms\copylib".      
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib".             
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib".      
      
       FD  DOCFILE.
       01  DOCFILE01.
           02 DOC-KEY.
             03 DOC-INS PIC XXX.
             03 DOC-NUM PIC XX.
           02 DOC-FEDID PIC X(14).
           02 DOC-PVNUM PIC X(14).
           02 DOC-UPIN PIC X(6).
           02 DOC-NPI PIC X(10).
           02 DOC-IND PIC X.
           02 DOC-GROUP PIC X(14).
           02 DOC-NPIGROUP PIC X(10).
           02 DOC-NAME PIC X(24).
           02 DOC-GROUPNAME PIC X(29).
           02 DOC-SSNUM PIC X(9).
           02 DOC-TAXONOMY PIC X(10).
           02 DOC-NEIC PIC X(5).
           02 DOC-TAXGROUP PIC X(10).
           02 DOC-WEBTAX PIC XXX.
           02 DOC-FUTURE PIC X(12).  

       FD  INSFILE.
           COPY INSFILE.CPY IN "C:\Users\sid\cms\copylib".      

       FD  CHARDATE.
       01  CHARDATE01. 
           02 LOW-CHARDATE PIC X(8).
           02 HIGH-CHARDATE PIC X(8).               
       
       FD  PAYDATE.
       01  PAYDATE01. 
           02 LOW-PAYDATE PIC X(8).
           02 HIGH-PAYDATE PIC X(8).

       FD  CCPROCIN.
       01  CCPROCIN01 PIC X(11).
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(500).

       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).

       01  MON-TAB-RE01.
           02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
           02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
           02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
           02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       
       01  MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
     
       01  PLINDX PIC 99 VALUE 0.
       01  LOW-CLAIM PIC X(6).
       01  HIGH-CLAIM PIC X(6).
       01  X PIC 99.
       01  Y PIC 99.
       01  CC-PL PIC X.
       01  FLAG PIC 9.
       01  TOT-AMOUNT PIC S9(7)V99.
       01  HOLD-CHARCUR01 PIC X(156).
       01  G-FIRST PIC X(20).
       01  G-LAST PIC X(20).
       01  G-MIDDLE PIC X(10).
       01  G-PRIFIRST PIC X(20).
       01  G-PRILAST PIC X(20).
       01  G-PRIMIDDLE PIC X(10).
       01  G-SECFIRST PIC X(20).
       01  G-SECLAST PIC X(20).
       01  G-SECMIDDLE PIC X(10).
       01  W-PRINSNAME PIC X(22).
       01  W-SEINSNAME PIC X(22).
       01  W-TRINSNAME PIC X(22).
       01  W-PRINSNEIC PIC X(5).       
       01  W-SEINSNEIC PIC X(5).
       01  W-TRINSNEIC PIC X(5).   
       01  W-PR-RELATE PIC X(6).
       01  W-SE-RELATE PIC X(6).    
       01  W-TR-RELATE PIC X(6).

      *     COPY charback.CPY IN "C:\Users\sid\cms\copylib\rri".      
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT DOCFILE GARFILE CHARDATE PAYDATE CHARCUR PAYCUR
             CCPROCIN INSFILE.
           OPEN OUTPUT FILEOUT.
           READ CHARDATE.
      *     READ PAYDATE.
      *     READ DOCFILE AT END GO TO P1.
           MOVE SPACE TO CHARCUR-KEY
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO P99.
   
       P1. 
           READ CHARCUR
             AT END
               GO TO P99.

           IF CC-DATE-T < LOW-CHARDATE
             GO TO P1.

           MOVE CHARCUR01 TO HOLD-CHARCUR01.

       WRITE-FO. 
           IF CC-PATID = G-GARNO GO TO P1.
           
           MOVE HOLD-CHARCUR01(1:8) TO G-GARNO
           READ GARFILE 
               INVALID 
                   MOVE SPACE TO G-GARNAME.

      *     STRING G-GARNAME DELIMITED BY ";" INTO G-LAST G-FIRST 
      *       G-MIDDLE.

           MOVE G-PRINS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY "WHAT THE HECK".

           UNSTRING G-GARNAME DELIMITED BY ";" INTO G-LAST G-FIRST
             G-MIDDLE.

           UNSTRING G-PRNAME DELIMITED BY ";" INTO G-PRILAST G-PRIFIRST
             G-PRIMIDDLE.      

           IF (G-PR-RELATE = "2" AND G-SEX = "M") OR
              (G-PR-RELATE = "K" AND G-SEX = "F") OR
              (G-PR-RELATE = SPACE) OR
              (G-PRINS = "001")
              MOVE "SELF" TO W-PR-RELATE
           ELSE
              MOVE "SPOUSE" TO W-PR-RELATE
           END-IF
           
           MOVE INS-NAME TO W-PRINSNAME
           MOVE INS-NEIC TO W-PRINSNEIC

           MOVE G-SEINS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY "WHAT THE HECK".

           UNSTRING G-SENAME DELIMITED BY ";" INTO G-SECLAST G-SECFIRST
             G-SECMIDDLE.      

           IF (G-SE-RELATE = "2" AND G-SEX = "M") OR
              (G-SE-RELATE = "K" AND G-SEX = "F") OR
              (G-SE-RELATE = SPACE) OR
              (G-SEINS = "001")
              MOVE "SELF" TO W-SE-RELATE
           ELSE
              MOVE "SPOUSE" TO W-SE-RELATE
           END-IF      
           
           MOVE INS-NAME TO W-SEINSNAME
           MOVE INS-NEIC TO W-SEINSNEIC
           
           IF G-TRINS = "000" OR "001"
             MOVE SPACE TO W-TRINSNAME W-TRINSNEIC
           ELSE 
             MOVE G-TRINS TO INS-KEY
             READ INSFILE
               INVALID
                 DISPLAY "WHAT THE HECK"
             END-READ
             MOVE INS-NAME TO W-TRINSNAME
             MOVE INS-NEIC TO W-TRINSNEIC
           END-IF

           STRING g-garno "," G-LAST "," G-FIRST "," G-MIDDLE ","
             G-BILLADD "," G-STREET "," G-CITY "," G-STATE "," G-ZIP ","
             G-PHONE "," G-SEX "," G-DOB "," 
             W-PRINSNAME "," W-PRINSNEIC "," W-PR-RELATE "," 
             G-PRIPOL "," G-PR-GROUP "," 
             G-PRILAST "," G-PRIFIRST "," G-PRIMIDDLE "," 
             W-SEINSNAME "," W-SEINSNEIC "," W-SE-RELATE "," 
             G-SECPOL "," G-SE-GROUP "," 
             G-SECLAST "," G-SECFIRST "," G-SECMIDDLE "," 
             W-TRINSNAME "," W-TRINSNEIC "," HOLD-CHARCUR01(80:8) 
             DELIMITED BY SIZE INTO FILEOUT01.
   
           
           WRITE FILEOUT01.

           GO TO P1.

       P99. 
           CLOSE DOCFILE GARFILE CHARDATE PAYDATE CHARCUR
             PAYCUR CCPROCIN FILEOUT INSFILE.
           STOP RUN.
