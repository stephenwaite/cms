      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. find-cdm.
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
             ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           
           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.

       DATA DIVISION.

       FILE SECTION.

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib\rri".      
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".             
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".      
      
       FD  DOCFILE.
           COPY DOCFILE.CPY IN "C:\Users\sid\cms\copylib\rri".  

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
       01  FILEOUT01.
           02 FO-DOCP PIC XX.
           02 FO-DUM PIC X.
           02 FO-SERVICE PIC X.
           02 FO-PROC PIC X(11).
           02 FO-DIAG PIC X(7).
           02 FO-AMOUNT PIC S9(7)V99.
           02 FO-IO PIC X.
           02 FO-NAME PIC X(24).
           02 FO-DATE PIC X(8).
           02 FO-CKEY PIC X(12).
           02 FO-MSG PIC X(20).

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

      *     COPY charback.CPY IN "C:\Users\sid\cms\copylib\rri".      
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT DOCFILE GARFILE CHARDATE PAYDATE CHARCUR PAYCUR
             CCPROCIN.
           OPEN OUTPUT FILEOUT.
           READ CCPROCIN.
           READ CHARDATE.
      *     READ PAYDATE.
      *     READ DOCFILE AT END GO TO P1.
   
       P1. 
           READ CHARCUR
             AT END
               GO TO P99.

           IF CC-PROC NOT = CCPROCIN01 
             GO TO P1.                      
           
           IF CC-DATE-T < LOW-CHARDATE OR > HIGH-CHARDATE
             GO TO P1.           

      *     perform DF-SEARCH thru DF-SEARCH-EXIT

      *     DISPLAY "hit THORACENTESIS "
      *       CC-PROC(1:4)
      *     accept OMITTED                            

       WRITE-FO. 
           MOVE SPACE TO FILEOUT01.
           MOVE CC-PATID TO G-GARNO
           READ GARFILE 
             INVALID 
               MOVE SPACE TO G-GARNAME.

           MOVE G-GARNAME TO FO-NAME.           

           MOVE "1" TO FO-SERVICE.
           MOVE "1" TO FO-DUM.
           MOVE "1" TO FO-IO.
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PROC TO FO-PROC
           MOVE CC-DATE-T TO FO-DATE
           MOVE CC-DIAG TO FO-DIAG

      *     MOVE TOT-AMOUNT TO FO-AMOUNT
           
           MOVE G-GARNAME TO FO-NAME
           STRING " " CHARCUR-KEY INTO FO-CKEY

           IF CC-AMOUNT = 0 
             STRING " CHARGE ZEROED " INTO FO-MSG
           END-IF
           WRITE FILEOUT01.

           GO TO P1.

       P99. 
           CLOSE DOCFILE GARFILE CHARDATE PAYDATE CHARCUR
             PAYCUR CCPROCIN FILEOUT.
           STOP RUN.
