      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr254.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO   "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARFILE-KEY.

           SELECT CHARFILEBK ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILEBK-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO    "S40" ORGANIZATION IS 
               LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.

           SELECT EMAILAUTHFILE ASSIGN TO "S50"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC RECORD KEY IS EA-KEY
               ALTERNATE RECORD KEY IS EA-MEDREC WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-EMAIL WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-AUTH WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-DATE-E WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-SSN WITH DUPLICATES
               LOCK MODE MANUAL.                

       DATA DIVISION.

       FILE SECTION.
  
      *  THIS IS CHARNEW FROM THE LOAD/CODING

       FD  CHARFILE.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC0 PIC X(4).
           02 CD-PROC1 PIC X(7).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AGE PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-EPSDT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-QP1 PIC XX.
           02 CD-QP2 PIC XX.
           02 CD-DX5-3 PIC X(3).
      *    Grabbing 6 from unused DX6     
           02 CD-AUTH PIC 9(6).
           02 CD-DX6 PIC X.
           02 CD-CLINICAL PIC X(40).
           02 CD-ADMIT-DIAG PIC X(30).

      * THIS BECOMES CHARFILE IN AC     

       FD  CHARFILEBK.
       01  CHARFILEBK01.
           02 CHARFILEBK-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
      * 25     
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC0 PIC X(4).
           02 CD-PROC1 PIC X(7).
      * 44     
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
      * 50     
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
      * 65     
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
      * 75     
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AGE PIC X.
      * 80     
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
      * 106     
           02 CD-EPSDT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
      * 151     
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
      * 160     
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-QP1 PIC XX.
           02 CD-QP2 PIC XX.
           02 CD-DX5-3 PIC X(3).
      * 166     
      *    taking 6 from unused DX6 for auth  
           02 CD-AUTH PIC 9(6).    
           02 CD-DX6 PIC X.
           02 CD-FUTURE PIC X(6).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).   

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  EMAILAUTHFILE.
           copy emailauthfile.cpy in "c:\users\sid\cms\copylib\rri".           


       WORKING-STORAGE SECTION.

       01  CONSTANTS.
           02 AMT PIC S9999999V99 VALUE 0.
           02 NEF-11 PIC ZZ,ZZZ,ZZ9.99CR.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT CHARFILE GARFILE EMAILAUTHFILE
           OPEN I-O CHARFILEBK
           OPEN OUTPUT FILEOUT.

       P1.
           READ CHARFILE
             AT END
               GO TO P2
           END-READ

           ADD CD-AMOUNT OF CHARFILE01 TO AMT
           MOVE CORR CHARFILE01 TO CHARFILEBK01
           MOVE SPACE TO CD-FUTURE
           MOVE CHARFILE-KEY TO CHARFILEBK-KEY
      
      *    PET scans need mod Q0 and Z006 as 2nd DX?
      *    is this for all CPT 78815 and 78816?

           IF (CD-PROC0 OF CHARFILEBK01 = "4080" OR "4082" OR "4087")
      *         MOVE "Q0" TO CD-MOD2 OF CHARFILEBK01
      *         MOVE "Z006   " TO CD-DX2 OF CHARFILEBK01
               STRING "FOR ACCT " CD-KEY8 OF CHARFILEBK01 
                   " used to add PET SCAN MOD Q0 AND DX Z006 " 
                   " rrmc sent mod of " CD-MOD2 OF CHARFILEBK01     
                      " FOR DATE " CD-DATE-T OF CHARFILEBK01
                      " PROCEDURE " CD-PROC1 OF CHARFILEBK01
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF

           perform LM-1 thru LM-1-EXIT.

           WRITE CHARFILEBK01 
           GO TO P1.

       LM-1.
           move CD-KEY8 OF CHARFILE01 to g-garno
           READ GARFILE
             invalid
               display "this shouldn't happen"
               GO TO LM-1-EXIT.  
           
           MOVE G-ACCT TO EA-MEDREC
           START EMAILAUTHFILE KEY NOT > EA-MEDREC
             INVALID 
               DISPLAY "NO AUTHS ON FILE FOR " G-GARNO " ON " CD-DATE-T
                 OF CHARFILE01  
               GO TO LM-1-EXIT.

       LM-2.
           READ EMAILAUTHFILE NEXT
             AT END 
               GO TO LM-1-EXIT.
           
           IF EA-MEDREC NOT = G-ACCT GO TO LM-1-EXIT.

           IF EA-DATE-E NOT = (CD-DATE-T of CHARFILE01) GO TO LM-2.

           IF EA-AUTH = SPACE GO TO LM-2.

           move space to FILEOUT01.

           STRING "PLACING AUTH " EA-AUTH " ON " CHARFILEBK-KEY 
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01

           MOVE EA-KEY TO CD-AUTH OF CHARFILEBK01.    
                      
       LM-1-EXIT.           
           EXIT.

       P2.
           CLOSE CHARFILE CHARFILEBK FILEOUT GARFILE EMAILAUTHFILE.
           STOP RUN.
