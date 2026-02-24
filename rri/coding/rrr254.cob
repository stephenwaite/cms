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

           SELECT CHARNEW ASSIGN TO   "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARNEW-KEY.

           SELECT CHARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO    "S40" ORGANIZATION IS 
               LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
  
       FD  CHARNEW.
           COPY "charnew.cpy" IN "C:\Users\sid\cms\copylib\rri".

       FD  CHARFILE.
           COPY "charfile.cpy" IN "C:\Users\sid\cms\copylib\rri".
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).   

       WORKING-STORAGE SECTION.

       01  CONSTANTS.
           02 AMT PIC S9999999V99 VALUE 0.
           02 NEF-11 PIC ZZ,ZZZ,ZZ9.99CR.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT CHARNEW
           OPEN I-O CHARFILE
           OPEN OUTPUT FILEOUT.

       P1.
           READ CHARNEW
             AT END
               GO TO P2
           END-READ

           ADD CD-AMOUNT OF CHARNEW01 TO AMT
           MOVE CORR CHARNEW01 TO CHARFILE01
           MOVE SPACE TO CD-FUTURE
           MOVE CHARNEW-KEY TO CHARFILE-KEY
      
      *    PET scans need mod Q0 and Z006 as 2nd DX?
      *    is this for all CPT 78815 and 78816?

           IF (CD-PROC0 OF CHARFILE01 = "4080" OR "4082" OR "4087")
      *         MOVE "Q0" TO CD-MOD2 OF CHARFILE01
      *         MOVE "Z006   " TO CD-DX2 OF CHARFILE01
               STRING "FOR ACCT " CD-KEY8 OF CHARFILE01 
                   " used to add PET SCAN MOD Q0 AND DX Z006 " 
                   " rrmc sent mod of " CD-MOD2 OF CHARFILE01     
                      " FOR DATE " CD-DATE-T OF CHARFILE01
                      " PROCEDURE " CD-PROC1 OF CHARFILE01
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF

           WRITE CHARFILE01 
           GO TO P1.
       P2.
           CLOSE CHARNEW CHARFILE FILEOUT.
           STOP RUN.
