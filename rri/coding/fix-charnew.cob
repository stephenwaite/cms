      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. fix-charnew.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARNEW ASSIGN TO   "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARNEW-KEY.
         

           SELECT FILEOUT ASSIGN TO    "S35" ORGANIZATION IS 
               LINE SEQUENTIAL.

           SELECT PROCFILE ASSIGN TO   "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
               LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CHARNEW.
           COPY charnew.CPY IN "C:\Users\sid\cms\copylib\rri". 
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).   

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib\rri". 

       WORKING-STORAGE SECTION.

       01  CONSTANTS.
           02 AMT PIC S9999999V99 VALUE 0.
           02 NEF-11 PIC ZZ,ZZZ,ZZ9.99CR.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O CHARNEW
           OPEN OUTPUT FILEOUT.

       P1.
           READ CHARNEW
             AT END
               GO TO P2
           END-READ
               

           DISPLAY CD-DATE-T " CD-DATE-T".
           
           IF CD-DATE-T(1:4) NOT = 2022
               STRING "SINCE " CD-DATE-T(1:4) " FOR ACCT " CD-KEY8 
                      " CHANGING FEE TO FOR PROCEDURE " CD-PROC1
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01

           END-IF

      *     REWRITE CHARNEW01
           GO TO P1.
       P2.
           CLOSE CHARNEW FILEOUT.
           STOP RUN.
