      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. change-rpgprocfile-fees.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO   "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARFILE-KEY.
         
           SELECT FILEOUT ASSIGN TO    "S35" ORGANIZATION IS 
               LINE SEQUENTIAL.

           SELECT RPGPROCFILE ASSIGN TO   "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
               LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CHARFILE.
           COPY chafile.CPY IN "C:\Users\sid\cms\copylib\rri". 
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).   

       FD  RPGPROCFILE.
           COPY rpgprocfile.CPY IN "C:\Users\sid\cms\copylib\rri". 

       WORKING-STORAGE SECTION.

       01  CONSTANTS.
           02 AMT PIC S9999999V99 VALUE 0.
           02 NEF-11 PIC ZZ,ZZZ,ZZ9.99CR.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O CHARFILE
           OPEN INPUT  rpgprocfile.
           OPEN OUTPUT FILEOUT.

       P1.
           READ CHARFILE
             AT END
               GO TO P2
           END-READ

           MOVE CD-PROC TO PROC-KEY
           READ rpgprocfile
             INVALID
               DISPLAY "NO RPG PROC ON FILE"
           END-READ
           
           
           IF CD-DATE-T(1:4) NOT = 2022
               AND CD-AMOUNT NOT = RPGPROC-AMOUNT
               STRING "SINCE " CD-DATE-T(1:4) " FOR ACCT " CD-KEY8 
                      " CHANGING FEE TO " RPGPROC-AMOUNT
                      " FOR PROCEDURE " CD-PROC
                 DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE RPGPROC-AMOUNT TO CD-AMOUNT
      *         REWRITE CHARFILE01
           END-IF

           GO TO P1.
       P2.
           CLOSE CHARFILE FILEOUT.
           STOP RUN.
