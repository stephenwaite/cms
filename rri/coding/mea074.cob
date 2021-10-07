      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea074.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT CHARFILE ASSIGN TO  "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO   "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CHARFILE
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC.
              03 CD-PROC0 PIC X(4).
              03 CD-PROC1 PIC X(5).
              03 CD-PROC2 PIC XX.
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-REC-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AUTH PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-ESPDT PIC X.
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
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).     

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O CHARFILE. 
           OPEN OUTPUT FILEOUT.

           MOVE SPACE TO CHARFILE-KEY.          

       P0.
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P2
           END-START.    

       P1. 
           READ CHARFILE NEXT WITH LOCK
             AT END
               GO TO P2
           END-READ          

           IF CD-PAYCODE NOT = "074" 
               GO TO P1
           END-IF

           IF CD-MOD2 = "ME" OR "MG"
             STRING "REMOVED AUC MOD " CD-MOD2 " " CHARFILE-KEY
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01  
             MOVE SPACE TO CD-MOD2
             REWRITE CHARFILE01
           END-IF

           IF CD-MOD3 = "ME" OR "MG"
             STRING "REMOVED AUC MOD " CD-MOD3 " " CHARFILE-KEY
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             MOVE SPACE TO CD-MOD3
             REWRITE CHARFILE01
           END-IF

           IF CD-MOD4 = "ME" OR "MG"
             STRING "REMOVED AUC MOD " CD-MOD4 " " CHARFILE-KEY
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             MOVE SPACE TO CD-MOD4
             REWRITE CHARFILE01
           END-IF

           GO TO P1.
                                 
       P2.
           CLOSE CHARFILE FILEOUT.
           STOP RUN.
