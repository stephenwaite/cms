      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sssr803.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT FILEIN ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
       
           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT INSFILE ASSIGN TO "S45" ORGANIZATION INDEXED
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
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".
       

       FD  FILEIN.
       01  FILEIN01. 
           02 FI-1 PIC X(11).
           02 FILLER PIC X(160).

       FD  FILEOUT.
       01  FILEOUT01. 
           02 FO-1 PIC X(160).
           02 FO-2 PIC X(5).

       FD  INSFILE.
           COPY insfile.cpy IN "C:\Users\sid\cms\copylib\rri".

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARCUR FILEIN INSFILE
           OPEN OUTPUT FILEOUT.
       P1. 
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P2
           END-READ

           MOVE FI-1 TO CHARCUR-KEY
           
           READ CHARCUR
             INVALID
               DISPLAY FILEIN01
               GO TO P1
           END-READ

           MOVE CC-PAYCODE TO INS-KEY

           READ INSFILE
             INVALID
               DISPLAY FILEIN01
               GO TO P1
           END-READ

           MOVE CHARCUR01 TO FO-1
           MOVE INS-NEIC TO FO-2.

           WRITE FILEOUT01

           GO TO P1.
       P2.
           CLOSE CHARCUR FILEOUT FILEIN INSFILE
           STOP RUN.
