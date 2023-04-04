      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wcomp.
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

           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL STATUS IS GARFILE-STAT.    
           
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
           02 FO-CHARCUR-KEY PIC X(11).
           02 FO-INS-NEIC PIC X(5).
           02 FO-INS-NAME PIC X(22).
           02 FO-INS-STREET PIC X(24).
           02 FO-INS-CITY PIC X(15).
           02 FO-INS-STATE PIC X(2).
           02 FO-INS-ZIP PIC X(9).
           02 FO-G-PRIPOL PIC X(16).
           02 FO-G-GARNAME PIC X(24).
           02 FO-G-GARNO PIC X(8).
           02 FO-CC-DATE-T PIC X(8).
           02 FO-CC-DATE-A PIC X(8).

       FD  INSFILE.
           COPY INSFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARCUR FILEIN INSFILE GARFILE
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

           MOVE CC-KEY8 TO G-GARNO

           READ GARFILE
             INVALID
               DISPLAY G-GARNO
               GO TO P1
           END-READ

           MOVE CC-PAYCODE TO INS-KEY

           READ INSFILE
             INVALID
               DISPLAY FILEIN01
               GO TO P1
           END-READ

           MOVE CHARCUR-KEY TO FO-CHARCUR-KEY
           MOVE INS-NEIC TO FO-INS-NEIC
           MOVE INS-NAME TO FO-INS-NAME
           MOVE INS-STREET TO FO-INS-STREET
           MOVE INS-CITY TO FO-INS-CITY
           MOVE INS-STATE TO FO-INS-STATE
           MOVE INS-ZIP TO FO-INS-ZIP
           MOVE G-PRIPOL TO FO-G-PRIPOL
           MOVE G-GARNAME TO FO-G-GARNAME
           MOVE G-GARNO TO FO-G-GARNO
           MOVE CC-DATE-T TO FO-CC-DATE-T
           MOVE CC-DATE-A TO FO-CC-DATE-A
           


           WRITE FILEOUT01

           GO TO P1.
       P2.
           CLOSE CHARCUR FILEOUT FILEIN INSFILE GARFILE
           STOP RUN.
