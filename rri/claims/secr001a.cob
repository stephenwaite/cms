      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. secr001a.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
             LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
             LINE SEQUENTIAL.

           SELECT MPLRFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY.

           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT FILEOUT2 ASSIGN TO "S55" ORGANIZATION
             LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT2.
       01  FILEOUT201 PIC X(160).

       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(11).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AUTH PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).

       FD FILEIN.
       01 FILEIN01.
           02 FI-INS PIC X(3).
           02 FI-PATID PIC X(8).
           02 FI-KEY8 PIC X(8).
           02 FI-FILLER PIC X(31).
           02 FI-PAPER PIC X.

       FD FILEOUT.
       01 FILEOUT01. 
           02 FO-1 PIC X(51).
           02 FO-SORTFIELD PIC X(9).

       FD  MPLRFILE.
       01  MPLRFILE01.
           02 MPLR-KEY PIC X(8). 
           02 MPLR-NAME PIC X(22).
           02 MPLR-STREET PIC X(24).
           02 MPLR-CITY PIC X(15).
           02 MPLR-STATE PIC XX.
           02 MPLR-ZIP PIC X(9).
           02 MPLR-CLAIMNO PIC X(15).
           02 MPLR-TRINS PIC XXX.
           02 MPLR-TR-ASSIGN PIC X.
           02 MPLR-TR-GROUP PIC X(12).
           02 MPLR-TRIPOL PIC X(14).
           02 MPLR-TR-NAME PIC X(24).
           02 MPLR-TR-RELATE PIC X.
           02 MPLR-FUTURE PIC X(6).
       
       FD GARFILE.
       01 GARFILE01.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP. 
              03 GZIP5 PIC X(5).
              03 GZIP4 PIC X(4).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB.
              03 G-DOBYY PIC X(4). 
              03 G-DOBMM PIC XX.
              03 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL PIC X(14).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-ADDRCODE PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL PIC X(14).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).

       WORKING-STORAGE SECTION.

       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  ALF8 PIC X(8).
       01  ALF24 PIC X(24).
       01  ALF25 PIC X(24).

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARCUR GARFILE MPLRFILE FILEIN 
           OPEN OUTPUT FILEOUT FILEOUT2.

       P1.  
           
           READ FILEIN
             AT END
               GO TO P9.

           IF FI-PAPER = "E"
             MOVE FILEIN01(12:11) TO CHARCUR-KEY
             READ CHARCUR
               INVALID
                 GO TO PX
             END-READ
             WRITE FILEOUT201 FROM CHARCUR01
             GO TO P1
           END-IF.

       PX.     
           MOVE FILEIN01 TO FO-1
           MOVE FI-KEY8 TO G-GARNO
           READ GARFILE
             INVALID
               DISPLAY "BAD " FI-KEY8 
               GO TO P1.

           MOVE "T" TO FO-1(51:1).
           IF FI-INS = "004" OR "281"
             MOVE "O" TO FO-1(51:1)
             MOVE SPACE TO FO-SORTFIELD
             GO TO P2.

           IF FI-INS = G-PRINS MOVE "P" TO FO-1(51:1).
           
           IF FI-INS = G-SEINS MOVE "S" TO FO-1(51:1).
           
           IF FI-PAPER = "O" MOVE FI-PAPER TO FO-1(51:1).
           
           MOVE SPACE TO FO-SORTFIELD
           
           IF FI-INS = "062" MOVE G-PR-GROUP TO FO-SORTFIELD.
           
           IF FI-INS NOT = "091" GO TO P2.
           
           MOVE SPACE TO FO-SORTFIELD
           
           IF G-ADDRCODE NOT = SPACE
             MOVE G-ADDRCODE TO FO-SORTFIELD.
             MOVE G-GARNO TO MPLR-KEY
             READ MPLRFILE
               INVALID
                 GO TO P2.

           IF MPLR-NAME = SPACE GO TO P2.
           
           MOVE G-GARNO TO FO-SORTFIELD.

       P2.
           WRITE FILEOUT01
           GO TO P1.

       P9. 
           CLOSE FILEOUT FILEOUT2 CHARCUR GARFILE MPLRFILE FILEIN
           STOP RUN.
