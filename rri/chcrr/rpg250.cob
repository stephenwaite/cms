      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rpg250.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RPGACTFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS RPG-ACTNO
             ALTERNATE RECORD KEY IS RPG-GARNO WITH DUPLICATES
             ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL
             STATUS IS GAR-STAT.

           SELECT RPGCHARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS RPGCHARFILE-KEY
             LOCK MODE MANUAL.

           SELECT CHARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
             LOCK MODE MANUAL.

           SELECT CLAIMFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC  RECORD KEY IS CLAIM-KEY
             STATUS IS CLM-STAT
             LOCK MODE MANUAL.

           SELECT WORK249 ASSIGN TO "S55" ORGANIZATION LINE
             SEQUENTIAL.

           SELECT NEWGARNOS ASSIGN TO "S60" ORGANIZATION LINE
             SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  NEWGARNOS.
       01  NEWGARNOS01 PIC X(8).
       FD  WORK249.
       01  WORK24901 PIC X(32).
       FD RPGACTFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS RPGACTFILE01.
       01 RPGACTFILE01.
           02 RPG-ACTNO. 
              03 RPG-ACTNONAME PIC X(24).
              03 RPG-ACTNODOB PIC X(8).
           02 RPG-GARNAME PIC X(24).
           02 RPG-BILLADD PIC X(22).
           02 RPG-STREET PIC X(22).
           02 RPG-CITY PIC X(18).
           02 RPG-STATE PIC X(2).
           02 RPG-ZIP PIC X(9).
           02 RPG-COLLT PIC X.
           02 RPG-PHONE.
             03 RPG-PHONE1 PIC XXX.
             03 RPG-PHONE2 PIC XXX.
             03 RPG-PHONE3 PIC X(4).
           02 RPG-SEX PIC X.
           02 RPG-RELATE PIC X.
           02 RPG-MSTAT PIC X.
           02 RPG-DOB PIC X(8).
           02 RPG-DUNNING PIC X.
           02 RPG-ACCTSTAT PIC X.
           02 RPG-PR-MPLR PIC X(4).
           02 RPG-PRINS PIC XXX.
           02 RPG-PR-ASSIGN PIC X.
           02 RPG-PR-OFFICE PIC X(4).
           02 RPG-PR-GROUP PIC X(10).
           02 RPG-PRIPOL.
             03 RPG-PRIPOL1 PIC X(9).
             03 RPG-PRIPOL2 PIC XXX.
             03 RPG-PR-FILLER PIC X(4).
           02 RPG-PRNAME PIC X(24).
           02 RPG-PR-RELATE PIC X.
           02 RPG-SE-MPLR PIC X(4).
           02 RPG-SEINS PIC XXX.
           02 RPG-SE-ASSIGN PIC X.
           02 RPG-TRINSIND PIC X.
           02 RPG-TRINS PIC XXX.
           02 RPG-SE-GROUP PIC X(10).
           02 RPG-SECPOL.
             03 RPG-SECPOL1 PIC X(9).
             03 RPG-SECPOL2 PIC XXX.
             03 RPG-SE-FILLER PIC X(4).
           02 RPG-SENAME PIC X(24).
           02 RPG-SE-RELATE PIC X.
           02 RPG-INSPEND PIC X(7).
           02 RPG-LASTBILL PIC X(8).
           02 RPG-ASSIGNM PIC X.
           02 RPG-PRIVATE PIC X.
           02 RPG-BILLCYCLE PIC X.
           02 RPG-DELETE PIC X.
           02 RPG-FILLER PIC XXX.
           02 RPG-GARNO PIC X(8).
           02 RPG-PRGRPNAME PIC X(15).
           02 RPG-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.
       FD  RPGCHARFILE
           DATA RECORD IS RPGCHARFILE01.
       01  RPGCHARFILE01.
           02 RPGCHARFILE-KEY.
             03 RPG-KEY8 PIC X(32).
             03 RPG-KEY3 PIC XXX.
           02 RPG-PATID PIC X(8).
           02 RPG-CLAIM PIC X(6).
           02 RPG-SERVICE PIC X.
           02 RPG-DIAG PIC X(7).
           02 RPG-PROC. 
              03 RPG-PROC1 PIC X(4).
              03 RPG-PROC2 PIC X(7).
           02 RPG-MOD2 PIC XX.
           02 RPG-MOD3 PIC XX.
           02 RPG-MOD4 PIC XX.
           02 RPG-AMOUNT PIC X(6).
           02 RPG-DOCR PIC X(3).
           02 RPG-DOCP PIC X(2).
           02 RPG-PAYCODE PIC XXX.
           02 RPG-STAT PIC X.
           02 RPG-WORK PIC XX.
           02 RPG-DAT1 PIC X(8).
           02 RPG-RESULT PIC X.
           02 RPG-ACT PIC X.
           02 RPG-SORCREF PIC X.
           02 RPG-COLLECT PIC X.
           02 RPG-AUTH PIC X.
           02 RPG-PAPER PIC X.
           02 RPG-PLACE PIC X.
           02 RPG-NAME PIC X(24).
           02 RPG-EPSDT PIC X.
           02 RPG-DATE-T PIC X(8).
           02 RPG-DATE-E PIC X(8).
           02 RPG-ORDER PIC X(6).
           02 RPG-DX2 PIC X(7).
           02 RPG-DX3 PIC X(7).
           02 RPG-DATE-A PIC X(8).
           02 RPG-ACC-TYPE PIC X.
           02 RPG-DATE-M PIC X(8).
           02 RPG-ASSIGN PIC X.
           02 RPG-NEIC-ASSIGN PIC X.
           02 RPG-DX4 PIC X(7).
           02 RPG-DX5 PIC X(7).
           02 RPG-DX6 PIC X(7).
           02 RPG-FUTURE PIC X(6).
       
       FD  CHARFILE.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC PIC X(11).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC X(6).
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
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

       FD GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".      

       FD  CLAIMFILE
           DATA RECORD IS CLAIM01.
       01  CLAIM01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).
       WORKING-STORAGE SECTION.
       01  GAR-STAT PIC XX.
       01 CLM-STAT PIC XX.
       01  CHARBACK PIC X(189).
       01  INPUT-DATE-S.      
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE-S.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01 DATE-X PIC X(8).
       01     NAME-CYCLE.
             03 NC1 PIC X.
             03 NC2 PIC X.
             03 FILLER PIC X(22).
       01     YEARDAY.
             03 YEAR-1.
               04 YD1 PIC X.
               04 YD2 PIC X.
             03 DAY3 PIC XXX.
       01 NUM-3 PIC 999.
       01  XYZ PIC 9.
       01  XXX PIC 999 VALUE 0.
       01  GARBACK.
           02 HOLD-GARNO PIC X(8).
           02 FILLER PIC X(301).
       01  CNTR PIC 99 VALUE 0.
       01  CINS.
           02 CINS1 PIC X.
           02 CINS2 PIC XX.
       01  FLAG5999 PIC 9 VALUE 0.
       01  X-COLLT PIC X.
       01  X-DUNNING PIC X.
       01  X-ACCTSTAT PIC X.
       01  X-INSPEND PIC S9(5)V99.
       01  X-LASTBILL PIC X(8).
       01  ACTBK.
           02 ACTBK1 PIC X(24).
           02 ACTBK2 PIC X(315).
           02 ACTBK3 PIC XXX.
       01  CHARBK.
           02 CHARBK1 PIC X(24).
           02 CHARBK2 PIC X(189).
      *
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT RPGCHARFILE WORK249
           OPEN OUTPUT NEWGARNOS
           OPEN I-O RPGACTFILE  GARFILE CHARFILE CLAIMFILE.
           MOVE "A" TO CLAIM-KEY READ CLAIMFILE WITH LOCK INVALID
           DISPLAY "CLAIMFILE IS BAD. THIS PROG. IS TERMINATED"
           GO TO P11.
           IF CLM-STAT = "61" DISPLAY "CLAIMFILE IS BEING WRITTEN"
           DISPLAY "TRY 250 RUN WHEN CLAIMFILE IS FREE" GO TO P11.
       P1. READ WORK249 AT END GO TO P11.
           MOVE WORK24901 TO RPG-ACTNO
           READ RPGACTFILE WITH LOCK INVALID 
           DISPLAY "BAD ACCT " RPG-ACTNO
           GO TO P1.
           IF (RPG-GARNO = SPACE) GO TO P2.
           MOVE RPG-GARNO TO G-GARNO.
       P1-0.
           READ GARFILE WITH LOCK INVALID 
           DISPLAY RPG-GARNO " " RPG-ACTNO
           DISPLAY "A NEW ACCOUNT IS CREATED"
           GO TO P2.
           IF G-DUNNING > "1" GO TO P2.
           IF G-PRINS NOT = RPG-PRINS GO TO P2.
           GO TO WRITE-NEW.
       P2. 
           DISPLAY G-GARNO " " G-GARNAME " NEW ACCOUNT STARTED".
           MOVE RPGACTFILE01 TO ACTBK
           MOVE ACTBK2 TO GARFILE01
           MOVE RPG-ACTNO TO G-ACCT
           ACCEPT YEARDAY FROM DAY.
           MOVE YD2 TO G-PRIVATE.
           MOVE " " TO G-BILLCYCLE.
       P3.
           MOVE SPACE TO G-GARNO.
           MOVE "1" TO G-DELETE
           MOVE GARFILE01 TO GARBACK
           MOVE DAY3 TO ID2 NUM-3.
           MOVE G-GARNAME(1:3) TO ID1.
           MOVE 0 TO XYZ.
           MOVE "G" TO ID3(2:1).
       P4.
           ADD 1 TO XYZ.
           MOVE XYZ  TO ID3(1:1).
           MOVE G-GARNO TO HOLD-GARNO.
           READ GARFILE INVALID KEY GO TO P5.
           IF XYZ = 9 ADD 1 TO NUM-3
           MOVE NUM-3 TO ID2 MOVE 0 TO XYZ. GO TO P4.
       P5.
           MOVE GARBACK TO GARFILE01.
           MOVE HOLD-GARNO TO G-GARNO
           MOVE HOLD-GARNO TO RPG-GARNO
           MOVE G-GARNO TO G-ACCT
           WRITE GARFILE01 INVALID 
           DISPLAY "NO UPDATE " G-GARNO " " RPG-GARNAME
           ACCEPT CINS
           GO TO P1.
           MOVE G-GARNO TO NEWGARNOS01 
           WRITE NEWGARNOS01
           REWRITE RPGACTFILE01.
       P6. MOVE RPG-ACTNO TO RPG-KEY8  
           MOVE SPACE TO RPG-KEY3
           START RPGCHARFILE KEY NOT < RPGCHARFILE-KEY 
           INVALID GO TO P1.
           MOVE 0 TO XXX.
       P7. READ RPGCHARFILE NEXT AT END GO TO P1.
           IF RPG-KEY8 NOT = RPG-ACTNO GO TO P1.
           MOVE RPGCHARFILE01 TO CHARBK
           MOVE CHARBK2 TO CHARFILE01
           MOVE G-GARNO TO CD-KEY8 CD-PATID
           ACCEPT CD-ORDER FROM TIME.
           ADD 1 TO CLAIMNO MOVE CLAIMNO TO CD-CLAIM
           MOVE CHARFILE01 TO CHARBACK.
       P9.
           ADD 1 TO XXX MOVE XXX TO CD-KEY3.
           READ CHARFILE INVALID GO TO P10.
           GO TO P9.
       P10. MOVE CHARBACK TO CHARFILE01 MOVE XXX TO CD-KEY3.
           WRITE CHARFILE01. 
           GO TO P7.
       WRITE-NEW. 
      *     IF NOT (G-PRINS = "003" OR "028") 
            MOVE RPGACTFILE01 TO ACTBK
            MOVE ACTBK2 TO GARFILE01
            MOVE RPG-GARNO TO G-GARNO 
            MOVE RPG-GARNO TO G-ACCT
            REWRITE GARFILE01  
               INVALID DISPLAY "INVALID READ ON REWRITE GARFILE"
               GO TO P2
            END-REWRITE
            DISPLAY "REWRITE GARFILE " G-GARNAME
      *     END-IF.
           GO TO P6.
       P11.
           REWRITE CLAIM01.
           CLOSE CHARFILE GARFILE CLAIMFILE NEWGARNOS
           RPGACTFILE RPGCHARFILE.
           DISPLAY "POSTING PROGRAM HAS ENDED".
           STOP RUN.
