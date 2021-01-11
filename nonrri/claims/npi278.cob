      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npi278.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE-OUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARFILE-KEY.
           SELECT PAYFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY.
           SELECT FILEOUT ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT DOCFILE ASSIGN TO "S55"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO.
           SELECT FILEIN ASSIGN TO "S65"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT2 ASSIGN TO "S75"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INSFILE
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01  G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB PIC X(8).
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.

       FD  CHARFILE
      *    BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC PIC X(7).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC 99.
           02 CD-PAYCODE PIC 999.
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

       FD  PAYFILE
      *    BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS PAYFILE01.
       01  PAYFILE01.
           02 PAYFILE-KEY.
             03 PD-KEY8 PIC X(8).
             03 PD-KEY3 PIC XXX.
           02 PD-NAME PIC X(24).
           02 PD-AMOUNT PIC S9(4)V99.
           02 PD-PAYCODE PIC XXX.
           02 PD-DENIAL PIC XX.
           02 PD-CLAIM PIC X(6).
           02 PD-DATE-T PIC X(8).
           02 PD-DATE-E PIC X(8).
           02 PD-ORDER PIC X(6).
           02 PD-BATCH PIC X(6).
        FD  DOCFILE.
        01  DOCFILE01.
            02 DF-1 PIC 99.
            02 DF-2 PIC 99.
       FD FILEIN.
       01 FILEIN01. 
          02 FI-1 PIC 999.
          02 FI-2 PIC X.
       FD FILEOUT.
       01 FILEOUT01 PIC X(190).
       FD FILEOUT2.
       01 FILEOUT201 PIC X(190).

       FD  INSIN
           DATA RECORD IS INSIN01.
       01  INSIN01.
           02 INS-1 PIC 999.
           02 INS-2 PIC XX.
       FD FILE-OUT
           DATA RECORD IS FILE-OUT01.
       01  FILE-OUT01.
           02 FO-PC PIC XXX.
           02 FO-PATID PIC X(8).
           02 FILEOUT-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
       WORKING-STORAGE SECTION.
       01  X USAGE IS INDEX.
       01  INSTAB01.
           02 INSTAB PIC 99 OCCURS 999 TIMES.
       01  PAYTAB01.
           02 PAYTAB PIC 999 OCCURS 999 TIMES.

      *
       01  DOCTAB01.
           02 DOCTAB PIC 99 OCCURS 90 TIMES.
       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT FILE-OUT FILEOUT FILEOUT2.
           OPEN INPUT PAYFILE INSIN CHARFILE DOCFILE GARFILE
             INSFILE FILEIN.
           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X > 999.
           PERFORM A2 VARYING X FROM 1 BY 1 UNTIL X > 90.
           MOVE SPACE TO CHARFILE-KEY
           START CHARFILE KEY > CHARFILE-KEY INVALID GO TO P6.

       PZ. 
           READ DOCFILE AT END GO TO P00.
           MOVE DF-2 TO DOCTAB(DF-1) GO TO PZ.

       P00. 
           READ INSIN AT END GO TO P000.
           MOVE INS-2 TO INSTAB(INS-1)
           GO TO P00.

       P000. 
           READ FILEIN AT END GO TO P1.
           MOVE 1 TO PAYTAB(FI-1)
           IF FI-2 = "x" MOVE 2 TO PAYTAB(FI-1).
           GO TO P000.

       P1. 
           READ CHARFILE NEXT AT END GO TO P6.

           IF CD-PAYCODE = 122 OR 123 GO TO P1.

           IF PAYTAB(CD-PAYCODE) = "2" GO TO P1.

           IF (CD-PAYCODE > 99 AND < 200) 
           OR (PAYTAB(CD-PAYCODE) = 1) NEXT SENTENCE
           ELSE GO TO P1.

           IF (CD-AMOUNT = 0) AND (CD-PAYCODE NOT = "141") GO TO P1.

           IF CD-PROC = "1      " OR "2       " GO TO P1.

           IF CD-PROC < "00100  " GO TO P1.

           IF CD-DIAG = "0000000" GO TO P1.

           IF CD-STAT > "1" GO TO P1.

           IF CD-PAPER NOT = "E" GO TO P1.

           IF INSTAB(CD-PAYCODE) NOT = 0
           MOVE INSTAB(CD-PAYCODE) TO CD-DOCP.

           IF DOCTAB(CD-DOCP) = 99 
           OR INSTAB(CD-PAYCODE) = 99 GO TO P1.

           IF DOCTAB(CD-DOCP) NOT = 0
           MOVE DOCTAB(CD-DOCP) TO CD-DOCP.

           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE 
             INVALID
               DISPLAY "BAD BAD BAD BAD " G-GARNO
               GO TO P1
           END-READ    

           IF G-STREET = SPACE AND G-BILLADD = SPACE
             DISPLAY "NO ADDRESS " G-GARNAME
              GO TO P1.

           IF CD-PAYCODE NOT = G-PRINS GO TO P1.

           IF G-PRINS = 003 OR 028 GO TO P1.

           MOVE CD-PAYCODE TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY "BAD " INS-KEY
               GO TO P1
           END-READ    

           IF INS-NEIC = SPACE
             DISPLAY "NO NEIC CODE " INS-KEY
             GO TO P1
           END-IF  

           IF INS-NEIC =  "VACCN"
               MOVE SPACE TO FILEOUT01
               STRING CD-KEY8 " " INS-NEIC " chance to add real auth "
                 "before send all 9s with posted charge" 
                   DELIMITED BY SIZE INTO FILEOUT01
               GO TO P1   
           END-IF

           IF INS-CITY = SPACE OR INS-STREET = SPACE
             OR INS-STATE = SPACE OR INS-ZIP = SPACE
             DISPLAY "NO ADDRESS " INS-KEY
              GO TO P1.

           MOVE SPACE TO FILEOUT01
           STRING CHARFILE01 INS-NEIC DELIMITED BY SIZE INTO FILEOUT01
           IF INS-NEIC = "14165" 
            WRITE FILEOUT201 FROM FILEOUT01
           ELSE
            WRITE FILEOUT01
           END-IF
            GO TO P1.
       A1. MOVE 0 TO INSTAB(X) PAYTAB(X).
       A2. MOVE 0 TO DOCTAB(X).
       S4. MOVE CD-KEY8 TO PD-KEY8.
           MOVE "000" TO PD-KEY3.
           START PAYFILE KEY > PAYFILE-KEY INVALID GO TO S4-EXIT.
       S7. READ PAYFILE NEXT AT END GO TO S4-EXIT.
           IF PD-KEY8 NOT = CD-KEY8 GO TO S4-EXIT.
           IF PD-CLAIM NOT = CD-CLAIM GO TO S7.
           ADD PD-AMOUNT TO CD-AMOUNT GO TO S7.
       S4-EXIT. EXIT.
       P6. CLOSE FILE-OUT FILEOUT FILEOUT2 GARFILE CHARFILE INSFILE
           PAYFILE. STOP RUN.
