      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npi046.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PAPEROUT ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT DIAGFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GAPFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT ERRORFILE ASSIGN TO "S70" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PROCFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT PLACEFILE ASSIGN TO "S80" ORGANIZATION 
            LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S85" ORGANIZATION 
            LINE SEQUENTIAL.
           SELECT PROVCAID ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROV-KEY
           ALTERNATE RECORD KEY IS PROV-NPI WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-TAX WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-NAME WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT AUTHFILE ASSIGN TO "S95" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  AUTHFILE
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS AUTHFILE01.
       01  AUTHFILE01.
           02 AUTH-KEY.
              03 AUTH-KEY8 PIC X(8).
              03 AUTH-KEY6 PIC X(6).
           02 AUTH-NUM PIC X(15).
           02 AUTH-QNTY PIC XX.
           02 AUTH-DATE-E PIC X(8).
           02 AUTH-NDC PIC X(11).
           02 AUTH-FILLER PIC X(30).
       FD  PROVCAID.
       01  PROVCAID01.
           02 PROV-KEY PIC X(7).
           02 PROV-NAME PIC X(24).
           02 PROV-NPI PIC X(10).
           02 PROV-TAX PIC X(10).
           02 PROV-STREET PIC X(20).
           02 PROV-CITY PIC X(20).
           02 PROV-STATE PIC XX.
           02 PROV-ZIP PIC X(5).
       FD  PARMFILE.
       01  PARMFILE01 PIC X(7).
       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
       
       FD  PROCFILE
           DATA RECORD IS PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY PIC X(7).
           02 PROC-NDC PIC X.
           02 PROC-OLD PIC X(6).
           02 PROC-TYPE PIC X.
           02 PROC-BCBS PIC X(4).
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC S9(4)V99.
           02 CARE-AMOUNT PIC S9(4)V99.
       
       FD  ERRORFILE
           DATA RECORD IS ERRORFILE01.
       01  ERRORFILE01.
           02 EF1 PIC X(12).
           02 EF2 PIC X(37).
           02 EF3 PIC X(24).
       
       FD  REFPHY
      *    BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-NPI PIC X(10).
       FD  GAPFILE.
       01  GAPFILE01.
           02 GAPKEY PIC X(7).
           02 GAP-NAME PIC X(25).
           02 GAP-ADDR PIC X(22).
           02 GAP-CITY PIC X(15).
           02 GAP-STATE PIC XX.
           02 GAP-ZIP PIC X(9).
           02 GAP-TYPE PIC X.
           02 GAP-FUTURE PIC X(40).
       FD  DIAGFILE
           BLOCK CONTAINS 15 RECORDS
           DATA RECORD IS DIAG01.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).
       
       FD  GARFILE
           BLOCK CONTAINS 3 RECORDS
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
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC X(4).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL0.
             03 G-SECPOL PIC X(9).
             03 G-SE-SUFX PIC XXX.
             03 G-SE-FILLER PIC X(4).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       FD  PAPEROUT.
       01  PAPEROUT01.
           02 FO-PC PIC 999.
           02 FO-PATID.
             03 FO-PATID7 PIC X(7).
             03 FO-PATID8 PIC X.
           02 FO-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOC PIC XX.
           02 FO-PAPER PIC X.
       FD  FILEIN.
       01  FILEIN01 PIC XXX.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(156).
       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
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
           02 CC-AGE PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-IOPAT PIC X.
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
       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 29 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
             03 PL-STREET PIC X(18).
             03 PL-CITY PIC X(15).
             03 PL-STATE PIC XX.
             03 PL-ZIP PIC X(9).
       01  PLINDX PIC 99 VALUE 0.
       01  DIAGFLAG PIC 9.
       01  ALF7 PIC X(7).
       01  FLAG PIC 9.
       01  Y PIC 99.
       01  NAMEFIRST PIC X(24).
       01  NAMELAST PIC X(24).

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PLACEFILE FILEIN REFPHY GARFILE AUTHFILE
           GAPFILE DIAGFILE PROCFILE PARMFILE PROVCAID.
           OPEN OUTPUT PAPEROUT FILEOUT ERRORFILE.
           MOVE "MEDICAID ELECTRONIC CLAIMS ERRORS" TO ERRORFILE01
           WRITE ERRORFILE01
           MOVE SPACE TO ERRORFILE01.
           
           OPEN I-O CHARCUR.
           READ PARMFILE AT END GO TO P00.
       P00. READ PLACEFILE AT END GO TO P0.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           GO TO P00.
       
       P0. READ FILEIN AT END GO TO P6.
           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P0.

       P1. 
           READ CHARCUR NEXT WITH LOCK AT END GO TO P0.
           IF CC-PAYCODE NOT = FILEIN01 GO TO P0.
           IF CC-PROC < "00100  "
           OR CC-CLAIM = "999995"
           OR CC-REC-STAT > "1"
           GO TO P1.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID MOVE "NO GARNO" TO EF2
           PERFORM S1 
           GO TO P1.
           IF (G-PRINS = CC-PAYCODE)
             IF NOT ((G-PRIPOL NOT NUMERIC) AND (G-PRIPOL NOT = SPACE))
                MOVE CHARCUR-KEY TO EF1
                MOVE "BAD POLICY " TO EF2
                PERFORM S1
                GO TO P1
             END-IF
           END-IF.
           IF (G-BILLADD = SPACE) AND (G-STREET = SPACE)
           MOVE CHARCUR-KEY TO EF1 MOVE "STREET ADDRESS ?" TO EF2
           PERFORM S1
           GO TO P1.
           IF G-DOB = "00000000"
           MOVE CHARCUR-KEY TO EF1 MOVE "NO D.O.B.   " TO EF2
           PERFORM S1
           GO TO P1.
           IF NOT ((G-PRINS = FILEIN01)
           OR (G-SEINS = FILEIN01)
           OR (G-TRINS = FILEIN01))
           MOVE CHARCUR-KEY TO EF1 
           MOVE "NO MATCH OF INSURANCE" TO EF2
           PERFORM S1
           GO TO P1.
           
           IF (CC-PROC1 = "54150")
           AND (PARMFILE01 = "0009114") 
           MOVE "O" TO CC-PAPER
           REWRITE CHARCUR01
           PERFORM PAPER-1
           GO TO P1.
           IF (CC-PROC1 = "53899" OR "54400" OR "55250" OR "54150" 
            OR "56301" OR "56302" OR "56307" OR "56308"  
            OR "58150" OR "58180" OR "58200" OR "58210" OR "58260"
            OR "58262" OR "58263" OR "58267" OR "58285" OR "58290"
            OR "58291" OR "58550" OR "58552" OR "58553"  OR "58600" 
            OR "58605" OR "58611" OR "58670" OR "58982" OR "58150" 
            OR "58152" OR "58983" OR "99070" OR "99360" OR "90799"
            OR "58563" OR "58565" OR "A4560" OR "A4560" OR "J9999")
            OR (CC-PROC = "90699ZM" OR "J7300  " OR "58670  ")
            OR ((CC-PROC = "90782  ") AND (CC-AMOUNT <  4.00))
             MOVE "O" TO CC-PAPER
             REWRITE CHARCUR01
             PERFORM PAPER-1
           GO TO P1.
           IF CC-PAPER = "O" OR "P" PERFORM PAPER-1 GO TO P1.
           IF (G-TRINS  = FILEIN01) PERFORM PAPER-1 GO TO P1.
           IF (G-SEINS = FILEIN01)
             IF (FILEIN01 = "004" OR "064")
                GO TO P1
              ELSE PERFORM PAPER-1 GO TO P1
             END-IF
           END-IF.
           
       TEST-IT.
           IF CC-DIAG = "0000000" MOVE "NO DIAG" TO EF2 PERFORM S1
           GO TO P1.
           MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE "OLD DIAG CODE" TO EF2
           MOVE CC-DIAG TO EF3 PERFORM S1 
           GO TO P1.
           MOVE 0 TO DIAGFLAG
           IF CC-DX2 NOT = "0000000" MOVE CC-DX2 TO ALF7
           MOVE 0 TO DIAGFLAG
           PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 MOVE "OLD DX2 CODE" TO EF2 PERFORM S1
           GO TO P1.
           IF CC-DX3 NOT = "0000000" MOVE CC-DX3 TO ALF7
           MOVE 0 TO DIAGFLAG
           PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 MOVE "OLD DX3 CODE" TO EF2 PERFORM S1
           GO TO P1.
           IF CC-DX4 NOT = "0000000" MOVE CC-DX4 TO ALF7
           MOVE 0 TO DIAGFLAG
           PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 MOVE "OLD DX4 CODE" TO EF2 PERFORM S1
           GO TO P1.
           IF (CC-PROC1 > "99240" AND < "99280")
           AND (CC-DOCR = "000")
           MOVE "CONSULT NEEDS REF MD" TO EF2 
           PERFORM S1 
           GO TO P1.
           IF CC-DOCR = "000" GO TO TEST-IT2.
           MOVE CC-DOCR TO REF-KEY
           READ REFPHY INVALID 
            STRING CC-DOCR " IS AN INVALID REF-KEY" DELIMITED BY "**" 
            INTO EF2
            PERFORM S1 
            GO TO P1
           END-READ.
           IF REF-NPI = SPACE
            MOVE SPACE TO EF2
            STRING CC-DOCR " HAS NO NPI" DELIMITED BY "**" 
            INTO EF2
            PERFORM S1 
            GO TO P1
           END-IF.
      *     MOVE REF-CDNUM TO PROV-KEY
      *     READ PROVCAID INVALID
      *      MOVE SPACE TO EF2
      *      STRING CC-DOCR " HAS NO LEGACY CAID NUM" DELIMITED BY "**" 
      *      INTO EF2
      *      PERFORM S1 
      *      GO TO P1
      *     END-READ.

           MOVE SPACE TO NAMELAST NAMEFIRST
           UNSTRING REF-NAME DELIMITED BY 
           "; " OR ";" OR " ; " OR " ," OR ", " OR " , " OR ","   
           INTO NAMELAST NAMEFIRST
            IF NAMEFIRST = SPACE
            STRING CC-DOCR  " " REF-NAME " NAME FORMAT ERROR" 
            DELIMITED BY "!!" INTO EF2 
            PERFORM S1
            GO TO P1.
       TEST-IT2.
           IF (CC-PROC1 = "11719" OR "11055" OR "11056" OR "11057"
           OR "G0127") AND (CC-DATE-M = ZEROES)
           MOVE SPACE TO EF2
           STRING CC-PROC " NO LAST SEEN DATE" DELIMITED BY "!!"
           INTO EF2 PERFORM S1 
           GO TO P1.
           
           MOVE CC-PROC TO PROC-KEY.
           READ PROCFILE INVALID
           MOVE "INVALID PROCEDURE CODE" TO EF2 PERFORM S1 GO TO P1.
           IF PROC-NDC = "1"
            MOVE CC-KEY8 TO AUTH-KEY8
            MOVE CC-CLAIM TO AUTH-KEY6
            READ AUTHFILE INVALID MOVE SPACE TO AUTH-NDC
            END-READ
            IF AUTH-NDC = SPACE
             MOVE SPACE TO EF2
             STRING CC-PROC " NDC # NEEDED" DELIMITED BY SIZE INTO EF2
             PERFORM S1 GO TO P1.

           WRITE FILEOUT01 FROM CHARCUR01 
           GO TO P1.

       DF-SEARCH2.  
           IF CC-PLACE = PL-TAB(Y) 
           AND PL-NUM(Y) = "3"
           AND CC-DATE-M = ZEROES
           MOVE 1 TO FLAG
           MOVE PLINDX TO Y.
       GAP-1.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID MOVE 1 TO FLAG 
           MOVE SPACE TO EF2
           STRING CC-KEY8 " BAD ACCOUNT #" DELIMITED BY "//"
           INTO EF2
           PERFORM S1 
           GO TO GAP-1-EXIT.
           
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE INVALID 
           MOVE SPACE TO EF2
           STRING G-GARNO " INVALID MEDIGAPE CODE" DELIMITED BY "//"
           INTO EF2
           PERFORM S1 
           MOVE 1 TO FLAG GO TO GAP-1-EXIT.
           IF GAP-TYPE = "X" OR "Y"
           MOVE 1 TO FLAG.
       GAP-1-EXIT.  EXIT.
       DIAG-CHECK.
           MOVE 0 TO DIAGFLAG
           MOVE ALF7 TO DIAG-KEY
           READ DIAGFILE INVALID MOVE 1 TO DIAGFLAG.

       S1. MOVE CHARCUR-KEY TO EF1 MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID MOVE SPACE TO G-GARNAME.
           MOVE G-GARNAME TO EF3
           WRITE ERRORFILE01.
       PAPER-1.
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-PATID TO FO-PATID
           MOVE CHARCUR-KEY TO FO-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN.
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOC 
           MOVE CC-PAPER TO FO-PAPER
           WRITE PAPEROUT01.

       P6.
           CLOSE FILEOUT PAPEROUT ERRORFILE.
           CLOSE GARFILE DIAGFILE REFPHY GAPFILE PROCFILE
           STOP RUN.
