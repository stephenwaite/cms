      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npi078.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INSIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT FILE-OUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
       
           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT DOCFILE ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT GARFILE ASSIGN TO "S60"     ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM         RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
       
           SELECT FILEIN   ASSIGN TO "S65" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT INSFILE ASSIGN TO "S70"    ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT PLACEFILE ASSIGN TO "S75" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT ERRORFILE ASSIGN TO "S80" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT FILEOUT2 ASSIGN TO "S85" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT AUTHFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL. 

       DATA DIVISION.
       FILE SECTION.
       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
       FD  ERRORFILE.
       01  ERRORFILE01.
           02 EF1 PIC X(12).
           02 EF2 PIC X(23).
           02 EF3 PIC X(12).
           02 EF4 PIC X(10).
           02 EF5 PIC X(24).
       FD  INSFILE
     *     BLOCK CONTAINS 6 RECORDS
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
       
       FD  FILEIN.
       01  FILEIN01 PIC 999.
       
       FD GARFILE
           DATA RECORD IS GARFILE01.
       01 GARFILE01.
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

        FD  DOCFILE.
        01  DOCFILE01.
            02 DF-1 PIC 99.
            02 DF-2 PIC 99.
      
       FD FILEOUT.
       01 FILEOUT01 PIC X(161).
      
       FD FILEOUT2.
       01 FILEOUT201 PIC X(161).

       FD  PAYCUR
           DATA RECORD IS PAYCUR01.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).
 
       FD  INSIN
           DATA RECORD IS INSIN01.
       01  INSIN01.
           02 INS-1 PIC 999.
           02 INS-2 PIC XX.
 
       FD FILE-OUT.
       01  FILE-OUT01.
           02 FO-PC PIC XXX.
           02 FO-PATID PIC X(8).
           02 FILEOUT-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOCP PIC XX.
           02 FO-PAPER PIC X.
 
       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC XXX.
           02 CC-DOCP PIC 99.
           02 CC-PAYCODE PIC 999.
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

       FD  AUTHFILE
           DATA RECORD IS AUTHFILE01.
       01  AUTHFILE01.
           02 AUTH-KEY.
              03 AUTH-KEY8 PIC X(8).
              03 AUTH-KEY6 PIC X(6).
           02 AUTH-NUM PIC X(15).
           02 AUTH-QNTY PIC XX.
           02 AUTH-DATE-E PIC X(8).
           02 AUTH-FILLER PIC X(41).           
 
       WORKING-STORAGE SECTION.
       01  INSTAB01.
           02 INSTAB PIC 99 OCCURS 999 TIMES.
      *
       01  DOCTAB01.
           02 DOCTAB PIC 99 OCCURS 20 TIMES.
       01  NUM3 PIC 999.
       01  PLINDX PIC 99 VALUE 0.
       01  CC-PL PIC X.
       01  X PIC 999.
       01  Y PIC 99.
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 29 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
             03 PL-STREET PIC X(18).
             03 PL-CITY PIC X(15).
             03 PL-STATE PIC XX.
             03 PL-ZIP PIC X(9).
       01  ALF11 PIC X(11).

       PROCEDURE DIVISION.

       P0.
           OPEN OUTPUT FILE-OUT FILEOUT FILEOUT2 ERRORFILE.
           
           OPEN INPUT PAYCUR INSIN CHARCUR DOCFILE GARFILE
             FILEIN INSFILE PLACEFILE AUTHFILE.

           MOVE SPACE TO ERRORFILE01
           MOVE "  COMMERCIAL ELECTRONIC CLAIMS ERRORS      "
            TO ERRORFILE01
           WRITE ERRORFILE01
           MOVE SPACE TO ERRORFILE01.

           PERFORM A4 VARYING X FROM 1 BY 1 UNTIL X = 999.
           PERFORM A3 VARYING X FROM 1 BY 1 UNTIL X > 20.

       Z00.
           READ PLACEFILE AT END GO TO PZ.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           GO TO Z00.

       PZ. 
           READ DOCFILE AT END GO TO P00.
           MOVE DF-2 TO DOCTAB(DF-1) GO TO PZ.

       P00.
           READ INSIN AT END GO TO P000.
           MOVE INS-2 TO INSTAB(INS-1) GO TO P00.

       P000.
           MOVE 100 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P6.
           
       P1. 
           READ CHARCUR NEXT 
             AT END 
               GO TO P6.
           
           IF CC-PAYCODE > 199 GO TO P6.
           
           PERFORM A1 THRU A2 GO TO P1.

       A1.
           MOVE CHARCUR-KEY TO EF1
           IF CC-PROC = "1      " OR "2       " GO TO A2.

           IF CC-PROC < "00100  " GO TO A2.

           IF CC-AMOUNT = 0 GO TO A2.

           IF CC-REC-STAT > "1" GO TO A2.

           IF DOCTAB(CC-DOCP) = 99 
           OR INSTAB(CC-PAYCODE) = 99 GO TO A2.

           IF INSTAB(CC-PAYCODE) NOT = 0
            MOVE INSTAB(CC-PAYCODE) TO CC-DOCP.

           IF DOCTAB(CC-DOCP) NOT = 0
            MOVE DOCTAB(CC-DOCP) TO CC-DOCP.

           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID
           MOVE "BAD GARNO           " TO EF2
           MOVE SPACE TO EF3 EF5 PERFORM E1 GO TO A2.

           IF CC-DIAG  = "0000000" 
             MOVE "MISSING DIAG." TO EF2
             MOVE G-GARNAME TO EF5
             MOVE CC-PROC TO EF3
             PERFORM E1
             GO TO A2.

           MOVE G-GARNAME TO EF5
           MOVE G-PRINS TO NUM3
           
           IF G-STREET = SPACE AND G-BILLADD = SPACE
             MOVE "ADDRESS IS BLANK" TO EF2
             MOVE G-GARNAME TO EF5
             MOVE CC-KEY8 TO EF3
             PERFORM E1
             GO TO A2.

           IF (NUM3 NOT = CC-PAYCODE) AND (CC-PAPER = "E")
             MOVE "P" TO CC-PAPER.

           IF CC-PAYCODE = 153 OR "122" OR "123" MOVE "P" TO CC-PAPER.

           IF CC-PAPER = "E" GO TO A1-1.

           PERFORM PAPER-1 
           GO TO A2.

       A1-1.
           MOVE CC-PAYCODE TO INS-KEY
           READ INSFILE
             INVALID
              GO TO A2
           END-READ

           IF INS-NEIC = SPACE DISPLAY CHARCUR01 GO TO A2.
           
           IF INS-CITY = SPACE OR INS-STREET = SPACE
             OR INS-STATE = SPACE OR INS-ZIP = SPACE
             MOVE SPACE TO EF1 EF2
             MOVE CC-PAYCODE TO EF1
             MOVE  "NO INS. ADDRESS " TO EF2
             PERFORM E1
             GO TO A2
           END-IF

           PERFORM DF-SEARCH

           IF INS-NEIC = "57106" AND CC-DATE-M = "00000000"
             AND CC-PL = "3"
             MOVE "ADMIT DATE - TRICARE " TO EF2 
             MOVE G-GARNAME TO EF5 
             PERFORM E1
             GO TO A2.

           IF (INS-NEIC = "VACCN")
               MOVE CC-KEY8 TO AUTH-KEY8
               MOVE CC-CLAIM TO AUTH-KEY6
               READ AUTHFILE                  
                 INVALID 
                   MOVE "NO AUTH FOR " TO EF2
                   PERFORM E1
                   GO TO A2
               END-READ
           END-IF      

           IF INS-NEIC = SPACE
             MOVE "NO NEIC CODE PRESENT" TO EF2
             MOVE INS-KEY TO EF3
             PERFORM E1
             GO TO A2.

           IF G-PRIPOL = SPACE
             MOVE "POLICY NUMBER MISSING" TO EF2
             MOVE G-PRINS TO EF3
             PERFORM E1
             GO TO A2.

           IF G-PRIPOL = ZEROES
             MOVE "POLICY CANT BE 0" TO EF2
             MOVE G-PRIPOL TO EF3
             PERFORM E1
             GO TO A2.

           IF INS-NEIC = "23742"
             PERFORM PAPER-1
             GO TO P1.

           IF G-PR-GROUP = G-PRIPOL
             MOVE "GRP & POLICY ARE =" TO EF2
             MOVE G-GARNAME TO EF5
             MOVE G-PR-GROUP TO EF3
             PERFORM E1
             GO TO A2.

      *     IF (INS-NEIC = "14165") 
      *     MOVE G-PRIPOL TO ALF11
      *     IF ALF11 NOT NUMERIC
      *     MOVE "BAD POLICY NUMBER " TO EF2
      *     PERFORM E1 GO TO A2.
           MOVE SPACE TO FILEOUT01
           STRING CHARCUR01 INS-NEIC DELIMITED BY SIZE INTO FILEOUT01
           
           IF INS-NEIC = "14165" 
             WRITE FILEOUT201 FROM FILEOUT01
           ELSE
             WRITE FILEOUT01
           END-IF
           
           GO TO A2.

       PAPER-1.
           MOVE CC-PAYCODE TO FO-PC.
           MOVE CC-PATID TO  FO-PATID 
           MOVE CHARCUR-KEY TO FILEOUT-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PAPER TO FO-PAPER
           WRITE FILE-OUT01.

       A2.
           EXIT.

       E1.
           MOVE CC-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO EF4
           WRITE ERRORFILE01.

       DF-SEARCH.
           MOVE CC-PLACE TO CC-PL.
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.

       DF-SEARCH2. 
           IF CC-PLACE = PL-TAB(Y)
             MOVE PL-NUM(Y) TO CC-PL
             MOVE PLINDX TO Y.

       A4. 
           MOVE 0 TO INSTAB(X).

       A3. 
           MOVE 0 TO DOCTAB(X).

       S4. 
           MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO S4-EXIT.

       S7. 
           READ PAYCUR NEXT AT END GO TO S4-EXIT.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S4-EXIT.
           IF PC-CLAIM NOT = CC-CLAIM GO TO S7.
           ADD PC-AMOUNT TO CC-AMOUNT GO TO S7.

       S4-EXIT. 
           EXIT.

       P6. 
           READ FILEIN AT END GO TO P9.

           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P6.

       P7. 
           READ CHARCUR NEXT AT END GO TO P6.

           IF CC-PAYCODE NOT = FILEIN01 GO TO P6.
           PERFORM A1 THRU A2 GO TO P7.

       P9.
           CLOSE FILE-OUT FILEOUT FILEOUT2 ERRORFILE
             PAYCUR INSIN CHARCUR DOCFILE GARFILE FILEIN INSFILE
             PLACEFILE AUTHFILE.

           STOP RUN.
