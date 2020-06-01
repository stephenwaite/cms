       IDENTIFICATION DIVISION.
       PROGRAM-ID. mcc222.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TAGDIAG ASSIGN TO "S20" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TAG-KEY
           ALTERNATE RECORD KEY IS TAG-ICD9 WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO.
           SELECT GAPFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S125"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S220"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT ICD10FILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS ICD10-KEY
           ALTERNATE RECORD KEY IS ICD10-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  TAGDIAG.
       01  TAGDIAG01.
           02 TAG-KEY.
              03 TAG-7 PIC X(7).
              03 TAG-5 PIC X(5).
           02 TAG-ICD9.
              03 tag-icd9-5 PIC X(5).
              03 tag-icd9-7 PIC X(7).
       FD  ICD10FILE.
       01  ICD10FILE01.
           02 ICD10-KEY PIC X(7).
           02 ICD10-TITLE PIC X(61).
           02 ICD10-MEDB PIC X(5).
       FD  PARMFILE.
       01  PARMFILE01.
           02 PM1 PIC XX.
           02 FILLER PIC X(5).
           02 PM-NAME PIC X(20).
           
       FD  FILEOUT.
       01 FILEOUT01 PIC X(80).
       FD GAPFILE.
       01 GAPFILE01.
           02 GAPKEY PIC X(7).
           02 GAP-NAME PIC X(25).
           02 GAP-ADDR PIC X(22).
           02 GAP-CITY PIC X(15).
           02 GAP-STATE PIC XX.
           02 GAP-ZIP PIC X(9).
           02 GAP-TYPE PIC X.
           02 GAP-FUTURE PIC X(40).
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
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE.
             03 G-PHONE1 PIC XXX.
             03 G-PHONE2 PIC XXX.
             03 G-PHONE3 PIC XXXX.
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB.
             03 G-DOBCC PIC XX.
             03 G-DOBYY PIC XX.
             03 G-DOBMM PIC XX.
             03 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL. 
              03 G-PRIPOL1 PIC X(9).
              03 G-PRIPOL2 PIC XXX.
              03 FILLER PIC X(4).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL. 
              03 G-SECPOL1 PIC X(9).
              03 G-SECPOL2 PIC XXX.
              03 FILLER PIC X(4).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       WORKING-STORAGE SECTION.
       01  PM-UPIN PIC X(6).
       01  DOCTAB01.
           02 DOCTAB02 OCCURS 20 TIMES.
              03 DOCNAME PIC X(20).
       01  X-RELATE PIC X.
       01  PRIFLAG PIC 9.
       01  SECFLAG PIC 9.
       01  XTAB01.
           02 XT PIC X OCCURS 9 TIMES.

       01 LINE-13.
           02 FILLER PIC X(37) VALUE SPACE.
           02 L13-DATE PIC X(8).
           02 FILLER PIC X(12) VALUE SPACE.
           02 L13-HR PIC XX.
           02 L13-AM PIC X.
           
       01 LINE-14.
           02 FILLER PIC X VALUE SPACE.
           02 L14-LNAME PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 L14-FNAME PIC X(15).
           02 FILLER PIC X VALUE SPACE.
           02 L14-M PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L14-F PIC X.
           02 FILLER PIC X VALUE SPACE.
           02 L14-DOBMM PIC XX.
           02 FILLER PIC X VALUE "-".
           02 L14-DOBDD PIC XX.
           02 FILLER PIC X VALUE "-".
           02 L14-DOBYY PIC XXXX.
           02 FILLER PIC X VALUE SPACE.
           02 L14-PHONE1 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L14-PHONE2 PIC XXX.
           02 L14-SPACE PIC X.
           02 L14-PHONE3 PIC X(4).
       01 LINE-18.
          02 FILLER PIC X(23) VALUE SPACE.
          02 L18-STREET PIC X(22).
          02 FILLER PIC X VALUE SPACE.
          02 L18-CITY PIC X(15).
          02 FILLER PIC X VALUE SPACE.
          02 L18-STATE PIC XX.
          02 FILLER PIC X VALUE SPACE.
          02 L18-ZIP PIC X(9).
          02 FILLER PIC X VALUE SPACE.
       01 LINE-20.
          02 FILLER PIC X VALUE SPACE.
          02 L20-UPIN PIC X(7).
          02 FILLER PIC X VALUE SPACE.
          02 L20-NAME PIC X(20).
          02 FILLER PIC X(15) VALUE SPACE.
          02 FO-DX1 PIC X(7).
          02 FILLER PIC X(8) VALUE SPACE.
          02 FO-DX2 PIC X(7).
          02 FILLER PIC X(5) VALUE SPACE.
          02 FO-DX3 PIC X(7).
          02 FILLER PIC X(2) VALUE SPACE.
      *    02 FO-DX4 PIC X(7).
      *    02 FILLER PIC X(2) VALUE SPACE.

       01 LINE-23.
          02 FILLER PIC X(9) VALUE SPACE.
          02 L23-PS PIC X(9).
          02 FILLER PIC X(8) VALUE SPACE.
          02 L23-D1 PIC X.
          02 FILLER PIC X VALUE SPACE.
          02 L23-D2 PIC X.
          02 FILLER PIC X VALUE SPACE.
          02 L23-D3 PIC X.
          02 FILLER PIC XX VALUE SPACE.
          02 L23-D4 PIC X.
          02 FILLER PIC X VALUE SPACE.
          02 L23-D5 PIC X.
          02 FILLER PIC XXX VALUE SPACE.
          02 L23-D6 PIC X.
          02 FILLER PIC X VALUE SPACE.
          02 L23-D7 PIC X.
          02 FILLER PIC X VALUE SPACE.
          02 L23-D8 PIC X.
          02 FILLER PIC X VALUE SPACE.
          02 L23-D9 PIC X.
          02 FILLER PIC XXXX VALUE SPACE.
          02 L23-SUFX PIC XXX.
          02 FILLER PIC XX VALUE SPACE.
          02 L23-TRAVELERS  PIC X(16).
          02 FILLER PIC X(9) VALUE SPACE.
       01 LINE-27.
          02 FILLER PIC X(9) VALUE SPACE.
          02 L27-INSNAME PIC X(17).
          02 FILLER PIC X VALUE SPACE.
          02 L27-POLICY PIC X(16).
          02 FILLER PIC XX VALUE SPACE.
          02 L27-GROUP PIC X(9).
          02 FILLER PIC X VALUE SPACE.
          02 L27-NAME PIC X(15).
          02 FILLER PIC X VALUE SPACE.
          02 L27-RELATE PIC X(9).
       01 LINE-29.
          02 FILLER PIC X(9) VALUE SPACE.
          02 L29-PS PIC X(11).
          02 FILLER PIC X(6) VALUE SPACE.
          02 L29-STREET PIC X(18).
          02 FILLER PIC XX VALUE SPACE.
          02 L29-CITY PIC X(15).
          02 FILLER PIC X VALUE SPACE.
          02 L29-STATE PIC XX.
          02 FILLER PIC X(9) VALUE SPACE.
          02 L29-ZIP PIC X(5).
          02 FILLER PIC XX VALUE SPACE.
       01  LINE-30.
           02 FILLER PIC X VALUE SPACE.
           02 L30-GAPKEY PIC X(7).
           02 FILLER PIC X(72) VALUE SPACE.
       01 LINE-31.
          02 FILLER PIC X(15) VALUE SPACE.
          02 L31-INSNAME PIC X(15).
          02 FILLER PIC X VALUE SPACE.
          02 L31-POLICY PIC X(12).
          02 FILLER PIC X(37) VALUE SPACE.
       01  ALF12.
           02 ALF12-1 PIC X(7).
           02 ALF12-2 PIC X(5).
       01  TAB01.
           02 TAB02 PIC X OCCURS 3 TIMES.
       01  IN-FIELD.
               04 IN-FIELD-10.
               05  IN-FIELD-9.
           06 IN-FIELD-8.
                 07  IN-FIELD-7.
                   08  IN-FIELD-6.
                     09  IN-FIELD-5.
                       10  IN-FIELD-4.
                         11  IN-FIELD-3.
                           12  IN-FIELD-2.
                             13  IN-FIELD-1  PIC X.
                             13  FILLER PIC X.
                           12  FILLER  PIC X.
                         11  FILLER    PIC X.
                       10  FILLER      PIC X.
                     09  FILLER        PIC X.
                   08  FILLER          PIC X.
           07 FILLER  PIC X.
           06 FILLER PIC X.
           05 FILLER PIC X.
               04  FILLER              PIC X(5).
       01 IN-FIELDTAB01 REDEFINES IN-FIELD.
          02 IN-FIELD-TAB PIC X OCCURS 15 TIMES.
       01 ACTION PIC XXX.
       01 X PIC 999.
       01  ALF1 PIC X.
       01  ALF2 PIC XX.
       01 ALF3 PIC XXX.
       01 ALF-3 PIC XXX.
       01 FLAGX PIC 9.
       01  ALF24 PIC X(24).
       01  RIGHT-2 PIC XX JUST RIGHT.
       01  RIGHT-3 PIC XXX JUST RIGHT.
       01  NUM2 PIC 99.
       01  YYY PIC 999.
       01  FLAG PIC 9.
       01  ANS PIC X(10).
       01  DISPLAY-DATE.
           02 DD-MM PIC XX.
           02 FILLER PIC X VALUE "-".
           02 DD-DD PIC XX.
           02 FILLER PIC X VALUE "-".
           02 DD-YY PIC XX.
       01  NEF-D PIC ZZ,ZZZ.99CR.
       01  TEST-DATE.
           05  T-CC            PIC 99.
           05  T-YY            PIC 99.
           05  T-MM            PIC 99.
           05  T-DD            PIC 99.
       01  NUMOFDOCS PIC 99 VALUE 0.
       01  NUM-2 PIC 99.
       01  CAREFLAG PIC 9.
       01  CAIDFLAG PIC 9.
       01  ICD10-FLAG PIC X.
       01  DATE-X PIC X(8).
       01  RETURN-FLAG PIC 9.
       01  ALF-2 PIC XX.
       01  ALF-5 PIC X(5).
       01  ALF-7 PIC X(7).
       01  NEF-2 PIC XX JUST RIGHT.
       01  Y PIC 99.
       01  TAGTAB01.
           02 TAGTAB PIC XX OCCURS 20 TIMES.
       LINKAGE SECTION.
       01 XGARNO PIC X(8).
       PROCEDURE DIVISION USING XGARNO.
       P0. OPEN INPUT PARMFILE GARFILE GAPFILE INSFILE ICD10FILE
                      TAGDIAG.
           OPEN OUTPUT FILEOUT.
           ACCEPT DATE-X FROM DATE YYYYMMDD.

           MOVE SPACE TO LINE-18 LINE-20 LINE-23
           LINE-27 LINE-29 LINE-30 LINE-31.
           MOVE 0 TO FLAG CAREFLAG CAIDFLAG.
           ACCEPT TEST-DATE FROM DATE YYYYMMDD.
           MOVE T-YY TO DD-YY
           MOVE T-MM TO DD-MM
           MOVE T-DD TO DD-DD
           MOVE DISPLAY-DATE TO L13-DATE.
           ACCEPT TEST-DATE FROM TIME
           MOVE "P" TO L13-AM.
           IF T-CC < 12 
           MOVE "A" TO L13-AM.
           IF T-CC > 12 
           SUBTRACT 12 FROM T-CC.
           MOVE T-CC TO L13-HR.
       P000. MOVE XGARNO TO G-GARNO
           READ GARFILE INVALID DISPLAY "INVALID ACCT" GO TO P6.
       DOC1. READ PARMFILE AT END GO TO P1-0.
             MOVE PM1 TO NUM2
      *       MOVE PM-UPIN TO DOCUPIN(NUM2)
             MOVE PM-NAME TO DOCNAME(NUM2)
             ADD 1 TO NUMOFDOCS
             GO TO DOC1.
       P1-0. IF NUMOFDOCS = 1 MOVE 1 TO NUM-2 GO TO P1.
             DISPLAY "ORDERING DOCTOR?"
             ACCEPT PM-UPIN
             IF PM-UPIN = "L" PERFORM DOCLIST VARYING
             NUM2 FROM 1 BY 1 UNTIL NUM2 > NUMOFDOCS
             GO TO P1-0.
             IF PM-UPIN = "?"
             DISPLAY "L  TO LIST THE LOCAL DOCTOR NUMBERS"
             DISPLAY "OR ENTER A NUMBER FROM 1 TO " NUMOFDOCS
           DISPLAY "E TO QUIT"
             GO TO P1-0.
           IF PM-UPIN = SPACE MOVE "01    " TO PM-UPIN.
           IF PM-UPIN = "E" MOVE 1 TO FLAG GO TO P6.
           MOVE SPACE TO RIGHT-2
           UNSTRING PM-UPIN DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO PM1
           IF PM1 NUMERIC MOVE PM1 TO NUM-2.
           IF (NUM-2 > 0) AND (NUM-2 NOT > NUMOFDOCS)
           DISPLAY  DOCNAME(NUM-2) GO TO P1.
           DISPLAY "BAD CHOICE " GO TO P1-0.
       P1. DISPLAY "PRIMARY DIAG?"
           ACCEPT IN-FIELD-7
           IF IN-FIELD-7 = "?"
           DISPLAY "ENTER A DIAG"
           DISPLAY "F OR M = SEARCH FOR DIAG"
           DISPLAY "CD = CHANGE ORDERING DOCTOR"
           DISPLAY "E =  END PROGRAM WITHOUT PRINTING"
           GO TO P1.
           IF IN-FIELD-7 = "CD" GO TO P1-0.
           IF IN-FIELD-7= "E" MOVE 1 TO FLAG GO TO P6.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT.
           IF RETURN-FLAG = 1 GO TO P1.
           IF IN-FIELD-7(6:2) = "??" MOVE "  " TO IN-FIELD-7(6:2).
           MOVE IN-FIELD-7 TO FO-DX1.

       P2. DISPLAY "SECONDARY DIAG?"
           ACCEPT IN-FIELD-7
           IF IN-FIELD-7= "?"
           DISPLAY "ENTER A DIAG"
           DISPLAY "F OR M = SEARCH FOR DIAG"
           DISPLAY "E = PRINT LAB-FORM"
           DISPLAY "BK = BACK TO PRIMARY DIAG"
           GO TO P2.
           IF IN-FIELD-7= "BK" GO TO P1.
           IF IN-FIELD-7= "E" GO TO P4.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT.
           IF RETURN-FLAG = 1 GO TO P2.
           IF IN-FIELD-7(6:2) = "??" MOVE "  " TO IN-FIELD-7(6:2).
           MOVE IN-FIELD-7 TO FO-DX2.

       P3. DISPLAY "TERTIARY DIAG?"
           ACCEPT IN-FIELD-7
           IF IN-FIELD-7= "?"
           DISPLAY "ENTER A DIAG"
           DISPLAY "F OR M = SEARCH FOR DIAG"
           DISPLAY "E = PRINT LAB-FORM"
           DISPLAY "BK = BACK TO SECONDARY DIAG"
           GO TO P3.
           IF IN-FIELD-7= "BK" GO TO P2.
           IF IN-FIELD-7= "E" GO TO P4.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT.
           IF RETURN-FLAG = 1 GO TO P3.
           IF IN-FIELD-7(6:2) = "??" MOVE "  " TO IN-FIELD-7(6:2).
           MOVE IN-FIELD-7 TO FO-DX3.
           GO TO P4.
       P44. DISPLAY "QUATENARY DIAG?"
           ACCEPT IN-FIELD-7
           IF IN-FIELD-7= "?"
           DISPLAY "ENTER A DIAG"
           DISPLAY "F OR M = SEARCH FOR DIAG"
           DISPLAY "E = PRINT LAB-FORM"
           DISPLAY "BK = BACK TO SECONDARY DIAG"
           GO TO P44.
           IF IN-FIELD-7= "BK" GO TO P2.
           IF IN-FIELD-7= "E" GO TO P4.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT.
           IF RETURN-FLAG = 1 GO TO P44.
      *     MOVE IN-FIELD-7 TO FO-DX4.
      *     IF FO-DX4(6:2) = "??" MOVE "  " TO FO-DX4(6:2).

       P4. DISPLAY "PRINT FORM? Y/N"
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "BK = BACK TO TERTIARY DIAG"
           DISPLAY "Y = YES PRINT"
           DISPLAY "N = END WITHOUT PRINTING FORM"
           GO TO P4.
           IF ANS = "BK" GO TO P3.
           IF ANS = "N" MOVE 1 TO FLAG GO TO P6.
           IF ANS = "Y" GO TO P6.
           GO TO P4.
       P6. 
           IF FLAG = 0
           PERFORM P5 THRU P5-EXIT
           PERFORM PX
           CALL "SYSTEM" USING "PRINT $HOME/W1$tid".
           CLOSE TAGDIAG PARMFILE GAPFILE GARFILE INSFILE ICD10FILE
                 FILEOUT
           EXIT PROGRAM.

       P5. MOVE SPACE TO FILEOUT01
           PERFORM A1 1 TIMES
           MOVE SPACE TO L14-LNAME L14-FNAME 
           UNSTRING G-GARNAME DELIMITED BY ";" INTO L14-LNAME L14-FNAME
           MOVE G-BILLADD TO L18-STREET 
           IF G-BILLADD = SPACE
           MOVE G-STREET TO L18-STREET.
           MOVE G-DOBMM TO L14-DOBMM
           MOVE G-DOBDD TO L14-DOBDD
           MOVE G-DOBYY TO L14-DOBYY
           MOVE "MM" TO L14-M MOVE SPACE TO L14-F
           IF G-SEX = "F" MOVE SPACE TO L14-M MOVE "F" TO L14-F.
           MOVE G-CITY TO L18-CITY
           MOVE G-STATE TO L18-STATE
           MOVE G-ZIP TO L18-ZIP
           IF G-PHONE1 NOT = "000"
           MOVE G-PHONE1 TO L14-PHONE1
           MOVE G-PHONE2 TO L14-PHONE2
           MOVE "-" TO L14-SPACE
           MOVE G-PHONE3 TO L14-PHONE3
           ELSE MOVE SPACE TO L14-PHONE1 L14-PHONE2
           L14-PHONE3 L14-SPACE.
      *     MOVE GARDOC(X) TO NUM2
           MOVE SPACE TO L20-UPIN
      *     MOVE DOCUPIN(NUM2) TO L20-UPIN
           MOVE DOCNAME(NUM2) TO L20-NAME
           MOVE SPACE TO LINE-23 LINE-27 LINE-29 LINE-31
           MOVE 0 TO PRIFLAG SECFLAG
           IF G-PRINS = "001" GO TO P5-EXIT.
           IF G-PRINS = "003" OR G-SEINS = "003" 
           MOVE 1 TO CAREFLAG
           PERFORM MAKE-23-03.
           IF G-PRINS = "028" OR G-SEINS = "028" 
           MOVE 1 TO CAREFLAG
           PERFORM MAKE-23-28.
           IF (G-PRINS = "004" OR "064" OR "026") 
           OR (G-SEINS = "004" OR "064" OR "026") 
           MOVE 1 TO CAIDFLAG
           PERFORM MAKE-31-04.
      *     IF (G-PRINS = "102")
      *     AND (CAREFLAG = 0 AND CAIDFLAG = 0)
      *     MOVE 0 TO PRIFLAG
      *     MOVE 1 TO SECFLAG
      *     GO TO MAKE-27.
           IF (CAREFLAG = 1 AND CAIDFLAG = 1) GO TO P5-EXIT.
           IF (G-PRINS = "004" OR "026" OR "064") GO TO P5-EXIT.
      *     OR (CAREFLAG = 0 AND CAIDFLAG = 0) GO TO P5-EXIT.
           MOVE 1 TO SECFLAG
           GO TO MAKE-27.
       MAKE-23-03.
           IF G-PRINS = "003" MOVE G-PRINS TO INS-KEY
           MOVE "PRIMARY" TO L23-PS
           MOVE G-PRIPOL1 TO XTAB01
           MOVE G-PRIPOL2 TO L23-SUFX
           MOVE 1 TO PRIFLAG
           ELSE MOVE "SECONDARY" TO L23-PS
           MOVE G-SEINS TO INS-KEY
           MOVE G-SECPOL1 TO XTAB01
           MOVE G-SECPOL2 TO L23-SUFX
           MOVE 1 TO SECFLAG.
           MOVE SPACE TO L23-TRAVELERS
           MOVE XT(1) TO L23-D1
           MOVE XT(2) TO L23-D2
           MOVE XT(3) TO L23-D3
           MOVE XT(4) TO L23-D4
           MOVE XT(5) TO L23-D5
           MOVE XT(6) TO L23-D6
           MOVE XT(7) TO L23-D7
           MOVE XT(8) TO L23-D8
           MOVE XT(9) TO L23-D9.
       MAKE-23-28.
           IF G-PRINS = "028" MOVE "PRIMARY" TO L23-PS
           MOVE G-PRIPOL TO L23-TRAVELERS
           MOVE 1 TO  PRIFLAG
           ELSE MOVE "SECONDARY" TO L23-PS
           MOVE G-SECPOL TO L23-TRAVELERS
           MOVE 1 TO SECFLAG.
       MAKE-31-04.
           IF G-PRINS = "004" OR "064" OR "026" 
           MOVE G-PRIPOL1 TO L31-POLICY
           MOVE "PRIMARY" TO L29-PS
           MOVE 1 TO PRIFLAG
           ELSE MOVE G-SECPOL TO L31-POLICY
           MOVE "SECONDARY" TO L29-PS
           MOVE 1 TO SECFLAG.
           IF (G-PRINS = "004" OR "064") OR (G-SEINS = "004" OR "064")
           MOVE "VERMONT" TO L31-INSNAME
           ELSE MOVE "NEW YORK" TO L31-INSNAME.
       MAKE-27.
           
           IF PRIFLAG = 0 MOVE G-PRINS TO INS-KEY
           MOVE G-PRIPOL TO L27-POLICY
           MOVE G-PR-GROUP TO L27-GROUP
           MOVE G-PRNAME TO L27-NAME
           MOVE "PRIMARY" TO L29-PS
           MOVE G-PR-RELATE TO X-RELATE.
           IF G-SEINS = "001" GO TO MAKE-27-2.
           IF SECFLAG = 0 MOVE G-SEINS TO INS-KEY
           MOVE G-SECPOL TO L27-POLICY
           MOVE G-SE-GROUP TO L27-GROUP
           MOVE G-SENAME TO L27-NAME
           MOVE "SECONDARY" TO L29-PS
           MOVE G-SE-RELATE TO X-RELATE.
           IF X-RELATE = SPACE MOVE SPACE TO L27-RELATE.
           IF G-RELATE = X-RELATE MOVE "SELF  " TO L27-RELATE.
           GO TO MAKE-27-2.
           IF G-RELATE = "2" OR "K" MOVE "SPOUSE" TO L27-RELATE.
           GO TO MAKE-27-2.
           MOVE "CHILD " TO L27-RELATE.
       MAKE-27-2.
           READ INSFILE INVALID MOVE "NO INSURANCE" TO INS-NAME.
           MOVE INS-NAME TO L27-INSNAME
           MOVE INS-STREET TO L29-STREET
           MOVE INS-CITY TO L29-CITY
           MOVE INS-STATE TO L29-STATE
           MOVE INS-ZIP TO L29-ZIP.
           IF INS-KEY NOT = "062" MOVE SPACE TO LINE-30 GO TO P5-EXIT.
           MOVE G-PR-GROUP TO ALF12
           MOVE ALF12-1 TO GAPKEY
           READ GAPFILE INVALID MOVE "NO INSURANCE" TO GAP-NAME.
           MOVE GAP-NAME TO L27-INSNAME
           MOVE GAP-ADDR   TO L29-STREET
           MOVE GAP-CITY TO L29-CITY
           MOVE GAP-STATE TO L29-STATE
           MOVE GAP-ZIP TO L29-ZIP
           IF GAP-TYPE = "X"
           MOVE GAPKEY TO L30-GAPKEY
           ELSE MOVE SPACE TO L30-GAPKEY.
       P5-EXIT. EXIT.
       PX. MOVE SPACE TO FILEOUT01
           MOVE LINE-13 TO FILEOUT01
           WRITE FILEOUT01
           PERFORM A6 
           MOVE LINE-14 TO FILEOUT01
           WRITE FILEOUT01
           PERFORM A6 
           MOVE LINE-18 TO FILEOUT01
           WRITE FILEOUT01
           PERFORM A6
           MOVE LINE-20 TO FILEOUT01
           WRITE FILEOUT01
           PERFORM A6 2 TIMES
           MOVE LINE-23 TO FILEOUT01
           WRITE FILEOUT01
           PERFORM A6 
           MOVE LINE-27 TO FILEOUT01
           WRITE FILEOUT01
           PERFORM A6
           MOVE LINE-29 TO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           MOVE LINE-30 TO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           MOVE LINE-31 TO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01.
           PERFORM A6 10 TIMES.
       
       A1. WRITE FILEOUT01.
       A6. 
           MOVE SPACE TO FILEOUT01.
           WRITE FILEOUT01.

       DOCLIST. DISPLAY NUM2 " "  DOCNAME(NUM2).
       CD10.
           IF IN-FIELD = "F" GO TO 1ICD10-SEARCH.
           IF IN-FIELD = "M" GO TO 1MAP.
           IF (IN-FIELD-7(1:1) NUMERIC) OR (IN-FIELD-7(1:1) = "V")
            MOVE SPACE TO ALF-7
            STRING IN-FIELD-7(1:5) "??" DELIMITED BY SIZE INTO ALF-7
            MOVE ALF-7 TO IN-FIELD-7
           END-IF.
           MOVE IN-FIELD-7 TO ICD10-KEY.
           READ ICD10FILE INVALID DISPLAY "NOT ON FILE"
            MOVE 1 TO RETURN-FLAG
            GO TO CD10-EXIT
           END-READ
           IF (DATE-X < "20151001" AND ICD10-KEY(6:2) NOT = "??")
            DISPLAY "USE ICD9 CODE WITH THIS DATE"
            MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           IF (DATE-X > "20150930" AND ICD10-KEY(6:2) = "??")
            DISPLAY "USE ICD10 CODE WITH THIS DATE"
            MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           DISPLAY ICD10-TITLE
           GO TO CD10-EXIT.


       1ICD10-SEARCH.
           MOVE 1 TO ICD10-FLAG
           IF DATE-X < "20151001" MOVE 9 TO ICD10-FLAG.
           DISPLAY "SEARCH KEY ?".
           ACCEPT ICD10-TITLE.
           IF ICD10-TITLE = "?"
           DISPLAY "ICD9 BY TITLE, TYPE AT LEAST 1ST 2 LETTERS"
           DISPLAY "ICD9 BY CODE, TYPE AT LEAST 1ST 2 NUMBERS"
           DISPLAY "ICD9 V CODES, TYPE V AND THEN AT LEAST 1 #"
           DISPLAY " "
           DISPLAY "ICD10 BY TITLE, TYPE AT LEAST ONE LETTER"
           DISPLAY "ICD10 BY CODE, TYPE A LETTER THEN AT LEAST 1 #"
           GO TO 1ICD10-SEARCH
           END-IF.
           IF ICD10-TITLE = SPACE
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           MOVE ICD10-TITLE TO IN-FIELD
           IF (ICD10-FLAG = 9)
            AND
              (((ICD10-TITLE(1:1) = "V") AND (ICD10-TITLE(2:1) NUMERIC))
              OR
            ((ICD10-TITLE(1:1) NUMERIC) AND (ICD10-TITLE(2:1) NUMERIC)))
            MOVE ICD10-TITLE(1:7) TO ICD10-KEY
            GO TO 4DIAG
           END-IF

           IF (ICD10-FLAG NOT = 9)
             AND (ICD10-TITLE(1:1) ALPHABETIC)
             AND (ICD10-TITLE(2:1) NUMERIC)
             MOVE ICD10-TITLE(1:7) TO ICD10-KEY
              GO TO 4DIAG
           END-IF.

           START ICD10FILE KEY NOT < ICD10-TITLE INVALID
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           MOVE 0 TO X.
           GO TO 3DIAG.
       4DIAG.
           START ICD10FILE KEY NOT < ICD10-KEY INVALID
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           MOVE 0 TO X.
       3DIAG.
           READ ICD10FILE NEXT AT END
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           IF (ICD10-FLAG = 9) AND (ICD10-KEY(6:2) = "??")
             DISPLAY ICD10-KEY(1:5) " " ICD10-MEDB " " ICD10-TITLE
            IF ICD10-FLAG = 9 AND ICD10-KEY >= "V9199??"
             DISPLAY "END OF FILE"
             MOVE 1 TO RETURN-FLAG
             GO TO CD10-EXIT
            END-IF

           ADD 1 TO X
           END-IF.

           IF (ICD10-FLAG = 1) AND (ICD10-KEY(6:2) NOT = "??")
             DISPLAY ICD10-KEY " " ICD10-MEDB " " ICD10-TITLE
           ADD 1 TO X
           END-IF.


           IF X > 5
            ACCEPT ANS
             IF ANS NOT = SPACE
              MOVE 1 TO RETURN-FLAG
              GO TO CD10-EXIT
             END-IF
           MOVE 0 TO X
           END-IF
           GO TO 3DIAG.
       1MAP.
           DISPLAY "ENTER A VALID ICD9 CODE OR X TO QUIT."
           ACCEPT ALF-5
           IF ALF-5 = "X"
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           MOVE ALF-5 TO TAG-ICD9-5
           MOVE SPACE TO TAG-ICD9-7
           START TAGDIAG KEY NOT < TAG-ICD9
             INVALID
             MOVE 1 TO RETURN-FLAG
             GO TO CD10-EXIT
           END-START.
           MOVE 0 TO Y.
       2MAP.
           READ TAGDIAG NEXT AT END GO TO 4MAP.
           IF TAG-ICD9-5 NOT = ALF-5 GO TO 4MAP.
           ADD 1 TO Y
           MOVE TAG-ICD9-7 TO TAGTAB(Y)
           MOVE TAG-ICD9-7 TO ICD10-KEY
           READ ICD10FILE INVALID GO TO 2MAP.
           MOVE Y TO NEF-2

           DISPLAY NEF-2 " " TAG-ICD9-7 " " ICD10-TITLE.
       3MAP.
           IF Y < 20 GO TO 2MAP.
       4MAP.
           IF Y = 0
           GO TO 1MAP
           END-IF
           DISPLAY "CHOOSE FROM THE LIST".
           IF Y = 20
            DISPLAY " OR CONTINUE LOOKING"
           END-IF
           ACCEPT ALF-2.

           IF ALF-2 = "?"
           DISPLAY "A = USE A DIFFERENT ICD9 CODE"
           DISPLAY "X = STOP THE MAPPING"
           DISPLAY "PICK A NUMBER FROM THE LIST"
           DISPLAY " TO MAKE A SELECTION"
           DISPLAY "ENTER KEY TO CONTINUE THE MAPPING"
           GO TO 4MAP.
           IF ALF-2 = "A" GO TO 1MAP.
           IF ALF-2 = "X" GO TO CD10-EXIT.
           IF ALF-2 = SPACE MOVE 0 TO Y GO TO 2MAP.
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
             MOVE ALF-2 TO NUM-2
             IF (NUM-2 > 0) AND (NUM-2 <= Y)
              MOVE TAGTAB(NUM-2) TO IN-FIELD-7
              IF DATE-X < "20151001"
               DISPLAY "MUST USE ICD9 CODE FOR THIS DATE"
               DISPLAY DATE-X(5:2) "-" DATE-X(7:2) "-" DATE-X(1:4)
               ACCEPT ANS
               MOVE 1 TO RETURN-FLAG
              END-IF
              MOVE IN-FIELD-7 TO ICD10-KEY
              READ ICD10FILE INVALID
               MOVE 1 TO RETURN-FLAG
               CONTINUE
              END-READ
              DISPLAY ICD10-TITLE
              GO TO CD10-EXIT
             END-IF
           END-IF
            GO TO 4MAP.
       CD10-EXIT.
           EXIT.
