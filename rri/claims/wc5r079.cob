      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wc5r079.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT PATFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT SEGFILE ASSIGN TO "S45" ORGANIZATION 
           LINE SEQUENTIAL.
       
           SELECT FILEIN ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT ERRFILE ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT PARMFILE ASSIGN TO "S60" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT REFPHY ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT DIAGFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT AUTHFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.
       
           SELECT MPLRFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS MPLR-KEY
           LOCK MODE MANUAL.
       
           SELECT INSFILE ASSIGN TO "S85" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT GAPFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT PAYCUR ASSIGN TO "S95" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
       
           SELECT PLACEFILE ASSIGN TO "S100" ORGANIZATION 
           LINE SEQUENTIAL.
       
           SELECT PARMFILE2 ASSIGN TO "S105" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT WEBFILE  ASSIGN TO "S110" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS WEB-KEY.
       
           SELECT DOCFILENEW  ASSIGN TO "S115" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DOC-KEY.

       DATA DIVISION.

       FILE SECTION.

       FD  WEBFILE.
       01  WEBFILE01.
           02 WEB-KEY PIC X(8).
           02 WEB-NUM PIC 9999.

       FD  DOCFILENEW.
           COPY DOCFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
           02 DF8 PIC X(10).

       FD  PARMFILE.
       01  PARMFILE01 PIC X(80).

       FD  PARMFILE2.
       01  PARMFILE201 PIC X(15).
       
       FD  ERRFILE.
       01  ERRFILE01.
           02 EF-1 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 EF-2 PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 EF-3 PIC X(16).
           02 EF-4 PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 EF-5 PIC X(10).

       FD  FILEIN.
       01  FILEIN01.
           02 FILEIN-KEY.
             03 FI-KEY8 PIC X(8).
             03 FI-KEY3 PIC XXX.
           02 FI-PATID.
             03 FI-PATID7 PIC X(7).
             03 FI-PATID8 PIC X.
           02 FI-CLAIM PIC X(6).
           02 FI-SERVICE PIC X.
           02 FI-DIAG PIC X(7).
           02 FI-PROC.
             03 FI-PROC0 PIC X(4).
             03 FI-PROC1 PIC X(5).
             03 FI-PROC2 PIC XX.
           02 FI-MOD2 PIC XX.
           02 FI-MOD3 PIC XX.
           02 FI-MOD4 PIC XX.
           02 FI-AMOUNT PIC S9(4)V99.
           02 FI-DOCR PIC X(3).
           02 FI-DOCP PIC 99.
           02 FI-PAYCODE PIC XXX.
           02 FI-STUD PIC X.
           02 FI-WORK PIC 99.
           02 FI-DAT1 PIC X(8).
           02 FI-RESULT PIC X.
           02 FI-ACTION PIC X.
           02 FI-SORCREF PIC X.
           02 FI-COLLT PIC X.
           02 FI-AGE PIC X.
           02 FI-PAPER PIC X.
           02 FI-PLACE PIC X.
           02 FI-IOPAT PIC X.
           02 FI-DATE-T PIC X(8).
           02 FI-DATE-A PIC X(8).
           02 FI-DATE-P PIC X(8).
           02 FI-REC-STAT PIC X.
           02 FI-DX2 PIC X(7).
           02 FI-DX3 PIC X(7).
           02 FI-ACC-TYPE PIC X.
           02 FI-DATE-M PIC X(8).
           02 FI-ASSIGN PIC X.
           02 FI-NEIC-ASSIGN PIC X.
           02 FI-DX4 PIC X(7).
           02 FI-DX5 PIC X(7).
           02 FI-DX6 PIC X(7).
           02 FI-FUTURE PIC X(6).
           02 FI-NEIC PIC X(5).

       FD  SEGFILE.
       01  SEGFILE01 PIC X(160).
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib\rri".
           
       FD  INSFILE.
           COPY INSFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  PATFILE.
           COPY PATFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  MPLRFILE.
           COPY MPLRFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  AUTHFILE.
           COPY AUTHFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  REFPHY.
           COPY REFPHY.CPY IN "C:\Users\sid\cms\copylib".
           
       FD  GAPFILE.
           COPY GAPFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  DIAGFILE.
           COPY DIAGFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.

       COPY "hip5010_837.cpy" IN "C:\Users\sid\cms\copylib".      

       01  TEST-DATE.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
       01  DISPLAY-DATE.
           05 T-MM PIC XX.
           05 FILLER PIC X VALUE "/".
           05 T-DD PIC XX.
           05 FILLER PIC X VALUE "/".
           05 T-CC PIC XX.
           05 T-YY PIC XX.
       01  ALF14.
           02 ALF14-3 PIC XXX.
           02 ALF14-9 PIC X(9).
           02 FILLER PIC XX.
       01  DATE-X PIC X(8).
       01  TIME-X. 
           02 TIME-HHMM PIC X(4).
           02 FILLER PIC X(4).
       01  FLAG PIC 9.
       01  ORDER-FLAG PIC 9.
       01  END-FLAG PIC 9 VALUE 0.
       01  GAP-FLAG PIC 9.
       01  CNTR PIC 99.
       01  DIAG-CNTR PIC 99.
       01  DX-CNTR-PT PIC 9.
       01  X PIC 99.
       01  DIAG-X PIC X(7).
       01  Y PIC 99.
       01  Z PIC 9.
       01  A PIC 99.
       01  B PIC 99.
       01  C PIC 999.
       01  D PIC 999.
       01  ALF108 PIC X(108).

       01  TAB10801.
           02 TAB108 PIC X OCCURS 108 TIMES.

       01  DIAG9-ARRAY01.
           02 DIAG9-ARRAY OCCURS 12 TIMES.
              03 DIAG9-BF PIC XXXX.
              03 DIAG9-CODE PIC X(5).

       01  DIAG10-ARRAY01.
           02 DIAG10-ARRAY OCCURS 12 TIMES.
              03 DIAG10-BF PIC XXXXX.
              03 DIAG10-CODE PIC X(7).
              
       01  MOD-ARRAY01.
           02 MOD-ARRAY OCCURS 4 TIMES.
              03 MOD-C PIC X.
              03 MOD-CODE PIC XX.

       01  DIAG-POINTER01.
           02 DIAG-POINTER OCCURS 12 TIMES.
              03 DIAG-PT PIC Z9.
              03 DIAG-C PIC X.

       01  DIAGTAB01.
           02 DIAGTAB PIC X(7) OCCURS 12 TIMES.

       01  FILETAB01.
           02 FILETAB PIC X(165) OCCURS 50 TIMES.

       01  GROUP-3 PIC XXX.
       01  ALF1 PIC X.
       01  ALF-2.
           02 ALF-2-1 PIC X.
           02 ALF-2-2 PIC X.

       01  ALF-4.
           02 ALF-4-1 PIC XX.
           02 ALF-4-2 PIC XX.
           
       01  ALF10 PIC X(10).
       01  ALF20 PIC X(20).
       01  ALF5 PIC X(5).
       01  ALF7.
           02 ALF71 PIC XX.
           02 ALF72 PIC X(5).
       01  ALF9 PIC X(9).
       01  ALFS PIC X(5).
       01  ALFS8 PIC X(8).
       01  ALFS9 PIC X(9).
       01  ALF5Z PIC ZZZZZ.
       01  ALF9Z PIC ZZZZZZZZZ.
       01  ALF5NUM PIC X(5).
       01  ALF9NUM PIC X(9).
       01  NUM7 PIC 9(5)V99.
       01  ALF8 PIC X(8).
       01  ALF8Z PIC ZZZZ9.99.
       01  ALF8NUM PIC X(8).
       01  NUM5 PIC 9(5). 
       01  NUM9 PIC 9(9).
       01  NUM2 PIC 99. 
       01  HL-NUMPARENT PIC 9(5).
       01  HL-NUM PIC 9(5) VALUE 0.
       01  EINSS PIC X(9).
       01  EINSS-TYPE PIC X.
       01  PARMLAST PIC X(15).
       01  PARMFIRST PIC X(15).
       01  PARMMIDDLE PIC X.
       01  ORG-NAME PIC X(40).
       01  ORG-CITY PIC X(30).
       01  ORG-STATE PIC XX.
       01  ORG-ZIP PIC X(9).
       01  SITE-ID.
           02 SITE-ID-1 PIC X(4).
           02 SITE-ID-2 PIC XX.

       01  INSGROUP-CODE PIC X(12).
       01  SUBMIT01.
           02 SUBMIT-1 PIC X(8).
           02 SUBMIT-2 PIC XX.

       01  TELE-PHONE PIC X(10).
       01  ORG-STREET PIC X(24).
       01  EIN-CODE PIC X(10).
       01  CONTACT-NAME PIC X(30).
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 29 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
             03 PL-STREET PIC X(18).
             03 PL-CITY PIC X(15).
             03 PL-STATE PIC XX.
             03 PL-ZIP PIC X(9).
             03 PL-NPI PIC X(10).
       01  PLINDX PIC 99 VALUE 0.
       01  CC-PL PIC X.
       01  HL-NUMPRV-SAVE PIC X(5).
       01  HL-SBR-SAVE PIC X(5).
       01  X-RELATE PIC X.
       01  SBR-RELATEHOLD PIC X.
       01  SUB-RELATE PIC X.
       01  SUB-NAME PIC X(24).
       01  SUB-GROUP PIC X(10).
       01  SUB-POLICY PIC X(16).
       01  TOT-AMOUNT PIC 9(6)V99.
       01  PLACE-POINTER PIC 99.
       01  SAVE01 PIC X(165).
       01  X-MOD.
           02 X-MOD1 PIC XX.
           02 X-MOD2 PIC XX.
           02 X-MOD3 PIC XX.
       01  NAME-1 PIC X(24).
       01  NAME-2 PIC X(24).
       01  HOLD-FILEIN01.
           02 HOLD-FILEIN-KEY.
             03 HOLD-KEY8 PIC X(8).
             03 HOLD-KEY3 PIC XXX.
           02 HOLD-PATID.
             03 HOLD-PATID7 PIC X(7).
             03 HOLD-PATID8 PIC X.
           02 HOLD-CLAIM PIC X(6).
           02 HOLD-SERVICE PIC X.
           02 HOLD-DIAG PIC X(7).
           02 HOLD-PROC.
             03 HOLD-PROC0 PIC X(4).
             03 HOLD-PROC1 PIC X(5).
             03 HOLD-PROC2 PIC XX.
           02 HOLD-MOD2 PIC XX.
           02 HOLD-MOD3 PIC XX.
           02 HOLD-MOD4 PIC XX.
           02 HOLD-AMOUNT PIC S9(4)V99.
           02 HOLD-DOCR PIC X(3).
           02 HOLD-DOCP PIC 99.
           02 HOLD-PAYCODE PIC XXX.
           02 HOLD-STUD PIC X.
           02 HOLD-WORK PIC 99.
           02 HOLD-DAT1 PIC X(8).
           02 HOLD-RESULT PIC X.
           02 HOLD-ACTION PIC X.
           02 HOLD-SORCREF PIC X.
           02 HOLD-COLLT PIC X.
           02 HOLD-AGE PIC X.
           02 HOLD-PAPER PIC X.
           02 HOLD-PLACE PIC X.
           02 HOLD-IOPAT PIC X.
           02 HOLD-DATE-T PIC X(8).
           02 HOLD-DATE-A PIC X(8).
           02 HOLD-DATE-P PIC X(8).
           02 HOLD-REC-STAT PIC X.
           02 HOLD-DX2 PIC X(7).
           02 HOLD-DX3 PIC X(7).
           02 HOLD-ACC-TYPE PIC X.
           02 HOLD-DATE-M PIC X(8).
           02 HOLD-ASSIGN PIC X.
           02 HOLD-NEIC-ASSIGN PIC X.
           02 HOLD-DX4 PIC X(7).
           02 HOLD-DX5 PIC X(7).
           02 HOLD-DX6 PIC X(7).
           02 HOLD-FUTURE PIC X(6).
           02 HOLD-NEIC PIC X(5).
       01  LASTREF PIC XXX.
       01  GROUP-TAX PIC X(10).
       01  ST-CNTR PIC 9999.
       01  GE-CNTR PIC 9999.
       01  SAVE-RELATE PIC XX.
       01  X-DOB.
	       02 X-YYYY PIC 9999.
	       02 X-MM PIC 99.
	       02 X-DD PIC 99.
       01  MONTH-TABLE-CONS.
	       05  FILLER PIC X(24) VALUE "312931303130313130313031".
       01  MONTH-TABLE REDEFINES MONTH-TABLE-CONS.
	       05  DAYS-IN-MONTH OCCURS 12 TIMES PIC 99.
       01  ANS PIC X.
       01  MAMMO-FLAG PIC 9.
       01  MAMMO-NUM PIC X(6).
       01  AUTH-FLAG PIC 9.
       01  CLM-DOCR PIC XXX.
       01  CLM-DOCP pic 99.
       
       PROCEDURE DIVISION.
       P0. 
           OPEN INPUT FILEIN GARFILE PATFILE INSFILE REFPHY
                      AUTHFILE MPLRFILE DIAGFILE PLACEFILE
                      GAPFILE PARMFILE PARMFILE2 DOCFILENEW. 
           
           OPEN OUTPUT SEGFILE ERRFILE.   
           
           OPEN I-O CHARCUR WEBFILE.

       P00.
           READ PLACEFILE
             AT END
               GO TO P00-X
           END-READ

           ADD 1 TO PLINDX
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           MOVE DF8 TO PL-NPI(PLINDX)
           
           GO TO P00.

       P00-X.
           ACCEPT BHT-DATE FROM CENTURY-DATE.
           MOVE BHT-DATE TO WEB-KEY
           
           READ WEBFILE WITH LOCK
             INVALID                            
               MOVE 1 TO WEB-NUM
               WRITE WEBFILE01
               END-WRITE
             NOT INVALID
               ADD 1 TO WEB-NUM             
               REWRITE WEBFILE01
           END-READ
           
           COMPUTE NUM9 = WEB-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO GS-NUM
           MOVE ALF9NUM TO GE-NUM
           MOVE 0 TO ST-CNTR GE-CNTR.

       P000.
           PERFORM A0 THRU A0-EXIT.
           MOVE SPACE TO SEGFILE01
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO BHT-TIME GS-5.
           MOVE BHT-DATE TO GS-4.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM GS01
           MOVE SPACE TO SEGFILE01.

       START-ST.
           ADD 1 TO ST-CNTR.
           MOVE ST-CNTR TO ST-NUM
           MOVE ST-CNTR TO SE-NUM
           WRITE SEGFILE01 FROM ST01.
           COMPUTE NUM9 = 1
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO BHT-NUM
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM BHT01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBM01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBPER01.
           MOVE "40 " TO NM1-1
           MOVE "2" TO  NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF
           MOVE "WEBMD" TO NM1-NAMEL
           MOVE "46" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE "133052274" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.

       START-BEGIN.
           READ FILEIN
             AT END
               GO TO P98
           END-READ.    

       START-HIGHER.
           MOVE FILEIN01 TO HOLD-FILEIN01
           MOVE FI-PAYCODE TO DOC-INS
           MOVE FI-DOCP TO DOC-NUM
           
           READ DOCFILENEW
             INVALID
               MOVE "000" TO DOC-INS
               READ DOCFILENEW
                 INVALID
                   GO TO START-BEGIN
               END-READ
           END-READ
           
      *    get POS for claim
           PERFORM DF-SEARCH
           PERFORM 2000A THRU 2000B.
           MOVE FI-PAYCODE TO INS-KEY
           READ INSFILE 
             INVALID
               DISPLAY FI-PAYCODE.
      *     MOVE INS-NAME TO RECNM1-NAMEL
      *     MOVE INS-NEIC TO RECNM1-CODE
      *     WRITE SEGFILE01 FROM RECNM101.
           GO TO P0000-1.
           
       P0000.
           MOVE FILEIN01 TO HOLD-FILEIN01.
           PERFORM DF-SEARCH.

       P0000-1.
           MOVE 0 TO CNTR DIAG-CNTR TOT-AMOUNT MAMMO-FLAG
           GO TO P1-1.

       P1. 
           READ FILEIN
             AT END
               MOVE 1 TO END-FLAG
               GO TO P2
           END-READ.

       P1-1. 
           IF FI-NEIC NOT = HOLD-NEIC 
             GO TO P2.

           IF DIAG-CNTR > 11 
             GO TO P2.
                      
           IF (FI-PLACE      = HOLD-PLACE
             AND FI-KEY8     = HOLD-KEY8
             AND FI-PATID    = HOLD-PATID
      *       AND FI-DOCP = HOLD-DOCP
      *       AND FI-DOCR = HOLD-DOCR
             AND FI-DATE-T   = HOLD-DATE-T
             AND FI-DAT1     = HOLD-DAT1
      *       AND FI-ACC-TYPE = HOLD-ACC-TYPE
             AND CNTR < 50)
               
             PERFORM DIAG-1 THRU DIAG-EXIT 
               
             IF DIAG-CNTR > 12
               GO TO P2
             END-IF

             ADD 1 TO CNTR 
           
      *       IF FI-PROC1 = "76090" OR "76091" OR "76092"
      *         OR "77055" OR "77056" OR "77057"
      *         MOVE 1 TO MAMMO-FLAG
      *       END-IF

             MOVE FILEIN01 TO FILETAB(CNTR)
             ADD FI-AMOUNT TO TOT-AMOUNT
             GO TO P1
           END-IF.    

       P2.  
           MOVE FILEIN01 TO SAVE01

           PERFORM 2300CLM THRU 2300CLM-EXIT

           PERFORM HI-DIAG THRU HI-DIAG-EXIT

           PERFORM 2310A THRU 2310A-EXIT
           
           IF EINSS-TYPE = "E" 
             PERFORM 2310B.

           PERFORM 2310D THRU 2310D-EXIT

           PERFORM 2310E THRU 2310E-EXIT
           
           PERFORM 2400SRV THRU 2400SRV-EXIT
              VARYING X FROM 1 BY 1 UNTIL X > CNTR
           
           IF END-FLAG = 1 GO TO P98.
           
           MOVE SAVE01 TO FILEIN01
           
           IF FI-NEIC NOT = HOLD-NEIC
               MOVE SPACE TO SEGFILE01
               WRITE SEGFILE01 FROM SE01
               ADD 1 TO GE-CNTR
               MOVE 0 TO HL-NUM
               PERFORM START-ST
               GO TO START-HIGHER
           END-IF    
           
           IF FI-DOCP NOT = HOLD-DOCP 
               MOVE FILEIN01 TO HOLD-FILEIN01
               MOVE HOLD-DOCP TO DOC-NUM
               MOVE "000" to DOC-INS
               
               READ DOCFILENEW
                 INVALID
                   MOVE "000" TO DOC-INS                   
                   READ DOCFILENEW
                     INVALID
                       GO TO START-BEGIN
                   END-READ
               END-READ

               PERFORM DOCP-1
           END-IF

           MOVE FILEIN01 TO HOLD-FILEIN01
           PERFORM 2000B 
           GO TO P0000.
           
       DIAG-1.
           IF FI-DIAG = "0000000"  GO TO DIAG-EXIT.

           MOVE FI-DIAG TO DIAG-X
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DIAG TO DIAGTAB(DIAG-CNTR).

           IF FI-DX2 = "0000000"  GO TO DIAG-EXIT.

           MOVE FI-DX2 TO DIAG-X.
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DX2 TO DIAGTAB(DIAG-CNTR).

           IF FI-DX3 = "0000000"  GO TO DIAG-EXIT.

           MOVE FI-DX3 TO DIAG-X
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DX3 TO DIAGTAB(DIAG-CNTR).

           IF FI-DX4 = "0000000"  GO TO DIAG-EXIT.

           MOVE FI-DX4 TO DIAG-X
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DX4 TO DIAGTAB(DIAG-CNTR).

       DIAG-EXIT. EXIT.
       DIAG-2. IF DIAGTAB(X) = DIAG-X
           MOVE DIAG-CNTR TO X
           MOVE 1 TO FLAG.

       2000A. 
           ADD 1 TO HL-NUM
           MOVE HL-NUM TO HL-NUMPRV-SAVE
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE SPACE TO HL-PARENT
           MOVE "20  " TO HL-CODE
           MOVE "1" TO HL-CHILD
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01
           MOVE GROUP-TAX TO PRV-TAX
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM PRV01
      *     PERFORM DOCP-1.

      *   PAY-TO PROVIDER/ADDRESS

       2010AA.
           MOVE "2" TO NM1-SOLO
           MOVE ORG-NAME TO NM1-NAMEL
           MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE "85" TO NM1-1
           MOVE SPACE TO NM1-CODE
           MOVE "XX" TO NM1-EINSS
           MOVE INSGROUP-CODE TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE SPACE TO N3-STREET
           MOVE "160 ALLEN ST" to N3-STREET
           WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE ORG-CITY TO N4-CITY
           MOVE ORG-STATE TO N4-STATE
           MOVE ORG-ZIP TO N4-ZIP
           
           IF N4-ZIP(6:4) = SPACE
             MOVE "9999" TO N4-ZIP(6:4)
           END-IF
           
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401
           
           MOVE "EI"TO REF-CODE
           MOVE EIN-CODE TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01
           
           MOVE SPACE TO REF-CODE REF-ID
           MOVE "G5" TO REF-CODE
           MOVE SITE-ID TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01                      

           MOVE "RUTLAND RADIOLOGISTS" TO PER-CONTACT
           MOVE "8003718685" TO PER-PHONE
           MOVE "FX" TO PER-S4
           MOVE "8027705175" TO PER-S6
           MOVE "EM" TO PER-S8
           MOVE "RRIBILLING@GMAIL.COM" TO PER-S10
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PER01
           
           IF EINSS-TYPE = "E"
               MOVE "2" TO NM1-SOLO
               MOVE ORG-NAME TO NM1-NAMEL
               MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
               MOVE "EI" TO REF-CODE
           ELSE 
               MOVE "1" TO NM1-SOLO
               MOVE PARMLAST TO NM1-NAMEL
               MOVE PARMFIRST TO NM1-NAMEF
               MOVE PARMMIDDLE TO NM1-NAMEM
               MOVE "SY" TO REF-CODE
           END-IF

           MOVE "87" TO NM1-1
           MOVE SPACE TO NM1-CODE
           MOVE "XX" TO NM1-EINSS
           MOVE INSGROUP-CODE TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE ORG-STREET TO N3-STREET
           MOVE SPACE TO SEGFILE01
           MOVE SPACE TO N3-STREET
           MOVE "PO BOX 440" TO N3-STREET
           WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE ORG-CITY TO N4-CITY
           MOVE ORG-STATE TO N4-STATE
           MOVE ORG-ZIP TO N4-ZIP
           
           IF N4-ZIP(6:4) = SPACE
             MOVE "9999" TO N4-ZIP(6:4).
           MOVE SPACE TO SEGFILE01
           MOVE "057020440" TO N4-ZIP
           WRITE SEGFILE01 FROM N401.           
     
       2000B.
           ADD 1 TO HL-NUM
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE HL-NUMPRV-SAVE TO NUM5
           PERFORM NUM-LEFT
           MOVE "1    " TO HL-PARENT
           MOVE "22  " TO HL-CODE
           PERFORM SUBSCRIBER-1 THRU SUBSCRIBER-EXIT.
           MOVE SBR-RELATE TO SAVE-RELATE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01
           MOVE SPACE TO SEGFILE01
           MOVE "18" TO SBR-RELATE
           WRITE SEGFILE01 FROM SBR01
           PERFORM 2010BA.
           PERFORM 2010BB.
           IF  SAVE-RELATE NOT = "18"
           MOVE SAVE-RELATE TO SBR-PST
           PERFORM 2000C.
       2010BA.
           MOVE "IL " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
            UNSTRING G-PRNAME DELIMITED BY ";" INTO
            NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "JR." OR "SR" OR "II" OR "III" OR "IV"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.
           MOVE SPACE TO NM1-CODE
           MOVE G-PRIPOL TO NM1-CODE
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           IF SAVE-RELATE NOT = SPACE
           MOVE N301 TO SAVEPAT-N301.

           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
           MOVE "9999" TO N4-ZIP(6:4).
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           IF SAVE-RELATE NOT = SPACE
           MOVE N401 TO SAVEPAT-N401.

           MOVE G-DOB TO DMG-DOB
           MOVE G-SEX TO DMG-GENDER
           IF SAVE-RELATE NOT = "18"
            PERFORM MAKE-IT-UP THRU MAKE-IT-UP-EXIT
           ELSE 
            MOVE "M" TO DMG-GENDER
            IF G-PR-RELATE NOT NUMERIC
             MOVE "F" TO DMG-GENDER
            END-IF
           END-IF.
           MOVE SPACE TO SEGFILE01.
           WRITE SEGFILE01 FROM DMG01.
       2010BB.
           MOVE "PR " TO NM1-1
           MOVE "2" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE FI-PAYCODE TO INS-KEY
           READ INSFILE INVALID MOVE SPACE TO INS-NAME.

           MOVE INS-NAME TO NM1-NAMEL
           MOVE "PI" TO NM1-EINSS
           MOVE INS-NEIC TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE INS-STREET TO N3-STREET
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE INS-CITY TO N4-CITY
           MOVE INS-STATE TO N4-STATE
           MOVE INS-ZIP TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
           MOVE "9999" TO N4-ZIP(6:4).
           MOVE SPACE TO SEGFILE01.
           WRITE SEGFILE01 FROM N401.
       2000C.
           MOVE HL-NUMX TO HL-PARENT
           MOVE "0" TO HL-CHILD
           ADD 1 TO HL-NUM
           MOVE HL-NUM TO HL-NUMPRV-SAVE
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE "23" TO HL-CODE.
           MOVE SPACE TO SEGFILE01.
           WRITE SEGFILE01 FROM HL01.
           MOVE SAVE-RELATE TO PAT-RELATE
           MOVE SPACE TO PAT-LOCATE PAT-EMPLOYM PAT-STUD
              PAT-QUAL PAT-DATE PAT-MEAS PAT-WT PAT-PREGO
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PAT01.

           MOVE "QC " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
            UNSTRING G-GARNAME DELIMITED BY ";" INTO
            NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "JR." OR "SR" OR "II" OR "III" OR "IV"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.
           MOVE SPACES TO NM1-EINSS NM1-CODE
           MOVE SPACE TO NM1-CODE
      *     MOVE G-PRIPOL TO NM1-CODE
      *     MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-EINSS
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SAVEPAT-N301.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SAVEPAT-N401.
           MOVE G-DOB TO DMG-DOB
           MOVE G-SEX TO DMG-GENDER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.

       2300CLM.
           MOVE HOLD-KEY8 TO SUBMIT-1
           MOVE SUBMIT01 TO CLM-1
           COMPUTE NUM7 = TOT-AMOUNT
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO CLM-2
           MOVE SPACE TO CLM-11 CLM-11-4 
      *    CURRENTLY UNUSED     
      *     CLM-11-2 CLM-11-3 CLM-11-5
           
           IF HOLD-DAT1 NOT = ZEROES
               PERFORM ACCIDENT-1 THRU ACCIDENT-EXIT.
           
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM CLM01.
           
           IF HOLD-DAT1 NOT = ZEROES
               MOVE "439" TO DTP-1
               MOVE HOLD-DAT1 TO DTP-3
               MOVE SPACE TO SEGFILE01
               WRITE SEGFILE01 FROM DTP01
           END-IF

           IF (CLM-5 = "21" OR "61")
               MOVE "435" TO DTP-1
               MOVE HOLD-DATE-M TO DTP-3
               MOVE SPACE TO SEGFILE01
               WRITE SEGFILE01 FROM DTP01
           END-IF                          
           
      *    add auth for VA/VACCN outpatient claims
           MOVE 0 TO AUTH-FLAG
      *     IF (HOLD-PAYCODE = "079" OR "225" OR "926")
              MOVE HOLD-KEY8 TO AUTH-KEY8
              MOVE HOLD-CLAIM TO AUTH-KEY6
              READ AUTHFILE INVALID
                  MOVE 1 TO AUTH-FLAG
                  GO TO 2300CLM-EXIT
              END-READ    
              MOVE SPACE TO REF-CODE
              MOVE "G1" TO REF-CODE
              MOVE SPACE TO REF-ID
              IF (AUTH-FLAG = 0 AND AUTH-NUM NOT = SPACE)
                MOVE AUTH-NUM TO REF-ID
      *        ELSE
      *          MOVE "VA9999999999" TO REF-ID
              END-IF  
              MOVE SPACE TO SEGFILE01
              WRITE SEGFILE01 FROM REF01.
      *     END-IF.   

       2300CLM-EXIT.
           EXIT.

       ACCIDENT-1.
           MOVE "OA" TO CLM-11.           
           IF INS-NEIC = "WX867" OR "J1868"
             MOVE "EM" TO CLM-11
             MOVE ":::VT" TO CLM-11-4.
             
       ACCIDENT-EXIT.
           EXIT.

       2310D.
           IF HOLD-PLACE NOT = "2"
               MOVE "77 " TO NM1-1
               
               IF HOLD-PLACE = "4"
                   MOVE "IL " TO NM1-1
               END-IF
               
               MOVE "2" TO NM1-SOLO
               MOVE PL-NAME(PLACE-POINTER) TO NM1-NAMEL
               MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
               MOVE "  " TO NM1-EINSS
               MOVE SPACE TO NM1-CODE
               
               IF PL-NPI(PLACE-POINTER) NOT = SPACE
                   MOVE "XX" TO NM1-EINSS
                   MOVE PL-NPI(PLACE-POINTER) TO NM1-CODE
               END-IF
               
               MOVE SPACE TO SEGFILE01
               WRITE SEGFILE01 FROM NM101

               MOVE SPACE TO N3-STREET N3-BILLADD
               MOVE PL-STREET(PLACE-POINTER) TO N3-STREET
               MOVE SPACE TO SEGFILE01
               WRITE SEGFILE01 FROM N301
               
               MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
               MOVE PL-CITY(PLACE-POINTER) TO N4-CITY
               MOVE PL-STATE(PLACE-POINTER) TO N4-STATE
               MOVE PL-ZIP(PLACE-POINTER) TO N4-ZIP
               MOVE SPACE TO SEGFILE01
               WRITE SEGFILE01 FROM N401
           END-IF.
       2310D-EXIT.
           EXIT.
       2310E.
           IF LASTREF = SPACE GO TO 2310E-EXIT.
           MOVE LASTREF TO REF-KEY
           READ REFPHY INVALID GO TO 2310E-1.
           MOVE "DQ " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           UNSTRING REF-NAME DELIMITED BY 
           "; " OR ";" OR " ; " OR " ," OR ", " OR " , " OR ","  
           INTO NM1-NAMEL NM1-NAMEF
           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE "XX" TO NM1-EINSS
           MOVE REF-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           GO TO 2310E-EXIT.
       2310E-1.
           IF ORDER-FLAG = 1
           MOVE HOLD-DOCP TO NUM2
           MOVE "DQ " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF
           UNSTRING DOC-NAME DELIMITED BY ";" INTO
             NM1-NAMEL NM1-NAMEF
           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE "XX " TO NM1-EINSS
           MOVE DOC-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       2310E-EXIT.
           EXIT.



       2320A.
           GO TO 2320A-EXIT.

           IF G-SEINS = "001" OR "012" OR "075" OR "076"
           GO TO 2320A-EXIT.
           IF G-SEINS = "005" PERFORM CMP-1 GO TO 2320A-EXIT.
           IF G-SEINS = "004" OR "064" PERFORM CAID-1 GO TO 2320A-EXIT.
           MOVE 0 TO GAP-FLAG
           IF G-SEINS = "062" PERFORM GAP-1 THRU GAP-1-EXIT. 
       2320A-EXIT. EXIT.
       CMP-1.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           MOVE "18" TO SBR-RELATE
           MOVE "SP" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "MB" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.

           IF G-SENAME = SPACE MOVE G-GARNAME TO G-SENAME
                               MOVE G-RELATE TO G-SE-RELATE.

           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY ALL " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.

           MOVE "1" TO NM1-SOLO
           MOVE "IL" TO NM1-1
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE G-SECPOL TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           MOVE "MEDICOMP" TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "PR" TO NM1-1
           MOVE "PI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE "00026" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       CAID-1.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           MOVE "18" TO SBR-RELATE
           MOVE "MC" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "MC" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.

           IF G-SENAME = SPACE MOVE G-GARNAME TO G-SENAME
                               MOVE G-RELATE TO G-SE-RELATE.

           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY ALL " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.

           MOVE "1" TO NM1-SOLO
           MOVE "IL" TO NM1-1
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE G-SECPOL TO ALF9
           MOVE ALF9 TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           MOVE "VT MEDICAID" TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "PR" TO NM1-1
           MOVE "PI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE "VTXIX" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       GAP-1. 
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE INVALID MOVE 1 TO GAP-FLAG GO TO GAP-1-EXIT.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           IF G-RELATE = G-SE-RELATE
           MOVE "18" TO SBR-RELATE
           ELSE MOVE "01" TO SBR-RELATE.
           IF GAP-TYPE = "X"
           MOVE "MI" TO SBR-TYPE
           ELSE MOVE "SP" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "CI" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           IF G-SE-RELATE = "2" MOVE "M" TO DMG-GENDER
           ELSE MOVE "F" TO DMG-GENDER.
           WRITE SEGFILE01 FROM DMG01.

           IF G-SENAME = SPACE MOVE G-GARNAME TO G-SENAME
                               MOVE G-RELATE TO G-SE-RELATE.

           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "JR." OR "SR" OR "II" OR "III" OR "IV"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY ALL " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.
           MOVE "1" TO NM1-SOLO
           MOVE "IL" TO NM1-1
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE G-SECPOL TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
       
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           MOVE GAP-NAME TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "PR" TO NM1-1
           MOVE "PI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE GAPKEY TO ALF7
           MOVE ALF72 TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       
       GAP-1-EXIT.
           EXIT.
       
       2400SRV.
           MOVE FILETAB(X) TO FILEIN01
           MOVE FI-PROC1 TO SV1-PROC.
           PERFORM SV-MOD
           COMPUTE NUM7 = FI-AMOUNT
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO SV1-AMT
           COMPUTE NUM5 = FI-WORK
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO SV1-WORK
           
           MOVE SPACE TO SV1-PLACE
           MOVE 0 TO DX-CNTR-PT
           MOVE SPACE TO DIAG-POINTER01
           MOVE FI-DIAG TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
           MOVE FI-DX2 TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
           MOVE FI-DX3 TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
           MOVE FI-DX4 TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12

           MOVE SPACE TO DIAG-C(DX-CNTR-PT)
           MOVE DIAG-POINTER01 TO SV1-PT
           GO TO SV1-0.

       DIAG-3.
           IF DIAGTAB(A) = DIAG-X
           ADD 1 TO DX-CNTR-PT
           MOVE A TO DIAG-PT(DX-CNTR-PT) 
           MOVE ":" TO DIAG-C(DX-CNTR-PT)
           MOVE 12 TO A.

       SV1-0.
           MOVE " " TO SV1-EMER
           IF CLM-5 = "23" MOVE "Y" TO SV1-EMER.

           COMPUTE NUM5 = X
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO LX-1
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM LX01.

           MOVE SPACE TO SV1-COPAY.
       2400SRV-1.
           MOVE SPACE TO TAB10801
           MOVE 0 TO D
           
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 108
               IF SV101(C:1) NOT = " "
                   ADD 1 TO D
                   MOVE SV101(C:1) TO TAB108(D)
               END-IF
           END-PERFORM
           
           MOVE TAB10801 TO ALF108
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM ALF108.

           MOVE "472" TO DTP-1 D8TP-1
           MOVE SPACE TO SEGFILE01
           MOVE FI-DATE-T TO DTP-3
           WRITE SEGFILE01 FROM DTP01
           
           IF HOLD-DOCR NOT = CLM-DOCR
             display hold-docr " hold-docr"
             display clm-docr " clm-docr"
             accept omitted
             PERFORM 2310A THRU 2310A-EXIT
           end-if  

           display hold-docp " hold-docp"
           display clm-docp " clm-docp"
           accept omitted
           
           if hold-docp not = clm-docp
             display hold-docp " hold-docp"
             display clm-docp " clm-docp"
             accept omitted
             
             perform 2420a through 2420a-exit
           end-if

           MOVE FILEIN-KEY TO CHARCUR-KEY
           
           READ CHARCUR WITH LOCK
             INVALID CONTINUE
           
             NOT INVALID 
               
               IF CC-REC-STAT = "0"
                   MOVE "2" TO CC-REC-STAT
               END-IF
               
               IF CC-REC-STAT = "1"
                   MOVE "3" TO CC-REC-STAT
               END-IF
           
               MOVE BHT-DATE TO CC-DATE-A
           REWRITE CHARCUR01
           END-READ.

       2400SRV-EXIT.
           EXIT.

       2420A.
           MOVE CLM-DOCP TO DOC-NUM
           MOVE "000" TO DOC-INS
           READ DOCFILENEW
             INVALID
               MOVE "000" TO DOC-INS              
               READ DOCFILENEW
                 INVALID
                   GO TO START-BEGIN
               END-READ
           END-READ
           
           MOVE "82 " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE "XX" TO NM1-EINSS
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           UNSTRING DOC-NAME DELIMITED BY ";"
                INTO NM1-NAMEL NM1-NAMEF 
           MOVE DOC-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE "PE" TO PRV-1
           MOVE "PXC" TO PRV-2
           MOVE DOC-TAXONOMY TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.
       
      *     MOVE "SY" TO REF-CODE
      *     MOVE DOC-SSNUM TO REF-ID
      *     MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM REF01.

       2420A-EXIT.
           EXIT.

       2310A.
           IF HOLD-DOCR = "000" 
             GO TO REF-2.

           MOVE HOLD-DOCR TO REF-KEY 
           
      *     IF CNTR = 1
             MOVE HOLD-DOCR TO CLM-DOCR
      *     end-if

           READ REFPHY 
             INVALID 
               GO TO REF-2.

           MOVE "DN " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           UNSTRING REF-NAME DELIMITED BY ", " OR " ,"
             OR " , " OR "," OR ";" INTO NM1-NAMEL NM1-NAMEF

           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE "XX" TO NM1-EINSS
           MOVE REF-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           GO TO 2310A-EXIT.

       REF-2.
           MOVE "DN" TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           UNSTRING DOC-NAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE "XX" TO NM1-EINSS
           MOVE DOC-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.

       2310A-EXIT.
           EXIT.

       2310B.
      *     IF CNTR = 1
             MOVE hold-docp to CLM-DOCP
      *     end-if

           MOVE "82 " TO NM1-1
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           UNSTRING DOC-NAME DELIMITED BY ";" INTO
             NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE "1" TO NM1-SOLO
           MOVE DOC-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           
           MOVE "PE" TO PRV-1
           MOVE "PXC" TO PRV-2
           MOVE DOC-TAXONOMY TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.

       DOCP-1.
           MOVE "82 " TO SAVE-DOCNM1-1
           MOVE "1" TO SAVE-DOCNM1-SOLO
           MOVE SPACE TO SAVE-DOCNM1-NAMEL SAVE-DOCNM1-NAMEF
                       SAVE-DOCNM1-NAMEM SAVE-DOCNM1-NAMES
           UNSTRING DOC-NAME DELIMITED BY ";" INTO
           SAVE-DOCNM1-NAMEL SAVE-DOCNM1-NAMEF
           MOVE "XX" TO SAVE-DOCNM1-EINSS
           MOVE DOC-NPI TO SAVE-DOCNM1-CODE
           MOVE "XX" TO SAVE-DOCREF-CODE
           MOVE DOC-NPI TO SAVE-DOCREF-ID.


       SUBSCRIBER-1.
           MOVE HOLD-KEY8 TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD BAD BAD" GO TO P99.
           MOVE "P" TO SBR-PST.
           MOVE G-PR-GROUP TO SBR-GROUP

           MOVE FI-PAYCODE TO INS-KEY
           READ INSFILE 
             INVALID
               DISPLAY FI-PAYCODE.

           MOVE "CI" TO SBR-INSCODE
           IF INS-NEIC = "WX867" OR "J1868"
             MOVE "WC" TO SBR-INSCODE.
  
           MOVE SPACE TO SBR-TYPE.
           IF
      *     (G-GARNAME NOT = G-PRNAME) AND
           (G-RELATE NOT = G-PR-RELATE)
             GO TO SUBSCRIBER-2.
           MOVE "18" TO SBR-RELATE
           MOVE "0    " TO HL-CHILD
           GO TO SUBSCRIBER-EXIT.
       SUBSCRIBER-2.
           MOVE "01" TO SBR-RELATE.
           IF   (G-RELATE = "4" OR "M")
            MOVE "19" TO SBR-RELATE.
           IF   (G-RELATE = "5" OR "N")
            MOVE "17" TO SBR-RELATE.
           IF   (G-RELATE = "Q")
            MOVE "G8" TO SBR-RELATE.
           MOVE "1    " TO HL-CHILD.

       SUBSCRIBER-EXIT.
           EXIT.


       PAT-READ. 
           MOVE HOLD-PATID TO P-PATNO
           READ PATFILE INVALID MOVE "0" TO P-RELATE.
           MOVE P-RELATE TO X-RELATE.
       NUM-LEFT.
           MOVE NUM5 TO ALF5Z ALFS
           MOVE SPACE TO ALF5NUM
           MOVE ALF5Z TO ALF5
           UNSTRING ALF5 DELIMITED ALL " " INTO ALFS ALF5NUM.
       NUM-LEFT9.
           MOVE NUM9 TO ALF9Z
           MOVE SPACE TO ALF9NUM
           MOVE ALF9Z TO ALF9 ALFS9
           UNSTRING ALF9 DELIMITED ALL " " INTO ALFS9 ALF9NUM.
       AMT-LEFT.
           MOVE NUM7 TO ALF8Z
           MOVE SPACE TO ALF8NUM ALFS8
           MOVE ALF8Z TO ALF8
           UNSTRING ALF8 DELIMITED ALL " " INTO ALFS8 ALF8NUM.

      * ISA RECORD.
       ISA-1. 
      *  AUTHORIZATION INFO QUALIFIER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-1.
      *  AUTHORIZATION INFO
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-2.
      *  SECURITY INFO QUAL
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-3.
      *  SECURITY INFO
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-4.
      *  INTERCHANGE ID QUALIFIER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-5.
      *  INTERCHANGE SENDER ID
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-6.
      *  INTERCHANGE ID QUALIFIER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-7.
      *  INTERCHANGE RECEIVER ID
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-8.
      *  INTERCHANGE DATE
           READ PARMFILE2 AT END GO TO A0-EXIT.
           ACCEPT ISA-9 FROM DATE.
      *  INTERCHANGE TIME
           READ PARMFILE2 AT END GO TO A0-EXIT.
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO ISA-10.
      *  INTERCHAGNE CONTROL STANDARDS ID
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-11.
      *  INTERCHANGE CONTROL VERSION CODE
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-12.
      *  INTERCHANGE CONTROL NUMBER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-13.
      *  ACKNOWLEDGEMENT REQUESTED
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-14.
      *  USAGE INDICATOR
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-15.
      *  COMPONENT ELEMENT SEPARATOR
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-16.
       ISA-EXIT. EXIT.
      
       A0.    
      * TAX ID NUMBER  
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO EIN-CODE.

      * TAX ID TYPE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO EINSS-TYPE.
      * CONTACT NAME
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO CONTACT-NAME.

      * TELEPHONE #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO TELE-PHONE.
      * INSURANCE-CODE #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO SITE-ID
           MOVE SITE-ID-2 TO SUBMIT-2.
      * INSURANCE-GROUP #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE SPACE TO INSGROUP-CODE
           MOVE PARMFILE01 TO INSGROUP-CODE.

      * SUBMITER ID INDICATORS (2)
      *     READ PARMFILE AT END GO TO A0-EXIT.
      *     MOVE PARMFILE01 TO SUBMIT-2.
      * ORGANIZATION NAME
           READ PARMFILE AT END GO TO A0-EXIT.
           IF EINSS-TYPE = "S"
           MOVE SPACE TO PARMLAST PARMFIRST PARMMIDDLE
           UNSTRING PARMFILE01 DELIMITED BY ";"
           INTO PARMLAST PARMFIRST PARMMIDDLE
           ELSE MOVE PARMFILE01 TO ORG-NAME.

      * GROUP'S STREET
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-STREET

      * GROUP'S CITY
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-CITY

      * GROUP'S STATE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-STATE

      * GROUP'S ZIP CODE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-ZIP

      * GROUP 3-CHARACTER MNEMONIC CODE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO GROUP-3.

      *   MAMMO-NUMBER
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO MAMMO-NUM. 
      *   ACCT-TAX
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO GROUP-TAX. 

       A0-EXIT.  EXIT.

       DF-SEARCH. 
           MOVE 0 TO FLAG
           MOVE "1" TO CC-PL
           MOVE "11" TO CLM-5.
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.
           
           IF HOLD-PLACE = "2" MOVE "12" TO CLM-5.

       DF-SEARCH2.  
      *     DISPLAY HOLD-PROC1 " " HOLD-PLACE " " HOLD-KEY8 
           IF HOLD-PLACE = PL-TAB(Y) 
             MOVE PL-NUM(Y) TO CC-PL
             MOVE Y TO PLACE-POINTER
             PERFORM PLACE-OF-SERVICE THRU POS-EXIT
      *    DISPLAY HOLD-PROC1 " " HOLD-PLACE " " CLM-5
             MOVE PLINDX TO Y.

       PLACE-OF-SERVICE.
           IF CC-PL = "1" MOVE "11" TO CLM-5.
           IF CC-PL = "3" MOVE "21" TO CLM-5.
           IF CC-PL = "4" MOVE "32" TO CLM-5.
           IF CC-PL = "5" MOVE "22" TO CLM-5.
           IF CC-PL = "6" MOVE "81" TO CLM-5.
           IF CC-PL = "7" MOVE "61" TO CLM-5.
           IF CC-PL = "8" MOVE "99" TO CLM-5.
           IF CC-PL = "E" MOVE "23" TO CLM-5. 
           IF CC-PL = "W" MOVE "32" TO CLM-5.
           IF CC-PL = "K" MOVE "31" TO CLM-5.
           IF CC-PL = "D" MOVE "33" TO CLM-5.
           IF CC-PL = "Q" MOVE "13" TO CLM-5.
           IF CC-PL = "S" MOVE "03" TO CLM-5.
           
           IF HOLD-PROC1 > "99320" AND < "99329" MOVE "33" TO CLM-5.
           IF HOLD-PROC1 > "99330" AND < "99338" MOVE "33" TO CLM-5.
           IF (HOLD-PROC1 = "G0179" OR "G0180" OR "G0181" OR "G0182")
           MOVE "11" TO CLM-5.
       POS-EXIT. 
           EXIT.
       HI-DIAG.
           IF HOLD-DATE-T > "20150930" GO TO HI-DIAG10.
           MOVE HOLD-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE SPACE TO DIAG-MEDB.
           MOVE DIAG-MEDB TO HI9-DX1
           MOVE SPACE TO HI9-DIAG-FILLER DIAG9-ARRAY01

           PERFORM VARYING X FROM 2 BY 1 UNTIL X > DIAG-CNTR
            MOVE DIAGTAB(X) TO DIAG-KEY
             READ DIAGFILE INVALID CONTINUE
             END-READ
            MOVE "*BF:" TO DIAG9-BF(X - 1)
            MOVE DIAG-MEDB TO DIAG9-CODE(X - 1)
           END-PERFORM.
           MOVE DIAG9-ARRAY01 TO HI9-DIAG-FILLER
           WRITE SEGFILE01 FROM HI901.
           GO TO HI-DIAG-EXIT.
       HI-DIAG10.
           MOVE HOLD-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE SPACE TO DIAG-MEDB.
           MOVE DIAG-KEY TO HI10-DX1
           MOVE SPACE TO HI10-DIAG-FILLER DIAG10-ARRAY01

           PERFORM VARYING X FROM 2 BY 1 UNTIL X > DIAG-CNTR
            MOVE DIAGTAB(X) TO DIAG10-CODE(X - 1)
            MOVE "*ABF:" TO DIAG10-BF(X - 1)
           END-PERFORM.
           MOVE DIAG10-ARRAY01 TO HI10-DIAG-FILLER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HI1001.
       HI-DIAG-EXIT.
           EXIT.
       X2310B.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SAVE-DOCNM101
           MOVE "PE" TO PRV-1
           MOVE "PXC" TO PRV-2
           MOVE DOC-TAXONOMY TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.
           MOVE "SY" TO REF-CODE
           MOVE DOC-SSNUM TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01.

           MOVE "SY" TO REF-CODE
           MOVE DOC-SSNUM TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01.

       SV-MOD.



           MOVE SPACE TO SV1-MOD-FILLER MOD-ARRAY01
           MOVE FI-PROC2 TO MOD-CODE(1)
           MOVE FI-MOD2 TO MOD-CODE(2)
           MOVE FI-MOD3 TO MOD-CODE(3)
           IF FI-DOCP = "07" 
              MOVE "Q6" TO MOD-CODE(4)
            ELSE
              MOVE SPACE TO MOD-CODE(4)
           END-IF
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 3
             IF MOD-CODE(Z) = SPACE
               COMPUTE A = Z + 1
                PERFORM VARYING B FROM A BY 1 UNTIL B > 4
                  IF MOD-CODE(B) NOT = SPACE
                    MOVE MOD-CODE(B) TO MOD-CODE(Z)
                    MOVE SPACE TO MOD-CODE(B)
                    MOVE 4 TO B
                  END-IF
                END-PERFORM
             END-IF
           END-PERFORM.
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 4
             IF MOD-CODE(Z) NOT = SPACE
               MOVE ":" TO MOD-C(Z)
             END-IF
           END-PERFORM

           MOVE MOD-ARRAY01 TO SV1-MOD-FILLER.


      * MOD-FIX.
      *     IF (ALF-2-1 = SPACE AND ALF-2-2 NOT = SPACE)
      *     OR (ALF-2-2 = SPACE AND ALF-2-1 NOT = SPACE)
      *     MOVE SPACE TO ALF-2.
       MAKE-IT-UP.
           MOVE G-DOB TO X-DOB.
           IF G-RELATE = "4" OR "5" OR "M" OR "N"
            SUBTRACT 29 FROM X-YYYY
           END-IF.
           IF G-SEX = "F"
            COMPUTE X-YYYY = X-YYYY - 1
           END-IF.                    
           IF G-SEX = "M"
            COMPUTE X-YYYY = X-YYYY + 1
           
           END-IF.
           COMPUTE X-MM = 12 + X-DD.
       MAKE-IT-1.
           IF X-MM > 12
           COMPUTE X-MM = X-MM - 12
           GO TO MAKE-IT-1
           END-IF
           IF X-MM = 0 MOVE 1 TO X-MM.
            COMPUTE X-DD = X-DD + 32 + (X-YYYY - 1900).
       MAKE-IT-2.
           IF X-DD > DAYS-IN-MONTH(X-MM)
            COMPUTE X-DD = X-DD - DAYS-IN-MONTH(X-MM)  
	    GO TO MAKE-IT-2
           END-IF
           IF X-DD < 1 MOVE 1 TO X-DD.
           IF X-DD = 29 AND X-MM = 2
             MOVE 03 TO X-MM
             MOVE 02 TO X-DD
           END-IF.
           MOVE X-DOB TO DMG-DOB
           MOVE "M" TO DMG-GENDER
           IF G-PR-RELATE NOT NUMERIC
           MOVE "F" TO DMG-GENDER.
      *     DISPLAY G-DOB " " G-SE-RELATE " " G-GARNAME
      *     DISPLAY X-DOB " " G-PR-RELATE " " G-PRNAME.
       MAKE-IT-UP-EXIT.
           EXIT.
       P98.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SE01
           ADD 1 TO GE-CNTR
           MOVE SPACE TO SEGFILE01
           COMPUTE NUM5 = GE-CNTR
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO GE-CNTRX
           WRITE SEGFILE01 FROM GE01
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM IEA01.

       P99.
           REWRITE WEBFILE01.
            CLOSE GARFILE WEBFILE CHARCUR ERRFILE.
            STOP RUN.
