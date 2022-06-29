      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npir5447.
       AUTHOR. SID WAITE.
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
           
           SELECT HIPCLAIMFILE ASSIGN TO "S110" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS HIP-KEY
           LOCK MODE MANUAL.
           
           SELECT CAREFILE ASSIGN TO "S115" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CARE-KEY
           LOCK MODE MANUAL.
           
           SELECT PROVCAID ASSIGN TO "S120" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROV-KEY
           ALTERNATE RECORD KEY IS PROV-NPI WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-TAX WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-NAME WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

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

       FD  CAREFILE.
           COPY CAREFILE.CPY IN "C:\Users\sid\cms\copylib".
       
       FD  HIPCLAIMFILE.
       01  HIPCLAIMFILE01.
           02 HIP-KEY PIC X.
           02 HIP-NUM PIC 9(9).

       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
    
       FD  PARMFILE.
       01  PARMFILE01 PIC X(75).

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
             03 FI-PROC3 PIC XX.
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
           02 FI-FREQ PIC X.
           02 FI-FUTURE PIC X(5).

       FD  SEGFILE.
       01  SEGFILE01 PIC X(160).

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri". 

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".    

       FD  INSFILE.
           COPY INSFILE.CPY IN "C:\Users\sid\cms\copylib".
       
       FD  MPLRFILE.
           COPY MPLRFILE.CPY IN "C:\Users\sid\cms\copylib\rri".
       
       FD  AUTHFILE.
           COPY AUTHFILE.CPY IN "C:\Users\sid\cms\copylib".
       
       FD  REFPHY.
           COPY REFPHY.CPY IN "C:\Users\sid\cms\copylib".

       FD  GAPFILE.
           COPY GAPFILE.CPY IN "C:\Users\sid\cms\copylib".    

       FD  DIAGFILE.
           COPY DIAGFILE.CPY IN "C:\Users\sid\cms\copylib".    

       WORKING-STORAGE SECTION.
            
           COPY HIP5010-837.CPY IN "C:\Users\sid\cms\copylib".

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
       01  END-FLAG PIC 9 VALUE 0.
       01  GAP-FLAG PIC 9.
       01  CNTR PIC 99.
       01  DIAG-CNTR PIC 99.
       01  DX-CNTR-PT PIC 99.
       01  X PIC 99.
       01  DIAG-X PIC X(7).
       01  Y PIC 99.
       01  A PIC 99.
       01  B PIC 99.
       01  C PIC 999.
       01  D PIC 999.
       01  ALF116 PIC X(116).

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
           02 FILETAB PIC X(160) OCCURS 50 TIMES.

       01  TAB11601.
           02 TAB116 PIC X OCCURS 116 TIMES.

       01  GROUP-3 PIC XXX.

       01  ALF1 PIC X.
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
       01  ALF-9 PIC X(9).
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

       01  DOC-TAB01.
           02 DOC-TAB02 OCCURS 90 TIMES.
             03 DOC-TAX PIC X(10).
             03 DOC-SS PIC X(9).
             03 DOC-NUM PIC X(8).
             03 DOC-LASTNAME PIC X(20).
             03 DOC-FIRSTNAME PIC X(10).
             03 DOC-MI PIC X.
             03 DOC-NPI PIC X(10).

       01  PARM01.
           02 PM-1 PIC XX.
           02 FILLER PIC X.
           02 PM-2 PIC X(10).
           02 FILLER PIC X.
           02 PM-3 PIC X(9).
           02 FILLER PIC X.
           02 PM-4 PIC X(8).
           02 FILLER PIC X.
           02 PM-5 PIC X(31).
           02 FILLER PIC X.
           02 PM-6 PIC X(10).

       01  EINSS PIC X(9).
       01  EINSS-TYPE PIC X.
       01  PARMLAST PIC X(15).
       01  PARMFIRST PIC X(15).
       01  PARMMIDDLE PIC X.
       01  ORG-NAME PIC X(40).
       01  ORG-CITY PIC X(30).
       01  ORG-STATE PIC XX.
       01  ORG-ZIP PIC X(9).
       01  INSTYPE-CODE PIC XXX.
       01  INSGROUP-CODE PIC X(10).
       01  INSGROUP-LEG PIC X(7).

       01  SUBMIT01.
           02 SUBMIT-1 PIC X(8).
           02 SUBMIT-2 PIC XX.

       01  TELE-PHONE PIC X(10).
       01  ORG-STREET PIC X(24).
       01  EIN-CODE PIC X(12).
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
       01  TOT-AMOUNT PIC 9(4)V99.
       01  PLACE-POINTER PIC 99.
       01  SAVE01 PIC X(160).
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
           02 HOLD-FREQ PIC X.
           02 HOLD-FUTURE PIC X(5).
       01  MAMMO-FLAG PIC 9.
       01  CLIA-NUM PIC X(12).
       01  GROUP-TAX PIC X(10).
       01  AGEX.
           02 AGEXYY PIC 9999.
           02 AGEXMMDD PIC XXXX.
       01  AGEY.
           02 AGEYYY PIC 9999.
           02 AGEYMMDD PIC XXXX.
       01  AGEZ PIC 999.
       01  CAS-REDUCEX PIC S9(4)V99.
       01  CAS-REDUCE01.
           02 CAS-REDUCE PIC S9(4)V99 OCCURS 50 TIMES.
       01  DDTAB01.
            02 DDTAB PIC 9 occurs 50 times.
       01  CAS-ALLOWED01.
           02 CAS-ALLOWED PIC S9(4)V99 OCCURS 50 TIMES.
       01  CAS-PAID01.
           02 CAS-PAID PIC S9(4)V99 OCCURS 50 TIMES.
       01  CLM-BAL01.
           02 CLM-BAL PIC S9(4)V99 OCCURS 50 TIMES.
       01  CAS-PAYDATE01.
           02 CAS-PAYDATE PIC X(8) OCCURS 50 TIMES.
       01  TOT-BAL PIC S9(4)V99.
       01  CAS-TOT-REDUCE PIC 9(4)V99.
       01  CAS-TOT-CHARGE PIC 9(4)V99.
       01  CAS-TOT-ALLOWED PIC 9(4)V99.
       01  CAS-TOT-PAID PIC 9(4)V99.
       01  PRIME-FLAG PIC 9.
       01  REDUCE-FLAG PIC 9.
       01  AMOUNT-X PIC S9(4)V99.
       01  MONTH-TABLE-CONS.
           05  FILLER PIC X(24) VALUE "312931303130313130313031".
       01  MONTH-TABLE REDEFINES MONTH-TABLE-CONS.
           05  DAYS-IN-MONTH OCCURS 12 TIMES PIC 99.
       01  X-DOB.
           02 X-YYYY PIC 9999.
           02 X-MM PIC 99.
           02 X-DD PIC 99.
       01  CAS-INS PIC X(3).
       PROCEDURE DIVISION.

       P0. 
           OPEN INPUT FILEIN GARFILE INSFILE REFPHY
             AUTHFILE MPLRFILE DIAGFILE PLACEFILE GAPFILE PARMFILE
             PARMFILE2 CAREFILE PAYCUR PROVCAID
           OPEN OUTPUT SEGFILE ERRFILE.
           OPEN I-O HIPCLAIMFILE
           OPEN I-O CHARCUR

           MOVE "A" TO HIP-KEY
           READ HIPCLAIMFILE WITH LOCK
             INVALID 
               DISPLAY "BAD HIPCLAIMFILE"
               GO TO P99.

           COMPUTE NUM9 = HIP-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO GS-NUM
           MOVE ALF9NUM TO GE-NUM
           ADD 1 TO HIP-NUM           
           PERFORM ISA-1 THRU ISA-EXIT
           PERFORM A0 THRU A0-EXIT.
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM ISA01
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO BHT-TIME GS-5.
           ACCEPT BHT-DATE FROM CENTURY-DATE.
           MOVE BHT-DATE TO GS-4.
       
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM GS01
           MOVE SPACE TO SEGFILE01
       
           COMPUTE NUM9 = HIP-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO ST-NUM
           MOVE ALF9NUM TO SE-NUM
       

           ADD 1 TO HIP-NUM
           WRITE SEGFILE01 FROM ST01.
       
           COMPUTE NUM9 = HIP-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO BHT-NUM
       
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM BHT01.
      *     MOVE "87 " TO REF-CODE
      *     MOVE "004010X098A1" TO REF-ID
      *     MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM REF01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBM01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBPER01.
           MOVE SPACE TO SEGFILE01
           MOVE "VT MEDICAID" TO INSNM-NAME
           MOVE "822287119" TO INSNM-NUM.
           WRITE SEGFILE01 FROM RECNM101.


       P00.
           READ PLACEFILE
             AT END
               GO TO P000.

           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           GO TO P00.

       P000.    
           READ FILEIN 
             AT END
               GO TO P98.

           MOVE FILEIN01 TO HOLD-FILEIN01
           PERFORM DF-SEARCH
           PERFORM 2000A THRU 2000B
           GO TO P0000-1.
       
       P0000.
           MOVE SAVE01 TO HOLD-FILEIN01.
           PERFORM DF-SEARCH.

       P0000-1.
           MOVE 0 TO CNTR DIAG-CNTR TOT-AMOUNT MAMMO-FLAG
           GO TO P1-1.

       P1. 
           READ FILEIN
             AT END
               MOVE 1 TO END-FLAG 
               display "set end-flag"
               GO TO P2.

       P1-1. 
           IF DIAG-CNTR > 11 GO TO P2.

           IF  FI-PLACE = HOLD-PLACE
             AND FI-KEY8 = HOLD-KEY8
             AND FI-PATID = HOLD-PATID
             AND FI-DOCP = HOLD-DOCP
             AND FI-DOCR = HOLD-DOCR
             AND FI-DAT1 = HOLD-DAT1
             AND FI-ACC-TYPE = HOLD-ACC-TYPE
             AND CNTR < 50
             PERFORM DIAG-1 THRU DIAG-EXIT 
             IF DIAG-CNTR > 12
               GO TO P2
             END-IF
             ADD 1 TO CNTR 
             MOVE FILEIN01 TO FILETAB(CNTR)
             ADD FI-AMOUNT TO TOT-AMOUNT
             GO TO P1.

       P2.  
           MOVE FILEIN01 TO SAVE01
           PERFORM 2300CLM 
           PERFORM HI-DIAG THRU HI-DIAG-EXIT
           PERFORM 2310A THRU 2310A-EXIT.
           
           IF EINSS-TYPE = "E" PERFORM 2310B. 
            
           PERFORM 2310D
           PERFORM 2320A THRU 2320A-EXIT

           MOVE 0 TO CAS-TOT-REDUCE 
           MOVE 0 TO CAS-TOT-CHARGE
           MOVE 0 TO CAS-TOT-ALLOWED
           MOVE 0 TO CAS-TOT-PAID
           MOVE 0 TO TOT-BAL
           MOVE "003" TO CAS-INS
           display "about to perform cas-tot, CNTR IS " CNTR
           accept omitted
           
           PERFORM CAS-TOT THRU CAS-TOT-EXIT
             VARYING X FROM 1 BY 1 UNTIL X > CNTR
           
           PERFORM 2320S THRU 2320S-EXIT

      *     MOVE G-SEINS TO CAS-INS
      *     PERFORM CAS-TOT THRU CAS-TOT-EXIT
      *       VARYING X FROM 1 BY 1 UNTIL X > CNTR
           
           PERFORM 2400SRV THRU 2400SRV-EXIT
             VARYING X FROM 1 BY 1 UNTIL X > CNTR
           
           IF END-FLAG = 1 GO TO P98.
           
           MOVE SAVE01 TO FILEIN01
           
           IF FI-DOCP NOT = HOLD-DOCP 
             MOVE FILEIN01 TO HOLD-FILEIN01
             PERFORM DOCP-1.
           
           MOVE FILEIN01 TO HOLD-FILEIN01
           PERFORM 2000B 
           GO TO P0000.
       
       DIAG-1.
           IF FI-DIAG = "0000000"
             GO TO DIAG-EXIT.

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

       DIAG-EXIT.
           EXIT.

       DIAG-2.
           IF DIAGTAB(X) = DIAG-X
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
           WRITE SEGFILE01 FROM PRV01
           PERFORM DOCP-1.

      *  BILLING PROVIDER/ADDRESS
       2010AA.
           MOVE "85 " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE "34" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE NM1-NAMEL NM1-NAMEF
           NM1-NAMEM NM1-NAMES
           MOVE DOC-LASTNAME(HOLD-DOCP) TO NM1-NAMEL
           MOVE DOC-FIRSTNAME(HOLD-DOCP) TO NM1-NAMEF
           MOVE DOC-MI(HOLD-DOCP) TO NM1-NAMEM
           MOVE DOC-SS(HOLD-DOCP) TO NM1-CODE
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM NM101.
      *     PERFORM PLACE-OF-SERVICE
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE PL-STREET(PLACE-POINTER) TO N3-STREET
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE PL-CITY(PLACE-POINTER) TO N4-CITY
           MOVE PL-STATE(PLACE-POINTER) TO N4-STATE
           MOVE PL-ZIP(PLACE-POINTER) TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF

           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM N401.
           MOVE DOC-NUM(HOLD-DOCP) TO REF-ID
           MOVE INSTYPE-CODE TO REF-CODE
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM REF01.

           MOVE CONTACT-NAME TO PER-CONTACT
           MOVE TELE-PHONE TO PER-PHONE
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM PER01.

      *   PAY-TO PROVIDER/ADDRESS
       2010AB.    
           IF EINSS-TYPE = "E"
           MOVE "2" TO NM1-SOLO
           MOVE ORG-NAME TO NM1-NAMEL
           MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE "24" TO NM1-EINSS
           ELSE 
           MOVE "1" TO NM1-SOLO
           MOVE PARMLAST TO NM1-NAMEL
           MOVE PARMFIRST TO NM1-NAMEF
           MOVE PARMMIDDLE TO NM1-NAMEM
           MOVE "34" TO NM1-EINSS.
           MOVE "XX" TO NM1-EINSS.
           MOVE "85" TO NM1-1
           MOVE SPACE TO NM1-CODE
           MOVE INSGROUP-CODE TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE ORG-STREET TO N3-STREET
           MOVE SPACE TO SEGFILE01
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
           MOVE SPACE TO REF-CODE REF-ID
           MOVE INSTYPE-CODE TO REF-CODE
           MOVE INSGROUP-LEG TO REF-ID
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM REF01.
           MOVE SPACE TO REF-CODE REF-ID
           IF EINSS-TYPE = "E"
            MOVE "EI" TO REF-CODE
           ELSE
            MOVE "SY" TO REF-CODE
           END-IF
           MOVE EIN-CODE TO REF-ID
           MOVE SPACE TO SEGFILE01.
           WRITE SEGFILE01 FROM REF01.

       2000B.
           ADD 1 TO HL-NUM
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE HL-NUMPRV-SAVE TO NUM5
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-PARENT
           MOVE "22  " TO HL-CODE
           PERFORM SUBSCRIBER-1 THRU SUBSCRIBER-EXIT.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01
      *     MOVE SPACE TO SEGFILE01
      *     MOVE "PE" TO PRV-1
      *     MOVE DOC-TAX(HOLD-DOCP) TO PRV-TAX
      *     WRITE SEGFILE01 FROM PRV01
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01
           PERFORM 2010BA.
           PERFORM 2010BB.

       2010BA.
           MOVE "IL " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
      *  when dxc has a diff name than 03 it's a name change game
      *  let's try using SE-NAME     
           UNSTRING MPLR-TR-NAME DELIMITED BY "; " OR ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III"
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
           MOVE MPLR-TRIPOL0 TO NM1-CODE
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
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF

           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE G-DOB TO DMG-DOB
           MOVE G-SEX TO DMG-GENDER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
       2010BB.
           MOVE "PR " TO NM1-1
           MOVE "2" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE "VT MEDICAID" TO NM1-NAMEL
           MOVE "PI" TO NM1-EINSS
           MOVE "822287119" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE "PO BOX 888" TO N3-STREET
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE "WILLISTON" TO N4-CITY
           MOVE "VT" TO N4-STATE
           MOVE "05495" TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.

       2300CLM.
           MOVE HOLD-KEY8 TO SUBMIT-1
           MOVE SUBMIT01 TO CLM-1
           COMPUTE NUM7 = TOT-AMOUNT
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO CLM-2
           MOVE SPACE TO CLM-11 CLM-COLON-ACCIDENT
           IF HOLD-DAT1 NOT = ZEROES
           PERFORM ACCIDENT-1 THRU ACCIDENT-EXIT.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM CLM01.
           IF HOLD-DAT1 NOT = ZEROES
           MOVE "439" TO DTP-1
           MOVE HOLD-DAT1 TO DTP-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DTP01.

           IF (CLM-5 = "21")
             MOVE "435" TO DTP-1
             IF HOLD-DATE-M = "00000000"
               MOVE "17760704" TO HOLD-DATE-M
             END-IF
             MOVE HOLD-DATE-M TO DTP-3
             MOVE SPACE TO SEGFILE01
             WRITE SEGFILE01 FROM DTP01
           END-IF

           IF MAMMO-FLAG= 1
           MOVE SPACE TO REF-CODE REF-ID SEGFILE01
           MOVE "EW" TO REF-CODE
           MOVE "134668    " TO REF-ID
           WRITE SEGFILE01 FROM REF01.


       ACCIDENT-1.
           MOVE "OA" TO CLM-11.

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
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF

           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
       2320A.
           DISPLAY "EXITING 2320A"
           GO TO 2320A-EXIT.
           IF G-SEINS = "001" OR "012" OR "075" OR "076"
           GO TO 2320A-EXIT.
           IF G-SEINS = "005" PERFORM CMP-1 
           GO TO 2320A-EXIT.
           IF G-SEINS = "004" OR "064" PERFORM CAID-1 
           GO TO 2320A-EXIT.
           MOVE 0 TO GAP-FLAG
           IF G-SEINS = "062" PERFORM GAP-1 THRU GAP-1-EXIT. 

       2320A-EXIT. 
           EXIT.

       CAS-TOT.
           DISPLAY "HERE WE ARE IN CAS-TOT"
           ACCEPT OMITTED
           MOVE FILETAB(X) TO FILEIN01.
           DISPLAY "CAS-INS IS " CAS-INS
           ACCEPT OMITTED
           
           IF CAS-INS NOT = "003" 
             GO TO CAS-TOT-1.
           DISPLAY "WERE GOING TO READ FROM CAREFILE"
           ACCEPT OMITTED
           MOVE 0 TO CAS-REDUCE(X) CAS-PAID(X) CLM-BAL(X) DDTAB(X)
           MOVE FI-AMOUNT TO CAS-ALLOWED(X)
           MOVE FI-KEY8 TO CR-KEY8
           MOVE FI-DATE-T TO CR-DATE
           MOVE FI-PROC1 TO CR-PROC
           MOVE FI-PROC2 TO CR-MOD1
           MOVE FI-PROC3 TO CR-MOD2
           READ CAREFILE
             INVALID 
               DISPLAY "INVALID CAREFILE READ " FILEIN01
               accept omitted
               GO TO CAS-TOT-EXIT
           END-READ
           
           MOVE CR-PAYDATE TO CAS-PAYDATE(X)
           COMPUTE CAS-ALLOWED(X) = CR-ALLOWED
           COMPUTE CAS-REDUCE(X)  = CR-BILLED - CR-ALLOWED 
           COMPUTE CAS-PAID(X) = CR-PAID
           ADD CR-BILLED TO CAS-TOT-CHARGE
           ADD CR-ALLOWED TO CAS-TOT-ALLOWED
           ADD CAS-REDUCE(X) TO CAS-TOT-REDUCE
           ADD CR-PAID TO CAS-TOT-PAID
           DISPLAY FI-AMOUNT " FI-AMOUNT"
           DISPLAY CAS-TOT-CHARGE " CAS-TOT-CHARGE"
           DISPLAY CAS-REDUCE(X) " CAS-REDUCE(X)"
           DISPLAY CAS-ALLOWED(X) " CAS-ALLOWED(X)"
           DISPLAY CAS-TOT-PAID "  CAS-TOT-PAID"
           DISPLAY CAS-TOT-REDUCE "  CAS-TOT-REDUCE"
           DISPLAY CAS-TOT-ALLOWED "  CAS-TOT-ALLOWED".
           ACCEPT omitted.
           GO TO CAS-TOT-EXIT.
      *    LET'S USE CAS-TOT-1 FOR 2NDARY PAYS, ABOVE IS 03 PRI PAYS
       CAS-TOT-1.
           DISPLAY "WERE GOING TO READ FROM paycur"
           ACCEPT OMITTED
           MOVE FI-KEY8 TO PC-KEY8
           MOVE SPACE TO PC-KEY3
           MOVE 0 TO REDUCE-FLAG PRIME-FLAG 
           MOVE 0 TO CAS-REDUCEX CAS-REDUCE(X) 
             CAS-PAID(X) DDTAB(X)
             CLM-BAL(X)
           MOVE FI-DATE-T TO CAS-PAYDATE(X).
           START PAYCUR KEY NOT < PAYCUR-KEY 
             INVALID
               GO TO CAS-TOT-EXIT.

       CAS-TOT-2. 
           READ PAYCUR NEXT AT END GO TO CAS-TOT-3.
           IF PC-KEY8 NOT = FI-KEY8 GO TO CAS-TOT-3.
           IF PC-CLAIM NOT = FI-CLAIM GO TO CAS-TOT-2.
           IF (PC-PAYCODE = G-SEINS AND PC-DENIAL = "14")
            OR (PC-PAYCODE = "014" OR "015")
      *     DISPLAY PC-AMOUNT
            COMPUTE CAS-REDUCE(X) = CAS-REDUCE(X) + PC-AMOUNT
           GO TO CAS-TOT-2.
           IF (PC-PAYCODE = G-SEINS)  
             AND (PC-DENIAL = "  " OR "DD" OR "07" OR "08")
             COMPUTE CAS-PAID(X) = CAS-PAID(X) + PC-AMOUNT
             MOVE PC-DATE-T TO CAS-PAYDATE(X)
             IF PC-DENIAL = "DD"
               MOVE 1 TO DDTAB(X)
             END-IF
           END-IF
           GO TO CAS-TOT-2.

       CAS-TOT-3.
      *     DISPLAY FI-AMOUNT " FI-AMOUNT"
      *     DISPLAY CAS-TOT-CHARGE " CAS-TOT-CHARGE"
      *     DISPLAY CAS-REDUCE(X) " CAS-REDUCE(X)"
      *     DISPLAY CAS-ALLOWED(X) " CAS-ALLOWED(X)"
      *     DISPLAY CAS-TOT-PAID "  CAS-TOT-PAID"
      *     DISPLAY CAS-TOT-REDUCE "  CAS-TOT-REDUCE"
      *     DISPLAY CAS-TOT-ALLOWED "  CAS-TOT-ALLOWED"
      *     DISPLAY " "
       
           ADD FI-AMOUNT TO CAS-TOT-CHARGE
           IF CAS-REDUCE(X) NOT < 0 MOVE 0 TO CAS-REDUCE(X).
           COMPUTE CLM-BAL(X) = FI-AMOUNT + CAS-REDUCE(X) 
             + CAS-PAID(X)
           COMPUTE CAS-ALLOWED(X) = FI-AMOUNT + CAS-REDUCE(X)
           COMPUTE CAS-TOT-PAID = CAS-TOT-PAID 
             + ( -1 * CAS-PAID(X)).
           COMPUTE CAS-TOT-REDUCE = CAS-TOT-REDUCE 
                     + (-1 *  CAS-REDUCE(X)) 
           COMPUTE CAS-TOT-ALLOWED = CAS-TOT-ALLOWED 
             + CAS-ALLOWED(X).
           COMPUTE TOT-BAL = TOT-BAL + CLM-BAL(X).
      *     DISPLAY FI-AMOUNT " FI-AMOUNT"
      *     DISPLAY CAS-TOT-CHARGE " CAS-TOT-CHARGE"
      *     DISPLAY CAS-REDUCE(X) " CAS-REDUCE(X)"
      *     DISPLAY CAS-ALLOWED(X) " CAS-ALLOWED(X)"
      *     DISPLAY CAS-TOT-PAID "  CAS-TOT-PAID"
      *     DISPLAY CAS-TOT-REDUCE "  CAS-TOT-REDUCE"
      *     DISPLAY CAS-TOT-ALLOWED "  CAS-TOT-ALLOWED".
      *     ACCEPT ALF1.

       CAS-TOT-EXIT.
           EXIT.

       2320S.
           MOVE "P" TO SBR-PST 
           MOVE "18" TO SBR-RELATE 
           MOVE G-PRINS TO INS-KEY
           MOVE "CI " TO SBR-INSCODE
           READ INSFILE
             INVALID 
               MOVE "COMMERCIAL INS" TO INS-NAME
           END-READ

           MOVE SPACE TO SBR-GRNAME
      *     MOVE INS-NAME TO SBR-GRNAME 

           IF G-PRINS = "003" OR "028" OR "200" OR "245" OR "074"
             MOVE "MB " TO SBR-INSCODE SBR-TYPE.

           IF G-PRINS = "006"
             MOVE "OF " TO SBR-INSCODE.
           
           IF G-PRINS = "141"
             MOVE "CH " TO SBR-INSCODE.
           
           IF (G-PRINS = "002" OR "268") OR (INS-CAID = "EE ")
             MOVE "BL " TO SBR-INSCODE.
             MOVE SPACE TO SBR-6 SBR-7 SBR-8 SBR-TYPE
             MOVE SPACE TO SEGFILE01
             WRITE SEGFILE01 FROM SBR01.
       
           MOVE SPACE TO AMT-1 AMT-2
           MOVE "D  " TO AMT-1
           COMPUTE NUM7 = CAS-TOT-PAID
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO AMT-2
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM AMT01       
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
       
           MOVE "IL " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           UNSTRING G-SENAME DELIMITED BY ";" INTO
             NM1-NAMEL NM1-NAMEF
           
           MOVE "MI" TO NM1-EINSS
           MOVE G-PRIPOL TO NM1-CODE
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
           
           IF N4-ZIP(6:4) = SPACE
             MOVE "9999" TO N4-ZIP(6:4)
           END-IF

           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE "PR " TO NM1-1
           MOVE "2" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE INS-NAME TO NM1-NAMEL
           MOVE "PI" TO NM1-EINSS
           MOVE INS-CAID TO NM1-CODE
           IF G-PRINS = "900"
             MOVE "BV" TO NM1-CODE 
           END-IF
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.

       2320S-EXIT.  
           EXIT.

       CMP-1.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           MOVE "18" TO SBR-RELATE
           MOVE "SP" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "MC" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
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
           END-IF

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
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF

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
           END-IF

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
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF

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
           READ GAPFILE
             INVALID MOVE 1 TO GAP-FLAG GO TO GAP-1-EXIT.
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
           WRITE SEGFILE01 FROM DMG01.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III" OR "IV"
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
           END-IF

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
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF

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
           MOVE SPACE TO tab11601
           MOVE 0 TO D
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 116
            IF  SV101(C:1) NOT = " "
              ADD 1 TO D
              MOVE SV101(C:1) TO TAB116(D)
            END-IF
           END-PERFORM
           MOVE tab11601 TO ALF116
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM ALF116.

           MOVE "472" TO DTP-1
           MOVE FI-DATE-T TO DTP-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DTP01.
           PERFORM 2420A.


           MOVE SPACE TO REF-CODE  REF-ID
           MOVE "6R" TO REF-CODE
           MOVE FILEIN-KEY TO REF-ID
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM REF01.
           MOVE INS-CAID TO SVD-1
           IF (G-PRINS = "900") 
            MOVE "BV " TO SVD-1
           END-IF

           COMPUTE NUM7 = CAS-PAID(X)
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO SVD-2
           MOVE SPACE TO SVD-3
           STRING "HC:" SV1-PROC SV1-MOD-FILLER DELIMITED BY SIZE
           INTO SVD-3
           MOVE SV1-WORK TO SVD-4
           MOVE SPACE TO tab11601
           MOVE 0 TO D
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 116
            IF  SVD01(C:1) NOT = " "
              ADD 1 TO D
              MOVE SVD01(C:1) TO TAB116(D)
            END-IF
           END-PERFORM
           MOVE tab11601 TO ALF116
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM ALF116.
           MOVE SPACE TO CAS-1 CAS-2 CAS-3
           MOVE "CO" TO CAS-1
           MOVE "45" TO CAS-2
           COMPUTE NUM7 = CAS-REDUCE(X)
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO CAS-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM CAS01
           MOVE SPACE TO CAS-1 CAS-2 CAS-3
           MOVE "PR" TO CAS-1
           MOVE "2 " TO CAS-2
           IF (DDTAB(X) = 1)
             OR (CAS-TOT-PAID = 0)
             MOVE "1 " TO CAS-2
           END-IF
           COMPUTE NUM7 = CLM-BAL(X)
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO CAS-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM CAS01
           MOVE "573" TO DTP-1
           MOVE CAS-PAYDATE(X) TO DTP-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DTP01
           MOVE FILEIN-KEY TO CHARCUR-KEY
           READ CHARCUR WITH LOCK 
             INVALID 
               DISPLAY "exiting 2400srv"
               GO TO 2400SRV-EXIT.
           IF CC-REC-STAT = "0" MOVE "2" TO CC-REC-STAT.
           IF CC-REC-STAT = "1" MOVE "3" TO CC-REC-STAT.
           MOVE BHT-DATE TO CC-DATE-A.
           MOVE "E" TO CC-PAPER
           REWRITE CHARCUR01.
           DISPLAY "exiting 2400srv".

       2400SRV-EXIT.
           EXIT.

       2420A.
           MOVE "82 " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE "XX" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE NM1-NAMEL NM1-NAMEF
           NM1-NAMEM NM1-NAMES
           MOVE DOC-LASTNAME(FI-DOCP) TO NM1-NAMEL
           MOVE DOC-FIRSTNAME(FI-DOCP) TO NM1-NAMEF
           MOVE DOC-MI(FI-DOCP) TO NM1-NAMEM
           MOVE DOC-NPI(FI-DOCP) TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE "PE" TO PRV-1
           MOVE DOC-TAX(FI-DOCP) TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.
      *     MOVE "1D" TO REF-CODE
      *     MOVE DOC-NUM(FI-DOCP) TO REF-ID
      *     MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM REF01.

       2310A.
           IF HOLD-DOCR = "000" GO TO REF-2.
           MOVE HOLD-DOCR TO REF-KEY
           READ REFPHY INVALID GO TO REF-2.
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
           MOVE REF-CDNUM TO PROV-KEY
           READ PROVCAID INVALID MOVE "207Q00000X" TO PROV-TAX.
      *     MOVE "RF" TO PRV-1
      *     MOVE "PXC" TO PRV-2
      *     MOVE PROV-TAX TO PRV-TAX
      *     MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM PRV01.
           GO TO 2310A-EXIT.

       2310B.
      *     MOVE "82 " TO NM1-1
      *     MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
      *     MOVE INSGROUP-CODE TO NM1-CODE
      *     MOVE ORG-NAME TO NM1-NAMEL
      *     MOVE "2" TO NM1-SOLO
      *     MOVE "XX" TO NM1-EINSS
      *     MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM NM101
      *     MOVE "PE" TO PRV-1
      *     MOVE "PXC" TO PRV-2
      *     MOVE GROUP-TAX TO PRV-TAX
      *     MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM PRV01.

       REF-2.
           MOVE "DN" TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE DOC-LASTNAME(HOLD-DOCP) TO NM1-NAMEL
           MOVE DOC-FIRSTNAME(HOLD-DOCP) TO NM1-NAMEF
           MOVE DOC-MI(HOLD-DOCP) TO NM1-NAMEM
           MOVE "XX" TO NM1-EINSS
           MOVE DOC-NPI(HOLD-DOCP) TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
      *     MOVE "RF" TO PRV-1
      *     MOVE "PXC" TO PRV-2
      *     MOVE DOC-TAX(HOLD-DOCP) TO PRV-TAX
      *     MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM PRV01.

       2310A-EXIT.
           EXIT.

       DOCP-1.
           MOVE DOC-LASTNAME(HOLD-DOCP) TO SAVE-DOCNM1-NAMEL 
           MOVE DOC-FIRSTNAME(HOLD-DOCP) TO SAVE-DOCNM1-NAMEF 
           MOVE DOC-MI(HOLD-DOCP) TO SAVE-DOCNM1-NAMES 
           MOVE DOC-SS(HOLD-DOCP) TO SAVE-DOCNM1-CODE
           MOVE "82 " TO SAVE-DOCNM1-1
           MOVE "1" TO SAVE-DOCNM1-SOLO
           MOVE SPACE TO SAVE-DOCNM1-NAMES 
           MOVE "34" TO SAVE-DOCNM1-EINSS 
           MOVE "1D " TO SAVE-DOCREF-CODE
           MOVE DOC-NPI(HOLD-DOCP) TO SAVE-DOCREF-ID.

       SUBSCRIBER-1.
           MOVE HOLD-KEY8 TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD BAD BAD"
           GO TO P99.

           IF G-TRINS NOT = "001"
             MOVE "0" TO MPLR-TR-RELATE
             MOVE G-GARNO TO MPLR-KEY
             READ MPLRFILE
               INVALID CONTINUE
             END-READ
           END-IF

           MOVE G-RELATE TO SUB-RELATE
      *  use SE NAME for dxc     
           MOVE G-SENAME TO SUB-NAME
           MOVE G-PRIPOL TO SUB-POLICY
           MOVE SPACE TO SUB-GROUP
           MOVE "P" TO SBR-PST

           IF HOLD-PAYCODE = G-SEINS 
             MOVE "S" TO SBR-PST
             MOVE G-SE-RELATE TO SUB-RELATE
             MOVE G-SENAME TO SUB-NAME
             MOVE G-SECPOL TO ALF-9
             MOVE ALF-9 TO SUB-POLICY
             MOVE G-SE-GROUP TO SUB-GROUP
           END-IF.

           IF (HOLD-PAYCODE = G-TRINS) AND (MPLR-TR-RELATE NOT = "0")
             MOVE "T" TO SBR-PST
             MOVE MPLR-TR-RELATE TO SUB-RELATE
             MOVE MPLR-TR-NAME TO SUB-NAME
             MOVE MPLR-TRIPOL TO SUB-POLICY
             MOVE MPLR-TR-GROUP TO SUB-GROUP
           END-IF.

           MOVE G-RELATE TO X-RELATE.
                      
           IF X-RELATE = "0"  
             MOVE G-RELATE TO X-RELATE.
           
           IF X-RELATE = SUB-RELATE  
             MOVE "18" TO SBR-RELATE
             GO TO SUBSCRIBER-2.

           IF (X-RELATE = "2" OR "K") 
             AND (SUB-RELATE = "2" OR "K")
             MOVE "01" TO SBR-RELATEHOLD 
             GO TO SUBSCRIBER-2.
           
           IF (X-RELATE = "8" OR "Q") 
             MOVE "29" TO SBR-RELATEHOLD
             GO TO SUBSCRIBER-2.
           
           IF (X-RELATE = "4" OR "M") 
             MOVE "02" TO SBR-RELATEHOLD
             GO TO SUBSCRIBER-2.

           IF (X-RELATE = "5" OR "N") MOVE "17" TO SBR-RELATEHOLD
             GO TO SUBSCRIBER-2.

       SUBSCRIBER-2.
           MOVE SPACE TO SBR-GROUP
           MOVE "0    " TO HL-CHILD
           MOVE "MC" TO SBR-INSCODE

           IF SBR-PST = "S"
             MOVE HOLD-PAYCODE TO INS-KEY
             READ INSFILE
               INVALID
                 MOVE SPACE TO INS-NAME
             END-READ
             MOVE INS-NAME TO SBR-GRNAME
             IF G-PRINS = "091"
               MOVE "12" TO SBR-TYPE
               IF HOLD-ACC-TYPE = "2"
                 MOVE "14" TO SBR-TYPE
               END-IF
               IF HOLD-ACC-TYPE = "1"
                 MOVE "15" TO SBR-TYPE
               END-IF
             END-IF
           END-IF.

           MOVE SPACE TO SBR-TYPE.

       SUBSCRIBER-EXIT.
           EXIT.

       2000B-PAT.

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
           MOVE  NUM7 TO ALF8Z
           MOVE SPACE TO ALF8NUM ALFS8
           MOVE ALF8Z TO ALF8
           UNSTRING ALF8 DELIMITED ALL " " INTO ALFS8 ALF8NUM.

       2000C.
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

       ISA-EXIT.
           EXIT.
      * TAX ID NUMBER 
       A0.
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO EIN-CODE.
      * TAX ID TYPE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO EINSS-TYPE.
      * CONTACT NAME #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO CONTACT-NAME.
      * TELEPHONE #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO TELE-PHONE.
      * INSURANCE-CODE #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO INSTYPE-CODE.
      * INSURANCE-GROUP #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO INSGROUP-CODE.
      * SUBMITER ID INDICATORS (2)
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO SUBMIT-2.
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
      *   CLIA-NUMBER
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO CLIA-NUM. 

      * ACCT-TAXONOMY.
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO GROUP-TAX.

      * INDIVIDUAL PROVIDER DATA
       A1. READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO PARM01
           MOVE PM-1 TO NUM2
           MOVE PM-2 TO DOC-TAX(NUM2)
           MOVE PM-3 TO DOC-SS(NUM2)
           MOVE PM-4 TO DOC-NUM(NUM2)
           MOVE SPACE TO ALF20 ALF10 ALF1
           UNSTRING PM-5 DELIMITED BY ";" INTO ALF20 ALF10 ALF1
           MOVE ALF20 TO DOC-LASTNAME(NUM2)
           MOVE ALF10 TO DOC-FIRSTNAME(NUM2)
           MOVE ALF1 TO DOC-MI(NUM2)
           MOVE PM-6 TO DOC-NPI(NUM2)
           GO TO A1.

       A0-EXIT.
           EXIT.

       DF-SEARCH. 
           MOVE 0 TO FLAG.
           MOVE "1" TO CC-PL
           MOVE "11" TO CLM-5
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.

       DF-SEARCH2.  
           IF HOLD-PLACE = PL-TAB(Y) 
           MOVE PL-NUM(Y) TO CC-PL
           MOVE Y TO PLACE-POINTER
           PERFORM PLACE-OF-SERVICE
           MOVE PLINDX TO Y.

       PLACE-OF-SERVICE.
           IF CC-PL = "1" MOVE "11" TO CLM-5.
           IF CC-PL = "3" MOVE "21" TO CLM-5.
           IF CC-PL = "4" MOVE "32" TO CLM-5.
           IF CC-PL = "5" MOVE "22" TO CLM-5.
           IF CC-PL = "6" MOVE "81" TO CLM-5.
           IF CC-PL = "7" MOVE "61" TO CLM-5.
           IF CC-PL = "8" MOVE "99" TO CLM-5.
           IF CC-PL = "E" MOVE "23" TO CLM-5 
           IF CC-PL = "K" MOVE "31" TO CLM-5.
           IF HOLD-PROC1 > "99200" AND < "99206" MOVE "11" TO CLM-5.
           IF HOLD-PROC1 > "99210" AND < "99216" MOVE "11" TO CLM-5.
           IF HOLD-PROC1 > "99216" AND < "99221" MOVE "22" TO CLM-5.
           IF HOLD-PROC1 > "99220" AND < "99224" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99230" AND < "99234" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 = "99238" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99240" AND < "99246" MOVE "11" TO CLM-5.
           IF HOLD-PROC1 > "99250" AND < "99256" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99260" AND < "99264" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99280" AND < "99289" MOVE "23" TO CLM-5.
           IF HOLD-PROC1 > "99320" AND < "99324" MOVE "33" TO CLM-5.
           IF HOLD-PROC1 > "99330" AND < "99334" MOVE "33" TO CLM-5.

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

       SV-MOD.
           MOVE SPACE TO SV1-MOD-FILLER MOD-ARRAY01
           MOVE FI-PROC2 TO X-MOD1
           MOVE FI-MOD2 TO X-MOD2
           MOVE FI-MOD3 TO X-MOD3.
           IF X-MOD1 = SPACE AND X-MOD2 = SPACE
              MOVE X-MOD3 TO X-MOD1
              MOVE SPACE TO X-MOD3.
           IF X-MOD1 = SPACE 
              MOVE X-MOD2 TO X-MOD1
              MOVE X-MOD3 TO X-MOD2
              MOVE SPACE TO X-MOD3.
           IF X-MOD2 = SPACE
              MOVE X-MOD3 TO X-MOD2
              MOVE SPACE TO X-MOD3.
           IF X-MOD1 NOT = SPACE
             AND X-MOD2 = SPACE
              MOVE X-MOD3 TO X-MOD2
              MOVE SPACE TO X-MOD3.
           IF X-MOD1 NOT = SPACE
             MOVE ":" TO MOD-C(1)
             MOVE X-MOD1 TO MOD-CODE(1).
           IF X-MOD2 NOT = SPACE
             MOVE ":" TO MOD-C(2)
             MOVE X-MOD2 TO MOD-CODE(2).
           IF X-MOD3 NOT = SPACE
             MOVE ":" TO MOD-C(3)
             MOVE X-MOD3 TO MOD-CODE(3).
             MOVE MOD-ARRAY01 TO SV1-MOD-FILLER.

       MAKE-IT-UP. 
           MOVE G-DOB TO X-DOB.
           ADD 1 TO X-YYYY
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
      *     DISPLAY G-DOB " " G-SE-RELATE " " G-GARNAME 
      *     DISPLAY X-DOB " " G-PR-RELATE " " G-PRNAME.
      *     ACCEPT ANS
           MOVE X-DOB TO DMG-DOB
           MOVE "M" TO DMG-GENDER
           IF G-PR-RELATE NOT NUMERIC 
             MOVE "F" TO DMG-GENDER.

       MAKE-IT-UP-EXIT.
           EXIT.

       E1. 
       P98.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SE01
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM GE01
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM IEA01.

       P99. 
      *     REWRITE HIPCLAIMFILE01.
           CLOSE GARFILE HIPCLAIMFILE CHARCUR ERRFILE.
           STOP RUN.
