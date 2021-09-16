      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hipr146.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PARMFILE ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT GARFILE ASSIGN TO "S45"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT PAYFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.

           SELECT ERROR-FILE ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.

           SELECT CAIDFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CAID-KEY
           LOCK MODE MANUAL.

           SELECT MPLRFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY
           LOCK MODE IS MANUAL.

           SELECT rarcfile ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS rarc-key
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
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
           02 MPLR-TR-GROUP PIC X(10).
           02 MPLR-TRIPOL0. 
              03 MPLR-TRIPOL PIC X(9).
              03 MPLR-TR-FILLER PIC X(7).
           02 MPLR-TR-NAME PIC X(24).
           02 MPLR-TR-RELATE PIC X.
           02 MPLR-FUTURE PIC X(6).
       FD  CAIDFILE
           BLOCK CONTAINS 3 RECORDS.
       01  CAIDFILE01.
           02 CAID-KEY PIC XXX.
           02 CAID-REASON PIC X(70).
       FD  PAYCUR
           BLOCK CONTAINS 6 RECORDS
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
       FD  PARMFILE.
       01  PARMFILE01.
           02 PF-1 PIC X(10).
           02 PF-2 PIC XXX.
           02 PARMCODE PIC XXX.
           02 PF-4 PIC X(27).
       FD ERROR-FILE.
       01 ERROR-FILE01. 
          02 ERROR-FILE01-1 PIC X(132).
          02 FO-SORT PIC X.
       FD FILEIN.
       01  FILEIN01.
           02 F0.
             03 F1 PIC XXX.
             03 F2 PIC X(4).
           02 F3 PIC X(113).
       FD  PAYFILE
           BLOCK CONTAINS 4 RECORDS
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
       FD  CHARCUR
      *    BLOCK CONTAINS 3 RECORDS
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
              03 CC-PROC0 PIC X(4).
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
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO.
             03 ID1 PIC XXX.
             03 ID2 PIC XXX.
             03 ID3 PIC XX.
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
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
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
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL0. 
              03 G-SECPOL PIC X(9).
              03 G-SE-FILLER PIC X(7).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).

       FD  rarcfile.
       01  rarcfile01.
           02 rarc-key pic x(8).
           02 rarc-reason pic x(112). 

       WORKING-STORAGE SECTION.

       COPY "hip5010_835.cpy" IN "C:\Users\sid\cms\copylib".      

       
       01  HL01.
           02 HL-1 PIC X(40) VALUE SPACE.
           02 FILLER PIC X(21) VALUE SPACE.
           02 HL-2 PIC X(27) VALUE "  MEDICAID UNPOSTED LIST   ".
           02 FILLER PIC X(5) VALUE SPACE.
           02 HL-3 PIC X(6).
       01  ERR01.
           02 EF1 PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 EF2 PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 EF3 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 EF-PROC PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 EF4 PIC X(12).
           02 FILLER PIC X VALUE SPACE.
           02 EF5 PIC ZZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 EFSIGN PIC X.
           02 EF6 PIC ZZZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 EF7 PIC X(15).
           02 FILLER PIC X VALUE SPACE.
           02 EF8 PIC X(3).
           02 FILLER PIC X VALUE SPACE.
           02 EF-DENIAL02.
            03 EF-DENIAL1 PIC XXX.
            03 FILLER PIC X VALUE SPACE.
            03 EF-DENIAL2 PIC XXX.
            03 FILLER PIC X VALUE SPACE.
            03 EF-DENIAL3 PIC XXX.
            03 FILLER PIC X VALUE SPACE.
            03 EF-DENIAL4 PIC XXX.
            03 FILLER PIC X VALUE SPACE.
            03 EF-DENIAL5 PIC XXX.
            03 FILLER PIC X VALUE SPACE.
            03 EF-DENIAL6 PIC XXX.
        01  ERR201.
           02 EF2-NUM PIC ZZ9.
           02 FILLER PIC XX VALUE SPACE.
           02 EF2-DENIAL PIC X(3).
           02 FILLER PIC XX VALUE SPACE.
           02 EF2-REASON PIC X(70).
        01  ERR301.
           02 EF3-DENIAL1 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 EF3-DENIAL2 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 EF3-DENIAL3 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 EF3-DENIAL4 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 EF3-DENIAL5 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 EF3-DENIAL6 PIC XXX.

       01  TITLE01.
           02 T1 PIC X(4) VALUE "NAME".
           02 FILLER PIC X(7) VALUE SPACE.
           02 T2 PIC X(5) VALUE "HIC #".
           02 FILLER PIC X(5) VALUE SPACE.
           02 T3 PIC X(6) VALUE " DATE ".
           02 FILLER PIC XXX VALUE SPACE.
           02 FILLER PIC X(8) VALUE " PROC   ".
           02 FILLER PIC X(13) VALUE "KEY          ".
           02 T5 PIC X(7) VALUE " AMOUNT".
           02 FILLER PIC XXX VALUE SPACE.
           02 T6 PIC X(7) VALUE "  PAID ".
           02 FILLER PIC X VALUE SPACE.
           02 T7 PIC X(6) VALUE "I.C.N.".
           02 FILLER PIC X(9) VALUE SPACE.
           02 T9 PIC X(10) VALUE "       DNL".
           02 FILLER PIC X VALUE SPACE.
           02 T12 PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 T13 PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 T14 PIC XXX.
       01  XYZ PIC 999.
       01  TOT-AMT PIC S9(4)V99.
       01  PAYBACK PIC X(80).
       01  KEY11.
           02 KEY3 PIC XXX.
           02 KEY8 PIC X(8).
       01  CLAIM-TOT PIC S9(4)V99.
       01  FLAG PIC 9 VALUE 0.
       01  FLAGEND PIC 9 VALUE 0.
       01  TOT-PAY PIC S9(5)V99 VALUE 0.
       01 FLAGY PIC 9.
       01 FIND-CNTR PIC 9.
       01 CNTRY PIC 99.
       01 CNTR PIC 99.
       01  MULTCHAR PIC 99.
       01  X PIC 99.
       01  Y PIC 99.
       01  YY PIC 99.
       01  Z PIC 999.
       01  A PIC 99.
       01 HOLDKEY PIC X(11).
       01 HOLDAMT PIC S9(4)V99.
       01  RIGHT-4 PIC X(4) JUST RIGHT.
       01  ALF-3 PIC XXX.
       01  ALF3 PIC XXX.
       01  ALF-7.
           02 ALF-71 PIC X(5).
           02 ALF-72 PIC XX.
       01  ALF-6 PIC X(6).
       01  NUM-6 PIC 9(6).
       01  AMOUNT-X PIC S9(4)V99.
       01  ALF8. 
           02 ALF8-1 PIC X.
           02 ALF8-7 PIC X(7).
       01  ALF7 PIC X(7).
       01  ALF10 PIC X(10).
       01  ALF-11.
           02 ALF-11-4 PIC X(4).
           02 ALF-11-7 PIC X(7).
       01  ALF-17.
           02 FILLER PIC XXX.
           02 ALF-14 PIC X(14).
       01  INPUT-DATE.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
       01  OVERFLAG PIC 9.
       01  CO-PAY-PAID PIC S9(7)V99.
       01 DATE-X PIC X(8).
       01 DATE-CC PIC X(8).
       01 SVC-CNTR PIC 99.
       01 CAS-CNTR PIC 99.
       01  LQ-CNTR PIC 99.

       01  SVC-TAB01.
           02 SVC-TAB PIC X(120) OCCURS 32 TIMES.

       01  SVC-DATE01.
           02 SVC-DATE PIC X(8) OCCURS 32 TIMES.

       01  FOUND-TAB01.
           02 FOUND-KEY PIC X(11) OCCURS 32 TIMES.

       01  CAS-TAB01.
           02 CAS-TAB PIC X(120) OCCURS 32 TIMES.

       01  CAS-SVC01.
           02 CAS-SVC PIC 99 OCCURS 32 TIMES.

        01 LQ-TAB01.
           02 LQ-TAB PIC X(120) OCCURS 32 TIMES.

       01  LQ-SVC01.
           02 LQ-SVC PIC 99 OCCURS 32 TIMES.   
           
       01 SAVEFILE01 PIC X(120).
       01 CC-PROC1X PIC X(5).
       01 CC-PROC2X PIC XX.
       01 CC-MOD2X PIC XX.
       01 CC-MOD3X PIC XX.
       01  CENTS PIC XX.
       01  SIGN-DOLLAR PIC XXXX.
       01 LNAME PIC X(24).
       01 FNAME PIC X(24).
       01  DISPLAY-DATE.
           05 T-MM  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-DD  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01  ORDER-8.
           02 ORDER-6 PIC X(6).
           02 FILLER PIC XX.
       01  NAR-KEY01.
           02 NAR-KEY PIC XXX OCCURS 216 TIMES.
       01  NAR-CNTR01.
           02 NAR-CNTR PIC 999 OCCURS 216 TIMES.
       01  DENIAL-CNTR PIC 99.
       01  EF-TAB01.
           02 EF-TAB PIC X(4) OCCURS 36 TIMES.
       01  WRITE-FLAG PIC 9 VALUE 0.
       01  PROV-1 PIC X(10).
       01  PROV-2 PIC X(7).

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN CHARCUR GARFILE MPLRFILE PARMFILE PAYCUR
             caidfile rarcfile.

           OPEN I-O PAYFILE 

           OPEN OUTPUT ERROR-FILE.

           MOVE SPACE TO NAR-KEY01 
           MOVE ALL ZEROES TO NAR-CNTR01
           MOVE SPACE TO ERROR-FILE01
           READ PARMFILE AT END GO TO P9.
           MOVE PARMFILE01 TO HL-1
           READ PARMFILE AT END GO TO P9.
           MOVE PF-2 TO T14.
           MOVE PF-1 TO T13.
           MOVE PF-1 TO PROV-1
           READ PARMFILE AT END GO TO P9.
           MOVE PF-1 TO PROV-2
           READ FILEIN AT END DISPLAY "NO RECORDS" GO TO P9.
           MOVE FILEIN01 TO PD-DATE-E.
           MOVE SPACE TO ERROR-FILE01
           MOVE "A" TO FO-SORT
           MOVE HL01 TO ERROR-FILE01-1
           MOVE PD-DATE-E TO TEST-DATE 
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO HL-3.
           WRITE ERROR-FILE01 AFTER PAGE.

       P00.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P9.
           IF F1 NOT = "BPR" GO TO P00.
           MOVE SPACE TO BPR01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           BPR-0 BPR-1 BPR-2 BPR-3 BPR-4 BPR-5 BPR-6 BPR-7 BPR-8 
           BPR-9 BPR-10 BPR-11 BPR-12 BPR-13 BPR-14 BPR-15 BPR-16.
           MOVE BPR-16 TO DATE-X
           MOVE SPACE TO ALF10 ALF7.
       P000.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P9.
           IF F1 = "LX*" GO TO XXX.
       XX. 
      *      IF (F1 = "REF" AND F2= "*EV*")
      *     MOVE SPACE TO REF01
      *     UNSTRING FILEIN01 DELIMITED BY "*" INTO
      *     REF-0 REF-1 REF-2
      *     MOVE REF-2 TO ALF7
      *     GO TO P000.
           
           IF F1 = "N1*" 
             MOVE SPACE TO N101
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               N1-0 N1-1 N1-2 N1-3 N1-ID
             IF N1-1 = "PE" AND N1-3 = "XX"
               MOVE N1-ID TO ALF10
             END-IF
           END-IF

           GO TO P000.

       XXX.
           IF (ALF10 NOT = PROV-1)
             GO TO P00.

           IF WRITE-FLAG = 0
             MOVE 1 TO WRITE-FLAG
             MOVE DATE-X TO TEST-DATE
             MOVE CORR TEST-DATE TO DISPLAY-DATE
             MOVE DISPLAY-DATE TO T12
             MOVE SPACE TO ERROR-FILE01
             MOVE TITLE01 TO ERROR-FILE01-1
             MOVE "B" TO FO-SORT
             WRITE ERROR-FILE01.

       P1-CLP. 
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P9.

           IF F1 NOT = "CLP" GO TO P1-CLP.

       P1-CLP-1.
           MOVE SPACE TO CLP01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
           CLP-0 CLP-1 CLP-2CLMSTAT CLP-3TOTCLMCHG CLP-4TOTCLMPAY 
           CLP-5PATRESP CLP-6PLANCODE CLP-7ICN CLP-8FACILITY 
           CLP-9FREQ CLP-10PATSTAT CLP-11DRG CLP-12QUAN CLP-13PERCENT.
           MOVE SPACE TO NM101 CLMCAS01.
           MOVE CLP-2CLMSTAT TO EF8
           MOVE 0 TO CAS-CNTR
           MOVE 0 TO SVC-CNTR
           move 0 to LQ-CNTR
           MOVE SPACE TO SVC-DATE01.
           
       P1-NM1.
           MOVE SPACE TO FILEIN01

           READ FILEIN AT END GO TO P9.

           IF F1 = "SVC" GO TO P1-SVC-LOOP-0.

           IF F1 = "CLP" OR "SE*" 
            MOVE FILEIN01 TO SAVEFILE01
            GO TO P2-SVC-LOOP.

           IF F1 = "CAS" 
             MOVE SPACE TO CAS01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
             CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
             CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
             CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
             MOVE CAS01 TO CLMCAS01
             MOVE SPACE TO CAS01
            GO TO P1-NM1.

           IF (F1 = "NM1" AND F2 = "*QC*") 
            MOVE SPACE TO NM101
            UNSTRING FILEIN01 DELIMITED BY "*" INTO
            NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF NM1-NAMEM 
            NM1-NAMES NM1-EINSS NM1-PREFIX NM1-CODE
           GO TO P1-NM1.

           IF F1 = "DTM" AND F2 = "*233"
            MOVE SPACE TO DTM01
            UNSTRING FILEIN01 DELIMITED BY "*" INTO
            DTM-0 DTM-1 DTM-2 
            MOVE DTM-2 TO DATE-CC.
           GO TO P1-NM1.

       P1-SVC-LOOP.  
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P2-SVC-LOOP.
           
           IF F1 = "CLP" OR "SE*" 
            MOVE FILEIN01 TO SAVEFILE01
            GO TO P2-SVC-LOOP.

       P1-SVC-LOOP-0.    
           IF F1 = "SVC" 
             iF FILEIN01(12:1) = "F"
               GO TO P1-SVC-LOOP
             END-IF

             IF (FILEIN01(8:5) = "G9500" OR "G9547" OR "G9548"
               OR "G9549" OR "G9550" OR "G9551" OR "G9552"
               OR "G9553" OR "G9554" OR "G9555" OR "G9556"
               OR "G9557" OR "G9637" OR "G1004")
               GO TO P1-SVC-LOOP
             END-IF        
             
             ADD 1 TO SVC-CNTR
             MOVE FILEIN01 TO SVC-TAB(SVC-CNTR)
             GO TO P1-SVC-LOOP
           end-if  
           
           IF F1 = "CAS" 
             ADD 1 TO CAS-CNTR
             MOVE FILEIN01 TO CAS-TAB(CAS-CNTR)
             MOVE SVC-CNTR TO CAS-SVC(CAS-CNTR)
             GO TO P1-SVC-LOOP.
           
           IF F1 = "DTM" AND F2 = "*150"
             MOVE SPACE TO DTM01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO 
               DTM-0 DTM-1 DTM-2
             MOVE DTM-2 TO SVC-DATE(SVC-CNTR)
             GO TO P1-SVC-LOOP.

           IF F1 = "LQ*" 
             ADD 1 TO LQ-CNTR
             MOVE FILEIN01 TO LQ-TAB(LQ-CNTR)
             MOVE SVC-CNTR TO LQ-SVC(LQ-CNTR)
           end-if

           GO TO P1-SVC-LOOP.
 
      * VALIDATE INCOMING DATA AGAINST CHARGES
       P2-SVC-LOOP.
           
           IF SVC-CNTR = 0 
             PERFORM P1-NO-SVC
             GO TO P9-SVC-LOOP
           end-if  

           MOVE CLP-1 TO G-GARNO
           
           READ GARFILE 
             INVALID
               GO TO P3-SVC-LOOP
           END-READ
           
           MOVE 0 TO FIND-CNTR 
           
           PERFORM LOOK-CHG THRU LOOK-CHG-EXIT VARYING X FROM 1
             BY 1 UNTIL X > SVC-CNTR
             
           IF FIND-CNTR = SVC-CNTR
             GO TO P4-SVC-LOOP
           end-if.

       P3-SVC-LOOP.    
           MOVE 0 TO FIND-CNTR

           PERFORM FIND-GARNO THRU FIND-GARNO-EXIT

           IF FIND-CNTR NOT = SVC-CNTR
             PERFORM P1-DENIED-SVC THRU P1-LOST-SVC
               VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
             GO TO P9-SVC-LOOP
           END-IF.

      * RECORDS ARE GOOD! START MAKING PAYMENT RECORDS.
       P4-SVC-LOOP.
           IF NOT (CLP-2CLMSTAT = "1" OR "2" OR "3" OR "4")
             PERFORM P1-DENIED-SVC THRU P1-LOST-SVC
             VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
             GO TO P9-SVC-LOOP.

           PERFORM P5-SVC-LOOP THRU P5-SVC-LOOP-EXIT 
             VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
           GO TO P9-SVC-LOOP.

       P5-SVC-LOOP.
           MOVE SPACE TO FILEIN01
           MOVE SVC-TAB(X) TO FILEIN01
           MOVE SPACE TO SVC01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT SVC-4NUBC 
           SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.
           MOVE SPACE TO ALF8
           MOVE SVC-3PAYAMT TO ALF8
           IF ALF8-1 = "-" 
           PERFORM P1-LOST-SVC GO TO P5-SVC-LOOP-EXIT.
           PERFORM AMOUNT-1
           MULTIPLY AMOUNT-X BY -1 GIVING PD-AMOUNT.

           IF PD-AMOUNT = 0
             PERFORM P1-LOST-SVC
             GO TO P5-SVC-LOOP-EXIT.

           MOVE FOUND-KEY(X) TO CHARCUR-KEY
           READ CHARCUR INVALID  
             PERFORM P1-LOST-SVC GO TO P5-SVC-LOOP-EXIT.
           MOVE CC-CLAIM TO PD-CLAIM
           MOVE DATE-X TO PD-DATE-T
           MOVE G-GARNAME TO PD-NAME.
           MOVE "004" TO PD-PAYCODE.
           IF G-PRINS = "064" 
           OR G-SEINS = "064"
           OR G-TRINS = "064"
           MOVE "064" TO PD-PAYCODE.
           MOVE "  " TO PD-DENIAL.
           ACCEPT ORDER-8 FROM TIME
           MOVE ORDER-6 TO PD-ORDER
           MOVE SPACE TO PD-BATCH
           MOVE G-GARNO TO PD-KEY8
           MOVE PAYFILE01 TO PAYBACK.
           MOVE 0 TO XYZ.
       P3.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO P4.
           GO TO P3.
       P4.
           MOVE PAYBACK TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01.
           COMPUTE CLAIM-TOT = CC-AMOUNT + PD-AMOUNT
           PERFORM S4 THRU S5
           IF CLAIM-TOT NOT > 0 GO TO P5-SVC-LOOP-EXIT.
           COMPUTE PD-AMOUNT = -1 * CLAIM-TOT
           MOVE "14" TO PD-DENIAL
           MOVE PAYFILE01 TO PAYBACK.
       P4-0.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO P4-1.
           GO TO P4-0.
       P4-1. 
           MOVE PAYBACK TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01.
       P5-SVC-LOOP-EXIT.
           EXIT.
       P9-SVC-LOOP.
           MOVE SAVEFILE01 TO FILEIN01
           IF F1 = "CLP" GO TO P1-CLP-1.
           MOVE SPACE TO ALF10 ALF7.
           GO TO P000.
       P1-NO-SVC.
           MOVE SPACE TO EF1
           STRING NM1-NAMEL ";" NM1-NAMEF 
           DELIMITED BY "  " INTO EF1
           MOVE NM1-CODE(1:9) TO EF2
           MOVE DATE-CC TO TEST-DATE 
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO EF3
           MOVE CLP-1 TO EF4
           MOVE SPACE TO ALF8
           MOVE CLP-3TOTCLMCHG TO ALF8
           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO EF5
           MOVE SPACE TO ALF8
           MOVE CLP-4TOTCLMPAY TO ALF8 
           PERFORM AMOUNT-1
           MOVE SPACE TO EFSIGN
           IF ALF8-1 = "-"
           MOVE "-" TO EFSIGN
           END-IF
           IF EFSIGN NOT = "-"
           ADD AMOUNT-X TO TOT-PAY
           END-IF
           MOVE AMOUNT-X TO EF6
           MOVE CLP-7ICN TO EF7
           MOVE CLP-2CLMSTAT TO EF8
           MOVE SPACE TO EF-PROC
           MOVE CLMCAS-2 TO EF-DENIAL1
           MOVE CLMCAS-5 TO EF-DENIAL2
           MOVE CLMCAS-8 TO EF-DENIAL3
           MOVE CLMCAS-11 TO EF-DENIAL4
           MOVE CLMCAS-14 TO EF-DENIAL5
           MOVE CLMCAS-17 TO EF-DENIAL6
           MOVE SPACE TO ERROR-FILE01
           IF EF-DENIAL1 = "42 "
            MOVE EF-DENIAL2 TO EF-DENIAL1
            MOVE SPACE TO EF-DENIAL2
           END-IF
           IF EF-DENIAL2 = "42 "
            MOVE EF-DENIAL3 TO EF-DENIAL2
            MOVE SPACE TO EF-DENIAL3
           END-IF
           IF EF-DENIAL2 NOT = SPACE
           AND EF-DENIAL1 = SPACE
            MOVE EF-DENIAL2 TO EF-DENIAL1
            MOVE SPACE TO EF-DENIAL2
           END-IF
           IF EF-DENIAL3 NOT = SPACE
           AND EF-DENIAL2 = SPACE
            MOVE EF-DENIAL3 TO EF-DENIAL2
            MOVE SPACE TO EF-DENIAL3
           END-IF
           MOVE "C" TO FO-SORT
           MOVE ERR01 TO ERROR-FILE01-1
           WRITE ERROR-FILE01
           IF CLMCAS-2 NOT = SPACE
            MOVE CLMCAS-2 TO ALF3
            PERFORM NAR-1.
           IF CLMCAS-5 NOT = SPACE
            MOVE CLMCAS-5 TO ALF3
            PERFORM NAR-1.
           IF CLMCAS-8 NOT = SPACE
            MOVE CLMCAS-8 TO ALF3
            PERFORM NAR-1.
           IF CLMCAS-11 NOT = SPACE
           MOVE CLMCAS-11 TO ALF3
            PERFORM NAR-1.
           IF CLMCAS-14 NOT = SPACE
            MOVE CLMCAS-14 TO ALF3
            PERFORM NAR-1.
           IF CLMCAS-17 NOT = SPACE
            MOVE CLMCAS-17 TO ALF3
            PERFORM NAR-1.
       P1-DENIED-SVC.
           MOVE SPACE TO SVC01 
           MOVE SVC-TAB(X) TO FILEIN01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT SVC-4NUBC 
           SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.

       P1-LOST-SVC.
           MOVE SPACE TO EF1
           STRING NM1-NAMEL ";" NM1-NAMEF 
             DELIMITED BY "  " INTO EF1
           MOVE NM1-CODE(1:9) TO EF2
           IF SVC-DATE(X) = SPACE
             MOVE DATE-CC TO SVC-DATE(X)
           END-IF
           
           MOVE SVC-DATE(X) TO TEST-DATE 
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO EF3
           MOVE CLP-1 TO EF4
           MOVE SPACE TO ALF8
           MOVE SVC-2CHRGAMT TO ALF8
           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO EF5
           MOVE SPACE TO ALF8
           MOVE SVC-3PAYAMT TO ALF8
           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO EF6
           
           IF ALF8-1 NOT = "-"
             COMPUTE TOT-PAY = TOT-PAY + AMOUNT-X
           END-IF.
           
           MOVE SPACE TO EFSIGN
           
           IF ALF8-1 = "-"
             MOVE "-" TO EFSIGN.
           
           MOVE CLP-7ICN TO EF7
           MOVE SPACE TO ALF-17 CC-PROC1X CC-PROC2X CC-MOD2X CC-MOD3X
           
           MOVE SVC-1PROCMOD TO ALF-17
           UNSTRING ALF-14 DELIMITED BY ":" INTO 
             CC-PROC1X CC-PROC2X CC-MOD2X CC-MOD3X
           MOVE SPACE TO EF-PROC
           
           STRING CC-PROC1X CC-PROC2X CC-MOD2X CC-MOD3X 
             DELIMITED BY SIZE INTO EF-PROC
           display ef-proc
           accept omitted
             
           MOVE SPACE TO EF-TAB01
           MOVE 0 TO DENIAL-CNTR 
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > CAS-CNTR
             IF CAS-SVC(Y) = X 
               MOVE SPACE TO FILEIN01
               MOVE CAS-TAB(Y) TO FILEIN01
               MOVE SPACE TO CAS01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
               CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
               CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
               CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
               IF CAS-2 NOT = SPACE
                 ADD 1 TO DENIAL-CNTR
                 MOVE CAS-2 TO EF-TAB(DENIAL-CNTR)
                 MOVE CAS-2 TO ALF3
                 PERFORM NAR-1
               END-IF
               IF CAS-5 NOT = SPACE
                 ADD 1 TO DENIAL-CNTR
                 MOVE CAS-5 TO EF-TAB(DENIAL-CNTR)
                 MOVE CAS-5 TO ALF3
                 PERFORM NAR-1
               END-IF
               IF CAS-8 NOT = SPACE
                 ADD 1 TO DENIAL-CNTR
                 MOVE CAS-8 TO EF-TAB(DENIAL-CNTR)
                 MOVE CAS-8 TO ALF3
                 PERFORM NAR-1
               END-IF

               IF CAS-11 NOT = SPACE
                 ADD 1 TO DENIAL-CNTR
                 MOVE CAS-11 TO EF-TAB(DENIAL-CNTR)
                 MOVE CAS-11 TO ALF3
                 PERFORM NAR-1
               END-IF

               IF CAS-14 NOT = SPACE
                 ADD 1 TO DENIAL-CNTR
                 MOVE CAS-14 TO EF-TAB(DENIAL-CNTR)
                 MOVE CAS-14 TO ALF3
                 PERFORM NAR-1
               END-IF

               IF CAS-17 NOT = SPACE
                 ADD 1 TO DENIAL-CNTR
                 MOVE CAS-17 TO EF-TAB(DENIAL-CNTR)
                 MOVE CAS-17 TO ALF3
                 PERFORM NAR-1
               END-IF
             END-IF
           END-PERFORM.

           MOVE EF-TAB(1) TO EF-DENIAL1
           MOVE EF-TAB(2) TO EF-DENIAL2
           MOVE EF-TAB(3) TO EF-DENIAL3
           MOVE EF-TAB(4) TO EF-DENIAL4
           MOVE EF-TAB(5) TO EF-DENIAL5
           MOVE EF-TAB(6) TO EF-DENIAL6
           MOVE SPACE TO ERROR-FILE01

           IF EF-DENIAL1 = "42 "
            MOVE EF-DENIAL2 TO EF-DENIAL1
            MOVE SPACE TO EF-DENIAL2
           END-IF
           IF EF-DENIAL2 = "42 "
            MOVE EF-DENIAL3 TO EF-DENIAL2
            MOVE SPACE TO EF-DENIAL3
           END-IF
           IF EF-DENIAL2 NOT = SPACE
           AND EF-DENIAL1 = SPACE
            MOVE EF-DENIAL2 TO EF-DENIAL1
            MOVE SPACE TO EF-DENIAL2
           END-IF
           IF EF-DENIAL3 NOT = SPACE
           AND EF-DENIAL2 = SPACE
            MOVE EF-DENIAL3 TO EF-DENIAL2
            MOVE SPACE TO EF-DENIAL3
           END-IF
             MOVE "C" TO FO-SORT
             MOVE ERR01 TO ERROR-FILE01-1
             WRITE ERROR-FILE01.

             IF DENIAL-CNTR > 6
             MOVE EF-TAB(7) TO EF3-DENIAL1
             MOVE EF-TAB(8) TO EF3-DENIAL2
             MOVE EF-TAB(9) TO EF3-DENIAL3
             MOVE EF-TAB(10) TO EF3-DENIAL4
             MOVE EF-TAB(11) TO EF3-DENIAL5
             MOVE EF-TAB(12) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
           IF EF3-DENIAL1 = "42 "
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL2 = "42 "
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
           IF EF3-DENIAL2 NOT = SPACE
           AND EF3-DENIAL1 = SPACE
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL3 NOT = SPACE
           AND EF3-DENIAL2 = SPACE
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF

           IF EF3-DENIAL2 = "42 "
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF

             MOVE "C" TO FO-SORT
             MOVE ERR301 TO ERROR-FILE01-1
             WRITE ERROR-FILE01.
             IF DENIAL-CNTR > 12
             MOVE EF-TAB(13) TO EF3-DENIAL1
             MOVE EF-TAB(14) TO EF3-DENIAL2
             MOVE EF-TAB(15) TO EF3-DENIAL3
             MOVE EF-TAB(16) TO EF3-DENIAL4
             MOVE EF-TAB(17) TO EF3-DENIAL5
             MOVE EF-TAB(18) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
           IF EF3-DENIAL1 = "42 "
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL2 = "42 "
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
           IF EF3-DENIAL2 NOT = SPACE
           AND EF3-DENIAL1 = SPACE
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL3 NOT = SPACE
           AND EF3-DENIAL2 = SPACE
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
             MOVE "C" TO FO-SORT
             MOVE ERR301 TO ERROR-FILE01-1
             WRITE ERROR-FILE01. 
             IF DENIAL-CNTR > 18
             MOVE EF-TAB(19) TO EF3-DENIAL1
             MOVE EF-TAB(20) TO EF3-DENIAL2
             MOVE EF-TAB(21) TO EF3-DENIAL3
             MOVE EF-TAB(22) TO EF3-DENIAL4
             MOVE EF-TAB(23) TO EF3-DENIAL5
             MOVE EF-TAB(24) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
           IF EF3-DENIAL1 = "42 "
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL2 = "42 "
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
           IF EF3-DENIAL2 NOT = SPACE
           AND EF3-DENIAL1 = SPACE
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL3 NOT = SPACE
           AND EF3-DENIAL2 = SPACE
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
           IF EF3-DENIAL2 NOT = SPACE
           AND EF3-DENIAL1 = SPACE
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL3 NOT = SPACE
           AND EF3-DENIAL2 = SPACE
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
             MOVE "C" TO FO-SORT
             MOVE ERR301 TO ERROR-FILE01-1
             WRITE ERROR-FILE01. 
             IF DENIAL-CNTR > 24
             MOVE EF-TAB(25) TO EF3-DENIAL1
             MOVE EF-TAB(26) TO EF3-DENIAL2
             MOVE EF-TAB(27) TO EF3-DENIAL3
             MOVE EF-TAB(28) TO EF3-DENIAL4
             MOVE EF-TAB(29) TO EF3-DENIAL5
             MOVE EF-TAB(30) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
           IF EF3-DENIAL1 = "42 "
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL2 = "42 "
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
           IF EF3-DENIAL2 NOT = SPACE
           AND EF3-DENIAL1 = SPACE
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL3 NOT = SPACE
           AND EF3-DENIAL2 = SPACE
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
             MOVE "C" TO FO-SORT
             MOVE ERR301 TO ERROR-FILE01-1
             WRITE ERROR-FILE01. 
             IF DENIAL-CNTR > 30
             MOVE EF-TAB(31) TO EF3-DENIAL1
             MOVE EF-TAB(32) TO EF3-DENIAL2
             MOVE EF-TAB(33) TO EF3-DENIAL3
             MOVE EF-TAB(34) TO EF3-DENIAL4
             MOVE EF-TAB(35) TO EF3-DENIAL5
             MOVE EF-TAB(36) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
           IF EF3-DENIAL1 = "42 "
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL2 = "42 "
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
           IF EF3-DENIAL2 NOT = SPACE
           AND EF3-DENIAL1 = SPACE
            MOVE EF3-DENIAL2 TO EF3-DENIAL1
            MOVE SPACE TO EF3-DENIAL2
           END-IF
           IF EF3-DENIAL3 NOT = SPACE
           AND EF3-DENIAL2 = SPACE
            MOVE EF3-DENIAL3 TO EF3-DENIAL2
            MOVE SPACE TO EF3-DENIAL3
           END-IF
             MOVE "C" TO FO-SORT
             MOVE ERR301 TO ERROR-FILE01-1
             WRITE ERROR-FILE01. 

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LQ-CNTR
             IF LQ-SVC(Y) = X               
               MOVE SPACE TO FILEIN01
               MOVE LQ-TAB(Y) TO FILEIN01
               MOVE SPACE TO LQ01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                 LQ-0 LQ-1 LQ-2 
               IF NOT (LQ-2 = SPACE OR "N807" OR "MA130" or "N620")
                 MOVE LQ-2 TO rarc-key
                 READ rarcfile with lock
                   invalid
                     continue
                 end-read
                 MOVE SPACE TO ERROR-FILE01
                 STRING rarc-reason DELIMITED BY size INTO ERROR-FILE01
                 WRITE ERROR-FILE01
               end-if   
             end-if                  
           end-perform.   

       NAR-1.
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 216 
            IF ALF3 = NAR-KEY(Z)
            ADD 1 TO NAR-CNTR(Z)
            MOVE 216 TO Z
            END-IF
            IF NAR-KEY(Z) = SPACE
            MOVE ALF3 TO NAR-KEY(Z)
            MOVE 1 TO NAR-CNTR(Z)
            MOVE 216 TO Z
            END-IF
           END-PERFORM.

      *  NO MATCH ON GARNO FROM CLM-1 RETURNED BY PAYOR.
      *  SEARCH THROUGH GARFILE MATCHING POLICY NUMBER.
      *  AND THEN FINDING MATCH ON ALL CHARGES.
      *  ALL MUST MATCH WITHIN AN ACCOUNT OR THE WHOLE
      *  CLAIM IT DROPPED TO THE ERROR FILE.
      *  I.E., PERFECTION MUST REIGN!

       FIND-GARNO.    
           MOVE SPACE TO G-GARNO
           MOVE NM1-NAMEL(1:3) TO ID1
           MOVE ID1 TO ALF-3
           START GARFILE KEY NOT < G-GARNO INVALID 
           GO TO FIND-GARNO-EXIT.
       P2.  READ GARFILE NEXT AT END GO TO FIND-GARNO-EXIT.
           IF ID1 > ALF-3 GO TO FIND-GARNO-EXIT.
           IF NOT 
           ((G-PRINS = "004" OR "064") OR  (G-SEINS = "004" OR "064")
                 OR (G-TRINS = "004" OR "064"))
           GO TO P2.
           IF G-TRINS = "004" OR "064"
            MOVE G-GARNO TO MPLR-KEY
            READ MPLRFILE INVALID MOVE SPACE TO MPLR-TRIPOL
            END-READ
           END-IF.
           IF NOT 
           ((G-PRIPOL = NM1-CODE(1:9)) OR (G-SECPOL = NM1-CODE(1:9)) 
           OR (MPLR-TRIPOL = NM1-CODE(1:9)))
           GO TO P2.

      *  START LOOKING FOR MATCHING CHARGES WITH THE GARNO IN QUESTION.
           
           MOVE 0 TO FIND-CNTR
           PERFORM LOOK-CHG THRU LOOK-CHG-EXIT VARYING X FROM 1
            BY 1 UNTIL X > SVC-CNTR.
       FIND-GARNO-EXIT.
           EXIT.
       
       LOOK-CHG.
           MOVE SPACE TO SVC01 FILEIN01
           MOVE SVC-TAB(X) TO FILEIN01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT SVC-4NUBC 
           SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.
           MOVE SPACE TO ALF-17
           MOVE SVC-1PROCMOD TO ALF-17.
           MOVE SPACE TO CC-PROC1X CC-PROC2X CC-MOD2X CC-MOD3X
           UNSTRING ALF-14 DELIMITED BY ":" INTO CC-PROC1X
           CC-PROC2X CC-MOD2X CC-MOD3X.
           MOVE G-GARNO TO CC-KEY8 MOVE "000" TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID 
              GO TO LOOK-CHG-EXIT.
       LOOK-1. 
           READ CHARCUR NEXT AT END GO TO LOOK-CHG-EXIT.
           IF CC-KEY8 NOT = G-GARNO GO TO LOOK-CHG-EXIT.
           IF CC-PAYCODE NOT = "004" AND NOT = "064" GO TO LOOK-1.
           IF CC-PROC1 NOT = CC-PROC1X GO TO LOOK-1.
           IF CC-PROC2 NOT = CC-PROC2X GO TO LOOK-1.
           IF CC-MOD2  NOT = CC-MOD2X  GO TO LOOK-1.
           IF CC-MOD3  NOT = CC-MOD3X  GO TO LOOK-1.
      *     IF (CC-PROC2 = SPACE) AND (CC-MOD2 NOT = SPACE)
      *      MOVE CC-MOD2 TO CC-PROC2 
      *      MOVE SPACE TO CC-MOD2.
      *     IF NOT ((CC-PROC2 = CC-PROC2X) 
      *      OR ((CC-PROC2 = SPACE) AND (CC-PROC2X = "EP" OR "QW")))
      *        GO TO LOOK-1.
           IF NOT ((CC-DATE-T = DATE-CC) OR (CC-DATE-T = SVC-DATE(X)))
           GO TO LOOK-1.
           MOVE 0 TO FLAGY 
           PERFORM A5 THRU A5-EXIT
           IF FLAGY = 1 GO TO LOOK-1.

           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > FIND-CNTR
            IF CHARCUR-KEY = FOUND-KEY(Z)
             MOVE 1 TO FLAGY
             MOVE FIND-CNTR TO Z
            END-IF
           END-PERFORM
           IF FLAGY = 1 GO TO LOOK-1.
           ADD 1 TO FIND-CNTR
           MOVE CHARCUR-KEY TO FOUND-KEY(X).
       LOOK-CHG-EXIT.
           EXIT.
       A5. MOVE G-GARNO TO PD-KEY8 MOVE "000" TO PD-KEY3.
           START PAYFILE KEY NOT < PAYFILE-KEY INVALID GO TO A5-EXIT.
       A5-1. READ PAYFILE NEXT AT END GO TO A5-EXIT.
           IF PD-KEY8 NOT = CC-KEY8 GO TO A5-EXIT.
           IF PD-CLAIM NOT = CC-CLAIM
           GO TO A5-1.
           MOVE 1 TO FLAGY.
       A5-EXIT. EXIT.
       S4. MOVE CC-KEY8 TO PC-KEY8 MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT <  PAYCUR-KEY INVALID GO TO S5.
       S41. READ PAYCUR NEXT AT END GO TO S5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S5.
           IF PC-CLAIM NOT = CC-CLAIM GO TO S41. 
           ADD PC-AMOUNT TO CLAIM-TOT.
           GO TO S41.
       S5. EXIT.
       AMOUNT-1.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           IF ALF8-1 = "-"
           UNSTRING ALF8-7 DELIMITED BY "." INTO SIGN-DOLLAR CENTS
           ELSE
           UNSTRING ALF8 DELIMITED BY "." INTO SIGN-DOLLAR CENTS
           END-IF.
           INSPECT CENTS REPLACING ALL " " BY "0".
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING AMOUNT-X.
           IF ALF8-1 = "-"
           COMPUTE AMOUNT-X = -1 * AMOUNT-X.
       P9. MOVE "UNPOSTED" TO EF1
           MOVE "PAYMENTS" TO EF2
           MOVE "TOTAL" TO EF3
           MOVE "  =" TO EF4
           MOVE 0 TO EF5
           MOVE TOT-PAY TO EF6
           MOVE SPACE TO  EF7 EF8 EF-PROC
           MOVE SPACE TO ERROR-FILE01 
           MOVE "E" TO FO-SORT
           WRITE ERROR-FILE01
           MOVE ERR01 TO ERROR-FILE01-1
           MOVE "F" TO FO-SORT
           WRITE ERROR-FILE01. 
           MOVE SPACE TO ERROR-FILE01
           MOVE "DENIAL REASONS SUMMARY" TO ERROR-FILE01-1
           MOVE "G" TO FO-SORT
           WRITE ERROR-FILE01 
           MOVE SPACE TO ERROR-FILE01
           MOVE "H" TO FO-SORT
           WRITE ERROR-FILE01 


           MOVE SPACE TO ERROR-FILE01
           MOVE "I" TO FO-SORT
           WRITE ERROR-FILE01
           MOVE "FREQ  KEY  DESCRIPTION " TO ERROR-FILE01-1
           MOVE "J" TO FO-SORT
           WRITE ERROR-FILE01
           MOVE SPACE TO ERROR-FILE01
           MOVE "K" TO FO-SORT
           WRITE ERROR-FILE01

           PERFORM VARYING Z FROM 1 BY 1 UNTIL NAR-KEY(Z) = SPACE
            MOVE NAR-KEY(Z) TO CAID-KEY
             READ CAIDFILE INVALID MOVE SPACE TO CAID-REASON
             END-READ
            MOVE CAID-KEY TO EF2-DENIAL
            MOVE NAR-CNTR(Z) TO EF2-NUM 
            MOVE CAID-REASON TO EF2-REASON
            MOVE SPACE TO ERROR-FILE01
            MOVE "L" TO FO-SORT
            MOVE ERR201 TO ERROR-FILE01-1
            WRITE ERROR-FILE01 
           END-PERFORM
           CLOSE filein CHARCUR garfile mplrfile parmfile paycur
            caidfile rarcfile payfile error-file.
           STOP RUN.
