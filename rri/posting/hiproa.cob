      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hiproa.
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

           SELECT INSFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT TRNPAYFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TRNPAYFILE-KEY
           LOCK MODE MANUAL.

           SELECT rarcfile ASSIGN TO "S85" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS rarc-key
           LOCK MODE MANUAL.   

           SELECT REMITFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REMIT-KEY.

       DATA DIVISION.
       FILE SECTION.

       FD  REMITFILE.
       01  REMITFILE01.
           02 REMIT-KEY PIC X(68).
           02 REMIT-DATE-E PIC X(8).
           02 REMIT-DATE-P PIC X(8).

       FD  rarcfile.
       01  rarcfile01.
           02 rarc-key pic x(8).
           02 rarc-reason pic x(112).   

       FD  INSFILE.
           COPY INSFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  MPLRFILE.
           COPY MPLRFILE.CPY IN "C:\Users\sid\cms\copylib\rri".           

       FD  CAIDFILE.
       01  CAIDFILE01.
           02 CAID-KEY PIC XXX.
           02 CAID-REASON PIC X(70).

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib\rri".
      
       FD  PARMFILE.
       01  PARMFILE01.
           02 PF-1 PIC X(9).
           02 PF-2 PIC XXX.
           02 PARMCODE PIC XXX.
           02 PF-3 PIC X(27).

       FD  ERROR-FILE.
       01  ERROR-FILE01 PIC X(150).

       FD  FILEIN.
       01  FILEIN01.
           02 F0.
             03 F1 PIC XXX.
             03 F2. 
                04 F21 PIC XXX.
                04 FILLER PIC X.
           02 F3 PIC X(113).

       FD  PAYFILE.
           COPY payfile.CPY IN "C:\Users\sid\cms\copylib".      

       FD  TRNPAYFILE
           DATA RECORD IS TRNPAYFILE01.

       01  TRNPAYFILE01.
           02 TRNPAYFILE-KEY.
             03 TRN-KEY8 PIC X(8).
             03 TRN-KEY3 PIC XXX.
           02 TRN-NAME PIC X(24).
           02 TRN-AMOUNT PIC S9(4)V99.
           02 TRN-PAYCODE PIC XXX.
           02 TRN-DENIAL PIC XX.
           02 TRN-CLAIM PIC X(6).
           02 TRN-DATE-T PIC X(8).
           02 TRN-DATE-E PIC X(8).
           02 TRN-ORDER PIC X(6).
           02 TRN-BATCH PIC X(6).
           02 TRN-CHKNO PIC X(30).

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".       

       WORKING-STORAGE SECTION.

       COPY "HIP5010-835.CPY" IN "C:\Users\sid\cms\copylib".
      
       01  HL01.
           02 HL-1 PIC X(40) VALUE SPACE.
           02 FILLER PIC X(21) VALUE SPACE.
           02 HL-2 PIC X(27) VALUE "  NEIC UNPOSTED LIST   ".
           02 FILLER PIC X(5) VALUE SPACE.
           02 HL-3 PIC X(10).
       01  ERR01.
           02 EF1 PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 EF2 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 EF3 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 EF-PAYDATE PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 EF-PROC PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 EF4 PIC X(12).
           02 FILLER PIC X VALUE SPACE.
           02 EF5 PIC ZZZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 EFSIGN PIC X.
           02 EF6 PIC ZZZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 EF-REDUCE PIC ZZZZ9.99.
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
        01 ERR201.
           02 EF2-NUM PIC ZZ9.
           02 FILLER PIC XX VALUE SPACE.
           02 EF2-DENIAL PIC X(3).
           02 FILLER PIC XX VALUE SPACE.
           02 EF2-REASON PIC X(70).

        01 ERR301.
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
           02 FILLER PIC X(19) VALUE SPACE.
           02 T2 PIC X(5) VALUE "HIC #".
           02 FILLER PIC X(5) VALUE SPACE.
           02 T3 PIC X(6) VALUE " DATE ".
           02 FILLER PIC XX VALUE SPACE.
           02 T3 PIC X(6) VALUE " PAID ".
           02 FILLER PIC XXX VALUE SPACE.
           02 FILLER PIC X(8) VALUE " PROC   ".
           02 FILLER PIC X(14) VALUE "KEY           ".
           02 T5 PIC X(7) VALUE " AMOUNT".
           02 FILLER PIC XXX VALUE SPACE.
           02 T6 PIC X(7) VALUE "  PAID ".
           02 FILLER PIC X VALUE SPACE.
           02 T6 PIC X(7) VALUE " REDUCE".
           02 FILLER PIC XXXX VALUE SPACE.
           02 T7 PIC X(6) VALUE " CHKNO".
           02 FILLER PIC X(9) VALUE SPACE.
           02 T9 PIC X(11) VALUE " STAT  DNL ".
           
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
       01  TOT-CHARGE PIC S9(5)V99 VALUE 0.
       01  TOT-REDUCE PIC S9(5)V99 VALUE 0.
       01  FLAGY PIC 9.
       01  FIND-CNTR PIC 99.
       01  CNTRY PIC 99.
       01  CNTR PIC 99.
       01  MULTCHAR PIC 99.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 999.
       01  A PIC 99.
       01  HOLDKEY PIC X(11).
       01  HOLDAMT PIC S9(4)V99.
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
       01  ALF9 PIC X(9).
       01  ALF10.
           02 ALF10-1 PIC X(8).
           02 ALF10-2 PIC XX.
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
       01  CLAIM-PAID PIC S9(4)V99.
       01  DATE-X PIC X(8).
       01  DATE-CC PIC X(8).
       01  SVC-CNTR PIC 99.
       01  CAS-CNTR PIC 99.
       01  LQ-CNTR PIC 99.
       
       01  CLP-TAB01.
           02 CLP-TAB PIC XX OCCURS 64 TIMES.
       01  SVC-TAB01.
           02 SVC-TAB PIC X(120) OCCURS 64 TIMES.
           
       01  SVC-DATE01.
           02 SVC-DATE PIC X(8) OCCURS 64 TIMES.
       01  FOUND-TAB01.
           02 FOUND-KEY PIC X(11) OCCURS 64 TIMES.
       01  BAL-TAB01.
           02 BAL-TAB PIC S9(4)V99 OCCURS 64 TIMES.
       01  TOT-TOT PIC S9(4)V99.
       01  TOT-CLAIM PIC S9(4)V99.
       01  CAS-TAB01.
           02 CAS-TAB PIC X(120) OCCURS 64 TIMES.
       01  CAS-SVC01.
           02 CAS-SVC PIC 99 OCCURS 64 TIMES.

       01  LQ-TAB01.
           02 LQ-TAB PIC X(120) OCCURS 64 TIMES.

       01  LQ-SVC01.
           02 LQ-SVC PIC 99 OCCURS 64 TIMES.           

       01  SAVEFILE01 PIC X(120).
       01  CC-PROCX01.
           02 CC-PROC1X PIC X(5).
           02 CC-PROC2X PIC XX.
           02 CC-MOD2X PIC XX.
           02 CC-MOD3X PIC XX.
       01  CC-PROCY01.
           02 CC-PROC1Y PIC X(5).
           02 CC-PROC2Y PIC XX.
           02 CC-MOD2Y PIC XX.
           02 CC-MOD3Y PIC XX.
       01  CENTS PIC XX.
       01  SIGN-DOLLAR PIC XXXX.
       01  LNAME PIC X(24).
       01  FNAME PIC X(24).
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
           02 EF-TAB PIC X(4) OCCURS 64 TIMES.
       01  PARM-ADDR PIC X(11).
       01  TITLE-FLAG PIC 9 VALUE 0.
       01  GAR-FLAG PIC 9.
       01  ALF6 PIC X(6).
       01  INS-REDUCE PIC S9(5)V99.
       01  ALF25 PIC X(25).
       01  NEF-2 PIC Z9.
       01  PAYORID PIC X(5).
       01  ANS PIC X.
       01  NOT-FLAG PIC 9.
       01  STATUSCODES01.
           02 STATUSCODE PIC 9 OCCURS 27 TIMES.
       01  STATUSNAR01.
           02 STATUSNAR PIC X(25) OCCURS 27 TIMES.
       01  ALLW-TAB01.
           02 ALLW-TAB PIC 9(4)V99 OCCURS 64 TIMES.
       01  NEF-6 PIC Z,ZZZ.99.
       01  ID-NPI1 PIC X(10).
       01  ID-NPI PIC X(10).
       01  PERM-ID PIC X(10).
       01  PAYORID1 PIC X(5).
       01  PROV-FLAG PIC X.
       01  EQUITY-ID PIC X(9).
       01  INS-NAME-HOLD PIC X(5).
       01  ID-EIN PIC X(9).
       
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT INSFILE FILEIN CHARCUR GARFILE MPLRFILE PARMFILE
             PAYCUR CAIDFILE rarcfile.
           OPEN I-O PAYFILE REMITFILE.
           OPEN OUTPUT TRNPAYFILE ERROR-FILE.
           MOVE SPACE TO NAR-KEY01 
           MOVE ALL ZEROES TO NAR-CNTR01 STATUSCODES01 
           MOVE SPACE TO ERROR-FILE01
           PERFORM STATUS-0

           READ PARMFILE 
             AT END 
               GO TO P9
           END-READ

           MOVE PARMFILE01 TO HL-1.

           READ PARMFILE
             AT END
               GO TO P9
           END-READ    
           
           MOVE PARMFILE01 TO PARM-ADDR.
           
           READ PARMFILE
             AT END
               GO TO P9
           END-READ    
           
           MOVE PARMFILE01 TO ID-NPI1
           
           READ PARMFILE
             AT END
               GO TO P9
           END-READ    
           
           MOVE PARMFILE01 TO ID-NPI

           READ PARMFILE
             AT END
               GO TO P9
           END-READ    
           
           READ FILEIN
             AT END
               DISPLAY "NO RECORDS"
               GO TO P9
           END-READ    
           
           MOVE FILEIN01 TO PD-DATE-E
           MOVE PD-DATE-E TO TEST-DATE 
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO HL-3.

       P00.
           MOVE SPACE TO FILEIN01
           
           READ FILEIN
             AT END
               GO TO P9
           END-READ.    

       XX.
           IF F1 NOT = "BPR"
               GO TO P00
           END-IF

           MOVE SPACE TO BPR01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
               BPR-0 BPR-1 BPR-2 BPR-3 BPR-4 BPR-5 BPR-6 BPR-7 BPR-8 
               BPR-9 BPR-10 BPR-11 BPR-12 BPR-13 BPR-14 BPR-15 BPR-16.
           MOVE BPR-16 TO DATE-X.
           
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P9
           END-READ 

           IF F1 NOT = "TRN"
               GO TO P00
           END-IF  

           MOVE SPACE TO TRN01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
               TRN-0 TRN-1 TRN-2 TRN-3 TRN-4.

           IF TRN-3 = "1204581265" AND BPR-16 < "20240622"
               GO TO P00
           END-IF

           MOVE SPACE TO REMITFILE01.    

           STRING DATE-X TRN-2 TRN-3 TRN-4 DELIMITED BY SIZE
               INTO REMIT-KEY.   

           MOVE SPACE TO PAYORID PAYORID1 PROV-FLAG ID-EIN.

       P000.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P9
           END-READ    

           IF F1 = "CLP"
               GO TO P0000
           END-IF

           IF FILEIN01(1:5) = "N1*PR"
              MOVE SPACE TO N101
              UNSTRING FILEIN01 DELIMITED BY "*" INTO
                  N1-0 N1-1 N1-2 N1-3 N1-ID                  
              MOVE N1-ID(1:5) TO PAYORID1
              MOVE N1-ID TO EQUITY-ID
              IF N1-2(1:5) = "MVP H" AND PAYORID1 = space
                MOVE N1-2(1:5) TO INS-NAME-HOLD
              end-if  
           END-IF
        
           IF (F1 = "REF" AND F21 = "*2U")
               MOVE SPACE TO REF01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                   REF-0 REF-1 REF-2
               MOVE REF-2 TO PAYORID
           END-IF

           IF (F1 = "N1*" AND F21= "PE*")
               MOVE SPACE TO N101
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                   N1-0 N1-1 N1-2 N1-3 N1-ID
               MOVE N1-ID TO PERM-ID
           END-IF
           
           IF (F1 = "REF" AND F21= "*TJ")
               MOVE SPACE TO REF01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                   REF-0 REF-1 REF-2
               MOVE REF-2 TO ID-EIN    
           END-IF

           IF F1 = "DTM" AND F2 = "*405"
               MOVE F3(2:8) TO REMIT-DATE-P
           END-IF 
           
           GO TO P000.

       P0000.
           IF (PERM-ID NOT = ID-NPI1)
               AND (PERM-ID NOT = ID-NPI)
               AND (ID-EIN NOT =  PF-1)
               AND (PERM-ID NOT = PF-1)
               MOVE 1 TO PROV-FLAG
           END-IF

      *    FOR HEALTHEQUITY EFT 835s
      *     DISPLAY "EQUITY-ID " EQUITY-ID " PERM-ID " PERM-ID " PF-1 "
      *              PF-1
      *              ACCEPT OMITTED

           IF ((EQUITY-ID = "411410766")
               AND (PERM-ID = PF-1))
               MOVE 0 TO PROV-FLAG
           END-IF

      *     display FILEIN01
      *     accept omitted

           IF (PROV-FLAG = 1)
               GO TO P00
           END-IF

           READ REMITFILE     
               INVALID
                   ACCEPT REMIT-DATE-E FROM CENTURY-DATE
                   WRITE REMITFILE01
                   END-WRITE
               NOT INVALID
                   IF TRN-2 NOT = "0000000000"
                       MOVE SPACE TO ERROR-FILE01
                       STRING REMITFILE01 " DUPE CHECK"
                           DELIMITED BY SIZE INTO ERROR-FILE01
                       WRITE ERROR-FILE01
                       GO TO P00
                   END-IF    
           END-READ     
           
           IF PAYORID = SPACE
               MOVE PAYORID1 TO PAYORID
           END-IF           

           IF TITLE-FLAG = 0
               MOVE 1 TO TITLE-FLAG
               MOVE SPACE TO ERROR-FILE01
               WRITE ERROR-FILE01 FROM HL01 AFTER PAGE
               MOVE SPACE TO ERROR-FILE01
               MOVE TITLE01 TO ERROR-FILE01
               WRITE ERROR-FILE01
           END-IF.

       P1-CLP.
      *    MOVE SPACE TO FILEIN01
      *    READ FILEIN AT END GO TO P9.
      *    IF F1 = "SE*" GO TO P00.
      *    IF F1 NOT = "CLP" GO TO P1-CLP.

       P1-CLP-1.
           MOVE 0 TO NOT-FLAG
           MOVE SPACE TO CLP01 DATE-CC
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
               CLP-0 CLP-1 CLP-2CLMSTAT CLP-3TOTCLMCHG CLP-4TOTCLMPAY 
               CLP-5PATRESP CLP-6PLANCODE CLP-7ICN CLP-8FACILITY 
               CLP-9FREQ CLP-10PATSTAT CLP-11DRG CLP-12QUAN 
               CLP-13PERCENT.
           
           MOVE SPACE TO ALF10
           MOVE CLP-1 TO ALF10.
           MOVE CLP-4TOTCLMPAY TO ALF8
           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO CLAIM-PAID.

       P1-CLP-2.
           MOVE CLP-2CLMSTAT TO EF8
           MOVE SPACE TO NM101 CLMCAS01.
           MOVE SPACE TO SVC-DATE01
           MOVE 0 TO CAS-CNTR
           MOVE 0 TO SVC-CNTR
           move 0 to LQ-CNTR
           MOVE ALL ZEROES TO ALLW-TAB01.

       P1-NM1.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P9
           END-READ

           IF F1 = "CLP" AND SVC-CNTR = 0
               PERFORM P1-NO-SVC
               GO TO P1-CLP-1
           END-IF

           IF F1 = "SE*"
               MOVE FILEIN01 TO SAVEFILE01
               GO TO P2-SVC-LOOP
           END-IF    

           IF F1 = "SVC"
               GO TO P1-SVC-LOOP-0
           END-IF    

           IF F1 = "CAS" 
               MOVE SPACE TO CAS01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                   CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
                   CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
                   CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
               MOVE CAS01 TO CLMCAS01
               MOVE SPACE TO CAS01
               GO TO P1-NM1
           END-IF
               
           IF (F1 = "NM1" AND F2 = "*QC*")
               MOVE SPACE TO NM101
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                   NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF NM1-NAMEM 
                   NM1-NAMES NM1-EINSS NM1-PREFIX NM1-CODE0
               GO TO P1-NM1
           END-IF    

           IF F1 = "DTM" AND F2 = "*232"
               MOVE SPACE TO DTM01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                   DTM-0 DTM-1 DTM-2 
               MOVE DTM-2 TO DATE-CC
               GO TO P1-NM1
           END-IF          

           GO TO P1-NM1.
               
       P1-SVC-LOOP.  

           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P2-SVC-LOOP
           END-READ    
           
           IF F1 = "CLP" OR "SE*" 
               MOVE FILEIN01 TO SAVEFILE01
               GO TO P2-SVC-LOOP
           END-IF.

       P1-SVC-LOOP-0.               
           IF F1 = "SVC" 
             iF FILEIN01(12:1) = "F"
               GO TO P1-SVC-LOOP
             END-IF

             IF (FILEIN01(8:1) = "G")
               AND (FILEIN01(8:5) = "G9500" OR "G9547" OR "G9548"
               OR "G9549" OR "G9550" OR "G9551" OR "G9552"
               OR "G9553" OR "G9554" OR "G9555" OR "G9556"
               OR "G9557" OR "G9637" OR "G1004")
               GO TO P1-SVC-LOOP
             END-IF        
           
             ADD 1 TO SVC-CNTR
             MOVE FILEIN01 TO SVC-TAB(SVC-CNTR)
             GO TO P1-SVC-LOOP
           END-IF    
           
           IF F1 = "CAS" 
               ADD 1 TO CAS-CNTR
               MOVE FILEIN01 TO CAS-TAB(CAS-CNTR)
               MOVE SVC-CNTR TO CAS-SVC(CAS-CNTR)
               GO TO P1-SVC-LOOP
           END-IF    
           
           IF F1 = "AMT" AND F2 = "*B6*"
               MOVE SPACE TO AMT01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO 
                   AMT-0 AMT-1 AMT-2
               MOVE SPACE TO ALF8
               MOVE AMT-2 TO ALF8
               PERFORM AMOUNT-1
               MOVE AMOUNT-X TO ALLW-TAB(SVC-CNTR)
               GO TO P1-SVC-LOOP
           END-IF    

           IF F1 = "LQ*" 
             ADD 1 TO LQ-CNTR
             MOVE FILEIN01 TO LQ-TAB(LQ-CNTR)
             MOVE SVC-CNTR TO LQ-SVC(LQ-CNTR)
             GO TO P1-SVC-LOOP
           end-if  

           IF (F1 = "DTM") AND (F2 = "*150" OR "*472")
               MOVE SPACE TO DTM01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO 
                   DTM-0 DTM-1 DTM-2
               MOVE DTM-2 TO SVC-DATE(SVC-CNTR)
           END-IF

           GO TO P1-SVC-LOOP.

      * VALIDATE INCOMING DATA AGAINST CHARGES
       P2-SVC-LOOP.
           MOVE 0 TO GAR-FLAG
           
           IF SVC-CNTR = 0
              PERFORM P1-NO-SVC 
              GO TO P9-SVC-LOOP
           END-IF

           MOVE CLP-1 TO G-GARNO
           READ GARFILE
             INVALID
               GO TO P3-SVC-LOOP
           END-READ    
     
           MOVE 1 TO GAR-FLAG
           MOVE 0 TO FIND-CNTR TOT-TOT
           PERFORM LOOK-CHG THRU LOOK-CHG-EXIT VARYING X FROM 1
               BY 1 UNTIL X > SVC-CNTR
               
           IF FIND-CNTR = SVC-CNTR
      *         DISPLAY "GOING TO P4-SVC-LOOP"
      *         ACCEPT OMITTED
               GO TO P4-SVC-LOOP
           END-IF.

       P3-SVC-LOOP.                          
           PERFORM FIND-GARNO THRU FIND-GARNO-EXIT

      *  START LOOKING FOR MATCHING CHARGES WITH THE GARNO IN QUESTION.
           
           MOVE 0 TO FIND-CNTR TOT-TOT

           PERFORM LOOK-CHG THRU LOOK-CHG-EXIT VARYING X FROM 1 BY 1
               UNTIL X > SVC-CNTR.

           IF (FIND-CNTR NOT = SVC-CNTR)
               OR (NOT-FLAG = 1 OR 2)               
               PERFORM P1-DENIED-SVC THRU P1-LOST-SVC
                   VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
               GO TO P9-SVC-LOOP
           END-IF.

      * RECORD ARE GOOD! START MAKING PAYMENT RECORDS.
       P4-SVC-LOOP.                      
           IF NOT (CLP-2CLMSTAT = "1 " OR "2 " OR "3 " OR "19"
                               OR "20" OR "21")
               PERFORM P1-DENIED-SVC THRU P1-LOST-SVC
                   VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
               GO TO P9-SVC-LOOP
           END-IF.

       P4-UNITED-START.
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

      *    eliminate qpp codes from printing to error list 
      *    mammo measure    
           IF SVC-1PROCMOD(8:1) = "F"
               GO TO P5-SVC-LOOP-EXIT
           END-IF    
      *    other measures
           IF SVC-1PROCMOD(4:3) = "G95" OR "G96"
               GO TO P5-SVC-LOOP-EXIT
           END-IF    
                      
           MOVE SPACE TO ALF8
      *    health equity pay amount is in CLP 
           IF PAYORID = "43700" or "92916"
             MOVE CLP-4TOTCLMPAY TO ALF8
           ELSE 
             MOVE SVC-3PAYAMT to ALF8
           END-IF  
           IF ALF8 = "-"
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT
           END-IF    
           
           PERFORM AMOUNT-1
           
           MULTIPLY AMOUNT-X BY -1 GIVING PD-AMOUNT.

           IF PD-AMOUNT = 0
               MOVE 0 TO FLAG
      *         DISPLAY "PAID AMOUNT IS ZERO " PD-AMOUNT " DUMP-50 NEXT"
      *         ACCEPT OMITTED                   
               PERFORM DUMP50 
               IF FLAG = 1
                   PERFORM P1-LOST-SVC
                   GO TO P5-SVC-LOOP-EXIT
               END-IF
           END-IF

           PERFORM NO-SURPRISE.
           
           MOVE FOUND-KEY(X) TO CHARCUR-KEY
           READ CHARCUR
             INVALID  
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT
           END-READ

           MOVE CC-CLAIM TO PD-CLAIM
           MOVE DATE-X TO PD-DATE-T
           MOVE G-GARNAME TO PD-NAME.
                     
           IF CC-PAYCODE = "062" AND (CLP-2CLMSTAT NOT = "1")
               MOVE CC-PAYCODE TO PD-PAYCODE
               GO TO P7-NEXT
           END-IF

           IF INS-NAME-HOLD = "MVP H"
             MOVE "14156" TO PAYORID
           end-if

      *     DISPLAY PAYORID " PAYORID"
      *     ACCEPT OMITTED
           
           IF PAYORID = space OR "11329"
             PERFORM P1-LOST-SVC 
             GO TO P5-SVC-LOOP-EXIT.

           MOVE PAYORID TO INS-NEIC
           START INSFILE KEY NOT < INS-NEIC
             INVALID
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT                               
           END-START.

       P3-NEXT.
           READ INSFILE NEXT
             AT end
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT
           end-read

           IF PAYORID = "43700"
             MOVE "075" TO PD-PAYCODE
             GO TO P7-NEXT
           END-IF
             
           IF INS-NEIC NOT = PAYORID
             AND PAYORID NOT = "52192"
             GO TO P3-NEXT.

           IF CLP-2CLMSTAT = "1" OR "19"
             IF G-PRINS NOT = INS-KEY
               GO TO P3-NEXT
             ELSE
               MOVE G-PRINS TO PD-PAYCODE
               GO TO P7-NEXT.

           IF CLP-2CLMSTAT = "2"
             IF G-SEINS NOT = INS-KEY
               GO TO P3-NEXT
             ELSE
               MOVE G-SEINS TO PD-PAYCODE
               GO TO P7-NEXT.                                                
           
           GO TO P3-NEXT.                 

       P7-NEXT.
           MOVE "  " TO PD-DENIAL.

           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR
               IF CAS-SVC(Z) = X
                   MOVE SPACE TO CAS01 
                   MOVE CAS-TAB(Z) TO FILEIN01
                   UNSTRING FILEIN01 DELIMITED BY "*" INTO
                       CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
                       CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
                       CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
               
                   IF (CAS-2 = "1  " OR "126" OR "25 " OR "37 ") 
                       OR (CAS-5 = "1  " OR "126" OR "25 " OR "37 ")
                       OR (CAS-8 = "1  " OR "126" OR "25 " OR "37 ")
                       OR (CAS-11 = "1  " OR "126" OR "25 " OR "37 ")  
                       OR (CAS-14 = "1  " OR "126" OR "25 " OR "37 ") 
                       OR (CAS-17 = "1  " OR "126" OR "25 " OR "37 ") 
                       MOVE "DD" TO PD-DENIAL
                       MOVE CAS-CNTR TO Z
                   END-IF
               END-IF
           END-PERFORM

           IF NOT (PD-PAYCODE = G-PRINS OR G-SEINS OR G-TRINS
                    OR "075")
               PERFORM P1-LOST-SVC 
               GO TO P5-SVC-LOOP-EXIT
           END-IF

           COMPUTE CLAIM-TOT = CC-AMOUNT + PD-AMOUNT
           
      *     DISPLAY CLAIM-TOT " CLAIM-TOT " CC-AMOUNT " CC-AMOUNT "
      *       PD-AMOUNT " PD-AMOUNT"
      *     ACCEPT OMITTED
           
           PERFORM S4 THRU S5
           
           IF CLAIM-TOT < 0
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT
           END-IF

           MOVE CC-AMOUNT TO TOT-CLAIM
           
           PERFORM DMP4 THRU DMP5
           
           IF TOT-CLAIM = 0
               GO TO P5-SVC-LOOP-EXIT
           END-IF    
           
           ACCEPT ORDER-8 FROM TIME
           MOVE ORDER-6 TO PD-ORDER
           MOVE SPACE TO PD-BATCH
           MOVE G-GARNO TO PD-KEY8
           MOVE PAYFILE01 TO PAYBACK.
           MOVE 0 TO XYZ.

       P3.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE
             INVALID
               GO TO P4
           END-READ

           GO TO P3.

       P4.
           MOVE PAYBACK TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01

           MOVE PAYFILE01 TO TRNPAYFILE01
           MOVE TRN-2 TO TRN-CHKNO
           WRITE TRNPAYFILE01
           
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR           
               IF CAS-SVC(Z) = X
                   MOVE SPACE TO CAS01 ALF8
                   MOVE CAS-TAB(Z) TO FILEIN01
                   UNSTRING FILEIN01 DELIMITED BY "*" INTO
                     CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
                     CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
                     CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
              
                   IF (CAS-2 = "104") MOVE CAS-3 TO ALF8
                   END-IF
              
                   IF (CAS-5 = "104") MOVE CAS-6 TO ALF8
                   END-IF
              
                   IF (CAS-8 = "104") MOVE CAS-9 TO ALF8
                   END-IF
              
                   IF (CAS-11 = "104") MOVE CAS-12 TO ALF8
                   END-IF
              
                   IF (CAS-14 = "104") MOVE CAS-15 TO ALF8
                   END-IF
              
                   IF (CAS-17 = "104") MOVE CAS-18 TO ALF8
                   END-IF
              
                   IF ALF8 NOT = SPACE
                       MOVE "DI" TO PD-DENIAL
                       PERFORM AMOUNT-1
                       MULTIPLY AMOUNT-X BY -1 GIVING PD-AMOUNT
                       PERFORM WRITE-ADJ THRU WRITE-ADJ-EXIT
                       MOVE CAS-CNTR TO Z
                   END-IF
               END-IF
           END-PERFORM
           
           MOVE 0 TO INS-REDUCE
           
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR
           
               IF CAS-SVC(Z) = X
                   MOVE SPACE TO CAS01 
                   MOVE CAS-TAB(Z) TO FILEIN01
                   UNSTRING FILEIN01 DELIMITED BY "*" INTO
                     CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
                     CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
                     CAS-15 CAS-16 CAS-17 CAS-18 CAS-19

                   IF (CAS-1 = "CO" OR "PI" OR "OA")
                               AND
                      ((CAS-2 = "A1" OR "A2" OR "B6" OR "B9" OR "B10" OR
                                "B13" OR "18" OR "24" OR "42" OR "45" OR 
                                "59" OR "253" OR "131" OR "P12" 
                                OR "P24")
                               OR
                      (CAS-5 = "A1" OR "A2" OR "B6" OR "B9" OR "B10" OR 
                               "B13" OR "18" OR "42" OR "45" OR
                               "59" OR "253" OR "131" OR "P12" OR 
                               "P23" OR "P24"))
                               AND 
                      NOT (CLP-2CLMSTAT = "2 " OR "3 ")
                       
      *                display CAS-1 " CAS-1 " CAS-2 " CAS-2 " 
      *                  CAS-3 " CAS-3 " CAS-4 " CAS-4 " CAS-5 " CAS-5"
      *                accept omitted             
                       
                       IF CAS-3 NOT = SPACE
                           MOVE SPACE TO ALF8
                           MOVE CAS-3 TO ALF8
                           PERFORM AMOUNT-1
                           COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
      *                     DISPLAY INS-REDUCE " INS-REDUCE"
                       END-IF
               
                       IF CAS-6 NOT = SPACE AND CAS-5 NOT = "104"
                           MOVE SPACE TO ALF8
                           MOVE CAS-6 TO ALF8
                           PERFORM AMOUNT-1
                           COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                       END-IF
               
                       IF CAS-9 NOT = SPACE
                           MOVE SPACE TO ALF8
                           MOVE CAS-9 TO ALF8
                           PERFORM AMOUNT-1
                           COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                       END-IF
               
                       IF CAS-12 NOT = SPACE
                           MOVE SPACE TO ALF8
                           MOVE CAS-12 TO ALF8
                           PERFORM AMOUNT-1
                           COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                       END-IF
               
                       IF CAS-15 NOT = SPACE
                           MOVE SPACE TO ALF8
                           MOVE CAS-15 TO ALF8
                           PERFORM AMOUNT-1
                           COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                       END-IF
               
                       IF CAS-18 NOT = SPACE
                           MOVE SPACE TO ALF8
                           MOVE CAS-18 TO ALF8
                           PERFORM AMOUNT-1
                           COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           IF ALF8-1 = "-" AND PAYORID = "J4110"
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT
           END-IF    
           
           COMPUTE CLAIM-TOT = CC-AMOUNT - INS-REDUCE

      *     display claim-tot " claim-tot " ins-reduce " ins-reduce"
      *     accept omitted
           
           IF CLAIM-TOT = 0
               PERFORM P1-LOST-SVC
               GO TO P5-SVC-LOOP-EXIT
           END-IF
           
           IF INS-REDUCE NOT = 0
               COMPUTE CLAIM-TOT = CC-AMOUNT + PD-AMOUNT - INS-REDUCE
               PERFORM S4 THRU S5
               
      *         DISPLAY CLAIM-TOT " CLAIM-TOT"
      *         ACCEPT OMITTED
               
               IF CLAIM-TOT < 0
                   PERFORM P1-LOST-SVC
                   GO TO P5-SVC-LOOP-EXIT
               END-IF

               MOVE "14" TO PD-DENIAL
               MULTIPLY INS-REDUCE BY -1 GIVING PD-AMOUNT
               PERFORM WRITE-ADJ THRU WRITE-ADJ-EXIT
               MOVE CAS-CNTR TO Z
           END-IF
           
           GO TO P5-SVC-LOOP-EXIT.

       WRITE-ADJ.     
           MOVE PAYFILE01 TO PAYBACK.

       P4-0.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE
             INVALID
               GO TO P4-1
           END-READ

           GO TO P4-0.

       P4-1. 
           MOVE PAYBACK TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01.

           MOVE PAYFILE01 TO TRNPAYFILE01
           MOVE TRN-2 TO TRN-CHKNO
           WRITE TRNPAYFILE01.

       WRITE-ADJ-EXIT.
           EXIT.

       P5-SVC-LOOP-EXIT.
           EXIT.

       DUMP50.
           IF CAS-CNTR = 0
             MOVE 1 TO FLAG
           ELSE
               PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR
                   IF CAS-SVC(Z) = X
                       MOVE SPACE TO CAS01 
                       MOVE CAS-TAB(Z) TO FILEIN01
                       UNSTRING FILEIN01 DELIMITED BY "*" INTO
                         CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
                         CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
                         CAS-15 CAS-16 CAS-17 CAS-18 CAS-19

                     IF (CAS-2 = "50" OR "109" OR "167" OR "B13")
                       OR (CAS-1 = "CO" AND CAS-2 = "4    ")
                       OR (CAS-1 = "CO" AND CAS-2 = "11   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "16   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "18   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "29   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "31   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "55   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "58   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "95   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "96   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "97   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "146  ")                   
                       OR (CAS-1 = "CO" AND CAS-2 = "151  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "197  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "234  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "242  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "252  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "226  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "284  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "288  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "A1   ")
                       OR (CAS-1 = "CO" AND CAS-2 = "B11  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "B20  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "P12  ")
                       OR (CAS-1 = "CO" AND CAS-2 = "P14  ")
                       OR (CAS-1 = "OA" AND CAS-2 = "18   ")
                       OR (CAS-1 = "OA" AND CAS-2 = "226  ")
                       OR (CAS-1 = "OA" AND CAS-2 = "A1   ")
                       OR (CAS-1 = "OA" AND CAS-2 = "B11  ")
                       OR (CAS-1 = "OA" AND CAS-2 = "B13  ")
                       OR (CAS-1 = "OA" AND CAS-2 = "P8   ")
                       OR (CAS-1 = "PI" AND CAS-2 = "5    ")
                       OR (CAS-1 = "PI" AND CAS-2 = "11   ")
                       OR (CAS-1 = "PI" AND CAS-2 = "96   ")
                       OR (CAS-1 = "PI" AND CAS-2 = "97   ")
                       OR (CAS-1 = "PI" AND CAS-2 = "149  ")
                       OR (CAS-1 = "PI" AND CAS-2 = "234  ")
                       OR (CAS-1 = "PI" AND CAS-2 = "P4   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "16   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "26   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "27   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "31   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "35   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "96   ")
                       OR (CAS-1 = "PR" AND CAS-2 = "151  ")                  
                       OR (CAS-1 = "PR" AND CAS-2 = "243  ")                  

                       MOVE 1 TO FLAG
                       MOVE CAS-CNTR TO Z
                     END-IF               
                   END-IF               
               END-PERFORM
           END-IF.    

       P9-SVC-LOOP.
           MOVE SAVEFILE01 TO FILEIN01
           IF F1 = "CLP" GO TO P1-CLP-1.
           GO TO XX.

       P1-NO-SVC.
           PERFORM STATUS-1
           MOVE SPACE TO EF1
           STRING NM1-NAMEL ";" NM1-NAMEF 
           DELIMITED BY "  " INTO EF1
           MOVE NM1-CODE0 TO EF2

           IF NOT-FLAG = 1
            MOVE "?NOT YOURS?" TO EF2
           END-IF

           IF NOT-FLAG = 2
            MOVE "PAIN OTHER?" TO EF2
           END-IF

           MOVE DATE-CC TO TEST-DATE 
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO EF3
           MOVE CLP-1 TO EF4
           MOVE SPACE TO ALF8
           MOVE CLP-3TOTCLMCHG TO ALF8
           MOVE SPACE TO EFSIGN

           IF ALF8-1 = "-"
           MOVE "-" TO EFSIGN
           END-IF

           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO EF5
           MOVE SPACE TO ALF8
           MOVE CLP-4TOTCLMPAY TO ALF8 

           IF ALF8-1 = "-"
           MOVE "-" TO EFSIGN
           END-IF

           PERFORM AMOUNT-1

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
           WRITE ERROR-FILE01 FROM ERR01
           
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
           PERFORM STATUS-1
           MOVE SPACE TO SVC01 
           MOVE SVC-TAB(X) TO FILEIN01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT SVC-4NUBC 
           SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.

       P1-LOST-SVC.
           PERFORM STATUS-1

           MOVE SPACE TO EF1
           STRING NM1-NAMEL ";" NM1-NAMEF 
           DELIMITED BY "  " INTO EF1
           
           MOVE NM1-CODE0 TO EF2
           
           IF NOT-FLAG = 1
            MOVE "?NOT YOURS?" TO EF2
           END-IF
           
           IF NOT-FLAG = 2
            MOVE "PAID OTHERS" TO EF2
           END-IF

           IF SVC-DATE(X) = SPACE
            MOVE DATE-CC TO SVC-DATE(X)
           END-IF
           
           MOVE SVC-DATE(X) TO TEST-DATE 
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO EF3
           MOVE BPR-16 TO EF-PAYDATE
           MOVE CLP-1 TO EF4
           MOVE SPACE TO ALF8
           MOVE SVC-2CHRGAMT TO ALF8
           MOVE SPACE TO EFSIGN
           
           IF ALF8-1 = "-"
               MOVE "-" TO EFSIGN
           END-IF
           
           PERFORM AMOUNT-1
           
           MOVE AMOUNT-X TO EF5
           ADD AMOUNT-X TO TOT-CHARGE
           MOVE SPACE TO ALF8
           MOVE SVC-3PAYAMT TO ALF8
           
           IF ALF8-1 = "-"
               MOVE "-" TO EFSIGN
           END-IF

           PERFORM AMOUNT-1
      *     IF (PAYORID = "87726")
      *     AND (CLP-2CLMSTAT = "2 ")
      *     MOVE SPACE TO ALF8
      *    MOVE ALLW-TAB(X) TO AMOUNT-X
      *     MOVE SPACE TO EFSIGN
      *    IF ALF8-1 = "-"
      *     MOVE "-" TO EFSIGN
      *    END-IF
      *     MOVE 0 TO AMOUNT-X
      *    END-IF
           MOVE AMOUNT-X TO EF6
           IF ALF8-1 NOT = "-"
            COMPUTE TOT-PAY = TOT-PAY + AMOUNT-X
           END-IF.
            MOVE TRN-2 TO EF7
            MOVE SPACE TO ALF-17 CC-PROCX01
            MOVE SVC-1PROCMOD TO ALF-17
            UNSTRING ALF-14 DELIMITED BY ":" INTO 
            CC-PROC1X CC-PROC2X CC-MOD2X CC-MOD3X
            MOVE SPACE TO EF-PROC
            STRING CC-PROC1X CC-PROC2X CC-MOD3X DELIMITED BY SIZE
            INTO EF-PROC
            MOVE SPACE TO EF-TAB01
            MOVE 0 TO DENIAL-CNTR  INS-REDUCE
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

             IF CAS-1 = "CO"
             AND (CLP-2CLMSTAT = "1 ")
               IF CAS-3 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-3 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
               END-IF
               IF CAS-6 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-6 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                END-IF
               IF CAS-9 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-9 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                END-IF
               IF CAS-12 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-12 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                END-IF
               IF CAS-15 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-15 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
                END-IF
               IF CAS-18 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-18 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE INS-REDUCE = INS-REDUCE + AMOUNT-X
               END-IF
             END-IF
            END-IF
           END-PERFORM.
             MOVE INS-REDUCE TO EF-REDUCE.
             ADD INS-REDUCE TO TOT-REDUCE
             MOVE EF-TAB(1) TO EF-DENIAL1
             MOVE EF-TAB(2) TO EF-DENIAL2
             MOVE EF-TAB(3) TO EF-DENIAL3
             MOVE EF-TAB(4) TO EF-DENIAL4
             MOVE EF-TAB(5) TO EF-DENIAL5
             MOVE EF-TAB(6) TO EF-DENIAL6
             MOVE SPACE TO ERROR-FILE01
             WRITE ERROR-FILE01 FROM ERR01.

             IF DENIAL-CNTR > 6
             MOVE EF-TAB(7) TO EF3-DENIAL1
             MOVE EF-TAB(8) TO EF3-DENIAL2
             MOVE EF-TAB(9) TO EF3-DENIAL3
             MOVE EF-TAB(10) TO EF3-DENIAL4
             MOVE EF-TAB(11) TO EF3-DENIAL5
             MOVE EF-TAB(12) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
             WRITE ERROR-FILE01 FROM ERR301.
             IF DENIAL-CNTR > 12
             MOVE EF-TAB(13) TO EF3-DENIAL1
             MOVE EF-TAB(14) TO EF3-DENIAL2
             MOVE EF-TAB(15) TO EF3-DENIAL3
             MOVE EF-TAB(16) TO EF3-DENIAL4
             MOVE EF-TAB(17) TO EF3-DENIAL5
             MOVE EF-TAB(18) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
             WRITE ERROR-FILE01 FROM ERR301.
             IF DENIAL-CNTR > 18
             MOVE EF-TAB(19) TO EF3-DENIAL1
             MOVE EF-TAB(20) TO EF3-DENIAL2
             MOVE EF-TAB(21) TO EF3-DENIAL3
             MOVE EF-TAB(22) TO EF3-DENIAL4
             MOVE EF-TAB(23) TO EF3-DENIAL5
             MOVE EF-TAB(24) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
             WRITE ERROR-FILE01 FROM ERR301.
             IF DENIAL-CNTR > 24
             MOVE EF-TAB(25) TO EF3-DENIAL1
             MOVE EF-TAB(26) TO EF3-DENIAL2
             MOVE EF-TAB(27) TO EF3-DENIAL3
             MOVE EF-TAB(28) TO EF3-DENIAL4
             MOVE EF-TAB(29) TO EF3-DENIAL5
             MOVE EF-TAB(30) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
             WRITE ERROR-FILE01 FROM ERR301.
             IF DENIAL-CNTR > 30
             MOVE EF-TAB(31) TO EF3-DENIAL1
             MOVE EF-TAB(32) TO EF3-DENIAL2
             MOVE EF-TAB(33) TO EF3-DENIAL3
             MOVE EF-TAB(34) TO EF3-DENIAL4
             MOVE EF-TAB(35) TO EF3-DENIAL5
             MOVE EF-TAB(36) TO EF3-DENIAL6
             MOVE SPACE TO ERROR-FILE01
             WRITE ERROR-FILE01 FROM ERR301.

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LQ-CNTR
             IF LQ-SVC(Y) = X               
               MOVE SPACE TO FILEIN01
               MOVE LQ-TAB(Y) TO FILEIN01
               MOVE SPACE TO LQ01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                 LQ-0 LQ-1 LQ-2 

               IF NOT (LQ-2 = SPACE OR "N807" OR "MA130" OR "N620"
                 OR "N535")
                 MOVE LQ-2 TO rarc-key
                 READ rarcfile with lock
                   invalid
                     MOVE SPACE TO RARC-REASON
                     STRING LQ-2 " INVALID RARC" DELIMITED BY size 
                       INTO ERROR-FILE01
                     WRITE ERROR-FILE01  
                     continue
                 end-read
                 MOVE SPACE TO ERROR-FILE01
                 STRING rarc-reason DELIMITED BY size INTO ERROR-FILE01
                 WRITE ERROR-FILE01
               end-if
             end-if               
           END-PERFORM. 

       DUMP-1.
           MOVE SPACE TO ERROR-FILE01
           STRING "NM1 " NM1-NAMEL ";" NM1-NAMEF DELIMITED
           BY SIZE INTO ERROR-FILE01
           WRITE ERROR-FILE01
            MOVE SPACE TO FILEIN01
            MOVE CLP01 TO FILEIN01
            INSPECT FILEIN01 REPLACING ALL "*" BY " "
            INSPECT FILEIN01 REPLACING LEADING "CLP" BY "   "
            MOVE SPACE TO ERROR-FILE01
            WRITE ERROR-FILE01 FROM FILEIN01

           PERFORM ERR-UNITED THRU ERR-UNITED-EXIT
           VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
           GO TO P9-SVC-LOOP.
           
       ERR-UNITED.

            MOVE SPACE TO FILEIN01
            MOVE SVC-TAB(X) TO FILEIN01
            INSPECT FILEIN01 REPLACING ALL "*" BY " "
            MOVE SPACE TO ERROR-FILE01
            WRITE ERROR-FILE01 FROM FILEIN01
            MOVE SPACE TO FILEIN01
            MOVE SVC-DATE(X) TO FILEIN01
            INSPECT FILEIN01 REPLACING ALL "*" BY " "
            MOVE SPACE TO ERROR-FILE01
            WRITE ERROR-FILE01 FROM FILEIN01
            PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR
             IF CAS-SVC(Z) = X
              MOVE SPACE TO FILEIN01
              MOVE CAS-TAB(X) TO FILEIN01
              INSPECT FILEIN01 REPLACING ALL "*" BY " "
              MOVE SPACE TO ERROR-FILE01
              WRITE ERROR-FILE01 FROM FILEIN01
             END-IF
            END-PERFORM
            MOVE SPACE TO FILEIN01
            MOVE ALLW-TAB(X) TO NEF-6
            MOVE SPACE TO ERROR-FILE01
            WRITE ERROR-FILE01 FROM NEF-6.
       ERR-UNITED-EXIT.
           EXIT.

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
      *  CLAIM IS DROPPED TO THE ERROR FILE.
      *  I.E., PERFECTION MUST REIGN!

       FIND-GARNO.    
           MOVE SPACE TO G-GARNO
           MOVE NM1-NAMEL(1:3) TO ID1
           MOVE ID1 TO ALF-3
           START GARFILE KEY NOT < G-GARNO
             INVALID 
               GO TO FIND-GARNO-EXIT
           END-START.

       P2. 
           READ GARFILE NEXT
             AT END
               GO TO FIND-GARNO-EXIT
           END-READ

           IF ID1 > ALF-3
               GO TO FIND-GARNO-EXIT
           END-IF.

       P3LOOK.
           IF NOT ((G-PRIPOL = NM1-CODE)  
                OR (G-SECPOL = NM1-CODE)
                OR (MPLR-TRIPOL = NM1-CODE))
           GO TO P2.      

       FIND-GARNO-EXIT.
           EXIT.
       
       LOOK-CHG.       
           MOVE SPACE TO SVC01 FILEIN01
           MOVE SVC-TAB(X) TO FILEIN01  
      *    display filein01
      *    accept omitted                    
           
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
               SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT SVC-4NUBC 
               SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.

           MOVE SPACE TO ALF-17

           IF SVC-6COMPOSITE = SPACE
               MOVE SVC-1PROCMOD TO ALF-17
           ELSE
               MOVE SVC-6COMPOSITE TO ALF-17
           END-IF

           MOVE SPACE TO CC-PROCX01
           UNSTRING ALF-14 DELIMITED BY ":" INTO CC-PROC1X
               CC-PROC2X CC-MOD2X CC-MOD3X

           MOVE G-GARNO TO CC-KEY8
           MOVE "000" TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID 
               GO TO LOOK-CHG-EXIT
           END-START.

       LOOK-1. 
           READ CHARCUR NEXT
             AT END
               GO TO LOOK-CHG-EXIT
           END-READ                  

           IF CC-KEY8 NOT = G-GARNO
               GO TO LOOK-CHG-EXIT
           END-IF
                   
           IF SVC-DATE(X) = SPACE
               MOVE DATE-CC TO SVC-DATE(X)
           END-IF    

           IF CC-DATE-T NOT = SVC-DATE(X)
               GO TO LOOK-1
           END-IF    
      
           MOVE SPACE TO CC-PROCY01
           MOVE CC-PROC1 TO CC-PROC1Y
           MOVE CC-PROC2 TO CC-PROC2Y
           MOVE CC-MOD2 TO CC-MOD2Y
           MOVE CC-MOD3 TO CC-MOD3Y
           
           IF CC-PROC2Y = SPACE
               MOVE CC-MOD2Y TO CC-PROC2Y
               MOVE SPACE TO CC-MOD2Y
           END-IF

           IF CC-MOD2Y = SPACE
               MOVE CC-MOD3Y TO CC-MOD2Y
               MOVE SPACE TO CC-MOD3Y
           END-IF

           IF CC-MOD2X = "51"
               MOVE SPACE TO CC-MOD2X
           END-IF

           IF CC-PROC1X NOT = CC-PROC1Y
               GO TO LOOK-1
           END-IF

           MOVE 0 TO FLAGY 
           
           PERFORM A5 THRU A5-EXIT
           
           IF FLAGY = 1
               GO TO LOOK-1
           END-IF

           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > FIND-CNTR
           
               IF CHARCUR-KEY = FOUND-KEY(Z)
                   MOVE 1 TO FLAGY
                   MOVE FIND-CNTR TO Z               
               END-IF

           END-PERFORM

           IF FLAGY = 1
               GO TO LOOK-1
           END-IF

           ADD 1 TO FIND-CNTR
           MOVE CHARCUR-KEY TO FOUND-KEY(X).

      *     DISPLAY CHARCUR-KEY
      *     ACCEPT OMITTED.

      *     MOVE CC-AMOUNT TO TOT-CLAIM
      *     PERFORM DMP4 THRU DMP5
      *     MOVE TOT-CLAIM TO BAL-TAB(X)
      *     ADD TOT-CLAIM TO TOT-TOT.

       LOOK-CHG-EXIT.
           EXIT.

       A5. 
           MOVE G-GARNO TO PD-KEY8 
           MOVE "000" TO PD-KEY3.
           START PAYFILE KEY NOT < PAYFILE-KEY
             INVALID
               GO TO A5-EXIT
           END-START.    

       A5-1.
           READ PAYFILE NEXT
             AT END
               GO TO A5-EXIT
           END-READ

           IF PD-KEY8 NOT = CC-KEY8
               GO TO A5-EXIT
           END-IF    
           
           IF PD-CLAIM NOT = CC-CLAIM
               GO TO A5-1
           END-IF

           MOVE 1 TO FLAGY.

       A5-EXIT. 
           EXIT.

       S4. 
           MOVE CC-KEY8 TO PC-KEY8 
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT <  PAYCUR-KEY INVALID GO TO S5.

       S41.
           READ PAYCUR NEXT AT END GO TO S5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S5.
           IF PC-CLAIM NOT = CC-CLAIM GO TO S41. 
           IF (PC-PAYCODE = G-PRINS)
             AND ((PC-DENIAL = "14") OR (PC-PAYCODE = "014"))
             AND (CLP-2CLMSTAT = "2") 
             GO TO S41
           END-IF             
           ADD PC-AMOUNT TO CLAIM-TOT.
           GO TO S41.

       S5. 
           EXIT.

       DMP4. 
           MOVE CC-KEY8 TO PC-KEY8 
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT <  PAYCUR-KEY
             INVALID
               GO TO DMP5
           END-START.

       DMP41. 
           READ PAYCUR NEXT
             AT END
               GO TO DMP5
           END-READ

           IF PC-KEY8 NOT = CC-KEY8 GO TO DMP5.
           
           IF PC-CLAIM NOT = CC-CLAIM GO TO DMP41. 
           
           IF (PC-PAYCODE = G-PRINS)
               AND ((PC-DENIAL = "14") OR (PC-PAYCODE = "014"))
               AND (CLP-2CLMSTAT = "2") 
               GO TO DMP41
           END-IF             

           ADD PC-AMOUNT TO TOT-CLAIM.
           GO TO DMP41.

       DMP5. 
           EXIT.

       AMOUNT-1.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           
           IF ALF8-1 = "-"
               UNSTRING ALF8-7 DELIMITED BY "." INTO SIGN-DOLLAR CENTS
           ELSE
               UNSTRING ALF8 DELIMITED BY "." INTO SIGN-DOLLAR CENTS
           END-IF

           INSPECT CENTS REPLACING ALL " " BY "0".
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING AMOUNT-X.
           
           IF ALF8-1 = "-"
               COMPUTE AMOUNT-X = -1 * AMOUNT-X
           END-IF.

       P9. 
           MOVE "UNPOSTED" TO EF1
           MOVE "PAYMENTS" TO EF2
           MOVE "TOTAL" TO EF3
           MOVE "  =" TO EF4
           MOVE TOT-CHARGE TO EF5
           MOVE TOT-REDUCE TO EF-REDUCE
           MOVE TOT-PAY TO EF6
           MOVE SPACE TO  EF7 EF8 EF-PROC EF-DENIAL02
           MOVE SPACE TO ERROR-FILE01 
           WRITE ERROR-FILE01
           MOVE ERR01 TO ERROR-FILE01
           WRITE ERROR-FILE01. 
           MOVE SPACE TO ERROR-FILE01
           MOVE "DENIAL REASONS SUMMARY" TO ERROR-FILE01
           WRITE ERROR-FILE01 AFTER 2
           MOVE SPACE TO ERROR-FILE01
           WRITE ERROR-FILE01
           MOVE "FREQ  KEY  DESCRIPTION " TO ERROR-FILE01
           WRITE ERROR-FILE01
           MOVE SPACE TO ERROR-FILE01
           WRITE ERROR-FILE01

           PERFORM VARYING Z FROM 1 BY 1 UNTIL NAR-KEY(Z) = SPACE
            MOVE NAR-KEY(Z) TO CAID-KEY
             READ CAIDFILE INVALID MOVE SPACE TO CAID-REASON
             END-READ
            MOVE CAID-KEY TO EF2-DENIAL
            MOVE NAR-CNTR(Z) TO EF2-NUM 
            MOVE CAID-REASON TO EF2-REASON
            MOVE SPACE TO ERROR-FILE01
            WRITE ERROR-FILE01 FROM ERR201
           END-PERFORM
           
           MOVE SPACE TO ERROR-FILE01
           WRITE ERROR-FILE01
           MOVE "STATUS CODES" TO ERROR-FILE01
           WRITE ERROR-FILE01
           
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > 27
            IF STATUSCODE(A) = 1
             MOVE STATUSNAR(A) TO ALF25
             MOVE A TO NEF-2
             MOVE SPACE TO ERROR-FILE01
             STRING NEF-2 " " ALF25 
             DELIMITED BY SIZE INTO ERROR-FILE01
             WRITE ERROR-FILE01
            END-IF
           END-PERFORM.
           
           CLOSE INSFILE FILEIN CHARCUR GARFILE MPLRFILE PARMFILE
               PAYCUR CAIDFILE rarcfile PAYFILE REMITFILE
               TRNPAYFILE ERROR-FILE.
           STOP RUN.

       P169.
            PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR
             IF CAS-SVC(Z) = X
              MOVE SPACE TO CAS01 
              MOVE CAS-TAB(Z) TO FILEIN01
              UNSTRING FILEIN01 DELIMITED BY "*" INTO
              CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
              CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
              CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
              IF CAS-1 = "OA" AND CAS-2 = "1  "
                MOVE SPACE TO ALF8
                MOVE CAS-3 TO ALF8
                MOVE CAS-CNTR TO Z
                GO TO P169-EXIT
               END-IF
             END-IF
           END-PERFORM.
       P169-EXIT.
           EXIT.
       P999.
               IF (CAS-2 = "42 " OR "B6 ") MOVE CAS-3 TO ALF8
               END-IF
               IF (CAS-5 = "42 " OR "B6 ") MOVE CAS-5 TO ALF8
               END-IF
               IF (CAS-8 = "42 " OR "B6 ") MOVE CAS-8 TO ALF8
               END-IF
               IF (CAS-11 = "42 " OR "B6 ") MOVE CAS-11 TO ALF8
               END-IF
               IF (CAS-14 = "42 " OR "B6 ") MOVE CAS-14 TO ALF8
               END-IF
               IF (CAS-17 = "42 " OR "B6 ") MOVE CAS-17 TO ALF8
               END-IF.
       STATUS-0.
               MOVE "PROCESSED AS PRIMARY    " TO STATUSNAR(1).
               MOVE "PROCESSED AS SECONDARY  " TO STATUSNAR(2).
               MOVE "PROCESSED AS TERTIARY   " TO STATUSNAR(3).
               MOVE "DENIED                  " TO STATUSNAR(4).
               MOVE "PENDED                  " TO STATUSNAR(5).
               MOVE "RECIEVED NOT IN PROCESS " TO STATUSNAR(10).
               MOVE "SUSPENDED               " TO STATUSNAR(13).
               MOVE "SUSPENDED -INVESTIGATED " TO STATUSNAR(15).
               MOVE "SUSPENDED RETURNED      " TO STATUSNAR(16).
               MOVE "SUSPENDED REVIEW PENDING" TO STATUSNAR(17).
               MOVE "PRIMARY FOWARDED TO 2ND " TO STATUSNAR(19).
               MOVE "SECONDARY FOWARD TO 3RD " TO STATUSNAR(20).
               MOVE "TERTIARY FOWARD TO ADD'L" TO STATUSNAR(21).
               MOVE "REVERSAL OF PREV. PAMENT" TO STATUSNAR(22).
               MOVE "NOT OUR CLAIM FORWARDED " TO STATUSNAR(23).
               MOVE "PREDETERMINATION PRICING" TO STATUSNAR(25).
               MOVE "REVIEWED                " TO STATUSNAR(27).
       STATUS-1.        
               IF CLP-2CLMSTAT = "1 " MOVE 1 TO STATUSCODE(1).
               IF CLP-2CLMSTAT = "2 " MOVE 1 TO STATUSCODE(2).
               IF CLP-2CLMSTAT = "3 " MOVE 1 TO STATUSCODE(3).
               IF CLP-2CLMSTAT = "4 " MOVE 1 TO STATUSCODE(4).
               IF CLP-2CLMSTAT = "5 " MOVE 1 TO STATUSCODE(5).
               IF CLP-2CLMSTAT = "10" MOVE 1 TO STATUSCODE(10).
               IF CLP-2CLMSTAT = "13" MOVE 1 TO STATUSCODE(13).
               IF CLP-2CLMSTAT = "15" MOVE 1 TO STATUSCODE(15).
               IF CLP-2CLMSTAT = "16" MOVE 1 TO STATUSCODE(16).
               IF CLP-2CLMSTAT = "17" MOVE 1 TO STATUSCODE(17).
               IF CLP-2CLMSTAT = "19" MOVE 1 TO STATUSCODE(19).
               IF CLP-2CLMSTAT = "20" MOVE 1 TO STATUSCODE(20).
               IF CLP-2CLMSTAT = "21" MOVE 1 TO STATUSCODE(21).
               IF CLP-2CLMSTAT = "22" MOVE 1 TO STATUSCODE(22).
               IF CLP-2CLMSTAT = "23" MOVE 1 TO STATUSCODE(23).
               IF CLP-2CLMSTAT = "25" MOVE 1 TO STATUSCODE(25).
               IF CLP-2CLMSTAT = "27" MOVE 1 TO STATUSCODE(27).

      *     IF (PARM-ADDR = SPACE) AND (ID-NPI1 = SPACE)
      *      GO TO P1-CLP-2.
      *     IF ((PARM-ADDR = N3-STREET) OR (PARM-ADDR = N3-STREET2))
      *         AND (ID-NPI1 = ALF10-2)
      *     GO TO P1-CLP-2.
      *     IF ((PARM-ADDR = N3-STREET) OR (PARM-ADDR = N3-STREET2))
      *         AND (PARM-ADDR NOT = SPACE)
      *     AND ((ID-NPI1 NOT = ALF10-2) AND (ID-NPI1 NOT = SPACE))
      *     MOVE 1 TO NOT-FLAG
      *     GO TO P1-CLP-2.
      *     IF 
      *          ((PARM-ADDR NOT = N3-STREET) 
      *             AND (PARM-ADDR NOT = N3-STREET2)
      *             AND (PARM-ADDR NOT = SPACE))
      *     AND ((ID-NPI1 = ALF10-2) AND (ID-NPI1 NOT = SPACE))
      *     MOVE 2 TO NOT-FLAG
      *     GO TO P1-CLP-2.
      *     GO TO P1-CLP.

       NO-SURPRISE.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LQ-CNTR
             IF LQ-SVC(Y) = X               
               MOVE SPACE TO FILEIN01
               MOVE LQ-TAB(Y) TO FILEIN01
               MOVE SPACE TO LQ01
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                 LQ-0 LQ-1 LQ-2 

               IF LQ-2 = "N860" OR "N877"
                 MOVE LQ-2 TO rarc-key
                 READ rarcfile with lock
                   invalid
                     continue
                 end-read
                 PERFORM P1-LOST-SVC
               end-if
             end-if               
           END-PERFORM.    