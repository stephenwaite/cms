      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rep136.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PARMFILE ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

          
           SELECT ERROR-FILE ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

      
      
       FD  PARMFILE.
       01  PARMFILE01.
           02 PF-1 PIC X(10).
           02 PF-2 PIC XXX.
           02 PARMCODE PIC XXX.
           02 PF-3 PIC X(27).

       FD ERROR-FILE.
       01 ERROR-FILE01 PIC X(132).
       FD FILEIN.
       01  FILEIN01.
           02 F0.
             03 F1 PIC XXX.
             03 F2 PIC X(4).
           02 F3 PIC X(113).
      

       WORKING-STORAGE SECTION.

           COPY "hip5010_835.cpy" IN "C:\Users\sid\cms\copylib".      
      
       01  HL01.
           02 HL-1 PIC X(40) VALUE SPACE.
           02 FILLER PIC X(21) VALUE SPACE.
           02 HL-2 PIC X(27) VALUE "  BCBSVT   UNPOSTED LIST   ".
           02 FILLER PIC X(5) VALUE SPACE.
           02 HL-3 PIC X(8).
       
       01  ERR01.
           02 EF1 PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 EF2 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 EF3 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 EF-PROC PIC X(11).
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
           02 FILLER PIC X(19) VALUE SPACE.
           02 T2 PIC X(5) VALUE "HIC #".
           02 FILLER PIC X(5) VALUE SPACE.
           02 T3 PIC X(6) VALUE " DATE ".
           02 FILLER PIC XXX VALUE SPACE.
           02 FILLER PIC X(12) VALUE " PROC       ".
           02 FILLER PIC X(14) VALUE "KEY           ".
           02 T5 PIC X(7) VALUE " AMOUNT".
           02 FILLER PIC XXX VALUE SPACE.
           02 T6 PIC X(7) VALUE "  PAID ".
           02 FILLER PIC X VALUE SPACE.
           02 T6 PIC X(7) VALUE " REDUCE".
           02 FILLER PIC XX VALUE SPACE.
           02 T7 PIC X(6) VALUE "I.C.N.".
           02 FILLER PIC X(7) VALUE SPACE.
           02 T9 PIC X(11) VALUE " STAT  DNL ".
           02 FILLER PIC X VALUE SPACE.
           02 T12 PIC X(10).
       01  XYZ PIC 999.
       01  TOT-AMT PIC S9(4)V99.
       01  PAYBACK PIC X(80).
       01  KEY11.
           02 KEY3 PIC XXX.
           02 KEY8 PIC X(8).
       01  CLAIM-TOT PIC S9(5)V99.
       01  FLAG PIC 9 VALUE 0.
       01  FLAGEND PIC 9 VALUE 0.
       01  FLAGZERO PIC 9.
       01  FLAGCAS PIC 9.
       01  TOT-PAY PIC S9(5)V99 VALUE 0.
       01 FLAGY PIC 9.
       01 FIND-CNTR PIC 99.
       01 CNTRY PIC 99.
       01 CNTR PIC 99.
       01  MULTCHAR PIC 99.
       01  X PIC 99.
       01  Y PIC 99.
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
       01  AMOUNT-Y PIC S9(4)V99.
       01  CHARGE-X PIC S9(4)V99.
       01  ALF5 PIC X(5).
       01  ALF8. 
           02 ALF8-1 PIC X.
           02 ALF8-7 PIC X(7).
       01  ALF9 PIC X(9).
       01  ALF10 PIC X(10).
       01  ALF-11.
           02 ALF-11-4 PIC X(4).
           02 ALF-11-7 PIC X(7).
       01  ALF-14.
           02 ALF14-1 PIC X(9).
           02 ALF14-2 PIC X(5).
       01  ALF-14X.
           02 ALF14X-1 PIC XXX.
           02 ALF14X-2 PIC X(9).
           02 ALF14X-3 PIC XX.
       01  ALF6-14.
           02 ALF614-1 PIC X.
           02 ALF614-2 PIC X(8).
           02 ALF614-3 PIC X(5).
       01  ALF-17.
           02 FILLER PIC XXX.
           02 ALF-14P PIC X(14).
       01  INPUT-DATE.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
       01  OVERFLAG PIC 9.
       01  CO-PAY-PAID PIC S9(7)V99.
       01  DATE-X PIC X(8).
       01  DATE-CC PIC X(8).
       01  SVC-CNTR PIC 99.
       01  CAS-CNTR PIC 99.
       01  LQ-CNTR PIC 99.

       01  SVC-TAB01.
           02 SVC-TAB PIC X(120) OCCURS 64 TIMES.
       01  SVC-DATE01.
           02 SVC-DATE PIC X(8) OCCURS 64 TIMES.
       01  FOUND-TAB01.
           02 FOUND-KEY PIC X(11) OCCURS 64 TIMES.

       01  CAS-TAB01.
           02 CAS-TAB PIC X(120) OCCURS 64 TIMES.
       01  CAS-SVC01.
           02 CAS-SVC PIC 99 OCCURS 64 TIMES.

        01  LQ-TAB01.
           02 LQ-TAB PIC X(120) OCCURS 64 TIMES.

       01  LQ-SVC01.
           02 LQ-SVC PIC 99 OCCURS 64 TIMES.

       01  SAVEFILE01 PIC X(120).
       01  CC-PROC1X PIC X(5).
       01  CC-PROC2X PIC XX.
       01  CC-MOD2X PIC XX.
       01  CC-MOD3X PIC XX.
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
       01  MNGD-CARE PIC XXX.
       01  TITLE-FLAG PIC 9 VALUE 0.
       01  TOT-CHARGE PIC S9(5)V99 VALUE 0.
       01  TOT-REDUCE PIC S9(5)V99 VALUE 0.
       01  STATUSCODES01.
           02 STATUSCODE PIC 9 OCCURS 27 TIMES.
       01  STATUSNAR01.
           02 STATUSNAR PIC X(25) OCCURS 27 TIMES.
       01  INS-REDUCE PIC S9(5)V99.
       01  BAL-REDUCE PIC S9(5)V99.
       01  CP-AMOUNT PIC S9(5)V99.
       01  ALF25 PIC X(25).
       01  NEF-2 PIC Z9.
       01  ALF2 PIC XX.
       01  PROV-1 PIC X(10).
       01  PROV-2 PIC X(10).
       01  PD-MANAGE PIC XX.
       01  PATRESP PIC XXX.
       01  DELIM PIC X.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN PARMFILE.
           OPEN OUTPUT ERROR-FILE.
           MOVE SPACE TO NAR-KEY01 
           MOVE ALL ZEROES TO NAR-CNTR01
      *     PERFORM STATUS-0
           MOVE SPACE TO ERROR-FILE01

           READ PARMFILE AT END GO TO P9.
           MOVE PARMFILE01 TO HL-1.

           READ PARMFILE AT END GO TO P9.
           MOVE PARMFILE01 TO MNGD-CARE.

           READ PARMFILE AT END GO TO P9.
           MOVE SPACE TO PROV-1 PROV-2
           UNSTRING PARMFILE01 DELIMITED BY " " INTO PROV-1 PROV-2
          
           MOVE SPACE TO ERROR-FILE01
           WRITE ERROR-FILE01 FROM HL01 AFTER PAGE.
           READ FILEIN AT END GO TO P9.
           MOVE FILEIN01(105:1) TO DELIM.
       P00.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P9.
           IF F1 NOT = "BPR" GO TO P00.
           MOVE SPACE TO BPR01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           BPR-0 BPR-1 BPR-2 BPR-3 BPR-4 BPR-5 BPR-6 BPR-7 BPR-8 
           BPR-9 BPR-10 BPR-11 BPR-12 BPR-13 BPR-14 BPR-15 BPR-16.
           MOVE BPR-16 TO DATE-X.
      
       P000.
           MOVE SPACE TO FILEIN01
           READ FILEIN 
             AT END
               GO TO P9.

       XX. 
           IF NOT (F1 = "N1*") GO TO P000.

           MOVE SPACE TO N101
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
             N1-0 N1-1 N1-2 N1-3 N1-ID
           
           IF N1-1 NOT = "PE" GO TO P000.
           
           MOVE N1-ID TO ALF10
           
           IF NOT (ALF10 = PROV-1 OR PROV-2) GO TO P00.
           
           IF TITLE-FLAG = 0
             MOVE 1 TO TITLE-FLAG
             MOVE SPACE TO ERROR-FILE01
             MOVE DATE-X TO TEST-DATE
             MOVE CORR TEST-DATE TO DISPLAY-DATE
             MOVE DISPLAY-DATE TO T12
             MOVE SPACE TO ERROR-FILE01
             MOVE TITLE01 TO ERROR-FILE01
             WRITE ERROR-FILE01.

       P1-CLP. 
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P9.

           IF F1 NOT = "CLP" GO TO P1-CLP.

       P1-CLP-1.
      *     display filein01
      *     accept omitted
           MOVE SPACE TO CLP01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
             CLP-0 CLP-1 CLP-2CLMSTAT CLP-3TOTCLMCHG CLP-4TOTCLMPAY 
             CLP-5PATRESP CLP-6PLANCODE CLP-7ICN CLP-8FACILITY 
             CLP-9FREQ CLP-10PATSTAT CLP-11DRG CLP-12QUAN CLP-13PERCENT.
           MOVE CLP-2CLMSTAT TO EF8
           MOVE SPACE TO NM101 CLMCAS01.
           MOVE SPACE TO SVC-DATE01
           MOVE 0 TO CAS-CNTR
           MOVE 0 TO SVC-CNTR
           move 0 to lq-cntr.
           
       P1-NM1.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P9.

           IF F1 = "SVC" GO TO P1-SVC-LOOP-0.

           IF F1 = "CLP" OR "SE*" 
             MOVE FILEIN01 TO SAVEFILE01
             GO TO P2-SVC-LOOP.

           IF F1 = "CAS" 
             display "we have a cas record"
             display filein01
             MOVE SPACE TO CAS01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
               CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
               CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
             IF CAS-1 = "OA" AND CAS-2 = "253"
                move space to error-file01
                STRING CLP-1 " " DTM-2 " " SVC-1PROCMOD " " CAS-3 
                  DELIMITED BY SIZE INTO ERROR-FILE01
                WRITE ERROR-FILE01
             END-IF     
             MOVE CAS01 TO CLMCAS01
             MOVE SPACE TO CAS01
             GO TO P1-NM1.

           IF (F1 = "NM1" AND F2 = "*QC*") 
             MOVE SPACE TO NM101
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF NM1-NAMEM 
               NM1-NAMES NM1-EINSS NM1-PREFIX NM1-CODE
             GO TO P1-NM1.

           IF (F1 = "DTM" AND F2 = "*232")
             MOVE SPACE TO DTM01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               DTM-0 DTM-1 DTM-2 
             MOVE DTM-2 TO DATE-CC SVC-DATE(1).

           GO TO P1-NM1.

       P1-SVC-LOOP.  
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P2-SVC-LOOP.
           
           IF F1 = "CLP" OR "SE*" 
             MOVE FILEIN01 TO SAVEFILE01
             GO TO P2-SVC-LOOP.

       P1-SVC-LOOP-0.    
           IF F1 = "SVC" 
             ADD 1 TO SVC-CNTR
             MOVE FILEIN01 TO SVC-TAB(SVC-CNTR)
             GO TO P1-SVC-LOOP.
           
           IF F1 = "CAS" 
             ADD 1 TO CAS-CNTR
             MOVE FILEIN01 TO CAS-TAB(CAS-CNTR)
             MOVE SVC-CNTR TO CAS-SVC(CAS-CNTR)
             GO TO P1-SVC-LOOP.
           
           IF F1 = "DTM" AND (F2 = "*150" OR F2 = "*472")
             MOVE SPACE TO DTM01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO 
               DTM-0 DTM-1 DTM-2
             MOVE DTM-2 TO SVC-DATE(SVC-CNTR)             
             GO TO P1-SVC-LOOP.

           IF F1 = "LQ*" 
             ADD 1 TO LQ-CNTR
             MOVE FILEIN01 TO LQ-TAB(LQ-CNTR)
             MOVE SVC-CNTR TO LQ-SVC(LQ-CNTR)
             GO TO P1-SVC-LOOP
           end-if   

           GO TO P1-SVC-LOOP.  

       P2-SVC-LOOP.
           IF F1 = "CLP" GO TO P1-CLP-1.

           GO TO XX.     

       P9.
           CLOSE filein    parmfile 
                error-file.
           STOP RUN.

      