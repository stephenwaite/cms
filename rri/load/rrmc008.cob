      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrmc008.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT PROCFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS DYNAMIC RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.

           SELECT ERRFILE ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERRFILE.
       01  ERRFILE01 PIC X(132).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(1070).

       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC XX.
           02 FI-2 PIC X(1068).

       FD  PROCFILE
           DATA RECORD PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(5).
             03 PROC-KEY3 PIC XX.
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.

       WORKING-STORAGE SECTION.

       01  REC101.
           02 R1-1 PIC XX.
           02 R1-PATNUM PIC X(8).
           02 R1-PATNAME.
             03 R1-PATNAME-L PIC X(19).
             03 R1-PATNAME-F PIC X(12).
           02 R1-PATADDR1 PIC X(25).
           02 R1-PATADDR2 PIC X(25).
           02 R1-PATCITY PIC X(25).
           02 R1-PATSTATE PIC XX.
           02 R1-PATZIP PIC X(10).
           02 R1-ADMIT.
             03 R1-ADMITMM PIC XX.
             03 FILLER PIC X.
             03 R1-ADMITDD PIC XX.
             03 FILLER PIC X.
             03 R1-ADMITYY PIC XXXX.
           02 R1-ADMITTIME PIC X(5).
           02 R1-WORKCOMP PIC X.
           02 R1-GARNAME.
             03 R1-GARNAME1.
               05 R1-GARNAME1-L PIC X(21).
               05 R1-GARNAME1-F PIC X(14).
           02 R1-GARADDR1 PIC X(25).
           02 R1-GARADDR2 PIC X(25).
           02 R1-GARCITY PIC X(25).
           02 R1-GARSTATE PIC XX.
           02 R1-GARZIP PIC X(10).
           02 R1-EMAIL PIC X(30).
           02 R1-IP1 PIC X(5).
           02 R1-ID1 PIC X(30).
           02 R1-CERT11 PIC X(20).
           02 R1-GRP1 PIC X(20).
           02 R1-GRPNAME11 PIC X(30).
           02 R1-SUBNAME11 PIC X(35).
           02 R1-EMPLOYNAME11 PIC X(30).
           02 R1-GENDER11 PIC X.
           02 FILLER PIC X.
           02 R1-DOB11 PIC X(10).
           02 R1-SSN11 PIC X(9).
           02 R1-RELATE1 PIC XX.
           02 INSURANCE-1.
             03 R1-INSNAME1 PIC X(25).
             03 R1-INSCONTACT1 PIC X(25).
             03 R1-INSADDR11 PIC X(20).
             03 R1-INSADDR21 PIC X(15).
             03 R1-INSCITY1 PIC X(20).
             03 R1-INSSTATE1 PIC XX.
             03 R1-INSZIP1 PIC X(10).
             03 R1-INSPHONE1 PIC X(12).
           02 R1-AUTH PIC X(20).
           02 R1-IP2 PIC X(5).
           02 R1-ID2 PIC X(30).
           02 R1-CERT22 PIC X(20).
           02 R1-GRP2 PIC X(20).
           02 R1-GRPNAME22 PIC X(30).
           02 R1-SUBNAME22 PIC X(35).
           02 R1-EMPLOYNAME22 PIC X(30).
           02 R1-GENDER22 PIC X.
           02 FILLER PIC X.
           02 R1-DOB22 PIC X(10).
           02 R1-SSN22 PIC X(9).
           02 R1-RELATE2 PIC XX.

           02 INSURANCE-2.
             03 R1-INSNAME2 PIC X(25).
             03 R1-INSCONTACT2 PIC X(25).
             03 R1-INSADDR12 PIC X(20).
             03 R1-INSADDR22 PIC X(15).
             03 R1-INSCITY2 PIC X(20).
             03 R1-INSSTATE2 PIC XX.
             03 R1-INSZIP2 PIC X(10).
             03 R1-INSPHONE2 PIC X(12).

       01  REC201.
           02 R2-1 PIC XX.
           02 R2-ACC.
             03 R2-ACCMM PIC XX.
             03 R2-ACCDD PIC XX.
             03 R2-ACCYY PIC XX.
             03 R2-ACCHHMM PIC X(5).
           02 R2-REFDOC.
             03 R2-REFDOC4 PIC XXXX.
             03 R2-REFDOC22 PIC X(18).
           02 R2-DIAG PIC X(130).
           02 R2-IP3 PIC X(5).
           02 R2-ID3 PIC X(30).
           02 R2-CERT33 PIC X(20).
           02 R2-GRP3 PIC X(20).
           02 R2-GRPNAME33 PIC X(30).
           02 R2-SUBNAME33 PIC X(35).
           02 R2-EMPLOYNAME33 PIC X(30).
           02 R2-GENDER33 PIC X.
           02 FILLER PIC X.
           02 R2-DOB33 PIC X(10).
           02 R2-SSN33 PIC X(9).
           02 R2-RELATE3 PIC XX.
           02 INSURANCE-3.
             03  R2-INSNAME3 PIC X(25).
             03  R2-INSCONTACT3 PIC X(25).
             03  R2-INSADDR13 PIC X(20).
             03  R2-INSADDR23 PIC X(15).
             03  R2-INSCITY3 PIC X(20).
             03  R2-INSSTATE3 PIC XX.
             03  R2-INSZIP3 PIC X(10).
             03  R2-INSPHONE3 PIC X(12).
           02 FILLER PIC X(20).
           02 R2-IP4 PIC X(5).
           02 R2-ID4 PIC X(30).
           02 R2-CERT44 PIC X(20).
           02 R2-GRP4 PIC X(20).
           02 R2-GRPNAME44 PIC X(30).
           02 R2-SUBNAME44 PIC X(35).
           02 R2-EMPLOYNAME44 PIC X(30).
           02 R2-GENDER44 PIC X.
           02 FILLER PIC X.
           02 R2-DOB44 PIC X(10).
           02 R2-SSN44 PIC X(9).
           02 R2-RELATE4 PIC XX.
           02 INSURANCE-4.
             03  R2-INSNAME4 PIC X(25).
             03  R2-INSCONTACT4 PIC X(25).
             03  R2-INSADDR14 PIC X(20).
             03  R2-INSADDR24 PIC X(15).
             03  R2-INSCITY4 PIC X(20).
             03  R2-INSSTATE4 PIC XX.
             03  R2-INSZIP4 PIC X(10).
             03  R2-INSPHONE4 PIC X(12).
           02 FILLER PIC X(20).
           02 R2-MEDREC.
             03 R2-MEDREC1 PIC XX.
             03 FILLER PIC X.
             03 R2-MEDREC2 PIC XX.
             03 FILLER PIC X.
             03 R2-MEDREC3 PIC XX.
           02 R2-EMPNAME PIC X(30).
           02 R2-EMPLOYER-ADDR1 PIC X(25).
           02 R2-EMPLOYER-ADDR2 PIC X(25).
           02 R2-EMPLOYER-CITY PIC X(25).
           02 R2-EMPLOYER-STATE PIC XX.
           02 R2-EMPLOYER-ZIP PIC X(10).
           02 R2-GUARSSN PIC X(9).
           02 R2-PHONE.
             03 R2-PHONE1 PIC XXX.
             03 FILLER PIC X.
             03 R2-PHONE2 PIC XXX.
             03 FILLER PIC X.
             03 R2-PHONE3 PIC X(4).
           02 R2-DOC PIC X(8).
           02 R2-REFIND PIC X.
           02 R2-NPI PIC X(10).
           02 R2-ADMIT-DATE-DUPLICATE PIC X(6).
           02 R2-PATSEX PIC X.
           02 R2-PATDOB.
             03 R2-DOBMM PIC XX.
             03 R2-DOBDD PIC XX.
             03 R2-DOBYY PIC XX.
           02 R2-DISCHARGE PIC X(6).

       01  REC301.
           02 R3-1 PIC XX.
           02 R3-IND PIC XXX.
           02 R3-DEPT PIC XX.
           02 R3-GLC PIC X.
           02 R3-PROC.
             03 R3-PROC1 PIC X.
             03 FILLER PIC XXX.
           02 R3-DATE.
              03 R3-DATEMM PIC XX.
              03 R3-DATEDD PIC XX.
              03 R3-DATEYY PIC XX.
           02 R3-UNIT PIC XXX.
      * col 22
           02 R3-CLINICAL PIC X(40).
           02 FILLER PIC X(24).
      * col 86
           02 R3-PLACE PIC X(4).
           02 R3-DOCP PIC X(4).
           02 R3-DOCPFILLER PIC X(18).
      * col 112
           02 R3-CPT PIC X(5).
           02 FILLER PIC X(3).
      * col 120
           02 R3-HCPCS PIC X(5).
           02 FILLER PIC X(3).
      * col 128
           02 R3-MOD1 PIC X(2).
           02 FILLER PIC X.
      * col 131
           02 R3-MOD2 PIC X(2).
           02 FILLER PIC X.
      * col 134
           02 R3-MOD3 PIC X(2).
           02 FILLER PIC X(5).
      * col 141
           02 R3-OBSERV PIC X(25).
      * col 166
           02 FILLER PIC X(15).
      * col 181
           02 R3-LOCO PIC X(4).
           02 FILLER PIC X(36).
      * col 221
           02 R3-ACCESSION PIC X(7).     

       01  ANS PIC X.
       01  HOLDNAME PIC X(15).
       01  ALF13 PIC X(13).
       01  ALF20 PIC X(20).
       01  MEDREC PIC X(6).
       01  BILAT-FLAG PIC X.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN PROCFILE.
           OPEN OUTPUT FILEOUT ERRFILE.
           MOVE "0" TO BILAT-FLAG.

       P1.
           READ FILEIN
             AT END
               GO TO P99
           END-READ

           IF FI-1 = "##"
               MOVE FILEIN01(10:15) TO HOLDNAME
           END-IF

           IF FI-1 = "##"
               MOVE FILEIN01 TO REC101
           END-IF

           IF FI-1 = "++"
               MOVE FILEIN01 TO REC201
               STRING R2-MEDREC1 R2-MEDREC2 R2-MEDREC3
                   DELIMITED BY SIZE INTO MEDREC
           END-IF

           IF FI-1 NOT = "$$"
               WRITE FILEOUT01 FROM FILEIN01
               GO TO P1
           END-IF

           MOVE FILEIN01 TO REC301

           if r3-cpt not = space and r3-hcpcs not = space
             move space to ERRFILE01
             STRING "for " MEDREC " rrmc sent us both a hcpcs "
               R3-hcpcs " AND a cpt " R3-CPT " with mod " R3-MOD1
               " for DOS " R3-DATE
               DELIMITED BY SIZE INTO ERRFILE01
             WRITE ERRFILE01
           end-if

           MOVE R3-PROC TO PROC-KEY1

           IF R3-CPT = SPACE
               MOVE R3-HCPCS TO PROC-KEY2
           ELSE
               MOVE R3-CPT TO PROC-KEY2
           END-IF

           MOVE "26" TO PROC-KEY3.

       P2.
           READ PROCFILE
             INVALID
               MOVE "  " TO PROC-KEY3
               READ PROCFILE
                 INVALID
                   GO TO BAD-1
               END-READ
           END-READ           

           IF R3-PROC = "6250" and 
             (r3-cpt = "C8901" or r3-hcpcs = "C8901")
               move space to ERRFILE01
               string "RRMC SENT US " R3-CPT " " R3-HCPCS
                 " CHANGING THIS TO CPT 74185" 
                   delimited by size INTO ERRFILE01    
               write errfile01               
               MOVE "74185   " TO R3-CPT    
           END-IF

            IF R3-PROC = "6251" and 
              (r3-cpt = "C8900" or r3-hcpcs = "C8900")
               move space to ERRFILE01
               string "RRMC SENT US " R3-CPT " " R3-HCPCS
                 " CHANGING THIS TO CPT 74185" 
                   delimited by size INTO ERRFILE01    
               write errfile01               
               MOVE "74185   " TO R3-CPT    
           END-IF

           IF R3-PROC = "6252" and 
             (r3-cpt = "C8902" OR r3-hcpcs = "C8902")
               move space to ERRFILE01
               string "RRMC SENT US " R3-CPT " " R3-HCPCS
                 " CHANGING THIS TO CPT 74185" 
                   delimited by size INTO ERRFILE01    
               write errfile01               
               MOVE "74185" TO R3-CPT    
           END-IF

           IF R3-PROC = "6327" and 
              (r3-cpt = "C8908" OR r3-hcpcs = "C8908")
               move space to ERRFILE01
               string "RRMC SENT US " R3-CPT " " R3-HCPCS
                 " CHANGING THIS TO CPT 77049" 
                   delimited by size INTO ERRFILE01    
               write errfile01               
               MOVE "77049" TO R3-CPT    
           END-IF

           IF R3-PROC = "6254" and 
              (r3-cpt = "C8909" OR r3-hcpcs = "C8909")
               move space to ERRFILE01
               string "RRMC SENT US " R3-CPT " " R3-HCPCS
                 " CHANGING THIS TO CPT 71555" 
                   delimited by size INTO ERRFILE01    
               write errfile01               
               MOVE "71555" TO R3-CPT    
           END-IF  

           IF (R3-PROC = "6263" OR "6267") 
              AND (r3-cpt = "C8912" OR r3-hcpcs = "C8912")
               move space to ERRFILE01
               string "RRMC SENT US " R3-CPT " " R3-HCPCS
                 " CHANGING THIS TO CPT 73725" 
                   delimited by size INTO ERRFILE01    
               write errfile01               
               MOVE "73725" TO R3-CPT    
           END-IF

           IF PROC-AMOUNT = 0
               AND R3-GLC NOT = 0
               GO TO BAD-2
           END-IF

           if R3-LOCO = "RVOC"
             if R3-MOD1 = "26"
               move space to R3-MOD1
               if r3-mod2 not = space
                 move r3-mod2 to R3-MOD1
               end-if  
             end-if

             if R3-MOD1 = R3-MOD2
               MOVE SPACE TO R3-MOD2
             end-if    
           end-if 

           IF ((R3-PROC = "0111" or "1204" OR "1283" OR "1284" or "1285" 
                or "3030" or "3085" or "5024" or "5282") 
             AND R3-MOD1 = "  "
             AND BILAT-FLAG = "1")
             MOVE SPACE TO ERRFILE01
             STRING "CHANGING BILAT STUDY TO RT LT, MEDREC " 
               MEDREC " CDM " R3-PROC " CPT " R3-CPT " MOD1 " R3-MOD1 
               " DOS " R3-DATE
               DELIMITED BY SIZE INTO ERRFILE01
             WRITE ERRFILE01
      *    special handling for cdm 1285 cpt 73562 from rrmc
      *    set flag back to 0
             MOVE "0" TO BILAT-FLAG
             MOVE "LT" TO R3-MOD1
           end-if

      *    VT Medicaid threw a wrench
           IF (R3-PROC = "0111" or "1204" OR "1283" or "1284" or "1285"
               OR "3030" or "3085" or "5024" or "5282") 
             AND R3-MOD1 = "50"
      *    RVOC sends 50 mod twice sometimes :|         
             IF BILAT-FLAG = "1"
               MOVE "0" TO BILAT-FLAG
               MOVE "LT" TO R3-MOD1
             ELSE   
               MOVE "1" TO BILAT-FLAG
               MOVE "RT" TO R3-MOD1
             END-IF  
           end-if                                    

           WRITE FILEOUT01 FROM REC301
           GO TO P1.

       BAD-1.
           MOVE SPACE TO ERRFILE01.
           STRING "UNDEFINED PROCEDURE FOR MRN " MEDREC
             " CDM " R3-PROC " CPT " R3-CPT " HCPCS " R3-HCPCS
             " DOS " R3-DATE DELIMITED BY SIZE INTO ERRFILE01
           WRITE ERRFILE01

           IF R3-GLC = 0
             MOVE SPACE TO ERRFILE01
             if R3-LOCO = "RVOC"
               STRING "** STOP and ADD THIS CDM-CPT IN THE 52 "
                 "TO CAPTURE THE new RVOC CDM, re-run qqq thank you. " 
                   r3-loco " **"
               delimited BY size INTO ERRFILE01
               WRITE ERRFILE01
             else
               STRING "DON'T WORRY ABOUT IT DUE TO RRMC AUC "
               delimited BY size INTO ERRFILE01
               WRITE ERRFILE01
             END-IF
           else
             STRING "** STOP! And ADD THIS CDM-CPT IN THE 52 "
               "TO CAPTURE THE NEW RRMC CDM, re-run qqq "
               "thank you. " r3-loco " **"
               delimited BY size INTO ERRFILE01
               WRITE ERRFILE01
           end-if    

           GO TO P1.

       BAD-2.
           MOVE SPACE TO ERRFILE01.

           STRING "zero dollar procedure for mrn " MEDREC
             " CDM " PROC-KEY1 " CPT " R3-CPT " HCPCS " R3-HCPCS
             " DOS " R3-DATE " safe to ignore, usually is but DBL CK!"
             DELIMITED BY SIZE INTO ERRFILE01

           WRITE ERRFILE01.

           GO TO P1.

       P99.
           CLOSE PROCFILE FILEIN FILEOUT ERRFILE.
           STOP RUN.
