      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr250.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
             ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
             ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.
      *       STATUS IS GAR-STAT.

           SELECT ORDFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS ORDNO
             ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT PROCFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
             LOCK MODE MANUAL.

           SELECT CHARNEW ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARNEW-KEY
             LOCK MODE MANUAL.

           SELECT CLAIMFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS CLAIM-KEY
             STATUS IS CLM-STAT
             LOCK MODE MANUAL.

           SELECT ORD-DELETES ASSIGN TO "S60" ORGANIZATION IS LINE
             SEQUENTIAL.

           SELECT NEW-GARNOS ASSIGN TO "S70" ORGANIZATION IS LINE
             SEQUENTIAL.

           SELECT WORK249 ASSIGN TO "S75" ORGANIZATION LINE
             SEQUENTIAL.

           SELECT FILEIN ASSIGN TO "S80" ORGANIZATION LINE
             SEQUENTIAL.

           SELECT INSFILE ASSIGN TO "S85" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
             ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
             LOCK MODE MANUAL.
          
       DATA DIVISION.
       FILE SECTION.

       FD  INSFILE.
           COPY insfile.CPY IN "C:\Users\sid\cms\copylib".      

       FD  CHARNEW.
           COPY charnew.CPY IN "C:\Users\sid\cms\copylib\rri".      

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".      

       FD  ACTFILE.
           COPY actfile.CPY IN "C:\Users\sid\cms\copylib\rri".      
      

       FD  FILEIN.
       01  FILEIN01 PIC X(8).

       FD  WORK249.
       01  WORK24901 PIC X(8).
       
       FD  NEW-GARNOS.
       01  NEW-GARNOS01.
           02 NEW-GARNOS2 PIC X(8).
           02 NEW-GARNOS1 PIC X(8).

       FD  ORD-DELETES.
       01  ORD-DELETES01 PIC X(11).

       FD  CLAIMFILE
           DATA RECORD IS CLAIM01.
       01  CLAIM01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib\rri".      

       FD  ORDFILE.
           COPY ordfile.CPY IN "C:\Users\sid\cms\copylib\rri".      

       WORKING-STORAGE SECTION.
       01  ANS PIC X.
       01  GAR-STAT PIC XX.
       01  CLM-STAT PIC XX.
       01  CHARBACK PIC X(258).
       01  INPUT-DATE-S.      
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE-S.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01 DATE-X PIC X(8).
       01     NAME-CYCLE.
             03 NC1 PIC X.
             03 NC2 PIC X.
             03 FILLER PIC X(22).
       01     YEARDAY.
             03 YEAR-1.
               04 YD1 PIC X.
               04 YD2 PIC X.
             03 DAY3 PIC XXX.
       01 NUM-3 PIC 999.
       01  XYZ PIC 9.
       01  XXX PIC 999 VALUE 0.
       01  GARBACK.
           02 HOLD-GARNO PIC X(8).
           02 FILLER PIC X(301).
       01  CNTR PIC 99 VALUE 0.
       01  CINS.
           02 CINS1 PIC X.
           02 CINS2 PIC XX.
       01  FLAG5999 PIC 9 VALUE 0.
       01  X-COLLT PIC X.
       01  X-DUNNING PIC X.
       01  X-ACCTSTAT PIC X.
       01  X-INSPEND PIC S9(5)V99.
       01  X-LASTBILL PIC X(8).
       01  X-BILLCYCLE PIC X.
       01   X-PRINS    PIC XXX.
       01   X-PR-ASSIGN PIC X.
       01   X-PRIPOL  PIC X(16).
       01   X-PR-GROUP PIC X(10).
       01   X-PRNAME  PIC X(24).
       01   X-PR-RELATE PIC X.
       01   X-SEINS    PIC XXX.
       01   X-SE-ASSIGN PIC X.
       01   X-SECPOL  PIC X(16).
       01   X-SE-GROUP PIC X(10).
       01   X-SENAME  PIC X(24).
       01   X-SE-RELATE PIC X.
       01  LNAME PIC X(24).
       01  FNAME PIC X(24).
       01  MNAME PIC X(24).
       01  LNAME2 PIC X(24).
       01  FNAME2 PIC X(24).
       01  MNAME2 PIC X(24).
       01  FLAGPROC PIC 9.

      *
       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT ACTFILE ORDFILE PROCFILE WORK249 FILEIN INSFILE.
           OPEN I-O GARFILE CHARNEW CLAIMFILE.
           OPEN OUTPUT ORD-DELETES NEW-GARNOS.
           MOVE "A" TO CLAIM-KEY
           READ CLAIMFILE WITH LOCK
             INVALID
               DISPLAY "CLAIMFILE IS BAD. THIS PROG. IS TERMINATED"
               GO TO P11
           END-READ

           IF CLM-STAT = "61"
               DISPLAY "CLAIMFILE IS BEING WRITTEN"
               DISPLAY "TRY 250 RUN WHEN CLAIMFILE IS FREE"
               GO TO P11
           END-IF

           READ FILEIN
             AT END
               DISPLAY "NO HIGH DATE RECORD."
               GO TO P11
           END-READ.

       P1. 
           READ WORK249
             AT END
               GO TO P11
           END-READ

           IF WORK24901 NOT NUMERIC 
               DISPLAY WORK24901 " NON-NUMERIC ACCOUNT"
               ACCEPT X-ACCTSTAT
               GO TO P1
           END-IF

           MOVE WORK24901 TO A-ACTNO
           READ ACTFILE
             INVALID
               DISPLAY "BAD ACCT " A-ACTNO " SHOULD NEVER HAPPEN!"
               ACCEPT X-ACCTSTAT
               GO TO P1
           END-READ           

           IF (A-GARNO = SPACE)
             GO TO P2
           END-IF

           MOVE A-GARNO TO G-GARNO.

       P1-0.

           READ GARFILE WITH LOCK
             INVALID
               DISPLAY A-GARNO " " A-ACTNO
               DISPLAY "INVALID GARNO, A NEW ACCOUNT WILL BE CREATED"
               ACCEPT OMITTED                      
               GO TO P2
           END-READ           

           IF (G-DUNNING NOT = "1" AND G-PRINS NOT = "003")
               DISPLAY G-GARNO " " G-GARNAME " NEW ACCOUNT STARTED"
                 " SINCE GARNO WAS IN COLLECTION"
               ACCEPT OMITTED      
               GO TO P2
           END-IF

           GO TO REWRITE-NEW.

       P2. 

           MOVE ACTFILE01 TO GARFILE01
           MOVE A-ACTNO TO G-ACCT
           ACCEPT YEARDAY FROM DAY.
           MOVE YD2 TO G-PRIVATE.
           MOVE G-GARNAME TO NAME-CYCLE.
           MOVE "4" TO G-BILLCYCLE.

           IF NC1 = "1" MOVE NC2 TO NC1.
           
           IF NC1 < "T" MOVE "3" TO G-BILLCYCLE.
           
           IF NC1 < "K" MOVE "2" TO G-BILLCYCLE.
           
           IF NC1 < "F" MOVE "1" TO G-BILLCYCLE.

       P3.
           MOVE SPACE TO G-GARNO.
           MOVE GARFILE01 TO GARBACK
           MOVE DAY3 TO ID2 NUM-3.
           MOVE G-GARNAME(1:3) TO ID1.
           MOVE 0 TO XYZ.
           MOVE "G" TO ID3(2:1).

       P4.
           ADD 1 TO XYZ.
           MOVE XYZ TO ID3.
           MOVE G-GARNO TO HOLD-GARNO.
           READ GARFILE
             INVALID KEY 
               GO TO P5
           END-READ

           IF XYZ = 9 
               ADD 1 TO NUM-3
               MOVE NUM-3 TO ID2
               MOVE 0 TO XYZ
           END-IF
           
           GO TO P4.

       P5.

           MOVE GARBACK TO GARFILE01.
           
           IF G-PRINS = "003" AND G-SEINS = "076"
               MOVE "662" TO G-SEINS
               MOVE "A" TO G-SE-ASSIGN
           END-IF
           
           MOVE HOLD-GARNO TO A-GARNO.
           IF (G-PRINS = "003" AND G-SEINS = "715")
               MOVE "062" TO G-SEINS
               MOVE "A" TO G-SE-ASSIGN
               MOVE SPACE TO G-PR-GROUP
               MOVE "0099001" TO G-PR-GROUP
           END-IF

           MOVE "1" TO G-DELETE
           IF (G-PRINS = "003") AND (G-SEINS = "108" OR "116")
               MOVE SPACE TO G-PR-GROUP
               MOVE "0000567" TO G-PR-GROUP
               MOVE "062" TO G-SEINS
           END-IF

           MOVE SPACE TO LNAME FNAME MNAME
               LNAME2 FNAME2 MNAME2
           UNSTRING G-GARNAME DELIMITED BY ";" INTO
               LNAME FNAME MNAME
           UNSTRING G-PRNAME DELIMITED BY ";" INTO
               LNAME2 FNAME2 MNAME2

           IF LNAME = LNAME2
               AND FNAME = FNAME2
               AND G-RELATE NOT = G-PR-RELATE
               MOVE G-GARNAME TO G-PRNAME
               MOVE G-RELATE TO G-PR-RELATE
           END-IF
           
           INSPECT G-STREET REPLACING ALL ":" BY " ".
           INSPECT G-BILLADD REPLACING ALL ":" BY " ".
           WRITE GARFILE01
             INVALID
               DISPLAY "NO UPDATE " G-GARNO
               ACCEPT CD-DX3
               GO TO P1
           END-WRITE

      *     IF GAR-STAT = "61" GO TO P4.
           MOVE HOLD-GARNO TO NEW-GARNOS2
           MOVE A-ACTNO TO NEW-GARNOS1 
           WRITE NEW-GARNOS01.
           CLOSE ACTFILE
           OPEN I-O ACTFILE
           READ ACTFILE WITH LOCK.
           MOVE HOLD-GARNO TO A-GARNO
           REWRITE ACTFILE01
           CLOSE ACTFILE
           OPEN INPUT ACTFILE.

       P5-0.
           MOVE G-PRINS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY G-GARNO " " G-PRINS " " G-GARNAME 
                 "  HAS AN INVALID PRIMARY INSURANCE"
               DISPLAY " FIX THIS IN GP AND LET STEVE KNOW"
               ACCEPT OMITTED    
               MOVE "001" TO G-PRINS
           END-READ

           MOVE G-GARNO TO CD-PATID
           MOVE "0000000" TO CD-DIAG CD-DX2 CD-DX3 CD-DX4
           MOVE SPACE TO CD-MOD2 CD-MOD3 CD-MOD4 CD-QP1 CD-QP2 CD-DX5-3 
             CD-DX6.
           
           MOVE G-PRINS TO CD-PAYCODE
           MOVE "0" TO CD-STAT
           MOVE "01" TO CD-WORK
           MOVE "1" TO CD-RESULT
           MOVE "4" TO CD-ACT
           MOVE "2" TO CD-SORCREF
           MOVE "0" TO CD-COLLT
           MOVE "0" TO CD-AGE
           MOVE G-GARNAME TO CD-NAME
           MOVE "0" TO CD-EPSDT
           MOVE "00000000" TO CD-DATE-A
           MOVE " " TO CD-ACC-TYPE
           MOVE INS-CLAIMTYPE TO CD-PAPER
           MOVE INS-ASSIGN TO CD-ASSIGN
           MOVE INS-NEIC-ASSIGN TO CD-NEIC-ASSIGN.

       P6. 
           MOVE A-ACTNO TO ORD8  
           MOVE "   " TO ORD3.
           
           START ORDFILE KEY NOT < ORDNO
             INVALID
               GO TO P1
           END-START

           MOVE G-GARNAME TO CD-NAME
           MOVE 0 TO XXX
           MOVE G-GARNO TO CD-KEY8 
           MOVE "000" TO CD-KEY3.

       P7. 
           READ ORDFILE NEXT
             AT END
               GO TO P1
           END-READ

           IF ORD8 NOT = A-ACTNO
               GO TO P1
           END-IF

           IF C-IND = "-"
               GO TO P7
           END-IF

           MOVE C-DOCP TO CD-DOCP
           MOVE C-REF TO CD-DOCR
           MOVE C-DATE-A TO CD-DAT1.
           MOVE C-IOPAT TO CD-PLACE
           MOVE C-DATE-T TO CD-DATE-T.
           MOVE C-DATE-ADMIT TO CD-DATE-M
           MOVE C-CLINICAL TO CD-CLINICAL
           MOVE C-ADMIT-DIAG TO CD-ADMIT-DIAG
           ACCEPT CD-DATE-E FROM CENTURY-DATE.

           MOVE C-PROC TO PROC-CDM
           MOVE C-CPT TO PROC-CPT
           MOVE "26" TO PROC-MOD.
           
       P8. 
           READ PROCFILE 
            INVALID
               MOVE "  " TO PROC-MOD
               READ PROCFILE
                 INVALID
                   GO TO P8-EXIT
               END-READ    
           END-READ

           MOVE PROC-KEY TO CD-PROC           
           MOVE C-MOD2 TO CD-MOD2
           MOVE C-MOD3 TO CD-MOD3
           MOVE C-MOD4 TO CD-MOD4
           MOVE PROC-AMOUNT TO CD-AMOUNT
           MOVE PROC-TYPE TO CD-SERVICE
           ACCEPT CD-ORDER FROM TIME

           ADD 1 TO CLAIMNO 
           MOVE CLAIMNO TO CD-CLAIM
           MOVE CHARNEW01 TO CHARBACK.

       P9.
           ADD 1 TO XXX 
           MOVE XXX TO CD-KEY3
           READ CHARNEW
             INVALID
               GO TO P10
           END-READ

           GO TO P9.

       P10.
           MOVE CHARBACK TO CHARNEW01
           MOVE XXX TO CD-KEY3.
           MOVE "01" TO CD-WORK
           WRITE CHARNEW01. 
           WRITE ORD-DELETES01 FROM ORDNO
           GO TO P7.
       
       P8-EXIT.
           DISPLAY CD-NAME
           DISPLAY C-CPT
           DISPLAY CD-DATE-T
           DISPLAY "NON MATCHING CPT BETWEEN HOSPRRI AND PROCFILE"
           DISPLAY "FOR HOSP CODE " PROC-CDM "." 
           DISPLAY "THIS RECORD WILL BE DISCARDED"
           DISPLAY "BUT MUST BE CORRECTED IN HOSPRRI AND USED"
           DISPLAY "NOTIFY STEPHEN IMMEDIATELY."
           ACCEPT OMITTED
           GO TO P7.

       REWRITE-NEW. 

      * save some G details
           MOVE G-COLLT TO X-COLLT
           MOVE G-DUNNING TO X-DUNNING
           MOVE G-ACCTSTAT TO X-ACCTSTAT
           MOVE G-INSPEND TO X-INSPEND
           MOVE G-LASTBILL TO X-LASTBILL
           MOVE G-BILLCYCLE TO X-BILLCYCLE
      * overwrite garfile with actfile and bring billing details back
      * and the garno from actfile
           MOVE ACTFILE01 TO GARFILE01
           MOVE A-GARNO TO G-GARNO
           MOVE A-ACTNO TO G-ACCT
           MOVE X-COLLT TO G-COLLT
           MOVE X-DUNNING TO G-DUNNING
           MOVE X-ACCTSTAT TO G-ACCTSTAT
           MOVE X-INSPEND TO G-INSPEND
           MOVE X-LASTBILL TO G-LASTBILL
           MOVE X-BILLCYCLE TO G-BILLCYCLE
           MOVE "1" TO G-DELETE

           IF NOT (G-BILLCYCLE = "1" OR "2" OR "3" OR "4")
               MOVE "1" TO G-BILLCYCLE
           END-IF    
           
           IF (G-PRINS = "003") AND (G-SEINS = "108" OR "116")
               MOVE SPACE TO G-PR-GROUP
               MOVE "0000567" TO G-PR-GROUP
               MOVE "062" TO G-SEINS
           END-IF    
           
           MOVE SPACE TO LNAME FNAME MNAME LNAME2 FNAME2 MNAME2
           UNSTRING G-GARNAME DELIMITED BY ";" INTO LNAME FNAME MNAME
           UNSTRING G-PRNAME DELIMITED BY ";" INTO LNAME2 FNAME2 MNAME2
           
           IF (LNAME = LNAME2 AND FNAME = FNAME2
             AND G-RELATE NOT = G-PR-RELATE)
             MOVE G-GARNAME TO G-PRNAME
             MOVE G-RELATE TO G-PR-RELATE
           END-IF
           
           INSPECT G-BILLADD REPLACING ALL ":" BY " ".
           INSPECT G-STREET REPLACING ALL ":" BY " ".

           REWRITE GARFILE01
             INVALID 
               DISPLAY "NO REWRITE ON " G-GARNO
               DISPLAY "CHARGE(S) WILL BE LOST! CALL STEVE."
               ACCEPT ANS
               GO TO P1
           END-REWRITE    

           GO TO P5-0.

       P11.
           REWRITE CLAIM01.
           
           IF CLM-STAT = "61" 
               DISPLAY CLAIM01
               GO TO P11
           END-IF    
           
           CLOSE ACTFILE ORDFILE PROCFILE WORK249 FILEIN INSFILE
               GARFILE CHARNEW CLAIMFILE ORD-DELETES NEW-GARNOS.
           DISPLAY "POSTING PROGRAM HAS ENDED".
           STOP RUN.
