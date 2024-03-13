       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea003.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARNEW ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARNEW-KEY
           LOCK MODE MANUAL.
           
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           
           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(70).

       FD  CHARNEW.
           copy "charnew.cpy" in "c:\Users\sid\cms\copylib\rri".

       FD  GARFILE.
           COPY "garfile.cpy" IN "C:\Users\sid\cms\copylib\rri".               

       WORKING-STORAGE SECTION.
       01  HOLD8 PIC X(8).

       01  SVC-DATE.
           02 SVC-YYYY PIC 9999.
           02 SVC-MMDD  PIC 9999.

       01  G-DATE.
           02 G-YYYY PIC 9999.
           02 G-MMDD  PIC 9999.

       01  G-AGE PIC 999.    

       PROCEDURE DIVISION.
       0005-START.
           OPEN I-O CHARNEW 
           OPEN OUTPUT FILEOUT.
           OPEN INPUT GARFILE
           MOVE SPACE TO CHARNEW-KEY.
       P1. 
           READ CHARNEW NEXT WITH LOCK AT END
               GO TO P2
           END-READ

      *    move these above check for 03 since these are acr registry
      *    009 paycode is QMM26: Screening Abdominal Aortic Aneurysm
      *    reporting with recommendations
           
           IF (CD-CPT = "76706")
             PERFORM CHECK-AGE
      *    measure is for 50 and up 
             IF (G-AGE < 50) 
               GO TO P1
             END-IF  

             MOVE "009" TO CD-PAYCODE
             MOVE SPACE TO FILEOUT01
             STRING "QMM26 " CD-PAYCODE " " CD-CPT " " CD-DATE-T " "
                CD-KEY8 " " CD-NAME DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             REWRITE CHARNEW01
             GO TO P1
           END-IF

      *    010 paycode is MSN15: Use of thyroid imaging reporting &
      *      and data system (TI-RADS) in final report to stratify
      *      thyroid nodule risk

           IF (CD-CPT = "76536")
             PERFORM CHECK-AGE
      *    measure is for 19 and up 
             IF (G-AGE < 19) 
               GO TO P1
             END-IF  
             MOVE "010" TO CD-PAYCODE
             MOVE SPACE TO FILEOUT01
             STRING "MSN15 " CD-PAYCODE " " CD-CPT " " CD-DATE-T " "
                CD-KEY8 " " CD-NAME DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             REWRITE CHARNEW01
             GO TO P1
           END-IF              

           IF (CD-PAYCODE NOT = "003")
               GO TO P1
           END-IF

      *    011 paycode is QMM19: DEXA/DXA and Fracture Risk Assess
      
           IF (CD-CPT = "77080" OR "77081" OR "77085" OR "77086")
             PERFORM CHECK-AGE
      *    measure is for 40 to 90
             IF NOT (G-AGE > 39 AND G-AGE < 91) 
               GO TO P1
             END-IF  
             MOVE "011" TO CD-PAYCODE
             MOVE SPACE TO FILEOUT01
             STRING "QMM19 " CD-PAYCODE " " CD-CPT " " CD-DATE-T " "
                CD-KEY8 " " CD-NAME DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             REWRITE CHARNEW01
             GO TO P1
           END-IF              

           IF (CD-PAYCODE NOT = "003")
               GO TO P1
           END-IF

            
      *    paycode 012 is measure 405  
           IF (CD-CPT =  "71250" OR "71260" OR "71270" OR "71271"
             OR "71275" OR "71555" OR "72131" OR "72191" OR "72192"
             OR "72193" OR "72194" OR "72195" OR "72196" OR "72197"
             OR "72198" OR "74150" OR "74160" OR "74170" OR "74176"
             OR "74177" OR "74178" OR "74181" OR "74182" OR "74183")
             MOVE "012" TO CD-PAYCODE
             MOVE SPACE TO FILEOUT01
             STRING "405 " CD-PAYCODE " " CD-CPT " " CD-DATE-T " "
               CD-KEY8 " " CD-NAME DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01 
             REWRITE CHARNEW01
             GO TO P1
           END-IF

      *    paycode 013 is measure 406       
	       IF (CD-CPT = "70486" OR "70487" OR "70488" OR "70490" 
             OR "70491" OR "70492" OR "70498" OR "70540" OR "70542" 
             OR "70543" OR "70547" OR "70548" OR "70549" OR "71250" 
             OR "71260" OR "71270" OR "71271" OR "71555" OR "72125" 
             OR "72126" OR "72127" OR "72141" OR "72142" OR "72156" 
             OR "71550" OR "71551" OR "71552")
             MOVE "013" TO CD-PAYCODE
             MOVE SPACE TO FILEOUT01
             STRING "406 " CD-PAYCODE " " CD-CPT " " CD-DATE-T " " 
               CD-KEY8 " " CD-NAME DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01 
             REWRITE CHARNEW01
             GO TO P1
           END-IF

      *    paycode 014 is measure 436
           IF (CD-CPT = "70450" OR "70460" OR "70470" OR "70480"
               OR "70481" OR "70482" OR "70486" OR "70487" OR "70488"
               OR "70490" OR "70491" OR "70492" OR "70496" OR "70498" 
               OR "71250" OR "71260" OR "71270" OR "71271" OR "71275"
               OR "72125" OR "72126" OR "72127" OR "72128" OR "72129"
               OR "72130" OR "72131" OR "72132" OR "72133" OR "72191"
               OR "72192" OR "72193" OR "72194"
               OR "73200" OR "73201" OR "73202" OR "73206"
               OR "73700" OR "73701" OR "73702" OR "73706"
               OR "74150" OR "74160" OR "74170" OR "74174" OR "74175"
               OR "74176" OR "74177" OR "74178" OR "74261" OR "74262"
               OR "74263" OR "75571" OR "75572" OR "75573" OR "75574"
               OR "75635" OR "76380" OR "76497" OR "77011" OR "77012"
               OR "77013" OR "77014" OR "77078" OR "0042T")
               MOVE "014" TO CD-PAYCODE
               MOVE SPACE TO FILEOUT01
               STRING "436 " CD-PAYCODE " " 
                      CD-CPT " " CD-DATE-T " " CD-KEY8 " " CD-NAME 
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               REWRITE CHARNEW01 
               GO TO P1
           END-IF

           GO TO P1.

       CHECK-AGE.
           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID               
               DISPLAY "GARNO NOT AVAILABLE FOR SOME UNKNOWN REASON"
               DISPLAY "PLEASE RECORD THIS FACT " CD-KEY8
               GO TO P1
           END-READ

           MOVE G-DOB TO G-DATE
           MOVE CD-DATE-T TO SVC-DATE

           COMPUTE G-AGE = SVC-YYYY - G-YYYY.

       P2.
           CLOSE CHARNEW.
           STOP RUN.


