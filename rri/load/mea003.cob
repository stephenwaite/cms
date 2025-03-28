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

      *    011 paycode is QMM19: DEXA/DXA and Fracture Risk Assess
      
           IF (CD-CPT = "77080" OR "77081" OR "77085" OR "77086")
             PERFORM CHECK-AGE
             IF G-AGE < 40 OR G-AGE > 90
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

      *    008 paycode is measure 145
           IF (CD-CPT = "0075T" OR "0202T" OR "0234T" OR "0235T" 
               OR "0236T" OR "0237T" OR "0238T" OR "0338T" OR "0339T"
               OR "22526" OR "25606" OR "25651" OR "26608" OR "26650"
               OR "26676" OR "26706" OR "26727" OR "27235" OR "27244"
               OR "27245" OR "27509" OR "27756" OR "27759" OR "28406"
               OR "28436" OR "28456" OR "28476" OR "33477" OR "33741"
               OR "33745" OR "33897" OR "33900" OR "33901" OR "33902"
               OR "33903" OR "34703" OR "34704" OR "34705"
               OR "34706" OR "34718" OR "34841" OR "34842" OR "34843" 
               OR "34844" OR "34845" OR "34846" OR "34847" OR "34848" 
               OR "36221" OR "36222" OR "36223" OR "36224"
               OR "36225" OR "36226" OR "36251" OR "36252" OR "36253"
               OR "36254" OR "36598" OR "36901" OR "36902" OR "36903"
               OR "36904" OR "36905" OR "36906" OR "37182" OR "37183"
               OR "37184" OR "37187" OR "37188" OR "37211" OR "37212"
               OR "37213" OR "37214" OR "37215" OR "37216" OR "37217"
               OR "37218" OR "37220" OR "37221" OR "37224" OR "37225"
               OR "37226" OR "37227" OR "37228" OR "37229" OR "37230"
               OR "37231" OR "37236" OR "37238" OR "37241" OR "37242" 
               OR "37243" OR "37244" OR "37246" OR "37248"
               OR "43260" OR "43261" OR "43262" OR "43263"
               OR "43264" OR "43265" OR "43274" OR "43275" OR "43276"
               OR "43277" OR "43278" OR "43752" OR "47537" OR "49440"
               OR "49441" OR "49442" OR "49446" OR "49450" OR "49451"
               OR "49452" OR "49460" OR "50382" OR "50384"
               OR "50385" OR "50386" OR "50387" OR "50389" 
               OR "61623" OR "61630" OR "61635" OR "61640" OR "61645"
               OR "61650" OR "62263" OR "62264" OR "62280" OR "62281"
               OR "62282" OR "62302" OR "62303" OR "62304" OR "62305")
               MOVE "008" TO CD-PAYCODE
               MOVE SPACE TO FILEOUT01
               STRING "145 " CD-PAYCODE " "
                      CD-CPT " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               REWRITE CHARNEW01
               GO TO P1
           END-IF

      *    measure 145 again :)
           IF (CD-CPT = "70010" OR "70015" OR "70170"
               OR "70332" OR "70370" OR "70371" OR "70390" OR "72240"
               OR "72255" OR "72265" OR "72270" OR "72285"
               OR "72295" OR "73040" OR "73085" OR "73115" OR "73525"
               OR "73580" OR "73615" OR "74210" OR "74220"
               OR "74221" OR "74235" OR "74240" OR "74246"
               OR "74251" OR "74270"
               OR "74280" OR "74300" OR "74328"
               OR "74329" OR "74330" OR "74340" OR "74355" OR "74360"
               OR "74363" OR "74425" OR "74440" OR "74445"
               OR "74450" OR "74470" OR "74485"
               OR "74742" OR "75600" OR "75605" OR "75625" OR "75630"
               OR "75705" OR "75710" OR "75716" OR "75726" OR "75731"
               OR "75733" OR "75736" OR "75741" OR "75743" OR "75746"
               OR "75756" OR "75801" OR "75803" OR "75805" OR "75807"
               OR "75810" OR "75825" OR "75827"
               OR "75831" OR "75833" OR "75840" OR "75842" OR "75860"
               OR "75870" OR "75872" OR "75880" OR "75885" OR "75887"
               OR "75889" OR "75891" OR "75893" OR "75894" OR "75898"
               OR "75901" OR "75902" OR "75956" OR "75957" OR "75958"
               OR "75959" OR "75970" OR "76000" OR "76080" OR "76496" 
               OR "77001" OR "77002" OR "77003"   
               OR "92611" OR "93451" OR "93452" OR "93453" OR "93454"
               OR "93455" OR "93456" OR "93457" OR "93458" OR "93459"
               OR "93460" OR "93461" OR "93503" OR "93505" OR "93580" 
               OR "93581"
               OR "93583" OR "93593" OR "93594" OR "93595" OR "93596"
               OR "93597" OR "G0106" OR "G0120" OR "G0122")
               MOVE "008" TO CD-PAYCODE
               MOVE SPACE TO FILEOUT01
               STRING "145 " CD-PAYCODE " "
                      CD-CPT " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               REWRITE CHARNEW01
               GO TO P1
           END-IF

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


