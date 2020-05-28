       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DIAGFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PROVCAID ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROV-KEY
           ALTERNATE RECORD KEY IS PROV-NPI WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-TAX WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-NAME WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT UPINFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS UPIN-PROVNUM
             ALTERNATE RECORD KEY IS UPIN-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS UPIN-KEY     WITH DUPLICATES
             ALTERNATE RECORD KEY IS UPIN-CITY WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT TAGDIAG ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TAG-KEY
           ALTERNATE RECORD KEY IS TAG-ICD9 WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CARRIERFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CAR-KEY
           ALTERNATE RECORD KEY IS CAR-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS CAR-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS CAR-STATE WITH DUPLICATES
           LOCK MODE IS MANUAL.
           SELECT GAPFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT TOWNADDR1 ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TOWN-KEY1
           LOCK MODE IS MANUAL.
           SELECT TOWNADDR2 ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TOWN-KEY2
           LOCK MODE IS MANUAL.
           SELECT TOWNADDR3 ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TOWN-KEY3
           LOCK MODE IS MANUAL.

           SELECT HIPCLAIMFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS HIP-KEY
           LOCK MODE IS MANUAL.

           SELECT WEBFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS WEB-KEY
           LOCK MODE IS MANUAL.

           SELECT CSCFILE ASSIGN TO "S85" ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY IS CSC-KEY
           LOCK MODE MANUAL.

           SELECT CSCCFILE ASSIGN TO "S90" ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY IS CSCC-KEY
           LOCK MODE MANUAL.

           SELECT CASCODEFILE ASSIGN TO "S95" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CAID-KEY
           LOCK MODE MANUAL.

           SELECT FILE1 ASSIGN TO "S300"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE2 ASSIGN TO "S350"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE3 ASSIGN TO "S400"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE4 ASSIGN TO "S450"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE5 ASSIGN TO "S500"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE6 ASSIGN TO "S550"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE7 ASSIGN TO "S600"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE8 ASSIGN TO "S650"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE9 ASSIGN TO "S700"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE10 ASSIGN TO "S750"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE11 ASSIGN TO "S800"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE12 ASSIGN TO "S850"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE13 ASSIGN TO "S900"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE14 ASSIGN TO "S950"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S999"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01  FILE101 PIC X(73).
       FD  FILE2.
       01  FILE201 PIC X(98).
       FD  FILE3.
       01  FILE301 PIC X(70).
       FD  FILE4.
       01  FILE401 PIC X(24).
       FD  FILE5.
       01  FILE501 PIC X(70).
       FD  FILE6.
       01  FILE601 PIC X(121).
       FD  FILE7.
       01  FILE701 PIC X(27).
       FD  FILE8.
       01  FILE801 PIC X(27).
       FD  FILE9.
       01  FILE901 PIC X(27).
       FD  FILE10.
       01  FILE1001 PIC X(10).
       FD  FILE11.
       01  FILE1101 PIC X(12).
       FD  FILE12.
       01  FILE1201 PIC X(41).
       FD  FILE13.
       01  FILE1301 PIC X(243).
       FD  FILE14.
       01  FILE1401 PIC X(273).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(130).
       FD  DIAGFILE.
       01  DIAGFILE01.
           02 DIAG-KEY.
              03 diag-9 PIC X(5).
              03 diag-10 pic xx.
           02 DIAG-TITLE.
             03 DIAG-T1 PIC XXXXX.
             03 DIAG-T2 PIC X(56).
           02 DIAG-MEDB PIC X(5).
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
       FD  UPINFILE.
       01  UPIN01.
           02 UPIN-PROVNUM.
             03 UPIN-PROVNUM1 PIC XX.
             03 UPIN-PROVNUM2 PIC X(4).
           02 UPIN-NAME PIC X(33).
           02 UPIN-KEY PIC X(6).
           02 UPIN-CITY PIC X(18).
           02 UPIN-STATE PIC XX.
           02 UPIN-ZIP PIC X(5).
       FD  TAGDIAG.
       01  TAGDIAG01.
           02 TAG-KEY.
              03 TAG-7 PIC X(7).
              03 TAG-5 PIC X(5).
           02 TAG-ICD9.
              03 tag-icd9-5 PIC X(5).
              03 tag-icd9-7 PIC X(7).


       FD  CARRIERFILE.
       01  CARRIERFILE01.
           02 CAR-NAME PIC X(45).
           02 CAR-CITY PIC X(20).
           02 CAR-STATE PIC XX.
           02 CAR-KEY PIC XXX.
 
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
       FD  TOWNADDR1.
       01  TOWNADDR101.
           02 TOWN-KEY1 PIC XX.
           02 TOWN-CITY1 PIC X(18).
           02 TOWN-STATE1 PIC XX.
           02 TOWN-ZIP1 PIC X(5).
       FD  TOWNADDR2.
       01  TOWNADDR201.
           02 TOWN-KEY2 PIC XX.
           02 TOWN-CITY2 PIC X(18).
           02 TOWN-STATE2 PIC XX.
           02 TOWN-ZIP2 PIC X(5).
       FD  TOWNADDR3.
       01  TOWNADDR301.
           02 TOWN-KEY3 PIC XX.
           02 TOWN-CITY3 PIC X(18).
           02 TOWN-STATE3 PIC XX.
           02 TOWN-ZIP3 PIC X(5).
       FD  HIPCLAIMFILE.
       01  HIPCLAIMFILE01.
           02 HIP-KEY PIC X.
           02 HIP-NUM PIC 9(9).
       FD  WEBFILE.
       01  WEBFILE01.
           02 WEB-KEY PIC X(8).
           02 WEB-NUM PIC 9(4).
       FD  CSCCFILE.
       01  CSCCFILE01.
           02 CSCC-KEY PIC XXX.
           02 CSCC-TITLE PIC X(240).
       FD  CSCFILE.
       01  CSCFILE01.
           02 CSC-KEY PIC XXX.
           02 CSC-TITLE PIC X(261).       
       FD  CASCODEFILE.
       01  CASCODEFILE01.
           02 CAID-KEY PIC XXX.
           02 CAID-REASON PIC X(70).       
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       PROCEDURE DIVISION.
       P0.
           OPEN output diagfile PROVCAID UPINFILE TAGDIAG WEBFILE
                        CARRIERFILE gapfile  townaddr1 fileout
                       townaddr2 townaddr3 hipclaimfile
                       cscfile csccfile cascodefile.
           close  diagfile PROVCAID UPINFILE TAGDIAG WEBFILE
                        CARRIERFILE gapfile  townaddr1
                       townaddr2 townaddr3 hipclaimfile
                       cscfile csccfile cascodefile.           
           OPEN I-O diagfile PROVCAID UPINFILE TAGDIAG
                        CARRIERFILE gapfile  townaddr1 WEBFILE
                       townaddr2 townaddr3 hipclaimfile
                       cscfile csccfile cascodefile.
           open input FILE1 FILE2 FILE3 FILE4 FILE5 FILE6
                      FILE7 FILE8 FILE9 FILE10 FILE11 FILE12 FILE13 
                      FILE14. 
       p1.
           read FILE1 at end go to p2
           end-read
           move FILE101 to diagfile01
           write diagfile01
           invalid
           MOVE SPACE TO FILEOUT01 
           write fileout01 from file101
           end-write.
           go to p1.
       p2.
           read FILE2 at end go to p3
           end-read
           move FILE201 to provcaid01
           write provcaid01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file201
           end-write.
           go to p2.
       p3.
           read FILE3 at end go to p4
           end-read
           move FILE301 to upin01
           write upin01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file301
           end-write.
           go to p3.
       p4.
           read FILE4 at end go to p5
           end-read
           move FILE401 to tagdiag01
           write tagdiag01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file401
           end-write.
           go to p4.
       p5.
           read FILE5 at end go to p6
           end-read
           move FILE501 to carrierfile01
           write carrierfile01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file501
           end-write.
           go to p5.
       p6.
           read FILE6 at end go to p7
           end-read
           move FILE601 to gapfile01
           write gapfile01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file601
           end-write.
           go to p6.
       p7.
           read FILE7 at end go to p8
           end-read
           move FILE701 to townaddr101
           write townaddr101
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file701
           end-write.
           go to p7.	
       p8.
           read FILE8 at end go to p9
           end-read
           move FILE801 to townaddr201
           write townaddr201
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file801
           end-write.
           go to p8.
       p9.
           read FILE9 at end go to p10
           end-read
           move FILE901 to townaddr301
           write townaddr301
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file901
           end-write.
           go to p9.
       p10.
           read FILE10 at end go to p11
           end-read
           move FILE1001 to HIPCLAIMFILE01
           write HIPCLAIMFILE01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file1001
           end-write.
           go to p10.
       p11.
           read FILE11 at end go to p12
           end-read
           move FILE1101 to WEBFILE01
           write WEBFILE01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file1101
           end-write.
           go to p11.
       p12.
           read FILE12 at end go to p13
           end-read
           move FILE1201 to CSCFILE01
           write CSCFILE01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file1201
           end-write.
           go to p12.
       p13.
           read FILE13 at end go to p14
           end-read
           move FILE1301 to CSCCFILE01
           write CSCCFILE01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file1301
           end-write.
           go to p13.
       p14.
           read FILE14 at end go to p99
           end-read
           move FILE1401 to CASCODEFILE01
           write CASCODEFILE01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file1401
           end-write.
           go to p14.

       p99.
           CLOSE DIAGFILE PROVCAID UPINFILE TAGDIAG
                 carrierfile gapfile townaddr1 WEBFILE
                  townaddr2 townaddr3 hipclaimfile
                  cscfile csccfile cascodefile.
           CLOSE FILE1 FILE2 FILE3 FILE4 FILE5 FILE6
                 FILE7 FILE8 FILE9 FILE10 FILE11 
                 FILE12 FILE13 FILE14 FILEOUT.
           STOP RUN.
