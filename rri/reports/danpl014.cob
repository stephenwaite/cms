      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. danpl014.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARDATE ASSIGN TO "S25"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT PAYDATE ASSIGN TO "S30"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CPTIN ASSIGN TO "S45"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT DOCFILE ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           
           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.

       DATA DIVISION.

       FILE SECTION.

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib\rri".      
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".             
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".      
      
       FD  DOCFILE.
           COPY DOCFILE.CPY IN "C:\Users\sid\cms\copylib\rri".  

       FD  CHARDATE.
       01  CHARDATE01. 
           02 LOW-CHARDATE PIC X(8).
           02 HIGH-CHARDATE PIC X(8).               
       
       FD  PAYDATE.
       01  PAYDATE01. 
           02 LOW-PAYDATE PIC X(8).
           02 HIGH-PAYDATE PIC X(8).

       FD  CPTIN.
       01  CPTIN01 PIC X(5).
       
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-DOCP PIC XX.
           02 FO-DUM PIC X.
           02 FO-SERVICE PIC X.
           02 FO-PROC PIC X(11).
           02 FO-AMOUNT PIC S9(7)V99.
           02 FILLER PIC X VALUE " ".
           02 FO-IO PIC X.
           02 FO-NAME PIC X(24).
           02 FO-DATE PIC X(8).
           02 FO-CKEY PIC X(12).
           02 FILLER PIC X VALUE " ".
           02 FO-INS PIC X(3).

       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).

       01  MON-TAB-RE01.
           02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
           02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
           02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
           02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       
       01  MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
     
       01  PLINDX PIC 99 VALUE 0.
       01  LOW-CLAIM PIC X(6).
       01  HIGH-CLAIM PIC X(6).
       01  X PIC 99.
       01  Y PIC 99.
       01  CC-PL PIC X.
       01  FLAG PIC 9.
       01  TOT-AMOUNT PIC S9(7)V99.

      *     COPY charback.CPY IN "C:\Users\sid\cms\copylib\rri".      
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT DOCFILE GARFILE CHARDATE PAYDATE CHARCUR PAYCUR
             CPTIN.
           OPEN OUTPUT FILEOUT.
           READ CPTIN.
           READ CHARDATE.
           READ PAYDATE.
           READ DOCFILE AT END GO TO P00.

       P00. 
           READ DOCFILE AT END GO TO P1.
           ADD 1 TO PLINDX.
      *    not interested in docfile at this time
      *    would have to be configured to support docfile isam file     
      *     MOVE DF1 TO PL-TAB(PLINDX)
      *     MOVE DF2 TO PL-NUM(PLINDX)
      *     MOVE DF3 TO PL-NAME(PLINDX).
           GO TO P00.               

       P1. 
           READ CHARCUR
             AT END
               GO TO P99.

           IF CC-PROC(5:5) NOT = CPTIN01 
             GO TO P1.                      
           
           IF CC-DATE-T < LOW-CHARDATE OR > HIGH-CHARDATE
             GO TO P1.           

      *     perform DF-SEARCH thru DF-SEARCH-EXIT

      *     DISPLAY "hit THORACENTESIS "
      *       CC-PROC(1:4)
      *     accept OMITTED         

           MOVE 0 TO TOT-AMOUNT.

           MOVE CC-KEY8 TO PC-KEY8  
           MOVE SPACE TO PC-KEY3
           START PAYCUR KEY NOT < PAYCUR-KEY
             invalid
               display "NON START OF PAYCUR"
               go to P99  
           end-start.      

       P2.                 
           READ PAYCUR NEXT
             AT END
              GO TO P1.

           IF PC-KEY8 NOT = CC-KEY8
             PERFORM WRITE-FO
             GO TO P1. 

           IF PC-CLAIM NOT = CC-CLAIM
             GO TO P2.
           
           IF PC-DATE-T < LOW-PAYDATE OR > HIGH-PAYDATE
             GO TO P2.      

           IF (PC-PAYCODE = "007" OR "008" OR "009" OR "011" 
             OR "012" OR "013" OR "014" OR "015" OR "016" OR "017" ) 
             OR (PC-DENIAL = "DI" OR "15" OR "14")
             GO TO P2
           END-IF    

           COMPUTE PC-AMOUNT = PC-AMOUNT * -1
           COMPUTE TOT-AMOUNT = TOT-AMOUNT + PC-AMOUNT
      *     MOVE PC-DATE-T TO FO-DATE
          
           GO TO P2.

       DF-SEARCH. 
           MOVE "2" TO FO-IO.
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.

       DF-SEARCH2. 
           IF CC-PLACE = PL-TAB(Y) AND PL-NUM(Y) = "3"
             MOVE "1" TO FO-IO.
           
           IF CC-PLACE = PL-TAB(Y)
             MOVE PLINDX TO Y.

       DF-SEARCH-EXIT.    
           EXIT.

       WRITE-FO. 
           MOVE CC-PATID TO G-GARNO
           READ GARFILE INVALID MOVE SPACE TO G-GARNAME
           MOVE G-GARNAME TO FO-NAME.
           MOVE G-PRINS TO FO-INS.           

           MOVE "1" TO FO-SERVICE.
           MOVE "1" TO FO-DUM.
           MOVE "1" TO FO-IO.
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PROC TO FO-PROC
           MOVE CC-DATE-T TO FO-DATE

           MOVE TOT-AMOUNT TO FO-AMOUNT
           
           MOVE G-GARNAME TO FO-NAME
           STRING " " CHARCUR-KEY INTO FO-CKEY
           WRITE FILEOUT01.

       P99. 
           CLOSE DOCFILE GARFILE CHARDATE PAYDATE CHARCUR
             PAYCUR CPTIN FILEOUT.
           STOP RUN.
