      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. one803.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT AGEDATE ASSIGN TO "S25"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO  "S35" ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION LINE SEQUENTIAL.

           SELECT INSFILE ASSIGN TO "S45" ORGANIZATION INDEXED
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
       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
      * 60     
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
      * 104     
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
      * 150     
           02 CC-FUTURE PIC X(6).
       
       FD  INSFILE.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).

       FD  FILEIN.
       01  FILEIN01. 
           02 FI-1 PIC X(7).      

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(156).
           02 FO-2 PIC X(5).

       FD  AGEDATE.
       01  AGEDATE01.
           02 DATE-LOW PIC X(8).
           02 DATE-HIGH PIC X(8).           

       WORKING-STORAGE SECTION. 

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT AGEDATE CHARCUR FILEIN INSFILE
           OPEN OUTPUT FILEOUT.

           READ AGEDATE.

       P1. 
           MOVE SPACE TO FILEIN01
           
           READ FILEIN 
             AT END
               GO TO P9
           END-READ.           

       P2.               
           READ CHARCUR NEXT             
             AT END  
               GO TO P1
           END-READ

           IF CC-DATE-T < DATE-LOW OR > DATE-HIGH
              GO TO P2.

           IF CC-PROC NOT = FI-1
             GO TO P2.   

           MOVE CC-PAYCODE TO INS-KEY

           READ INSFILE
             INVALID
               DISPLAY "PAYCODE NOT IN INSFILE? " CC-PAYCODE " "
                   CHARCUR01 
               ACCEPT OMITTED
               CONTINUE
           END-READ

           MOVE CHARCUR01 TO FO-1
           MOVE INS-NEIC TO FO-2.

           WRITE FILEOUT01

           GO TO P2.
           
       P9.
           CLOSE AGEDATE CHARCUR FILEOUT FILEIN INSFILE
           STOP RUN.
