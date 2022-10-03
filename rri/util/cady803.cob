      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cady803.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.          

           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.      
       
           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

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
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  garfile.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".                  

       FD  FILEOUT.
       01  FILEOUT01. 
           02 FO-1 PIC X(160).
           02 FO-2 PIC X(5).

       FD  INSFILE.
           COPY insfile.cpy IN "C:\Users\sid\cms\copylib".

       working-storage section.

       01  garno pic x(8).
       01  dos   pic x(8).
       01  ans   pic x.    
       01  CHARCUR-BACK PIC X(160).

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT INSFILE garfile
           OPEN INPUT CHARCUR
           OPEN OUTPUT FILEOUT.

       P00.
           DISPLAY "ENTER THE GARNO".
           ACCEPT GARNO.

           IF GARNO = SPACE OR "END" OR "X" 
             GO TO P99.

           DISPLAY GARNO.    

       P000.
           DISPLAY "ENTER DATE OF CHARGE, YYYYMMDD, "
             "OR BK FOR DIFF GARNO"
           ACCEPT DOS.

           IF DOS = "BK" 
             GO TO P00.

           MOVE GARNO TO G-GARNO
           READ GARFILE 
             INVALID 
               DISPLAY "BAD" 
               GO TO P00.

           DISPLAY G-GARNO " " G-GARNAME " " G-PRINS "/" G-SEINS
             " " G-DOB.           
              
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           MOVE DOS TO CC-DATE-T.           

       P01.    
           START CHARCUR KEY IS NOT LESS THAN OR EQUAL TO
             CHARCUR-KEY
             invalid
               DISPLAY "BAD CHARCUR START"
               GO TO P00.

       P1.
           READ CHARCUR NEXT
             AT END
               GO TO P2
           END-READ           

           IF CC-KEY8 NOT = G-GARNO
             GO TO P99.

           IF CC-DATE-T NOT = DOS
             GO TO P1.                      

           DISPLAY CC-PROC(5:5) " " CC-MOD2 " " CC-MOD3 " " CC-PAYCODE
             " " CC-AMOUNT " " CC-DATE-T.
  
           DISPLAY "Y OR y FOR YES OR ANY KEY FOR NO."

           accept ANS

           IF NOT (ANS = "Y" OR "y")
             GO TO P1
           END-IF.

       P2. 
           DISPLAY CC-PAYCODE " CC-PAYCODE"

           IF NOT (CC-PAYCODE = G-PRINS OR G-SEINS OR G-TRINS)
             DISPLAY "WARNING, CHARGE NOT CODED WITH AN INS ON GARNO"
             DISPLAY "CHANGE TO WHICH INS " G-PRINS " " G-SEINS " "
               G-TRINS
             DISPLAY "ENTER 3 DIGIT INS CODE TO USE"   
             ACCEPT CC-PAYCODE
             GO TO P2
           END-IF

           MOVE CC-PAYCODE TO INS-KEY  

           READ INSFILE 
             INVALID 
               DISPLAY INS-KEY " NOT A DEFINED INSURANCE, try again" 
               GO TO P2.
               
           DISPLAY "ACCT " INS-ASSIGN "  CLM " INS-NEIC-ASSIGN
             " CLAIM-TYPE  " INS-CLAIMTYPE  "    " INS-NAME
           DISPLAY "ASSIGNMENT ATTRIBUTES ARE NOW CHANGED AS ABOVE"
           
           MOVE INS-ASSIGN TO CC-ASSIGN
           MOVE INS-NEIC-ASSIGN TO CC-NEIC-ASSIGN
           MOVE INS-CLAIMTYPE TO CC-PAPER
           MOVE INS-KEY TO CC-PAYCODE 
           MOVE CHARCUR01 TO CHARCUR-BACK
           CLOSE CHARCUR
           OPEN I-O CHARCUR
           MOVE CHARCUR-BACK TO CHARCUR01
           PERFORM RE-WRITE-CC THRU RE-WRITE-CC-EXIT
           ACCEPT OMITTED
           
           MOVE CHARCUR-BACK(1:11) TO CHARCUR-KEY
           
           MOVE CHARCUR-BACK TO FO-1
           MOVE INS-NEIC TO FO-2

           WRITE FILEOUT01

           GO TO P01.

       RE-WRITE-CC.
           REWRITE CHARCUR01 INVALID
                DISPLAY "RECORD NOT MODIFIED AT THIS TIME"
                CLOSE CHARCUR
                OPEN INPUT CHARCUR
                GO TO RE-WRITE-CC-EXIT
           END-REWRITE

           CLOSE CHARCUR
           OPEN INPUT CHARCUR.
           DISPLAY "RECORD CHANGED".

       RE-WRITE-CC-EXIT.
           EXIT.     

       P99.
           CLOSE CHARCUR garfile FILEOUT INSFILE.
           
           STOP RUN.
