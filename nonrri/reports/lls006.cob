      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. lls006.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.   

           SELECT PAYCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.           
	  
    	     SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
	           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
	           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
	   
	         SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
	           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO.
 
           SELECT CAREFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS CARE-KEY
             LOCK MODE MANUAL.  

           SELECT OUTFILE ASSIGN TO "S60" ORGANIZATION LINE SEQUENTIAL.  
	  
       DATA DIVISION.

       FILE SECTION.

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".      
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib".             
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  CAREFILE.
           COPY CAREFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  OUTFILE.
       01  OUTFILE01 PIC X(120).      

       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT PAYCUR GARFILE CHARCUR CAREFILE.
           OPEN OUTPUT OUTFILE.

           MOVE LOW-VALUES TO PAYCUR-KEY

           START PAYCUR KEY NOT < PAYCUR-KEY
             invalid
               display "paycur non starter"
           END-START.      

       P0.          
           READ PAYCUR NEXT
             AT END
               GO TO P99.

           IF PC-PAYCODE NOT = "003"
             GO TO P0.

           IF NOT (PC-DENIAL = "  " OR "DD")
             GO TO P0.

           IF PC-AMOUNT = 0 AND PC-DENIAL = "  "
             GO TO P0.
               
           IF PC-CLAIM = "147977"
             DISPLAY "WHY DID WE GRAB THIS? pc-denial " pc-denial               
             IF PC-DENIAL = space
               DISPLAY "PC-DENIAL = SPACE"
             END-IF                
             ACCEPT OMITTED
           end-if  

           MOVE PC-KEY8 TO CC-KEY8
           MOVE "  " TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               DISPLAY "INVALID KEY, BAD CHARCUR START" CHARCUR-KEY
               ACCEPT OMITTED
               GO TO P0.

       P1.               
           READ CHARCUR NEXT
             AT END
               GO TO P0             
           END-READ

           IF CC-KEY8 NOT = PC-KEY8
             GO TO P0.

           IF CC-PLACE NOT = "T"
             GO TO P1.  

           IF CC-CLAIM NOT = PC-CLAIM
             GO TO P1.
             
           MOVE CC-KEY8 TO G-GARNO  
             
           READ GARFILE 
             INVALID KEY
               DISPLAY "INVALID KEY, BAD NAME CHANGE?" G-GARNO
               ACCEPT OMITTED  
               GO TO P0
           END-READ   

           PERFORM CARE-1 THRU CARE-2-EXIT

           GO TO P0.
      
       CARE-1.
           MOVE SPACE TO CARE-KEY
           MOVE CC-KEY8 TO CR-KEY8
           MOVE CC-DATE-T TO CR-DATE
           MOVE CC-PROC(1:5) TO CR-PROC
           MOVE SPACE TO CR-MOD1 CR-MOD2
      *     MOVE CC-PROC(6:2) TO CR-MOD1
      *     MOVE CC-MOD2 TO CR-MOD2
           
           START CAREFILE KEY NOT < CARE-KEY
             INVALID
               DISPLAY "BAD READ OF CAREFILE"
           END-START.      

       CARE-2.        
           READ CAREFILE next
             AT END 
               MOVE SPACE TO OUTFILE01
               STRING CHARCUR-KEY " " CC-PAYCODE 
                 " NO MATCH OF CHARGE IN CAREFILE " 
                 CARE-KEY " " delimited BY SIZE INTO OUTFILE01
               WRITE OUTFILE01
           end-read
  
           IF CR-KEY8 NOT = CC-KEY8
             MOVE SPACE TO OUTFILE01
             STRING CHARCUR-KEY " " "   "
               " NO MATCH OF CHARGE IN CAREFILE " 
               CARE-KEY " " delimited BY SIZE INTO OUTFILE01
             WRITE OUTFILE01
             GO TO CARE-2-EXIT           
           end-if

           IF CR-DATE NOT = CC-DATE-T
             GO TO CARE-2.

           IF CR-PROC NOT = CC-PROC(1:5)
             GO TO CARE-2.

           MOVE SPACE TO OUTFILE01

           STRING G-PRNAME ", " G-PRIPOL ", " CC-DATE-T ", " CR-ICN ", "
             CC-PROC DELIMITED BY SIZE INTO OUTFILE01 

           WRITE OUTFILE01.

       CARE-2-EXIT.
           EXIT.    
           
       P99.
           CLOSE OUTFILE GARFILE CHARCUR CAREFILE PAYCUR.
           STOP RUN.
