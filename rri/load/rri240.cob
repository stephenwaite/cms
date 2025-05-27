      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri240.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
             ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
             ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
             STATUS IS ACCT-STAT.
           
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           
           SELECT ORDFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS ORDNO
             ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES.
           
           SELECT ERRORFILE ASSIGN TO "S45" ORGANIZATION
             LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ORDFILE.
           copy "ordfile.cpy" in "c:\Users\sid\cms\copylib\rri".

       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS GARFILE01.
       01 GARFILE01.
           02 G-GARNO.
             03 ID1 PIC XXX.
             03 ID2 PIC XXX.
             03 ID3 PIC X.
             03 ID4 PIC X.
           02 G-GARNAME.
             03 G-GN1 PIC XXX.
             03 G-GN2 PIC X(21).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB PIC X(8).
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).
       FD ACTFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS ACTFILE01.
       01 ACTFILE01.
           02 A-ACTNO PIC X(8).
           02 A-GARNAME.
             03 A-GN1 PIC XXX.
             03 A-GN2 PIC X(21).
           02 A-BILLADD PIC X(22).
           02 A-STREET PIC X(22).
           02 A-CITY PIC X(18).
           02 A-STATE PIC X(2).
           02 A-ZIP.
             03 A-ZIP5 PIC X(5).
             03 A-ZIP4 PIC X(4).
           02 A-COLLT PIC X.
           02 A-PHONE.
             03 A-PHONE1 PIC XXX.
             03 A-PHONE2 PIC XXX.
             03 A-PHONE3 PIC X(4).
           02 A-SEX PIC X.
           02 A-RELATE PIC X.
           02 A-MSTAT PIC X.
           02 A-DOB PIC X(8).
           02 A-DUNNING PIC X.
           02 A-ACCTSTAT PIC X.
           02 A-PR-MPLR PIC X(4).
           02 A-PRINS PIC XXX.
           02 A-PR-ASSIGN PIC X.
           02 A-PR-OFFICE PIC X(4).
           02 A-PR-GROUP PIC X(10).
           02 A-PRIPOL PIC X(16).
           02 A-PRNAME PIC X(24).
           02 A-PR-RELATE PIC X.
           02 A-SE-MPLR PIC X(4).
           02 A-SEINS PIC XXX.
           02 A-SE-ASSIGN PIC X.
           02 A-TRINSIND PIC X.
           02 A-TRINS PIC XXX.
           02 A-SE-GROUP PIC X(10).
           02 A-SECPOL PIC X(16).
           02 A-SENAME PIC X(24).
           02 A-SE-RELATE PIC X.
           02 A-INSPEND PIC X(7).
           02 A-LASTBILL PIC X(8).
           02 A-ASSIGNM PIC X.
           02 A-PRIVATE PIC X.
           02 A-BILLCYCLE PIC X.
           02 A-DELETE PIC X.
           02 A-FILLER PIC XXX.
           02 A-GARNO PIC X(8).
           02 A-PRGRPNAME PIC X(15).
           02 A-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.
       FD  ERRORFILE.
       01  ERRORFILE01 PIC X(80).

       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(11).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
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
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).

       WORKING-STORAGE SECTION.
       01  GARTAB01.
           02 GARTAB PIC X(8) OCCURS 15 TIMES.
       01  X PIC 99.
       01  Y PIC 99.
       01  LINE-1.
           02 L1F1 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L1F2 PIC X(24).
           02 FILLER PIC X VALUE SPACE.
           02 L1F3 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L1F4 PIC X(24).
       01 FLAG PIC 9.
       01 LINE-2.
           02 FO-1 PIC X(24).
           02 FILLER PIC X VALUE SPACE.
           02 FO-2 PIC X(12).
           02 FILLER PIC X VALUE SPACE.
           02 FO-3 PIC X(12).
       01  LINE-3.
           02 FILLER PIC X(20) VALUE SPACE.
           02 L3F1 PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 L3F2 PIC X(16).
           02 FILLER PIC X VALUE SPACE.
           02 L3F3 PIC X.
           02 FILLER PIC X VALUE SPACE.
           02 L3F4 PIC X(24).
       01  HOLD8 PIC X(8).
       01  X-GARNO PIC X(8).
       01  X-ACTNO PIC X(8).
       01  ACCT-STAT PIC XX.
       01  CNTR PIC 99.
       01  GAR-TABLE.
           02 GAR-TAB PIC X(8) OCCURS 99 TIMES.
           02 DATE-TAB PIC X(8) OCCURS 99 TIMES.    
       01  DATE-X PIC X(8).   
       01  GAR-FLAG PIC X.  
       
       PROCEDURE DIVISION.

       P0.

           OPEN INPUT ACTFILE GARFILE ORDFILE CHARCUR
           OPEN OUTPUT ERRORFILE

           MOVE SPACE TO HOLD8
           
           MOVE SPACE TO ORDNO
           START ORDFILE KEY NOT < ORDNO
             INVALID
               GO TO P6.

       P1.

           READ ORDFILE NEXT
             AT END 
               GO TO P6
           END-READ   

           IF ORD8 = HOLD8
             GO TO P1
           END-IF    
           
           MOVE ORD8 TO HOLD8
           
           MOVE ORD8 TO A-ACTNO
           READ ACTFILE
             INVALID 
               MOVE SPACE TO ERRORFILE01
               STRING "THIS SHOULD NEVER HAPPEN, CALL STEVE"
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01  
               GO TO P1
           END-READ

           MOVE A-GARNO TO X-GARNO
           MOVE A-ACTNO TO X-ACTNO G-ACCT

           START GARFILE KEY NOT < G-ACCT
             INVALID
      *    no mrn in garfile, need to blank out A-GARNO
               IF A-GARNO NOT = SPACE
                 MOVE SPACE TO X-GARNO
                 PERFORM P5                                                          
               END-IF

               MOVE SPACE TO ERRORFILE01
               STRING "COULD NOT START GAR SEARCH WITH " A-GARNO
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01  
                              
               GO TO P1  
           END-START

           MOVE 0 TO GAR-FLAG
           MOVE 0 TO CNTR.

       P2.
           READ GARFILE NEXT
             AT END
      *        no good candidates
                 GO TO P3                
           END-READ   

           IF (G-ACCT NOT = X-ACTNO)
      *    no good candidates          
                GO TO P3            
           END-IF  

      *    set flag to zero for validate run, and 1 to gar-flag
      *    since was able to find at least 1 GARNO with that MRN
           MOVE 1 TO GAR-FLAG
           MOVE 0 TO FLAG      
           PERFORM A1 THRU A2 
           
           IF FLAG = 0
      *    we've got 1      
               ADD 1 TO CNTR
               MOVE G-GARNO TO GAR-TAB(CNTR)
           END-IF

           GO TO P2.

       A1.    
           IF (G-GN1 NOT = A-GN1)
               MOVE 1 TO FLAG
               MOVE SPACE TO ERRORFILE01
               STRING G-GARNO " " A-ACTNO " NAMES DON'T MATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF    

           IF (G-DUNNING NOT = "1" AND A-PRINS NOT = "003")
               MOVE 1 TO FLAG
               MOVE SPACE TO ERRORFILE01
               STRING G-GARNO " " A-ACTNO " IN COLLT" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF    
           
           IF A-DOB NOT = G-DOB
               MOVE 1 TO FLAG
               MOVE SPACE TO ERRORFILE01
               STRING G-GARNO " " A-ACTNO " DOB MISMATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF    
           
           IF (A-PRINS NOT = G-PRINS)
               IF A-PRINS = "225" AND G-PRINS = "079"
                   MOVE SPACE TO ERRORFILE01
                   STRING G-GARNO " " A-ACTNO " 225 USING 079 GARNO" 
                   DELIMITED BY SIZE INTO ERRORFILE01
               ELSE
                   MOVE 1 TO FLAG
                   MOVE SPACE TO ERRORFILE01
                   STRING G-GARNO " " A-ACTNO " NO INS MATCH" 
                       DELIMITED BY SIZE INTO ERRORFILE01
                   WRITE ERRORFILE01             
               END-IF    
               GO TO A2
           END-IF
      
      *    non matches of non-medicare 2ndary insurance     
           IF ((A-SEINS NOT = G-SEINS) AND 
               NOT (G-PRINS = "003" OR "200" OR "245"))
               MOVE 1 TO FLAG   
               MOVE SPACE TO ERRORFILE01
               STRING G-GARNO " " A-ACTNO " 2NDARY INS MISMATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF

      *    non matches of medigaps on assigned policies
           IF ((G-SE-ASSIGN NOT = "U") AND
      *    add special policy check for medicomp     
               ((A-SECPOL(1:3) NOT = G-SECPOL(1:3)) OR
               (A-SECPOL(5:8) NOT = G-SECPOL(5:8))))
               MOVE 1 TO FLAG   
               MOVE SPACE TO ERRORFILE01
               STRING G-GARNO " " A-ACTNO " 2NDARY INS MISMATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF.
            
       A2.
           EXIT.

       P3.
           IF CNTR = 0
             MOVE SPACE TO X-GARNO
             PERFORM P5
             MOVE SPACE TO ERRORFILE01
             
             IF GAR-FLAG = 1
               STRING "MRN " A-ACTNO " END OF GARFILE SEARCH"
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
             ELSE
               STRING "MRN " A-ACTNO " IS NEW"
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01
             END-IF                
             
             GO TO P1             
           END-IF  

           IF CNTR = 1
             MOVE SPACE TO ERRORFILE01
             STRING "MRN " A-ACTNO " ONLY 1 IN GARFILE AND IS A MATCH"
               DELIMITED BY SIZE INTO ERRORFILE01
             WRITE ERRORFILE01 

             IF A-GARNO = SPACE
               PERFORM P5
             END-IF              

             GO TO P1
           END-IF
      
      *    more than 1, let's find the 1 with most recent charges
           PERFORM P4 THRU P4-EXIT
             VARYING X FROM 1 BY 1 UNTIL X > CNTR

           MOVE ZEROES TO DATE-X
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > CNTR     
             IF DATE-TAB(X) > DATE-X
               MOVE DATE-TAB(X) TO DATE-X
               MOVE GAR-TAB(X) TO X-GARNO
             END-IF
           END-PERFORM

           IF X-GARNO NOT = A-GARNO
             PERFORM P5
           ELSE
             MOVE SPACE TO ERRORFILE01
             STRING "MRN " A-ACTNO " AND " A-GARNO " WAS FINE"
               DELIMITED BY SIZE INTO ERRORFILE01
             WRITE ERRORFILE01               
           END-IF  

           GO TO P1.  

       P4.
           MOVE GAR-TAB(X) TO CC-KEY8           
           MOVE 999 TO CC-KEY3
           START CHARCUR KEY NOT > CHARCUR-KEY
             INVALID
               MOVE ZEROES TO DATE-TAB(X)
               GO TO P4-EXIT.

       P4-1.
           READ CHARCUR PREVIOUS
             AT END 
               GO TO P4-EXIT. 

           IF CC-KEY8 NOT = GAR-TAB(X)
             GO TO P4-EXIT
           END-IF

           MOVE CC-DATE-T TO DATE-TAB(X).               

       P4-EXIT.
           EXIT.                  

       P5.    
           CLOSE ACTFILE
           OPEN I-O ACTFILE
           READ ACTFILE WITH LOCK INVALID
             DISPLAY "ACTFILE LOCKED STATUS IS " ACCT-STAT
           END-READ

      *     DISPLAY "A-ACTNO IS " A-ACTNO " A-GARNO IS " A-GARNO
      *       " X-GARNO IS " X-GARNO
      *     ACCEPT OMITTED

           MOVE SPACE TO ERRORFILE01
           STRING "MRN " A-ACTNO " UPDATED FROM " A-GARNO " TO " X-GARNO
             DELIMITED BY SIZE INTO ERRORFILE01
           WRITE ERRORFILE01

           MOVE X-GARNO TO A-GARNO     
           REWRITE ACTFILE01
             INVALID
               DISPLAY "UH OH, CALL STEVE".                  
           
           CLOSE ACTFILE
           OPEN INPUT ACTFILE.


       P6. 
           CLOSE GARFILE ACTFILE ORDFILE ERRORFILE CHARCUR.
           STOP RUN.
