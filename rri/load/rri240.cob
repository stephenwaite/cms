      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RRI240.
       AUTHOR. SID WAITE.
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

       DATA DIVISION.
       FILE SECTION.
       FD ORDFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS ORDFILE01.
       01 ORDFILE01.
           02 ORDNO.
             03 ORD8 PIC X(8).
             03 ORD3 PIC XXX.
           02 C-PROC PIC X(4).
           02 C-IND PIC X.
           02 C-REF PIC XXX.
           02 C-IOPAT PIC X.
           02 C-DATE-A PIC X(8).
           02 C-DATE-T PIC X(8).
           02 C-DATE-ADMIT PIC X(8).
           02 C-ORDER PIC XXXX.
           02 C-CLINICAL PIC X(40).
           02 C-ADMIT-DIAG PIC X(30).
           02 C-DATE-E PIC X(8).
           02 C-CPT PIC X(5).
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
       
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT ACTFILE GARFILE ORDFILE
           OPEN OUTPUT ERRORFILE
           MOVE SPACE TO HOLD8
           MOVE SPACE TO ORDNO
           START ORDFILE KEY NOT < ORDNO INVALID GO TO P6.
       P1.
           READ ORDFILE NEXT AT END 
               GO TO P6
           END-READ    
           IF ORD8 = HOLD8
               GO TO P1
           END-IF    
           MOVE ORD8 TO HOLD8
           MOVE ORD8 TO A-ACTNO
           READ ACTFILE INVALID 
               DISPLAY "THIS SHOULD NEVER HAPPEN, CALL STEVE"
               DISPLAY "BAD READ ON ACTFILE " ORD8
               ACCEPT X 
               GO TO P1
           END-READ           
           MOVE A-GARNO TO X-GARNO.
           MOVE A-ACTNO TO X-ACTNO.          

           MOVE A-GARNO TO G-GARNO
           READ GARFILE INVALID
               GO TO P1-1
           END-READ

           MOVE 0 TO FLAG

      *    validate current garno on actfile     
           PERFORM A1 THRU A2

           IF FLAG = 0
               MOVE SPACE TO ERRORFILE01
               STRING "FOR MRN " A-ACTNO " NO CHANGE TO " G-GARNO
               " A-GARNO WAS " A-GARNO
               DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01  
               GO TO P1
           END-IF.    

      *    fall into p1-1 searching garfile for a candidate    

       P1-1.
           MOVE A-ACTNO TO G-ACCT
           START GARFILE KEY NOT < G-ACCT
             INVALID
               MOVE SPACE TO X-GARNO
      *    no matches in garfile
               MOVE SPACE TO ERRORFILE01
               STRING "MRN " A-ACTNO " HAS NO GARNOS IN GARFILE"
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO P1
           END-START.  

       P2.
           READ GARFILE NEXT
             AT END
               MOVE SPACE TO X-GARNO
      *        no good candidates
               MOVE SPACE TO ERRORFILE01
               STRING "MRN " A-ACTNO " GARNOS DO NOT HAVE USABLE DEMOS"
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO P1
           END-READ   

           IF (G-ACCT NOT = A-ACTNO)
             MOVE SPACE TO X-GARNO
      *    no good candidates          
             MOVE SPACE TO ERRORFILE01
             STRING "MRN " A-ACTNO " GARNOS DO NOT HAVE USABLE DEMOS"
               DELIMITED BY SIZE INTO ERRORFILE01
             WRITE ERRORFILE01             
             GO TO P1
           END-IF    
      *    reset X-GARNO 
           MOVE G-GARNO TO X-GARNO 

      *    set flag to zero again for another validate run
           MOVE 0 TO FLAG      
           PERFORM A1 THRU A2 
           
           IF FLAG = 0
      *    we need to update actfile
             GO TO P4
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
               STRING G-GARNO " " A-ACTNO " DOB MISMATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF    
           
           IF (A-PRINS NOT = G-PRINS) 
               MOVE 1 TO FLAG
               STRING G-GARNO " " A-ACTNO " NO INS MATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF
           
           IF (A-SEINS NOT = G-SEINS) AND 
             (G-SEINS NOT = "062" OR G-SE-ASSIGN NOT = "U")
               MOVE 1 TO FLAG   
               STRING G-GARNO " " A-ACTNO " 2NDARY INS MISMATCH" 
                 DELIMITED BY SIZE INTO ERRORFILE01
               WRITE ERRORFILE01             
               GO TO A2
           END-IF.

       A2.
           EXIT.

       P4.
           MOVE SPACE TO ERRORFILE01
           STRING "FOR MRN " A-ACTNO " CHANGED " A-GARNO " TO " X-GARNO
             DELIMITED BY SIZE INTO ERRORFILE01
           WRITE ERRORFILE01  
           CLOSE ACTFILE
           OPEN I-O ACTFILE
           READ ACTFILE WITH LOCK INVALID
               DISPLAY "ACTFILE LOCKED STATUS IS " ACCT-STAT
           END-READ
      *     DISPLAY "A-ACTNO IS " A-ACTNO " A-GARNO IS " A-GARNO
      *     ACCEPT OMITTED
           MOVE X-GARNO TO A-GARNO     
           REWRITE ACTFILE01
           CLOSE ACTFILE
           OPEN INPUT ACTFILE
           GO TO P1.

       P6. 
           CLOSE GARFILE ACTFILE ORDFILE ERRORFILE.
           STOP RUN.
