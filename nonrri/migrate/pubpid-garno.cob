      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pubpid-garno.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT ERRFILE ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
             LOCK MODE MANUAL.           

           SELECT ERRFILE2 ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERRFILE.
       01  ERRFILE01 PIC X(90).

       FD  ERRFILE2.
       01  ERRFILE201 PIC X(90).
      
       Fd  garfile.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib".      
      
       FD  FILEIN.
       01  F01 PIC X(90).

       FD  FILEOUT.
       01  FILEOUT01 pic x(120).

       WORKING-STORAGE SECTION.
       01  LNAME PIC X(10).
       01  FNAME PIC X(5).
       01  TESTDOB PIC X(8).
       01  F-DATEHLD PIC X(10).
       01  LNAMEHLD PIC X(10).
       01  FNAMEHLD PIC X(5).
       01  F-DATE PIC X(10).
       01  F-DOBHLD PIC X(10).
       01  F-SEXHLD PIC X.
       01  F-MRHLD PIC X(5).
       01  F-DOB PIC X(10).
       01  F-SEX PIC X.
       01  F-MR PIC X(5).
       01  ERRFILEHOLD-01 PIC X(90).
       01  W-INS-NAME PIC X(22).

       PROCEDURE DIVISION.

       P0.
           OPEN OUTPUT FILEOUT ERRFILE ERRFILE2.
           OPEN INPUT FILEIN GARFILE insFILE.                  
            
       P1.
           READ FILEIN 
             AT END 
               PERFORM P2 THRU P2-EXIT
               GO TO P99
           END-READ.  

           MOVE SPACE TO F-DATE LNAME FNAME F-DOB F-SEX F-MR.

           UNSTRING F01 DELIMITED BY "," INTO
             F-DATE LNAME FNAME F-DOB F-SEX F-MR

      *     DISPLAY "LNAME " LNAME " FNAME " FNAME " DOB " F-DOB
      *       " SEX " F-SEX " MR " F-MR
      *     ACCEPT omitted    
                      
           PERFORM P2 THRU P2-EXIT

           GO TO P1.

       P2.
           MOVE SPACE TO G-GARNO       
           MOVE LNAME(1:3) TO G-GARNO(1:3)

           START GARFILE KEY NOT < G-GARNO
             INVALID 
               PERFORM E1
               GO TO P2-EXIT
           END-START. 
           
       P3.
           READ GARFILE NEXT 
             AT END 
               PERFORM E1 
               GO TO P2-EXIT
           END-READ.

      *     DISPLAY G-GARNO.
      *     ACCEPT OMITTED.

           IF G-GARNO(1:3) > LNAME(1:3)
               PERFORM E1
               GO TO P2-EXIT
           END-IF

           MOVE SPACE TO TESTDOB
           
           STRING F-DOB(1:4) F-DOB(6:2) F-DOB(9:2) DELIMITED
               BY SIZE INTO TESTDOB
               
           MOVE SPACE TO LNAMEHLD FNAMEHLD

           UNSTRING G-GARNAME DELIMITED BY ";" INTO LNAMEHLD FNAMEHLD
           
           IF (LNAMEHLD(1:3) = LNAME(1:3))
             AND (FNAMEHLD(1:3) = FNAME(1:3))
      *       AND (F-SEX(1:1) = G-SEX)
             AND (TESTDOB = G-DOB)
             PERFORM P-INS THRU P-INS-EXIT.
             
             MOVE SPACE TO FILEOUT01
             STRING G-GARNO " " LNAMEHLD " " FNAMEHLD
               " " G-GARNAME " " G-SEX " " W-INSNAME
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             GO TO P2-EXIT
           END-IF 

           IF (LNAMEHLD(1:3) = LNAME(1:3))
              AND
               (FNAMEHLD(1:3) = FNAME(1:3))
              AND
              (TESTDOB = G-DOB)
              PERFORM E2
              GO TO P2-EXIT
           END-IF         

           GO TO P3. 

       P2-EXIT.
           EXIT.

       P-INS.           
           MOVE SPACE TO W-INS-NAME.

           IF G-PRINS = "001" MOVE "SELF-PAY" TO W-INS-NAME
             GO TO P-INS-EXIT.

           IF G-PRINS = "002" MOVE "COMMERCIAL" TO W-INS-NAME
             GO TO P-INS-EXIT.
          
           IF G-PRINS = "003" MOVE "MEDICARE" TO W-INSNAME
             GO TO P-INS-EXIT.

           IF G-PRINS = "004" MOVE "MEDICAID" TO W-INS-NAME
             GO TO P-INS-EXIT.

           IF G-PRINS = "006" OR "079" OR "225" MOVE "FEDERAL" 
             TO W-INS-NAME  GO TO P-INS-EXIT.

           IF G-DOB < "19550101" 
             MOVE "MEDICARE" TO W-INS-NAME
           ELSE 
             MOVE "COMMERCIAL" TO W-INS-NAME.             

       P-INS-EXIT.
           EXIT.

       E1.
      *    DISPLAY "G-GARNO " G-GARNO " IS GREATER THAN LNAMEHLD "
      *         LNAMEHLD " " FNAMEHLD " " F-DOBHLD " " F-SEXHLD " "
      *         F-MRHLD
      *         ACCEPT OMITTED
           MOVE F01 TO ERRFILE01
           WRITE ERRFILE01.   

       E2.
           MOVE F01 TO ERRFILE201
           WRITE ERRFILE201.    

        P99.
           WRITE FILEOUT01
           CLOSE FILEOUT ERRFILE GARFILE ERRFILE2
           STOP RUN
            
