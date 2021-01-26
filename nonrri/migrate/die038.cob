      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. die038.
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
       01  FILEOUT01 pic x(90).

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
       PROCEDURE DIVISION.

       P0.
           OPEN OUTPUT FILEOUT ERRFILE ERRFILE2.
           OPEN INPUT FILEIN GARFILE.
           
           READ FILEIN AT END GO TO P99.

       P0-1.    
           MOVE SPACE TO F-DATEHLD
           MOVE SPACE TO LNAMEHLD
           MOVE SPACE TO FNAMEHLD 
           MOVE SPACE TO F-DOBHLD 
           MOVE SPACE TO F-SEXHLD
           MOVE SPACE TO F-MRHLD.

           UNSTRING F01 DELIMITED BY "," INTO
             F-DATEHLD LNAMEHLD FNAMEHLD F-DOBHLD F-SEXHLD F-MRHLD
           
           MOVE F01 TO ERRFILE01.
            
       P1.
           READ FILEIN 
             AT END 
               PERFORM P2 THRU P2-EXIT
               GO TO P99
           END-READ.  

           MOVE SPACE TO F-DATE LNAME FNAME F-DOB F-SEX F-MR.
           UNSTRING F01 DELIMITED BY "," INTO
             F-DATE LNAME FNAME F-DOB F-SEX F-MR
           
           IF F-MR = F-MRHLD GO TO P1.
           
           PERFORM P2 THRU P2-EXIT

           GO TO P0-1.

       P2.
           MOVE SPACE TO G-GARNO       
           MOVE LNAMEHLD(1:3) TO G-GARNO(1:3)
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
           IF G-GARNO(1:3) > LNAMEHLD(1:3)
               PERFORM E1
               GO TO P2-EXIT
           END-IF

           MOVE SPACE TO TESTDOB
           
           STRING F-DOBHLD(1:4) F-DOBHLD(6:2) F-DOBHLD(9:2) DELIMITED
               BY SIZE INTO TESTDOB
           MOVE SPACE TO LNAME FNAME

           UNSTRING G-GARNAME DELIMITED BY ";" INTO LNAME FNAME
           
           IF (LNAMEHLD(1:3) = LNAME(1:3))
             AND (FNAMEHLD(1:3) = FNAME(1:3))
             AND (F-SEXHLD(1:1) = G-SEX)
             AND (TESTDOB = G-DOB)
             MOVE SPACE TO FILEOUT01
             STRING  F-MRHLD(1:5) G-GARNO " " LNAMEHLD " " FNAMEHLD
               " " G-GARNAME DELIMITED BY SIZE INTO FILEOUT01
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
       E1.
      *    DISPLAY "G-GARNO " G-GARNO " IS GREATER THAN LNAMEHLD "
      *         LNAMEHLD " " FNAMEHLD " " F-DOBHLD " " F-SEXHLD " "
      *         F-MRHLD
      *         ACCEPT OMITTED
           WRITE ERRFILE01.   

       E2.
           MOVE ERRFILE01 TO ERRFILE201
           WRITE ERRFILE201.    

        P99.
           WRITE FILEOUT01
           CLOSE FILEOUT ERRFILE GARFILE ERRFILE2
           STOP RUN
            
