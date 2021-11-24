      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkforemails.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO       "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
               ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
               ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES.

           SELECT FILEIN ASSIGN TO        "S35" ORGANIZATION 
               LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO       "S40" ORGANIZATION 
               LINE SEQUENTIAL.

           SELECT EMAILAUTHFILE ASSIGN TO "S45" ORGANIZATION INDEXED
               ACCESS MODE DYNAMIC RECORD KEY IS EA-KEY
               ALTERNATE RECORD KEY IS EA-MEDREC WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-EMAIL WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-AUTH WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-DATE-E WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-SSN WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT ERRFILE ASSIGN TO       "S50" ORGANIZATION
               LINE SEQUENTIAL.
            
       DATA DIVISION.
       
       FILE SECTION.

       FD  ACTFILE.
           copy actfile.cpy in "c:\users\sid\cms\copylib\rri".             

       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC XX.
           02 FI-2 PIC X(1068).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  EMAILAUTHFILE.
           copy emailauthfile.cpy in "c:\users\sid\cms\copylib\rri".             

       FD  ERRFILE.
       01  ERRFILE01 PIC X(80).
       
       WORKING-STORAGE SECTION.

           copy rec101.cpy in "c:\users\sid\cms\copylib\rri\ws".
           copy rec201.cpy in "c:\users\sid\cms\copylib\rri\ws".             
           copy rec301.cpy in "c:\users\sid\cms\copylib\rri\ws".             

       01  X-MEDREC.
              02 X-MEDREC0 PIC XX VALUE "00".
              02 X-MEDREC1 PIC XX.
              02 X-MEDREC2 PIC XX.
              02 X-MEDREC3 PIC XX.

       01  AUTH-FLAG PIC X.
       01  EMAIL-ADDR PIC X(30).
       01  EMAIL-DOMAIN PIC X(30).
       01  CNTR PIC 999.   
       01  LAST-EMAIL PIC X(30).

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT ACTFILE EMAILAUTHFILE FILEIN.
           OPEN OUTPUT FILEOUT ERRFILE.

       P1.
           READ FILEIN AT END
             GO TO 9100CMF
           END-READ.  

       P1-1.
           IF FI-1 NOT = "##"
               DISPLAY "THIS RECORD SHOULD BE A ## " FILEIN01
               ACCEPT OMITTED
               GO TO 9100CMF
           END-IF

           MOVE FILEIN01 TO REC101

           READ FILEIN
             AT END
               DISPLAY "BAD ENDING"
               ACCEPT OMITTED
               GO TO 9100CMF
           END-READ

           IF FI-1 = "##" OR "$$"
               DISPLAY "BAD ++ RECORD " FILEIN01
               ACCEPT OMITTED
               GO TO 9100CMF
           END-IF

           MOVE FILEIN01 TO REC201

           MOVE R2-MEDREC1 TO X-MEDREC1
           MOVE R2-MEDREC2 TO X-MEDREC2
           MOVE R2-MEDREC3 TO X-MEDREC3
           MOVE X-MEDREC TO R2-MEDREC
           
           IF R2-MEDREC = "00000000"
               DISPLAY "MRN IS ZEROES FOR " R1-PATNAME
               ACCEPT OMITTED
               GO TO B1
           END-IF

           MOVE SPACE TO EMAIL-ADDR EMAIL-DOMAIN
           UNSTRING R1-EMAIL DELIMITED BY "@" INTO
             EMAIL-ADDR EMAIL-DOMAIN

           IF EMAIL-DOMAIN = SPACE
             IF EMAIL-ADDR NOT = SPACE
               MOVE SPACE TO ERRFILE01
               STRING "BAD EMAIL               " R2-MEDREC " " R1-EMAIL
                 DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01 
             END-IF
             MOVE SPACE TO ERRFILE01
             STRING   "BAD EMAIL               " R2-MEDREC R1-EMAIL
               DELIMITED BY SIZE INTO ERRFILE01
             WRITE ERRFILE01              
             GO TO B1
           END-IF               


           MOVE SPACE TO FILEOUT01
           STRING "EMAIL OF " R1-EMAIL " IN FILE     FOR " R2-MEDREC
              DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01              

           MOVE R2-MEDREC TO A-ACTNO
           MOVE R2-MEDREC TO EA-MEDREC           
           
           READ ACTFILE
             INVALID
               DISPLAY "NOT A VALID ACTNO " A-ACTNO
               ACCEPT OMITTED
               GO TO B1             
           END-READ

           START EMAILAUTHFILE KEY NOT < EA-MEDREC
             INVALID         
               DISPLAY "BAD START FOR " EA-MEDREC
               DISPLAY "BUT WE HAVE EMAIL OF " R1-EMAIL
               ACCEPT OMITTED
               GO TO B1
           END-START

           MOVE 0 TO CNTR
           MOVE SPACE TO LAST-EMAIL.

       P1-2.
           READ EMAILAUTHFILE NEXT
             AT END
               DISPLAY "END OF EMAILAUTHFILE "
               ACCEPT OMITTED
               GO TO B1
           END-READ    

           IF EA-MEDREC NOT = R2-MEDREC
             IF CNTR = 0
               MOVE SPACE TO ERRFILE01
               STRING "EMAIL " R1-EMAIL " NOT IN DB FOR " R2-MEDREC
                 DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01 
             END-IF  
             GO TO B1
           END-IF

           ADD 1 TO CNTR           

           IF EA-EMAIL NOT = LAST-EMAIL
             MOVE SPACE TO FILEOUT01
             STRING "EMAIL OF " EA-EMAIL " IN DATABASE FOR " EA-MEDREC
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
           END-IF  

           MOVE EA-EMAIL TO LAST-EMAIL

           GO TO P1-2.

       B1. 
           READ FILEIN
             AT END
               GO TO 9100CMF
           END-READ

           IF FI-1 NOT = "$$"
               GO TO P1-1
           END-IF     
           
           GO TO B1.
      
       9100CMF.
           CLOSE ACTFILE EMAILAUTHFILE FILEOUT ERRFILE FILEIN.           
           STOP RUN.
