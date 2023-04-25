      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri-acc-type.
       AUTHOR. s WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARNEW ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARNEW-KEY.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       
       FD  CHARNEW.
           copy charnew.cpy in "c:\users\sid\cms\copylib\rri".

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).   

       PROCEDURE DIVISION.

       P0. 
           OPEN I-O    CHARNEW
           OPEN OUTPUT FILEOUT
           MOVE SPACE TO CHARNEW-KEY
           START CHARNEW KEY NOT < CHARNEW-KEY
             INVALID
               DISPLAY "EMPTY FILE"
               GO TO P2
           END-START.    

       P1. 
           READ CHARNEW NEXT
             AT END
               GO TO P2
           END-READ    
           
           IF  CD-PAYCODE = "160"
               MOVE "2" TO CD-ACC-TYPE
               REWRITE CHARNEW01
               STRING "MVA INS " CD-NAME " " CD-DATE-T " " CD-PROC1
                 " ACC-TYPE " CD-ACC-TYPE
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           IF ((CD-PROC2(1:5) = "77065") AND 
               (CD-PROC1 = "1091" OR "1092" OR "1441"))
               MOVE CD-MOD2 TO CD-MOD3
               MOVE "GG" TO CD-MOD2
               REWRITE CHARNEW01
               STRING "GG mod for same day screen with diag "
                       CD-NAME " " CD-DATE-T " " CD-PROC1
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01        
           END-IF

           GO TO P1.
       P2. 
           CLOSE CHARNEW FILEOUT.
           STOP RUN.
