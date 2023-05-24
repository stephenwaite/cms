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

           SELECT INSFILE ASSIGN TO "S40" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
               ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.

       DATA DIVISION.

       FILE SECTION.
       
       FD  CHARNEW.
           copy charnew.cpy in "c:\users\sid\cms\copylib\rri".

       FD  INSFILE.
           copy insfile.cpy in "c:\users\sid\cms\copylib".    

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).   

       PROCEDURE DIVISION.

       P0. 
           OPEN INPUT INSFILE
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
           
           MOVE CD-PAYCODE TO INS-KEY.
           READ INSFILE
             INVALID 
               DISPLAY "BAD INS " CHARNEW01.

           IF  CD-PAYCODE = "160" OR INS-ACC-TYPE = "2"
               MOVE "2" TO CD-ACC-TYPE
               REWRITE CHARNEW01
               STRING "MVA INS " CD-NAME " " CD-DATE-T " " CD-PROC1
                 " ACC-TYPE " CD-ACC-TYPE
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           IF  CD-PAYCODE = "091" OR INS-ACC-TYPE = "1"
               MOVE "1" TO CD-ACC-TYPE
               REWRITE CHARNEW01
               STRING "W/C INS " CD-NAME " " CD-DATE-T " " CD-PROC1
                 " ACC-TYPE " CD-ACC-TYPE
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           

           GO TO P1.
       P2. 
           CLOSE CHARNEW FILEOUT INSFILE.
           STOP RUN.
