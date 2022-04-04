      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pas225.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO  "S35" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO  "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT EMAILAUTHFILE ASSIGN TO "S45"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC RECORD KEY IS EA-KEY
               ALTERNATE RECORD KEY IS EA-MEDREC WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-EMAIL WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-AUTH WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-DATE-E WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-SSN WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT AUTHFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS AUTH-KEY
               LOCK MODE MANUAL
               STATUS IS AUTHFILE-STAT.               

       DATA DIVISION.
       FILE SECTION.

       FD  CHARFILE.
           copy charfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD  FILEOUT.
       01  FILEOUT01 PIC X(189). 

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  EMAILAUTHFILE.
           copy emailauthfile.cpy in "c:\users\sid\cms\copylib\rri". 

       FD  AUTHFILE.
           copy authfile.cpy in "c:\users\sid\cms\copylib\rri".      

       WORKING-STORAGE SECTION.
       01  AUTHFILE-STAT PIC XX.
       01  AUTHFILE-BACK PIC X(91).
      *
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT GARFILE EMAILAUTHFILE AUTHFILE.
           OPEN I-O CHARFILE.
           OPEN OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.

       P1. 
           READ CHARFILE NEXT
             AT END
               GO TO P99
           END-READ

           IF CD-PAYCODE NOT = "225" GO TO P1.

           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID
             PERFORM P3         
             GO TO P1
           END-READ

           move g-acct to ea-medrec
           start emailauthfile key not > ea-medrec
             invalid
               PERFORM P3
               GO TO P1
             not invalid
               perform emailauth-1 thru emailauth-exit
           end-start

           GO TO P1.

       emailauth-1.           
           read emailauthfile previous
             at end
               PERFORM P3
               go to emailauth-exit.    

           if ea-medrec not = g-acct
             PERFORM P3
             go to emailauth-exit.

      *     display EMAILAUTHFILE01

           IF EA-DATE-E = CD-DATE-T
               if ea-auth(1:2) NOT = "VA"
                  PERFORM P4
                  go to emailauth-1
               END-IF    
               move ea-auth TO AUTH-NUM
               MOVE EA-DATE-E TO AUTH-DATE-E
               MOVE AUTHFILE01 TO AUTHFILE-BACK
      *         PERFORM WRITE-AU THRU WRITE-AU-EXIT 
               MOVE 1 TO CD-AUTH
      *         REWRITE CHARFILE01
               PERFORM P2             
               go to emailauth-exit
           END-IF    

           go to emailauth-1.

       emailauth-exit.
           exit.

       WRITE-AU.
           CLOSE AUTHFILE
           OPEN I-O AUTHFILE
           MOVE AUTHFILE-BACK TO AUTHFILE01
           WRITE AUTHFILE01 INVALID
                DISPLAY "RECORD NOT ADDED AT THIS TIME"
                DISPLAY AUTHFILE-STAT
                CLOSE AUTHFILE
                OPEN INPUT AUTHFILE
                GO TO WRITE-AU-EXIT
           END-WRITE
           
           DISPLAY "RECORD ADDED".
           CLOSE AUTHFILE
           OPEN INPUT AUTHFILE.

       WRITE-AU-EXIT.
           EXIT.             

       P2. 
           MOVE SPACE TO FILEOUT01
           STRING "VACCN AUTH " AUTH-NUM " ADDED FOR " CD-KEY8 " ON " 
             CD-DATE-T " FOR THE " CD-PROC1
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01.

       P3. 
           MOVE SPACE TO FILEOUT01
           STRING "NO VACCN AUTH FOR " CD-KEY8 " ON " 
             CD-DATE-T " FOR THE " CD-PROC1
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01.

       P4. 
           MOVE SPACE TO FILEOUT01
           STRING "DATE MATCHES BUT NOT A GOOD AUTH, " AUTH-NUM 
             ", FOR " CD-KEY8 " ON " CD-DATE-T " FOR THE " CD-PROC1
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01.    

       P99.
           CLOSE CHARFILE GARFILE EMAILAUTHFILE AUTHFILE FILEOUT. 
           STOP RUN.
