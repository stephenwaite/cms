      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wellcare-lat.
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
           
           SELECT FILEIN ASSIGN TO   "S45" ORGANIZATION LINE SEQUENTIAL.          

       DATA DIVISION.
       FILE SECTION.

       FD  CHARFILE.
           copy charfile.cpy in "c:\users\sid\cms\copylib\rri".
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(189). 

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  FILEIN.
       01  FILEIN01 PIC X(11).    
   
       WORKING-STORAGE SECTION.

       01  PROC-TAB01.
           02 CDM-TAB PIC X(4) OCCURS 90 TIMES.
           02 PROC-TAB PIC X(5) OCCURS 90 TIMES.
           02 MOD-TAB PIC X(2) OCCURS 90 TIMES.

       01  X PIC 999 VALUE 0.    
       01  Y PIC 999.
       
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT GARFILE CHARFILE FILEIN.
           OPEN OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.

       P00.
           READ FILEIN 
             AT END 
               GO TO P1.

           ADD 1 TO X

           MOVE FILEIN01(1:4) TO CDM-TAB(X).
           MOVE FILEIN01(5:5) TO PROC-TAB(X).
           MOVE FILEIN01(9:2) TO MOD-TAB(X).   

           display "cdm " cdm-tab(x) " proc " proc-tab(x) 
               " mod " mod-tab(x) " x " X
           ACCEPT OMITTED    

           GO TO P00.
       P1. 
           READ CHARFILE NEXT
             AT END
               GO TO P99
           END-READ

           PERFORM P2 VARYING Y FROM 1 BY 1 UNTIL Y > X.

           GO TO P1.
           

       P2.
           IF (CD-PROC0 = CDM-TAB(Y))
             AND NOT (CD-MOD2 = "LT" OR "RT" OR "50")
             MOVE SPACE TO FILEOUT01
             STRING "CHARGE MISSING LAT MOD FOR " CD-KEY8 " ON " 
               CD-DATE-T " FOR THE " CD-PROC0 " " CD-PROC1 
               " AND MODIFIER " CD-MOD2
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             MOVE X TO Y
           END-IF.  
           
       P99.
           CLOSE CHARFILE GARFILE FILEOUT FILEIN.
           STOP RUN.

