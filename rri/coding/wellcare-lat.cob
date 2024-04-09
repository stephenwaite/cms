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

       DATA DIVISION.
       FILE SECTION.

       FD  CHARFILE.
           copy charfile.cpy in "c:\users\sid\cms\copylib\rri".
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(189). 

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".
   

       WORKING-STORAGE SECTION.

       
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT GARFILE CHARFILE.
           OPEN OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.

       P1. 
           READ CHARFILE NEXT
             AT END
               GO TO P99
           END-READ

      *    IF THE INSURANCE ISN'T WELCARE READ THE NEXT RECORD
      *    AKA GO TO P1

      *     IF CD-PAYCODE NOT = "270" GO TO P1.

      *    IF THE PROCEDURE ISN'T CDM 7216...
           IF NOT (CD-PROC0 = "1072" OR "2013" OR "7156" OR "8456")
             GO TO P1.

      *    IF THE 1ST MODIFIER IS EMPTY FOR SURG PROCS
      *    CD PROC1 HAS THE CPT AND MOD1 SO START IN 6TH POSITION
      *    IF YOU NEED TO CHECK THAT BUT WE'LL LOOK AT MOD2
           IF CD-MOD2 NOT = SPACE     
             GO TO P1.
          
       P2. 
      *    WE SHOULD BE ABLE TO LOG WARNINGS FOR WELLCARE CDM 7216
      *    THAT ARE MISSING MODS HERE, WE'RE ONLY CHECKING 1ST MOD

           MOVE SPACE TO FILEOUT01
           STRING "CHARGE MISSING LAT MOD FOR " CD-KEY8 " ON " 
             CD-DATE-T " FOR THE " CD-PROC1 " AND MODIFIER " CD-MOD2
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01.

           GO TO P1.

       P99.
           CLOSE CHARFILE GARFILE FILEOUT. 
           STOP RUN.

