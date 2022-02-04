      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pas002.
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

       FD  CHARFILE
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC. 
               03 CD-PROC0 PIC X(4).
               03 CD-PROC1 PIC X(5).
               03 CD-PROC3 PIC XX.
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AUTH PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-ESPDT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(189). 

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".  

       WORKING-STORAGE SECTION.
       01  HOLDIT PIC X(8).
       01  ANS PIC X.
      *
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARFILE GARFILE
           OPEN OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.

       P1. 
           READ CHARFILE NEXT
             AT END
               GO TO P99
           END-READ

           IF CD-PAYCODE NOT = "002" GO TO P1.

           IF CD-PLACE NOT = "5" GO TO P1.

           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID               
             GO TO P1
           END-READ

      *    The State of Vermont Total Choice Plan (prefix FVT)
      *    doesn't require prior approval
           IF G-PRIPOL(1:3) = "FVT" GO TO P1.

      *    02 pa guide attachment III
      *    ct colonography
           IF CD-PROC1 = "74261" OR "74262" OR "74263"
             GO TO P2.

      *    02 CT Scans       
           IF CD-PROC1 = "70450" OR "70460" OR "70470" OR "70480" OR
             "70481" OR "70482" OR "70486" OR "70487" OR "70488" OR 
             "70490" OR "70491" OR "70492" OR "71250" OR "71260" OR 
             "71270" OR "71271" OR "72125" OR "72126" OR "72127" OR 
             "72128" OR "72129" OR "72130" OR "72131" OR "72132" OR 
             "72133" OR "72192" OR "72193" OR "72194" OR "73200" OR
             "73201" OR "73202" OR "73700" OR "73701" OR "73702" OR 
             "74150" OR "74160" OR "74170" OR "74176" OR "74177" OR
             "74178" OR "75571" OR "75572" OR "75573" OR "77078"
             GO TO P2.  

      *    Magnetic Resonance Imaging (MRI)     
           IF CD-PROC1 = "70336" OR "70540" OR "70542" OR "70543" OR
             "70551" OR "70552" OR "70553" OR "70554" OR "70555" OR
             "71550" OR "71551" OR "71552" OR "72141" OR "72142" OR 
             "72146" OR "72147" OR "72148" OR "72149" OR "72156" OR
             "72157" OR "72158" OR "72195" OR "72196" OR "72197" OR
             "73218" OR "73219" OR "73220" OR "73221" OR "73222" OR
             "73223" OR "73718" OR "73719" OR "73720" OR "73721" OR
             "73722" OR "73723" OR "74181" OR "74182" OR "74183" OR
             "74712" OR "74713" OR "75557" OR "75559" OR "75561" OR
             "75563" OR "75565" OR "76390" OR "76391" OR "77046" OR
             "77047" OR "77048" OR "77049" OR "77084" 
             GO TO P2.
             
      *    Positron Emission Tomography (PET) Scans
           IF CD-PROC1 = "78459" OR "78491" OR "78429" OR "78430" OR
             "78431" OR "78432" OR "78433" OR "78434" OR "78492" OR 
             "78608" OR "78609" OR "78811" OR "78812" OR "78813" OR 
             "78814" OR "78815" OR "78816" 
             GO TO P2.  
               
      *    Single-Photon Emission Computed Tomography
           IF CD-PROC1 = "78803" OR "78830" OR "78831" OR "78832"
             GO TO P2.

      *    Attachment V, BCBSVT members
      *    Cardiac blood pool imaging
           IF CD-PROC1 = "78472" OR "78473" OR "78481" OR "78483" OR
             "78494"
             GO TO P3.

      *    CTA Scans not in attachment III
           IF CD-PROC1 = "70496" OR "70498" OR "71275" OR "72191" OR
             "73206" OR "73706" OR "74174" OR "74175" OR "75574" OR
             "75635"
             GO TO P3.  

      *    MRA Scans not in attachment III
           IF CD-PROC1 = "70544" OR "70545" OR "70546" OR "70547" OR
             "70548" OR "70549" OR "71555" OR "72159" OR "72198" OR
             "73225" OR "73725" OR "74185"
             GO TO P3.

           GO TO P1.

       P2. 
           MOVE SPACE TO FILEOUT01
           STRING "SOV employer or BCBSVT member " CD-PROC1 " " 
             CD-DATE-T " " CD-KEY8 " needs PA" 
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01 
           GO TO P1.

       P3. 
           MOVE SPACE TO FILEOUT01
           STRING "BCBSVT member only            " CD-PROC1 " " 
             CD-DATE-T " " CD-KEY8 " needs PA" 
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01 
           GO TO P1.
    

       P99.
           CLOSE CHARFILE FILEOUT. 
           STOP RUN.
