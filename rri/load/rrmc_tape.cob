      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrmc_tape.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           
           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.              

       FD  FILEOUT.
       01  FILEOUT01 PIC X(1070).
       
       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC XX.
           02 FI-2 PIC X(1068).
             
       
       WORKING-STORAGE SECTION.

       01  REC101.
           02 R1-1 PIC XX.
           02 R1-PATNUM PIC X(7).
           02 R1-PATNAME.
             03 R1-PATNAME-L PIC X(19).
             03 R1-PATNAME-F PIC X(12).
           02 R1-PATADDR1 PIC X(25).
           02 R1-PATADDR2 PIC X(25).
           02 R1-PATCITY PIC X(25).
           02 R1-PATSTATE PIC XX.
           02 R1-PATZIP PIC X(10).
           02 R1-ADMIT. 
             03 R1-ADMITMM PIC XX.
             03 FILLER PIC X.
             03 R1-ADMITDD PIC XX.
             03 FILLER PIC X.
             03 R1-ADMITYY PIC XXXX.
           02 R1-ADMITTIME PIC X(5).
           02 R1-WORKCOMP PIC X.
           02 R1-GARNAME.
             03 R1-GARNAME1.
               05 R1-GARNAME1-L PIC X(21).
               05 R1-GARNAME1-F PIC X(14).
           02 R1-GARADDR1 PIC X(25).
           02 R1-GARADDR2 PIC X(25).
           02 R1-GARCITY PIC X(25).
           02 R1-GARSTATE PIC XX.
           02 R1-GARZIP PIC X(10).
           02 R1-EMAIL PIC X(30).
           02 R1-IP1 PIC X(5).
           02 R1-ID1 PIC X(30).
           02 R1-CERT11 PIC X(20).
           02 R1-GRP1 PIC X(20).
           02 R1-GRPNAME11 PIC X(30).
           02 R1-SUBNAME11 PIC X(35).
           02 R1-EMPLOYNAME11 PIC X(30).
           02 R1-GENDER11 PIC X.
           02 FILLER PIC X.
           02 R1-DOB11 PIC X(10).
           02 R1-SSN11 PIC X(9).
           02 R1-RELATE1 PIC XX.
           02 INSURANCE-1.
             03 R1-INSNAME1 PIC X(25).
             03 R1-INSCONTACT1 PIC X(25).
             03 R1-INSADDR11 PIC X(20).
             03 R1-INSADDR21 PIC X(15).
             03 R1-INSCITY1 PIC X(20).
             03 R1-INSSTATE1 PIC XX.
             03 R1-INSZIP1 PIC X(10).
             03 R1-INSPHONE1 PIC X(12).
           02 R1-AUTH PIC X(20).
           02 R1-IP2 PIC X(5).
           02 R1-ID2 PIC X(30).
           02 R1-CERT22 PIC X(20).
           02 R1-GRP2 PIC X(20).
           02 R1-GRPNAME22 PIC X(30).
           02 R1-SUBNAME22 PIC X(35).
           02 R1-EMPLOYNAME22 PIC X(30).
           02 R1-GENDER22 PIC X.
           02 FILLER PIC X.
           02 R1-DOB22 PIC X(10).
           02 R1-SSN22 PIC X(9).
           02 R1-RELATE2 PIC XX.

           02 INSURANCE-2.
             03 R1-INSNAME2 PIC X(25).
             03 R1-INSCONTACT2 PIC X(25).
             03 R1-INSADDR12 PIC X(20).
             03 R1-INSADDR22 PIC X(15).
             03 R1-INSCITY2 PIC X(20).
             03 R1-INSSTATE2 PIC XX.
             03 R1-INSZIP2 PIC X(10).
             03 R1-INSPHONE2 PIC X(12).

       01  REC201.
           02 R2-1 PIC XX.
           02 R2-ACC.
             03 R2-ACCMM PIC XX.
             03 R2-ACCDD PIC XX.
             03 R2-ACCYY PIC XX.
             03 R2-ACCHHMM PIC X(5).
           02 R2-REFDOC.
             03 R2-REFDOC4 PIC XXXX.
             03 R2-REFDOC22 PIC X(18).
           02 R2-DIAG PIC X(130).
           02 R2-IP3 PIC X(5).
           02 R2-ID3 PIC X(30).
           02 R2-CERT33 PIC X(20).
           02 R2-GRP3 PIC X(20).
           02 R2-GRPNAME33 PIC X(30).
           02 R2-SUBNAME33 PIC X(35).
           02 R2-EMPLOYNAME33 PIC X(30).
           02 R2-GENDER33 PIC X.
           02 FILLER PIC X.
           02 R2-DOB33 PIC X(10).
           02 R2-SSN33 PIC X(9).
           02 R2-RELATE3 PIC XX.
           02 INSURANCE-3.
             03  R2-INSNAME3 PIC X(25).
             03  R2-INSCONTACT3 PIC X(25).
             03  R2-INSADDR13 PIC X(20).
             03  R2-INSADDR23 PIC X(15).
             03  R2-INSCITY3 PIC X(20).
             03  R2-INSSTATE3 PIC XX.
             03  R2-INSZIP3 PIC X(10).
             03  R2-INSPHONE3 PIC X(12).
           02 FILLER PIC X(20).
           02 R2-IP4 PIC X(5).
           02 R2-ID4 PIC X(30).
           02 R2-CERT44 PIC X(20).
           02 R2-GRP4 PIC X(20).
           02 R2-GRPNAME44 PIC X(30).
           02 R2-SUBNAME44 PIC X(35).
           02 R2-EMPLOYNAME44 PIC X(30).
           02 R2-GENDER44 PIC X.
           02 FILLER PIC X.
           02 R2-DOB44 PIC X(10).
           02 R2-SSN44 PIC X(9).
           02 R2-RELATE4 PIC XX.
           02 INSURANCE-4.
             03  R2-INSNAME4 PIC X(25).
             03  R2-INSCONTACT4 PIC X(25).
             03  R2-INSADDR14 PIC X(20).
             03  R2-INSADDR24 PIC X(15).
             03  R2-INSCITY4 PIC X(20).
             03  R2-INSSTATE4 PIC XX.
             03  R2-INSZIP4 PIC X(10).
             03  R2-INSPHONE4 PIC X(12).
           02 FILLER PIC X(20).
           02 R2-MEDREC.
             03 R2-MEDREC1 PIC XX.
             03 FILLER PIC X.
             03 R2-MEDREC2 PIC XX.
             03 FILLER PIC X.
             03 R2-MEDREC3 PIC XX.
           02 R2-EMPNAME PIC X(30).
           02 R2-EMPLOYER-ADDR1 PIC X(25).
           02 R2-EMPLOYER-ADDR2 PIC X(25).
           02 R2-EMPLOYER-CITY PIC X(25).
           02 R2-EMPLOYER-STATE PIC XX.
           02 R2-EMPLOYER-ZIP PIC X(10).
           02 R2-GUARSSN PIC X(9).
           02 R2-PHONE.
             03 R2-PHONE1 PIC XXX.
             03 FILLER PIC X.
             03 R2-PHONE2 PIC XXX.
             03 FILLER PIC X.
             03 R2-PHONE3 PIC X(4).
           02 R2-DOC PIC X(8).
           02 R2-REFIND PIC X.
           02 R2-NPI PIC X(10).
           02 R2-ADMIT-DATE-DUPLICATE PIC X(6).
           02 R2-PATSEX PIC X.
           02 R2-PATDOB.
             03 R2-DOBMM PIC XX.
             03 R2-DOBDD PIC XX.
             03 R2-DOBYY PIC XX.
           02 R2-DISCHARGE PIC X(6).
    
       01  REC301.
           02 R3-1 PIC XX.
           02 R3-IND PIC XXX.
           02 R3-DEPT PIC XX.
           02 R3-GLC PIC X.
           02 R3-PROC.
             03 R3-PROC1 PIC X.
             03 FILLER PIC XXX.
           02 R3-DATE. 
              03 R3-DATEMM PIC XX.
              03 R3-DATEDD PIC XX.
              03 R3-DATEYY PIC XX.
           02 R3-UNIT PIC XXX.
      * col 22     
           02 R3-CLINICAL PIC X(40).
           02 FILLER PIC X(24).
      * col 86     
           02 R3-PLACE PIC X(4).
           02 R3-DOCP PIC X(4).
           02 R3-DOCPFILLER PIC X(18).
      * col 112     
           02 R3-CPT PIC X(5).
           02 FILLER PIC X(3).
      * col 120     
           02 R3-HCPCS PIC X(5).
           02 FILLER PIC X(3).
      * col 128     
           02 R3-MOD1 PIC X(2).
           02 FILLER PIC X.
      * col 131     
           02 R3-MOD2 PIC X(2).
           02 FILLER PIC X.
      * col 134
           02 R3-MOD3 PIC X(2).
           02 FILLER PIC X(5).
      * col 141     
           02 R3-OBSERV PIC X(5).
           02 FILLER PIC X(35).
           02 R3-LOCO PIC X(4).

       01  HOLDNAME PIC X(15).    

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN.
           OPEN OUTPUT FILEOUT.

       P1.
           READ FILEIN
             AT END
               GO TO P99
           END-READ

           IF FI-1 = "##"
               MOVE FILEIN01(10:15) TO HOLDNAME
           END-IF    

           IF FI-1 = "##"
               MOVE FILEIN01 TO REC101
           END-IF

           IF FI-1 = "++"
               MOVE FILEIN01 TO REC201              
           END-IF

           IF FI-1 NOT = "$$"
               WRITE FILEOUT01 FROM FILEIN01
               GO TO P1
           END-IF

           WRITE FILEOUT01 FROM FILEIN01
           GO TO P1.               
           
       P99.
           CLOSE FILEIN FILEOUT.
           STOP RUN.
