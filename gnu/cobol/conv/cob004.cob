        IDENTIFICATION DIVISION.
       PROGRAM-ID. cob004.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MPLRFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY
           LOCK MODE IS MANUAL.

           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.

           SELECT HISFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS HISFILE-KEY
           LOCK MODE MANUAL.

           SELECT BATCHFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS BATCH-KEY
           ALTERNATE RECORD KEY IS BA-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS BA-DATE-A WITH DUPLICATES
           ALTERNATE RECORD KEY IS BA-STAT WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT DOCFILENEW  ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DOC-KEY.

           SELECT CAREFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CARE-KEY
           LOCK MODE MANUAL.

           SELECT FILE1 ASSIGN TO "S300" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE3 ASSIGN TO "S400" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE4 ASSIGN TO "S450" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE5 ASSIGN TO "S500" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE6 ASSIGN TO "S550" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE7 ASSIGN TO "S600" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD  FILE1.
       01  FILE101 PIC X(156).

       FD  FILE3.
       01  FILE301 PIC X(50).

       FD  FILE4.
       01  FILE401 PIC X(158).

       FD  FILE5.
       01  FILE501 PIC X(43).

       FD  FILE6.
       01  FILE601 PIC X(176).

       FD  FILE7.
       01  FILE701 PIC X(137).

       FD  MPLRFILE.
       01  MPLRFILE01.
           02 MPLR-KEY PIC X(8). 
           02 MPLR-NAME PIC X(22).
           02 MPLR-STREET PIC X(24).
           02 MPLR-CITY PIC X(15).
           02 MPLR-STATE PIC XX.
           02 MPLR-ZIP PIC X(9).
           02 MPLR-CLAIMNO PIC X(15).
           02 MPLR-TRINS PIC XXX.
           02 MPLR-TR-ASSIGN PIC X.
           02 MPLR-TR-GROUP PIC X(10).
           02 MPLR-TRIPOL PIC X(16).
           02 MPLR-TR-NAME PIC X(24).
           02 MPLR-TR-RELATE PIC X.
           02 MPLR-FUTURE PIC X(6).

       FD  PAYCUR.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).

       FD  HISFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS HISFILE01.
       01  HISFILE01.
           02 HISFILE-KEY.
             03 HI-KEY8 PIC X(8).
             03 HI-CLAIM PIC X(6).
             03 HI-REC-TYPE PIC X.
             03 HI-KEY4 PIC XXXX.
           02 HI-PATID PIC X(8).
           02 HI-SERVICE PIC X.
           02 HI-DIAG PIC X(5).
           02 HI-PROC PIC X(11).
           02 HI-MOD2 PIC XX.
           02 HI-MOD3 PIC XX.
           02 HI-MOD4 PIC XX.
           02 HI-AMOUNTX PIC X(6).
           02 HI-DOCR PIC X(3).
           02 HI-DOCP PIC X(2).
           02 HI-PAYCODE PIC XXX.
           02 HI-STUD PIC X.
           02 HI-WORK PIC XX.
           02 HI-DAT1 PIC X(8).
           02 HI-RESULT PIC X.
           02 HI-ACT PIC X.
           02 HI-SORCREF PIC X.
           02 HI-COLLT PIC X.
           02 HI-AGE PIC X.
           02 HI-PAPER PIC X.
           02 HI-PLACE PIC X.
           02 HI-EPSDT PIC X.
           02 HI-DATE-T.
             03 HI-DATE-TCC PIC XX.
             03 HI-DATE-TYY PIC XX.
             03 HI-DATE-TMM PIC XX.
             03 HI-DATE-TDD PIC XX.
           02 HI-DATE-A PIC X(8).
           02 HI-DATE-E PIC X(8).
           02 HI-REC-STAT PIC X.
           02 HI-DX2 PIC X(5).
           02 HI-DX3 PIC X(5).
           02 HI-AHI-TYPE PIC X.
           02 HI-DATE-M PIC X(8).
           02 HI-ASSIGN PIC X.
           02 HI-NEIC-ASSIGN PIC X.
           02 HI-FUTURE PIC X(6).
       FD  BATCHFILE.
       01  BATCHFILE01.
           02 BATCH-KEY PIC X(6).
           02 BA-NAME PIC X(10).
           02 BA-DATE-A PIC X(8).
           02 BA-DATE-C PIC X(8).
           02 BA-AMT PIC S9(8)V99.
           02 BA-STAT  PIC X.
       FD  DOCFILENEW.
       01  DOCFILENEW01.
           02 DOC-KEY.
             03 DOC-INS PIC XXX.
             03 DOC-NUM PIC XX.
           02 DOC-FEDID PIC X(14).
           02 DOC-PVNUM PIC X(14).
           02 DOC-UPIN PIC X(6).
           02 DOC-NPI PIC X(10).
           02 DOC-IND PIC X.
           02 DOC-GROUP PIC X(14).
           02 DOC-NPIGROUP PIC X(10).
           02 DOC-NAME PIC X(24).
           02 DOC-GROUPNAME PIC X(29).
           02 DOC-SSNUM PIC X(9).
           02 DOC-TAXONOMY PIC X(10).
           02 DOC-NEIC PIC X(5).
           02 DOC-TAXGROUP PIC X(10).
           02 DOC-WEBTAX PIC XXX.
           02 DOC-FUTURE PIC X(12).

       FD  CAREFILE.
       01  CAREFILE01.
           02 CARE-KEY.
              03 CR-KEY8 PIC X(8).
              03 CR-DATE PIC X(8).
              03 CR-PROC PIC X(5).
              03 CR-MOD1 PIC XX.  
              03 CR-MOD2 PIC XX.  
           02 CR-PAYDATE PIC X(8).
           02 CR-DOCP    PIC X(6).
           02 CR-POS     PIC XX.  
           02 CR-BILLED PIC  9(4)V99.
           02 CR-ALLOWED PIC 9(4)V99.
           02 CR-DEDUCT  PIC 9(4)V99.
           02 CR-PAYED   PIC 9(4)V99.
           02 CR-DENIAL1 PIC X(4).   
           02 CR-DENIAL2 PIC X(4).   
           02 CR-DENIAL3 PIC X(4).   
           02 CR-DENIAL4 PIC X(4).   
           02 CR-PAYDENIAL PIC X(4). 
           02 CR-ICN PIC X(13).
           02 CR-CK-EFT PIC X(9).
           02 CR-INSNAME PIC X(30).
 
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       PROCEDURE DIVISION.
       P0.
           OPEN output
             mplrfile  paycur hisfile batchfile docfilenew carefile.
           CLOSE
             mplrfile  paycur hisfile batchfile docfilenew carefile.
           OPEN output
             mplrfile  paycur hisfile batchfile docfilenew carefile.

           open input
           file1  file3 file4 file5 file6 file7.
       p1.
           read file1 at end go to p3.
           move file101 to mplrfile01
           write mplrfile01
           go to p1.



       p3.
           read file3 at end go to p4.
           move file301 to paycur01
           write paycur01
           go to p3.
       p4.
           read file4 at end go to p5.
           move file401 to hisfile01
           write hisfile01
           go to p4.
       p5.
           read file5 at end go to p6.
           move file501 to batchfile01
           write batchfile01
           invalid continue
           end-write
           go to p5.
       p6.
           read file6 at end go to p7.
           move file601 to docfilenew01
           write docfilenew01
           go to p6.
       p7.
           read file7 at end go to p99.
           move file701 to carefile01
           write carefile01
           go to p7.

       p99.
           close
             mplrfile  paycur hisfile batchfile docfilenew carefile.
           close
             file1 file3 file4 file5 file6 file7.
           STOP RUN.
