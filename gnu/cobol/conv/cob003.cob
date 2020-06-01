       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob003.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S20" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S25" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT PARMNDEX ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PARM-KEY
           LOCK MODE MANUAL.
           SELECT CLAIMFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CLAIM-KEY
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT CMNTFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CMNT-KEY
           LOCK MODE MANUAL.
           SELECT NEWAUTHFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS NEWAUTH-KEY
           LOCK MODE MANUAL.


           SELECT FILE1 ASSIGN TO "S200" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE2 ASSIGN TO "S250" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE3 ASSIGN TO "S300" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE4 ASSIGN TO "S350" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE5 ASSIGN TO "S400" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE6 ASSIGN TO "S450" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE7 ASSIGN TO "S500" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE8 ASSIGN TO "S550" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE9 ASSIGN TO "S600" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE10 ASSIGN TO "S650" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE11 ASSIGN TO "S700" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01  FILE101 PIC X(185).

       FD  FILE2.
       01  FILE201 PIC X(80).

       FD  FILE3.
       01  FILE301 PIC X(75).

       FD  FILE4.
       01  FILE401 PIC X(7).

       FD  FILE5.
       01  FILE501 PIC X(51).

       FD  FILE6.
       01  FILE601 PIC X(277).

       FD  FILE7.
       01  FILE701 PIC X(120).

       FD  FILE8.
       01  FILE801 PIC X(61).

       FD  FILE9.
       01  FILE901 PIC X(59).

       FD  FILE10.
       01  FILE1001 PIC X(99).

       FD  FILE11.
       01  FILE1101 PIC X(80).



       FD  CHARFILE.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC PIC X(7).
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
       FD  PAYFILE.
       01  PAYFILE01.
           02 PAYFILE-KEY.
             03 PD-KEY8 PIC X(8).
             03 PD-KEY3 PIC XXX.
           02 PD-NAME PIC X(24).
           02 PD-AMOUNT PIC S9(4)V99.
           02 PD-PAYCODE PIC XXX.
           02 PD-DENIAL PIC XX.
           02 PD-CLAIM PIC X(6).
           02 PD-DATE-T PIC X(8).
           02 PD-DATE-E PIC X(8).
           02 PD-ORDER PIC X(6).
           02 PD-BATCH PIC X(6).
       FD  PARMNDEX.
       01  PARMNDEX01.
           02 PARM-KEY.
             03 PM-KEY8 PIC X(8).
             03 PM-KEY3 PIC XXX.
           02 PM-DATA PIC X(64).
       FD  CLAIMFILE.
       01  CLAIMFILE01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).
       FD PATFILE.
       01 PATFILE01.
           02 P-PATNO PIC X(8).
           02 P-GARNO PIC X(8).
           02 P-PATNAME PIC X(24).
           02 P-SEX PIC X.
           02 P-RELATE PIC X.
           02 P-MSTAT PIC X.
           02 P-DOB PIC X(8).
       FD GARFILE.
       01 GARFILE01.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB. 
              03 G-DOBYY PIC XXXX.
              03 G-DOBMM PIC XX.
              03 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-ADDRCODE PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       FD  INSFILE.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).
       FD  REFPHY.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-KP PIC X(7).
           02 REF-FUTURE PIC XXX.
       FD  PROCFILE.
       01  PROCFILE01.
           02 PROC-KEY PIC X(7).
           02 PROC-OLD PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-BCBS PIC X(4).
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC S9(4)V99.
           02 PROC-CARE PIC S9(4)V99.
       FD  CMNTFILE.
       01  CMNTFILE01.
           02 CMNT-KEY.
             03 CM-KEY8 PIC X(8).
             03 CM-KEY3 PIC XXX.
           02 CMNT PIC X(80).
           02 CMNT-DATE-E PIC X(8).

       FD  NEWAUTHFILE.
       01  NEWAUTHFILE01.
           02 NEWAUTH-KEY.
              03 NEWAUTH-KEY8 PIC X(8).
              03 NEWAUTH-KEY6 PIC X(6).
           02 NEWAUTH-NUM PIC X(15).
           02 NEWAUTH-QNTY PIC XX.
           02 NEWAUTH-DATE-E PIC X(8).
           02 NEWAUTH-NDC PIC X(11).
           02 NEWAUTH-FILLER PIC X(30).
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       PROCEDURE DIVISION.
       P0.
           OPEN output charfile payfile parmndex claimfile patfile.
           OPEN output garfile insfile refphy procfile cmntfile 
                       newauthfile.
           close charfile payfile parmndex claimfile patfile.
           close garfile insfile refphy procfile cmntfile newauthfile.

           OPEN I-O charfile payfile parmndex claimfile patfile.
           OPEN I-O garfile insfile refphy procfile cmntfile
                        newauthfile.
            
           MOVE ALL "0" TO charfile01.
           WRITE charfile01.
           CLOSE charfile.  
           OPEN I-O charfile.
           MOVE ALL "0" TO charfile01.
           DELETE charfile RECORD.
           CLOSE charfile.

           MOVE ALL "0" TO payfile01.
           WRITE payfile01.
           CLOSE payfile.  
           OPEN I-O payfile.
           MOVE ALL "0" TO payfile01.
           DELETE payfile RECORD.
           CLOSE payfile.

           open output charfile payfile.
           open input file1 file2 file3 file4 file5 file6.
           open input file7 file8 file9 file10 file11.
       p1.
           read file1 at end go to p2.
           move file101 to charfile01
           write charfile01
           go to p1.
       p2.
           read file2 at end go to p3.
           move file201 to payfile01
           write payfile01
           go to p2.
       p3.
           read file3 at end go to p4.
           move file301 to parmndex01
           write parmndex01
           go to p3.
       p4.
           read file4 at end go to p5.
           move file401 to claimfile01
           write claimfile01
           go to p4.
       p5.
           read file5 at end go to p6.
           move file501 to patfile01
           write patfile01
           go to p5.
       p6.
           read file6 at end go to p7.
           move file601 to garfile01
           write garfile01
           go to p6.
       p7.
           read file7 at end go to p8.
           move file701 to insfile01
           write insfile01
           go to p7.
       p8.
           read file8 at end go to p9.
           move file801 to refphy01
           write refphy01
           go to p8.
       p9.
           read file9 at end go to p10.
           move file901 to procfile01 
           write procfile01 
           go to p9.
       p10.
           read file10 at end go to p11.
           move file1001 to cmntfile01
           write cmntfile01
           go to p10.
       p11.
           read file11 at end go to p99.
           move file1101 to newauthfile01
           write newauthfile01 
           go to p11.
       p99.
           close charfile payfile parmndex claimfile patfile
             garfile insfile refphy procfile cmntfile NEWAUTHfile
           close file1 file2 file3 file4 file5 file6 file7 file8 
                 file9 file10 file11
           STOP RUN.
