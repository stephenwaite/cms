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
      * 478
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
           02  R1-AUTH PIC X(20).
      * 638
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
      * 801
           02 INSURANCE-2.
            03 R1-INSNAME2 PIC X(25).
            03 R1-INSCONTACT2 PIC X(25).
            03 R1-INSADDR12 PIC X(20).
            03 R1-INSADDR22 PIC X(15).
            03 R1-INSCITY2 PIC X(20).
            03 R1-INSSTATE2 PIC XX.
            03 R1-INSZIP2 PIC X(10).
            03 R1-INSPHONE2 PIC X(12).
      *    02 R1-IO PIC X(4).
      *    02 FILLER PIC X(2).
