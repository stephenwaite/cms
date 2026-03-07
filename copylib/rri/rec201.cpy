       01  REC201.
           02 R2-1 PIC XX.
           02 R2-ACC.
             03 R2-ACCMM PIC XX.
             03 R2-ACCDD PIC XX.
             03 R2-ACCYY PIC XX.
             03 R2-ACCHHMM PIC X(5).
      * 13       
           02 R2-REFDOC.
             03 R2-REFDOC4 PIC XXXX.
             03 R2-REFDOC22 PIC X(18).
      * 35       
           02 R2-DIAG PIC X(130).
      * 165
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
      * 358     
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
      * col 1005     
           02 R2-NPI PIC X(10).
           02 R2-ADMIT-DATE-DUPLICATE PIC X(6).
           02 R2-PATSEX PIC X.
           02 R2-PATDOB.
             03 R2-DOBMM PIC XX.
             03 R2-DOBDD PIC XX.
             03 R2-DOBYY PIC XX.
           02 R2-DISCHARGE PIC X(6).
      *     02 FILLER PIC X(7).
