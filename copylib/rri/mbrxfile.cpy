      *    payer returned member id per garno + payer, from the 835.
      *    holds ids too long for g-pripol (va icn is 17)
       01  MBRXFILE01.
           02 MX-KEY.
             03 MX-KEY8 PIC X(8).
             03 MX-PAYERID PIC X(10).
           02 MX-MEMBID PIC X(20).
           02 MX-QUAL PIC XX.
           02 MX-ENTITY PIC XX.
           02 MX-SRC PIC X(3).
           02 MX-DATE PIC X(8).
           02 MX-ICN PIC X(13).
           02 MX-FILLER PIC X(14).
