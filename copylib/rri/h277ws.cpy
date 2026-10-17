      *    working storage for h277proc.cpy
       01  H-SEG PIC X(256).
      *    rri billing npis (parm139 line 2) and fed tax id (line 3),
      *    only claims whose nm1*85 id matches one of them are kept
       01  H-PROV1 PIC X(10) VALUE SPACE.
       01  H-PROV2 PIC X(10) VALUE SPACE.
       01  H-PROVTIN PIC X(10) VALUE SPACE.
       01  H277-WORK.
           02 H-STTYPE PIC X(3).
           02 H-ISACTL PIC X(9).
           02 H-RPTDATE PIC X(8).
           02 H-PAYID PIC X(10).
           02 H-PAYNAME PIC X(24).
           02 H-TRACE PIC X(30).
           02 H-NPI PIC X(10).
           02 H-HLCODE PIC XX.
           02 H-INCLM PIC X VALUE "N".
           02 H-INSVC PIC X VALUE "N".
           02 H-PCN PIC X(10).
           02 H-DOS PIC X(8).
           02 H-PAYCLM PIC X(20).
           02 H-X PIC 99.
           02 H-STC-CNT PIC 99 VALUE 0.
           02 H-STC-TAB OCCURS 20 TIMES.
             03 HS-STAT PIC X(12).
             03 HS-DATE PIC X(8).
             03 HS-ACTION PIC XX.
             03 HS-AMT PIC X(18).
             03 HS-MSG PIC X(60).
       01  H-FLD.
           02 H-F PIC X(60) OCCURS 17 TIMES.
       01  HOLD-H277-DATA PIC X(212).
       01  H277-WCNT PIC 9(6) VALUE 0.
       01  H277-DCNT PIC 9(6) VALUE 0.
