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
