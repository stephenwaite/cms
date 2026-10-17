      *    277 / 277CA claim status history, one row per claim-level STC
      *    key = patient control nbr (CLM01) + dos + payer status date
       01  H277FILE01.
           02 H277-KEY.
             03 H277-PCN.
               04 H277-KEY8 PIC X(8).
               04 H277-SUFX PIC XX.
             03 H277-DOS PIC X(8).
             03 H277-RECDATE PIC X(8).
             03 H277-SEQ PIC 99.
           02 H277-DATA.
             03 H277-PAYID PIC X(10).
             03 H277-PAYNAME PIC X(24).
             03 H277-PAYCLM PIC X(20).
             03 H277-TRACE PIC X(30).
             03 H277-STAT PIC X(12).
             03 H277-ACTION PIC XX.
             03 H277-AMOUNT PIC S9(5)V99.
             03 H277-NPI PIC X(10).
             03 H277-ISACTL PIC X(9).
             03 H277-RPTDATE PIC X(8).
             03 H277-MSG PIC X(60).
             03 H277-FILLER PIC X(20).
