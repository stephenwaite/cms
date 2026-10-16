      *    277 history paragraphs. needs h277ws.cpy in working storage
      *    and H277FILE (h277file.cpy) open i-o.
      *    set H-PROV1 / H-PROV2 to the rri npis and H-PROVTIN to the
      *    rri tax id - claims whose billing provider (nm1*85, XX npi
      *    or FI tax id) matches none of them are not kept.
      *    move each segment to H-SEG and PERFORM H277-SEG,
      *    PERFORM H277-FLUSH at end of file.
       H277-SEG.
           INSPECT H-SEG REPLACING ALL "~" BY SPACE
           MOVE SPACE TO H-FLD
           UNSTRING H-SEG DELIMITED BY "*" INTO
             H-F(1) H-F(2) H-F(3) H-F(4) H-F(5) H-F(6) H-F(7)
             H-F(8) H-F(9) H-F(10) H-F(11) H-F(12) H-F(13) H-F(14)
             H-F(15) H-F(16) H-F(17)

           EVALUATE TRUE
             WHEN H-F(1) = "ISA"
               MOVE H-F(14) TO H-ISACTL
             WHEN H-F(1) = "ST"
               PERFORM H277-FLUSH
               MOVE H-F(2) TO H-STTYPE
             WHEN H-F(1) = "SE" OR "GE" OR "IEA"
               PERFORM H277-FLUSH
               MOVE SPACE TO H-HLCODE H-STTYPE
             WHEN H-F(1) = "BHT"
               MOVE H-F(5) TO H-RPTDATE
             WHEN H-F(1) = "HL"
               PERFORM H277-FLUSH
               MOVE H-F(4) TO H-HLCODE
             WHEN H-F(1) = "NM1" AND H-F(2) = "PR"
               MOVE H-F(4) TO H-PAYNAME
               MOVE H-F(10) TO H-PAYID
             WHEN H-F(1) = "NM1" AND H-F(2) = "85"
               MOVE H-F(10) TO H-NPI
             WHEN H-F(1) = "TRN" AND H-F(2) = "1"
               MOVE H-F(3) TO H-TRACE
             WHEN H-F(1) = "TRN" AND H-F(2) = "2"
                  AND H-HLCODE = "PT" AND H-STTYPE = "277"
               PERFORM H277-FLUSH
               MOVE "Y" TO H-INCLM
               MOVE H-F(3) TO H-PCN
               MOVE SPACE TO H-DOS H-PAYCLM
             WHEN H-INCLM NOT = "Y"
               CONTINUE
             WHEN H-F(1) = "SVC"
               MOVE "Y" TO H-INSVC
             WHEN H-INSVC = "Y"
               CONTINUE
             WHEN H-F(1) = "STC" AND H-STC-CNT < 20
               ADD 1 TO H-STC-CNT
               MOVE H-F(2) TO HS-STAT(H-STC-CNT)
               MOVE H-F(3) TO HS-DATE(H-STC-CNT)
               MOVE H-F(4) TO HS-ACTION(H-STC-CNT)
               MOVE H-F(5) TO HS-AMT(H-STC-CNT)
               MOVE H-F(13) TO HS-MSG(H-STC-CNT)
             WHEN H-F(1) = "DTP" AND H-F(2) = "472" AND H-DOS = SPACE
               MOVE H-F(4)(1:8) TO H-DOS
             WHEN H-F(1) = "REF" AND H-F(2) = "1K"
               MOVE H-F(3) TO H-PAYCLM
           END-EVALUATE.

       H277-FLUSH.
           IF H-INCLM = "Y"
              AND H-NPI NOT = SPACE
              AND (H-NPI = H-PROV1 OR H-NPI = H-PROV2
                   OR H-NPI = H-PROVTIN)
             PERFORM H277-PUT
               VARYING H-X FROM 1 BY 1 UNTIL H-X > H-STC-CNT
           END-IF
           MOVE "N" TO H-INCLM H-INSVC
           MOVE 0 TO H-STC-CNT.

       H277-PUT.
           MOVE SPACE TO H277FILE01
           MOVE H-PCN TO H277-PCN
           MOVE H-DOS TO H277-DOS
           MOVE HS-DATE(H-X) TO H277-RECDATE
           IF H277-RECDATE = SPACE
             MOVE H-RPTDATE TO H277-RECDATE
           END-IF
           MOVE H-PAYID TO H277-PAYID
           MOVE H-PAYNAME TO H277-PAYNAME
           MOVE H-PAYCLM TO H277-PAYCLM
           MOVE H-TRACE TO H277-TRACE
           MOVE HS-STAT(H-X) TO H277-STAT
           MOVE HS-ACTION(H-X) TO H277-ACTION
           MOVE 0 TO H277-AMOUNT
           IF HS-AMT(H-X) NOT = SPACE
             COMPUTE H277-AMOUNT = FUNCTION NUMVAL(HS-AMT(H-X))
           END-IF
           MOVE H-NPI TO H277-NPI
           MOVE H-ISACTL TO H277-ISACTL
           MOVE H-RPTDATE TO H277-RPTDATE
           MOVE HS-MSG(H-X) TO H277-MSG
           MOVE 1 TO H277-SEQ
           PERFORM H277-WRITE THRU H277-WRITE-EXIT.

      *    same key already on file: skip if identical (rerun of the
      *    same 277), otherwise bump the seq
       H277-WRITE.
           WRITE H277FILE01 INVALID GO TO H277-WRITE-DUP.
           ADD 1 TO H277-WCNT
           GO TO H277-WRITE-EXIT.
       H277-WRITE-DUP.
           MOVE H277-DATA TO HOLD-H277-DATA
           READ H277FILE INVALID GO TO H277-WRITE-EXIT.
           IF H277-DATA = HOLD-H277-DATA
             ADD 1 TO H277-DCNT
             GO TO H277-WRITE-EXIT.
           MOVE HOLD-H277-DATA TO H277-DATA
           IF H277-SEQ = 99
             DISPLAY H277-PCN " " H277-DOS " 277 HIST SEQ FULL"
             GO TO H277-WRITE-EXIT.
           ADD 1 TO H277-SEQ
           GO TO H277-WRITE.
       H277-WRITE-EXIT.
           EXIT.
