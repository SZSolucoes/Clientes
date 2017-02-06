DEFINE VARIABLE c-num-nota AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-total LIKE it-doc-fisc.vl-tot-item.


FIND FIRST doc-fiscal NO-LOCK
    WHERE doc-fiscal.cod-estabel  = ""
      AND doc-fiscal.serie        = ""
      AND doc-fiscal.nr-doc-fis   = ""
      AND doc-fiscal.cod-emitente = 3
      AND doc-fiscal.nat-operacao = "" NO-ERROR.

IF AVAIL doc-fiscal THEN DO:
    FOR EACH nota-fisc-adc NO-LOCK
        WHERE nota-fisc-adc.cod-estab        = doc-fiscal.cod-estabel
          AND nota-fisc-adc.cod-serie        = doc-fiscal.serie
          AND nota-fisc-adc.cod-nota-fisc    = doc-fiscal.nr-doc-fis
          AND nota-fisc-adc.cdn-emitente     = doc-fiscal.cod-emitente
          AND nota-fisc-adc.cod-natur-operac = doc-fiscal.nat-operacao:

        ASSIGN c-num-nota = c-num-nota + STRING(nota-fisc-adc.cod-docto-referado).

        FOR EACH it-doc-fisc NO-LOCK OF doc-fiscal:

            ASSIGN i-total = i-total + it-doc-fisc.vl-tot-item.

        END.

    END.

    MESSAGE "Notas" c-num-nota SKIP
            "Valor" i-total
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   
END.



    
