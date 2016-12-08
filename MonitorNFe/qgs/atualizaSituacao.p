FOR EACH nfe003 EXCLUSIVE-LOCK
    WHERE nfe003.idi-orig-trad = 2
      AND nfe003.idi-situacao <> 3:

    FOR FIRST docum-est
        WHERE docum-est.serie-docto  = nfe003.serie-docto  
          AND docum-est.nro-docto    = nfe003.nro-docto   
          AND docum-est.cod-emitente = nfe003.cod-emitente
          AND docum-est.nat-operacao = "194oo":
        
        IF docum-est.ce-atual THEN
            ASSIGN nfe003.idi-situacao = 3 /* Atualizada */ .
        ELSE
            ASSIGN nfe003.idi-situacao = 1 /* Digitada */ .
    END.
END.
