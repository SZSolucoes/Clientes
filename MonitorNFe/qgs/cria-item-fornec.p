FOR EACH item-doc-est NO-LOCK
   WHERE item-doc-est.cod-emitente = 100949
     AND item-doc-est.serie-docto = "4" 
     AND item-doc-est.nro-docto = "0012206"
     AND item-doc-est.nat-operacao = "190201",
     FIRST ITEM WHERE ITEM.it-codigo = item-doc-est.it-codigo NO-LOCK,
     FIRST emitente WHERE emitente.cod-emitente = item-doc-est.cod-emitente NO-LOCK:

    FIND ITEM-fornec NO-LOCK WHERE
         item-fornec.it-codigo = item-doc-est.it-codigo AND
         item-fornec.cod-emitente = item-doc-est.cod-emitente NO-ERROR.
    IF NOT AVAIL item-fornec THEN DO:
        CREATE item-fornec.
        ASSIGN item-fornec.it-codigo = item-doc-est.it-codigo   
               item-fornec.cod-emitente = item-doc-est.cod-emitente
               item-fornec.item-do-forn = TRIM(SUBSTRING(item-doc-est.it-codigo,4,50)).

       assign 
           item-fornec.unid-med-for = item.un
           item-fornec.fator-conver = 1
           item-fornec.num-casa-dec = 0
           item-fornec.ativo        = yes
           item-fornec.cod-cond-pag = emitente.cod-cond-pag
           item-fornec.classe-repro = 1.
    END.

END.
