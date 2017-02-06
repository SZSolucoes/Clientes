DEFINE VARIABLE da-movto                LIKE docum-est.dt-emissao NO-UNDO.

DEFINE TEMP-TABLE tt-concilia-fiscal NO-UNDO 
    field cod-emitente like movto-estoq.cod-emitente
    field cod-estabel  like movto-estoq.cod-estabel
    field serie-docto  like movto-estoq.serie-docto
    field nro-docto    like movto-estoq.nro-docto
    field nat-operacao like movto-estoq.nat-operacao 
    field dt-trans     like movto-estoq.dt-trans
    field valor-ce-db as dec
    field valor-ce-cr as dec
    field valor-ft-cr as dec
    field valor-ft-db as dec
    field valor-of-cr as dec
    field valor-of-db as dec
    field valor-dif   as dec.
    
for each tt-concilia-fiscal no-lock:
delete tt-concilia-fiscal.
end.    

DO da-movto = 12/02/2016 TO 12/02/2016:
    
    FOR EACH movto-estoq  
       WHERE movto-estoq.dt-trans      = da-movto
         AND movto-estoq.cod-emitente >= 9710
         AND movto-estoq.cod-emitente <= 9710
         AND movto-estoq.cod-estabel  >= "701"
         AND movto-estoq.cod-estabel  <= "701"
         AND movto-estoq.serie-docto  >= "1"
         AND movto-estoq.serie-docto  <= "1"
         AND movto-estoq.nro-docto    >= "0588596"
         AND movto-estoq.nro-docto    <= "0588596" NO-LOCK: 
         
        

        FIND FIRST tt-concilia-fiscal
             WHERE tt-concilia-fiscal.cod-emitente = movto-estoq.cod-emitente
               AND tt-concilia-fiscal.cod-estabel  = movto-estoq.cod-estabel
               AND tt-concilia-fiscal.serie-docto  = movto-estoq.serie-docto
               AND tt-concilia-fiscal.nro-docto    = movto-estoq.nro-docto
    /*           and tt-concilia-fiscal.nat-operacao = movto-estoq.nat-operacao*/ NO-ERROR.
        IF NOT AVAIL tt-concilia-fiscal THEN DO:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY movto-estoq TO tt-concilia-fiscal.
        END.
        

        /*MESSAGE 1 SKIP
            "Trans" movto-estoq.nr-trans skip
            "Item" movto-estoq.it-codigo skip
            "Estab" movto-estoq.cod-estabel skip
            "Data Trans" movto-estoq.dt-trans skip
            "Valor 1" movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        MESSAGE 2 SKIP
            "Trans" movto-estoq.nr-trans skip
            "Item" movto-estoq.it-codigo skip
            "Estab" movto-estoq.cod-estabel skip
            "Data Trans" movto-estoq.dt-trans skip
            "Valor 2"  movto-estoq.valor-ggf-m[1]
                     + movto-estoq.valor-mat-m[1]
                     + movto-estoq.valor-mob-m[1]
                     + movto-estoq.valor-icm
                     + movto-estoq.valor-ipi
                     + movto-estoq.valor-iss
                     + movto-estoq.valor-pis
                     + movto-estoq.val-cofins
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       
        IF movto-estoq.tipo-trans = 1 THEN
            ASSIGN tt-concilia-fiscal.valor-ce-db = tt-concilia-fiscal.valor-ce-db + (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1])
                                  /*+ movto-estoq.valor-nota*/
                                  /*+ movto-estoq.valor-icm
                                  + movto-estoq.valor-ipi
                                  + movto-estoq.valor-iss
                                  + movto-estoq.valor-pis
                                  + movto-estoq.val-cofins*/ . 
        ELSE
            ASSIGN tt-concilia-fiscal.valor-ce-cr = tt-concilia-fiscal.valor-ce-cr + (movto-estoq.valor-ggf-m[1]
                     + movto-estoq.valor-mat-m[1]
                     + movto-estoq.valor-mob-m[1]
                     + movto-estoq.valor-icm
                     + movto-estoq.valor-ipi
                     + movto-estoq.valor-iss
                     + movto-estoq.valor-pis
                     + movto-estoq.val-cofins)
                                  /*+ movto-estoq.valor-nota*/
                                  /*+ movto-estoq.valor-icm
                                  + movto-estoq.valor-ipi
                                  + movto-estoq.valor-iss                              
                                  + movto-estoq.valor-pis
                                  + movto-estoq.val-cofins*/. 
    END.

    /*Entradas*/
    FOR EACH doc-fiscal NO-LOCK
       WHERE doc-fiscal.dt-docto      = da-movto
         AND doc-fiscal.cod-estabel  >= "701"    
         AND doc-fiscal.cod-estabel  <= "701"    
         AND doc-fiscal.serie        >= "1"      
         AND doc-fiscal.serie        <= "1"      
         AND doc-fiscal.nr-doc-fis   >= "0588596"
         AND doc-fiscal.nr-doc-fis   <= "0588596"
         AND (doc-fiscal.tipo-nat   = 1 OR 
             (doc-fiscal.tipo-nat   = 3 AND
              doc-fiscal.cod-cfop = "1933" OR
              doc-fiscal.cod-cfop = "2933"))         
         AND   doc-fiscal.ind-sit-doc = 1,
        EACH it-doc-fisc OF doc-fiscal:
        
        FIND FIRST tt-concilia-fiscal
             WHERE tt-concilia-fiscal.cod-emitente = doc-fiscal.cod-emitente
               AND tt-concilia-fiscal.cod-estabel  = doc-fiscal.cod-estabel
               AND tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
               AND tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
    /*           and tt-concilia-fiscal.nat-operacao = doc-fiscal.nat-operacao*/ NO-ERROR.
        IF NOT AVAIL tt-concilia-fiscal THEN DO:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY doc-fiscal TO tt-concilia-fiscal.
            ASSIGN tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
                   tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
                   tt-concilia-fiscal.dt-trans     = doc-fiscal.dt-docto.
        END.
        
        ASSIGN tt-concilia-fiscal.valor-of-db = tt-concilia-fiscal.valor-of-db 
                                              + it-doc-fisc.vl-tot-item
                                              /*+ it-doc-fisc.val-pis
                                              + it-doc-fisc.val-cofins
                                              + it-doc-fisc.vl-iss-it 
                                              + it-doc-fisc.vl-icms-it 
                                              + it-doc-fisc.vl-ipi-it 
                                              + it-doc-fisc.vl-ipi-devol
                                              + it-doc-fisc.vl-icmsub-it*/.
    END.

    /*Sa¡das*/
    FOR EACH doc-fiscal NO-LOCK
       WHERE doc-fiscal.dt-docto      = da-movto
         AND doc-fiscal.cod-estabel  >= "701"    
         AND doc-fiscal.cod-estabel  <= "701"    
         AND doc-fiscal.serie        >= "1"      
         AND doc-fiscal.serie        <= "1"      
         AND doc-fiscal.nr-doc-fis   >= "0588596"
         AND doc-fiscal.nr-doc-fis   <= "0588596"
         AND ((doc-fiscal.tipo-nat    = 3 AND
               (doc-fiscal.cod-cfop = "5933" OR
                doc-fiscal.cod-cfop = "6933")) OR
           doc-fiscal.tipo-nat = 2 OR  
          (doc-fiscal.tipo-nat = 1 and doc-fiscal.ind-sit-doc = 2)),
        EACH it-doc-fisc OF doc-fiscal no-lock:
        
        FIND FIRST tt-concilia-fiscal
             WHERE tt-concilia-fiscal.cod-emitente = doc-fiscal.cod-emitente
               AND tt-concilia-fiscal.cod-estabel  = doc-fiscal.cod-estabel
               AND tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
               AND tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
    /*           and tt-concilia-fiscal.nat-operacao = doc-fiscal.nat-operacao*/ NO-ERROR.
        IF NOT AVAIL tt-concilia-fiscal THEN DO:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY doc-fiscal TO tt-concilia-fiscal.
            ASSIGN tt-concilia-fiscal.cod-emitente = doc-fiscal.cod-emitente
                   tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
                   tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
                   tt-concilia-fiscal.dt-trans     = doc-fiscal.dt-docto.
        END.
        
        ASSIGN tt-concilia-fiscal.valor-of-cr = tt-concilia-fiscal.valor-of-cr
                                              + it-doc-fisc.vl-tot-item
                                              /*+ it-doc-fisc.val-pis
                                              + it-doc-fisc.val-cofins
                                              + it-doc-fisc.vl-iss-it 
                                              + it-doc-fisc.vl-icms-it 
                                              + it-doc-fisc.vl-ipi-it 
                                              + it-doc-fisc.vl-ipi-devol
                                              + it-doc-fisc.vl-icmsub-it*/ .
    END.

END.
for each tt-concilia-fiscal no-lock:
assign tt-concilia-fiscal.valor-dif = tt-concilia-fiscal.valor-ce-db - tt-concilia-fiscal.valor-ce-cr.
disp tt-concilia-fiscal with 1 col.
end.
