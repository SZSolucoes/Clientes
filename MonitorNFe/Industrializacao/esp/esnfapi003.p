
DEF INPUT  PARAM p-cod-emit     AS INT NO-UNDO.
DEF INPUT  PARAM p-cod-estabel  AS char NO-UNDO.
DEF INPUT  PARAM p-ch  LIKE nfe013.ch-acesso-comp-nfe NO-UNDO.
DEF INPUT  PARAM p-idi AS INT NO-UNDO.
DEF INPUT  PARAM p-seq AS INT NO-UNDO.
DEF INPUT  PARAM p-cod-ncm  AS char NO-UNDO.

DEF OUTPUT PARAM l-ok       AS LOG  NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-ean-aux AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-it-codigo-aux AS CHARACTER   NO-UNDO.

def var de-indice as dec no-undo.
def var l-ean as log no-undo.

ASSIGN c-ean-aux       = ''
       c-it-codigo-aux = '' .
       
FIND FIRST nfe013 
     WHERE nfe013.ch-acesso-comp-nfe =  p-ch
       AND nfe013.idi-orig-trad      =  1
       AND nfe013.seq-item           =  p-seq NO-LOCK NO-ERROR .

IF AVAIL nfe013 THEN DO:
    ASSIGN c-ean-aux       = nfe013.cod-ean
           c-it-codigo-aux = nfe013.it-codigo .
END.
ELSE DO:
    RETURN 'nok'.
END.


FIND FIRST nfe013 
     WHERE nfe013.ch-acesso-comp-nfe =  p-ch
       AND nfe013.idi-orig-trad      =  p-idi
       AND nfe013.seq-item           =  p-seq EXCLUSIVE-LOCK NO-ERROR .

IF AVAIL nfe013 THEN DO:

    FOR EACH nfe017     
       WHERE nfe017.ch-acesso-comp-nfe = p-ch
         AND nfe017.cd-msg  = 47 EXCLUSIVE-LOCK :
       DELETE nfe017.
    END.
           
    /** DE-PARA FORNECEDOR **/
               
    FOR FIRST item-fornec 
        WHERE item-fornec.item-do-forn = c-it-codigo-aux          
        AND   item-fornec.cod-emitente = p-cod-emit NO-LOCK:

    END.

    IF AVAIL item-fornec THEN DO:
        ASSIGN l-ok = YES.
        ASSIGN nfe013.it-codigo      = item-fornec.it-codigo
               nfe013.un-comercial   = item-fornec.unid-med-for
               nfe013.un-interna     = item-fornec.unid-med-for
               de-indice             = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
               nfe013.qtd-interna    = nfe013.qtd-comercial / de-indice
               nfe013.preco-unit     = nfe013.preco-total / nfe013.qtd-interna .

        FIND FIRST ITEM 
             WHERE ITEM.it-codigo = nfe013.it-codigo NO-LOCK NO-ERROR .
        IF AVAIL ITEM THEN DO:
        
            ASSIGN nfe013.class-fiscal = ITEM.class-fiscal.
            FOR FIRST nfe003 NO-LOCK 
                WHERE nfe003.ch-acesso-comp-nfe = p-ch
                  AND nfe003.idi-orig-trad      = 2:
                 
                 assign nfe013.nat-operacao = nfe003.nat-oper-com .
                 
            END. 
        
            FIND FIRST item-uni-estab 
                 WHERE item-uni-estab.it-codigo   =  ITEM.it-codigo
                   AND item-uni-estab.cod-estabel =  p-cod-estabel NO-LOCK NO-ERROR .
            IF AVAIL item-uni-estab THEN DO:
                   ASSIGN nfe013.cod-depos   = item-uni-estab.deposito-pad 
                          nfe013.cod-localiz = item-uni-estab.cod-localiz .
        
                   IF item-uni-estab.contr-qualid = YES THEN DO:
                       FIND FIRST estabelec 
                            WHERE estabele.cod-estabel = p-cod-estabel NO-LOCK NO-ERROR .
                       IF AVAIL estabelec THEN DO:
                           ASSIGN nfe013.cod-depos = estabelec.deposito-cq .
                       END.
                   END.
        
            END.
            ELSE DO:
                ASSIGN nfe013.cod-depos   = ITEM.deposito-pad 
                       nfe013.cod-localiz = ITEM.cod-localiz. 
            END.
        END.
    END.
    ELSE DO:  
        ASSIGN l-ok = NO.
        CREATE nfe017.
        ASSIGN nfe017.ch-acesso-comp-nfe = p-ch
               nfe017.idi-orig-trad      = 2
               nfe017.dt-msg             = TODAY
               nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
               nfe017.log-ativo          = YES
               nfe017.cd-msg             = 47
               nfe017.texto-msg          = "XML Item " + c-it-codigo-aux + " n∆o cadastrado no ems. (cc0105)" 
               nfe017.seq-msg            = NEXT-VALUE(seq-msg-ret-nfe-rec). 
    END.
    
END.


