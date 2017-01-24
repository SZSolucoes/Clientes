/*****************************************************************************
 ** PROGRAMA..: ESCC005RP.P
 ** OBJETIVO..: Listar Pedidos Emitidos
 ** AUTOR.....: SZ SOLUCOES
 ** CLIENTE...: NOBLE
 ** VERSAO....: 2.06.00.001 - 10/10/2014 
 ******************************************************************************/
 
/**** Defini‡äes ****/
{include/i-prgvrs.i ESCC005 2.06.00.001}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD cod-estabel-ini   AS CHARACTER FORMAT "x(5)"
    FIELD cod-estabel-fim   AS CHARACTER FORMAT "x(5)"
    FIELD data-pedido-ini   AS DATE FORMAT "99/99/9999"
    FIELD data-pedido-fim   AS DATE FORMAT "99/99/9999"
    FIELD cod-emitente-ini  AS INTEGER
    FIELD cod-emitente-fim  AS INTEGER
    FIELD it-codigo-ini     AS CHARACTER FORMAT "x(16)"
    FIELD it-codigo-fim     AS CHARACTER FORMAT "x(16)"
    FIELD familia-ini       AS CHARACTER FORMAT "x(16)"
    FIELD familia-fim       AS CHARACTER FORMAT "x(16)"
    FIELD cod-comprado-ini  AS CHARACTER FORMAT "x(12)"
    FIELD cod-comprado-fim  AS CHARACTER FORMAT "x(12)"
    FIELD rd-situacao       AS INTEGER
    FIELD nr-contrato-ini   AS INTEGER
    FIELD nr-contrato-fim   AS INTEGER
    FIELD dt-publicacao-ini AS DATE 
    FIELD dt-publicacao-fim AS DATE 
    FIELD dt-emissao-ini    AS DATE
    FIELD dt-emissao-fim    AS DATE.

DEFINE TEMP-TABLE tt-digita NO-UNDO 
    FIELD responsavel LIKE pedido-compr.responsavel
    FIELD c-nome-comprador LIKE comprador.nome.

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

DEFINE VARIABLE chExcel            AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chworkbook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chexcelapplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE i-linha            AS INTEGER          NO-UNDO.
DEFINE VARIABLE c-nome-arq         AS CHARACTER        NO-UNDO.
DEFINE VARIABLE h-acomp            AS HANDLE           NO-UNDO.
DEFINE VARIABLE arquivo-at         AS CHAR             NO-UNDO.
DEFINE VARIABLE arquivo-cp         AS CHAR             NO-UNDO.

/*********************** VARIAVEIS LOCAIS ***********************/
DEFINE VARIABLE d-data-contrato             AS DATE NO-UNDO.
DEFINE VARIABLE data-entrega                AS DATE NO-UNDO.
DEFINE VARIABLE c-situacao                  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-natureza                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nome-abrev                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-desc-item                 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-un-item                   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-fm-codigo                 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cond-pagto-desc           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-pre-unit-for              AS DECIMAL    NO-UNDO.
DEFINE VARIABLE i-mo-codigo                 AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-cod-cond-pag              AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-sit-pedido                AS CHARACTER   NO-UNDO. 
DEFINE VARIABLE c-ped-comlink               AS CHARACTER FORMAT "x(30)"   NO-UNDO. 
DEFINE VARIABLE d-data-public               AS DATE FORMAT "99/99/9999"   NO-UNDO.
DEFINE VARIABLE dt-aprova-solic             AS DATE FORMAT "99/99/9999"   NO-UNDO.
DEFINE VARIABLE c-prioridade-ordem-compra   AS CHARACTER   NO-UNDO.
/****************************************************************/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Gerando Dados Excel").
ASSIGN c-nome-arq = SESSION:TEMP-DIRECTORY + "ESCC005-" + STRING(TODAY, "999999") + "-" + STRING(TIME) + ".xls".

ASSIGN i-linha = 2.

FIND FIRST tt-digita NO-LOCK NO-ERROR.
IF AVAIL tt-digita THEN
    RUN pi-carrega-tt-digita.
ELSE DO:

    IF tt-param.rd-situacao =  1 THEN DO: /* S¢ lista pedidos sem contratos */

        FILE-INFO:FILE-NAME = "modelo/escc005.xls".
        arquivo-at = FILE-INFO:FULL-PATHNAME.
        arquivo-cp = SESSION:TEMP-DIR + "escc005.xls".
        OS-COMMAND SILENT COPY VALUE(arquivo-at) VALUE(arquivo-cp).
        CREATE "excel.application" chexcelapplication.
        chworkbook  = chexcelapplication:workbooks:ADD(arquivo-cp). /*substituir por "arquivo" */
        chworksheet = chexcelapplication:sheets:ITEM(1).
        chexcelapplication:VISIBLE = FALSE.
        chexcelapplication:DisplayAlerts = FALSE.

        FOR EACH pedido-compr
            WHERE pedido-compr.cod-estabel  >= tt-param.cod-estabel-ini
              AND pedido-compr.cod-estabel  <= tt-param.cod-estabel-fim
              AND pedido-compr.data-pedido  >= tt-param.data-pedido-ini
              AND pedido-compr.data-pedido  <= tt-param.data-pedido-fim
              AND pedido-compr.cod-emitente >= tt-param.cod-emitente-ini
              AND pedido-compr.cod-emitente <= tt-param.cod-emitente-fim NO-LOCK:

            IF pedido-compr.nr-contrato <> 0 THEN NEXT.
        
            RUN pi-acompanhar IN h-acomp (INPUT "Num Pedido: " + STRING(pedido-compr.num-pedido)).
    
            ASSIGN c-sit-pedido = "Sem Aprova‡Æo".

            FIND LAST doc-pend-aprov USE-INDEX doc-pend-aprov-2 
                WHERE doc-pend-aprov.ind-tip-doc  = 4
                  AND doc-pend-aprov.num-pedido   = pedido-compr.num-pedido NO-LOCK NO-ERROR.
            IF AVAIL doc-pend-aprov THEN
                ASSIGN c-sit-pedido = {ininc/i02in631.i 04 doc-pend-aprov.ind-situacao}.    

            FIND emitente
                WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
            IF AVAIL emitente THEN
                ASSIGN c-nome-abrev = emitente.nome-abrev.
            ELSE
                ASSIGN c-nome-abrev = "".
    
            ASSIGN c-natureza = {ininc/i01in295.i 04 pedido-compr.natureza}.
        
            FOR EACH ordem-compra USE-INDEX pedido-item
                WHERE ordem-compra.num-pedido  = pedido-compr.num-pedido
                AND   ordem-compra.it-codigo    >= tt-param.it-codigo-ini
                AND   ordem-compra.it-codigo    <= tt-param.it-codigo-fim
                AND   ordem-compra.cod-comprado >= tt-param.cod-comprado-ini
                AND   ordem-compra.cod-comprado <= tt-param.cod-comprado-fim NO-LOCK:

                ASSIGN c-prioridade-ordem-compra = "".
                    
                FIND requisicao NO-LOCK
                    WHERE requisicao.nr-requisicao = ordem-compra.nr-requisicao NO-ERROR.
                
                ASSIGN dt-aprova-solic = DATE("").
                
                FOR EACH doc-pend-aprov NO-LOCK
                    WHERE doc-pend-aprov.ind-tip-doc  = 1
                      AND doc-pend-aprov.nr-requisicao = ordem-compra.nr-requisicao 
                    BREAK BY doc-pend-aprov.nr-requisicao:
                    
                    IF LAST-OF(doc-pend-aprov.nr-requisicao) THEN
                        ASSIGN dt-aprova-solic = doc-pend-aprov.dt-aprova.
                END.             
                      
                FIND FIRST cotacao-item
                    WHERE cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      AND cotacao-item.it-codigo    = ordem-compra.it-codigo
                      AND cotacao-item.cod-emitente = ordem-compra.cod-emitente 
                      AND cotacao-item.cot-aprovada = YES NO-LOCK NO-ERROR.
                IF AVAIL cotacao-item THEN
                    ASSIGN d-pre-unit-for = cotacao-item.pre-unit-for
                           i-mo-codigo    = cotacao-item.mo-codigo
                           i-cod-cond-pag = cotacao-item.cod-cond-pag.
                ELSE
                    ASSIGN d-pre-unit-for = dec("")
                           i-mo-codigo    = int("")
                           i-cod-cond-pag = int("").
        
                FIND ITEM
                    WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
                IF AVAIL ITEM THEN
                    ASSIGN c-desc-item      = ITEM.desc-item
                           c-un-item        = ITEM.un
                           c-fm-codigo = ITEM.fm-codigo.
                ELSE 
                    ASSIGN c-desc-item      = ""
                           c-un-item        = ""
                           c-fm-codigo = "".
                
                FIND contrato-for
                    WHERE contrato-for.nr-contrato  = ordem-compra.nr-contrato NO-LOCK NO-ERROR.
                IF AVAIL contrato-for THEN
                    ASSIGN d-data-contrato = contrato-for.dt-contrato.
                ELSE
                    ASSIGN d-data-contrato = DATE("").
        
                FIND cond-pagto
                    WHERE cond-pagto.cod-cond-pag = ordem-compra.cod-cond-pag NO-LOCK NO-ERROR.
                IF AVAIL cond-pagto THEN
                    ASSIGN c-cond-pagto-desc = cond-pagto.descricao.
                ELSE
                    ASSIGN c-cond-pagto-desc = "".
        
                ASSIGN c-situacao = {ininc/i02in274.i 04 ordem-compra.situacao}.
                
                /* Prioridade Aprova‡Æo Ordem Compra */
                IF ordem-compra.prioridade-aprov = 150 THEN
                    ASSIGN c-prioridade-ordem-compra = "BAIXA".
                    
                IF ordem-compra.prioridade-aprov = 450 THEN 
                    ASSIGN c-prioridade-ordem-compra = "MDIA".
               
                IF ordem-compra.prioridade-aprov = 750 THEN
                    ASSIGN c-prioridade-ordem-compra = "ALTA".
                    
                IF ordem-compra.prioridade-aprov = 999 THEN
                    ASSIGN c-prioridade-ordem-compra = "MUITO ALTA".

                FOR EACH it-requisicao NO-LOCK
                    WHERE it-requisicao.nr-requisicao = requisicao.nr-requisicao:
                    
                    FOR EACH prazo-compra
                        WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:
                      
                        ASSIGN chexcelapplication:Range("A" + STRING(i-linha)):VALUE  = pedido-compr.num-pedido
                               chexcelapplication:Range("B" + STRING(i-linha)):VALUE  = pedido-compr.data-pedido
                               chexcelapplication:Range("C" + STRING(i-linha)):VALUE  = c-natureza
                               chexcelapplication:Range("D" + STRING(i-linha)):VALUE  = pedido-compr.emergencial
                               chexcelapplication:Range("E" + STRING(i-linha)):VALUE  = pedido-compr.responsavel
                               chexcelapplication:Range("F" + STRING(i-linha)):VALUE  = ordem-compra.cod-comprado
                               chexcelapplication:Range("G" + STRING(i-linha)):VALUE  = ordem-compra.nr-contrato
                               chexcelapplication:Range("H" + STRING(i-linha)):NUMBERFORMAT  = "@"
                               chexcelapplication:Range("H" + STRING(i-linha)):VALUE  = STRING(d-data-contrato,"99/99/9999")
                               chexcelapplication:Range("I" + STRING(i-linha)):VALUE  = pedido-compr.cod-emitente
                               chexcelapplication:Range("J" + STRING(i-linha)):VALUE  = c-nome-abrev
                               chexcelapplication:Range("K" + STRING(i-linha)):VALUE  = i-cod-cond-pag
                               chexcelapplication:Range("L" + STRING(i-linha)):VALUE  = c-cond-pagto-desc
                               chexcelapplication:Range("M" + STRING(i-linha)):VALUE  = ordem-compra.numero-ordem
                               chexcelapplication:Range("N" + STRING(i-linha)):VALUE  = ordem-compra.num-ord-inv
                               chexcelapplication:Range("O" + STRING(i-linha)):VALUE  = ordem-compra.ordem-servic
                               chexcelapplication:Range("P" + STRING(i-linha)):VALUE  = ordem-compra.conta-contabil
                               chexcelapplication:Range("Q" + STRING(i-linha)):VALUE  = pedido-compr.cod-estabel
                               chexcelapplication:Range("R" + STRING(i-linha)):VALUE  = ordem-compra.it-codigo
                               chexcelapplication:Range("S" + STRING(i-linha)):VALUE  = c-desc-item
                               chexcelapplication:Range("T" + STRING(i-linha)):VALUE  = c-un-item
                               chexcelapplication:Range("U" + STRING(i-linha)):VALUE  = c-fm-codigo
                               chexcelapplication:Range("V" + STRING(i-linha)):VALUE  = d-pre-unit-for
                               chexcelapplication:Range("W" + STRING(i-linha)):VALUE  = i-mo-codigo                       
                               chexcelapplication:Range("X" + STRING(i-linha)):VALUE  = c-situacao
                               chexcelapplication:Range("Y" + STRING(i-linha)):VALUE  = prazo-compra.parcela
                               chexcelapplication:Range("Z" + STRING(i-linha)):NUMBERFORMAT  = "@"
                               chexcelapplication:Range("Z" + STRING(i-linha)):VALUE  = STRING(prazo-compra.data-entrega,"99/99/9999")
                               chexcelapplication:Range("AA" + STRING(i-linha)):VALUE = prazo-compra.quantidade
                               chexcelapplication:Range("AB" + STRING(i-linha)):VALUE = prazo-compra.quant-receb
                               chexcelapplication:Range("AC" + STRING(i-linha)):VALUE = prazo-compra.quant-saldo
                               chexcelapplication:Range("AD" + STRING(i-linha)):VALUE = ordem-compra.pre-unit-for
                               chexcelapplication:Range("AE" + STRING(i-linha)):VALUE = c-sit-pedido
                               chexcelapplication:Range("AF" + STRING(i-linha)):VALUE = ordem-compra.data-emissao
                               chexcelapplication:Range("AG" + STRING(i-linha)):VALUE = IF AVAIL requisicao THEN requisicao.nr-requisicao ELSE ""
                               chexcelapplication:Range("AH" + STRING(i-linha)):VALUE = IF AVAIL requisicao THEN requisicao.dt-requisicao ELSE ?
                               chexcelapplication:Range("AI" + STRING(i-linha)):VALUE = dt-aprova-solic
                               chexcelapplication:Range("AJ" + STRING(i-linha)):VALUE = c-prioridade-ordem-compra
                               chexcelapplication:Range("AK" + STRING(i-linha)):VALUE = it-requisicao.prioridade-aprov
                               i-linha = i-linha + 1.
                    END.
                END.
            END.
        END.
        chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit.
        chworkbook:saveas(c-nome-arq,-4143,,,,,,, TRUE).
        chExcelApplication:VISIBLE = TRUE.
        RELEASE OBJECT chExcelApplication.
        RELEASE OBJECT chWorkbook.
        RELEASE OBJECT chWorksheet.
        
        RUN pi-finalizar IN h-acomp.
    
        RETURN "OK".
    END.
    ELSE DO: /* S¢ lista pedidos com contratos */
   
        FILE-INFO:FILE-NAME = "modelo/escc005c.xls".
        arquivo-at = FILE-INFO:FULL-PATHNAME.
        arquivo-cp = session:TEMP-DIR + "escc006.xls".
        OS-COMMAND SILENT COPY VALUE(arquivo-at) VALUE(arquivo-cp).
        CREATE "excel.application" chexcelapplication.
        chworkbook  = chexcelapplication:workbooks:ADD(arquivo-cp). /*substituir por "arquivo" */
        chworksheet = chexcelapplication:sheets:ITEM(1).
        chexcelapplication:VISIBLE = FALSE.
        chexcelapplication:DisplayAlerts = FALSE.

        FOR EACH contrato-for NO-LOCK
            WHERE contrato-for.nr-contrato >= tt-param.nr-contrato-ini
              AND contrato-for.nr-contrato <= tt-param.nr-contrato-fim
              AND contrato-for.dt-contrato >= tt-param.dt-emissao-ini
              AND contrato-for.dt-contrato <= tt-param.dt-emissao-fim,
              EACH pedido-compr NO-LOCK
                WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato,
            EACH ordem-compra
                  WHERE ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK,
            EACH mgemsesp.pedido-contrato
                  WHERE pedido-contrato.num-pedido   = pedido-compr.num-pedido 
                    AND pedido-contrato.numero-ordem = ordem-compra.numero-ordem 
                    AND pedido-contrato.data-public >= tt-param.dt-publicacao-ini
                    AND pedido-contrato.data-public <= tt-param.dt-publicacao-fim NO-LOCK:
            
            RUN pi-acompanhar IN h-acomp (INPUT "Nr Contrato: " + STRING(contrato-for.nr-contrato)).
                  
            ASSIGN c-ped-comlink = pedido-contrato.ped-comlink
                   d-data-public = pedido-contrato.data-public
                   c-prioridade-ordem-compra = "".
                  
            FIND ITEM                                                          
                WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN                                                 
                ASSIGN c-desc-item = ITEM.desc-item.
            ELSE                                                               
                ASSIGN c-desc-item = "".
                       
            /* Prioridade Aprova‡Æo Ordem Compra */
            IF ordem-compra.prioridade-aprov = 150 THEN
                ASSIGN c-prioridade-ordem-compra = "BAIXA".
                
            IF ordem-compra.prioridade-aprov = 450 THEN 
                ASSIGN c-prioridade-ordem-compra = "MDIA".
            
            IF ordem-compra.prioridade-aprov = 750 THEN
                ASSIGN c-prioridade-ordem-compra = "ALTA".
                
            IF ordem-compra.prioridade-aprov = 999 THEN
                ASSIGN c-prioridade-ordem-compra = "MUITO ALTA".

            FIND requisicao NO-LOCK
                WHERE requisicao.nr-requisicao = ordem-compra.nr-requisicao NO-ERROR.

            FOR EACH it-requisicao NO-LOCK
                WHERE it-requisicao.nr-requisicao = requisicao.nr-requisicao:

                FOR EACH prazo-compra
                    WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:
                    
                    ASSIGN  chexcelapplication:Range("A" + STRING(i-linha)):VALUE  = contrato-for.nr-contrato
                            chexcelapplication:Range("B" + STRING(i-linha)):VALUE  = contrato-for.des-contrat
                            chexcelapplication:Range("C" + STRING(i-linha)):VALUE  = contrato-for.dt-contrato
                            chexcelapplication:Range("D" + STRING(i-linha)):VALUE  = ordem-compra.cod-estabel
                            chexcelapplication:Range("E" + STRING(i-linha)):VALUE  = pedido-compr.nr-contrato
                            chexcelapplication:Range("F" + STRING(i-linha)):VALUE  = ordem-compra.numero-ordem
                            chexcelapplication:Range("G" + STRING(i-linha)):VALUE  = ordem-compra.it-codigo 
                            chexcelapplication:Range("H" + STRING(i-linha)):VALUE  = c-desc-item
                            chexcelapplication:Range("I" + STRING(i-linha)):VALUE  = contrato-for.dt-contrato
                            chexcelapplication:Range("J" + STRING(i-linha)):VALUE  = prazo-compra.quantidade
                            chexcelapplication:Range("K" + STRING(i-linha)):VALUE  = ordem-compra.preco-unit
                            chexcelapplication:Range("L" + STRING(i-linha)):VALUE  = prazo-compra.quantidade * ordem-compra.preco-unit
                            chexcelapplication:Range("M" + STRING(i-linha)):VALUE  = c-ped-comlink
                            chexcelapplication:Range("N" + STRING(i-linha)):NUMBERFORMAT  = "@"
                            chexcelapplication:Range("N" + STRING(i-linha)):VALUE  = STRING(d-data-public,"99/99/9999")
                            chexcelapplication:Range("O" + STRING(i-linha)):VALUE = c-prioridade-ordem-compra
                            chexcelapplication:Range("P" + STRING(i-linha)):VALUE = it-requisicao.prioridade-aprov
                            i-linha = i-linha + 1.
                END.
            END.
        END.
        chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit.
        chworkbook:saveas(c-nome-arq,-4143,,,,,,, TRUE).
        chExcelApplication:VISIBLE = TRUE.
        RELEASE OBJECT chExcelApplication.
        RELEASE OBJECT chWorkbook.
        RELEASE OBJECT chWorksheet.
        
        RUN pi-finalizar IN h-acomp.
    
        RETURN "OK". 
    END.
END.

PROCEDURE pi-carrega-tt-digita:
    FOR EACH tt-digita NO-LOCK:
        FOR EACH pedido-compr
            WHERE pedido-compr.cod-estabel  >= tt-param.cod-estabel-ini
              AND pedido-compr.cod-estabel  <= tt-param.cod-estabel-fim
              AND pedido-compr.data-pedido  >= tt-param.data-pedido-ini
              AND pedido-compr.data-pedido  <= tt-param.data-pedido-fim
              AND pedido-compr.cod-emitente >= tt-param.cod-emitente-ini
              AND pedido-compr.cod-emitente <= tt-param.cod-emitente-fim NO-LOCK:

            IF pedido-compr.nr-contrato <> 0 THEN NEXT.
        
            RUN pi-acompanhar IN h-acomp (INPUT "Num Pedido: " + STRING(pedido-compr.num-pedido)).
    
            ASSIGN c-sit-pedido = "Sem Aprova‡Æo".

            FIND LAST doc-pend-aprov USE-INDEX doc-pend-aprov-2 
                WHERE doc-pend-aprov.ind-tip-doc  = 4
                  AND doc-pend-aprov.num-pedido   = pedido-compr.num-pedido NO-LOCK NO-ERROR.
            IF AVAIL doc-pend-aprov THEN
                ASSIGN c-sit-pedido = {ininc/i02in631.i 04 doc-pend-aprov.ind-situacao}.

            FIND emitente
                WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
            IF AVAIL emitente THEN
                ASSIGN c-nome-abrev = emitente.nome-abrev.
            ELSE
                ASSIGN c-nome-abrev = "".
    
            ASSIGN c-natureza = {ininc/i01in295.i 04 pedido-compr.natureza}.
        
            FOR EACH ordem-compra
                WHERE ordem-compra.num-pedido  = pedido-compr.num-pedido
                AND   ordem-compra.it-codigo    >= tt-param.it-codigo-ini
                AND   ordem-compra.it-codigo    <= tt-param.it-codigo-fim
                AND   ordem-compra.cod-comprado >= tt-param.cod-comprado-ini
                AND   ordem-compra.cod-comprado <= tt-param.cod-comprado-fim NO-LOCK:                                
                    
                FIND requisicao NO-LOCK
                    WHERE requisicao.nr-requisicao = ordem-compra.nr-requisicao NO-ERROR.
                
                ASSIGN dt-aprova-solic           = DATE("")
                       c-prioridade-ordem-compra = "".
                
                FOR EACH doc-pend-aprov NO-LOCK
                    WHERE doc-pend-aprov.ind-tip-doc  = 1
                      AND doc-pend-aprov.nr-requisicao = ordem-compra.nr-requisicao 
                    BREAK BY doc-pend-aprov.nr-requisicao:
                    
                    IF LAST-OF(doc-pend-aprov.nr-requisicao) THEN
                        ASSIGN dt-aprova-solic = doc-pend-aprov.dt-aprova.
                END.               
                      
                FIND FIRST cotacao-item
                    WHERE cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      AND cotacao-item.it-codigo    = ordem-compra.it-codigo
                      AND cotacao-item.cod-emitente = ordem-compra.cod-emitente 
                      AND cotacao-item.cot-aprovada = YES NO-LOCK NO-ERROR.
                IF AVAIL cotacao-item THEN
                    ASSIGN d-pre-unit-for = cotacao-item.pre-unit-for
                           i-mo-codigo    = cotacao-item.mo-codigo
                           i-cod-cond-pag = cotacao-item.cod-cond-pag.
                ELSE
                    ASSIGN d-pre-unit-for = dec("")
                           i-mo-codigo    = int("")
                           i-cod-cond-pag = int("").
        
                FIND ITEM
                    WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
                IF AVAIL ITEM THEN
                    ASSIGN c-desc-item      = ITEM.desc-item
                           c-un-item        = ITEM.un
                           c-fm-codigo = ITEM.fm-codigo.
                ELSE 
                    ASSIGN c-desc-item      = ""
                           c-un-item        = ""
                           c-fm-codigo = "".
                
                FIND contrato-for
                    WHERE contrato-for.nr-contrato  = ordem-compra.nr-contrato NO-LOCK NO-ERROR.
                IF AVAIL contrato-for THEN
                    ASSIGN d-data-contrato = contrato-for.dt-contrato.
                ELSE
                    ASSIGN d-data-contrato = DATE("").
        
                FIND cond-pagto
                    WHERE cond-pagto.cod-cond-pag = ordem-compra.cod-cond-pag NO-LOCK NO-ERROR.
                IF AVAIL cond-pagto THEN
                    ASSIGN c-cond-pagto-desc = cond-pagto.descricao.
                ELSE
                    ASSIGN c-cond-pagto-desc = "".
        
                ASSIGN c-situacao = {ininc/i02in274.i 04 ordem-compra.situacao}.

                /* Prioridade Aprova‡Æo Ordem Compra */
                IF ordem-compra.prioridade-aprov = 150 THEN
                    ASSIGN c-prioridade-ordem-compra = "BAIXA".
                    
                IF ordem-compra.prioridade-aprov = 450 THEN 
                    ASSIGN c-prioridade-ordem-compra = "MDIA".
                
                IF ordem-compra.prioridade-aprov = 750 THEN
                    ASSIGN c-prioridade-ordem-compra = "ALTA".
                    
                IF ordem-compra.prioridade-aprov = 999 THEN
                    ASSIGN c-prioridade-ordem-compra = "MUITO ALTA".
                
                FOR EACH it-requisicao NO-LOCK
                    WHERE it-requisicao.nr-requisicao = requisicao.nr-requisicao:
        
                    FOR EACH prazo-compra
                        WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:
                      
                       ASSIGN  chexcelapplication:Range("A" + STRING(i-linha)):VALUE  = pedido-compr.num-pedido
                               chexcelapplication:Range("B" + STRING(i-linha)):VALUE  = pedido-compr.data-pedido
                               chexcelapplication:Range("C" + STRING(i-linha)):VALUE  = c-natureza
                               chexcelapplication:Range("D" + STRING(i-linha)):VALUE  = pedido-compr.emergencial
                               chexcelapplication:Range("E" + STRING(i-linha)):VALUE  = pedido-compr.responsavel
                               chexcelapplication:Range("F" + STRING(i-linha)):VALUE  = ordem-compra.cod-comprado
                               chexcelapplication:Range("G" + STRING(i-linha)):VALUE  = ordem-compra.nr-contrato
                               chexcelapplication:Range("H" + STRING(i-linha)):NUMBERFORMAT  = "@"
                               chexcelapplication:Range("H" + STRING(i-linha)):VALUE  = STRING(d-data-contrato,"99/99/9999")
                               chexcelapplication:Range("I" + STRING(i-linha)):VALUE  = pedido-compr.cod-emitente
                               chexcelapplication:Range("J" + STRING(i-linha)):VALUE  = c-nome-abrev
                               chexcelapplication:Range("K" + STRING(i-linha)):VALUE  = i-cod-cond-pag
                               chexcelapplication:Range("L" + STRING(i-linha)):VALUE  = c-cond-pagto-desc
                               chexcelapplication:Range("M" + STRING(i-linha)):VALUE  = ordem-compra.numero-ordem
                               chexcelapplication:Range("N" + STRING(i-linha)):VALUE  = ordem-compra.num-ord-inv
                               chexcelapplication:Range("O" + STRING(i-linha)):VALUE  = ordem-compra.ordem-servic
                               chexcelapplication:Range("P" + STRING(i-linha)):VALUE  = ordem-compra.conta-contabil
                               chexcelapplication:Range("Q" + STRING(i-linha)):VALUE  = pedido-compr.cod-estabel
                               chexcelapplication:Range("R" + STRING(i-linha)):VALUE  = ordem-compra.it-codigo
                               chexcelapplication:Range("S" + STRING(i-linha)):VALUE  = c-desc-item
                               chexcelapplication:Range("T" + STRING(i-linha)):VALUE  = c-un-item
                               chexcelapplication:Range("U" + STRING(i-linha)):VALUE  = c-fm-codigo
                               chexcelapplication:Range("V" + STRING(i-linha)):VALUE  = d-pre-unit-for
                               chexcelapplication:Range("W" + STRING(i-linha)):VALUE  = i-mo-codigo                       
                               chexcelapplication:Range("X" + STRING(i-linha)):VALUE  = c-situacao
                               chexcelapplication:Range("Y" + STRING(i-linha)):VALUE  = prazo-compra.parcela
                               chexcelapplication:Range("Z" + STRING(i-linha)):NUMBERFORMAT  = "@"
                               chexcelapplication:Range("Z" + STRING(i-linha)):VALUE  = STRING(prazo-compra.data-entrega,"99/99/9999")
                               chexcelapplication:Range("AA" + STRING(i-linha)):VALUE = prazo-compra.quantidade
                               chexcelapplication:Range("AB" + STRING(i-linha)):VALUE = prazo-compra.quant-receb
                               chexcelapplication:Range("AC" + STRING(i-linha)):VALUE = prazo-compra.quant-saldo
                               chexcelapplication:Range("AD" + STRING(i-linha)):VALUE = ordem-compra.pre-unit-for
                               chexcelapplication:Range("AE" + STRING(i-linha)):VALUE = c-sit-pedido
                               chexcelapplication:Range("AF" + STRING(i-linha)):VALUE = ordem-compra.data-emissao
                               chexcelapplication:Range("AG" + STRING(i-linha)):VALUE = IF AVAIL requisicao THEN requisicao.nr-requisicao ELSE ""
                               chexcelapplication:Range("AH" + STRING(i-linha)):VALUE = IF AVAIL requisicao THEN requisicao.dt-requisicao ELSE ?
                               chexcelapplication:Range("AI" + STRING(i-linha)):VALUE = dt-aprova-solic
                               chexcelapplication:Range("AJ" + STRING(i-linha)):VALUE = c-prioridade-ordem-compra
                               chexcelapplication:Range("AK" + STRING(i-linha)):VALUE = it-requisicao.prioridade-aprov
                               i-linha = i-linha + 1.
                    END.
                END.
            END.
        END.
    END.
    chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit.
    chworkbook:saveas(c-nome-arq,-4143,,,,,,, TRUE).
    chExcelApplication:VISIBLE = TRUE. 
    RELEASE OBJECT chExcelApplication. 
    RELEASE OBJECT chWorkbook.         
    RELEASE OBJECT chWorksheet. 
    RUN pi-finalizar IN h-acomp.
    RETURN "OK".
END PROCEDURE.
