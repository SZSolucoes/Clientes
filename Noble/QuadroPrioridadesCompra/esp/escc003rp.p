/*****************************************************************************
 ** PROGRAMA..: ESCC002RP.P
 ** OBJETIVO..: Listar Pendˆncias
 ** AUTOR.....: SZ SOLUCOES
 ** CLIENTE...: NOBLE
 ** VERSAO....: 2.06.00.001 - 27/10/2014 
 ******************************************************************************/
 /**** Defini‡äes ****/
{include/i-prgvrs.i ESCC003RP 2.06.00.001} 

DEFINE TEMP-TABLE tt-param        NO-UNDO
    FIELD destino                 AS INTEGER
    FIELD arquivo                 AS CHAR format "x(35)"
    FIELD usuario                 AS CHAR format "x(12)"
    FIELD data-exec               AS DATE
    FIELD hora-exec               AS INTEGER
    FIELD cod-estabel-ini         AS CHARACTER FORMAT "x(5)"
    FIELD cod-estabel-fim         AS CHARACTER FORMAT "x(5)"
    FIELD data-prazo-entrega-ini  AS DATE FORMAT "99/99/9999"
    FIELD data-prazo-entrega-fim  AS DATE FORMAT "99/99/9999"
    FIELD cod-comprado-ini        AS CHARACTER FORMAT "x(12)"
    FIELD cod-comprado-fim        AS CHARACTER FORMAT "x(12)"
    FIELD responsavel-ini         AS CHARACTER FORMAT "x(12)"
    FIELD responsavel-fim         AS CHARACTER FORMAT "x(12)"
    FIELD data-pedido-ini         AS DATE FORMAT "99/99/9999"
    FIELD data-pedido-fim         AS DATE FORMAT "99/99/9999".

define temp-table tt-digita no-undo
    field fornecedor like emitente.cod-emitente
    FIELD c-nome-abrev LIKE emitente.nome-abrev.

def temp-table tt-raw-digita
    field raw-digita as raw.

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

/******************** Variaveis Locais ********************/
DEFINE VARIABLE c-emitente-nome-abrev       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-situacao                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-item-desc                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nf-gerada                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dec-tot-valor               LIKE item-doc-est.preco-total NO-UNDO.
DEFINE VARIABLE dt-aprov-ped                AS DATE        NO-UNDO.
DEFINE VARIABLE dt-validade-ini             LIKE contrato-for.dt-ini-validade NO-UNDO.
DEFINE VARIABLE dt-validade-fim             LIKE contrato-for.dt-ter-validade NO-UNDO.
DEFINE VARIABLE d-qtde-recebimento          LIKE item-doc-est.quantidade      NO-UNDO.
DEFINE VARIABLE d-preco-unit                LIKE item-doc-est.preco-unit      NO-UNDO.
DEFINE VARIABLE de-valor-ipi                LIKE ordem-compra.preco-unit      NO-UNDO.
DEFINE VARIABLE de-preco                    LIKE ordem-compra.preco-unit      NO-UNDO.
DEFINE VARIABLE c-fm-codigo                 LIKE ITEM.fm-codigo               NO-UNDO.
DEFINE VARIABLE c-prioridade-ordem-compra   AS CHARACTER   NO-UNDO.
/**********************************************************/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Gerando Dados Excel").
ASSIGN c-nome-arq = SESSION:TEMP-DIRECTORY + "escc003-" + STRING(TODAY, "999999") + "-" + STRING(TIME) + ".xls".

FILE-INFO:FILE-NAME = "modelo/escc003.xls".
arquivo-at = FILE-INFO:FULL-PATHNAME.
arquivo-cp = SESSION:TEMP-DIR + "escc003.xls".


OS-COMMAND SILENT COPY VALUE(arquivo-at) VALUE(arquivo-cp).
CREATE "excel.application" chexcelapplication.
chworkbook  = chexcelapplication:workbooks:ADD(arquivo-cp). /*substituir por "arquivo" */
chworksheet = chexcelapplication:sheets:ITEM(1).
chexcelapplication:VISIBLE = FALSE.
chexcelapplication:DisplayAlerts = FALSE.

ASSIGN chworksheet:range("A1:AJ1"):font:bold = TRUE.

/**** Main Block ****/
ASSIGN i-linha = 2.

FIND FIRST tt-digita NO-LOCK NO-ERROR.
IF AVAIL tt-digita THEN
    RUN pi-carrega-tt-digita.
ELSE DO:
       
    FOR EACH pedido-compr
        WHERE pedido-compr.num-pedido   >= 0
          AND pedido-compr.num-pedido   <= 999999999
          AND pedido-compr.data-pedido >= tt-param.data-pedido-ini
          AND pedido-compr.data-pedido <= tt-param.data-pedido-fim 
          AND pedido-compr.cod-estabel >= tt-param.cod-estabel-ini
          AND pedido-compr.cod-estabel <= tt-param.cod-estabel-fim
          AND pedido-compr.responsavel >= tt-param.responsavel-ini
          AND pedido-compr.responsavel <= tt-param.responsavel-fim NO-LOCK:
    
        RUN pi-acompanhar IN h-acomp (INPUT "Num Pedido: " + STRING(pedido-compr.num-pedido)).

        FIND emitente
             WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN
            ASSIGN c-emitente-nome-abrev = emitente.nome-abrev.
        ELSE
            ASSIGN c-emitente-nome-abrev = "".

        FIND contrato-for 
            WHERE contrato-for.nr-contrato = pedido-compr.nr-contrato NO-LOCK NO-ERROR.
        IF AVAIL contrato-for THEN
            ASSIGN dt-validade-ini = contrato-for.dt-ini-validade
                   dt-validade-fim = contrato-for.dt-ter-validade.
        ELSE 
            ASSIGN dt-validade-ini = ?
                   dt-validade-fim = ?.

        ASSIGN dt-aprov-ped = DATE("")
               c-prioridade-ordem-compra = "".

        /*FOR EACH doc-pend-aprov NO-LOCK
            WHERE doc-pend-aprov.num-pedido = pedido-compr.num-pedido 
            BREAK BY doc-pend-aprov.num-pedido:
            
            IF last-of(doc-pend-aprov.num-pedido) THEN
                ASSIGN dt-aprov-ped = doc-pend-aprov.dt-aprova.
        END.*/

        FIND LAST doc-pend-aprov USE-INDEX doc-pend-aprov-2 
            WHERE doc-pend-aprov.ind-tip-doc  = 4
              AND doc-pend-aprov.num-pedido   = pedido-compr.num-pedido NO-LOCK NO-ERROR.
        IF AVAIL doc-pend-aprov THEN
            ASSIGN dt-aprov-ped = doc-pend-aprov.dt-aprova.
    
        FOR EACH ordem-compra USE-INDEX pedido
            WHERE ordem-compra.num-pedido    = pedido-compr.num-pedido
              AND ordem-compra.situacao  <> 4
            AND ordem-compra.cod-comprado >= tt-param.cod-comprado-ini
              AND ordem-compra.cod-comprado <= tt-param.cod-comprado-fim NO-LOCK:

            ASSIGN c-situacao = {ininc/i02in274.i 04 ordem-compra.situacao}
                   de-valor-ipi = (ordem-compra.preco-unit * ordem-compra.aliquota-ipi) / (100 + ordem-compra.aliquota-ipi)
                   de-preco     =  ordem-compra.preco-unit - de-valor-ipi.
            
            FIND ITEM
                WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
                ASSIGN c-item-desc = ITEM.desc-item
                       c-fm-codigo = ITEM.fm-codigo.
            ELSE
                ASSIGN c-item-desc = ""
                       c-fm-codigo = "".

            FIND requisicao NO-LOCK
                 WHERE requisicao.nr-requisicao = ordem-compra.nr-requisicao NO-ERROR.

            IF ordem-compra.prioridade-aprov = 1 THEN
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "BAIXA".
                
            IF ordem-compra.prioridade-aprov = 2 THEN 
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "MDIA".
           
            IF ordem-compra.prioridade-aprov = 3 THEN
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "ALTA".
                
            IF (ordem-compra.prioridade-aprov = 4 OR
                ordem-compra.prioridade-aprov = 0) OR 
                ordem-compra.prioridade-aprov = ? THEN
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "MUITO ALTA".

            /* Prioridade Aprova‡Æo Ordem Compra */
            IF ordem-compra.prioridade-aprov = 1 THEN
                ASSIGN c-prioridade-ordem-compra = "BAIXA".
                
            IF ordem-compra.prioridade-aprov = 2 THEN 
                ASSIGN c-prioridade-ordem-compra = "MDIA".
            
            IF ordem-compra.prioridade-aprov = 3 THEN
                ASSIGN c-prioridade-ordem-compra = "ALTA".
                
            IF ordem-compra.prioridade-aprov = 4 THEN
                ASSIGN c-prioridade-ordem-compra = "MUITO ALTA".

            FOR EACH it-requisicao NO-LOCK
                WHERE it-requisicao.nr-requisicao = requisicao.nr-requisicao:
          
                FOR EACH prazo-compra USE-INDEX ordem-sit
                    WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem
                      AND prazo-compra.data-entrega >= tt-param.data-prazo-entrega-ini
                      AND prazo-compra.data-entrega <= tt-param.data-prazo-entrega-fim
                      AND prazo-compra.situacao     <> 4 NO-LOCK:
                
                    IF prazo-compra.quant-saldo <= 0 THEN NEXT.

                    IF it-requisicao.prioridade-aprov = 150 THEN
                        ASSIGN wh-it-prioridade:SCREEN-VALUE = "BAIXA".
                        
                    IF it-requisicao.prioridade-aprov = 450 THEN 
                        ASSIGN wh-it-prioridade:SCREEN-VALUE = "MDIA".
                   
                    IF it-requisicao.prioridade-aprov = 750 THEN
                        ASSIGN wh-it-prioridade:SCREEN-VALUE = "ALTA".
                        
                    IF it-requisicao.prioridade-aprov = 999 THEN
                        ASSIGN wh-it-prioridade:SCREEN-VALUE = "MUITO ALTA".
                
                    ASSIGN d-qtde-recebimento = 0
                           d-preco-unit[1]    = 0
                           c-nf-gerada = "NAO".
                    FOR EACH  item-doc-est USE-INDEX itmdctst-09
                        WHERE item-doc-est.num-pedido   = pedido-compr.num-pedido
                          AND item-doc-est.numero-ordem = ordem-compra.numero-ordem 
                          AND item-doc-est.parcela      = prazo-compra.parcela NO-LOCK:
                        ASSIGN d-qtde-recebimento = d-qtde-recebimento + item-doc-est.quantidade
                               d-preco-unit[1]    = item-doc-est.preco-unit[1]
                               dec-tot-valor[1]   = dec-tot-valor[1] + item-doc-est.preco-total[1]
                               c-nf-gerada = "SIM".
                    END.
                
                    ASSIGN chexcelapplication:Range("A"  + STRING(i-linha)):VALUE  = pedido-compr.cod-emitente
                           chexcelapplication:Range("B"  + STRING(i-linha)):VALUE  = c-emitente-nome-abrev
                           chexcelapplication:Range("C"  + STRING(i-linha)):VALUE  = pedido-compr.num-pedido
                           chexcelapplication:Range("D"  + STRING(i-linha)):NUMBERFORMAT = "@"
                           chexcelapplication:Range("D"  + STRING(i-linha)):VALUE  = STRING(pedido-compr.data-pedido,"99/99/9999")
                           chexcelapplication:Range("E"  + STRING(i-linha)):NUMBERFORMAT = "@"
                           chexcelapplication:Range("E"  + STRING(i-linha)):VALUE  = STRING(dt-aprov-ped,"99/99/9999") 
                           chexcelapplication:Range("F"  + STRING(i-linha)):VALUE  = pedido-compr.nr-contrato
                           chexcelapplication:Range("G"  + STRING(i-linha)):NUMBERFORMAT = "@"
                           chexcelapplication:Range("G"  + STRING(i-linha)):VALUE  = STRING(dt-validade-ini,"99/99/9999") 
                           chexcelapplication:Range("H"  + STRING(i-linha)):NUMBERFORMAT = "@"
                           chexcelapplication:Range("H"  + STRING(i-linha)):VALUE  = STRING(dt-validade-fim,"99/99/9999")
                           chexcelapplication:Range("I"  + STRING(i-linha)):VALUE  = ordem-compra.numero-ordem
                           chexcelapplication:Range("J"  + STRING(i-linha)):VALUE  = ordem-compra.conta-contabil 
                           chexcelapplication:Range("K"  + STRING(i-linha)):VALUE  = ordem-compra.num-ord-inv
                           chexcelapplication:Range("L"  + STRING(i-linha)):VALUE  = ordem-compra.ordem-servic
                           chexcelapplication:Range("M"  + STRING(i-linha)):VALUE  = pedido-compr.cod-estabel
                           chexcelapplication:Range("N"  + STRING(i-linha)):VALUE  = ordem-compra.cod-comprado
                           chexcelapplication:Range("O"  + STRING(i-linha)):VALUE  = pedido-compr.responsavel
                           chexcelapplication:Range("P"  + STRING(i-linha)):VALUE  = c-situacao
                           chexcelapplication:Range("Q"  + STRING(i-linha)):NUMBERFORMAT = "@"
                           chexcelapplication:Range("Q"  + STRING(i-linha)):VALUE  = STRING(ordem-compra.data-emissao,"99/99/9999")
                           chexcelapplication:Range("R"  + STRING(i-linha)):VALUE  = ordem-compra.it-codigo
                           chexcelapplication:Range("S"  + STRING(i-linha)):VALUE  = ordem-compra.mo-codigo
                           chexcelapplication:Range("T"  + STRING(i-linha)):VALUE  = c-item-desc
                           chexcelapplication:Range("U"  + STRING(i-linha)):VALUE  = prazo-compra.parcela
                           chexcelapplication:Range("V"  + STRING(i-linha)):VALUE  = prazo-compra.quantidade  
                           chexcelapplication:Range("W"  + STRING(i-linha)):VALUE  = de-preco
                           chexcelapplication:Range("X"  + STRING(i-linha)):VALUE  = de-valor-ipi
                           chexcelapplication:Range("Y"  + STRING(i-linha)):VALUE  = prazo-compra.quantidade  * ordem-compra.pre-unit-for
                           chexcelapplication:Range("Z"  + STRING(i-linha)):VALUE  = prazo-compra.quantidade - prazo-compra.qtd-sal-forn /*d-qtde-recebimento*/
                           chexcelapplication:Range("AA" + STRING(i-linha)):VALUE  = d-preco-unit[1]
                           chexcelapplication:Range("AB" + STRING(i-linha)):VALUE  = dec-tot-valor[1] 
                           chexcelapplication:Range("AC" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn
                           chexcelapplication:Range("AD" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn * ordem-compra.pre-unit-for
                           chexcelapplication:Range("AE" + STRING(i-linha)):VALUE  = prazo-compra.dec-1
                           chexcelapplication:Range("AF" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn
                           chexcelapplication:Range("AG" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn * ordem-compra.pre-unit-for
                           chexcelapplication:Range("AH"  + STRING(i-linha)):NUMBERFORMAT = "@"
                           chexcelapplication:Range("AH" + STRING(i-linha)):VALUE  = STRING(prazo-compra.data-entrega,"99/99/9999")
                           chexcelapplication:Range("AI" + STRING(i-linha)):VALUE  = 0
                           chexcelapplication:Range("AJ" + STRING(i-linha)):VALUE  = c-nf-gerada
                           chexcelapplication:Range("AK" + STRING(i-linha)):VALUE  = prazo-compra.dec-1
                           chexcelapplication:Range("AL" + STRING(i-linha)):VALUE  = c-fm-codigo
                           chexcelapplication:Range("AM" + STRING(i-linha)):VALUE  = ordem-compra.ct-codigo
                           chexcelapplication:Range("AN" + STRING(i-linha)):VALUE  = ordem-compra.sc-codigo
                           chexcelapplication:Range("AO" + STRING(i-linha)):VALUE  = c-prioridade-ordem-compra
                           chexcelapplication:Range("AP" + STRING(i-linha)):VALUE  = it-requisicao.prioridade-aprov
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

PROCEDURE pi-carrega-tt-digita:               
    FOR EACH tt-digita NO-LOCK:
        FOR EACH pedido-compr
            WHERE pedido-compr.num-pedido   >= 0
              AND pedido-compr.num-pedido   <= 999999999
              AND pedido-compr.cod-emitente = tt-digita.fornecedor
              AND pedido-compr.cod-estabel >= tt-param.cod-estabel-ini
              AND pedido-compr.cod-estabel <= tt-param.cod-estabel-fim
              AND pedido-compr.responsavel >= tt-param.responsavel-ini
              AND pedido-compr.responsavel <= tt-param.responsavel-fim
              AND pedido-compr.data-pedido >= tt-param.data-pedido-ini
              AND pedido-compr.data-pedido <= tt-param.data-pedido-fim NO-LOCK:
        
            RUN pi-acompanhar IN h-acomp (INPUT "Num Pedido: " + STRING(pedido-compr.num-pedido)).

        FIND emitente
             WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN
            ASSIGN c-emitente-nome-abrev = emitente.nome-abrev.
        ELSE
            ASSIGN c-emitente-nome-abrev = "".
        
        FIND contrato-for 
            WHERE contrato-for.nr-contrato = pedido-compr.nr-contrato NO-LOCK NO-ERROR.
        IF AVAIL contrato-for THEN
            ASSIGN dt-validade-ini = contrato-for.dt-ini-validade
                   dt-validade-fim = contrato-for.dt-ter-validade.
        ELSE 
            ASSIGN dt-validade-ini = ?
                   dt-validade-fim = ?.
        
        ASSIGN dt-aprov-ped = DATE("")
               c-prioridade-ordem-compra = "".
        
        /*FOR EACH doc-pend-aprov NO-LOCK
            WHERE doc-pend-aprov.num-pedido = pedido-compr.num-pedido 
            BREAK BY doc-pend-aprov.num-pedido:
            
            IF last-of(doc-pend-aprov.num-pedido) THEN
                ASSIGN dt-aprov-ped = doc-pend-aprov.dt-aprova.
        END.*/

        FIND LAST doc-pend-aprov USE-INDEX doc-pend-aprov-2 
            WHERE doc-pend-aprov.ind-tip-doc  = 4
              AND doc-pend-aprov.num-pedido   = pedido-compr.num-pedido NO-LOCK NO-ERROR.
        IF AVAIL doc-pend-aprov THEN
            ASSIGN dt-aprov-ped = doc-pend-aprov.dt-aprova.

        
            FOR EACH ordem-compra USE-INDEX pedido
                WHERE ordem-compra.num-pedido    = pedido-compr.num-pedido
                  AND ordem-compra.situacao  <> 4
                AND   ordem-compra.cod-comprado >= tt-param.cod-comprado-ini
                  AND   ordem-compra.cod-comprado <= tt-param.cod-comprado-fim NO-LOCK:
            
                ASSIGN c-situacao = {ininc/i02in274.i 04 ordem-compra.situacao}
                       de-valor-ipi = (ordem-compra.preco-unit * ordem-compra.aliquota-ipi) / (100 + ordem-compra.aliquota-ipi)
                       de-preco     =  ordem-compra.preco-unit - de-valor-ipi.
                
                FIND ITEM
                    WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
                IF AVAIL ITEM THEN
                    ASSIGN c-item-desc = ITEM.desc-item
                           c-fm-codigo = ITEM.fm-codigo.
                ELSE
                    ASSIGN c-item-desc = ""
                           c-fm-codigo = "".

                FIND requisicao NO-LOCK
                 WHERE requisicao.nr-requisicao = ordem-compra.nr-requisicao NO-ERROR.

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
              
                    FOR EACH prazo-compra USE-INDEX ordem-sit
                        WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem
                          AND prazo-compra.data-entrega >= tt-param.data-prazo-entrega-ini
                          AND prazo-compra.data-entrega <= tt-param.data-prazo-entrega-fim
                          AND prazo-compra.situacao     <> 4 NO-LOCK:
                    
                        IF prazo-compra.quant-saldo <= 0 THEN NEXT.
                    
                        ASSIGN d-qtde-recebimento = 0
                               d-preco-unit[1]    = 0.
                        FOR EACH  item-doc-est USE-INDEX itmdctst-09
                            WHERE item-doc-est.num-pedido   = pedido-compr.num-pedido
                              AND item-doc-est.numero-ordem = ordem-compra.numero-ordem 
                              AND item-doc-est.parcela      = prazo-compra.parcela NO-LOCK:
                            ASSIGN d-qtde-recebimento = d-qtde-recebimento + item-doc-est.quantidade
                                   d-preco-unit[1]    = item-doc-est.preco-unit[1]
                                   dec-tot-valor[1]   = dec-tot-valor[1] + item-doc-est.preco-total[1]
                                   c-nf-gerada = "SIM".
                        END.   
                    
                        ASSIGN chexcelapplication:Range("A"  + STRING(i-linha)):VALUE  = pedido-compr.cod-emitente
                               chexcelapplication:Range("B"  + STRING(i-linha)):VALUE  = c-emitente-nome-abrev
                               chexcelapplication:Range("C"  + STRING(i-linha)):VALUE  = pedido-compr.num-pedido
                               chexcelapplication:Range("D"  + STRING(i-linha)):NUMBERFORMAT = "@"
                               chexcelapplication:Range("D"  + STRING(i-linha)):VALUE  = STRING(pedido-compr.data-pedido,"99/99/9999")
                               chexcelapplication:Range("E"  + STRING(i-linha)):NUMBERFORMAT = "@"
                               chexcelapplication:Range("E"  + STRING(i-linha)):VALUE  = STRING(dt-aprov-ped,"99/99/9999") 
                               chexcelapplication:Range("F"  + STRING(i-linha)):VALUE  = pedido-compr.nr-contrato
                               chexcelapplication:Range("G"  + STRING(i-linha)):NUMBERFORMAT = "@"
                               chexcelapplication:Range("G"  + STRING(i-linha)):VALUE  = STRING(dt-validade-ini,"99/99/9999") 
                               chexcelapplication:Range("H"  + STRING(i-linha)):NUMBERFORMAT = "@"
                               chexcelapplication:Range("H"  + STRING(i-linha)):VALUE  = STRING(dt-validade-fim,"99/99/9999")
                               chexcelapplication:Range("I"  + STRING(i-linha)):VALUE  = ordem-compra.numero-ordem
                               chexcelapplication:Range("J"  + STRING(i-linha)):VALUE  = ordem-compra.conta-contabil 
                               chexcelapplication:Range("K"  + STRING(i-linha)):VALUE  = ordem-compra.num-ord-inv
                               chexcelapplication:Range("L"  + STRING(i-linha)):VALUE  = ordem-compra.ordem-servic
                               chexcelapplication:Range("M"  + STRING(i-linha)):VALUE  = pedido-compr.cod-estabel
                               chexcelapplication:Range("N"  + STRING(i-linha)):VALUE  = ordem-compra.cod-comprado
                               chexcelapplication:Range("O"  + STRING(i-linha)):VALUE  = pedido-compr.responsavel
                               chexcelapplication:Range("P"  + STRING(i-linha)):VALUE  = c-situacao
                               chexcelapplication:Range("Q"  + STRING(i-linha)):NUMBERFORMAT = "@"
                               chexcelapplication:Range("Q"  + STRING(i-linha)):VALUE  = STRING(ordem-compra.data-emissao,"99/99/9999")
                               chexcelapplication:Range("R"  + STRING(i-linha)):VALUE  = ordem-compra.it-codigo
                               chexcelapplication:Range("S"  + STRING(i-linha)):VALUE  = ordem-compra.mo-codigo
                               chexcelapplication:Range("T"  + STRING(i-linha)):VALUE  = c-item-desc
                               chexcelapplication:Range("U"  + STRING(i-linha)):VALUE  = prazo-compra.parcela
                               chexcelapplication:Range("V"  + STRING(i-linha)):VALUE  = prazo-compra.quantidade  
                               chexcelapplication:Range("W"  + STRING(i-linha)):VALUE  = de-preco
                               chexcelapplication:Range("X"  + STRING(i-linha)):VALUE  = de-valor-ipi
                               chexcelapplication:Range("Y"  + STRING(i-linha)):VALUE  = prazo-compra.quantidade  * ordem-compra.pre-unit-for
                               chexcelapplication:Range("Z"  + STRING(i-linha)):VALUE  = prazo-compra.quantidade - prazo-compra.qtd-sal-forn /*d-qtde-recebimento*/
                               chexcelapplication:Range("AA" + STRING(i-linha)):VALUE  = d-preco-unit[1]
                               chexcelapplication:Range("AB" + STRING(i-linha)):VALUE  = dec-tot-valor[1] 
                               chexcelapplication:Range("AC" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn
                               chexcelapplication:Range("AD" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn * ordem-compra.pre-unit-for
                               chexcelapplication:Range("AE" + STRING(i-linha)):VALUE  = prazo-compra.DEC-1
                               chexcelapplication:Range("AF" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn
                               chexcelapplication:Range("AG" + STRING(i-linha)):VALUE  = prazo-compra.qtd-sal-forn * ordem-compra.pre-unit-for
                               chexcelapplication:Range("AH"  + STRING(i-linha)):NUMBERFORMAT = "@"
                               chexcelapplication:Range("AH" + STRING(i-linha)):VALUE  = STRING(prazo-compra.data-entrega,"99/99/9999")
                               chexcelapplication:Range("AI" + STRING(i-linha)):VALUE  = 0
                               chexcelapplication:Range("AJ" + STRING(i-linha)):VALUE  = c-nf-gerada
                               chexcelapplication:Range("AK" + STRING(i-linha)):VALUE  = prazo-compra.dec-1
                               chexcelapplication:Range("AL" + STRING(i-linha)):VALUE  = c-fm-codigo
                               chexcelapplication:Range("AM" + STRING(i-linha)):VALUE  = ordem-compra.ct-codigo
                               chexcelapplication:Range("AN" + STRING(i-linha)):VALUE  = ordem-compra.sc-codigo
                               chexcelapplication:Range("AO" + STRING(i-linha)):VALUE  = c-prioridade-ordem-compra
                               chexcelapplication:Range("AP" + STRING(i-linha)):VALUE  = it-requisicao.prioridade-aprov
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




