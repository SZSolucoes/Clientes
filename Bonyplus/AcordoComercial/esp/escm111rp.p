/*****************************************************************************
**       Programa: ESCM111RP.p
**       Data....: 13/12/2016
**       Objetivo: Relatorio Acordo Comercial 
**       Vers’o..: 2.12.00.001 
*******************************************************************************/

{include/i-prgvrs.i ESCM111RP 2.12.00.001}  /*** 010001 ***/

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino               AS INTEGER
    FIELD arquivo               AS CHAR FORMAT "x(35)"
    FIELD usuario               AS CHAR FORMAT "x(12)"
    FIELD data-exec             AS DATE
    FIELD hora-exec             AS INTEGER
    FIELD classifica            AS INTEGER
    FIELD desc-classifica       AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf            AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf         AS LOG
    FIELD estab-ini             AS CHAR
    FIELD estab-fim             AS CHAR
    FIELD cod-area-ini          AS INT
    FIELD cod-area-fim          AS INT
    FIELD cod-repres-ini        AS INT
    FIELD cod-repres-fim        AS INT
    FIELD cod-emitente-ini      AS INT
    FIELD cod-emitente-fim      AS INT
    FIELD nr-acordo-comerc-ini  AS CHAR
    FIELD nr-acordo-comerc-fim  AS CHAR
    FIELD dt-criacao-ini        AS DATE
    FIELD dt-criacao-fim        AS DATE
    FIELD sit-acordo            AS INT.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEFINE VARIABLE h-acomp             AS HANDLE  NO-UNDO.

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

DEFINE VARIABLE chexcelapplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE arquivo-at         AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-cp         AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-nv         AS CHAR NO-UNDO.
DEFINE VARIABLE i-linha            AS INT  NO-UNDO.
DEFINE VARIABLE c-desc-sit         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLinha             AS INTEGER.
DEFINE VARIABLE c-nome-abrev       AS CHAR NO-UNDO.
DEFINE VARIABLE c-sit-acordo-app   AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-val_sdo_tit_ap   AS INTEGER FORMAT ">>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE i-val_pago_tit_ap  AS INTEGER FORMAT ">>>,>>>,>>9.99" NO-UNDO.

/* Propriedade : HorizontalAlignment (Alinhamento Horizontal) ******************/
&global-define xlHAlignCenter       -4108  /* 01 - Centralizado */
&global-define xlHAlignLeft         -4131  /* 04 - Esquerda */

/* Propriedade : LineStyle  (Estilo de Linha) **********************************/
&global-define xlContinuous     1       /* 01 - Continua */ 

{include/i-rpvar.i}
{cdp/cdcfgmat.i}
{utp/ut-glob.i}

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

ASSIGN c-programa = "ESCM111RP"
       c-versao   = "2.12"
       c-revisao  = "001".

{utp/ut-liter.i Relatorio_Acordo_Comercial * r}
ASSIGN c-titulo-relat = RETURN-VALUE.

{utp/ut-liter.i Acordo * r}
ASSIGN c-sistema = RETURN-VALUE.

{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Acordo_Comercial *}

CREATE "Excel.Application" chexcelapplication.
chexcelapplication:VISIBLE = YES.
chexcelapplication:displayalerts = NO.
chexcelapplication:Workbooks:OPEN(SEARCH("layout/escm111.xls")).

RUN pi-inicializar IN h-acomp (INPUT  RETURN-VALUE ).

ASSIGN ilinha = 2.

FOR EACH es-acordo-comerc  NO-LOCK                                                     
    WHERE es-acordo-comerc.cod-estab         >= tt-param.estab-ini                              
      AND es-acordo-comerc.cod-estab         <= tt-param.estab-fim                              
      AND es-acordo-comerc.cod-area          >= tt-param.cod-area-ini                          
      AND es-acordo-comerc.cod-area          <= tt-param.cod-area-fim                          
      AND es-acordo-comerc.cod-repres        >= tt-param.cod-repres-ini                         
      AND es-acordo-comerc.cod-repres        <= tt-param.cod-repres-fim                         
      AND es-acordo-comerc.cod-emitente      >= tt-param.cod-emitente-ini                       
      AND es-acordo-comerc.cod-emitente      <= tt-param.cod-emitente-fim
      AND es-acordo-comerc.nr-acordo-comerc  >= tt-param.nr-acordo-comerc-ini                       
      AND es-acordo-comerc.nr-acordo-comerc  <= tt-param.nr-acordo-comerc-fim
      AND es-acordo-comerc.dt-criacao        >= tt-param.dt-criacao-ini                       
      AND es-acordo-comerc.dt-criacao        <= tt-param.dt-criacao-fim
    BREAK BY es-acordo-comerc.nr-acordo-comerc:

    RUN pi-acompanhar IN h-acomp (INPUT es-acordo-comerc.nr-acordo-comerc). 

    FIND repres NO-LOCK
        WHERE repres.cod-rep = es-acordo-comerc.cod-repres NO-ERROR.
    
    FIND emitente NO-LOCK                                                          
        WHERE emitente.cod-emitente = es-acordo-comer.cod-emitente NO-ERROR. 
    
    FIND es-acordo-area NO-LOCK
        WHERE es-acordo-area.cod-area = es-acordo-comerc.cod-area NO-ERROR.

    /* Verifica se o Acordo j  deu entrada no RE1001 */
    FIND FIRST es-tit_ap NO-LOCK
        WHERE es-tit_ap.cdn_fornecedor     = es-acordo-comerc.cod-emitente
          AND es-tit_ap.nr-acordo-comerc   = es-acordo-comerc.nr-acordo-comerc NO-ERROR.

    ASSIGN i-val_sdo_tit_ap = 0
           i-val_pago_tit_ap = 0.

    FOR EACH es-tit_ap NO-LOCK
        WHERE es-tit_ap.cdn_fornecedor     = es-acordo-comerc.cod-emitente
          AND es-tit_ap.nr-acordo-comerc   = es-acordo-comerc.nr-acordo-comerc:

        FIND tit_ap NO-LOCK
            WHERE tit_ap.cod_estab       = es-tit_ap.cod_estab      
              AND tit_ap.cdn_fornecedor  = es-tit_ap.cdn_fornecedor 
              AND tit_ap.cod_espec_docto = es-tit_ap.cod_espec_docto
              AND tit_ap.cod_ser_docto   = es-tit_ap.cod_ser_docto  
              AND tit_ap.cod_tit_ap      = es-tit_ap.cod_tit_ap     
              AND tit_ap.cod_parcela     = es-tit_ap.cod_parcela NO-ERROR. 

        IF AVAIL tit_ap THEN DO:
            /* Valor em Aberto*/
            ASSIGN i-val_sdo_tit_ap = i-val_sdo_tit_ap + tit_ap.val_sdo_tit_ap.
            /* Valor Pago */
            ASSIGN i-val_pago_tit_ap = i-val_pago_tit_ap + (tit_ap.val_origin_tit_ap - tit_ap.val_sdo_tit_ap).

        END.
    END.

    RUN pi-verifica-situacao.
    
    IF tt-param.sit-acordo = 4 THEN  /* Todos */

        RUN pi-imprime-excel.
    

    IF tt-param.sit-acordo = 3
        AND c-sit-acordo-app = "Reprovado Acordo" THEN /* Reprovado Acordo */
        
        RUN pi-imprime-excel.

    IF tt-param.sit-acordo = 2 
        AND c-sit-acordo-app = "Aprovado Acordo" THEN  /* Aprovado Acordo */
        
        RUN pi-imprime-excel.

    IF tt-param.sit-acordo = 1 
        AND c-sit-acordo-app = "Pendente Acordo" THEN  /* Pendente Acordo */
        
        RUN pi-imprime-excel.
    
    
END.

PROCEDURE pi-verifica-situacao:

    FIND FIRST es-acordo-pendencia NO-LOCK
        WHERE es-acordo-pendencia.nr-acordo-comerc = es-acordo-comerc.nr-acordo-comerc
          AND es-acordo-pendencia.ind-situacao = 2 NO-ERROR.

    IF AVAIL es-acordo-pendencia THEN
         ASSIGN c-sit-acordo-app = "Reprovado Acordo".

    ELSE DO:
        FIND FIRST es-acordo-pendencia NO-LOCK
            WHERE es-acordo-pendencia.nr-acordo-comerc = es-acordo-comerc.nr-acordo-comerc
              AND es-acordo-pendencia.ind-situacao = 0 NO-ERROR.

        IF AVAIL es-acordo-pendencia THEN
             ASSIGN c-sit-acordo-app = "Pendente Acordo".

        ELSE DO:
            FIND FIRST es-acordo-pendencia NO-LOCK
                WHERE es-acordo-pendencia.nr-acordo-comerc = es-acordo-comerc.nr-acordo-comerc
                  AND es-acordo-pendencia.ind-situacao = 1 NO-ERROR.

            IF AVAIL es-acordo-pendencia THEN
                 ASSIGN c-sit-acordo-app = "Aprovado Acordo".
            
            ELSE
                ASSIGN c-sit-acordo-app = "".

        END.

    END.

END PROCEDURE.

PROCEDURE pi-imprime-excel:

    ASSIGN chexcelapplication:range("A" + STRING(iLinha)):VALUE = es-acordo-comerc.cod-estab
           chexcelapplication:range("B" + STRING(iLinha)):VALUE = IF AVAIL es-acordo-area THEN es-acordo-area.descricao ELSE ""
           chexcelapplication:range("C" + STRING(iLinha)):VALUE = IF AVAIL repres THEN repres.nome-abrev ELSE ""      
           chexcelapplication:range("D" + STRING(iLinha)):VALUE = IF AVAIL emitente THEN emitente.nome-abrev ELSE ""  
           chexcelapplication:range("E" + STRING(iLinha)):VALUE = es-acordo-comerc.nr-acordo-comerc
           chexcelapplication:range("F" + STRING(iLinha)):VALUE = STRING(es-acordo-comerc.dt-criacao,"99/99/9999")
           chexcelapplication:range("G" + STRING(iLinha)):VALUE = STRING(es-acordo-comerc.dt-ini-period,"99/99/9999")
           chexcelapplication:range("H" + STRING(iLinha)):VALUE = STRING(es-acordo-comerc.dt-fim-period,"99/99/9999")
           chexcelapplication:range("I" + STRING(iLinha)):VALUE = es-acordo-comerc.vl-acordo
           chexcelapplication:range("J" + STRING(iLinha)):VALUE = i-val_pago_tit_ap
           chexcelapplication:range("K" + STRING(iLinha)):VALUE = i-val_sdo_tit_ap
           chexcelapplication:range("L" + STRING(iLinha)):VALUE = c-sit-acordo-app
           chexcelapplication:range("M" + STRING(iLinha)):VALUE = STRING(es-acordo-pendencia.dt-aprov,"99/99/9999")
           chexcelapplication:range("N" + STRING(iLinha)):VALUE = IF AVAIL es-tit_ap THEN "Sim" ELSE "NÆo"
           chexcelapplication:range("O" + STRING(iLinha)):VALUE = ""
           chexcelapplication:range("P" + STRING(iLinha)):VALUE = es-acordo-pendencia.cod-aprovador
           iLinha = iLinha + 1.

END PROCEDURE.

chexcelapplication:ActiveSheet:Cells:Select. /* Deixar a coluna com o tamanho da celula*/
chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit. /* Deixar a coluna com o tamanho da celula*/

RUN pi-finalizar IN h-acomp.         

chExcelApplication:VISIBLE = TRUE.
RELEASE OBJECT chExcelApplication.      

RETURN "OK":U. 

