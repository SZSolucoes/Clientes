/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF005RP 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**
**       Programa: ESNF005RP
**
**       Data....: Setembro 2008
**
**       Objetivo: Relatorio de Inconsistàncias
**
**       Versao..: 1.00.000
**
*******************************************************************************/
{cdp/cdcfgmat.i}

&GLOBAL-DEFINE RTF NO

/** DEFINIÄ«O TEMP-TABLES **/
    DEFINE TEMP-TABLE tt-param NO-UNDO
        FIELD destino          AS INTEGER
        FIELD arquivo          AS CHAR FORMAT "x(35)":U
        FIELD arquivo-excel    AS CHAR FORMAT "x(35)":U
        FIELD usuario          AS CHAR FORMAT "x(12)":U
        FIELD data-exec        AS DATE
        FIELD hora-exec        AS INTEGER
        FIELD classifica       AS INTEGER
        FIELD desc-classifica  AS CHAR FORMAT "x(40)":U
        FIELD modelo           AS CHAR FORMAT "x(35)":U
        FIELD l-habilitaRtf    AS LOG
        FIELD cod-emitente-ini LIKE emitente.cod-emitente
        FIELD cod-emitente-fim LIKE emitente.cod-emitente
        FIELD nome-abrev-ini   LIKE emitente.nome-abrev
        FIELD nome-abrev-fim   LIKE emitente.nome-abrev
        FIELD tg-parametros    AS LOG .

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD cod-emitente LIKE emitente.cod-emitente 
    FIELD nome-abrev   LIKE emitente.nome-abrev
    INDEX id cod-emitente.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

DEFINE TEMP-TABLE tt-erros
    FIELD cd-msg    AS CHAR
    FIELD desc-erro AS CHAR
    FIELD tipo-erro AS INT.
/************************************************/

/** PARAMETROS DE ENTRADA **/
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.
/************************************************/

/** DEFINIÄ«O VARIµVEIS **/
{include/i-rpvar.i}
DEFINE VARIABLE h-acomp         AS HANDLE                              NO-UNDO.
DEFINE VARIABLE c-destino       AS CHARACTER  FORMAT "x(20)"           NO-UNDO.
DEFINE VARIABLE c-arquivo       AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE c-arquivo-aux   AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE vchExcel        AS COM-HANDLE                          NO-UNDO.
DEFINE VARIABLE i-linha         AS INTEGER                             NO-UNDO.
DEFINE VARIABLE i-linha-emit    AS INTEGER                             NO-UNDO.
DEFINE VARIABLE ch-Arquivo      AS COMPONENT-HANDLE                    NO-UNDO.
DEFINE VARIABLE ch-Planilha     AS COMPONENT-HANDLE                    NO-UNDO.
DEFINE VARIABLE l-erro          AS LOGICAL                             NO-UNDO.
DEFINE VARIABLE l-digita        AS LOGICAL                             NO-UNDO.
/************************************************/

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST tt-param     NO-LOCK NO-ERROR.

ASSIGN l-digita = NO.
IF NOT CAN-FIND(FIRST tt-digita) THEN DO:
    FOR EACH emitente FIELDS (cod-emitente &IF "{&bf_mat_versao_ems}" < "2.07" &THEN char-1 &ELSE log-possui-nf-eletro &ENDIF)
        WHERE emitente.cod-emitente >= tt-param.cod-emitente-ini
          AND emitente.cod-emitente <= tt-param.cod-emitente-fim
          AND emitente.nome-abrev   >= tt-param.nome-abrev-ini  
          AND emitente.nome-abrev   <= tt-param.nome-abrev-fim NO-LOCK:

        IF  &IF "{&bf_mat_versao_ems}" < "2.07" &THEN
           SUBSTRING(emitente.char-1,99,1) <> "1"
        &ELSE
           NOT emitente.log-possui-nf-eletro 
        &ENDIF THEN
            NEXT.

        CREATE tt-digita.
        ASSIGN tt-digita.cod-emitente = emitente.cod-emitente. 
    END.
END.
ELSE 
    ASSIGN l-digita = YES.

ASSIGN c-programa 	  = "ESNF005RP"
       c-versao	      = "2.00"
       c-revisao	  = ".00.000"
       c-empresa      = param-global.grupo
       c-sistema	  = "Especifico"
       c-titulo-relat = "Inconsistàncias Item x Fornecedor".

{include/i-rpcab.i}
{include/i-rpout.i}

IF tt-param.destino = 2
OR tt-param.destino = 3 THEN DO:
    ASSIGN c-arquivo-aux = SEARCH ("modelo/esnf005.xlsx")
           c-arquivo-aux = REPLACE(c-arquivo-aux, "/","~\").
    
    CREATE "Excel.Application" vchexcel.
    ASSIGN FILE-INFO:FILE-NAME = c-arquivo-aux.

    ch-Arquivo  = vchexcel:WorkBooks:OPEN(FILE-INFO:FILE-NAME,,TRUE). 
    ch-planilha = ch-arquivo:worksheets:ITEM(1):activate.

    ASSIGN c-arquivo           = REPLACE (tt-param.arquivo-excel, "/","~\") 
           FILE-INFO:FILE-NAME = c-arquivo.
    
    IF SEARCH(c-arquivo) <> ? THEN DO:
        OS-DELETE VALUE(c-arquivo) NO-ERROR.
    END.
    
    vchexcel:ActiveWorkbook:SaveAs(FILE-INFO:FILE-NAME,,"","",NO,NO,NO).
END.

/** BLOCO PRINCIPAL **/
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

ASSIGN i-linha = 3.

/** LEITURA SOLICITAÄ«O **/
FOR EACH tt-digita,
    FIRST emitente FIELDS (cod-emitente nome-abrev 
                           &IF "{&bf_mat_versao_ems}" < "2.07" &THEN char-1 &ELSE log-possui-nf-eletro &ENDIF)
    WHERE emitente.cod-emitente = tt-digita.cod-emitente NO-LOCK
    BREAK BY tt-digita.cod-emitente:

    IF  &IF "{&bf_mat_versao_ems}" < "2.07" &THEN
        SUBSTRING(emitente.char-1,99,1) <> "1"
    &ELSE
        NOT emitente.log-possui-nf-eletro 
    &ENDIF THEN
        NEXT.

    vchexcel:Range("A" + STRING(i-linha, "999999")):VALUE  = emitente.cod-emitente.
    vchexcel:Range("B" + STRING(i-linha, "999999")):VALUE  = emitente.nome-abrev.
    
    ASSIGN tt-digita.nome-abrev = emitente.nome-abrev
           i-linha-emit         = i-linha
           l-erro               = NO.

    /** LEITURA ITEM X FORNECEDOR **/
    FOR EACH item-fornec FIELDS (it-codigo item-do-forn ativo unid-med-for fator-conv)
        WHERE item-fornec.cod-emitente  = emitente.cod-emitente NO-LOCK
        BREAK BY item-fornec.cod-emitente:

        RUN pi-acompanhar IN h-acomp (INPUT "Emitente: " + STRING(emitente.cod-emitente) + " Item: " + item-fornec.it-codigo).

        FOR FIRST ITEM FIELDS (desc-item)
            WHERE ITEM.it-codigo = item-fornec.it-codigo.
        END.

        /** ITEM EXISTE **/
        IF NOT AVAIL ITEM THEN DO:
            RUN utp/ut-msgs.p (INPUT "MSG":U, INPUT 2, INPUT "Item":U).
            vchexcel:Range("C" + STRING(i-linha, "999999")):VALUE  = item-fornec.it-codigo.
            vchexcel:Range("D" + STRING(i-linha, "999999")):VALUE  = "".
            vchexcel:Range("E" + STRING(i-linha, "999999")):VALUE  = "2".
            vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = RETURN-VALUE.
            vchexcel:Range("G" + STRING(i-linha, "999999")):VALUE  = item-fornec.item-do-forn.
            vchexcel:Range("H" + STRING(i-linha, "999999")):VALUE  = IF item-fornec.ativo THEN "Sim":U ELSE "N∆o".
            vchexcel:Range("I" + STRING(i-linha, "999999")):VALUE  = item-fornec.unid-med-for.
            vchexcel:Range("J" + STRING(i-linha, "999999")):VALUE  = item-fornec.fator-conv.
            vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = 44.

            ASSIGN i-linha = i-linha + 1
                   l-erro  = YES.

            NEXT.
        END.
            
        /** FORNECEDOR INATIVO **/
        IF  NOT item-fornec.ativo THEN DO:
            RUN utp/ut-msgs.p (INPUT "MSG":U, INPUT 27558, INPUT "":U).
            vchexcel:Range("C" + STRING(i-linha, "999999")):VALUE  = item-fornec.it-codigo.
            vchexcel:Range("D" + STRING(i-linha, "999999")):VALUE  = ITEM.desc-item.
            vchexcel:Range("E" + STRING(i-linha, "999999")):VALUE  = "2".
            vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = RETURN-VALUE.
            vchexcel:Range("G" + STRING(i-linha, "999999")):VALUE  = item-fornec.item-do-forn.
            vchexcel:Range("H" + STRING(i-linha, "999999")):VALUE  = IF item-fornec.ativo THEN "Sim":U ELSE "N∆o".
            vchexcel:Range("I" + STRING(i-linha, "999999")):VALUE  = item-fornec.unid-med-for.
            vchexcel:Range("J" + STRING(i-linha, "999999")):VALUE  = item-fornec.fator-conv.
            vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = 44.

            ASSIGN i-linha = i-linha + 1
                   l-erro  = YES.
        END.

        /** ITEM EMITENTE = ITEM INTERNO **/
        IF  item-fornec.it-codigo = item-fornec.item-do-forn THEN DO:
            vchexcel:Range("C" + STRING(i-linha, "999999")):VALUE  = item-fornec.it-codigo.
            vchexcel:Range("D" + STRING(i-linha, "999999")):VALUE  = ITEM.desc-item.
            vchexcel:Range("E" + STRING(i-linha, "999999")):VALUE  = "15825".
            vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = "O c¢digo interno do item Ç igual ao c¢digo do fornecedor.".
            vchexcel:Range("G" + STRING(i-linha, "999999")):VALUE  = item-fornec.item-do-forn.
            vchexcel:Range("H" + STRING(i-linha, "999999")):VALUE  = IF item-fornec.ativo THEN "Sim":U ELSE "N∆o".
            vchexcel:Range("I" + STRING(i-linha, "999999")):VALUE  = item-fornec.unid-med-for.
            vchexcel:Range("J" + STRING(i-linha, "999999")):VALUE  = item-fornec.fator-conv.
            vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = 44.

            ASSIGN i-linha = i-linha + 1
                   l-erro  = YES.
        END.

        /** ITEM EMITENTE = BRANCO **/
        IF  item-fornec.item-do-forn = "":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "MSG":U, INPUT 164, INPUT "C¢digo Item Fornecedor":U).
            vchexcel:Range("C" + STRING(i-linha, "999999")):VALUE  = item-fornec.it-codigo.
            vchexcel:Range("D" + STRING(i-linha, "999999")):VALUE  = ITEM.desc-item.
            vchexcel:Range("E" + STRING(i-linha, "999999")):VALUE  = "164".
            vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = RETURN-VALUE.
            vchexcel:Range("G" + STRING(i-linha, "999999")):VALUE  = item-fornec.item-do-forn.
            vchexcel:Range("H" + STRING(i-linha, "999999")):VALUE  = IF item-fornec.ativo THEN "Sim":U ELSE "N∆o".
            vchexcel:Range("I" + STRING(i-linha, "999999")):VALUE  = item-fornec.unid-med-for.
            vchexcel:Range("J" + STRING(i-linha, "999999")):VALUE  = item-fornec.fator-conv.
            vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = 44.

            ASSIGN i-linha = i-linha + 1
                   l-erro  = YES.
        END.

        /** UNIDADE DE MEDIDA INEXISTENTE **/
        IF NOT CAN-FIND(FIRST tab-unidade
                        WHERE tab-unidade.un = item-fornec.unid-med-for) THEN
        IF  item-fornec.item-do-forn = "":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "MSG":U, INPUT 2, INPUT "Unidade Medida Fornecedor":U).
            vchexcel:Range("C" + STRING(i-linha, "999999")):VALUE  = item-fornec.it-codigo.
            vchexcel:Range("D" + STRING(i-linha, "999999")):VALUE  = ITEM.desc-item.
            vchexcel:Range("E" + STRING(i-linha, "999999")):VALUE  = "2".
            vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = RETURN-VALUE.
            vchexcel:Range("G" + STRING(i-linha, "999999")):VALUE  = item-fornec.item-do-forn.
            vchexcel:Range("H" + STRING(i-linha, "999999")):VALUE  = IF item-fornec.ativo THEN "Sim":U ELSE "N∆o".
            vchexcel:Range("I" + STRING(i-linha, "999999")):VALUE  = item-fornec.unid-med-for.
            vchexcel:Range("J" + STRING(i-linha, "999999")):VALUE  = item-fornec.fator-conv.
            vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = 44.

            ASSIGN i-linha = i-linha + 1
                   l-erro  = YES.
        END.

        /** FATOR DE CONVERS«O = ZERO **/
        IF  item-fornec.fator-conver = 0 THEN DO:
            RUN utp/ut-msgs.p (INPUT "MSG":U, INPUT 36, INPUT "Fator de Convers∆o":U).
            vchexcel:Range("C" + STRING(i-linha, "999999")):VALUE  = item-fornec.it-codigo.
            vchexcel:Range("D" + STRING(i-linha, "999999")):VALUE  = ITEM.desc-item.
            vchexcel:Range("E" + STRING(i-linha, "999999")):VALUE  = "164".
            vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = RETURN-VALUE.
            vchexcel:Range("G" + STRING(i-linha, "999999")):VALUE  = item-fornec.item-do-forn.
            vchexcel:Range("H" + STRING(i-linha, "999999")):VALUE  = IF item-fornec.ativo THEN "Sim":U ELSE "N∆o".
            vchexcel:Range("I" + STRING(i-linha, "999999")):VALUE  = item-fornec.unid-med-for.
            vchexcel:Range("J" + STRING(i-linha, "999999")):VALUE  = item-fornec.fator-conv.
            vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = 44.

            ASSIGN i-linha = i-linha + 1
                   l-erro  = YES.
        END.
    END.
    /************************************************/

    IF NOT l-erro THEN DO:
        vchexcel:Range("F" + STRING(i-linha, "999999")):VALUE  = "N∆o foram encontradas inconsistàncias para o Emitente.".
        vchexcel:Range("A" + STRING(i-linha, "999999"), "J" + STRING(i-linha, "999999")):Interior:ColorIndex = IF i-linha MODULO 2 = 0 THEN 15 ELSE 2.
        ASSIGN i-linha = i-linha + 1.                                                                                     
    END.
END.

vchexcel:Range("A" + STRING(3, "999999"), "A" + STRING(i-linha, "999999")):Borders(7):LineStyle  = 1.
vchexcel:Range("A" + STRING(3, "999999"), "A" + STRING(i-linha, "999999")):Borders(7):Weight     = -4138.
vchexcel:Range("A" + STRING(3, "999999"), "A" + STRING(i-linha, "999999")):Borders(7):ColorIndex = -4105.
vchexcel:Range("A" + STRING(3, "999999"), "A" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("A" + STRING(3, "999999"), "A" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("A" + STRING(3, "999999"), "A" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("A" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("A" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("A" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("B" + STRING(3, "999999"), "B" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("B" + STRING(3, "999999"), "B" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("B" + STRING(3, "999999"), "B" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("B" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("B" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("B" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("C" + STRING(3, "999999"), "C" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("C" + STRING(3, "999999"), "C" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("C" + STRING(3, "999999"), "C" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("C" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("C" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("C" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("D" + STRING(3, "999999"), "D" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("D" + STRING(3, "999999"), "D" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("D" + STRING(3, "999999"), "D" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("D" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("D" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("D" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("E" + STRING(3, "999999"), "E" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("E" + STRING(3, "999999"), "E" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("E" + STRING(3, "999999"), "E" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("E" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("E" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("E" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("F" + STRING(3, "999999"), "F" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("F" + STRING(3, "999999"), "F" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("F" + STRING(3, "999999"), "F" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("F" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("F" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("F" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("G" + STRING(3, "999999"), "G" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("G" + STRING(3, "999999"), "G" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("G" + STRING(3, "999999"), "G" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("G" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("G" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("G" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("H" + STRING(3, "999999"), "H" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("H" + STRING(3, "999999"), "H" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("H" + STRING(3, "999999"), "H" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("H" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("H" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("H" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("I" + STRING(3, "999999"), "I" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("I" + STRING(3, "999999"), "I" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("I" + STRING(3, "999999"), "I" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("I" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("I" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("I" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

vchexcel:Range("J" + STRING(3, "999999"), "J" + STRING(i-linha, "999999")):Borders(10):LineStyle  = 1.
vchexcel:Range("J" + STRING(3, "999999"), "J" + STRING(i-linha, "999999")):Borders(10):Weight     = -4138.
vchexcel:Range("J" + STRING(3, "999999"), "J" + STRING(i-linha, "999999")):Borders(10):ColorIndex = -4105.
vchexcel:Range("J" + STRING(i-linha, "999999")):Borders(9):LineStyle  = 1.    
vchexcel:Range("J" + STRING(i-linha, "999999")):Borders(9):Weight     = -4138.
vchexcel:Range("J" + STRING(i-linha, "999999")):Borders(9):ColorIndex = -4105.

IF tt-param.destino <> 1 THEN DO:
    vchexcel:ActiveWorkbook:SAVE.
    vchexcel:ActiveWorkbook:CLOSE.
    vchexcel:VISIBLE = YES.
    vchexcel:Workbooks:OPEN(c-arquivo).
    RELEASE OBJECT vchexcel.

    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.

    PUT UNFORMATTED "RELAT‡RIO FOI GERADO EM " tt-param.arquivo-excel SKIP(3).
END.
/************************************************/


/** IMPRESS«O PARAMETROS **/
IF tt-param.tg-parametros THEN DO:
    PAGE.

    CASE tt-param.destino:
       WHEN 1 THEN
           ASSIGN c-destino = "Impressora".
       WHEN 2 THEN
           ASSIGN c-destino = "Arquivo".
       WHEN 3 THEN
           ASSIGN c-destino = "Terminal".
    END CASE.
    
    PUT UNFORMATTED
        "SELEÄ«O"               AT 01  SKIP(1)
        "    Emitente: "        AT 10  tt-param.cod-emitente-ini  "|< >| " AT 45 tt-param.cod-emitente-fim  SKIP
        "  Nome Abrev: "        AT 10  tt-param.nome-abrev-ini    "|< >| " AT 45 tt-param.nome-abrev-fim    SKIP(1).

    IF l-digita THEN DO:
        PUT UNFORMATTED
            "DIGITAÄ«O"            AT 01  SKIP(1)
            "         Emitente  Nome Abrev  " SKIP
            "         --------- ------------" SKIP.

        FOR EACH tt-digita:
            PUT UNFORMATTED
                tt-digita.cod-emitente AT 10
                tt-digita.nome-abrev   AT 20.
                 
        END.

        PUT UNFORMATTED SKIP(1).
    END.
    
    PUT UNFORMATTED
        "IMPRESS«O"             AT 01  SKIP(1)
        "            Destino: " AT 10  c-destino          " - "    tt-param.arquivo SKIP
        "            Usu†rio: " AT 10  tt-param.usuario                             SKIP
        "Imprimir ParÉmetros: " AT 10  "Sim"
        . 
END.
/************************************************/

{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

