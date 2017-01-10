&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS PROCEDURE 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** alterado dia 25/02 - rmk - Replicacao tabela esp-ped-item, tratativa item-uf
** release de tabelas.
*******************************************************************************/
{include/i-prgvrs.i ESCRM004RP 2.06.00.002}

/* CREATE an unnamed pool to store all the widgets CREATEd 
     by this PROCEDURE. This is a good default which assures
     that this PROCEDURE's triggers and internal PROCEDUREs 
     will execute in this PROCEDURE's storage, and that proper
     cleanup will occur on deletion of the PROCEDURE. */

/* ***************************  Definitions  ************************** */
DEFINE NEW GLOBAL SHARED VARIABLE i-ep-codigo-usuario AS CHAR NO-UNDO.
/* Temporary Table Definitions ---                                      */
define temp-table tt-PARAM
    FIELD destino          AS INT
    FIELD arq-destino      AS CHAR
    FIELD arq-entrada      AS CHAR
    FIELD todos            AS INT
    FIELD usuario          AS CHAR
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INT
    FIELD l-execucao-cont  AS LOG
    FIELD i-minutos        AS INT.

DEFINE TEMP-TABLE tt-raw-digita 
    FIELD raw-digita       AS RAW.
    
/* PARAMeters Definitions ---                                           */
DEFINE INPUT  PARAMETER raw-PARAM AS RAW      NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR tt-raw-digita.

/* Recebimento de ParÉmetros */
CREATE tt-PARAM.
RAW-TRANSFER raw-PARAM TO tt-PARAM.

/* Local Variable Definitions ---                                       */
DEFINE STREAM str-log. /* 18/08/2015 - modificado str-rp para str-log */
DEFINE STREAM str-file.
/* DEFINE STREAM str-open. - 18/08/2015 nao usado */
DEF VAR h-acomp AS HANDLE     NO-UNDO.
{btb/btb912zc.i3}
/* include padr∆o para vari†veis para o log  */
{include/i-rpvar.i}

/*** Definicao da Temp Tables do Peidido *************************************/
DEF TEMP-TABLE tt-ped-venda  NO-UNDO LIKE ped-venda
    FIELD r-ROWID AS ROWID.

DEF TEMP-TABLE tt-ped-item   NO-UNDO LIKE ped-item
    FIELD r-ROWID AS ROWID.

DEF NEW SHARED TEMP-TABLE RowErrors NO-UNDO
    FIELD errorSequence         AS INT
    FIELD errorNumber           AS INT
    FIELD errorDescription      AS CHAR
    FIELD errorPARAMeters       AS CHAR
    FIELD errorType             AS CHAR
    FIELD errorHelp             AS CHAR
    FIELD errorsubtype          AS CHAR.

DEF TEMP-TABLE tt-erro-copia
    FIELD cod-emitente          LIKE ped-venda.cod-emitente
    FIELD nr-pedcli             LIKE ped-item.nr-pedcli
    FIELD importou              AS LOG FORMAT "Sim/Nao" INIT "Nao"
    FIELD errorNumber           AS INT
    FIELD errorDescription      AS CHAR.

DEF TEMP-TABLE tt-log-ped-venda NO-UNDO
    FIELD nome-abrev            LIKE ped-item.nome-abrev
    FIELD nr-pedcli             LIKE ped-item.nr-pedcli
    FIELD importou              AS   LOG  FORMAT "Sim/Nao" INIT "Nao"
    FIELD data-import           AS   DATE FORMAT "99/99/9999"
    FIELD hora-import           AS   CHAR FORMAT "x(10)" 
    FIELD cd-erro               AS   INT
    FIELD mensagem              AS   CHAR FORMAT "x(300)"
    FIELD cod-estabel           LIKE estabelec.cod-estabel   
    FIELD emergencial           AS   INT
    FIELD tipo-transacao        AS   INT
    FIELD dt-implant            AS   DATE
    FIELD dt-entrega            AS   DATE
    FIELD nat-operacao          AS   CHAR
    FIELD cod-cond-pag          AS   INT
    FIELD nr-tabpre             AS   CHAR
    FIELD cod-priori            AS   INT
    FIELD perc-desco1           AS   DEC
    FIELD perc-desco2           AS   DEC
    FIELD observacoes           AS   CHAR                                          
    FIELD nome-transp           AS   CHAR
    FIELD tb-preco              AS   INT
    FIELD mo-codigo             AS   INT
    FIELD no-ab-reppri          AS   CHAR
    FIELD vl-desconto           AS   DEC
    FIELD nome-ab-rep           AS   CHAR
    FIELD cod-entrega           AS   CHAR
    FIELD tipo-fatur            AS   INT
    FIELD dt-min-fat            AS   DATE
    FIELD transp-redesp         AS   CHAR
    FIELD nr-pedrep             AS   CHAR
    FIELD cidade-cif            AS   CHAR
    FIELD cod-priori-log        as   int
    FIELD modal-doc-fiscal      AS   CHAR
    INDEX ch-Pedido IS PRIMARY
          nome-abrev
          nr-pedcli.

DEF TEMP-TABLE tt-log-ped-item  NO-UNDO
    FIELD nome-abrev            LIKE ped-item.nome-abrev
    FIELD nr-pedcli             LIKE ped-item.nr-pedcli
    FIELD nr-sequencia          LIKE ped-item.nr-sequencia
    FIELD it-codigo             LIKE ped-item.it-codigo
    FIELD cod-refer             LIKE ped-item.cod-refer
    FIELD importou              AS   LOG  FORMAT "Sim/Nao" INIT "Nao"
    FIELD cd-erro               AS   INT
    FIELD mensagem              AS   CHAR FORMAT "x(300)"
    FIELD cod-estabel           LIKE estabelec.cod-estabel          
    FIELD tipo-transacao        AS   INT
    FIELD nat-operacao          LIKE natur-oper.nat-operacao         
    FIELD qt-pedida             AS   DEC
    FIELD vl-preori             AS   DEC
    FIELD vl-preuni             AS   DEC
    FIELD per-des-item          AS   DEC
    FIELD nr-tabpre             AS   CHAR
    FIELD tb-preco              AS   INT
    FIELD vl-total              AS   DEC
    FIELD perc-icms             AS   DEC
    FIELD obs-item-ped          AS   CHAR
    INDEX ch-item-ped IS PRIMARY
          nome-abrev
          nr-pedcli
          nr-sequencia
          it-codigo
          cod-refer.

/* Variaveis de Leitura de Pedido */
DEFINE TEMP-TABLE tt-arq_ped NO-UNDO
    FIELD c_arq_ped_cod-estabel      AS CHAR
    FIELD c_arq_ped_cod-emitente     AS CHAR
    FIELD c_arq_ped_nr-pedcli        AS CHAR
    FIELD c_arq_ped_emergencial      AS CHAR
    FIELD c_arq_ped_dt-entrega       AS CHAR
    FIELD c_arq_ped_cod-cond-pag     AS CHAR
    FIELD c_arq_ped_nr-tabpre        AS CHAR
    FIELD c_arq_ped_cod-priori       AS CHAR
    FIELD c_arq_ped_observacoes      AS CHAR
    FIELD c_arq_ped_nome-transp      AS CHAR
    FIELD c_arq_ped_tb-preco         AS CHAR
    FIELD c_arq_ped_mo-codigo        AS CHAR
    FIELD c_arq_ped_no-ab-reppri     AS CHAR
    FIELD c_arq_ped_vl-desconto      AS CHAR
    FIELD c_arq_ped_nome-ab-rep      AS CHAR
    FIELD c_arq_ped_cod-entrega      AS CHAR
    FIELD c_arq_ped_tipo-fatur       AS CHAR
    FIELD c_arq_ped_nat-operacao     AS CHAR
    FIELD c_arq_ped_dt-min-fat       AS CHAR
    FIELD c_arq_ped_transp-redesp    AS CHAR
    FIELD c_arq_ped_nr-pedrep        AS CHAR
    FIELD c_arq_ped_cidade-cif       AS CHAR
    FIELD c_arq_ped_cod-canal-venda  AS CHAR
    FIELD c_arq_ped_e-mail           AS CHAR
    FIELD c_arq_ped_nome-abrev-tri   AS CHAR
    FIELD c_arq_ped_cod-entrega-tri  AS CHAR
    FIELD c_cod_unid_negocio         AS CHAR
    FIELD c_arq_ped_dest_merc        AS CHAR
    FIELD c_arq_ped_cod_priori_log   AS CHAR
    FIELD c_arq_ped_modal_doc_fiscal AS CHAR.


/* Variaveis de Leitura de Item */

DEFINE TEMP-TABLE tt-arq_ite NO-UNDO
    FIELD c_arq_ped_cod-emitente     AS CHAR
    FIELD c_arq_ped_nr-pedcli        AS CHAR
    FIELD c_arq_ite_nr-sequencia     AS CHAR
    FIELD c_arq_ite_it-codigo        AS CHAR
    FIELD c_arq_ite_qt-pedida        AS CHAR
    FIELD c_arq_ite_vl-preori        AS CHAR
    FIELD c_arq_ite_nr-tabpre        AS CHAR
    FIELD c_arq_ite_tb-preco         AS CHAR
    FIELD c_arq_ite_vl-total         AS CHAR
    FIELD c_arq_ite_perc-icms        AS CHAR
    FIELD c_arq_ite_obs-item-ped     AS CHAR
    FIELD c_arq_ite_nat-operacao     AS CHAR
    FIELD c_arq_ite_cod-estabel      AS CHAR
    FIELD c_arq_ite_dt-entrega       AS CHAR
    FIELD c_arq_ite_ind-icm-ret      AS CHAR
    FIELD c_arq_ite_lanc_minimo      AS CHAR
    FIELD c_arq_ite_lanc_obrigato    AS CHAR       
    FIELD c_arq_ite_ped_compr        AS CHAR
    FIELD c_arq_ite_seq_ped_compr    AS CHAR
    FIELD c_cod_unid_negocio         AS CHAR
    FIELD c_arq_ite_per_st_icm       AS CHAR
    FIELD c_arq_ite_per_icm_estad_st AS CHAR.


/*********Temp-table de lote- CFF - 15/02/2015********************************/
DEFINE TEMP-TABLE tt-arq_lote
    FIELD nome-abrev    like ped-item.nome-abrev 
    FIELD nr-pedcli     like ped-item.nr-pedcli  
    FIELD nr-sequencia  like ped-item.nr-sequenci
    FIELD it-codigo     like ped-item.it-codigo  
    FIELD cod-refer     like ped-item.cod-refer  
    FIELD codigo        AS CHAR
    FIELD cod-depos     AS CHAR
    FIELD cod-localiz   AS CHAR
    FIELD quantidade    AS DEC
    FIELD dt-validade   AS DATE.

DEFINE TEMP-TABLE tt-arq_lote_copia
    FIELD nome-abrev    like ped-item.nome-abrev 
    FIELD nr-pedcli     like ped-item.nr-pedcli  
    FIELD nr-sequencia  like ped-item.nr-sequenci
    FIELD it-codigo     like ped-item.it-codigo  
    FIELD cod-refer     like ped-item.cod-refer  
    FIELD codigo        AS CHAR
    FIELD cod-depos     AS CHAR
    FIELD cod-localiz   AS CHAR
    FIELD quantidade    AS DEC
    FIELD dt-validade   AS DATE.

/*****************************************************************************/

/****************XMLS*********************************************/
DEF VAR hXdoc       AS HANDLE     NO-UNDO.
DEF VAR hRoot       AS HANDLE     NO-UNDO.
DEF VAR hPedido     AS HANDLE     NO-UNDO.
DEF VAR hChild      AS HANDLE     NO-UNDO.
DEF VAR i           AS INT        NO-UNDO.
DEF VAR c-arquivo   AS CHAR       NO-UNDO.
DEF VAR content     AS LONGCHAR   NO-UNDO.
DEF VAR fileBinary  AS MEMPTR     NO-UNDO.
DEF VAR conteudo    AS LONGCHAR   NO-UNDO.
DEF VAR Memory      AS MEMPTR     NO-UNDO.
DEFINE STREAM str-imp.
DEF VAR lResult     AS LOG    NO-UNDO.
/*****************************************************************/

/* Variaveis da BO */
DEF VAR bo-ped-venda        AS HANDLE NO-UNDO.
DEF VAR bo-ped-venda-cpy    AS HANDLE NO-UNDO.
DEF VAR bo-ped-venda-sdf    AS HANDLE NO-UNDO.
DEF VAR bo-ped-venda-com    AS HANDLE NO-UNDO.
DEF VAR bo-ped-venda-cal    AS HANDLE NO-UNDO.
DEF VAR bo-ped-venda-can    AS HANDLE NO-UNDO.
DEF VAR bo-ped-item         AS HANDLE NO-UNDO.
DEF VAR bo-ped-item-sdf     AS HANDLE NO-UNDO.
DEF VAR bo-ped-item-can     AS HANDLE NO-UNDO.
DEF VAR bo-ped-repre        AS HANDLE NO-UNDO.
DEF VAR h-alocacao          AS HANDLE NO-UNDO.

DEF VAR c-nome-rep-indireto AS CHAR   NO-UNDO.
DEF VAR c-nome-rep-direto   AS CHAR   NO-UNDO.
DEF VAR c-nome-redespacho   AS CHAR   NO-UNDO.
DEF VAR c-file              AS CHAR   NO-UNDO.
DEF VAR c-linha             AS CHAR   NO-UNDO.
DEF VAR i-validator         AS INT    NO-UNDO.
DEF VAR i-embarque          AS INT    NO-UNDO.
DEF VAR l-arquivo-valido    AS LOG    NO-UNDO.
DEF VAR c-dir-importacao    AS CHAR   NO-UNDO.

define buffer buf-cont-emit for cont-emit.
define buffer bf-esp-ped-item for esp-ped-item.

DEF VAR l-erro-Pedido-xml       AS LOG     NO-UNDO.
DEF VAR l-erro-itens-Pedido-xml AS LOG     NO-UNDO.

DEF VAR c-estado-origem  like emitente.estado NO-UNDO.
DEF VAR c-estado-destino like emitente.estado NO-UNDO.

/*****/

DEF VAR i-situacao              AS INT                             NO-UNDO INIT 1.
DEF VAR i-natur-oper            AS INT                             NO-UNDO INIT 1.
DEF VAR l-exp-dt-entrega        AS LOG                             NO-UNDO.
DEF VAR hShowMsg                AS HANDLE                          NO-UNDO.
DEF VAR i-sequencia             AS INT                             NO-UNDO.
DEF VAR de-preco                LIKE ITEM.preco-base               NO-UNDO.
DEF VAR i-nr-Pedido-orig        like ped-venda.nr-Pedido           NO-UNDO.
DEF VAR c-nat-operacao-orig     like ped-venda.nat-operacao        NO-UNDO.
DEF VAR c-cod-estabel-orig      like ped-venda.cod-estabel         NO-UNDO.
DEF VAR i-cod-priori-orig       like dw-ped-venda.cod-priori       NO-UNDO.
DEF VAR i-nr-Pedido-novo        like ped-venda.nr-Pedido           NO-UNDO.
DEF VAR c-modal-doc-fiscal      LIKE dw-ped-venda.modal-doc-fiscal NO-UNDO.
DEF VAR i-cod-emitente          LIKE emitente.cod-emitente         NO-UNDO.
DEF VAR c-nr-pedcli-original    LIKE ped-venda.nr-pedcli           NO-UNDO.
DEF VAR c-nome-abrev-original   LIKE ped-venda.nome-abrev          NO-UNDO.
DEF VAR c-erro                  AS CHAR                            NO-UNDO.

DEF VAR cLoteAux                AS CHAR                            NO-UNDO.
DEF VAR cDepAux                 AS CHAR                            NO-UNDO.
DEF VAR cLocalAux               AS CHAR                            NO-UNDO.
DEF VAR deQtdeAux               AS DEC                             NO-UNDO.
DEF VAR dtValidLoteAux          AS DATE                            NO-UNDO.

def new global shared var gr-ped-venda as ROWID NO-UNDO.

/*{method/dbotterr.i}*/

{dibo/bodi159cpy.i}

/*****/



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-DEF PROCEDURE-TYPE PROCEDURE
&Scoped-DEF DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** PROCEDURE Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: PROCEDURE
   Allow: 
   Frames: 0
   Add FIELDS to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  CREATE Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW PROCEDURE ASSIGN
         HEIGHT             = 15.88
         WIDTH              = 39.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK PROCEDURE 


/* ***************************  Main Block  *************************** */

CREATE ALIAS eai       FOR DATABASE mgcad NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa     = "ESCRM004RP"
       c-versao       = "2.04"
       c-revisao      = ".00.001"
       c-titulo-relat = "Importaá∆o de Pedidos".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Importando *}

RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND FIRST param-integra NO-LOCK NO-ERROR.

IF param-integra.l-integra-Pedido THEN DO:
    /* complementando diretorio de importacao dos arquivos */
    FIND FIRST PARAM-global NO-LOCK NO-ERROR.
    
/*     IF AVAIL PARAM-global AND TRIM(STRING(PARAM-global.empresa-prin)) <> "" THEN */
/*        c-dir-importacao = param-integra.dir-arq-imp + "/empresa_" + TRIM(STRING(PARAM-global.empresa-prin)). */
/*     ELSE */
/*        c-dir-importacao = param-integra.dir-arq-imp. */
/*
ASSIGN c-dir-importacao = 'E:\temp\XML\'. 
*/
/*      message c-dir-importacao view-as alert-box. */
    INPUT STREAM str-file FROM OS-DIR(c-dir-importacao).
    REPEAT:
        IMPORT STREAM str-file c-file.

        /****elimina arquivos que n∆o s∆o XMls - CFF Sottelli ******/
        IF INDEX(c-file,"xml") <= 0 THEN NEXT.
    
        /* Zerar temp Tables */
        FOR EACH tt-arq_ped: DELETE tt-arq_ped. END.
        FOR EACH tt-arq_ite: DELETE tt-arq_ite. END.
        FOR EACH tt-log-ped-venda:  DELETE tt-log-ped-venda. END.
        FOR EACH tt-log-ped-item:   DELETE tt-log-ped-item.  END.

        /*****CFF - zera tt-lote ******************************************/
        EMPTY TEMP-TABLE tt-arq_lote.
        /******************************************************************/
        
        CREATE X-DOCUMENT hXdoc.
        CREATE X-NODEREF hRoot.
        
        c-arquivo = replace(c-dir-importacao + chr(47) + c-file , chr(92) , chr(47) ).
        
        ASSIGN l-arquivo-valido = NO
               l-erro-Pedido-xml = NO
               l-erro-itens-Pedido-xml = NO.

        SET-SIZE(fileBinary) = 0.
        FILE-INFO:FILE-NAME = c-arquivo.
        SET-SIZE(fileBinary) = FILE-INFO:FILE-SIZE.
        INPUT STREAM str-imp  FROM VALUE(c-arquivo) BINARY NO-CONVERT.
        IMPORT STREAM str-imp  fileBinary.
        INPUT STREAM str-imp  CLOSE.
        
        COPY-LOB fileBinary TO Memory CONVERT TARGET CODEPAGE "utf-8".

        lResult = hXdoc:LOAD("MEMPTR",Memory,FALSE) NO-ERROR.

        IF NOT lResult THEN DO:
            MESSAGE "Arquivo XML mal formatado: " VIEW-AS ALERT-BOX INFO BUTTONS OK.
            l-arquivo-valido = NO.
        END.
        ELSE DO:
            ASSIGN l-arquivo-valido = yes.
            hXdoc:GET-DOCUMENT-ELEMENT(hRoot).    
            
            IF TRIM(hRoot:NAME) <> "Pedido" THEN
               ASSIGN l-arquivo-valido = NO.
            ELSE DO:
                 CREATE X-NODEREF hChild.
        
                 CREATE tt-arq_ped.
        
                 DO i = 1 TO hRoot:NUM-CHILDREN:
                     hRoot:GET-CHILD(hChild,i).
        
                     IF hChild:NAME = "Itens" THEN
                        RUN pi-processa-itens (INPUT hChild,
                                               OUTPUT l-erro-Pedido-xml).
                     ELSE
                        RUN pi-processa-dados (INPUT hChild,
                                               OUTPUT l-erro-itens-Pedido-xml).
                 END.
             END.
        END.
        
        /*************************limpa lote sem dados**********/
        FOR EACH tt-arq_lote:
            IF tt-arq_lote.codigo = ? AND tt-arq_lote.quantidade = ? THEN
            DELETE tt-arq_lote.
        END.
        /****************************************************/
        SET-SIZE(Memory) = 0.
        DELETE OBJECT hChild NO-ERROR.
        DELETE OBJECT hRoot NO-ERROR.
        DELETE OBJECT hXdoc NO-ERROR.
    
        IF NOT l-arquivo-valido THEN DO:
            RUN pi-gera-arquivo-resposta.
            NEXT.
        END.
    
        FIND FIRST tt-arq_ped NO-LOCK NO-ERROR.
    
        RUN pi-validate-Pedido.
    
        IF RETURN-VALUE = "NOK" THEN DO:

            RUN pi-gera-arquivo-resposta.
            NEXT.
        END.
        RUN pi-cadastra-Pedido.
    
        RUN pi-gera-arquivo-resposta.
    END.
END.

DO TRANSACTION:
    FIND FIRST param-integra EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL param-integra THEN 
    DO:
        ASSIGN param-integra.dt-ult-execucao-ped = TODAY 
               param-integra.hr-ult-execucao-ped = REPLACE(STRING(TIME,"HH:MM:SS"),":","").
    END.
    release param-integra.
END.

INPUT STREAM str-file CLOSE.

RUN pi-cria-pendencia-rpw.

RUN pi-finalizar IN h-acomp. 

RETURN "Ok":U.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal PROCEDUREs  *********************** */

&IF DEFINED(EXCLUDE-pi-acompanhamento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-acompanhamento PROCEDURE 
PROCEDURE pi-acompanhamento :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acompanhamento AS CHAR FORMAT "x(40)" NO-UNDO.
    
    IF VALID-HANDLE(h-acomp) THEN
    DO:
       RUN pi-acompanhar IN h-acomp(p-acompanhamento).    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cadastra-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cadastra-Pedido PROCEDURE 
PROCEDURE pi-cadastra-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pi-acompanhamento("Pedido: " + STRING(tt-arq_ped.c_arq_ped_nr-pedcli)).  
    
    FOR EACH tt-ped-venda: DELETE tt-ped-venda. END.
    FOR EACH tt-ped-item:  DELETE tt-ped-item.  END.

    PedidoTotal:
    DO TRANSACTION:

        FIND FIRST PARAM-global NO-LOCK NO-ERROR.
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.cd-erro     = 0
               tt-log-ped-venda.mensagem    = ""
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        /* INTEGRA«√O EMAIL NFE **********/

        IF emitente.e-mail <> tt-arq_ped.c_arq_ped_e-mail THEN DO:
           DISABLE TRIGGERS FOR LOAD OF emitente.
           
           FIND CURRENT emitente EXCLUSIVE-LOCK NO-ERROR.

           ASSIGN emitente.e-mail = tt-arq_ped.c_arq_ped_e-mail.
           
           /* atualizando contato da NFe */
           FIND FIRST cont-emit OF emitente 
                             WHERE cont-emit.int-1 = 2 /* destinatario NFe */ EXCLUSIVE-LOCK NO-ERROR.
           
           IF NOT AVAIL cont-emit THEN
           DO:
              FIND LAST buf-cont-emit of emitente NO-LOCK NO-ERROR.
           
              CREATE cont-emit.
              ASSIGN cont-emit.cod-emitente = emitente.cod-emitente
                     cont-emit.sequencia    = IF NOT AVAIL buf-cont-emit THEN 10 ELSE buf-cont-emit.sequencia + 10
                     cont-emit.nome         = "CONTATO NFE"
                     cont-emit.identific    = emitente.identific
                     cont-emit.int-1        = 2.                     
           END.
           
           cont-emit.e-mail = emitente.e-mail.
        END.
        /*********************************/
    
        /*** Busca os Valores Defaults do Pedido *************************************************/

        IF NOT VALID-HANDLE(bo-ped-venda) THEN
           RUN dibo/bodi159.p    PERSISTENT SET bo-ped-venda.
        
        IF NOT VALID-HANDLE(bo-ped-venda-sdf) THEN
           RUN dibo/bodi159sdf.p PERSISTENT SET bo-ped-venda-sdf.
    
        IF VALID-HANDLE(bo-ped-venda) THEN
           RUN openQueryStatic IN bo-ped-venda (INPUT "ChPedido":U).

        RUN newRecord IN bo-ped-venda.
    
        RUN getRecord IN bo-ped-venda (OUTPUT TABLE tt-ped-venda).
    
        FIND FIRST tt-ped-venda NO-ERROR.

        /*** Cria a tt-ped-venda com os Valores do CRM *******************************************/
        ASSIGN tt-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-ped-venda.nr-Pedido   = NEXT-VALUE(seq-nr-Pedido)
               tt-ped-venda.cod-estabel = tt-arq_ped.c_arq_ped_cod-estabel.

        RUN INPUTtable             IN bo-ped-venda-sdf (INPUT TABLE tt-ped-venda).
        RUN setdefaultcustomer     IN bo-ped-venda-sdf.
        RUN setDefaultCentralSales IN bo-ped-venda-sdf.
        RUN outputTable            IN bo-ped-venda-sdf (OUTPUT TABLE tt-ped-venda). 

        FIND FIRST natur-oper WHERE natur-oper.nat-operacao =  tt-arq_ped.c_arq_ped_nat-operacao NO-LOCK NO-ERROR.
    
        FIND FIRST tt-ped-venda.
        /*** Altera Alguns Campos do Pedido passados *********************************************/
        ASSIGN tt-ped-venda.nat-operacao    = tt-arq_ped.c_arq_ped_nat-operacao
               tt-ped-venda.no-ab-reppri    = c-nome-rep-direto
               tt-ped-venda.cod-cond-pag    = IF AVAIL natur-oper and (natur-oper.transf OR NOT natur-oper.emite-duplic) THEN 0 ELSE INT(tt-arq_ped.c_arq_ped_cod-cond-pag)
               tt-ped-venda.dt-emissao      = TODAY
               tt-ped-venda.dt-implant      = TODAY
               tt-ped-venda.dt-entrega      = DATE(tt-arq_ped.c_arq_ped_dt-entrega)
               tt-ped-venda.dt-entorig      = DATE(tt-arq_ped.c_arq_ped_dt-entrega)
               tt-ped-venda.nome-transp     = tt-arq_ped.c_arq_ped_nome-transp
               tt-ped-venda.dt-fimvig       = ?
               tt-ped-venda.observacoes     = tt-arq_ped.c_arq_ped_observacoes
               tt-ped-venda.cond-espec      = ""
               tt-ped-venda.tp-preco        = INT(tt-arq_ped.c_arq_ped_tb-preco)
               tt-ped-venda.cidade-cif      = ""              
               tt-ped-venda.nr-tabpre       = tt-arq_ped.c_arq_ped_nr-tabpre
               tt-ped-venda.user-alt        = ""  
               tt-ped-venda.dt-useralt      = ?
               tt-ped-venda.nr-pedrep       = tt-arq_ped.c_arq_ped_nr-pedrep
               tt-ped-venda.nome-tr-red     = c-nome-redespacho
               tt-ped-venda.ind-fat-par     = IF INT(tt-arq_ped.c_arq_ped_tipo-fatur) = 2 THEN YES ELSE NO
               tt-ped-venda.cidade-cif      = IF tt-arq_ped.c_arq_ped_cidade-cif = "FOB" THEN "" ELSE "CIF"
               tt-ped-venda.cod-entrega     = tt-arq_ped.c_arq_ped_cod-entrega
               tt-ped-venda.cod-canal-venda = (IF INT(tt-arq_ped.c_arq_ped_cod-canal-venda) = 0 THEN tt-ped-venda.cod-canal-venda ELSE INT(tt-arq_ped.c_arq_ped_cod-canal-venda) )
               tt-ped-venda.cod-estabel     = tt-arq_ped.c_arq_ped_cod-estabel
               tt-ped-venda.nome-abrev-tri  = tt-arq_ped.c_arq_ped_nome-abrev-tri 
               tt-ped-venda.cod-entrega-tri = tt-arq_ped.c_arq_ped_cod-entrega-tri
               tt-ped-venda.cod-unid-negoc  = tt-arq_ped.c_cod_unid_negocio
               tt-ped-venda.cod-des-merc    = INT(tt-arq_ped.c_arq_ped_dest_merc).
               
               /**Caso a natureza seja de transferencia, atribuir o Destino da Mercadoria como - Comercio-Industria - Cfreire sottelli**/
               FIND FIRST natur-oper WHERE natur-oper.nat-operacao = tt-arq_ped.c_arq_ped_nat-operacao NO-LOCK NO-ERROR.
               if AVAIL natur-oper and natur-oper.transf THEN
                   ASSIGN tt-ped-venda.cod-des-merc  = 1.

        /*** Defauts Condicao de Pagamento *******************************************************/
        ASSIGN tt-ped-venda.nr-tab-finan = if natur-oper.transf = no THEN cond-pagto.nr-tab-finan ELSE 1
               tt-ped-venda.nr-ind-finan = if natur-oper.transf = no THEN cond-pagto.nr-ind-finan ELSE 2.
    
        /*** Avaliacao de Credito ****************************************************************/
        ASSIGN tt-ped-venda.cod-sit-aval  = 3  /* APROVADO */
               tt-ped-venda.desc-forc-cr  = "Liberado Automatico".
        
        /*** Cria o Pedido ***********************************************************************/
        RUN emptyRowErrors  IN bo-ped-venda.
        RUN setRecord       IN bo-ped-venda(INPUT TABLE tt-ped-venda).
        RUN CREATERecord    IN bo-ped-venda.
    
        /*** Verifica os Erros Retornados *******************************************************/
        RUN getRowErrors    IN bo-ped-venda(OUTPUT TABLE RowErrors).
            
        /*** ERROS ******************************************************************************/
        IF  CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN 
        DO:
            FOR EACH rowerrors WHERE RowErrors.ErrorSubType = "ERROR":U:

               ASSIGN tt-log-ped-venda.importou   = FALSE 
                      tt-log-ped-venda.cd-erro    = rowerrors.errornumber                        
                      tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription .
            END.
    
            IF VALID-HANDLE(bo-ped-venda) THEN DO:
               
                RUN destroyBo IN bo-ped-venda.
                
               IF VALID-HANDLE(bo-ped-venda) THEN DO:
                   DELETE PROCEDURE bo-ped-venda.
                   bo-ped-venda = ?.
               END.
        
            END.
    
            IF VALID-HANDLE(bo-ped-venda-sdf) THEN DO:
                DELETE PROCEDURE bo-ped-venda-sdf.
                bo-ped-venda-sdf = ?.
            END.    

            UNDO PedidoTotal, RETURN "NOK".
        END.
        
        /*** AVISOS ***************************************************************************/
        IF  CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType <> "ERROR":U) THEN 
        DO:
            FOR EACH rowerrors WHERE RowErrors.ErrorSubType <> "ERROR":U:                    
               ASSIGN tt-log-ped-venda.cd-erro    = rowerrors.errornumber                        
                      tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription .
           END.

           IF VALID-HANDLE(bo-ped-venda) THEN DO:
              RUN destroyBo IN bo-ped-venda.
               
              IF VALID-HANDLE(bo-ped-venda) THEN DO:
                  DELETE PROCEDURE bo-ped-venda.
                  bo-ped-venda = ?.
              END.
        
           END.
    
           IF VALID-HANDLE(bo-ped-venda-sdf) THEN DO:
               DELETE PROCEDURE bo-ped-venda-sdf.
               bo-ped-venda-sdf = ?.
           END.    

           UNDO PedidoTotal, RETURN "NOK".
        END.
        
        RUN getRecord IN bo-ped-venda (OUTPUT TABLE tt-ped-venda).

        FIND FIRST tt-ped-venda.
        
        RUN getROWID In bo-ped-venda(OUTPUT tt-ped-venda.r-ROWID).
        
        RUN pi-acompanhamento("Pedido: " + STRING(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Criando Repres").  
    
        /*** Comissao dos Representantes 
        **** Representante Direto
        ****************************************************************************/
        FIND FIRST ped-repre WHERE ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido
                               AND ped-repre.nome-ab-rep = c-nome-rep-direto NO-ERROR.
        IF NOT AVAIL ped-repre THEN
        DO:
            CREATE ped-repre.
            ASSIGN ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido
                   ped-repre.nome-ab-rep = c-nome-rep-direto
                   ped-repre.ind-repbase = YES
                   ped-repre.perc-comis  = repres.comis-direta
                   ped-repre.comis-emis  = repres.comis-emis.
        END. /*** Fim do IF NOT AVAIL ***/
    
        /*** Comissao dos Representantes 
        **** Representante Indireto   *********************************************/
        FIND FIRST ped-repre WHERE ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido
                               AND ped-repre.nome-ab-rep = c-nome-rep-indireto NO-ERROR.
        IF NOT AVAIL ped-repre THEN
        DO:
            CREATE ped-repre.
            ASSIGN ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido
                   ped-repre.nome-ab-rep = c-nome-rep-indireto
                   ped-repre.ind-repbase = YES
                   ped-repre.perc-comis  = 0
                   ped-repre.comis-emis  = 0.
        END. /*** Fim do IF NOT AVAIL ***/        
        
        FIND dw-ped-venda WHERE dw-ped-venda.nr-Pedido = tt-ped-venda.nr-Pedido EXCLUSIVE-LOCK NO-ERROR.
  
        IF not AVAIL dw-ped-venda THEN
           DO:
              CREATE dw-ped-venda.
              ASSIGN dw-ped-venda.nr-Pedido        = tt-ped-venda.nr-Pedido
                     dw-ped-venda.cod-priori       = int(tt-arq_ped.c_arq_ped_cod_priori_log)
                     dw-ped-venda.modal-doc-fiscal = tt-arq_ped.c_arq_ped_modal_doc_fiscal.
           END.
        ELSE
           ASSIGN dw-ped-venda.cod-priori       = int(tt-arq_ped.c_arq_ped_cod_priori_log)
                  dw-ped-venda.modal-doc-fiscal = tt-arq_ped.c_arq_ped_modal_doc_fiscal.

        find first dw-follow-up-ped no-lock
             where dw-follow-up-ped.nome-abrev = tt-ped-venda.nome-abrev
               and dw-follow-up-ped.nr-pedcli  = tt-ped-venda.nr-pedcli no-error.
        if not avail dw-follow-up-ped then do:
            create dw-follow-up-ped.
            assign dw-follow-up-ped.nome-abrev   = tt-ped-venda.nome-abrev 
                   dw-follow-up-ped.nr-pedcli    = tt-ped-venda.nr-pedcli
                   dw-follow-up-ped.user-criacao = tt-ped-venda.user-impl
                   dw-follow-up-ped.dt-criacao   = today
                   dw-follow-up-ped.hr-criacao   = STRING(TIME,'hh:mm:ss').
        end.
        
        IF VALID-HANDLE(bo-ped-venda) THEN DO:
           RUN destroyBo IN bo-ped-venda.
            
           IF VALID-HANDLE(bo-ped-venda) THEN DO:
               DELETE PROCEDURE bo-ped-venda.
               bo-ped-venda = ?.
           END.
    
        END.

        IF VALID-HANDLE(bo-ped-venda-sdf) THEN DO:
            DELETE PROCEDURE bo-ped-venda-sdf.
            bo-ped-venda-sdf = ?.
        END.    

        RUN pi-acompanhamento("Pedido: " + STRING(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Criando Itens").  

        FOR EACH tt-arq_ite WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
                              AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli NO-LOCK :

           /*** Limpa as Tabelas ****************************************************************/
           EMPTY TEMP-TABLE tt-ped-item. 

           /*** Cria Itens do Pedido ************************************************************/
           CREATE tt-ped-item.
           ASSIGN tt-ped-item.nome-abrev   = emitente.nome-abrev
                  tt-ped-item.nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli 
                  tt-ped-item.nr-sequencia = INT(tt-arq_ite.c_arq_ite_nr-sequencia)
                  tt-ped-item.it-codigo    = tt-arq_ite.c_arq_ite_it-codigo 
                  tt-ped-item.cod-refer    = ""
                  tt-ped-item.tipo-atend   = IF INT(tt-arq_ped.c_arq_ped_tipo-fatur) = 1 THEN 1 ELSE 2
                  tt-ped-item.cod-entrega  = tt-arq_ped.c_arq_ped_cod-entrega.

          /*** Cria Log de Acompanhamento ******************************************************/
          CREATE tt-log-ped-item.                                     
          ASSIGN tt-log-ped-item.nome-abrev   = tt-ped-venda.nome-abrev 
                 tt-log-ped-item.nr-pedcli    = tt-ped-venda.nr-pedcli  
                 tt-log-ped-item.nr-sequencia = tt-ped-item.nr-sequencia
                 tt-log-ped-item.it-codigo    = tt-ped-item.it-codigo   
                 tt-log-ped-item.cod-refer    = tt-ped-item.cod-refer 
                 tt-log-ped-item.importou     = FALSE.
         
          RUN pi-grava-log-peditem.

           /*** Busca Valores Defaults do Item do Pedido **************************************/
           RUN pi-acompanhamento("Pedido: " + STRING(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Itens API: BoDi154Sdf.p ").  

           IF NOT VALID-HANDLE(bo-ped-item-sdf) THEN
               RUN dibo/bodi154sdf.p PERSISTENT SET bo-ped-item-sdf.

           RUN INPUTtable      IN bo-ped-item-sdf (INPUT TABLE tt-ped-item).
           RUN setdefaultitem  IN bo-ped-item-sdf.
           RUN outputtable     IN bo-ped-item-sdf (OUTPUT TABLE tt-ped-item). 

           IF VALID-HANDLE(bo-ped-item-sdf) THEN DO:
               DELETE PROCEDURE bo-ped-item-sdf.
               bo-ped-item-sdf = ?.
           END.

           FIND FIRST tt-ped-item.
           /*** Altera Alguns Campos Passados *************************************************/
           ASSIGN tt-ped-item.qt-pedida               = DEC(tt-arq_ite.c_arq_ite_qt-pedida)
                  tt-ped-item.vl-preuni               = DEC(tt-arq_ite.c_arq_ite_vl-preori)
                  tt-ped-item.vl-preori               = DEC(tt-arq_ite.c_arq_ite_vl-preori)
                  tt-ped-item.des-pct-desconto-inform = "0"
                 /*tt-ped-item.qt-log-aloca            = tt-arq_ite.c_arq_ite_qt-pedida*/ 
                  tt-ped-item.dt-min-fat              = DATE(tt-arq_ped.c_arq_ped_dt-min-fat)
                  tt-ped-item.observacao              = tt-arq_ite.c_arq_ite_obs-item-ped
                  tt-ped-item.nat-operacao            = tt-arq_ite.c_arq_ite_nat-operacao
                  tt-ped-item.dt-entrega              = DATE(tt-arq_ite.c_arq_ite_dt-entrega)
                  tt-ped-item.dt-entorig              = tt-ped-item.dt-entrega

                  tt-ped-item.cod-ord-compra          = string(tt-arq_ite.c_arq_ite_ped_compr)   
                  tt-ped-item.parcela                 = int(tt-arq_ite.c_arq_ite_seq_ped_compr)

                  tt-ped-item.ind-icm-ret             = (tt-arq_ite.c_arq_ite_ind-icm-ret = "1")
                  tt-ped-item.cod-unid-negoc          = tt-arq_ite.c_cod_unid_negocio.

           /*** Valida a HANDLE do Item do Pedido *********************************************/
           RUN pi-acompanhamento("Pedido: " + STRING(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Itens API: BoDi154.p ").  

           IF NOT VALID-HANDLE(bo-ped-item) THEN
               RUN dibo/bodi154.p    PERSISTENT SET bo-ped-item.

           /*** Cria o Item do Pedido *********************************************************/
           RUN openQueryStatic IN bo-ped-item (INPUT "Main":U).
           RUN setRecord       IN bo-ped-item (INPUT TABLE tt-ped-item).
           RUN emptyRowErrors  IN bo-ped-item.
           RUN CREATERecord    IN bo-ped-item.

           RUN pi-acompanhamento("Pedido: " + STRING(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Itens Erros e Avisos").  

           /*** ERROS ***************************************************************************/
           RUN getRowErrors    IN bo-ped-item(OUTPUT TABLE RowErrors).
           IF  CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN 
           DO:
               FOR EACH rowerrors WHERE RowErrors.ErrorSubType = "ERROR":U:
                  ASSIGN tt-log-ped-item.importou = FALSE
                         tt-log-ped-item.cd-erro  = rowerrors.errornumber                        
                         tt-log-ped-item.mensagem = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription .  
               END.

               IF VALID-HANDLE(bo-ped-venda) THEN DO:
                  RUN destroyBo IN bo-ped-venda.
                   
                  IF VALID-HANDLE(bo-ped-venda) THEN DO:
                      DELETE PROCEDURE bo-ped-venda.
                      bo-ped-venda = ?.
                  END.
            
               END.
        
               IF VALID-HANDLE(bo-ped-venda-sdf) THEN DO:
                   DELETE PROCEDURE bo-ped-venda-sdf.
                   bo-ped-venda-sdf = ?.
               END.    

               UNDO PedidoTotal, RETURN "NOK".
           END.
           ELSE
           DO:
               IF  CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType <> "ERROR":U) THEN 
               DO:
                   FOR EACH rowerrors WHERE RowErrors.ErrorSubType <> "ERROR":U:

                      ASSIGN tt-log-ped-item.cd-erro      = rowerrors.errornumber                        
                             tt-log-ped-item.mensagem     = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription .  
                   END.
               END.
           END.
           
           FIND FIRST tt-ped-item.
           
           FIND FIRST esp-ped-item NO-LOCK 
           WHERE  esp-ped-item.nome-abrev        = tt-ped-item.nome-abrev
           and    esp-ped-item.nr-pedcli         = tt-ped-item.nr-pedcli
           and    esp-ped-item.nr-sequencia      = tt-ped-item.nr-sequencia
           and    esp-ped-item.it-codigo         = tt-ped-item.it-codigo
           and    esp-ped-item.cod-refer         = tt-ped-item.cod-refer no-error.

           
           if not avail esp-ped-item then do:
           
           CREATE esp-ped-item.
           ASSIGN esp-ped-item.nome-abrev        = tt-ped-item.nome-abrev
                  esp-ped-item.nr-pedcli         = tt-ped-item.nr-pedcli
                  esp-ped-item.nr-sequencia      = tt-ped-item.nr-sequencia
                  esp-ped-item.it-codigo         = tt-ped-item.it-codigo
                  esp-ped-item.cod-refer         = tt-ped-item.cod-refer
                  esp-ped-item.lance-minimo      = DEC(tt-arq_ite.c_arq_ite_lanc_minimo)
                  esp-ped-item.lance-obrigatorio = DEC(tt-arq_ite.c_arq_ite_lanc_obrigato)
                  esp-ped-item.ped-compr         = tt-arq_ite.c_arq_ite_ped_compr    
                  esp-ped-item.seq-ped-compr     = tt-arq_ite.c_arq_ite_seq_ped_compr
                  /* MODIFICADO EM 21/02/2016 POR ISRAEL ABRAHAO A PEDIDO DO SR. JUNIOR
                  esp-ped-item.per-sub-icm-trib  = DEC(tt-arq_ite.c_arq_ite_per_st_icm)      
                  esp-ped-item.per-icm-estad-sub = DEC(tt-arq_ite.c_arq_ite_per_icm_estad_st)
                  */
                  esp-ped-item.per-sub-icm-trib  = ?      
                  esp-ped-item.per-icm-estad-sub = ?.
                  
            end.
           
           ASSIGN tt-log-ped-item.importou     = TRUE.

           /* Wanderley - 4make - 11/2015 - Grava Subst Tribut Item/Unidade Federaá∆o - reitrada a logica 04/01/2016-rmk*/
/*                                                                                 */
           RUN pi-grava-subt (INPUT tt-arq_ped.c_arq_ped_cod-estabel,
                              INPUT tt-ped-item.it-codigo,
                              INPUT DEC(tt-arq_ite.c_arq_ite_per_st_icm),
                              INPUT DEC(tt-arq_ite.c_arq_ite_per_icm_estad_st),
                              INPUT INT(tt-arq_ite.c_arq_ped_cod-emitente)).

           
           
           /*** Deleta a HANDLE do Item do Pedido ************************************************/
           IF VALID-HANDLE(bo-ped-item) THEN DO:
               RUN destroyBo IN bo-ped-item.

               IF VALID-HANDLE(bo-ped-item) THEN DO:
                   DELETE PROCEDURE bo-ped-item.
                   bo-ped-item = ?.
               END.
           END.
        END.

        /*** Calcula o Pedido ***********************************************/
        RUN pi-acompanhamento("Pedido - Calculando Pedido").  
        RUN pi-calcula-Pedido.
        IF RETURN-VALUE = "NOK" THEN
            RETURN "NOK".

        /*** Efetiva o Pedido ***********************************************/
        RUN pi-acompanhamento("Pedido - Efetivando Pedido").  
        RUN pi-efetiva-Pedido.
        IF RETURN-VALUE = "NOK" THEN
            RETURN "NOK".

        /*** Elimina o Pedido da Intermediaria *******************************/
        RUN pi-acompanhamento("Terminando Processamento").  

        FIND FIRST ped-venda WHERE ped-venda.nome-abrev = emitente.nome-abrev
                               AND ped-venda.nr-pedcli  = tt-arq_ped.c_arq_ped_nr-pedcli NO-ERROR.

        IF AVAIL ped-venda THEN
        DO:
           /*** acerta qt-un-fat ***/
           
           FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
               ASSIGN ped-item.qt-un-fat = ped-item.qt-pedida.
           END.

           /***********************/

           IF ped-venda.cod-estabel <> "716" THEN
              DO:
            /**********CFF - Alocaá∆o de Lote 25/02/2015*******************************/
                 if can-FIND (FIRST ped-ent OF ped-venda) AND CAN-FIND (FIRST tt-arq_lote) THEN DO:
                 
                     IF NOT VALID-HANDLE(bo-ped-venda) THEN
                         RUN dibo/bodi159.p    PERSISTENT SET bo-ped-venda.
                 
                     run emptyRowErrors     in bo-ped-venda.
                     run validateAllocation in bo-ped-venda(INPUT ROWID(ped-venda)).
                     run getRowErrors       in bo-ped-venda(output table RowErrors).
                 
                     if  can-FIND(FIRST RowErrors WHERE RowErrors.ErrorType <> "INTERNAL":U) THEN DO:
                 
                          FOR EACH RowErrors NO-LOCK:
                              ASSIGN tt-log-ped-venda.cd-erro    = RowErrors.errornumber                        
                                     tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + RowErrors.errordescription .
                          END.
                     END.
                     ELSE DO:
                         FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
                             FIND FIRST ped-ent WHERE ped-ent.nome-abrev     = ped-venda.nome-abrev  
                                                  and ped-ent.nr-pedcli      = ped-venda.nr-pedcli   
                                                  and ped-ent.it-codigo      = ped-item.it-codigo   
                                                  and ped-ent.cod-refer      = ped-item.cod-refer   
                                                  and ped-ent.nr-sequencia   = ped-item.nr-sequencia NO-LOCK NO-ERROR.
                             IF AVAIL ped-ent THEN DO:
                                 FIND FIRST ITEM WHERE ITEM.it-codigo = ped-ent.it-codigo NO-LOCK NO-ERROR.
                                 
                                 FOR EACH tt-arq_lote WHERE tt-arq_lote.nome-abrev   = ped-venda.nome-abrev
                                                        and tt-arq_lote.nr-pedcli    = ped-venda.nr-pedcli 
                                                        and tt-arq_lote.nr-sequencia = ped-item.nr-sequencia 
                                                        and tt-arq_lote.it-codigo    = ped-item.it-codigo
                                                        and tt-arq_lote.cod-refer    = ped-item.cod-refer NO-LOCK:
                                 
                                      FOR EACH  saldo-estoq WHERE saldo-estoq.cod-depos   = tt-arq_lote.cod-depos                         
                                                              AND saldo-estoq.cod-estabel = ped-venda.cod-estabel 
                                                              AND saldo-estoq.cod-localiz = tt-arq_lote.cod-localiz
                                                              AND saldo-estoq.lote        = tt-arq_lote.codigo
                                                              and saldo-estoq.it-codigo   = ped-ent.it-codigo                                
                                                              and saldo-estoq.cod-refer   = ped-ent.cod-refer                                
                                                              and (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                                              saldo-estoq.qt-aloc-ped +                      
                                                                                              saldo-estoq.qt-aloc-prod)) > 0
                                           AND (ITEM.tipo-con-est < 3  OR saldo-estoq.dt-vali-lote >= ped-ent.dt-entrega) NO-LOCK,
                                         FIRST deposito WHERE deposito.cod-depos = saldo-estoq.cod-depos                                 
                                                         and deposito.ind-acabado NO-LOCK:   
                 
                                          IF (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                         saldo-estoq.qt-aloc-ped +                      
                                                                         saldo-estoq.qt-aloc-prod)) >= tt-arq_lote.quantidade  THEN DO:
                                             
                                             /*  run pi-aloca-fisica-man (INPUT ROWID(ped-ent), */
/*                                                                        INPUT-OUTPUT tt-arq_lote.quantidade, */
/*                                                                        INPUT ROWID(saldo-estoq)). */ /*retirada esta logica devido a implamtacao do dwpd002*/
                                          END.
                                          ELSE
                                              ASSIGN tt-log-ped-venda.cd-erro    = 99999                        
                                                     tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + "Quantidade informada do lote maior que quantiade disponivel." .
                                      END.                                                                                
                                 END.
                             END.
                         END.
                     END.
                 END.
                 
                 IF VALID-HANDLE (h-alocacao) THEN DO:
                     DELETE PROCEDURE h-alocacao.
                        h-alocacao = ?.
                 END.
                 IF VALID-HANDLE(bo-ped-venda) THEN DO:
                       RUN destroyBo IN bo-ped-venda.
                        
                       IF VALID-HANDLE(bo-ped-venda) THEN DO:
                           DELETE PROCEDURE bo-ped-venda.
                           bo-ped-venda = ?.
                       END.
                 
                 END.
              END.
           
            if emitente.estado = "PR" and ped-venda.cod-estabel = "716" THEN
               run pi-copia-Pedido (INPUT ROWID(ped-venda)).
        END.     

        ASSIGN tt-log-ped-venda.importou = TRUE.
        release ped-venda.
        release ped-item.
        release ped-ent.
        release emitente.
        release cont-emit.
        release dw-ped-venda.
        release dw-follow-up-ped.

        
    END. /* Transaction */
    

    RETURN "NOK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-calcula-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-Pedido PROCEDURE 
PROCEDURE pi-calcula-Pedido :
/*** pi-Calcula-Pedido
**** Calcula valores Totais do Pedido de Venda 
********************************************************************************/
    IF  NOT VALID-HANDLE(bo-ped-venda-cal) THEN
        RUN dibo/bodi159cal.p PERSISTENT SET bo-ped-venda-cal.

    FIND FIRST tt-ped-venda.

    RUN calculateOrder IN bo-ped-venda-cal(INPUT tt-ped-venda.r-ROWID).

    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN 
    DO:
       
        FOR EACH RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U:

            ASSIGN tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.errorDescription
                   tt-log-ped-venda.cd-erro  = RowErrors.errorNumber
                   tt-log-ped-venda.importou = FALSE.
       END.

       RETURN "NOK":U.

    END.

    IF VALID-HANDLE(bo-ped-venda-cal) THEN DO:
        DELETE PROCEDURE bo-ped-venda-cal.
        bo-ped-venda-cal = ?.
    END.

    RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-pendencia-rpw) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-pendencia-rpw PROCEDURE 
PROCEDURE pi-cria-pendencia-rpw :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR h-api  AS HANDLE     NO-UNDO.
    DEF VAR i_hora AS INT    NO-UNDO.
    DEF VAR dt_tim AS DATE       NO-UNDO.    
    
    IF NOT tt-PARAM.l-execucao-cont THEN
        RETURN "OK".
        
    FIND usuar_mestre WHERE usuar_mestre.cod_usuar = tt-PARAM.usuario NO-LOCK NO-ERROR.
    
    FOR EACH tt_param_segur.        DELETE tt_param_segur.        END.
    FOR EACH tt_ped_exec_aux_3.     DELETE tt_ped_exec_aux_3.     END.
    FOR EACH tt_ped_exec_PARAM.     DELETE tt_ped_exec_PARAM.     END.
    FOR EACH tt_ped_exec_sel.       DELETE tt_ped_exec_sel.       END.
    FOR EACH tt_ped_exec_PARAM_aux. DELETE tt_ped_exec_PARAM_aux. END.
    
    CREATE tt_param_segur.
    ASSIGN tt_param_segur.tta_num_vers_integr_api       = 3
           tt_param_segur.tta_cod_aplicat_dtsul_corren  = "TEC"
           tt_param_segur.tta_cod_empres_usuar          = STRING(i-ep-codigo-usuario)
           tt_param_segur.tta_cod_grp_usuar_lst         = "sup"
           tt_param_segur.tta_cod_idiom_usuar           = "POR"
           tt_param_segur.tta_cod_pais_empres_usuar     = "BRA"
           tt_param_segur.tta_cod_usuar_corren          = tt-PARAM.usuario
           tt_param_segur.tta_cod_usuar_corren_criptog  = ENCODE(tt-PARAM.usuario).
    
    ASSIGN i_hora = TIME + i-minutos * 60.
    
    IF i_hora > 86399 THEN
        ASSIGN i_hora = i_hora - 86399
               dt_tim = TODAY + 1.
    ELSE
        ASSIGN dt_tim = TODAY.
    
    CREATE tt_ped_exec_aux_3.
    ASSIGN tt_ped_exec_aux_3.tta_num_seq                = 1
           tt_ped_exec_aux_3.tta_num_ped_exec           = NEXT-VALUE(seq_ped_exec,emsfnd)
           tt_ped_exec_aux_3.tta_cod_usuario            = tt-PARAM.usuario
           tt_ped_exec_aux_3.tta_cod_prog_dtsul         = "escrm004"
           tt_ped_exec_aux_3.tta_cod_prog_dtsul_rp      = "esp/escrm004rp.p"
           tt_ped_exec_aux_3.tta_cod_release_prog_dtsu  = "2.04.00.001"
           tt_ped_exec_aux_3.tta_dat_exec_ped_exec      = dt_tim
           tt_ped_exec_aux_3.tta_hra_exec_ped_exec      = REPLACE(STRING(i_hora,"hh:mm:ss"),":","")
           tt_ped_exec_aux_3.tta_cod_servid_exec        = usuar_mestre.cod_servid_exec
           tt_ped_exec_aux_3.tta_cdn_estil_dwb          = 97.
    
    CREATE tt_ped_exec_PARAM.
    ASSIGN tt_ped_exec_PARAM.tta_num_seq                = 1
           tt_ped_exec_PARAM.tta_cod_dwb_file           = tt-PARAM.arq-destino
           tt_ped_exec_PARAM.tta_cod_dwb_output         = "Arquivo".
  
    FIND FIRST tt-PARAM NO-LOCK NO-ERROR.
  
    RAW-TRANSFER tt-PARAM TO tt_ped_exec_PARAM.tta_raw_PARAM_ped_exec.

    RUN btb/btb912zc.p PERSISTENT SET h-api.
    
    RUN pi-execute-3 IN h-api (INPUT-OUTPUT TABLE tt_param_segur,
                               INPUT-OUTPUT TABLE tt_ped_exec_aux_3,
                               INPUT        TABLE tt_ped_exec_PARAM,
                               INPUT        TABLE tt_ped_exec_PARAM_aux,
                               INPUT        TABLE tt_ped_exec_sel,
                               OUTPUT       TABLE tt_erros_envio_email).
                               
    DELETE PROCEDURE h-api.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-efetiva-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-efetiva-Pedido PROCEDURE 
PROCEDURE pi-efetiva-Pedido :
/*** pi-Efetiva-Pedido
**** Efetiva o Pedido de Venda, ou seja, Completa
********************************************************************************/

    FIND FIRST tt-ped-venda.
    
    IF NOT VALID-HANDLE(bo-ped-venda-com) THEN
        RUN dibo/bodi159com.p PERSISTENT SET bo-ped-venda-com.
    
    RUN completeOrder IN bo-ped-venda-com(INPUT  tt-ped-venda.r-ROWID,
                                          OUTPUT TABLE RowErrors).
    
    RUN destroyBO IN bo-ped-venda-com.
    DELETE PROCEDURE bo-ped-venda-com.
    bo-ped-venda-com = ?.    

    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U ) THEN 
    DO:
        FOR EACH RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U:

            ASSIGN tt-log-ped-venda.importou = FALSE
                   tt-log-ped-venda.cd-erro  = RowErrors.errorNumber
                   tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.errorDescription.
        END.

        RETURN "NOK":U.
    END.
    
    IF NOT CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U)  THEN
       ASSIGN tt-ped-venda.completo = YES.
    
    RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-gera-arquivo-resposta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-arquivo-resposta PROCEDURE 
PROCEDURE pi-gera-arquivo-resposta :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-contador AS INT    NO-UNDO.
    DEF VAR c-destino  AS CHAR  NO-UNDO.

    i-contador = (TIME MOD 3600) MOD 60. 
    REPEAT:
        c-destino =  param-integra.dir-arq-resp 
                    + "/" + STRING(YEAR(TODAY),"9999") + 
                    STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "resposta" +
                    REPLACE(STRING(TIME,"HH:MM"),":","") + STRING(i-contador,"99") + ".txt".
    
        FILE-INFO:FILE-NAME = c-destino.
    
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            LEAVE.
        ELSE
            i-contador = i-contador + 1.
    END.

/*     ASSIGN c-destino = 'c:\temp\4make\destino.txt'. */

    OUTPUT STREAM str-rp TO VALUE(c-destino) NO-CONVERT.

    IF l-arquivo-valido THEN DO:

        FIND FIRST tt-erro-copia NO-LOCK NO-ERROR.
        IF AVAIL tt-erro-copia THEN
           DO:
              EXPORT STREAM str-rp
                     DELIMITER ";" tt-erro-copia.cod-emitente
                                   tt-erro-copia.nr-pedcli
                                   tt-erro-copia.importou
                                   "Pedido Criado mas n∆o completado - " + TRIM(REPLACE(REPLACE(tt-erro-copia.errorDescription,CHR(13)," "),CHR(10),"")).

              FOR EACH tt-log-ped-item:
                  EXPORT STREAM str-rp
                       DELIMITER ";" tt-log-ped-item.nr-sequencia
                                     tt-log-ped-item.it-codigo
                                     tt-log-ped-item.importou
                                     REPLACE(REPLACE(tt-log-ped-item.mensagem,CHR(13)," "),CHR(10),"").
              END.
           END.
        ELSE
           DO:
              FIND FIRST tt-log-ped-venda NO-LOCK NO-ERROR.
              if AVAIL tt-log-ped-venda THEN DO:
                  FIND emitente WHERE emitente.nome-abrev = tt-log-ped-venda.nome-abrev NO-LOCK NO-ERROR.
              
                  EXPORT STREAM str-rp
                       DELIMITER ";" (IF AVAIL emitente THEN emitente.cod-emitente ELSE INT(tt-arq_ped.c_arq_ped_cod-emitente) )
                                     tt-log-ped-venda.nr-pedcli
                                     tt-log-ped-venda.importou
                                     REPLACE(REPLACE(tt-log-ped-venda.mensagem,CHR(13)," "),CHR(10),"").
                  FOR EACH tt-log-ped-item:
                      EXPORT STREAM str-rp
                           DELIMITER ";" tt-log-ped-item.nr-sequencia
                                         tt-log-ped-item.it-codigo
                                         tt-log-ped-item.importou
                                         REPLACE(REPLACE(tt-log-ped-item.mensagem,CHR(13)," "),CHR(10),"").
                  END.
               END.   
           END.
    END.
    ELSE DO:
        FIND FIRST tt-arq_ped NO-LOCK NO-ERROR.

        FIND emitente WHERE emitente.cod-emitente = INT(tt-arq_ped.c_arq_ped_cod-emitente) NO-LOCK NO-ERROR.

        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.nome-abrev = (IF AVAIL emitente THEN emitente.nome-abrev ELSE "")
               tt-log-ped-venda.nr-pedcli  = (IF AVAIL tt-arq_ped THEN tt-arq_ped.c_arq_ped_nr-pedcli    ELSE "")
               tt-log-ped-venda.importou   = NO
               tt-log-ped-venda.mensagem   = "ERRO: N∆o foi possivel importar o aquivo, pois o mesmo est† corrompido ***.".

        EXPORT STREAM str-rp
             DELIMITER ";" (IF AVAIL tt-arq_ped THEN INT(tt-arq_ped.c_arq_ped_cod-emitente) ELSE 0)
                           (IF AVAIL tt-arq_ped THEN tt-arq_ped.c_arq_ped_nr-pedcli    ELSE "")
                           NO
                           "ERRO: N∆o foi possivel importar o aquivo, pois o mesmo est† corrompido ***.".
    END.

    OUTPUT STREAM str-rp CLOSE.

    RUN pi-imprime-log.

    OS-DELETE VALUE(c-dir-importacao + "/" + c-file) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-grava-log-peditem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-log-peditem PROCEDURE 
PROCEDURE pi-grava-log-peditem :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    ASSIGN tt-log-ped-item.cod-estabel     = tt-arq_ite.c_arq_ite_cod-estabel          
           tt-log-ped-item.tipo-transacao  = 1
           tt-log-ped-item.nat-operacao    = tt-arq_ite.c_arq_ite_nat-operacao
           tt-log-ped-item.qt-pedida       = DEC(tt-arq_ite.c_arq_ite_qt-pedida)
           tt-log-ped-item.vl-preori       = DEC(tt-arq_ite.c_arq_ite_vl-preori)
           tt-log-ped-item.vl-preuni       = DEC(tt-arq_ite.c_arq_ite_vl-preori)
           tt-log-ped-item.per-des-item    = 0
           tt-log-ped-item.nr-tabpre       = tt-arq_ite.c_arq_ite_nr-tabpre
           tt-log-ped-item.tb-preco        = INT(tt-arq_ite.c_arq_ite_tb-preco)
           tt-log-ped-item.vl-total        = DEC(tt-arq_ite.c_arq_ite_vl-total)
           tt-log-ped-item.perc-icms       = DEC(tt-arq_ite.c_arq_ite_perc-icms)
           tt-log-ped-item.obs-item-ped    = tt-arq_ite.c_arq_ite_obs-item-ped.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-grava-log-pedvenda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-log-pedvenda PROCEDURE 
PROCEDURE pi-grava-log-pedvenda :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN tt-log-ped-venda.cod-estabel      = tt-arq_ped.c_arq_ped_cod-estabel
           tt-log-ped-venda.emergencial      = INT(tt-arq_ped.c_arq_ped_emergencial)
           tt-log-ped-venda.tipo-transacao   = 1                    
           tt-log-ped-venda.dt-implant       = TODAY
           tt-log-ped-venda.dt-entrega       = DATE(tt-arq_ped.c_arq_ped_dt-entrega)
           tt-log-ped-venda.nat-operacao     = tt-arq_ped.c_arq_ped_nat-operacao
           tt-log-ped-venda.cod-cond-pag     = INT(tt-arq_ped.c_arq_ped_cod-cond-pag)
           tt-log-ped-venda.nr-tabpre        = tt-arq_ped.c_arq_ped_nr-tabpre
           tt-log-ped-venda.cod-priori       = INT(tt-arq_ped.c_arq_ped_cod-priori)
           tt-log-ped-venda.perc-desco1      = 0                            
           tt-log-ped-venda.perc-desco2      = 0
           tt-log-ped-venda.observacoes      = tt-arq_ped.c_arq_ped_observacoes
           tt-log-ped-venda.nome-transp      = tt-arq_ped.c_arq_ped_nome-transp
           tt-log-ped-venda.tb-preco         = INT(tt-arq_ped.c_arq_ped_tb-preco)
           tt-log-ped-venda.mo-codigo        = INT(tt-arq_ped.c_arq_ped_mo-codigo)
           tt-log-ped-venda.no-ab-reppri     = tt-arq_ped.c_arq_ped_no-ab-reppri
           tt-log-ped-venda.vl-desconto      = DEC(tt-arq_ped.c_arq_ped_vl-desconto)
           tt-log-ped-venda.nome-ab-rep      = tt-arq_ped.c_arq_ped_nome-ab-rep
           tt-log-ped-venda.cod-entrega      = tt-arq_ped.c_arq_ped_cod-entrega
           tt-log-ped-venda.tipo-fatur       = INT(tt-arq_ped.c_arq_ped_tipo-fatur)
           tt-log-ped-venda.dt-min-fat       = DATE(tt-arq_ped.c_arq_ped_dt-min-fat)
           tt-log-ped-venda.transp-redesp    = tt-arq_ped.c_arq_ped_transp-redesp
           tt-log-ped-venda.nr-pedrep        = tt-arq_ped.c_arq_ped_nr-pedrep
           tt-log-ped-venda.cidade-cif       = tt-arq_ped.c_arq_ped_cidade-cif
           tt-log-ped-venda.cod-priori-log   = int(tt-arq_ped.c_arq_ped_cod_priori_log)
           tt-log-ped-venda.modal-doc-fiscal = tt-arq_ped.c_arq_ped_modal_doc_fiscal.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-imprime-log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-log PROCEDURE 
PROCEDURE pi-imprime-log :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMeters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-diretorio     AS CHAR FORMAT "x(40)"              NO-UNDO.
    DEF VAR c-format    AS CHAR.
    DEF VAR i-len       AS INT.

    /*** Se nao Houver Nenhum Log **********************************/
    FIND FIRST tt-log-ped-venda NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-log-ped-venda THEN RETURN.

    /*** Gera os Logs **********************************************/
    ASSIGN c-diretorio = param-integra.dir-arq-err 
                  + "/Pedido-Inc - " + 
                                              STRING(DAY(TODAY),"99")    + 
                                              STRING(MONTH(TODAY),"99")  + 
                                              STRING(YEAR(TODAY),"9999") + ".txt".
    
    /*ASSIGN c-diretorio = 'c:\temp\4make\Pedido-inc.txt'.*/

    OUTPUT STREAM str-log TO VALUE(c-diretorio) APPEND NO-CONVERT.

    FOR EACH tt-log-ped-venda:
        
        /*** Log ***************************************************************/
        ASSIGN i-len    = LENGTH(tt-log-ped-venda.mensagem)
               c-format = IF i-len > 0 THEN "x(" + STRING(i-len) + ")"
                          ELSE "x(1)"
               c-format = "x(100)".

        /*** Localiza o Pedido vindo do CRM ************************************/

        PUT STREAM str-log
            SKIP(1)
            "Nome Abrev   Nr Pedcli    Imp Data       Hora       Mensagem                                                                                            " 
            "Est Nr Cotacao Trans Dt Implant Dt Entrega Nat Op Cond Pagto Tab Pre Priori   Desc1    Desc2 Observacao           Transp       Tb Preco Moeda Repre Princ    Vl Desc " 
            "Repres       Cod Entrega Tp Fatur Dt Min Fat Transp Redesp Nr PedRep    CIF/FOB    Prior Log" Skip
            "------------ ------------ --- ---------- ---------- ----------------------------------------------------------------------------------------------------" Skip
            tt-log-ped-venda.nome-abrev     " "
            tt-log-ped-venda.nr-pedcli      " "
            tt-log-ped-venda.importou       " "
            tt-log-ped-venda.data-import    " " 
            tt-log-ped-venda.hora-import    " "
            tt-log-ped-venda.mensagem       FORMAT c-format " " 
            tt-log-ped-venda.cod-estabel    " "
            tt-log-ped-venda.emergencial     "   "
            tt-log-ped-venda.tipo-transacao "   "
            tt-log-ped-venda.dt-implant     " "
            tt-log-ped-venda.dt-entrega     " "
            tt-log-ped-venda.nat-operacao   "  "
            tt-log-ped-venda.cod-cond-pag   "         "
            tt-log-ped-venda.nr-tabpre      " "
            tt-log-ped-venda.cod-priori     "  "
            tt-log-ped-venda.perc-desco1    "  "
            tt-log-ped-venda.perc-desco2    " "
            tt-log-ped-venda.observacoes    FORMAT "x(20)"   " "
            tt-log-ped-venda.nome-transp    " "
            tt-log-ped-venda.tb-preco       "       "
            tt-log-ped-venda.mo-codigo      "    "
            tt-log-ped-venda.no-ab-reppri   " "
            tt-log-ped-venda.vl-desconto    " "
            tt-log-ped-venda.nome-ab-rep    " "
            tt-log-ped-venda.cod-entrega    "  "
            tt-log-ped-venda.tipo-fatur     "      "
            tt-log-ped-venda.dt-min-fat     " "
            tt-log-ped-venda.transp-redesp  "  "
            tt-log-ped-venda.nr-pedrep      " "
            tt-log-ped-venda.cidade-cif     " "
            tt-log-ped-venda.cod-priori-log " "
            tt-log-ped-venda.modal-doc-fiscal " "
            SKIP.

        FOR EACH tt-log-ped-item WHERE tt-log-ped-item.nome-abrev = tt-log-ped-venda.nome-abrev 
                                   AND tt-log-ped-item.nr-pedcli  = tt-log-ped-venda.nr-pedcli NO-LOCK
            BREAK BY tt-log-ped-item.nome-abrev
                  BY tt-log-ped-item.nr-pedcli:

            IF FIRST-OF(tt-log-ped-item.nome-abrev) AND
               FIRST-OF(tt-log-ped-item.nr-pedcli)  THEN
               PUT STREAM str-log
                    "Nr Sequencia    Item                      Imp  Mensagem                                                                                        " AT 10
                    "Est Trans Nat op    Quantidade      Preco Orig  Preco Unitario  Perc Desc  Tab Pre Tb Preco   Valor Total      Perc Icms Observacao Item "   Skip
                    "--------------- ------------------------- ---- ------------------------------------------------------------------------------------------------"  At 10 Skip.

            ASSIGN i-len    = LENGTH(tt-log-ped-item.mensagem)
                   c-format = IF i-len > 0 THEN "x(" + STRING(i-len) + ")"
                              ELSE "x(1)"
                   c-format = "x(97)".

            PUT STREAM str-log
                tt-log-ped-item.nr-sequencia    AT 19 " "
                tt-log-ped-item.it-codigo       " "
                tt-log-ped-item.cod-refer       " "
                tt-log-ped-item.importou        "  "
                tt-log-ped-item.mensagem        FORMAT c-format 
                tt-log-ped-item.cod-estabel     " "
                tt-log-ped-item.tipo-transacao  "    "
                tt-log-ped-item.nat-operacao    " "
                tt-log-ped-item.qt-pedida       " "
                tt-log-ped-item.vl-preori       " "
                tt-log-ped-item.vl-preuni       " "
                tt-log-ped-item.per-des-item    "      "
                tt-log-ped-item.nr-tabpre       " "
                tt-log-ped-item.tb-preco        " "
                tt-log-ped-item.vl-total        " "
                tt-log-ped-item.perc-icms       " "
                tt-log-ped-item.obs-item-ped    " "
                SKIP.
        END.
    END.

    OUTPUT STREAM str-log CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-validate-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate-Pedido PROCEDURE 
PROCEDURE pi-validate-Pedido :
/*****************************************************************************************/
    /*** Validacoes Necessarias **************************************************************/
    /*****************************************************************************************/
    DEF VAR l-erros-item-ped AS LOG    NO-UNDO.

    RUN pi-acompanhamento("Validando Cliente").
    FIND FIRST emitente WHERE emitente.cod-emitente = INT(tt-arq_ped.c_arq_ped_cod-emitente) NO-LOCK NO-ERROR.
    
    IF NOT AVAIL emitente THEN 
    DO:    
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Cliente nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = ""
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.

    FIND FIRST ped-venda WHERE ped-venda.nome-abrev = emitente.nome-abrev
                           AND ped-venda.nr-pedcli  = tt-arq_ped.c_arq_ped_nr-pedcli NO-LOCK NO-ERROR.

    IF AVAIL ped-venda THEN  DO:
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Pedido j† Cadastrado."
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK". /*  se ja existir o Pedido Ira Retornar e depois no ES000 ira Modificar */
    END.

    /*** REpresentante INDIRETO *******************************/
    RUN pi-acompanhamento("Validando Representante").

    ASSIGN c-nome-rep-indireto = "".
    FIND FIRST repres WHERE repres.nome-abrev = tt-arq_ped.c_arq_ped_no-ab-reppri NO-LOCK NO-ERROR.

    IF NOT AVAIL repres THEN 
    DO:
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Representante nao Cadastrado INDIRETO"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.
    ASSIGN c-nome-rep-indireto = repres.nome-abrev.

    /*** REpresentante DIRETO *****************************************/
    ASSIGN c-nome-rep-direto = "".
    FIND FIRST repres WHERE repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

    IF NOT AVAIL repres THEN 
    DO:
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Representante do Cliente nao Cadastrado DIRETO"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.
    ASSIGN c-nome-rep-direto = repres.nome-abrev.

    RUN pi-acompanhamento("Validando Estabelecimento").
    
    FIND FIRST estabelec WHERE estabelec.cod-estabel = tt-arq_ped.c_arq_ped_cod-estabel NO-LOCK NO-ERROR.

    IF NOT AVAIL estabelec THEN
    DO:
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Estabelecimento nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".              
               
    END.   

    RUN pi-acompanhamento("Validando Condicao de Pagamento").
    /*Verifica se a natureza eh de transferencia - Cezar Freire 19/11/2012*/
    FIND FIRST natur-oper WHERE natur-oper.nat-operacao =  tt-arq_ped.c_arq_ped_nat-operacao NO-LOCK NO-ERROR.
    if AVAIL natur-oper THEN DO:
       if natur-oper.transf = no THEN DO:

            FIND FIRST cond-pagto WHERE cond-pagto.cod-cond-pag = INT(tt-arq_ped.c_arq_ped_cod-cond-pag) NO-LOCK NO-ERROR.
        
            IF NOT AVAIL cond-pagto THEN
            DO:
                CREATE tt-log-ped-venda.
                ASSIGN tt-log-ped-venda.mensagem    = "Condicao de Pagamento nao Cadastrado"
                       tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
                       tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                       tt-log-ped-venda.importou    = FALSE
                       tt-log-ped-venda.data-import = TODAY
                       tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").
        
                RUN pi-grava-log-pedvenda.
        
                RETURN "NOK".
            END.
        END. 
    END.    

    RUN pi-acompanhamento("Validando Transportadora").
    
    ASSIGN c-nome-redespacho = "".        
    FIND FIRST transporte WHERE transporte.nome-abrev = tt-arq_ped.c_arq_ped_transp-redesp NO-LOCK NO-ERROR.
	
    IF NOT AVAIL transporte THEN
    DO:
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Transportadora de Redespacho nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.    

    ASSIGN c-nome-redespacho = transporte.nome-abrev.

    FIND FIRST transporte WHERE transporte.nome-abrev = tt-arq_ped.c_arq_ped_nome-transp NO-LOCK NO-ERROR.

    IF NOT AVAIL transporte THEN
    DO:
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Transportadora nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.    

    RUN pi-acompanhamento("Validando Natureza de Operacao").
    
    FIND FIRST natur-oper WHERE natur-oper.nat-operacao = tt-arq_ped.c_arq_ped_nat-operacao NO-LOCK NO-ERROR.

    IF NOT AVAIL natur-oper THEN
    DO: 
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Natureza de Operacao nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.

    RUN pi-acompanhamento("Validando Existencia de Itens no Pedido").
        
    FIND FIRST tt-arq_ite WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
                            AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-arq_ite THEN 
    DO:
        l-erros-item-ped = YES.
        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.mensagem    = "Nenhum Item para o Pedido foi Encontrato, impossivel continuar!"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
               tt-log-ped-venda.importou    = FALSE
               tt-log-ped-venda.data-import = TODAY
               tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

        RUN pi-grava-log-pedvenda.

        RETURN "NOK".
    END.

    RUN pi-acompanhamento("Validando Natureza de Operacao do Item do Pedido").

    FOR EACH tt-arq_ite WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
                          AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli NO-LOCK:

        FIND FIRST natur-oper WHERE natur-oper.nat-operacao = tt-arq_ite.c_arq_ite_nat-operacao NO-LOCK NO-ERROR.
        
        IF NOT AVAIL natur-oper THEN DO: 
             
           CREATE tt-log-ped-item.
           ASSIGN tt-log-ped-item.mensagem     = "Natureza de Operacao do Item nao Cadastrada"
                  tt-log-ped-item.nome-abrev   = emitente.nome-abrev 
                  tt-log-ped-item.nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli
                  tt-log-ped-item.it-codigo    = tt-arq_ite.c_arq_ite_it-codigo   
                  tt-log-ped-item.cod-refer    = "" 
                  tt-log-ped-item.nr-sequencia = INT(tt-arq_ite.c_arq_ite_nr-sequencia)
                  tt-log-ped-item.importou     = FALSE.
                  
           CREATE tt-log-ped-venda.
           ASSIGN tt-log-ped-venda.mensagem    = "Natureza de Operacao do Item nao Cadastrada"
                  tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                  tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                  tt-log-ped-venda.importou    = FALSE
                  tt-log-ped-venda.data-import = TODAY
                  tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").      
        
           RUN pi-grava-log-peditem.
        
           RETURN "NOK".
        
        END.
    END.

    /*** Verifica Erros nos Itens do Pedido ***************************************************/    
    RUN pi-acompanhamento("Validando Erros nos Itens do Pedido").

    ASSIGN l-erros-item-ped = NO.
    FOR EACH tt-arq_ite WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
                          AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli NO-LOCK :
       
       FIND FIRST ITEM WHERE ITEM.it-codigo = tt-arq_ite.c_arq_ite_it-codigo NO-LOCK NO-ERROR .

       IF NOT AVAIL ITEM THEN
       DO:
          l-erros-item-ped = YES.
          CREATE tt-log-ped-venda.
          ASSIGN tt-log-ped-venda.mensagem    = "Item do Pedido nao Cadastrado, impossovel Criar o Pedido: " + STRING(tt-arq_ite.c_arq_ite_it-codigo)
                 tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                 tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                 tt-log-ped-venda.importou    = FALSE
                 tt-log-ped-venda.data-import = TODAY
                 tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

          RUN pi-grava-log-pedvenda.
    
          RETURN "NOK".       

       END.       

       IF ITEM.ind-item-fat = NO THEN
       DO:
           l-erros-item-ped = YES.
           CREATE tt-log-ped-venda.
           ASSIGN tt-log-ped-venda.mensagem    = "Item nao Faturavel, impossivel Criar o Pedido: " + STRING(ITEM.it-codigo)
                  tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                  tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                  tt-log-ped-venda.importou    = FALSE
                  tt-log-ped-venda.data-import = TODAY
                  tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

           RUN pi-grava-log-pedvenda.

           RETURN "NOK".       
       END.

       FIND FIRST item-uni-estab WHERE item-uni-estab.it-codigo   = ITEM.it-codigo
                                   AND item-uni-estab.cod-estabel = tt-arq_ite.c_arq_ite_cod-estabel NO-LOCK NO-ERROR.
       IF NOT AVAIL item-uni-estab THEN
       DO:
          l-erros-item-ped = YES.
          CREATE tt-log-ped-venda.
          ASSIGN tt-log-ped-venda.mensagem    = "Item Uni Estab nao Cadastrado, impossovel Criar o Pedido: " + STRING(ITEM.it-codigo) + " Est: " + tt-arq_ite.c_arq_ite_cod-estabel
                 tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                 tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                 tt-log-ped-venda.importou    = FALSE
                 tt-log-ped-venda.data-import = TODAY
                 tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

          RUN pi-grava-log-pedvenda.
          
          RETURN "NOK".
       END.

       IF item-uni-estab.ind-item-fat = NO THEN
       DO:
           l-erros-item-ped = YES.
           CREATE tt-log-ped-venda.
           ASSIGN tt-log-ped-venda.mensagem    = "Item Uni Estab nao Faturavel, impossivel Criar o Pedido: " + STRING(ITEM.it-codigo)
                  tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                  tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                  tt-log-ped-venda.importou    = FALSE
                  tt-log-ped-venda.data-import = TODAY
                  tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

           RUN pi-grava-log-pedvenda.

           RETURN "NOK".
       END.
       
       /* DESABILITADO EM 21/02/16 POR ISRAEL ABRAHAO A PEDIDO DO SR JUNIOR
       **********************************************************************
       /* --- validacoes de St - implementadas no dia 07/11/2014 por Israel Abrahao - 4Make --- */

       /* --- 1a validacao --- */
       IF  tt-arq_ite.c_arq_ite_ind-icm-ret <> "1"              AND 
          (DEC(tt-arq_ite.c_arq_ite_per_st_icm)       <> ?  OR
           DEC(tt-arq_ite.c_arq_ite_per_icm_estad_st) <> ?) THEN
       DO:
           l-erros-item-ped = YES.
           CREATE tt-log-ped-venda.
           ASSIGN tt-log-ped-venda.mensagem    = "% Subst Trib ICMS e ICMS Estadual Subst Trib devem ser informados com interrogacao, impossivel Criar o Pedido: " + STRING(ITEM.it-codigo)
                  tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                  tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                  tt-log-ped-venda.importou    = FALSE
                  tt-log-ped-venda.data-import = TODAY
                  tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").

           RUN pi-grava-log-pedvenda.

           RETURN "NOK".
       END.
       
       IF tt-arq_ite.c_arq_ite_ind-icm-ret = "1" THEN
       DO:
          /* --- 2a validacao --- */
          IF DEC(tt-arq_ite.c_arq_ite_per_icm_estad_st) = 0 THEN
          DO:
              l-erros-item-ped = YES.
              CREATE tt-log-ped-venda.
              ASSIGN tt-log-ped-venda.mensagem    = "Quando item Retem ICMS na Fonte o campo ICMS Estadual Subst Trib nao deve ser informado com zero, impossivel Criar o Pedido: " + STRING(ITEM.it-codigo)
                     tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                     tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                     tt-log-ped-venda.importou    = FALSE
                     tt-log-ped-venda.data-import = TODAY
                     tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").
   
              RUN pi-grava-log-pedvenda.
   
              RETURN "NOK".
          END.
   
          /* --- 3a validacao --- */
          IF DEC(tt-arq_ite.c_arq_ite_per_icm_estad_st) <> ? AND
             DEC(tt-arq_ite.c_arq_ite_per_st_icm)        = ? THEN
          DO:
              l-erros-item-ped = YES.
              CREATE tt-log-ped-venda.
              ASSIGN tt-log-ped-venda.mensagem    = SUBSTITUTE("Quando informado o campo &1, tambem devera ser informado o campo &2, impossivel Criar o Pedido: &3", "ICMS Estadual Subst Trib", "% Subst Trib ICMS", ITEM.it-codigo) 
                     tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                     tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                     tt-log-ped-venda.importou    = FALSE
                     tt-log-ped-venda.data-import = TODAY
                     tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").
   
              RUN pi-grava-log-pedvenda.
   
              RETURN "NOK".
          END.

          /* --- 4a validacao --- */
          IF DEC(tt-arq_ite.c_arq_ite_per_icm_estad_st)  = ? AND
             DEC(tt-arq_ite.c_arq_ite_per_st_icm)       <> ? THEN
          DO:
              l-erros-item-ped = YES.
              CREATE tt-log-ped-venda.
              ASSIGN tt-log-ped-venda.mensagem    = SUBSTITUTE("Quando informado o campo &1, tambem devera ser informado o campo &2, impossivel Criar o Pedido: &3", "% Subst Trib ICMS", "ICMS Estadual Subst Trib", ITEM.it-codigo)
                     tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                     tt-log-ped-venda.nr-pedcli   = tt-arq_ped.c_arq_ped_nr-pedcli
                     tt-log-ped-venda.importou    = FALSE
                     tt-log-ped-venda.data-import = TODAY
                     tt-log-ped-venda.hora-import = STRING(TIME,"HH:MM:SS").
   
              RUN pi-grava-log-pedvenda.
   
              RETURN "NOK".
          END.
       END.
       */
    END.    
    IF l-erros-item-ped = YES THEN RETURN "NOK".

    /*****************************************************************************************/
    /*** Fim das Validacoes do Pedido de Venda ***********************************************/
    /*****************************************************************************************/
    RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/*********************************/
PROCEDURE pi-processa-dados :
/*********************************/

    DEF INPUT  PARAMETER hView AS HANDLE     NO-UNDO.
    DEF output PARAMeter l-retorna-dados as log NO-UNDO.
    DEF VAR i       AS INT    NO-UNDO.
    DEF VAR hValue  AS HANDLE     NO-UNDO.

    CREATE X-NODEREF hValue.
    
    IF hView:NAME = "cod_estabel" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-estabel = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "cod_emitente" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-emitente = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "nr_pedcli" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nr-pedcli = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "emergencial" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_emergencial = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "dt_entrega" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_dt-entrega = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "cod_cond_pag" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-cond-pag = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "nr_tabpre" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nr-tabpre = hValue:NODE-VALUE NO-ERROR.
        IF tt-arq_ped.c_arq_ped_nr-tabpre = ? THEN
            ASSIGN tt-arq_ped.c_arq_ped_nr-tabpre = "".
    END.

    IF hView:NAME = "cod_priori" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-priori = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "observacoes" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_observacoes = hValue:NODE-VALUE NO-ERROR.
        IF tt-arq_ped.c_arq_ped_observacoes = ? THEN
            ASSIGN tt-arq_ped.c_arq_ped_observacoes = "".
    END.

    IF hView:NAME = "nome_transp" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nome-transp = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "tipo_preco" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_tb-preco = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "mo_codigo" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_mo-codigo = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "no_ab_reppri" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_no-ab-reppri = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "vl_desconto" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_vl-desconto = STRING(DEC(REPLACE(hValue:NODE-VALUE  ,".",","))) NO-ERROR.
    END.

    IF hView:NAME = "nome_ab_rep" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nome-ab-rep = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "cod_entrega" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-entrega = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "tipo_fatur" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_tipo-fatur = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "nat_operacao" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nat-operacao = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "dt_min_fat" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_dt-min-fat = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "transp_redesp" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_transp-redesp = hValue:NODE-VALUE NO-ERROR.
        IF tt-arq_ped.c_arq_ped_transp-redesp = ?  THEN
            ASSIGN tt-arq_ped.c_arq_ped_transp-redesp = "".
    END.

    IF hView:NAME = "nr_pedrep" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nr-pedrep = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "cidade_cif" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cidade-cif = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "cod_canal_venda" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-canal-venda = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "e_mail" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_e-mail = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "nome-abrev-tri" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_nome-abrev-tri = hValue:NODE-VALUE NO-ERROR.
        IF tt-arq_ped.c_arq_ped_nome-abrev-tri  = ? THEN
            ASSIGN tt-arq_ped.c_arq_ped_nome-abrev-tri  = "".
    END.

    IF hView:NAME = "cod-entrega-tri" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod-entrega-tri = hValue:NODE-VALUE NO-ERROR.
        IF tt-arq_ped.c_arq_ped_cod-entrega-tri = ? THEN
            tt-arq_ped.c_arq_ped_cod-entrega-tri = "".
    END.

    IF hView:NAME = "BU" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_cod_unid_negocio = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "finalidade" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_dest_merc = hValue:NODE-VALUE NO-ERROR.
        
        ASSIGN l-retorna-dados = YES.
    END.
    
    IF hView:NAME = "prioridade_logistica" THEN DO:
        hView:GET-CHILD(hValue,1) NO-ERROR.
        tt-arq_ped.c_arq_ped_cod_priori_log = hValue:NODE-VALUE NO-ERROR.
    END.

    IF hView:NAME = "tipo_doc_fiscal" THEN DO:
       hView:GET-CHILD(hValue,1) NO-ERROR.
       tt-arq_ped.c_arq_ped_modal_doc_fiscal = hValue:NODE-VALUE NO-ERROR.
    END.


END PROCEDURE.

PROCEDURE pi-processa-itens :

   DEF INPUT  PARAMETER hXNODE AS HANDLE        NO-UNDO.
   DEF output PARAMeter l-retorna-itens  as log NO-UNDO.

   DEF VAR i                AS INT     NO-UNDO.
   DEF VAR j                AS INT     NO-UNDO.
   DEF VAR k                AS INT     NO-UNDO.
   DEF VAR m                 AS INT     NO-UNDO.
   DEF VAR hView            AS HANDLE      NO-UNDO.
   DEF VAR hValue           AS HANDLE      NO-UNDO.
   DEF VAR hItem            AS HANDLE      NO-UNDO.
   DEF VAR hItens           AS HANDLE      NO-UNDO.
   DEF VAR hLotes           AS HANDLE      NO-UNDO.
   DEF VAR hViewlote        AS HANDLE      NO-UNDO.
   DEF VAR hloteInterno     AS HANDLE      NO-UNDO.
   DEF VAR hValuelote       AS HANDLE      NO-UNDO.

   CREATE X-NODEREF hView.
   CREATE X-NODEREF hItens.
   CREATE X-NODEREF hItem.
   CREATE X-NODEREF hValue.
   CREATE X-NODEREF hViewlote.
   CREATE X-NODEREF hLotes.
   CREATE X-NODEREF hloteInterno.
   CREATE X-NODEREF hValuelote.

   IF hXNODE:NAME = "Itens" THEN DO:

       DO j = 1 TO hXNODE:NUM-CHILDREN:
           hXNODE:GET-CHILD(hItens,j).

           IF hItens:NAME = "Item" THEN DO:

               CREATE tt-arq_ite.
               ASSIGN tt-arq_ite.c_arq_ped_cod-emitente     = tt-arq_ped.c_arq_ped_cod-emitente
                      tt-arq_ite.c_arq_ped_nr-pedcli        = tt-arq_ped.c_arq_ped_nr-pedcli.
           END.
           DO i = 1 TO hItens:NUM-CHILDREN:
               hItens:GET-CHILD(hView,i).

               IF hView:NAME = "sequencia" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_nr-sequencia = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "it_codigo" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_it-codigo  = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "qt_pedida" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_qt-pedida = STRING(DEC(REPLACE(hValue:NODE-VALUE  ,".",","))) NO-ERROR.
               END.

               IF hView:NAME = "vl_preori" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_vl-preori = STRING(DEC(REPLACE(hValue:NODE-VALUE  ,".",","))) NO-ERROR.
               END.

               IF hView:NAME = "nr_tabpre" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_nr-tabpre = hValue:NODE-VALUE NO-ERROR.
                   IF tt-arq_ite.c_arq_ite_nr-tabpre = ? THEN
                       ASSIGN tt-arq_ite.c_arq_ite_nr-tabpre =  "".
               END.

               IF hView:NAME = "tp_preco" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_tb-preco = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "vl_total" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_vl-total = STRING(DEC(REPLACE(hValue:NODE-VALUE  ,".",","))) NO-ERROR.
               END.

               IF hView:NAME = "perc_icms" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_perc-icms = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "complemento_descricao" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_obs-item-ped = hValue:NODE-VALUE NO-ERROR.
                   IF tt-arq_ite.c_arq_ite_obs-item-ped = ? THEN
                       ASSIGN tt-arq_ite.c_arq_ite_obs-item-ped = "".
               END.

               IF hView:NAME = "nat_operacao" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_nat-operacao  = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "cod_estabel" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_cod-estabel = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "dt_entrega" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_dt-entrega  = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "ind-icm-ret" THEN DO:
                   hView:GET-CHILD(hValue,1) NO-ERROR.
                   ASSIGN tt-arq_ite.c_arq_ite_ind-icm-ret = hValue:NODE-VALUE NO-ERROR.
               END.

               IF hView:NAME = "minimo" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_arq_ite_lanc_minimo  = STRING(DEC(REPLACE(hValue:NODE-VALUE  ,".",","))) NO-ERROR.
                END.

                IF hView:NAME = "obrigatorio" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_arq_ite_lanc_obrigato = STRING(DEC(REPLACE(hValue:NODE-VALUE  ,".",","))) NO-ERROR.
                END.

                IF hView:NAME = "xml_xPed" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_arq_ite_ped_compr = hValue:NODE-VALUE NO-ERROR.
                    IF tt-arq_ite.c_arq_ite_ped_compr = ? THEN
                        ASSIGN tt-arq_ite.c_arq_ite_ped_compr = "".
                END.

                IF hView:NAME = "xml_nItemPed" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_arq_ite_seq_ped_compr = hValue:NODE-VALUE NO-ERROR.
                END.

                IF hView:NAME = "BU" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_cod_unid_negocio = hValue:NODE-VALUE NO-ERROR.
                END.

                IF hView:NAME = "iva" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_arq_ite_per_st_icm = hValue:NODE-VALUE NO-ERROR.
                END.

                IF hView:NAME = "icms_estadual_st" THEN DO:
                    hView:GET-CHILD(hValue,1) NO-ERROR.
                    ASSIGN tt-arq_ite.c_arq_ite_per_icm_estad_st = hValue:NODE-VALUE NO-ERROR.
                    
                    l-retorna-itens = YES.
                END.

                /***************************************Lotes**************************************/
               IF hView:NAME = "lotes" THEN DO:

                    DO k = 1 TO (hView:NUM-CHILDREN) :
                        hView:GET-CHILD(hLotes,k).
                    
                        IF hLotes:NAME = "lote" THEN DO:
    
                            FIND FIRST emitente WHERE emitente.cod-emitente = INT(tt-arq_ite.c_arq_ped_cod-emitente) NO-LOCK NO-ERROR.
                            IF NOT AVAIL emitente THEN DO:
                                 RETURN "NOK".
                            END.

                            ASSIGN cLoteAux       = ""
                                   cDepAux        = ""
                                   cLocalAux      = ""
                                   deQtdeAux      = 0
                                   dtValidLoteAux = ?.

                            DO m = 1 TO (hLotes:NUM-CHILDREN) :

                                hLotes:GET-CHILD(hViewlote,m).
    
                                IF hViewlote:NAME = "codigo" THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) NO-ERROR.
                                    ASSIGN cLoteAux = hValuelote:NODE-VALUE NO-ERROR.
                                END.
                                
                                IF hViewlote:NAME = "deposito" THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) NO-ERROR.
                                    ASSIGN cDepAux = REPLACE(hValuelote:NODE-VALUE  ,".",",") NO-ERROR.
                                END.

                                IF hViewlote:NAME = "localizacao" THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) NO-ERROR.
                                    ASSIGN cLocalAux = REPLACE(hValuelote:NODE-VALUE  ,".",",") NO-ERROR.
                                    IF cLocalAux = ? THEN
                                       ASSIGN cLocalAux = "".
                                END.
    
                                IF hViewlote:NAME = "quantidade" THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) NO-ERROR.
                                    ASSIGN deQtdeAux = DEC(REPLACE(hValuelote:NODE-VALUE  ,".",",")) NO-ERROR.
                                END.
    
                                IF hViewlote:NAME = "validade" THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) NO-ERROR.
                                    ASSIGN dtValidLoteAux = date(hValuelote:NODE-VALUE) NO-ERROR.
                                END.

                                IF m = hLotes:NUM-CHILDREN THEN
                                DO:
                                    FIND FIRST tt-arq_lote WHERE tt-arq_lote.nome-abrev    = emitente.nome-abrev
                                                             AND tt-arq_lote.nr-pedcli     = tt-arq_ped.c_arq_ped_nr-pedcli
                                                             AND tt-arq_lote.nr-sequencia  = INT(tt-arq_ite.c_arq_ite_nr-sequencia)
                                                             AND tt-arq_lote.it-codigo     = tt-arq_ite.c_arq_ite_it-codigo
                                                             AND tt-arq_lote.cod-refer     = ""
                                                             AND tt-arq_lote.codigo        = cLoteAux
                                                             AND tt-arq_lote.cod-localiz   = cLocalAux
                                    EXCLUSIVE-LOCK NO-ERROR.

                                    IF NOT AVAIL tt-arq_lote THEN 
                                    DO:
                                        CREATE tt-arq_lote.
                                        ASSIGN tt-arq_lote.nome-abrev    = emitente.nome-abrev
                                               tt-arq_lote.nr-pedcli     = tt-arq_ped.c_arq_ped_nr-pedcli
                                               tt-arq_lote.nr-sequencia  = INT(tt-arq_ite.c_arq_ite_nr-sequencia)
                                               tt-arq_lote.it-codigo     = tt-arq_ite.c_arq_ite_it-codigo
                                               tt-arq_lote.cod-refer     = ""
                                               tt-arq_lote.codigo        = cLoteAux 
                                               tt-arq_lote.cod-localiz   = cLocalAux
                                               tt-arq_lote.cod-depos     = cDepAux
                                               tt-arq_lote.quantidade    = deQtdeAux
                                               tt-arq_lote.dt-validade   = dtValidLoteAux NO-ERROR.
                                    END.

                                END.
    
                            END.
    
                        END.
    
                    END.
                END.
                /**********************************************************************************/

            END.
        END.
    END.
    /*************************limpa lote sem dados**********/
    FOR EACH tt-arq_lote:
        IF tt-arq_lote.codigo = ? AND tt-arq_lote.quantidade = ? THEN
        DELETE tt-arq_lote.
    END.
    /****************************************************/

END PROCEDURE.

/**************************/
PROCEDURE pi-grava-subt:
/**************************/

DEF INPUT PARAM pi-cod-estabel      LIKE tt-arq_ped.c_arq_ped_cod-estabel NO-UNDO.              
DEF INPUT PARAM pi-it-codigo        LIKE tt-arq_ite.c_arq_ite_it-codigo   NO-UNDO.                         
DEF INPUT PARAM pi-per_st_icm       AS DEC                                NO-UNDO.
DEF INPUT PARAM pi-per_icm_estad_st AS DEC                                NO-UNDO.
DEF INPUT PARAM pi-cod-emitente     LIKE ped-venda.cod-emitente           NO-UNDO.

if pi-per_st_icm <> ? and pi-per_icm_estad_st <> ? AND
   pi-per_st_icm <> 0 and pi-per_icm_estad_st <> 0 THEN
DO:
        assign c-estado-origem  = ""
               c-estado-destino = "".
                       
        find first estabelec where estabelec.cod-estabel = pi-cod-estabel no-lock no-error.

        if avail estabelec then
           assign c-estado-origem = estabelec.estado.
        
        find emitente where emitente.cod-emitente = pi-cod-emitente no-lock no-error.
        
        if avail emitente then
           assign c-estado-destino = emitente.estado.       
        

        /* Validacao incluida para evitar erro gerado no caso de inclus∆o de casas decimais no XML que causou erro alterado aliquotas de ICMS para 120%, 170% e 180% */
        if pi-per_icm_estad_st < 100 and pi-per_st_icm < 100 then
        do:
            find first item-uf 
                 where item-uf.it-codigo       = pi-it-codigo
                 and   item-uf.cod-estado-orig = c-estado-origem
                 and   item-uf.estado          = c-estado-destino no-error.
            
            if avail item-uf then
               assign item-uf.per-sub-tri = pi-per_st_icm
                      item-uf.dec-1       = pi-per_icm_estad_st.
            else
            do:
               create item-uf.
               assign item-uf.it-codigo       = pi-it-codigo    
                      item-uf.cod-estado-orig = c-estado-origem 
                      item-uf.estado          = c-estado-destino
                      item-uf.per-sub-tri     = pi-per_st_icm       
                      item-uf.dec-1           = pi-per_icm_estad_st.
            end.        
        end.

     
/*         FIND diferim-parcial-icms WHERE diferim-parcial-icms.cod-item   = pi-it-codigo               */
/*                                     and diferim-parcial-icms.cod-estado = c-estado-destino NO-ERROR. */
/*         if AVAIL diferim-parcial-icms THEN                                                           */
/*            ASSIGN diferim-parcial-icms.val-perc-icms-diferim = pi-per_icm_estad_st.                  */
/*         ELSE                                                                                         */
/*            DO:                                                                                       */
/*               CREATE diferim-parcial-icms.                                                           */
/*               ASSIGN diferim-parcial-icms.cod-item              = pi-it-codigo                       */
/*                      diferim-parcial-icms.cod-estado            = c-estado-destino                   */
/*                      diferim-parcial-icms.val-perc-icms-diferim = pi-per_icm_estad_st.               */
/*            END.                                                                                      */
   END.
           
END PROCEDURE.

/*****************************/
PROCEDURE pi-copia-Pedido:
/*****************************/

DEF INPUT PARAMeter p-rw-ped-venda as ROWID NO-UNDO.

IF NOT VALID-HANDLE(bo-ped-venda-sdf)                      OR 
                    bo-ped-venda-sdf:TYPE <> "PROCEDURE":U OR 
                    bo-ped-venda-sdf:FILE-NAME <> "dibo/bodi159sdf.p" THEN
   RUN dibo/bodi159sdf.p PERSISTENT SET bo-ped-venda-sdf.

   RUN SETDefaultOrderNumber in bo-ped-venda-sdf ( OUTPUT i-sequencia). 

IF VALID-HANDLE(bo-ped-venda-sdf) THEN
   DELETE PROCEDURE bo-ped-venda-sdf.

IF NOT VALID-HANDLE(bo-ped-venda-cpy)                      or
                    bo-ped-venda-cpy:TYPE <> "PROCEDURE":U or
                    bo-ped-venda-cpy:FILE-NAME <> "dibo/bodi159cpy.p" THEN
   RUN dibo/bodi159cpy.p PERSISTENT SET bo-ped-venda-cpy.

FOR EACH RowErrors:
    DELETE RowErrors.
END.

EMPTY TEMP-TABLE tt-ped-copia      NO-ERROR.
EMPTY TEMP-TABLE tt-item-copia     NO-ERROR.
EMPTY TEMP-TABLE tt-arq_lote_copia NO-ERROR.

FIND ped-venda WHERE ROWID(ped-venda) = p-rw-ped-venda NO-LOCK NO-ERROR.
IF AVAIL ped-venda THEN
DO:
      ASSIGN i-nr-Pedido-orig    = ped-venda.nr-Pedido
             c-nat-operacao-orig = ped-venda.nat-operacao
             c-cod-estabel-orig  = ped-venda.cod-estabel
             i-cod-emitente      = ped-venda.cod-emitente.

      ASSIGN c-nr-pedcli-original  = ped-venda.nr-pedcli
             c-nome-abrev-original = ped-venda.nome-abrev.
             
      FIND dw-ped-venda WHERE dw-ped-venda.nr-Pedido = ped-venda.nr-Pedido NO-LOCK NO-ERROR.
      if AVAIL dw-ped-venda THEN
         ASSIGN i-cod-priori-orig  = dw-ped-venda.cod-priori
                c-modal-doc-fiscal = dw-ped-venda.modal-doc-fiscal.
     
      FIND FIRST dw-PARAM-transf-oper WHERE dw-PARAM-transf-oper.cod-estabel-ori  = c-cod-estabel-orig
                                        and dw-PARAM-transf-oper.cod-estabel-dest = "701"
                                        and dw-PARAM-transf-oper.nat-operacao     = c-nat-operacao-orig NO-LOCK NO-ERROR.
      if AVAIL dw-PARAM-transf-oper THEN
         DO:
         
            FIND estabelec WHERE estabelec.cod-estabel = dw-PARAM-transf-oper.cod-estabel-ori NO-LOCK NO-ERROR.
            IF AVAIL estabelec THEN
               DO:
                  FIND emitente WHERE emitente.cod-emitente = estabelec.cod-emitente NO-LOCK NO-ERROR.
                  IF AVAIL emitente THEN
                     ASSIGN i-cod-emitente = emitente.cod-emitente.
               END.
         END.
      ELSE
         RETURN "NOK".

      FIND emitente WHERE emitente.cod-emitente = i-cod-emitente NO-LOCK NO-ERROR.

      CREATE tt-ped-copia.
      ASSIGN tt-ped-copia.nome-abrev   = emitente.nome-abrev
             tt-ped-copia.nr-Pedido    = i-sequencia
             tt-ped-copia.nr-pedcli    = "PD" + TRIM(ped-venda.nr-pedcli)
             tt-ped-copia.dt-emissao   = ped-venda.dt-emissao
             tt-ped-copia.dt-entrega   = ped-venda.dt-entrega
             tt-ped-copia.nr-tab-finan = ped-venda.nr-tab-finan
             tt-ped-copia.nr-ind-finan = ped-venda.nr-ind-finan
             tt-ped-copia.cod-cond-pag = 0 /*ped-venda.cod-cond-pag*/
             tt-ped-copia.cod-entrega  = "Padr∆o"
             tt-ped-copia.nome-transp  = ped-venda.nome-transp
             tt-ped-copia.perc-desco1  = ped-venda.perc-desco1
             tt-ped-copia.esp-ped      = ped-venda.esp-ped
             tt-ped-copia.tp-preco     = ped-venda.tp-preco
             tt-ped-copia.vl-desconto  = ped-venda.vl-desconto
             tt-ped-copia.e-mail       = emitente.e-mail
             tt-ped-copia.ind-apr-cred = emitente.ind-apr-cred
             tt-ped-copia.ind-cre-cli  = emitente.ind-cre-cli.

      FOR EACH ped-item of ped-venda NO-LOCK:
          CREATE tt-item-copia.
          ASSIGN tt-item-copia.nr-sequencia = ped-item.nr-sequencia
                 tt-item-copia.it-codigo    = ped-item.it-codigo
                 tt-item-copia.qt-pedida    = ped-item.qt-pedida
                 tt-item-copia.dt-entrega   = ped-item.dt-entrega
                 tt-item-copia.selecionado  = yes.

          FIND FIRST item WHERE item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
          IF  AVAIL item THEN
              ASSIGN tt-item-copia.desc-item = item.desc-item.
      END.
   END.   


RUN copyOrder in bo-ped-venda-cpy(INPUT  p-rw-ped-venda,
                              INPUT  TABLE tt-ped-copia,
                              INPUT  TABLE tt-item-copia,
                              INPUT  i-situacao,
                              INPUT  i-natur-oper,
                              INPUT  l-exp-dt-entrega,
                              OUTPUT gr-ped-venda,
                              OUTPUT TABLE RowErrors).

DELETE PROCEDURE bo-ped-venda-cpy.

IF  NOT CAN-FIND(FIRST RowErrors
                 WHERE RowErrors.ErrorTYPE <> "INTERNAL":U
                   AND RowErrors.ErrorSubTYPE = "Error":U) THEN DO:

    for each esp-ped-item of ped-venda no-lock:
        create bf-esp-ped-item.
        buffer-copy esp-ped-item except esp-ped-item.nr-pedcli esp-ped-item.nome-abrev to bf-esp-ped-item.
        assign bf-esp-ped-item.nome-abrev        = tt-ped-copia.nome-abrev
               bf-esp-ped-item.nr-pedcli         = tt-ped-copia.nr-pedcli.
    end.

    FOR EACH tt-ped-copia:
        DELETE tt-ped-copia.
    END.
END.

FIND FIRST ped-venda WHERE ROWID(ped-venda) = gr-ped-venda EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL ped-venda THEN
   DO:
      ASSIGN i-nr-Pedido-novo = ped-venda.nr-Pedido.
      
      FIND FIRST dw-PARAM-transf-oper WHERE dw-PARAM-transf-oper.cod-estabel-ori  = c-cod-estabel-orig
                                        and dw-PARAM-transf-oper.cod-estabel-dest = "701"
                                        and dw-PARAM-transf-oper.nat-operacao     = c-nat-operacao-orig NO-LOCK NO-ERROR.

      if AVAIL dw-PARAM-transf-oper THEN   
         ASSIGN ped-venda.cod-estabel  = dw-PARAM-transf-oper.cod-estabel-dest
                ped-venda.nat-operacao = dw-PARAM-transf-oper.nat-operacao-dest.
                
      ASSIGN ped-venda.observacoes = "Pedido Origem: " + STRING(i-nr-Pedido-orig).
      
      FIND first emitente no-lock
           where emitente.nome-abrev = ped-venda.nome-abrev NO-ERROR.

      FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
          FIND FIRST dw-PARAM-transf-oper WHERE dw-PARAM-transf-oper.cod-estabel-ori  = c-cod-estabel-orig
                                            and dw-PARAM-transf-oper.cod-estabel-dest = "701"
                                            and dw-PARAM-transf-oper.nat-operacao     = ped-item.nat-operacao NO-LOCK NO-ERROR.
          
          IF AVAIL dw-PARAM-transf-oper THEN
             ASSIGN ped-item.nat-operacao = dw-PARAM-transf-oper.nat-operacao-dest.

          ASSIGN ped-item.qt-un-fat    = ped-item.qt-pedida.

          if ped-venda.cod-estabel = "701" and emitente.estado = "PR" and ped-item.nat-operacao begins("54") then
             assign ped-item.ind-icm-ret = yes.
          ELSE
             assign ped-item.ind-icm-ret = NO.
      END.
      
      FIND dw-ped-venda WHERE dw-ped-venda.nr-Pedido = ped-venda.nr-Pedido EXCLUSIVE-LOCK NO-ERROR.
  
      IF not AVAIL dw-ped-venda THEN
         DO:
            CREATE dw-ped-venda.
            ASSIGN dw-ped-venda.nr-Pedido        = ped-venda.nr-Pedido
                   dw-ped-venda.cod-priori       = i-cod-priori-orig
                   dw-ped-venda.modal-doc-fiscal = c-modal-doc-fiscal.
         END.
      
      FIND dw-ped-venda WHERE dw-ped-venda.nr-Pedido = i-nr-Pedido-orig EXCLUSIVE-LOCK NO-ERROR.
  
      IF AVAIL dw-ped-venda THEN
         ASSIGN dw-ped-venda.nr-Pedido-transf = i-nr-Pedido-novo. 
      ELSE
         DO:
            CREATE dw-ped-venda.
            ASSIGN dw-ped-venda.nr-Pedido        = i-nr-Pedido-orig
                   dw-ped-venda.cod-priori       = i-cod-priori-orig
                   dw-ped-venda.modal-doc-fiscal = c-modal-doc-fiscal
                   dw-ped-venda.nr-Pedido-transf = i-nr-Pedido-novo.
         END.

        find first dw-follow-up-ped no-lock
             where dw-follow-up-ped.nome-abrev = ped-venda.nome-abrev
               and dw-follow-up-ped.nr-pedcli  = ped-venda.nr-pedcli no-error.
        if not avail dw-follow-up-ped then do:
            create dw-follow-up-ped.
            assign dw-follow-up-ped.nome-abrev   = ped-venda.nome-abrev 
                   dw-follow-up-ped.nr-pedcli    = ped-venda.nr-pedcli
                   dw-follow-up-ped.user-criacao = ped-venda.user-impl
                   dw-follow-up-ped.dt-criacao   = today
                   dw-follow-up-ped.hr-criacao   = STRING(TIME,'hh:mm:ss').
        end.

      /*** Alocaá∆o de Estoque - 4make - Wanderley - 17/12/2015 ***/
      
      FOR EACH tt-arq_lote WHERE tt-arq_lote.nr-pedcli  = c-nr-pedcli-original  
                             AND tt-arq_lote.nome-abrev = c-nome-abrev-original NO-LOCK:
          CREATE tt-arq_lote_copia.
          ASSIGN tt-arq_lote_copia.nome-abrev    = ped-venda.nome-abrev 
                 tt-arq_lote_copia.nr-pedcli     = ped-venda.nr-pedcli 
                 tt-arq_lote_copia.nr-sequencia  = tt-arq_lote.nr-sequencia
                 tt-arq_lote_copia.it-codigo     = tt-arq_lote.it-codigo   
                 tt-arq_lote_copia.cod-refer     = tt-arq_lote.cod-refer   
                 tt-arq_lote_copia.codigo        = tt-arq_lote.codigo      
                 tt-arq_lote_copia.cod-depos     = tt-arq_lote.cod-depos   
                 tt-arq_lote_copia.cod-localiz   = tt-arq_lote.cod-localiz 
                 tt-arq_lote_copia.quantidade    = tt-arq_lote.quantidade  
                 tt-arq_lote_copia.dt-validade   = tt-arq_lote.dt-validade.
      END.

      IF CAN-FIND(FIRST ped-ent OF ped-venda) AND CAN-FIND (FIRST tt-arq_lote_copia) THEN DO:

         IF NOT VALID-HANDLE(bo-ped-venda) THEN
             RUN dibo/bodi159.p    PERSISTENT SET bo-ped-venda.
         
         run emptyRowErrors     in bo-ped-venda.
         run validateAllocation in bo-ped-venda(INPUT ROWID(ped-venda)).
         run getRowErrors       in bo-ped-venda(output table RowErrors).
         
         IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorType <> "INTERNAL":U) THEN DO:
              FOR EACH RowErrors NO-LOCK:
                  ASSIGN tt-log-ped-venda.cd-erro    = RowErrors.errornumber                        
                         tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + RowErrors.errordescription .
              END.
         END.
         ELSE DO:
             FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
                 FIND FIRST ped-ent WHERE ped-ent.nome-abrev   = ped-item.nome-abrev  
                                      and ped-ent.nr-pedcli    = ped-item.nr-pedcli   
                                      and ped-ent.it-codigo    = ped-item.it-codigo   
                                      and ped-ent.cod-refer    = ped-item.cod-refer   
                                      and ped-ent.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
                 IF AVAIL ped-ent THEN DO:
                     FIND FIRST ITEM WHERE ITEM.it-codigo = ped-ent.it-codigo NO-LOCK NO-ERROR.
         
                     FOR EACH tt-arq_lote_copia WHERE tt-arq_lote_copia.nome-abrev   = ped-venda.nome-abrev
                                                  and tt-arq_lote_copia.nr-pedcli    = ped-venda.nr-pedcli 
                                                  and tt-arq_lote_copia.nr-sequencia = ped-item.nr-sequencia 
                                                  and tt-arq_lote_copia.it-codigo    = ped-item.it-codigo
                                                  and tt-arq_lote_copia.cod-refer    = ped-item.cod-refer NO-LOCK:
                         
                         FOR EACH  saldo-estoq WHERE saldo-estoq.cod-depos   = tt-arq_lote_copia.cod-depos                         
                                                 AND saldo-estoq.cod-estabel = ped-venda.cod-estabel 
                                                 AND saldo-estoq.cod-localiz = tt-arq_lote_copia.cod-localiz
                                                 AND saldo-estoq.lote        = tt-arq_lote_copia.codigo
                                                 and saldo-estoq.it-codigo   = ped-ent.it-codigo                                
                                                 and saldo-estoq.cod-refer   = ped-ent.cod-refer                                
                                                 and (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                                 saldo-estoq.qt-aloc-ped +                      
                                                                                 saldo-estoq.qt-aloc-prod)) > 0
                              AND (ITEM.tipo-con-est < 3  OR saldo-estoq.dt-vali-lote >= ped-ent.dt-entrega) NO-LOCK,
                            FIRST deposito WHERE deposito.cod-depos = saldo-estoq.cod-depos                                 
                                             and deposito.ind-acabado NO-LOCK:   
                         
                             IF (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                            saldo-estoq.qt-aloc-ped +                      
                                                            saldo-estoq.qt-aloc-prod)) >= tt-arq_lote_copia.quantidade  THEN DO:
                         
                                 run pi-aloca-fisica-man (INPUT ROWID(ped-ent),
                                                          INPUT-OUTPUT tt-arq_lote_copia.quantidade, 
                                                          INPUT ROWID(saldo-estoq)).
                             END.
                             ELSE
                                 ASSIGN tt-log-ped-venda.cd-erro    = 99999                        
                                        tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + "Quantidade informada do lote maior que quantiade disponivel." .
                         END.                                                                                
                     END.
                 END.
             END.
         END.

         IF VALID-HANDLE (h-alocacao) THEN DO:
            DELETE PROCEDURE h-alocacao.
            h-alocacao = ?.
         END.
            
         IF VALID-HANDLE(bo-ped-venda) THEN DO:
             RUN destroyBo IN bo-ped-venda.
                   
             IF VALID-HANDLE(bo-ped-venda) THEN DO:
                 DELETE PROCEDURE bo-ped-venda.
                 bo-ped-venda = ?.
             END.
         END.
      END.

      RUN pi-acompanhamento("Pedido - Efetivando Pedido Novo").  
      
      RUN pi-efetiva-Pedido-novo.
        
      /******/
   END.
   release ped-venda.
   release ped-item.
   release ped-ent.
   release dw-ped-venda.

END PROCEDURE.

/******************************/
PROCEDURE pi-aloca-fisica-man:
/******************************/

    DEF INPUT        PARAM r-ped-ent      as ROWID NO-UNDO.
    DEF INPUT-OUTPUT PARAM de-qt-a-alocar AS DEC  NO-UNDO.
    DEF INPUT        PARAM r-saldo-estoq  as ROWID NO-UNDO.
    
    DEF VAR de-saldo         AS DEC NO-UNDO.
    DEF VAR de-valor-a-maior AS DEC NO-UNDO.
    DEF VAR de-quant-aux     AS DEC NO-UNDO.

    FIND FIRST para-ped NO-LOCK.

    if  para-ped.tp-aloca-ped <> 3 THEN
        RETURN "NOK".
   
    FIND ped-ent WHERE ROWID(ped-ent) = r-ped-ent EXCLUSIVE-LOCK NO-ERROR.
    
    FIND saldo-estoq WHERE ROWID(saldo-estoq) = r-saldo-estoq EXCLUSIVE-LOCK NO-ERROR.

    if de-qt-a-alocar > ped-ent.qt-pedida - ped-ent.qt-alocada - ped-ent.qt-log-aloca THEN
       ASSIGN de-quant-aux   = de-qt-a-alocar
              de-qt-a-alocar = ped-ent.qt-pedida - ped-ent.qt-alocada - ped-ent.qt-log-aloca.
    
    ASSIGN de-saldo = (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada + saldo-estoq.qt-aloc-ped + saldo-estoq.qt-aloc-prod)).

    FIND ped-saldo WHERE
         ped-saldo.cod-depos   = saldo-estoq.cod-depos   and
         ped-saldo.cod-estabel = saldo-estoq.cod-estabel and
         ped-saldo.cod-localiz = saldo-estoq.cod-localiz and
         ped-saldo.lote        = saldo-estoq.lote        and
         ped-saldo.nome-abrev  = ped-ent.nome-abrev      and
         ped-saldo.nr-pedcli   = ped-ent.nr-pedcli       and
         ped-saldo.nr-seq-item = ped-ent.nr-sequencia    and
         ped-saldo.it-codigo   = ped-ent.it-codigo       and
         ped-saldo.cod-refer   = ped-ent.cod-refer       and
         ped-saldo.nr-entrega  = ped-ent.nr-entrega EXCLUSIVE-LOCK NO-ERROR.
    
    if  not AVAIL(ped-saldo) THEN DO:
        CREATE ped-saldo.
        ASSIGN ped-saldo.cod-depos   = saldo-estoq.cod-depo
               ped-saldo.cod-estabel = saldo-estoq.cod-estabel
               ped-saldo.cod-localiz = saldo-estoq.cod-localiz
               ped-saldo.lote        = saldo-estoq.lote
               ped-saldo.nome-abrev  = ped-ent.nome-abrev
               ped-saldo.nr-pedcli   = ped-ent.nr-pedcli
               ped-saldo.nr-seq-item = ped-ent.nr-sequencia
               ped-saldo.it-codigo   = ped-ent.it-codigo
               ped-saldo.cod-refer   = ped-ent.cod-refer
               ped-saldo.nr-entrega  = ped-ent.nr-entrega.
    END.

    if  de-saldo >= de-qt-a-alocar THEN
        ASSIGN saldo-estoq.qt-aloc-ped = saldo-estoq.qt-aloc-ped + de-qt-a-alocar
               ped-item.qt-log-aloca   = ped-item.qt-log-aloca   + de-qt-a-alocar
               ped-ent.qt-log-aloca    = ped-ent.qt-log-aloca    + de-qt-a-alocar
               ped-saldo.qt-aloc-ped   = ped-saldo.qt-aloc-ped   + de-qt-a-alocar
               de-qt-a-alocar          = 0.
    ELSE        
        ASSIGN saldo-estoq.qt-aloc-ped = saldo-estoq.qt-aloc-ped + de-saldo
               ped-item.qt-log-aloca   = ped-item.qt-log-aloca   + de-saldo
               ped-ent.qt-log-aloca    = ped-ent.qt-log-aloca    + de-saldo
               ped-saldo.qt-aloc-ped   = ped-saldo.qt-aloc-ped   + de-saldo
               de-qt-a-alocar          = de-qt-a-alocar          - de-saldo.
    
    RETURN "OK".
    
END.

/******************************/
PROCEDURE pi-verifica-saldo:
/******************************/

    DEF INPUT  PARAM c-it-codigo   AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-cod-refer   AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-cod-estabel AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-entrega    AS DATE NO-UNDO.
    def output PARAM de-saldo      AS DEC NO-UNDO.

    DEF VAR c-grupo-aloc AS CHAR NO-UNDO.
    
    def buffer b-estab    for estabelec.
    def buffer b-item     for item.
    def buffer b-saldo    for saldo-estoq.
    def buffer b-res-item for res-item.
    def buffer b-deposito for deposito.
    
    FIND b-estab WHERE 
         b-estab.cod-estabel = c-cod-estabel NO-LOCK NO-ERROR.
    FIND b-item  WHERE
         b-item.it-codigo = c-it-codigo NO-LOCK NO-ERROR.
    if  not AVAIL(b-estab)
    or  not AVAIL(b-item) THEN
        RETURN.

    FIND FIRST para-ped NO-LOCK.

    if  b-estab.grupo-aloca <> "" 
        &IF "{&bf_dis_versao_ems}" >= "2.04" &THEN
            AND {&TRUE-log-consid-grp-aloc}
        &ENDIF                         
        THEN DO:
        ASSIGN c-grupo-aloc = b-estab.grupo-aloc.
        FOR EACH b-estab FIELD(cod-estabel grupo-aloc) NO-LOCK
            WHERE b-estab.grupo-aloc = c-grupo-aloc:
            
            FOR EACH  b-saldo NO-LOCK
                WHERE b-saldo.cod-estabel = b-estab.cod-estabel
                and   b-saldo.it-codigo   = b-item.it-codigo
                and   b-saldo.cod-refer   = c-cod-refer,
                FIRST b-deposito FIELDS(cod-depos ind-acabado) NO-LOCK
                WHERE b-deposito.cod-depos = b-saldo.cod-depos
                and   b-deposito.ind-acabaDO:
    
   
                if  ((b-item.tipo-con-est = 3
                or  b-item.tipo-con-est = 4)
                and b-saldo.dt-vali-lote >= dt-entrega)
                or  b-item.tipo-con-est < 3 THEN 
                    ASSIGN de-saldo = de-saldo + 
                                      (b-saldo.qtidade-atu -
                                      (b-saldo.qt-alocada  +
                                      b-saldo.qt-aloc-ped  +
                                      b-saldo.qt-aloc-prod)).
    
            END.
            FIND b-res-item WHERE
                 b-res-item.cod-estabel = b-estab.cod-estabel and
                 b-res-item.it-codigo   = b-item.it-codigo    and
                 b-res-item.cod-refer   = c-cod-refer NO-LOCK NO-ERROR.

            if  AVAIL(b-res-item) THEN
                ASSIGN de-saldo = de-saldo - b-res-item.qt-alocada.
        END.
    END.
    ELSE DO:
        FOR EACH  b-saldo NO-LOCK
            WHERE b-saldo.cod-estabel = c-cod-estabel
            and   b-saldo.it-codigo   = b-item.it-codigo
            and   b-saldo.cod-refer   = c-cod-refer,
            FIRST b-deposito FIELDS(cod-depos ind-acabado) NO-LOCK
            WHERE b-deposito.cod-depos = b-saldo.cod-depos
            and   b-deposito.ind-acabaDO:

            if  ((b-item.tipo-con-est = 3
            or  b-item.tipo-con-est = 4)
            and b-saldo.dt-vali-lote >= dt-entrega)
            or  b-item.tipo-con-est < 3 THEN 
                ASSIGN de-saldo = de-saldo + 
                                  (b-saldo.qtidade-atu -
                                  (b-saldo.qt-alocada  +
                                  b-saldo.qt-aloc-ped  +
                                  b-saldo.qt-aloc-prod)).

        END.
        
        
        FIND b-res-item WHERE
             b-res-item.cod-estabel = c-cod-estabel    and
             b-res-item.it-codigo   = b-item.it-codigo and
             b-res-item.cod-refer   = c-cod-refer NO-LOCK NO-ERROR.
        if  AVAIL(b-res-item) THEN
            ASSIGN de-saldo = de-saldo - b-res-item.qt-alocada.
    END.
    
    if  de-saldo < 0 THEN
        ASSIGN de-saldo = 0.
                   
END.


/*********************************/
PROCEDURE pi-efetiva-Pedido-novo:
/*********************************/

IF NOT VALID-HANDLE(bo-ped-venda-com) THEN
    RUN dibo/bodi159com.p PERSISTENT SET bo-ped-venda-com.

RUN completeOrder IN bo-ped-venda-com(INPUT  rowid(ped-venda),
                                      OUTPUT TABLE RowErrors).

RUN destroyBO IN bo-ped-venda-com.
DELETE PROCEDURE bo-ped-venda-com.
bo-ped-venda-com = ?.    

IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U ) THEN 
DO:
    FOR EACH RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U:
        CREATE tt-erro-copia.
        ASSIGN tt-erro-copia.cod-emitente     = ped-venda.cod-emitente 
               tt-erro-copia.nr-pedcli        = ped-venda.nr-pedcli  
               tt-erro-copia.importou         = YES 
               tt-erro-copia.errorNumber      = RowErrors.errorNumber
               tt-erro-copia.errorDescription = RowErrors.errorDescription. 
    END.

    RETURN "NOK":U.
END.

RETURN "OK".

END PROCEDURE.

