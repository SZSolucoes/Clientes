&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCRM004RP 2.04.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

/* ***************************  Definitions  ************************** */
DEFINE NEW GLOBAL SHARED VARIABLE i-ep-codigo-usuario AS char NO-UNDO.
/* Temporary Table Definitions ---                                      */
define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD l-execucao-cont  AS LOGICAL
    FIELD i-minutos        AS INTEGER.

DEFINE TEMP-TABLE tt-raw-digita 
    FIELD raw-digita       AS RAW.
    
/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER raw-param AS RAW      NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR tt-raw-digita.

/* Recebimento de ParÉmetros */
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* Local Variable Definitions ---                                       */
DEFINE STREAM str-log. /* 18/08/2015 - modificado str-rp para str-log */
DEFINE STREAM str-file.
/* DEFINE STREAM str-open. - 18/08/2015 nao usado */
DEFINE VARIABLE h-acomp AS HANDLE     NO-UNDO.
{btb/btb912zc.i3}
/* include padr∆o para vari†veis para o log  */
{include/i-rpvar.i}

/*** Definicao da Temp Tables do Peidido *************************************/
DEF TEMP-TABLE tt-ped-venda  NO-UNDO LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-item   NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEF NEW SHARED TEMP-TABLE RowErrors NO-UNDO
    FIELD errorSequence         AS INT
    FIELD errorNumber           AS INT
    FIELD errorDescription      AS CHAR
    FIELD errorParameters       AS CHAR
    FIELD errorType             AS CHAR
    FIELD errorHelp             AS CHAR
    FIELD errorsubtype          AS CHAR.

DEF TEMP-TABLE tt-log-ped-venda NO-UNDO
    FIELD nome-abrev            LIKE ped-item.nome-abrev
    FIELD nr-pedcli             LIKE ped-item.nr-pedcli
    FIELD importou              AS   LOGICAL  FORMAT "Sim/Nao" INIT "Nao"
    FIELD data-import           AS   DATE FORMAT "99/99/9999"
    FIELD hora-import           AS   CHAR FORMAT "x(10)" 
    FIELD cd-erro               AS   INT
    FIELD mensagem              AS   CHAR FORMAT "x(300)"
    FIELD cod-estabel           LIKE estabelec.cod-estabel   
    FIELD emergencial           AS   INTEGER
    FIELD tipo-transacao        AS   INTEGER
    FIELD dt-implant            AS   DATE
    FIELD dt-entrega            AS   DATE
    FIELD nat-operacao          AS   CHARACTER
    FIELD cod-cond-pag          AS   INTEGER
    FIELD nr-tabpre             AS   CHARACTER
    FIELD cod-priori            AS   INTEGER
    FIELD perc-desco1           AS   DECIMAL
    FIELD perc-desco2           AS   DECIMAL
    FIELD observacoes           AS   CHARACTER                                          
    FIELD nome-transp           AS   CHARACTER
    FIELD tb-preco              AS   INTEGER
    FIELD mo-codigo             AS   INTEGER
    FIELD no-ab-reppri          AS   CHARACTER
    FIELD vl-desconto           AS   DECIMAL
    FIELD nome-ab-rep           AS   CHARACTER
    FIELD cod-entrega           AS   CHARACTER
    FIELD tipo-fatur            AS   INTEGER
    FIELD dt-min-fat            AS   DATE
    FIELD transp-redesp         AS   CHARACTER
    FIELD nr-pedrep             AS   CHARACTER
    FIELD cidade-cif            AS   CHARACTER
    INDEX ch-pedido IS PRIMARY
          nome-abrev
          nr-pedcli.

DEF TEMP-TABLE tt-log-ped-item  NO-UNDO
    FIELD nome-abrev            LIKE ped-item.nome-abrev
    FIELD nr-pedcli             LIKE ped-item.nr-pedcli
    FIELD nr-sequencia          LIKE ped-item.nr-sequencia
    FIELD it-codigo             LIKE ped-item.it-codigo
    FIELD cod-refer             LIKE ped-item.cod-refer
    FIELD importou              AS   LOGICAL  FORMAT "Sim/Nao" INIT "Nao"
    FIELD cd-erro               AS   INT
    FIELD mensagem              AS   CHAR FORMAT "x(300)"
    FIELD cod-estabel           LIKE estabelec.cod-estabel          
    FIELD tipo-transacao        AS   INTEGER
    FIELD nat-operacao          LIKE natur-oper.nat-operacao         
    FIELD qt-pedida             AS   DECIMAL
    FIELD vl-preori             AS   DECIMAL
    FIELD vl-preuni             AS   DECIMAL
    FIELD per-des-item          AS   DECIMAL
    FIELD nr-tabpre             AS   CHARACTER
    FIELD tb-preco              AS   INTEGER
    FIELD vl-total              AS   DECIMAL
    FIELD perc-icms             AS   DECIMAL
    FIELD obs-item-ped          AS   CHARACTER
    INDEX ch-item-ped IS PRIMARY
          nome-abrev
          nr-pedcli
          nr-sequencia
          it-codigo
          cod-refer.

/* Variaveis de Leitura de Pedido */
DEFINE TEMP-TABLE tt-arq_ped NO-UNDO
    FIELD c_arq_ped_cod-estabel     AS CHARACTER
    FIELD c_arq_ped_cod-emitente    AS CHARACTER
    FIELD c_arq_ped_nr-pedcli       AS CHARACTER
    FIELD c_arq_ped_emergencial      AS CHARACTER
    FIELD c_arq_ped_dt-entrega      AS CHARACTER
    FIELD c_arq_ped_cod-cond-pag    AS CHARACTER
    FIELD c_arq_ped_nr-tabpre       AS CHARACTER
    FIELD c_arq_ped_cod-priori      AS CHARACTER
    FIELD c_arq_ped_observacoes     AS CHARACTER
    FIELD c_arq_ped_nome-transp     AS CHARACTER
    FIELD c_arq_ped_tb-preco        AS CHARACTER
    FIELD c_arq_ped_mo-codigo       AS CHARACTER
    FIELD c_arq_ped_no-ab-reppri    AS CHARACTER
    FIELD c_arq_ped_vl-desconto     AS CHARACTER
    FIELD c_arq_ped_nome-ab-rep     AS CHARACTER
    FIELD c_arq_ped_cod-entrega     AS CHARACTER
    FIELD c_arq_ped_tipo-fatur      AS CHARACTER
    FIELD c_arq_ped_nat-operacao    AS CHARACTER
    FIELD c_arq_ped_dt-min-fat      AS CHARACTER
    FIELD c_arq_ped_transp-redesp   AS CHARACTER
    FIELD c_arq_ped_nr-pedrep       AS CHARACTER
    FIELD c_arq_ped_cidade-cif      AS CHARACTER
    FIELD c_arq_ped_cod-canal-venda AS CHARACTER
    FIELD c_arq_ped_e-mail              AS CHARACTER
    FIELD c_arq_ped_nome-abrev-tri  AS CHAR
    FIELD c_arq_ped_cod-entrega-tri AS CHAR
    field c_cod_unid_negocio        AS CHAR
    FIELD c_arq_ped_dest_merc       AS CHAR.


/* Variaveis de Leitura de Item */

DEFINE TEMP-TABLE tt-arq_ite NO-UNDO
    FIELD c_arq_ped_cod-emitente     AS CHARACTER
    FIELD c_arq_ped_nr-pedcli        AS CHARACTER
    FIELD c_arq_ite_nr-sequencia     AS CHARACTER
    FIELD c_arq_ite_it-codigo        AS CHARACTER
    FIELD c_arq_ite_qt-pedida        AS CHARACTER
    FIELD c_arq_ite_vl-preori        AS CHARACTER
    FIELD c_arq_ite_nr-tabpre        AS CHARACTER
    FIELD c_arq_ite_tb-preco         AS CHARACTER
    FIELD c_arq_ite_vl-total         AS CHARACTER
    FIELD c_arq_ite_perc-icms        AS CHARACTER
    FIELD c_arq_ite_obs-item-ped     AS CHARACTER
    FIELD c_arq_ite_nat-operacao     AS CHARACTER
    FIELD c_arq_ite_cod-estabel      AS CHARACTER
    FIELD c_arq_ite_dt-entrega       AS CHARACTER
    FIELD c_arq_ite_ind-icm-ret      AS CHARACTER
    FIELD c_arq_ite_lanc_minimo      AS CHARACTER
    FIELD c_arq_ite_lanc_obrigato    AS CHARACTER       
    FIELD c_arq_ite_ped_compr        AS CHARACTER
    FIELD c_arq_ite_seq_ped_compr    AS CHARACTER
    field c_cod_unid_negocio         as character
    FIELD c_arq_ite_per_st_icm       AS CHARACTER
    FIELD c_arq_ite_per_icm_estad_st AS CHARACTER.


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
/*****************************************************************************/

/****************XMLS*********************************************/
DEFINE VARIABLE hXdoc       AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRoot       AS HANDLE     NO-UNDO.
DEFINE VARIABLE hPedido     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hChild      AS HANDLE     NO-UNDO.
DEFINE VARIABLE i           AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-arquivo   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE content     AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE fileBinary  AS MEMPTR     NO-UNDO.
DEFINE VARIABLE conteudo    AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE Memory      AS MEMPTR     NO-UNDO.
DEFINE STREAM str-imp.
DEFINE VARIABLE lResult     AS LOGICAL    NO-UNDO.
/*****************************************************************/

/* Variaveis da BO */
DEF VAR bo-ped-venda                    AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-venda-sdf                AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-venda-com                AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-venda-cal                AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-venda-can                AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-item                     AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-item-sdf                 AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-item-can                 AS HANDLE                   NO-UNDO.
DEF VAR bo-ped-repre                    AS HANDLE                   NO-UNDO.
DEFINE VARIABLE h-alocacao AS HANDLE      NO-UNDO.

DEFINE VARIABLE c-nome-rep-indireto AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nome-rep-direto   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nome-redespacho   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-file              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-linha             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-validator         AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-embarque          AS INTEGER    NO-UNDO.
DEFINE VARIABLE l-arquivo-valido    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-dir-importacao    AS CHARACTER  NO-UNDO.

define buffer buf-cont-emit for cont-emit.


DEFINE VARIABLE l-erro-pedido-xml AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-erro-itens-pedido-xml AS LOGICAL     NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.88
         WIDTH              = 39.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


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
IF param-integra.l-integra-pedido THEN DO:
    /* complementando diretorio de importacao dos arquivos */
    FIND FIRST param-global NO-LOCK NO-ERROR.
    
    IF AVAIL param-global AND TRIM(STRING(param-global.empresa-prin)) <> '' THEN
       c-dir-importacao = param-integra.dir-arq-imp + "/empresa_" + TRIM(STRING(param-global.empresa-prin)).
    ELSE 
       c-dir-importacao = param-integra.dir-arq-imp.
     
    INPUT STREAM str-file FROM OS-DIR(c-dir-importacao).
    REPEAT:
        IMPORT STREAM str-file c-file.

/*         IF SUBSTRING(c-file,9,7) <> "pedidos" THEN NEXT. */

        /****elimina arquivos que n∆o s∆o XMls - CFF Sottelli ******/
        IF INDEX(c-file,"xml") <= 0 THEN NEXT.

/*         i-validator = INTEGER(SUBSTRING(c-file,1,8)) NO-ERROR.  */
/*         IF i-validator = ? OR i-validator = 0 THEN NEXT.        */
/*                                                                 */
/*         i-validator = INTEGER(SUBSTRING(c-file,16,6)) NO-ERROR. */
/*         IF i-validator = ? OR i-validator = 0 THEN NEXT.        */
    
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
               l-erro-pedido-xml = NO
               l-erro-itens-pedido-xml = NO.

        SET-SIZE(fileBinary) = 0.
        FILE-INFO:FILE-NAME = c-arquivo.
        SET-SIZE(fileBinary) = FILE-INFO:FILE-SIZE.
        INPUT STREAM str-imp  FROM VALUE(c-arquivo) BINARY NO-CONVERT.
        IMPORT STREAM str-imp  fileBinary.
        INPUT STREAM str-imp  CLOSE.
        
        COPY-LOB fileBinary TO Memory CONVERT TARGET CODEPAGE 'utf-8'.
        lResult = hXdoc:LOAD("MEMPTR",Memory,FALSE) NO-ERROR.
        IF NOT lResult THEN DO:
            MESSAGE "Arquivo XML mal formatado: " 
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
            l-arquivo-valido = NO.
        END.
        ELSE DO:
            assign l-arquivo-valido = yes.
           hXdoc:GET-DOCUMENT-ELEMENT(hRoot).    
            
             IF TRIM(hRoot:NAME) <> "Pedido" THEN DO:
                 l-arquivo-valido = NO.
             END.
               
             ELSE DO:
                 CREATE X-NODEREF hChild.
        
                 CREATE tt-arq_ped.
        
                 DO i = 1 TO hRoot:NUM-CHILDREN:
                     hRoot:GET-CHILD(hChild,i).
        
                     IF hChild:NAME = "Itens" THEN
                        RUN pi-processa-itens (INPUT hChild,
                                               OUTPUT l-erro-pedido-xml).
                     ELSE
                        RUN pi-processa-dados (INPUT hChild,
                                               OUTPUT l-erro-itens-pedido-xml).
                                               
                   
                        
/*                  if l-erro-pedido-xml  or l-erro-itens-pedido-xml then */
/*                      assign l-arquivo-valido = no.                     */
/*                  else                                                  */
/*                      assign l-arquivo-valido = yes.                    */
/*                                                                        */
                 END.

             END.
        END.
/*         FOR EACH tt-arq_ped NO-LOCK:                                          */
/*             MESSAGE                                                           */
/*                                                                               */
/*             'c_arq_ped_cod-estabel     - ' c_arq_ped_cod-estabel      skip    */
/*             'c_arq_ped_cod-emitente    - ' c_arq_ped_cod-emitente     skip    */
/*             'c_arq_ped_nr-pedcli       - ' c_arq_ped_nr-pedcli        skip    */
/*             'c_arq_ped_emergencial     - ' c_arq_ped_emergencial      skip    */
/*             'c_arq_ped_dt-entrega      - ' c_arq_ped_dt-entrega       skip    */
/*             'c_arq_ped_cod-cond-pag    - ' c_arq_ped_cod-cond-pag     skip    */
/*             'c_arq_ped_nr-tabpre       - ' c_arq_ped_nr-tabpre        skip    */
/*             'c_arq_ped_cod-priori      - ' c_arq_ped_cod-priori       skip    */
/*             'c_arq_ped_observacoes     - ' c_arq_ped_observacoes      skip    */
/*             'c_arq_ped_nome-transp     - ' c_arq_ped_nome-transp      skip    */
/*             'c_arq_ped_tb-preco        - ' c_arq_ped_tb-preco         skip    */
/*             'c_arq_ped_mo-codigo       - ' c_arq_ped_mo-codigo        skip    */
/*             'c_arq_ped_no-ab-reppri    - ' c_arq_ped_no-ab-reppri     skip    */
/*             'c_arq_ped_vl-desconto     - ' c_arq_ped_vl-desconto      skip    */
/*             'c_arq_ped_nome-ab-rep     - ' c_arq_ped_nome-ab-rep      skip    */
/*             'c_arq_ped_cod-entrega     - ' c_arq_ped_cod-entrega      skip    */
/*             'c_arq_ped_tipo-fatur      - ' c_arq_ped_tipo-fatur       skip    */
/*             'c_arq_ped_nat-operacao    - ' c_arq_ped_nat-operacao     skip    */
/*             'c_arq_ped_dt-min-fat      - ' c_arq_ped_dt-min-fat       skip    */
/*             'c_arq_ped_transp-redesp   - ' c_arq_ped_transp-redesp    skip    */
/*             'c_arq_ped_nr-pedrep       - ' c_arq_ped_nr-pedrep        skip    */
/*             'c_arq_ped_cidade-cif      - ' c_arq_ped_cidade-cif       skip    */
/*             'c_arq_ped_cod-canal-venda - ' c_arq_ped_cod-canal-venda  skip    */
/*             'c_arq_ped_e-mail          - ' c_arq_ped_e-mail           skip    */
/*             'c_arq_ped_nome-abrev-tri  - ' c_arq_ped_nome-abrev-tri   skip    */
/*             'c_arq_ped_cod-entrega-tri - ' c_arq_ped_cod-entrega-tri  skip    */
/*             'c_cod_unid_negocio        - ' c_cod_unid_negocio         skip    */
/*             'c_arq_ped_dest_merc       - ' c_arq_ped_dest_merc                */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                            */
/*         END.                                                                  */
/*                                                                               */
/*         FOR EACH tt-arq_ite NO-LOCK:                                          */
/*            MESSAGE                                                            */
/*                                                                               */
/*             'c_arq_ped_cod-emitente      - ' c_arq_ped_cod-emitente      skip */
/*             'c_arq_ped_nr-pedcli         - ' c_arq_ped_nr-pedcli         skip */
/*             'c_arq_ite_nr-sequencia      - ' c_arq_ite_nr-sequencia      skip */
/*             'c_arq_ite_it-codigo         - ' c_arq_ite_it-codigo         skip */
/*             'c_arq_ite_qt-pedida         - ' c_arq_ite_qt-pedida         skip */
/*             'c_arq_ite_vl-preori         - ' c_arq_ite_vl-preori         skip */
/*             'c_arq_ite_nr-tabpre         - ' c_arq_ite_nr-tabpre         skip */
/*             'c_arq_ite_tb-preco          - ' c_arq_ite_tb-preco          skip */
/*             'c_arq_ite_vl-total          - ' c_arq_ite_vl-total          skip */
/*             'c_arq_ite_perc-icms         - ' c_arq_ite_perc-icms         skip */
/*             'c_arq_ite_obs-item-ped      - ' c_arq_ite_obs-item-ped      skip */
/*             'c_arq_ite_nat-operacao      - ' c_arq_ite_nat-operacao      skip */
/*             'c_arq_ite_cod-estabel       - ' c_arq_ite_cod-estabel       skip */
/*             'c_arq_ite_dt-entrega        - ' c_arq_ite_dt-entrega        skip */
/*             'c_arq_ite_ind-icm-ret       - ' c_arq_ite_ind-icm-ret       skip */
/*             'c_arq_ite_lanc_minimo       - ' c_arq_ite_lanc_minimo       skip */
/*             'c_arq_ite_lanc_obrigato     - ' c_arq_ite_lanc_obrigato     skip */
/*             'c_arq_ite_ped_compr         - ' c_arq_ite_ped_compr         skip */
/*             'c_arq_ite_seq_ped_compr     - ' c_arq_ite_seq_ped_compr     skip */
/*             'c_cod_unid_negocio          - ' c_cod_unid_negocio          skip */
/*             'c_arq_ite_per_st_icm        - ' c_arq_ite_per_st_icm        skip */
/*             'c_arq_ite_per_icm_estad_st  - ' c_arq_ite_per_icm_estad_st       */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                */
/*                                                                               */
/*         END.                                                                  */
/*             FOR EACH  tt-arq_lote NO-LOCK:                                    */
/*         MESSAGE                                                               */
/*                                                                               */
/*         ' nome-abrev    - ' nome-abrev   skip                                 */
/*         ' nr-pedcli     - ' nr-pedcli    skip                                 */
/*         ' nr-sequencia  - ' nr-sequencia skip                                 */
/*         ' it-codigo     - ' it-codigo    skip                                 */
/*         ' cod-refer     - ' cod-refer    skip                                 */
/*         ' codigo        - ' codigo       skip                                 */
/*         ' quantidade    - ' quantidade   skip                                 */
/*         ' dt-validade   - ' dt-validade                                       */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                               */
/*             END.                                                              */
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
            
/*         INPUT STREAM str-open FROM VALUE(param-integra.dir-arq-imp + "/" + c-file) NO-CONVERT. */
/*                                                                                                */
/*         IMPORT STREAM str-open UNFORMAT c-linha.                                               */
/*                                                                                                */
/*         IF NUM-ENTRIES(c-linha,";") = 28 THEN DO:                                              */
/*             RUN pi-importa-tabelas(INPUT "ped-venda").                                         */
/*             REPEAT:                                                                            */
/*                 IMPORT STREAM str-open UNFORMAT c-linha.                                       */
/*                                                                                                */
/*                 IF c-linha = "#FIM#" THEN DO:                                                  */
/*                     l-arquivo-valido = YES.                                                    */
/*                     LEAVE.                                                                     */
/*                 END.                                                                           */
/*                                                                                                */
/*                 IF NUM-ENTRIES(c-linha,";") = 20 THEN                                          */
/*                     RUN pi-importa-tabelas(INPUT "ped-item").                                  */
/*                 ELSE DO:                                                                       */
/*                     l-arquivo-valido = NO.                                                     */
/*                     LEAVE.                                                                     */
/*                 END.                                                                           */
/*                                                                                                */
/*             END.                                                                               */
/*         END.                                                                                   */
/*                                                                                                */
/*         INPUT STREAM str-open CLOSE.                                                           */
    
        IF NOT l-arquivo-valido THEN DO:
            RUN pi-gera-arquivo-resposta.
            NEXT.
        END.
    
        FIND FIRST tt-arq_ped NO-LOCK NO-ERROR.
    
        RUN pi-validate-pedido.
    
        IF RETURN-VALUE = "NOK" THEN DO:

            RUN pi-gera-arquivo-resposta.
            NEXT.
        END.
        RUN pi-cadastra-pedido.
    
        RUN pi-gera-arquivo-resposta.
            
        
        /**************
        FOR EACH est-ped-venda EXCLUSIVE-LOCK
           WHERE est-ped-venda.embarque = 1:
        
            IF est-ped-venda.embarque > 1 THEN NEXT.
        
            RUN esp/escrm004rpa.p(INPUT  est-ped-venda.nome-abrev,
                                  INPUT  est-ped-venda.nr-pedcli,
                                  OUTPUT est-ped-venda.embarque).
        
        END.
        ***************/
        
    END. /* fim REPEAT */
    /* 18/08/2015 */
    INPUT STREAM str-file CLOSE.
   
END.

DO TRANSACTION:
    FIND FIRST param-integra EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL param-integra THEN DO:
        ASSIGN param-integra.dt-ult-execucao-ped = TODAY 
               param-integra.hr-ult-execucao-ped = REPLACE(STRING(TIME,"HH:MM:SS"),":","").
    END.
    release param-integra.
END.

/* 18/08/2015  - fechamento duvidoso */
/* INPUT STREAM str-file CLOSE. */

RUN pi-cria-pendencia-rpw.

RUN pi-finalizar IN h-acomp. 

RETURN "Ok":U.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-acompanhamento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-acompanhamento Procedure 
PROCEDURE pi-acompanhamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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

&IF DEFINED(EXCLUDE-pi-cadastra-pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cadastra-pedido Procedure 
PROCEDURE pi-cadastra-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli)).  
    
    FOR EACH tt-ped-venda: DELETE tt-ped-venda. END.
    FOR EACH tt-ped-item:  DELETE tt-ped-item.  END.

    PedidoTotal:
    DO TRANSACTION:

        FIND FIRST param-global NO-LOCK NO-ERROR.
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
           disable triggers for load of emitente.
           
           FIND CURRENT emitente EXCLUSIVE-LOCK NO-ERROR.

           ASSIGN emitente.e-mail = tt-arq_ped.c_arq_ped_e-mail.
           
           /* atualizando contato da NFe */
           find first cont-emit of emitente 
                where cont-emit.int-1 = 2 /* destinatario NFe */ 
           exclusive-lock no-error.
           
           if not avail cont-emit then
           do:
              find last buf-cont-emit of emitente no-lock no-error.
           
              create cont-emit.
              assign cont-emit.cod-emitente = emitente.cod-emitente
                     cont-emit.sequencia    = if not avail buf-cont-emit then 10 else buf-cont-emit.sequencia + 10
                     cont-emit.nome         = 'CONTATO NFE'
                     cont-emit.identific    = emitente.identific
                     cont-emit.int-1        = 2.                     
           end.
           
           cont-emit.e-mail = emitente.e-mail.
        END.
        /*********************************/
    
        /*** Busca os Valores Defaults do Pedido *************************************************/
        /* aqui
        RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - API: BoDi159Sdf.p").  
        */
        
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
               tt-ped-venda.nr-pedido   = NEXT-VALUE(seq-nr-pedido)
               tt-ped-venda.cod-estabel = tt-arq_ped.c_arq_ped_cod-estabel.


        RUN inputtable             IN bo-ped-venda-sdf (INPUT TABLE tt-ped-venda).
        RUN setdefaultcustomer     IN bo-ped-venda-sdf.
        RUN setDefaultCentralSales IN bo-ped-venda-sdf.
        RUN outputTable            IN bo-ped-venda-sdf (OUTPUT TABLE tt-ped-venda). 

        find first natur-oper where natur-oper.nat-operacao =  tt-arq_ped.c_arq_ped_nat-operacao no-lock no-error.
    
        FIND FIRST tt-ped-venda.
        /*** Altera Alguns Campos do Pedido passados *********************************************/
        ASSIGN tt-ped-venda.nat-operacao  = tt-arq_ped.c_arq_ped_nat-operacao
               tt-ped-venda.no-ab-reppri  = c-nome-rep-direto
               tt-ped-venda.cod-cond-pag  = if avail natur-oper and (natur-oper.transf OR NOT natur-oper.emite-duplic) then 0 else INTEGER(tt-arq_ped.c_arq_ped_cod-cond-pag)
               tt-ped-venda.dt-emissao    = TODAY
               tt-ped-venda.dt-implant    = TODAY
               tt-ped-venda.dt-entrega    = DATE(tt-arq_ped.c_arq_ped_dt-entrega)
               tt-ped-venda.dt-entorig    = DATE(tt-arq_ped.c_arq_ped_dt-entrega)
               tt-ped-venda.nome-transp   = tt-arq_ped.c_arq_ped_nome-transp
               tt-ped-venda.dt-fimvig     = ?
               tt-ped-venda.observacoes   = tt-arq_ped.c_arq_ped_observacoes
               tt-ped-venda.cond-espec    = ""
               tt-ped-venda.tp-preco      = INTEGER(tt-arq_ped.c_arq_ped_tb-preco)
               tt-ped-venda.cidade-cif    = ""              
               tt-ped-venda.nr-tabpre     = tt-arq_ped.c_arq_ped_nr-tabpre
               tt-ped-venda.user-alt      = ""  
               tt-ped-venda.dt-useralt    = ?
               tt-ped-venda.nr-pedrep     = tt-arq_ped.c_arq_ped_nr-pedrep
               tt-ped-venda.nome-tr-red   = c-nome-redespacho
               tt-ped-venda.ind-fat-par   = IF INTEGER(tt-arq_ped.c_arq_ped_tipo-fatur) = 2 THEN YES ELSE NO
               tt-ped-venda.cidade-cif    = IF tt-arq_ped.c_arq_ped_cidade-cif = "FOB" THEN "" ELSE "CIF"
               tt-ped-venda.cod-entrega   = tt-arq_ped.c_arq_ped_cod-entrega
               tt-ped-venda.cod-canal-venda = (IF INTEGER(tt-arq_ped.c_arq_ped_cod-canal-venda) = 0 THEN tt-ped-venda.cod-canal-venda 
                                               ELSE INTEGER(tt-arq_ped.c_arq_ped_cod-canal-venda) )
               tt-ped-venda.cod-estabel     = tt-arq_ped.c_arq_ped_cod-estabel
               tt-ped-venda.nome-abrev-tri  = tt-arq_ped.c_arq_ped_nome-abrev-tri 
               tt-ped-venda.cod-entrega-tri = tt-arq_ped.c_arq_ped_cod-entrega-tri
               tt-ped-venda.cod-unid-negoc  = tt-arq_ped.c_cod_unid_negocio
               tt-ped-venda.cod-des-merc    = INTEGER(tt-arq_ped.c_arq_ped_dest_merc).
               
               /**Caso a natureza seja de transferencia, atribuir o Destino da Mercadoria como - Comercio-Industria - Cfreire sottelli**/
               find first natur-oper where natur-oper.nat-operacao = tt-arq_ped.c_arq_ped_nat-operacao no-lock no-error.
               if avail natur-oper and natur-oper.transf then
                   assign tt-ped-venda.cod-des-merc  = 1.

        /*** Defauts Condicao de Pagamento *******************************************************/
        ASSIGN tt-ped-venda.nr-tab-finan = if natur-oper.transf = no then cond-pagto.nr-tab-finan else 1
               tt-ped-venda.nr-ind-finan = if natur-oper.transf = no then cond-pagto.nr-ind-finan else 2.
    
        /*** Avaliacao de Credito ****************************************************************/
        ASSIGN tt-ped-venda.cod-sit-aval  = 3  /* APROVADO */
               tt-ped-venda.desc-forc-cr  = "Liberado Automatico".
        /* aqui

        /*** Valida a HANDLE do Pedido ***********************************************************/
        RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - API: BoDi159.p").  
    
    */
        /*** Cria o Pedido ***********************************************************************/
        RUN emptyRowErrors  IN bo-ped-venda.
        RUN setRecord       IN bo-ped-venda(INPUT TABLE tt-ped-venda).
        RUN createRecord    IN bo-ped-venda.
    
        /*** Verifica os Erros Retornados *******************************************************/
        RUN getRowErrors    IN bo-ped-venda(OUTPUT TABLE RowErrors).

      /* aqui

        RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Erros e Avisos").  
        */
            
        /*** ERROS ******************************************************************************/
        IF  CAN-FIND(FIRST RowErrors    
                     WHERE RowErrors.ErrorSubType = "ERROR":U) THEN 
        DO:
            FOR EACH rowerrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U:

               ASSIGN tt-log-ped-venda.importou   = FALSE 
                      tt-log-ped-venda.cd-erro    = rowerrors.errornumber                        
                      tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp.
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
        IF  CAN-FIND(FIRST RowErrors    
                     WHERE RowErrors.ErrorSubType <> "ERROR":U) THEN 
        DO:
            FOR EACH rowerrors 
               WHERE RowErrors.ErrorSubType <> "ERROR":U:                    
               ASSIGN tt-log-ped-venda.cd-erro    = rowerrors.errornumber                        
                      tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp.
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
        
        RUN getRowid In bo-ped-venda(OUTPUT tt-ped-venda.r-rowid).
        
        RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Criando Repres").  
    
        /*** Comissao dos Representantes 
        **** Representante Direto
        ****************************************************************************/
        FIND FIRST ped-repre
             WHERE ped-repre.nr-pedido   = tt-ped-venda.nr-pedido
               AND ped-repre.nome-ab-rep = c-nome-rep-direto
               NO-ERROR.
        IF NOT AVAIL ped-repre THEN
        DO:
            CREATE ped-repre.
            ASSIGN ped-repre.nr-pedido   = tt-ped-venda.nr-pedido
                   ped-repre.nome-ab-rep = c-nome-rep-direto
                   ped-repre.ind-repbase = YES
                   ped-repre.perc-comis  = repres.comis-direta
                   ped-repre.comis-emis  = repres.comis-emis.
        END. /*** Fim do IF NOT AVAIL ***/
    
        /*** Comissao dos Representantes 
        **** Representante Indireto   *********************************************/
        FIND FIRST ped-repre
             WHERE ped-repre.nr-pedido   = tt-ped-venda.nr-pedido
               AND ped-repre.nome-ab-rep = c-nome-rep-indireto
               NO-ERROR.
        IF NOT AVAIL ped-repre THEN
        DO:
            CREATE ped-repre.
            ASSIGN ped-repre.nr-pedido   = tt-ped-venda.nr-pedido
                   ped-repre.nome-ab-rep = c-nome-rep-indireto
                   ped-repre.ind-repbase = YES
                   ped-repre.perc-comis  = 0
                   ped-repre.comis-emis  = 0.
        END. /*** Fim do IF NOT AVAIL ***/
        
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

        RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Criando Itens").  

        FOR EACH tt-arq_ite
           WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
             AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli 
             NO-LOCK :

           /*** Limpa as Tabelas ****************************************************************/
           EMPTY TEMP-TABLE tt-ped-item. 

           /*** Cria Itens do Pedido ************************************************************/
           CREATE tt-ped-item.
           ASSIGN tt-ped-item.nome-abrev   = emitente.nome-abrev
                  tt-ped-item.nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli 
                  tt-ped-item.nr-sequencia = INTEGER(tt-arq_ite.c_arq_ite_nr-sequencia)
                  tt-ped-item.it-codigo    = tt-arq_ite.c_arq_ite_it-codigo 
                  tt-ped-item.cod-refer    = ""
                  tt-ped-item.tipo-atend   = IF INTEGER(tt-arq_ped.c_arq_ped_tipo-fatur) = 1 THEN 1 ELSE 2
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
           RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Itens API: BoDi154Sdf.p ").  

           IF NOT VALID-HANDLE(bo-ped-item-sdf) THEN
               RUN dibo/bodi154sdf.p PERSISTENT SET bo-ped-item-sdf.

           RUN inputtable      IN bo-ped-item-sdf (INPUT TABLE tt-ped-item).
           RUN setdefaultitem  IN bo-ped-item-sdf.
           RUN outputtable     IN bo-ped-item-sdf (OUTPUT TABLE tt-ped-item). 

           IF VALID-HANDLE(bo-ped-item-sdf) THEN DO:
               DELETE PROCEDURE bo-ped-item-sdf.
               bo-ped-item-sdf = ?.
           END.


           FIND FIRST tt-ped-item.
           /*** Altera Alguns Campos Passados *************************************************/
           ASSIGN tt-ped-item.qt-pedida               = DECIMAL(tt-arq_ite.c_arq_ite_qt-pedida)
                  tt-ped-item.vl-preuni               = DECIMAL(tt-arq_ite.c_arq_ite_vl-preori)
                  tt-ped-item.vl-preori               = DECIMAL(tt-arq_ite.c_arq_ite_vl-preori)
                  tt-ped-item.des-pct-desconto-inform = "0"
                 /*tt-ped-item.qt-log-aloca            = tt-arq_ite.c_arq_ite_qt-pedida*/ 
                  tt-ped-item.dt-min-fat              = DATE(tt-arq_ped.c_arq_ped_dt-min-fat)
                  tt-ped-item.observacao              = tt-arq_ite.c_arq_ite_obs-item-ped
                  tt-ped-item.nat-operacao            = tt-arq_ite.c_arq_ite_nat-operacao
                  tt-ped-item.dt-entrega              = DATE(tt-arq_ite.c_arq_ite_dt-entrega)
                  tt-ped-item.dt-entorig              = tt-ped-item.dt-entrega
                  tt-ped-item.ind-icm-ret             = (tt-arq_ite.c_arq_ite_ind-icm-ret = "1")
                  tt-ped-item.cod-unid-negoc          = tt-arq_ite.c_cod_unid_negocio
                  tt-ped-item.cod-ord-compra          = tt-arq_ite.c_arq_ite_ped_compr.
                  tt-ped-item.parcela                 = INTEGER(tt-arq_ite.c_arq_ite_seq_ped_compr) NO-ERROR.

           IF tt-ped-item.parcela = ? THEN
              tt-ped-item.parcela = 0.

           /*** Valida a HANDLE do Item do Pedido *********************************************/
           RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Itens API: BoDi154.p ").  

           IF NOT VALID-HANDLE(bo-ped-item) THEN
               RUN dibo/bodi154.p    PERSISTENT SET bo-ped-item.

           /*** Cria o Item do Pedido *********************************************************/
           RUN openQueryStatic IN bo-ped-item (INPUT "Main":U).
           RUN setRecord       IN bo-ped-item (INPUT TABLE tt-ped-item).
           RUN emptyRowErrors  IN bo-ped-item.
           RUN createRecord    IN bo-ped-item.


           RUN pi-acompanhamento("Pedido: " + string(tt-arq_ped.c_arq_ped_nr-pedcli) + " - Itens Erros e Avisos").  

           /*** ERROS ***************************************************************************/
           RUN getRowErrors    IN bo-ped-item(OUTPUT TABLE RowErrors).
           IF  CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.ErrorSubType = "ERROR":U) THEN 
           DO:
               FOR EACH rowerrors 
                  WHERE RowErrors.ErrorSubType = "ERROR":U:
                  

                  ASSIGN tt-log-ped-item.importou     = FALSE
                         tt-log-ped-item.cd-erro      = rowerrors.errornumber                        
                         tt-log-ped-item.mensagem     = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp.  
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
               IF  CAN-FIND(FIRST RowErrors 
                            WHERE RowErrors.ErrorSubType <> "ERROR":U) THEN 
               DO:
                   FOR EACH rowerrors 
                      WHERE RowErrors.ErrorSubType <> "ERROR":U:

                      ASSIGN tt-log-ped-item.cd-erro      = rowerrors.errornumber                        
                             tt-log-ped-item.mensagem     = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp.  
                   END.

               END.

           END.
           
           FIND FIRST tt-ped-item.
           
           CREATE esp-ped-item.
           ASSIGN esp-ped-item.nome-abrev        = tt-ped-item.nome-abrev
                  esp-ped-item.nr-pedcli         = tt-ped-item.nr-pedcli
                  esp-ped-item.nr-sequencia      = tt-ped-item.nr-sequencia
                  esp-ped-item.it-codigo         = tt-ped-item.it-codigo
                  esp-ped-item.cod-refer         = tt-ped-item.cod-refer
                  esp-ped-item.lance-minimo      = DECIMAL(tt-arq_ite.c_arq_ite_lanc_minimo)
                  esp-ped-item.lance-obrigatorio = DECIMAL(tt-arq_ite.c_arq_ite_lanc_obrigato)
                  esp-ped-item.ped-compr         = tt-arq_ite.c_arq_ite_ped_compr    
                  esp-ped-item.seq-ped-compr     = tt-arq_ite.c_arq_ite_seq_ped_compr
                  esp-ped-item.per-sub-icm-trib  = DECIMAL(tt-arq_ite.c_arq_ite_per_st_icm)      
                  esp-ped-item.per-icm-estad-sub = DECIMAL(tt-arq_ite.c_arq_ite_per_icm_estad_st).
                  
           
           tt-log-ped-item.importou     = TRUE.
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
        RUN pi-calcula-pedido.
        IF RETURN-VALUE = "NOK" THEN
            RETURN "NOK".

        /*** Efetiva o Pedido ***********************************************/
        RUN pi-acompanhamento("Pedido - Efetivando Pedido").  
        RUN pi-efetiva-pedido.
        IF RETURN-VALUE = "NOK" THEN
            RETURN "NOK".

        /*** Elimina o Pedido da Intermediaria *******************************/
        RUN pi-acompanhamento("Terminando Processamento").  

        FIND FIRST ped-venda
             WHERE ped-venda.nome-abrev = emitente.nome-abrev
               AND ped-venda.nr-pedcli  = tt-arq_ped.c_arq_ped_nr-pedcli
               NO-ERROR.

        IF AVAIL ped-venda THEN
        DO:
            /**********CFF - Alocaá∆o de Lote 25/02/2015*******************************/
            if can-find (first ped-ent of ped-venda) 
                AND CAN-FIND (FIRST tt-arq_lote) then do:

                IF NOT VALID-HANDLE(bo-ped-venda) THEN
                    RUN dibo/bodi159.p    PERSISTENT SET bo-ped-venda.

                run emptyRowErrors     in bo-ped-venda.
                run validateAllocation in bo-ped-venda(input ROWID(ped-venda)).
                run getRowErrors       in bo-ped-venda(output table RowErrors).

           
                if  can-find(first RowErrors
                             where RowErrors.ErrorType <> "INTERNAL":U) THEN DO:

                     FOR EACH RowErrors NO-LOCK:
                         ASSIGN tt-log-ped-venda.cd-erro    = RowErrors.errornumber                        
                                tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp.
                     END.
                    
                END.
                else DO:                                  
                    FOR EACH ped-item OF ped-venda NO-LOCK:
                        FIND FIRST ped-ent 
                            WHERE ped-ent.nome-abrev     = ped-item.nome-abrev  
                              and ped-ent.nr-pedcli      = ped-item.nr-pedcli   
                              and ped-ent.it-codigo      = ped-item.it-codigo   
                              and ped-ent.cod-refer      = ped-item.cod-refer   
                              and ped-ent.nr-sequencia   = ped-item.nr-sequencia NO-LOCK NO-ERROR.
                        IF AVAIL ped-ent THEN DO:
                            FIND FIRST ITEM WHERE ITEM.it-codigo = ped-ent.it-codigo NO-LOCK NO-ERROR.
                            
                            FIND FIRST tt-arq_lote
                                 WHERE tt-arq_lote.nome-abrev   = ped-venda.nome-abrev
                                   and tt-arq_lote.nr-pedcli    = ped-venda.nr-pedcli 
                                   and tt-arq_lote.nr-sequencia = ped-item.nr-sequencia 
                                   and tt-arq_lote.it-codigo    = ped-item.it-codigo
                                   and tt-arq_lote.cod-refer    = ped-item.cod-refer NO-LOCK NO-ERROR.
                            IF AVAIL tt-arq_lote THEN DO:


                                 FOR each  saldo-estoq NO-LOCK                                                       
                                    where saldo-estoq.cod-depos   = tt-arq_lote.cod-depos                         
                                      AND saldo-estoq.cod-estabel = ped-venda.cod-estabel 
                                      AND saldo-estoq.cod-localiz = tt-arq_lote.cod-localiz
                                      AND saldo-estoq.lote        = tt-arq_lote.codigo
                                      and saldo-estoq.it-codigo   = ped-ent.it-codigo                                
                                      and saldo-estoq.cod-refer   = ped-ent.cod-refer                                
                                      and (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                      saldo-estoq.qt-aloc-ped +                      
                                                                      saldo-estoq.qt-aloc-prod)) > 0                 
                                                                                                                 
                                      AND (ITEM.tipo-con-est < 3  OR saldo-estoq.dt-vali-lote >= ped-ent.dt-entrega),
                                    first deposito no-lock                                                           
                                    where deposito.cod-depos = saldo-estoq.cod-depos                                 
                                      and deposito.ind-acabado:   

                                     IF (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                      saldo-estoq.qt-aloc-ped +                      
                                                                      saldo-estoq.qt-aloc-prod)) >= tt-arq_lote.quantidade  THEN DO:
                                        
                                         IF NOT VALID-HANDLE (h-alocacao) THEN
                                             run pdp/pdapi002.p persistent set h-alocacao.
                                    
                                         run pi-aloca-fisica-man in h-alocacao(input rowid(ped-ent),
                                                                                  input-output tt-arq_lote.quantidade, 
                                                                                  input rowid(saldo-estoq)).
                                                                               
                                     END.
                                     ELSE
                                         ASSIGN tt-log-ped-venda.cd-erro    = 99999                        
                                                tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + "Quantidade informada do lote maior que quantiade disponivel." .
                                                                                                                     
                                 END.                                                                                

                            END.

                        END.

                    END.
              
                END.
                    
            end.

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
           
           /**************************************************************************/



            /*** Cria o Pedido Emergencial ( SIM ou NAO ) ********************************/
            
            /******************************************************************
            FIND FIRST est-ped-venda
                 WHERE est-ped-venda.nome-abrev = ped-venda.nome-abrev
                   AND est-ped-venda.nr-pedcli  = ped-venda.nr-pedcli
                   NO-ERROR.

            IF NOT AVAIL est-ped-venda THEN
            DO:
                CREATE est-ped-venda.
                ASSIGN est-ped-venda.nome-abrev = ped-venda.nome-abrev
                       est-ped-venda.nr-pedcli  = ped-venda.nr-pedcli
                       est-ped-venda.embarque   = 1.

            END.
            IF INTEGER(tt-arq_ped.c_arq_ped_emergencial) = 1 THEN
               ASSIGN est-ped-venda.emergencial = YES. /* Emergencial */
            ELSE
               ASSIGN est-ped-venda.emergencial = NO.  /* NAO Emergencial */ 
            *****************************************************************/   

        END.     

        ASSIGN tt-log-ped-venda.importou = TRUE.
    END. /* Transaction */
    

    RETURN "NOK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-calcula-pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-pedido Procedure 
PROCEDURE pi-calcula-pedido :
/*** pi-Calcula-Pedido
**** Calcula valores Totais do Pedido de Venda 
********************************************************************************/
    IF  NOT VALID-HANDLE(bo-ped-venda-cal) THEN
        RUN dibo/bodi159cal.p PERSISTENT SET bo-ped-venda-cal.

    FIND FIRST tt-ped-venda.

    RUN calculateOrder IN bo-ped-venda-cal(INPUT tt-ped-venda.r-rowid).

    IF CAN-FIND(FIRST RowErrors
                WHERE RowErrors.ErrorSubType = "ERROR":U) THEN 
    DO:
       
        FOR EACH RowErrors  
            WHERE RowErrors.ErrorSubType = "ERROR":U:

            ASSIGN tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-pendencia-rpw Procedure 
PROCEDURE pi-cria-pendencia-rpw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-api  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE i_hora AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dt_tim AS DATE       NO-UNDO.    
    
    IF NOT tt-param.l-execucao-cont THEN
        RETURN "OK".
        
    FIND usuar_mestre NO-LOCK
    WHERE usuar_mestre.cod_usuar = tt-param.usuario NO-ERROR.
    
    FOR EACH tt_param_segur.        DELETE tt_param_segur.        END.
    FOR EACH tt_ped_exec_aux_3.     DELETE tt_ped_exec_aux_3.     END.
    FOR EACH tt_ped_exec_param.     DELETE tt_ped_exec_param.     END.
    FOR EACH tt_ped_exec_sel.       DELETE tt_ped_exec_sel.       END.
    FOR EACH tt_ped_exec_param_aux. DELETE tt_ped_exec_param_aux. END.
    
    CREATE tt_param_segur.
    ASSIGN tt_param_segur.tta_num_vers_integr_api       = 3
           tt_param_segur.tta_cod_aplicat_dtsul_corren  = "TEC"
           tt_param_segur.tta_cod_empres_usuar          = STRING(i-ep-codigo-usuario)
           tt_param_segur.tta_cod_grp_usuar_lst         = "sup"
           tt_param_segur.tta_cod_idiom_usuar           = "POR"
           tt_param_segur.tta_cod_pais_empres_usuar     = "BRA"
           tt_param_segur.tta_cod_usuar_corren          = tt-param.usuario
           tt_param_segur.tta_cod_usuar_corren_criptog  = ENCODE(tt-param.usuario).
    
    i_hora = TIME + i-minutos * 60.
    
    IF i_hora > 86399 THEN
        ASSIGN i_hora = i_hora - 86399
               dt_tim = TODAY + 1.
    ELSE
        ASSIGN dt_tim = TODAY.
    
    CREATE tt_ped_exec_aux_3.
    ASSIGN tt_ped_exec_aux_3.tta_num_seq                = 1
           tt_ped_exec_aux_3.tta_num_ped_exec           = NEXT-VALUE(seq_ped_exec,emsfnd)
           tt_ped_exec_aux_3.tta_cod_usuario            = tt-param.usuario
           tt_ped_exec_aux_3.tta_cod_prog_dtsul         = "escrm004"
           tt_ped_exec_aux_3.tta_cod_prog_dtsul_rp      = "esp/escrm004rp.p"
           tt_ped_exec_aux_3.tta_cod_release_prog_dtsu  = "2.04.00.001"
           tt_ped_exec_aux_3.tta_dat_exec_ped_exec      = dt_tim
           tt_ped_exec_aux_3.tta_hra_exec_ped_exec      = REPLACE(STRING(i_hora,"hh:mm:ss"),":","")
           tt_ped_exec_aux_3.tta_cod_servid_exec        = usuar_mestre.cod_servid_exec
           tt_ped_exec_aux_3.tta_cdn_estil_dwb          = 97.
    
    CREATE tt_ped_exec_param.
    ASSIGN tt_ped_exec_param.tta_num_seq                = 1
           tt_ped_exec_param.tta_cod_dwb_file           = tt-param.arq-destino
           tt_ped_exec_param.tta_cod_dwb_output         = "Arquivo".
  
    FIND FIRST tt-param NO-LOCK NO-ERROR.
  
    RAW-TRANSFER tt-param TO tt_ped_exec_param.tta_raw_param_ped_exec.

    RUN btb/btb912zc.p PERSISTENT SET h-api.
    
    RUN pi-execute-3 IN h-api (INPUT-OUTPUT TABLE tt_param_segur,
                               INPUT-OUTPUT TABLE tt_ped_exec_aux_3,
                               INPUT        TABLE tt_ped_exec_param,
                               INPUT        TABLE tt_ped_exec_param_aux,
                               INPUT        TABLE tt_ped_exec_sel,
                               OUTPUT       TABLE tt_erros_envio_email).
                               
    DELETE PROCEDURE h-api.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-efetiva-pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-efetiva-pedido Procedure 
PROCEDURE pi-efetiva-pedido :
/*** pi-Efetiva-Pedido
**** Efetiva o Pedido de Venda, ou seja, Completa
********************************************************************************/

    FIND FIRST tt-ped-venda.
    
    IF NOT VALID-HANDLE(bo-ped-venda-com) THEN
        RUN dibo/bodi159com.p PERSISTENT SET bo-ped-venda-com.
    
    RUN completeOrder IN bo-ped-venda-com(INPUT  tt-ped-venda.r-rowid,
                                          OUTPUT TABLE RowErrors).
    
    RUN destroyBO IN bo-ped-venda-com.
    DELETE PROCEDURE bo-ped-venda-com.
    bo-ped-venda-com = ?.    

    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.ErrorSubType = "ERROR":U ) THEN 
    DO:

        FOR EACH RowErrors 
           WHERE RowErrors.ErrorSubType = "ERROR":U:

            ASSIGN tt-log-ped-venda.importou = FALSE
                   tt-log-ped-venda.cd-erro  = RowErrors.errorNumber
                   tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + rowerrors.errordescription + "-" + STRING(rowerrors.errornumber) + "-" + rowerrors.errorHelp.
        END.

        RETURN "NOK":U.

    END.
    
    IF NOT CAN-FIND(FIRST RowErrors
                    WHERE RowErrors.ErrorSubType = "ERROR":U)  THEN
       ASSIGN tt-ped-venda.completo = YES.
    
    RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-gera-arquivo-resposta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-arquivo-resposta Procedure 
PROCEDURE pi-gera-arquivo-resposta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-contador AS INTEGER    NO-UNDO.
    DEFINE VARIABLE c-destino  AS CHARACTER  NO-UNDO.

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

    OUTPUT STREAM str-log TO VALUE(c-destino) NO-CONVERT.

    IF l-arquivo-valido THEN DO:
        FIND FIRST tt-log-ped-venda NO-LOCK NO-ERROR.
        if avail tt-log-ped-venda then do:
            FIND emitente NO-LOCK
            WHERE emitente.nome-abrev = tt-log-ped-venda.nome-abrev NO-ERROR.
    
            EXPORT STREAM str-log
                 DELIMITER ";" (IF AVAIL emitente THEN emitente.cod-emitente ELSE INTEGER(tt-arq_ped.c_arq_ped_cod-emitente) )
                               tt-log-ped-venda.nr-pedcli
                               tt-log-ped-venda.importou
                               REPLACE(REPLACE(tt-log-ped-venda.mensagem,CHR(13)," "),CHR(10),"").
            FOR EACH tt-log-ped-item:
                EXPORT STREAM str-log
                     DELIMITER ";" tt-log-ped-item.nr-sequencia
                                   tt-log-ped-item.it-codigo
                                   tt-log-ped-item.importou
                                   REPLACE(REPLACE(tt-log-ped-item.mensagem,CHR(13)," "),CHR(10),"").
            END.
         end.   

    END.
    ELSE DO:
        FIND FIRST tt-arq_ped NO-LOCK NO-ERROR.

        FIND emitente NO-LOCK 
        WHERE emitente.cod-emitente = INTEGER(tt-arq_ped.c_arq_ped_cod-emitente) NO-ERROR.

        CREATE tt-log-ped-venda.
        ASSIGN tt-log-ped-venda.nome-abrev = (IF AVAIL emitente THEN emitente.nome-abrev ELSE "")
               tt-log-ped-venda.nr-pedcli  = (IF AVAIL tt-arq_ped THEN tt-arq_ped.c_arq_ped_nr-pedcli    ELSE "")
               tt-log-ped-venda.importou   = NO
               tt-log-ped-venda.mensagem   = "ERRO: N∆o foi possivel importar o aquivo, pois o mesmo est† corrompido ***.".

        EXPORT STREAM str-log
             DELIMITER ";" (IF AVAIL tt-arq_ped THEN INTEGER(tt-arq_ped.c_arq_ped_cod-emitente) ELSE 0)
                           (IF AVAIL tt-arq_ped THEN tt-arq_ped.c_arq_ped_nr-pedcli    ELSE "")
                           NO
                           "ERRO: N∆o foi possivel importar o aquivo, pois o mesmo est† corrompido ***.".
    END.

    OUTPUT STREAM str-log CLOSE.

    RUN pi-imprime-log.

    OS-DELETE VALUE(c-dir-importacao + "/" + c-file) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-grava-log-peditem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-log-peditem Procedure 
PROCEDURE pi-grava-log-peditem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    ASSIGN tt-log-ped-item.cod-estabel     = tt-arq_ite.c_arq_ite_cod-estabel          
           tt-log-ped-item.tipo-transacao  = 1
           tt-log-ped-item.nat-operacao    = tt-arq_ite.c_arq_ite_nat-operacao
           tt-log-ped-item.qt-pedida       = DECIMAL(tt-arq_ite.c_arq_ite_qt-pedida)
           tt-log-ped-item.vl-preori       = DECIMAL(tt-arq_ite.c_arq_ite_vl-preori)
           tt-log-ped-item.vl-preuni       = DECIMAL(tt-arq_ite.c_arq_ite_vl-preori)
           tt-log-ped-item.per-des-item    = 0
           tt-log-ped-item.nr-tabpre       = tt-arq_ite.c_arq_ite_nr-tabpre
           tt-log-ped-item.tb-preco        = INTEGER(tt-arq_ite.c_arq_ite_tb-preco)
           tt-log-ped-item.vl-total        = DECIMAL(tt-arq_ite.c_arq_ite_vl-total)
           tt-log-ped-item.perc-icms       = DECIMAL(tt-arq_ite.c_arq_ite_perc-icms)
           tt-log-ped-item.obs-item-ped    = tt-arq_ite.c_arq_ite_obs-item-ped.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-grava-log-pedvenda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-log-pedvenda Procedure 
PROCEDURE pi-grava-log-pedvenda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN tt-log-ped-venda.cod-estabel     = tt-arq_ped.c_arq_ped_cod-estabel
           tt-log-ped-venda.emergencial      = INTEGER(tt-arq_ped.c_arq_ped_emergencial)
           tt-log-ped-venda.tipo-transacao  = 1                    
           tt-log-ped-venda.dt-implant      = TODAY
           tt-log-ped-venda.dt-entrega      = DATE(tt-arq_ped.c_arq_ped_dt-entrega)
           tt-log-ped-venda.nat-operacao    = tt-arq_ped.c_arq_ped_nat-operacao
           tt-log-ped-venda.cod-cond-pag    = INTEGER(tt-arq_ped.c_arq_ped_cod-cond-pag)
           tt-log-ped-venda.nr-tabpre       = tt-arq_ped.c_arq_ped_nr-tabpre
           tt-log-ped-venda.cod-priori      = INTEGER(tt-arq_ped.c_arq_ped_cod-priori)
           tt-log-ped-venda.perc-desco1     = 0                            
           tt-log-ped-venda.perc-desco2     = 0
           tt-log-ped-venda.observacoes     = tt-arq_ped.c_arq_ped_observacoes
           tt-log-ped-venda.nome-transp     = tt-arq_ped.c_arq_ped_nome-transp
           tt-log-ped-venda.tb-preco        = INTEGER(tt-arq_ped.c_arq_ped_tb-preco)
           tt-log-ped-venda.mo-codigo       = INTEGER(tt-arq_ped.c_arq_ped_mo-codigo)
           tt-log-ped-venda.no-ab-reppri    = tt-arq_ped.c_arq_ped_no-ab-reppri
           tt-log-ped-venda.vl-desconto     = DECIMAL(tt-arq_ped.c_arq_ped_vl-desconto)
           tt-log-ped-venda.nome-ab-rep     = tt-arq_ped.c_arq_ped_nome-ab-rep
           tt-log-ped-venda.cod-entrega     = tt-arq_ped.c_arq_ped_cod-entrega
           tt-log-ped-venda.tipo-fatur      = INTEGER(tt-arq_ped.c_arq_ped_tipo-fatur)
           tt-log-ped-venda.dt-min-fat      = DATE(tt-arq_ped.c_arq_ped_dt-min-fat)
           tt-log-ped-venda.transp-redesp   = tt-arq_ped.c_arq_ped_transp-redesp
           tt-log-ped-venda.nr-pedrep       = tt-arq_ped.c_arq_ped_nr-pedrep
           tt-log-ped-venda.cidade-cif      = tt-arq_ped.c_arq_ped_cidade-cif.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-imprime-log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-log Procedure 
PROCEDURE pi-imprime-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-diretorio     AS CHAR FORMAT "x(40)"              NO-UNDO.
    DEF VAR c-format    AS CHAR.
    DEF VAR i-len       AS INT.

    /*** Se nao Houver Nenhum Log **********************************/
    FIND FIRST tt-log-ped-venda NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-log-ped-venda THEN RETURN.

    /*** Gera os Logs **********************************************/
    /* 18/08/2015 */
    /*  + "\Pedido-Inc - " +  */
    ASSIGN c-diretorio = param-integra.dir-arq-err 
                  + "/Pedido-Inc - " + 
                                              STRING(DAY(TODAY),"99")    + 
                                              STRING(MONTH(TODAY),"99")  + 
                                              STRING(YEAR(TODAY),"9999") + ".txt".
    
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
            "Repres       Cod Entrega Tp Fatur Dt Min Fat Transp Redesp Nr PedRep    CIF/FOB" Skip
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
            SKIP.

        FOR EACH tt-log-ped-item 
           WHERE tt-log-ped-item.nome-abrev = tt-log-ped-venda.nome-abrev 
             AND tt-log-ped-item.nr-pedcli  = tt-log-ped-venda.nr-pedcli  
             NO-LOCK
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

&IF DEFINED(EXCLUDE-pi-validate-pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate-pedido Procedure 
PROCEDURE pi-validate-pedido :
/*****************************************************************************************/
    /*** Validacoes Necessarias **************************************************************/
    /*****************************************************************************************/
    DEFINE VARIABLE l-erros-item-ped AS LOGICAL    NO-UNDO.

    RUN pi-acompanhamento("Validando Cliente").
    FIND FIRST emitente 
         WHERE emitente.cod-emitente = INTEGER(tt-arq_ped.c_arq_ped_cod-emitente)
         NO-LOCK NO-ERROR.
    
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

    FIND FIRST ped-venda
         WHERE ped-venda.nome-abrev = emitente.nome-abrev
           AND ped-venda.nr-pedcli  = tt-arq_ped.c_arq_ped_nr-pedcli
           NO-LOCK NO-ERROR.

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
    FIND FIRST repres 
         WHERE repres.nome-abrev = tt-arq_ped.c_arq_ped_no-ab-reppri 
         No-lock No-error.

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
    assign c-nome-rep-direto = "".
    FIND FIRST repres
         WHERE repres.cod-rep = emitente.cod-rep
         NO-LOCK NO-ERROR.

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
    
    FIND FIRST estabelec 
         WHERE estabelec.cod-estabel = tt-arq_ped.c_arq_ped_cod-estabel
         NO-LOCK NO-ERROR.

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
    find first natur-oper where natur-oper.nat-operacao =  tt-arq_ped.c_arq_ped_nat-operacao no-lock no-error.
    if avail natur-oper then do:
       if natur-oper.transf = no then do:

            FIND FIRST cond-pagto
                 WHERE cond-pagto.cod-cond-pag = INTEGER(tt-arq_ped.c_arq_ped_cod-cond-pag)
                 NO-LOCK NO-ERROR.
        
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
        end.  

    end.    

    RUN pi-acompanhamento("Validando Transportadora").
    
    ASSIGN c-nome-redespacho = "".        
    FIND FIRST transporte
         WHERE transporte.nome-abrev = tt-arq_ped.c_arq_ped_transp-redesp
         NO-LOCK NO-ERROR.

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

    FIND FIRST transporte
         WHERE transporte.nome-abrev = tt-arq_ped.c_arq_ped_nome-transp
         NO-LOCK NO-ERROR.

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
    
    FIND FIRST natur-oper
         WHERE natur-oper.nat-operacao = tt-arq_ped.c_arq_ped_nat-operacao 
         NO-LOCK NO-ERROR.

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
        
    FIND FIRST tt-arq_ite
         WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
           AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli 
           NO-LOCK NO-ERROR.

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

    FOR EACH tt-arq_ite
         WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
           AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli NO-LOCK:

        FIND FIRST natur-oper
              WHERE natur-oper.nat-operacao = tt-arq_ite.c_arq_ite_nat-operacao 
              NO-LOCK NO-ERROR.
        
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
    FOR EACH tt-arq_ite 
       WHERE tt-arq_ite.c_arq_ped_cod-emitente = tt-arq_ped.c_arq_ped_cod-emitente
         AND tt-arq_ite.c_arq_ped_nr-pedcli    = tt-arq_ped.c_arq_ped_nr-pedcli 
         NO-LOCK :
       
       FIND FIRST ITEM
            WHERE ITEM.it-codigo = tt-arq_ite.c_arq_ite_it-codigo
            NO-LOCK NO-ERROR .

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

       FIND FIRST item-uni-estab
            WHERE item-uni-estab.it-codigo   = ITEM.it-codigo
              AND item-uni-estab.cod-estabel = tt-arq_ite.c_arq_ite_cod-estabel
              NO-LOCK NO-ERROR.
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
       
       /* --- validacoes de St - implementadas no dia 07/11/2014 por Israel Abrahao - 4Make --- */

       /* --- 1a validacao --- */
       IF  tt-arq_ite.c_arq_ite_ind-icm-ret <> "1"              AND 
          (DECIMAL(tt-arq_ite.c_arq_ite_per_st_icm)       <> ?  OR
           DECIMAL(tt-arq_ite.c_arq_ite_per_icm_estad_st) <> ?) THEN
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
          IF DECIMAL(tt-arq_ite.c_arq_ite_per_icm_estad_st) = 0 THEN
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
          IF DECIMAL(tt-arq_ite.c_arq_ite_per_icm_estad_st) <> ? AND
             DECIMAL(tt-arq_ite.c_arq_ite_per_st_icm)        = ? THEN
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
          IF DECIMAL(tt-arq_ite.c_arq_ite_per_icm_estad_st)  = ? AND
             DECIMAL(tt-arq_ite.c_arq_ite_per_st_icm)       <> ? THEN
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

PROCEDURE pi-processa-dados :

    DEFINE INPUT  PARAMETER hView AS HANDLE     NO-UNDO.
    define output parameter l-retorna-dados as log no-undo.
    DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hValue  AS HANDLE     NO-UNDO.

    CREATE X-NODEREF hValue.
    
    IF hView:NAME = "cod_estabel" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-estabel = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "cod_emitente" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-emitente = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "nr_pedcli" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nr-pedcli = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "emergencial" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_emergencial = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "dt_entrega" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_dt-entrega = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "cod_cond_pag" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-cond-pag = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "nr_tabpre" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nr-tabpre = hValue:NODE-VALUE no-error.
        IF tt-arq_ped.c_arq_ped_nr-tabpre = ? THEN
            ASSIGN tt-arq_ped.c_arq_ped_nr-tabpre = ''.
    END.

    IF hView:NAME = "cod_priori" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-priori = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "observacoes" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_observacoes = hValue:NODE-VALUE no-error.
        IF tt-arq_ped.c_arq_ped_observacoes = ? THEN
            ASSIGN tt-arq_ped.c_arq_ped_observacoes = ''.
    END.

    IF hView:NAME = "nome_transp" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nome-transp = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "tipo_preco" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_tb-preco = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "mo_codigo" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_mo-codigo = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "no_ab_reppri" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_no-ab-reppri = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "vl_desconto" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_vl-desconto = string(DECIMAL(REPLACE(hValue:NODE-VALUE  ,".",","))) no-error.
    END.

    IF hView:NAME = "nome_ab_rep" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nome-ab-rep = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "cod_entrega" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-entrega = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "tipo_fatur" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_tipo-fatur = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "nat_operacao" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nat-operacao = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "dt_min_fat" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_dt-min-fat = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "transp_redesp" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_transp-redesp = hValue:NODE-VALUE no-error.
        IF tt-arq_ped.c_arq_ped_transp-redesp = ?  THEN
            ASSIGN tt-arq_ped.c_arq_ped_transp-redesp = "".
    END.

    IF hView:NAME = "nr_pedrep" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nr-pedrep = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "cidade_cif" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cidade-cif = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "cod_canal_venda" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-canal-venda = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "e_mail" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_e-mail = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "nome-abrev-tri" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_nome-abrev-tri = hValue:NODE-VALUE no-error.
        IF tt-arq_ped.c_arq_ped_nome-abrev-tri  = ? THEN
            ASSIGN tt-arq_ped.c_arq_ped_nome-abrev-tri  = "".
    END.

    IF hView:NAME = "cod-entrega-tri" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_cod-entrega-tri = hValue:NODE-VALUE no-error.
        IF tt-arq_ped.c_arq_ped_cod-entrega-tri = ? THEN
            tt-arq_ped.c_arq_ped_cod-entrega-tri = "".
    END.

    IF hView:NAME = "BU" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_cod_unid_negocio = hValue:NODE-VALUE no-error.
    END.

    IF hView:NAME = "finalidade" THEN DO:
        hView:GET-CHILD(hValue,1) no-error.
        tt-arq_ped.c_arq_ped_dest_merc = hValue:NODE-VALUE no-error.
        
        assign l-retorna-dados = yes.
    END.
    

END PROCEDURE.

PROCEDURE pi-processa-itens :

   DEFINE INPUT  PARAMETER hXNODE AS HANDLE        NO-UNDO.
   define output parameter l-retorna-itens  as log no-undo.

   DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
   DEFINE VARIABLE j                AS INTEGER     NO-UNDO.
   DEFINE VARIABLE k                AS INTEGER     NO-UNDO.
   DEFINE VARIABLE m                 AS INT     NO-UNDO.
   DEFINE VARIABLE hView            AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hValue           AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hItem            AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hItens           AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hLotes           AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hViewlote        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hloteInterno     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hValuelote       AS HANDLE      NO-UNDO.

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
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_nr-sequencia = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "it_codigo" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_it-codigo  = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "qt_pedida" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_qt-pedida = STRING(DECIMAL(REPLACE(hValue:NODE-VALUE  ,".",","))) no-error.
               END.

               IF hView:NAME = "vl_preori" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_vl-preori = STRING(DECIMAL(REPLACE(hValue:NODE-VALUE  ,".",","))) no-error.
               END.

               IF hView:NAME = "nr_tabpre" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_nr-tabpre = hValue:NODE-VALUE no-error.
                   IF tt-arq_ite.c_arq_ite_nr-tabpre = ? THEN
                       ASSIGN tt-arq_ite.c_arq_ite_nr-tabpre =  ''.
               END.

               IF hView:NAME = "tp_preco" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_tb-preco = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "vl_total" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_vl-total = STRING(DECIMAL(REPLACE(hValue:NODE-VALUE  ,".",","))) no-error.
               END.

               IF hView:NAME = "perc_icms" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_perc-icms = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "complemento_descricao" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_obs-item-ped = hValue:NODE-VALUE no-error.
                   IF tt-arq_ite.c_arq_ite_obs-item-ped = ? THEN
                       ASSIGN tt-arq_ite.c_arq_ite_obs-item-ped = ''.
               END.

               IF hView:NAME = "nat_operacao" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_nat-operacao  = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "cod_estabel" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_cod-estabel = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "dt_entrega" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_dt-entrega  = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "ind-icm-ret" THEN DO:
                   hView:GET-CHILD(hValue,1) no-error.
                   ASSIGN tt-arq_ite.c_arq_ite_ind-icm-ret = hValue:NODE-VALUE no-error.
               END.

               IF hView:NAME = "minimo" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_arq_ite_lanc_minimo  = STRING(DECIMAL(REPLACE(hValue:NODE-VALUE  ,".",","))) no-error.
                END.

                IF hView:NAME = "obrigatorio" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_arq_ite_lanc_obrigato = STRING(DECIMAL(REPLACE(hValue:NODE-VALUE  ,".",","))) no-error.
                END.

                IF hView:NAME = "xml_xPed" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_arq_ite_ped_compr = hValue:NODE-VALUE no-error.
                    IF tt-arq_ite.c_arq_ite_ped_compr = ? THEN
                        ASSIGN tt-arq_ite.c_arq_ite_ped_compr = ''.
                END.

                IF hView:NAME = "xml_nItemPed" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_arq_ite_seq_ped_compr = hValue:NODE-VALUE no-error.
                END.

                IF hView:NAME = "BU" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_cod_unid_negocio = hValue:NODE-VALUE no-error.
                END.

                IF hView:NAME = "iva" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_arq_ite_per_st_icm = REPLACE(hValue:NODE-VALUE,".",",") no-error.
                END.

                IF hView:NAME = "icms_estadual_st" THEN DO:
                    hView:GET-CHILD(hValue,1) no-error.
                    ASSIGN tt-arq_ite.c_arq_ite_per_icm_estad_st = REPLACE(hValue:NODE-VALUE,".",",") no-error.
                    
                    l-retorna-itens = yes.
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
    
                            DO m = 1 TO (hLotes:NUM-CHILDREN) :
                                hLotes:GET-CHILD(hViewlote,m).
    
                                IF hViewlote:NAME = 'codigo' THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) no-error.
    
                                  FIND FIRST tt-arq_lote
                                        WHERE  tt-arq_lote.nome-abrev    = emitente.nome-abrev
                                          AND  tt-arq_lote.nr-pedcli     = tt-arq_ped.c_arq_ped_nr-pedcli
                                          AND  tt-arq_lote.nr-sequencia  = INTEGER(tt-arq_ite.c_arq_ite_nr-sequencia)
                                          AND  tt-arq_lote.it-codigo     = tt-arq_ite.c_arq_ite_it-codigo
                                          AND  tt-arq_lote.cod-refer     = ""
                                          AND  tt-arq_lote.codigo        = hValuelote:NODE-VALUE
                                       EXCLUSIVE-LOCK NO-ERROR.
                                    IF NOT AVAIL tt-arq_lote THEN DO:
                                        CREATE tt-arq_lote.
                                        ASSIGN tt-arq_lote.nome-abrev    = emitente.nome-abrev
                                               tt-arq_lote.nr-pedcli     = tt-arq_ped.c_arq_ped_nr-pedcli
                                               tt-arq_lote.nr-sequencia  = INTEGER(tt-arq_ite.c_arq_ite_nr-sequencia)
                                               tt-arq_lote.it-codigo     = tt-arq_ite.c_arq_ite_it-codigo
                                               tt-arq_lote.cod-refer     = ""
                                               tt-arq_lote.codigo        = hValuelote:NODE-VALUE no-error.
                                    END.
    
                                END.
                                
                                IF hViewlote:NAME = 'deposito' THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) no-error.
                                    ASSIGN tt-arq_lote.cod-depos = REPLACE(hValuelote:NODE-VALUE  ,".",",") no-error.
                                END.

                                IF hViewlote:NAME = 'localizacao' THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) no-error.
                                    ASSIGN tt-arq_lote.cod-localiz = REPLACE(hValuelote:NODE-VALUE  ,".",",") no-error.
                                END.
    
                                IF hViewlote:NAME = 'quantidade' THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) no-error.
                                    ASSIGN tt-arq_lote.quantidade = DECIMAL(REPLACE(hValuelote:NODE-VALUE  ,".",",")) no-error.
                                END.
    
                                IF hViewlote:NAME = 'validade' THEN DO:
                                    hViewlote:GET-CHILD(hValuelote,1) no-error.
                                    ASSIGN tt-arq_lote.dt-validade = date(hValuelote:NODE-VALUE) no-error.
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
