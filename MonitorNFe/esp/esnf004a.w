&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems2custom       PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenance


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-nfe003 NO-UNDO LIKE nfe003
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-nfe013 NO-UNDO LIKE nfe013
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-nfe013-trad NO-UNDO LIKE nfe013
       field r-rowid as rowid.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMaintenance 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF004A 2.00.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF004A
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1

&GLOBAL-DEFINE FolderLabels   Detalhes 1, Detalhes 2, Devolu‡Æo

&GLOBAL-DEFINE First          YES
&GLOBAL-DEFINE Prev           YES
&GLOBAL-DEFINE Next           YES
&GLOBAL-DEFINE Last           YES
&GLOBAL-DEFINE GoTo           NO
&GLOBAL-DEFINE Search         NO

&GLOBAL-DEFINE Add            no 
&GLOBAL-DEFINE Copy           no
&GLOBAL-DEFINE Update         NO
&GLOBAL-DEFINE Delete         no
&GLOBAL-DEFINE Undo           no
&GLOBAL-DEFINE Cancel         no
&GLOBAL-DEFINE Save           YES

&GLOBAL-DEFINE ExcludeBtQueryJoins      YES
&GLOBAL-DEFINE ExcludeBtReportsJoins    YES

&GLOBAL-DEFINE ttTable        tt-nfe013-trad
&GLOBAL-DEFINE hDBOTable      h-boes011 
&GLOBAL-DEFINE DBOTable       nfe013-trad

&GLOBAL-DEFINE ttTable2       tt-nfe013
&GLOBAL-DEFINE hDBOTable2     h-boes011-parent
&GLOBAL-DEFINE DBOTable       nfe013

&GLOBAL-DEFINE ttTable3       tt-nfe003
&GLOBAL-DEFINE hDBOTable3     h-boes003
&GLOBAL-DEFINE DBOTable       nfe003

&GLOBAL-DEFINE page0KeyFields 
&GLOBAL-DEFINE page0Fields   
&GLOBAL-DEFINE page1Fields    tt-nfe013.it-codigo  ~
                              tt-nfe013.seq-item   ~
                              tt-nfe013.cod-cfop   ~
                              tt-nfe013.num-pedido ~
                              tt-nfe013.numero-ordem ~
                              tt-nfe013.qtd-comercial ~
                              tt-nfe013-trad.un-comercial  ~
                              tt-nfe013.preco-unit    ~
                              tt-nfe013.preco-total   ~
                              tt-nfe013-trad.seq-item ~
                              tt-nfe013-trad.it-codigo ~
                              tt-nfe013-trad.nat-operacao ~
                              tt-nfe013-trad.num-pedido ~
                              tt-nfe013-trad.log-fifo-oc ~
                              tt-nfe013-trad.numero-ordem ~
                              tt-nfe013-trad.int-1 ~
                              tt-nfe013-trad.qtd-interna ~
                              tt-nfe013-trad.un-interna ~
                              tt-nfe013-trad.preco-unit  ~
                              tt-nfe013-trad.preco-total ~
                              tt-nfe013-trad.int-1 ~
                              fi-inf-compl ~
                              c-desc-item ~
                              c-desc-nat-oper ~
                              c-trad-1 ~
                              c-orig-1
&GLOBAL-DEFINE page2Fields    tt-nfe013.class-ipi ~
                              tt-nfe013.conta-contabil ~
                              tt-nfe013.nr-ord-produ ~
                              tt-nfe013.cod-refer ~
                              tt-nfe013.cod-depos ~
                              tt-nfe013.cod-localiz ~
                              tt-nfe013.lote-serie ~
                              tt-nfe013.dt-vali-lote ~
                              tt-nfe013-trad.class-fiscal ~
                              tt-nfe013-trad.conta-contabil ~
                              tt-nfe013-trad.centro-custo ~
                              c-desc-cc ~
                              tt-nfe013-trad.nr-ord-produ ~
                              tt-nfe013-trad.cod-refer ~
                              tt-nfe013-trad.cod-depos ~
                              tt-nfe013-trad.cod-localiz ~
                              tt-nfe013-trad.lote-serie ~
                              tt-nfe013-trad.dt-vali-lote ~
                              tt-nfe013-trad.cod-unid-negoc ~
                              c-desc-conta ~
                              c-desc-classif ~
                              c-desc-refer ~
                              c-desc-depos ~
                              c-des-unid-negoc ~
                              c-orig-2 ~
                              c-trad-2
&GLOBAL-DEFINE page3Fields    tt-nfe013-trad.serie-comp ~
                              tt-nfe013-trad.nro-comp ~
                              tt-nfe013-trad.seq-comp ~
                              tt-nfe013-trad.reabre-pd
/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER prTable         AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER prParent        AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller        AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER piSonPageNumber AS INTEGER   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}  AS HANDLE      NO-UNDO.
DEFINE VARIABLE {&hDBOTable2} AS HANDLE      NO-UNDO.
DEFINE VARIABLE {&hDBOTable3} AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boad049     AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin046     AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin084     AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin245     AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin377     AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin745     AS HANDLE      NO-UNDO.
DEFINE VARIABLE wh-pesquisa   AS HANDLE      NO-UNDO.
DEFINE VARIABLE l-enable      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-retorno     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cClassFiscal  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-formato-conta AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-conta-contabil-aux   AS CHARACTER   NO-UNDO.

DEF NEW GLOBAL SHARED VAR R_ROWID AS ROWID NO-UNDO.

def var l-aux as log init no no-undo.

DEFINE VARIABLE h_api_cta AS HANDLE      NO-UNDO.
def var i-empresa like param-global.empresa-prin no-undo.
def var p_cod_format_cta_ctbl AS CHAR no-undo.
def var v_cod_cta_ctbl AS CHAR no-undo.
def var v_des_cta_ctbl AS CHAR no-undo.
def var v_ind_finalid_cta AS CHAR no-undo.
def var v_num_tip_cta_ctbl AS INTEGER no-undo.
def var v_num_sit_cta_ctbl AS INTEGER no-undo.

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as integer format ">>>>,>>9" label "N?mero" column-label "N?mero"
    field ttv_des_msg_ajuda as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as character format "x(60)" label "Mensagem Erro" column-label "Inconsistªncia".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Maintenance
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btFirst btPrev btNext btLast ~
btSave btExit btHelp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 tt-nfe013-trad.serie-comp tt-nfe013-trad.nro-comp ~
tt-nfe013-trad.seq-comp 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMaintenance AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miFirst        LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM miPrev         LABEL "&Anterior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM miNext         LABEL "&Pr¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM miLast         LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       RULE
       MENU-ITEM miGoTo         LABEL "&V  Para"       ACCELERATOR "CTRL-T"
       MENU-ITEM miSearch       LABEL "&Pesquisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM miAdd          LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM miCopy         LABEL "&Copiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM miUpdate       LABEL "&Alterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM miDelete       LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM miUndo         LABEL "&Desfazer"      ACCELERATOR "CTRL-U"
       MENU-ITEM miCancel       LABEL "&Cancelar"      ACCELERATOR "CTRL-F4"
       RULE
       MENU-ITEM miSave         LABEL "&Salvar"        ACCELERATOR "CTRL-S"
       RULE
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       RULE
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "image\im-fir":U
     IMAGE-INSENSITIVE FILE "image\ii-fir":U
     LABEL "First":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btLast 
     IMAGE-UP FILE "image\im-las":U
     IMAGE-INSENSITIVE FILE "image\ii-las":U
     LABEL "Last":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btNext 
     IMAGE-UP FILE "image\im-nex":U
     IMAGE-INSENSITIVE FILE "image\ii-nex":U
     LABEL "Next":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btPrev 
     IMAGE-UP FILE "image\im-pre":U
     IMAGE-INSENSITIVE FILE "image\ii-pre":U
     LABEL "Prev":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btSave 
     IMAGE-UP FILE "image\im-sav":U
     IMAGE-INSENSITIVE FILE "image\ii-sav":U
     LABEL "Save" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 103 BY 1.5
     BGCOLOR 7 .

DEFINE VARIABLE c-desc-item AS CHARACTER FORMAT "x(60)" 
     LABEL "Descri‡Æo":R11 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-nat-oper AS CHARACTER FORMAT "x(35)" 
     VIEW-AS FILL-IN 
     SIZE 21.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-orig-1 AS CHARACTER FORMAT "X(20)":U INITIAL "Dados Originais" 
      VIEW-AS TEXT 
     SIZE 12 BY .67 NO-UNDO.

DEFINE VARIABLE c-trad-1 AS CHARACTER FORMAT "X(20)":U INITIAL "Dados Traduzidos" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE fi-inf-compl AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inf Compl It" 
     VIEW-AS FILL-IN 
     SIZE 36.29 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 12.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 12.

DEFINE VARIABLE c-des-unid-negoc AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 32.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-cc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.86 BY .79 NO-UNDO.

DEFINE VARIABLE c-desc-classif AS CHARACTER FORMAT "x(35)" 
     VIEW-AS FILL-IN 
     SIZE 32.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-conta AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 24.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-depos AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 37.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-refer AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 32.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-orig-2 AS CHARACTER FORMAT "X(20)":U INITIAL "Dados Originais" 
      VIEW-AS TEXT 
     SIZE 12 BY .67 NO-UNDO.

DEFINE VARIABLE c-trad-2 AS CHARACTER FORMAT "X(20)":U INITIAL "Dados Traduzidos" 
      VIEW-AS TEXT 
     SIZE 15 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 10.25.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 10.25.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 4.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btFirst AT ROW 1.13 COL 1.57 HELP
          "Primeira ocorrˆncia"
     btPrev AT ROW 1.13 COL 5.57 HELP
          "Ocorrˆncia anterior"
     btNext AT ROW 1.13 COL 9.57 HELP
          "Pr¢xima ocorrˆncia"
     btLast AT ROW 1.13 COL 13.57 HELP
          "éltima ocorrˆncia"
     btSave AT ROW 1.13 COL 30.86 HELP
          "Confirma altera‡äes"
     btExit AT ROW 1.13 COL 95.57 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 99.57 HELP
          "Ajuda"
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.57 BY 17
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage2
     tt-nfe013.class-ipi AT ROW 2.25 COL 15 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-nfe013-trad.class-fiscal AT ROW 2.25 COL 56.14 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     c-desc-classif AT ROW 2.25 COL 66.72 COLON-ALIGNED HELP
          "Descri‡Æo" NO-LABEL WIDGET-ID 42
     tt-nfe013.conta-contabil AT ROW 3.25 COL 15 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 20 BY .88
     tt-nfe013-trad.conta-contabil AT ROW 3.25 COL 56.14 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 18 BY .88
          BGCOLOR 14 
     c-desc-conta AT ROW 3.25 COL 74.57 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     tt-nfe013-trad.centro-custo AT ROW 4.25 COL 56 COLON-ALIGNED HELP
          "Centro Custo" WIDGET-ID 54 VIEW-AS FILL-IN SIZE 20 BY .88
     c-desc-cc AT ROW 4.25 COL 76.14 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     tt-nfe013.nr-ord-produ AT ROW 5.25 COL 15 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     tt-nfe013-trad.nr-ord-produ AT ROW 5.25 COL 56.14 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
          BGCOLOR 14 
     tt-nfe013.cod-refer AT ROW 6.25 COL 15 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-nfe013-trad.cod-refer AT ROW 6.25 COL 56.14 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     c-desc-refer AT ROW 6.25 COL 66.72 COLON-ALIGNED HELP
          "Descri‡Æo da referˆncia" NO-LABEL WIDGET-ID 44
     tt-nfe013.cod-depos AT ROW 7.25 COL 15 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-nfe013-trad.cod-depos AT ROW 7.25 COL 56.14 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
          BGCOLOR 14 
     c-desc-depos AT ROW 7.25 COL 61.72 COLON-ALIGNED HELP
          "Descri‡Æo do Dep¢sito" NO-LABEL WIDGET-ID 46
     tt-nfe013.cod-localiz AT ROW 8.25 COL 15 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     tt-nfe013-trad.cod-localiz AT ROW 8.25 COL 56.14 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
          BGCOLOR 14 
     tt-nfe013.lote-serie AT ROW 9.25 COL 15 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     tt-nfe013-trad.lote-serie AT ROW 9.25 COL 56.14 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
          BGCOLOR 14 
     tt-nfe013.dt-vali-lote AT ROW 10.25 COL 15 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-nfe013-trad.dt-vali-lote AT ROW 10.25 COL 56.14 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.cod-unid-negoc AT ROW 11.25 COL 56.14 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     c-des-unid-negoc AT ROW 11.25 COL 66.72 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     c-orig-2 AT ROW 1.25 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-trad-2 AT ROW 1.25 COL 43.43 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     RECT-28 AT ROW 1.5 COL 2 WIDGET-ID 2
     RECT-29 AT ROW 1.5 COL 43.14 WIDGET-ID 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.86 ROW 3.83
         SIZE 102 BY 12.67
         FONT 1 WIDGET-ID 300.

DEFINE FRAME fPage3
     tt-nfe013-trad.serie-comp AT ROW 2.88 COL 21 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.nro-comp AT ROW 3.88 COL 21 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.seq-comp AT ROW 4.83 COL 21 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 7.57 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.reabre-pd AT ROW 5.83 COL 23.14 WIDGET-ID 14
          VIEW-AS TOGGLE-BOX
          SIZE 12.86 BY .71
     "Devolu‡Æo" VIEW-AS TEXT
          SIZE 8.57 BY .67 AT ROW 1.75 COL 8.43 WIDGET-ID 12
     rt-key AT ROW 2.08 COL 6 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.86 ROW 3.83
         SIZE 102 BY 12.5
         FONT 1 WIDGET-ID 400.

DEFINE FRAME fPage1
     tt-nfe013.seq-item AT ROW 2.25 COL 15 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-nfe013-trad.seq-item AT ROW 2.25 COL 64 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-nfe013.it-codigo AT ROW 3.25 COL 15 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 30 BY .88
     tt-nfe013-trad.it-codigo AT ROW 3.25 COL 64 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 30 BY .88
          BGCOLOR 14 
     c-desc-item AT ROW 4.25 COL 64 COLON-ALIGNED WIDGET-ID 50
     tt-nfe013.cod-cfop AT ROW 5.25 COL 15 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     tt-nfe013-trad.nat-operacao AT ROW 5.25 COL 64 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
          BGCOLOR 14 
     c-desc-nat-oper AT ROW 5.25 COL 72.57 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     tt-nfe013.num-pedido AT ROW 6.25 COL 15 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-nfe013-trad.num-pedido AT ROW 6.25 COL 64 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.log-fifo-oc AT ROW 6.25 COL 79.86 WIDGET-ID 64
          VIEW-AS TOGGLE-BOX
          SIZE 17.29 BY .88
     tt-nfe013.numero-ordem AT ROW 7.25 COL 15 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-nfe013-trad.numero-ordem AT ROW 7.25 COL 64 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.int-1 AT ROW 7.25 COL 83.86 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
          BGCOLOR 14 
     tt-nfe013.qtd-comercial AT ROW 8.25 COL 15 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     tt-nfe013-trad.qtd-interna AT ROW 8.25 COL 64 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
          BGCOLOR 14 
     tt-nfe013-trad.un-comercial AT ROW 9.25 COL 15 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     tt-nfe013-trad.un-interna AT ROW 9.25 COL 64 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
          BGCOLOR 14 
     tt-nfe013.preco-unit AT ROW 10.25 COL 15 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
     tt-nfe013-trad.preco-unit AT ROW 10.25 COL 64 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
          BGCOLOR 14 
     tt-nfe013.preco-total AT ROW 11.25 COL 15 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
     tt-nfe013-trad.preco-total AT ROW 11.25 COL 64 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
          BGCOLOR 14 
     fi-inf-compl AT ROW 12.33 COL 58.57 COLON-ALIGNED WIDGET-ID 70
     c-orig-1 AT ROW 1.25 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     c-trad-1 AT ROW 1.25 COL 51.29 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     RECT-16 AT ROW 1.5 COL 2 WIDGET-ID 26
     RECT-17 AT ROW 1.5 COL 50.29 WIDGET-ID 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.86 ROW 3.83
         SIZE 102 BY 12.75
         FONT 1 WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Maintenance
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-nfe003 T "?" NO-UNDO ems2custom nfe003
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-nfe013 T "?" NO-UNDO ems2custom nfe013
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-nfe013-trad T "?" NO-UNDO ems2custom nfe013
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMaintenance ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 103.57
         MAX-HEIGHT         = 26.63
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 26.63
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wMaintenance 
/* ************************* Included-Libraries *********************** */

{maintenance/maintenance.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wMaintenance
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE
       FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage3:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* SETTINGS FOR FILL-IN c-desc-nat-oper IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-trad-1 IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.cod-cfop IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.int-1 IN FRAME fPage1
   NO-ENABLE                                                            */
ASSIGN 
       tt-nfe013-trad.int-1:HIDDEN IN FRAME fPage1           = TRUE.

/* SETTINGS FOR FILL-IN tt-nfe013.it-codigo IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.num-pedido IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.numero-ordem IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.preco-total IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.preco-total IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.qtd-comercial IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.seq-item IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.seq-item IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.un-comercial IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.un-interna IN FRAME fPage1
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-des-unid-negoc IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-classif IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-conta IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-depos IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-refer IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.class-ipi IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.cod-depos IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.cod-localiz IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.cod-refer IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.conta-contabil IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.dt-vali-lote IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.lote-serie IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe013.nr-ord-produ IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.nro-comp IN FRAME fPage3
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.seq-comp IN FRAME fPage3
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN tt-nfe013-trad.serie-comp IN FRAME fPage3
   NO-ENABLE 2                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMaintenance)
THEN wMaintenance:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage3
/* Query rebuild information for FRAME fPage3
     _Query            is NOT OPENED
*/  /* FRAME fPage3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wMaintenance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMaintenance wMaintenance
ON END-ERROR OF wMaintenance
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMaintenance wMaintenance
ON WINDOW-CLOSE OF wMaintenance
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wMaintenance
ON CHOOSE OF btExit IN FRAME fpage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst wMaintenance
ON CHOOSE OF btFirst IN FRAME fpage0 /* First */
OR CHOOSE OF MENU-ITEM miFirst IN MENU mbMain DO:

    /*RUN repositionRecord IN {&hDBOTable} (INPUT rowid({&ttTable})).*/

    RUN getFirst     IN THIS-PROCEDURE.
    ASSIGN btSave:SENSITIVE IN FRAME fPage0 = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wMaintenance
ON CHOOSE OF btHelp IN FRAME fpage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast wMaintenance
ON CHOOSE OF btLast IN FRAME fpage0 /* Last */
OR CHOOSE OF MENU-ITEM miLast IN MENU mbMain DO:

    /*RUN repositionRecord IN {&hDBOTable} (INPUT rowid({&ttTable})).*/

    RUN getLast      IN THIS-PROCEDURE.
    ASSIGN btSave:SENSITIVE IN FRAME fPage0 = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext wMaintenance
ON CHOOSE OF btNext IN FRAME fpage0 /* Next */
OR CHOOSE OF MENU-ITEM miNext IN MENU mbMain DO:

    /*RUN repositionRecord IN {&hDBOTable} (INPUT rowid({&ttTable})).*/

    RUN getNext      IN THIS-PROCEDURE.
    
    ASSIGN btSave:SENSITIVE IN FRAME fPage0 = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrev wMaintenance
ON CHOOSE OF btPrev IN FRAME fpage0 /* Prev */
OR CHOOSE OF MENU-ITEM miPrev IN MENU mbMain DO:
    
    /*RUN repositionRecord IN {&hDBOTable} (INPUT rowid({&ttTable})).*/

    RUN getPrev      IN THIS-PROCEDURE.
    ASSIGN btSave:SENSITIVE IN FRAME fPage0 = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wMaintenance
ON CHOOSE OF btSave IN FRAME fpage0 /* Save */
DO:
  FIND nfe013
      WHERE nfe013.ch-acesso-comp-nfe = tt-nfe013-trad.ch-acesso-comp-nfe
        and nfe013.idi-orig-trad      = tt-nfe013-trad.idi-orig-trad
        and nfe013.seq-item           = tt-nfe013-trad.seq-item EXCLUSIVE-LOCK NO-ERROR.

  ASSIGN nfe013.it-codigo      = string(tt-nfe013-trad.it-codigo       :screen-value in FRAME fPage1)
         nfe013.nat-operacao   = string(tt-nfe013-trad.nat-operacao    :screen-value in frame fPage1)
         nfe013.num-pedido     = INT(tt-nfe013-trad.num-pedido      :screen-value in frame fPage1)
         nfe013.numero-ordem   = INT(tt-nfe013-trad.numero-ordem    :screen-value in frame fPage1)
         nfe013.qtd-interna    = DEC(tt-nfe013-trad.qtd-interna     :screen-value in frame fPage1)
         nfe013.un-interna     = string(tt-nfe013-trad.un-interna      :screen-value in frame fPage1)
         nfe013.preco-unit     = DEC(tt-nfe013-trad.preco-unit      :screen-value in frame fPage1)
         nfe013.preco-total    = DEC(tt-nfe013-trad.preco-total     :screen-value in frame fPage1)
         nfe013.int-1          = INT(tt-nfe013-trad.int-1           :screen-value in frame fPage1)
         nfe013.class-fiscal   = string(tt-nfe013-trad.class-fiscal    :screen-value in FRAME fPage2)
         nfe013.conta-contabil = string(tt-nfe013-trad.conta-contabil  :screen-value in frame fPage2)
         nfe013.centro-custo   = string(tt-nfe013-trad.centro-custo  :screen-value in frame fPage2)
         nfe013.nr-ord-produ   = INT(tt-nfe013-trad.nr-ord-produ    :screen-value in frame fPage2)
         nfe013.cod-refer      = string(tt-nfe013-trad.cod-refer       :screen-value in frame fPage2)
         nfe013.cod-depos      = string(tt-nfe013-trad.cod-depos       :screen-value in frame fPage2)
         nfe013.cod-localiz    = string(tt-nfe013-trad.cod-localiz     :screen-value in frame fPage2)
         nfe013.lote-serie     = string(tt-nfe013-trad.lote-serie      :screen-value in frame fPage2)
         nfe013.dt-vali-lote   = DATE(tt-nfe013-trad.dt-vali-lote    :screen-value in frame fPage2)
         nfe013.cod-unid-negoc = string(tt-nfe013-trad.cod-unid-negoc  :screen-value in FRAME fPage2)
         nfe013.serie-comp     = string(tt-nfe013-trad.serie-comp      :screen-value in frame fPage3)
         nfe013.nro-comp       = string(tt-nfe013-trad.nro-comp        :screen-value in frame fPage3)
         nfe013.seq-comp       = INT(tt-nfe013-trad.seq-comp        :screen-value in frame fPage3).

  IF tt-nfe013-trad.reabre-pd:SCREEN-VALUE IN FRAME fPage3 = "YES" THEN
      ASSIGN nfe013.reabre-pd = YES.
  ELSE
      ASSIGN nfe013.reabre-pd = NO.

  IF tt-nfe013-trad.log-fifo-oc:SCREEN-VALUE IN FRAME fPage1 = "yes" THEN
      ASSIGN nfe013.log-fifo-oc = YES.
  ELSE
      ASSIGN nfe013.log-fifo-oc = NO.

  RETURN "OK".
        
/*     RUN saveRecord IN THIS-PROCEDURE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME tt-nfe013-trad.class-fiscal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.class-fiscal wMaintenance
ON ENTRY OF tt-nfe013-trad.class-fiscal IN FRAME fPage2 /* Classifica‡Æo Fiscal */
DO:
    
    ASSIGN tt-nfe013-trad.class-fiscal:FORMAT IN FRAME fPage2 = "9999.99.99":U. 
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.class-fiscal wMaintenance
ON F5 OF tt-nfe013-trad.class-fiscal IN FRAME fPage2 /* Classifica‡Æo Fiscal */
DO:
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=inzoom/z01in046.w
                       &campo=tt-nfe013-trad.class-fiscal
                       &campozoom=class-fiscal
                       &campo2=c-desc-classif
                       &campozoom2=descricao
                       &frame=fPage2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.class-fiscal wMaintenance
ON LEAVE OF tt-nfe013-trad.class-fiscal IN FRAME fPage2 /* Classifica‡Æo Fiscal */
DO:
    ASSIGN cClassFiscal = INPUT FRAME fPage2 tt-nfe013-trad.class-fiscal NO-ERROR.
    IF  ERROR-STATUS:ERROR THEN
        ASSIGN tt-nfe013-trad.class-fiscal:format in frame fPage2 = "x(10)":U no-error.

    ASSIGN tt-nfe013-trad.class-fiscal = REPLACE (tt-nfe013-trad.class-fiscal:SCREEN-VALUE, ".", "").

    RUN findClass_fiscal IN h-boin046 (INPUT  INPUT FRAME fPage2 tt-nfe013-trad.class-fiscal,
                                       OUTPUT c-retorno).

    IF c-retorno = "" THEN
        RUN getCharField IN h-boin046 (INPUT "descricao", OUTPUT c-desc-classif).
    ELSE
        ASSIGN c-desc-classif = ?.


    DISP c-desc-classif
        WITH FRAME fPage2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.class-fiscal wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.class-fiscal IN FRAME fPage2 /* Classifica‡Æo Fiscal */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.cod-depos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-depos wMaintenance
ON F5 OF tt-nfe013-trad.cod-depos IN FRAME fPage2 /* Dep¢sito */
DO:
    /*--- ZOOM SMART OBJECT ---*/
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom="inzoom/z01in084.w"
                       &campo=tt-nfe013-trad.cod-depos
                       &campozoom=cod-depos
                       &campo2=c-desc-depos
                       &campozoom2=nome
                       &frame=fPage2 }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-depos wMaintenance
ON LEAVE OF tt-nfe013-trad.cod-depos IN FRAME fPage2 /* Dep¢sito */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boin084"
                              &KeyValue1="tt-nfe013-trad.cod-depos:SCREEN-VALUE IN FRAME fPage2"
                              &FieldName1="nome"
                              &FieldScreen1="c-desc-depos"
                              &Frame1="fPage2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-depos wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.cod-depos IN FRAME fPage2 /* Dep¢sito */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.cod-localiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-localiz wMaintenance
ON F5 OF tt-nfe013-trad.cod-localiz IN FRAME fPage2 /* Localiza‡Æo */
DO:
    /* ZOOM SMART OBJECT */
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom="inzoom/z02in189.w"
                       &campo=tt-nfe013-trad.cod-localiz
                       &campozoom=cod-localiz
                       &frame=fPage2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-localiz wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.cod-localiz IN FRAME fPage2 /* Localiza‡Æo */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-refer wMaintenance
ON F5 OF tt-nfe013-trad.cod-refer IN FRAME fPage2 /* Referˆncia */
DO:
    /*--- ZOOM SMART OBJECT ---*/
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=inzoom/z01in375.w
                       &campo=tt-nfe013-trad.cod-refer
                       &campozoom=cod-refer
                       &campo2=c-desc-refer
                       &campozoom2=descricao
                       &frame=fPage2
                       &parametros="run pi-seta-inicial in wh-pesquisa (input tt-nfe013-trad.it-codigo:screen-value in frame fPage1)."} 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-refer wMaintenance
ON LEAVE OF tt-nfe013-trad.cod-refer IN FRAME fPage2 /* Referˆncia */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boin377"
                              &KeyValue1="tt-nfe013-trad.cod-refer:SCREEN-VALUE IN FRAME fPage2"
                              &FieldName1="descricao"
                              &FieldScreen1="c-desc-refer"
                              &Frame1="fPage2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-refer wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.cod-refer IN FRAME fPage2 /* Referˆncia */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.cod-unid-negoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-unid-negoc wMaintenance
ON F5 OF tt-nfe013-trad.cod-unid-negoc IN FRAME fPage2 /* Unidade Neg¢cio */
DO:
    {method/zoomfields.i &ProgramZoom="inzoom/z01in745.w"
                         &FieldZoom1="cod-unid-negoc"
                         &FieldScreen1="tt-nfe013-trad.cod-unid-negoc"
                         &Frame1="fPage2"
                         &FieldZoom2="des-unid-negoc"
                         &FieldScreen2="c-des-unid-negoc"
                         &Frame2="fPage2"
                         &EnableImplant="NO"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-unid-negoc wMaintenance
ON LEAVE OF tt-nfe013-trad.cod-unid-negoc IN FRAME fPage2 /* Unidade Neg¢cio */
DO:
    {method/referencefields.i &HandleDBOLeave="h-boin745"
                              &KeyValue1="tt-nfe013-trad.cod-unid-negoc:SCREEN-VALUE IN FRAME fPage2"
                              &FieldName1="des-unid-negoc"
                              &FieldScreen1="c-des-unid-negoc"
                              &Frame1="fPage2"}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.cod-unid-negoc wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.cod-unid-negoc IN FRAME fPage2 /* Unidade Neg¢cio */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.conta-contabil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.conta-contabil wMaintenance
ON ENTRY OF tt-nfe013-trad.conta-contabil IN FRAME fPage2 /* Conta Cont bil */
DO:
    
    /*ASSIGN tt-nfe013-trad.conta-contabil:FORMAT IN FRAME fPage2 = c-formato-conta.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.conta-contabil wMaintenance
ON F5 OF tt-nfe013-trad.conta-contabil IN FRAME fPage2 /* Conta Cont bil */
DO:
    FIND FIRST estabelec 
        WHERE estabelec.cod-estabel = param-estoq.estabel-pad NO-LOCK NO-ERROR.
    ASSIGN i-empresa = IF AVAIL estabelec THEN estabelec.ep-codigo ELSE "".
    
  /*run prgint/utb/utb743za.py persistent set h_api_cta.
  run pi_zoom_cta_ctbl_integr in h_api_cta (input  i-empresa,                 /* EMPRESA EMS2 */
                                                  input  "CEP",              /* M…DULO */         
                                                  input  "",                 /* PLANO DE CONTAS */
                                                  input  "(nenhum)",         /* FINALIDADES */    
                                                  input  today,              /* DATA TRANSACAO */
                                                  output v_cod_cta_ctbl,     /* CODIGO CONTA */   
                                                  output v_des_cta_ctbl,     /* DESCRICAO CONTA */
                                                  output v_ind_finalid_cta,  /* FINALIDADE DA CONTA */
                                                  output table tt_log_erro). /* ERROS */ 

  IF RETURN-VALUE = "Ok" THEN DO:
       if p_cod_format_cta_ctbl = "" then
           run pi_retorna_formato_cta_ctbl in h_api_cta (input  i-empresa,                 /* EMPRESA EMS 2 */
                                                      input  "",                 /* PLANO CONTAS */
                                                      input  TODAY, /* DATA DE TRANSACAO */
                                                      output p_cod_format_cta_ctbl,   /* FORMATO cta */
                                                      output table tt_log_erro). /* ERROS */
    
       assign tt-nfe013-trad.conta-contabil:SCREEN-VALUE IN FRAME {&frame-name} = v_cod_cta_ctbl /*, p_cod_format_cta_ctbl)*/
              c-desc-conta:SCREEN-VALUE IN FRAME {&frame-name} = v_des_cta_ctbl.
  END.
   DELETE OBJECT h_api_cta.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.conta-contabil wMaintenance
ON LEAVE OF tt-nfe013-trad.conta-contabil IN FRAME fPage2 /* Conta Cont bil */
DO:
    ASSIGN tt-nfe013-trad.conta-contabil:FORMAT IN FRAME {&frame-name} = "x(20)"
           tt-nfe013-trad.conta-contabil = REPLACE (tt-nfe013-trad.conta-contabil:SCREEN-VALUE, ".", "").
   
    /*IF tt-nfe013-trad.conta-contabil:SCREEN-VALUE <> "" THEN DO:
        run prgint/utb/utb743za.py persistent set h_api_cta.
        RUN pi_busca_dados_cta_ctbl IN h_api_cta (INPUT        i-empresa,               /* EMPRESA EMS2 */
                                                  INPUT        "",                      /* PLANO DE CONTAS */
                                                  INPUT-OUTPUT tt-nfe013-trad.conta-contabil,                 /* CONTA */
                                                  INPUT        today,                   /* DATA TRANSACAO */   
                                                  OUTPUT       v_des_cta_ctbl,          /* DESCRICAO CONTA */
                                                  OUTPUT       v_num_tip_cta_ctbl,      /* TIPO DA CONTA */
                                                  OUTPUT       v_num_sit_cta_ctbl,      /* SITUA°€O DA CONTA */
                                                  OUTPUT       v_ind_finalid_cta,       /* FINALIDADES DA CONTA */
                                                  OUTPUT TABLE tt_log_erro).            /* ERROS */    
                                                  
        IF RETURN-VALUE = "OK" THEN DO:
            ASSIGN tt-nfe013-trad.conta-contabil:SCREEN-VALUE = tt-nfe013-trad.conta-contabil
                   tt-nfe013-trad.conta-contabil:FORMAT IN FRAME {&frame-name} = p_cod_format_cta_ctbl
                   c-desc-conta:SCREEN-VALUE IN FRAME {&frame-name} = v_des_cta_ctbl.                   
        END.
        ELSE do:
            ASSIGN tt-nfe013-trad.conta-contabil:FORMAT IN FRAME {&frame-name} = p_cod_format_cta_ctbl
                   c-desc-conta:SCREEN-VALUE IN FRAME {&frame-name}= "".
        end.
        DELETE OBJECT h_api_cta.
    END.
    ELSE do: 
        ASSIGN tt-nfe013-trad.conta-contabil:FORMAT IN FRAME {&frame-name} = "x(20)"
               c-desc-conta:SCREEN-VALUE IN FRAME {&frame-name}= "".
    end.*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.conta-contabil wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.conta-contabil IN FRAME fPage2 /* Conta Cont bil */
DO:
    APPLY "F5":U TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.centro-custo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.centro-custo wMaintenance
ON F5 OF tt-nfe013-trad.centro-custo IN FRAME fPage2 /* Centro Custo */
DO:
    DEF VAR  h_api_ccusto AS HANDLE NO-UNDO.
    DEF VAR v_cod_ccusto  AS CHAR NO-UNDO.
    DEF VAR v_des_ccusto  AS CHAR NO-UNDO.
    DEF VAR v_cod_formato AS CHAR NO-UNDO.
    run prgint/utb/utb742za.py persistent set h_api_ccusto.

    run pi_zoom_ccusto_x_cta_ctbl in h_api_ccusto (input  string(i-empresa),  /* EMPRESA EMS2 */
                                                   input  "",                 /* ESTABELECIMENTO */
                                                   input  "YES",              /* CONSIDERA TODOS OS ESTABELECIMENTOS ?*/
                                                   input  "",                 /* UNIDADE DE NEGOCIO */
                                                   input  "",                 /* PLANO DE CONTAS */
                                                   input  replace(tt-nfe013-trad.conta-contabil:SCREEN-VALUE IN FRAME fPage2, ".", ""),     /* C…DIGO DA CONTA */
                                                   input  "",                 /* CODIGO DO PLANO CCUSTO */
                                                   input  tt-nfe003.dt-trans, /* DATA DE TRANSACAO */
                                                   output v_cod_ccusto,       /* CODIGO CCUSTO */
                                                   output v_des_ccusto,       /* DESCRICAO CCUSTO */
                                                   output table tt_log_erro). /* ERROS */ 

    IF RETURN-VALUE = "OK" THEN DO:
        run pi_retorna_formato_ccusto in h_api_ccusto (input  i-empresa,          /* EMPRESA EMS2 */
                                                       input  "",                 /* PLANO CONTAS */
                                                       input  tt-nfe003.dt-trans, /* DATA DE TRANSACAO */ 
                                                       output v_cod_formato,      /* FORMATO CONTA */
                                                       output table tt_log_erro). /* ERROS */

        IF v_cod_ccusto <> "" THEN
            ASSIGN tt-nfe013-trad.centro-custo:SCREEN-VALUE IN FRAME fPage2 = string(v_cod_ccusto, v_cod_formato)
                   c-desc-cc:SCREEN-VALUE IN FRAME fPage2 = v_des_ccusto.  
        ELSE
            ASSIGN tt-nfe013-trad.centro-custo:FORMAT IN FRAME fPage2 = "x(20)"
                   c-desc-cc:SCREEN-VALUE IN FRAME fPage2 = "".   
    END.
    ELSE do:
        ASSIGN tt-nfe013-trad.centro-custo:FORMAT IN FRAME fPage2 = "x(20)"
               c-desc-cc:SCREEN-VALUE IN FRAME fPage2 = "".                   
    end.

    delete object h_api_ccusto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.centro-custo wMaintenance
ON LEAVE OF tt-nfe013-trad.centro-custo IN FRAME fPage2 /* Centro Custo */
DO:
    DEFINE VARIABLE h_api_ccusto  AS HANDLE      NO-UNDO.
    DEFINE VARIABLE v_des_ccusto  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE v_cod_formato AS CHARACTER   NO-UNDO.
    run prgint/utb/utb742za.py persistent set h_api_ccusto.

    run pi_busca_dados_ccusto in h_api_ccusto (input  STRING(i-empresa),  /* EMPRESA EMS2 */
                                               input  "",                 /* CODIGO DO PLANO CCUSTO */
                                               input  tt-nfe013-trad.centro-custo:SCREEN-VALUE IN FRAME fPage2 , /* CCUSTO */
                                               input  tt-nfe003.dt-trans, /* DATA DE TRANSACAO */
                                               output v_des_ccusto,       /* DESCRICAO DO CCUSTO */
                                               output table tt_log_erro). /* ERROS */

    IF  RETURN-VALUE = "OK":U then do:
        run pi_retorna_formato_ccusto in h_api_ccusto (input  i-empresa,          /* EMPRESA EMS2 */
                                                       input  "",                 /* PLANO CONTAS */
                                                       input  tt-nfe003.dt-trans, /* DATA DE TRANSACAO */ 
                                                       output v_cod_formato,      /* FORMATO CONTA */
                                                       output table tt_log_erro). /* ERROS */

        IF tt-nfe013-trad.centro-custo:SCREEN-VALUE IN FRAME fPage2 <> "" THEN 
            ASSIGN tt-nfe013-trad.centro-custo:FORMAT IN FRAME fPage2 = v_cod_formato
                   c-desc-cc:SCREEN-VALUE IN FRAME fPage2 = v_des_ccusto.

        ELSE
            ASSIGN tt-nfe013-trad.centro-custo:FORMAT IN FRAME fPage2 = "x(20)"
                   c-desc-cc:SCREEN-VALUE IN FRAME fPage2 = "".  
    END.
    ELSE 
        assign c-desc-cc:screen-value       in frame fPage2 = "":U
               tt-nfe013-trad.centro-custo:FORMAT IN FRAME fPage2 = "X(20)".

    delete object h_api_ccusto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.centro-custo wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.centro-custo IN FRAME fPage2 /* Centro Custo */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME tt-nfe013-trad.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.it-codigo wMaintenance
ON F5 OF tt-nfe013-trad.it-codigo IN FRAME fPage1 /* Item */
DO:
  
     /*--- ZOOM SMART OBJECT ---*/ 
    {include/zoomvar.i &prog-zoom=inzoom/z02in172.w
                       &campo=tt-nfe013-trad.it-codigo
                       &campozoom=it-codigo
                       &campo2=c-desc-item
                       &campozoom2=desc-item
                       &frame="fPage1"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.it-codigo wMaintenance
ON LEAVE OF tt-nfe013-trad.it-codigo IN FRAME fPage1 /* Item */
DO:
    DEFINE VARIABLE de-indice AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE c-nr-lote AS CHARACTER   NO-UNDO.

    FOR FIRST item-fornec 
        WHERE item-fornec.item-do-forn = tt-nfe013.it-codigo
        AND   item-fornec.cod-emitente = tt-nfe003.cod-emitente NO-LOCK:
    END.
    IF AVAIL item-fornec THEN DO:
      ASSIGN tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}    =  item-fornec.it-codigo
             tt-nfe013-trad.un-comercial:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  item-fornec.unid-med-for
             /*de-indice                     = item-fornec.fator-conver / exp(10, item-fornec.num-casa-dec)
             tt-nfe013-trad.qtd-interna:SCREEN-VALUE IN FRAME {&FRAME-NAME}  =  string(tt-nfe013.qtd-comercial * de-indice)
             tt-nfe013-trad.qtd-interna                                      =  tt-nfe013.qtd-comercial * de-indice*/
             tt-nfe013-trad.preco-unit:SCREEN-VALUE IN FRAME {&FRAME-NAME}   =  string(tt-nfe013.preco-total / tt-nfe013-trad.qtd-interna)
             tt-nfe013-trad.preco-total:SCREEN-VALUE IN FRAME {&FRAME-NAME}  =  string(tt-nfe013.preco-total ).
    END.
    ELSE DO:
        FIND FIRST ITEM 
             WHERE ITEM.it-codigo = tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME fPage1  NO-LOCK  NO-ERROR.
        IF AVAIL ITEM THEN DO:
            ASSIGN tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = item.it-codigo
                   tt-nfe013-trad.un-comercial:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = item.un
                   tt-nfe013-trad.un-interna:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = item.un 
                   de-indice                     = 1
                   tt-nfe013-trad.qtd-interna                                        = tt-nfe013.qtd-comercial * de-indice
                   tt-nfe013-trad.qtd-interna:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = string(tt-nfe013.qtd-comercial * de-indice)
                   tt-nfe013-trad.preco-unit:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(tt-nfe013.preco-total / tt-nfe013-trad.qtd-interna)
                   tt-nfe013-trad.preco-total:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = string(tt-nfe013.preco-total).
        END.
        ELSE DO:
            FIND FIRST nfe020
                 WHERE nfe020.cod-emitente = tt-nfe003.cod-emitente
                   AND nfe020.item-do-forn = tt-nfe013.it-codigo NO-LOCK NO-ERROR.
            IF AVAIL nfe020 THEN Do:
                FIND ITEM 
                     WHERE ITEM.it-codigo = nfe020.it-codigo NO-LOCK NO-ERROR.
                IF AVAIL Item THEN DO:
                    ASSIGN tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = item.it-codigo
                           tt-nfe013-trad.un-comercial:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = item.un
                           tt-nfe013-trad.un-interna:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = item.un
                           de-indice             = 1
                           tt-nfe013-trad.qtd-interna                                        = tt-nfe013.qtd-comercial * de-indice
                           tt-nfe013-trad.qtd-interna:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = string(tt-nfe013.qtd-comercial * de-indice)
                           tt-nfe013-trad.preco-total:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = string(tt-nfe013.preco-total)
                           tt-nfe013-trad.preco-unit:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(tt-nfe013.preco-total / tt-nfe013-trad.qtd-interna ).
                END.
            END.
        END.
    END.

    IF VALID-HANDLE(h-boes011) THEN DO:
        RUN retornaDescItem IN h-boes011 (INPUT tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                          OUTPUT c-desc-item).
        DISP c-desc-item
            WITH FRAME fPage1.
    END.
    
    FOR FIRST ITEM 
         WHERE ITEM.it-codigo = tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME fPage1  NO-LOCK :

        IF tt-nfe013-trad.class-fiscal = "" THEN
             ASSIGN tt-nfe013-trad.class-fiscal   = ITEM.class-fiscal.

        ASSIGN tt-nfe013-trad.un-interna     = item.un.

             FIND FIRST item-uni-estab 
                  WHERE item-uni-estab.it-codigo   =  ITEM.it-codigo
                    AND item-uni-estab.cod-estabel =  tt-nfe003.cod-estabel NO-LOCK NO-ERROR .

             IF AVAIL item-uni-estab THEN DO:

                  ASSIGN tt-nfe013-trad.cod-depos  = item-uni-estab.deposito-pad 
                         tt-nfe013-trad.cod-localiz = item-uni-estab.cod-localiz. 
                  
             END.
             

        IF item.tipo-con-est = 3 THEN DO:
            /*
            FIND FIRST seq-num-lote WHERE SUBSTRING(STRING(seq-num-lote.lote),1,4) = STRING(YEAR(TODAY),"9999") EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL seq-num-lote THEN DO:
                ASSIGN c-nr-lote                  =  " "
                       c-nr-lote                 = string(NEXT-VALUE(seq-num-lote),"999999")
                       tt-nfe013-trad.lote-serie   = string(year(today),"9999") + string(c-nr-lote,"999999")
                       tt-nfe013-trad.dt-vali-lote = 12/31/2999 .
            end.           
            ELSE DO:
                ASSIGN CURRENT-VALUE(seq-num-lote) = 0.
                ASSIGN c-nr-lote                  =  " "
                       c-nr-lote                 = string(NEXT-VALUE(seq-num-lote),"999999")
                       tt-nfe013-trad.lote-serie   = string(year(today),"9999") + string(c-nr-lote,"999999")
                       tt-nfe013-trad.dt-vali-lote = 12/31/2999 .
            END.*/
        END.
        ELSE DO:
            ASSIGN tt-nfe013-trad.lote-serie   =  ''
                   tt-nfe013-trad.dt-vali-lote =  ? .
        END.
    END.

/*     DISP tt-nfe013-trad.it-codigo                         */
/*          tt-nfe013-trad.un-interna                        */
/*          tt-nfe013-trad.qtd-interna                       */
/*          tt-nfe013-trad.preco-unit                        */
/*          tt-nfe013-trad.preco-total    WITH FRAME fPage1. */

    DISP tt-nfe013-trad.class-fiscal 
         tt-nfe013-trad.cod-depos  
         tt-nfe013-trad.cod-localiz
         tt-nfe013-trad.lote-serie   
         tt-nfe013-trad.dt-vali-lote WITH FRAME fPage2.

    RUN habilitaCampos.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.it-codigo wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.it-codigo IN FRAME fPage1 /* Item */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.log-fifo-oc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.log-fifo-oc wMaintenance
ON VALUE-CHANGED OF tt-nfe013-trad.log-fifo-oc IN FRAME fPage1 /* FIFO Ordem Compra */
DO:
    IF INPUT FRAME fPage1 tt-nfe013-trad.log-fifo-oc THEN
        ASSIGN tt-nfe013-trad.numero-ordem:SENSITIVE    IN FRAME fPage1 = NO
               tt-nfe013-trad.numero-ordem:BGCOLOR      IN FRAME fPage1 = ?
               tt-nfe013-trad.num-pedido:SENSITIVE      IN FRAME fPage1 = NO
               tt-nfe013-trad.num-pedido:BGCOLOR        IN FRAME fPage1 = ?
               tt-nfe013-trad.numero-ordem:SCREEN-VALUE IN FRAME fPage1 = "0"
               tt-nfe013-trad.num-pedido:SCREEN-VALUE   IN FRAME fPage1 = "0".
    ELSE
        ASSIGN tt-nfe013-trad.numero-ordem:SENSITIVE    IN FRAME fPage1 = YES
               tt-nfe013-trad.numero-ordem:BGCOLOR      IN FRAME fPage1 = 14
               tt-nfe013-trad.num-pedido:SENSITIVE      IN FRAME fPage1 = YES
               tt-nfe013-trad.num-pedido:BGCOLOR        IN FRAME fPage1 = 14.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME tt-nfe013-trad.lote-serie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.lote-serie wMaintenance
ON LEAVE OF tt-nfe013-trad.lote-serie IN FRAME fPage2 /* Lote/S‚rie */
DO:
    IF  INPUT FRAME fPage2 tt-nfe013-trad.lote-serie   <> "" 
    AND INPUT FRAME fPage2 tt-nfe013-trad.dt-vali-lote  = ? THEN DO:
        RUN retornaValidLote IN {&hDBOTable} (INPUT  INPUT FRAME fPage1 tt-nfe013-trad.it-codigo,
                                              INPUT  INPUT FRAME fPage2 tt-nfe013-trad.lote-serie,
                                              INPUT  INPUT FRAME fPage2 tt-nfe013-trad.cod-refer,
                                              OUTPUT tt-nfe013-trad.dt-vali-lote).

        DISP tt-nfe013-trad.dt-vali-lote
            WITH FRAME fPage2.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME tt-nfe013-trad.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nat-operacao wMaintenance
ON F5 OF tt-nfe013-trad.nat-operacao IN FRAME fPage1 /* Natureza Opera‡Æo */
DO:
    {method/zoomfields.i &ProgramZoom="inzoom/z04in245.w"
                         &FieldZoom1="nat-operacao"
                         &FieldScreen1="tt-nfe013-trad.nat-operacao"
                         &Frame1="fPage1"
                         &FieldZoom2="denominacao"
                         &FieldScreen2="c-desc-nat-oper"
                         &Frame2="fPage1"
                         &EnableImplant="NO"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nat-operacao wMaintenance
ON LEAVE OF tt-nfe013-trad.nat-operacao IN FRAME fPage1 /* Natureza Opera‡Æo */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boin245"
                              &KeyValue1="tt-nfe013-trad.nat-operacao:SCREEN-VALUE IN FRAME fPage1"
                              &FieldName1="denominacao"
                              &FieldScreen1="c-desc-nat-oper"
                              &Frame1="fPage1"}
                              
    RUN habilitaCampos.
END.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nat-operacao wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.nat-operacao IN FRAME fPage1 /* Natureza Opera‡Æo */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME tt-nfe013-trad.nr-ord-produ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nr-ord-produ wMaintenance
ON F5 OF tt-nfe013-trad.nr-ord-produ IN FRAME fPage2 /* Ordem Produ‡Æo */
DO:
    /*--- ZOOM SMART OBJECT ---*/
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=inzoom/z01in271.w
                       &campo=tt-nfe013-trad.nr-ord-produ
                       &campozoom=nr-ord-produ
                       &frame=fPage2}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nr-ord-produ wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.nr-ord-produ IN FRAME fPage2 /* Ordem Produ‡Æo */
DO:
    APPLY "F5":U TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME tt-nfe013-trad.nro-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nro-comp wMaintenance
ON F5 OF tt-nfe013-trad.nro-comp IN FRAME fPage3 /* N£mero comp */
DO:
    {method/ZoomFields.i &ProgramZoom   = "dizoom\z05di088.w"
                         &FieldZoom1    = "serie"
                         &FieldScreen1  = "tt-nfe013-trad.serie-comp"
                         &Frame1        = "fPage3"
                         &FieldZoom2    = "nr-nota-fis"
                         &FieldScreen2  = "tt-nfe013-trad.nro-comp"
                         &Frame2        = "fPage3"
                         &FieldZoom3    = "nr-seq-fat"
                         &FieldScreen3  = "tt-nfe013-trad.seq-comp"
                         &Frame3        = "fPage3"
                         &RunMethod     = "run setParameters in hProgramZoom  (input tt-nfe003.cod-emitente, input tt-nfe003.cod-estabel). "
                         &EnableImplant = 'no'}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.nro-comp wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.nro-comp IN FRAME fPage3 /* N£mero comp */
DO:
    apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME tt-nfe013-trad.num-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.num-pedido wMaintenance
ON F5 OF tt-nfe013-trad.num-pedido IN FRAME fPage1 /* Pedido de Compra */
DO:
    {method/zoomfields.i &ProgramZoom="eszoom/z22es274.w"
                        &FieldZoom1="num-pedido"
                        &FieldScreen1="tt-nfe013-trad.num-pedido"
                        &Frame1="fPage1"                          
                        &FieldZoom2="numero-ordem"
                        &FieldScreen2="tt-nfe013-trad.numero-ordem"                                                    
                        &Frame2="fPage1"
                        &RunMethod="Run setParametersPedido in hProgramZoom ( input tt-nfe003.cod-emitente,
                                                                              input input frame fPage1 tt-nfe013-trad.it-codigo,
                                                                              input input frame fPage1 tt-nfe013-trad.num-pedido,
                                                                              input if   input frame fPage1 tt-nfe013-trad.num-pedido = 0 then 99999999
                                                                                    else input frame fPage1 tt-nfe013-trad.num-pedido,
                                                                              input c-seg-usuario,
                                                                              INPUT tt-nfe013-trad.r-rowid,
                                                                              INPUT tt-nfe003.dt-emissao )."


                        &EnableImplant="NO"}
                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.num-pedido wMaintenance
ON LEAVE OF tt-nfe013-trad.num-pedido IN FRAME fPage1 /* Pedido de Compra */
DO:

    assign tt-nfe013-trad.numero-ordem:screen-value in frame fpage1 = "0".

    if input frame fpage1 tt-nfe013-trad.num-pedido <> 0 then do:

        for first param-re fields(usuario variacao) where 
                  param-re.usuario = c-seg-usuario no-lock: 
        end.

        for each ordem-compra no-lock where 
                 ordem-compra.cod-emitente  = tt-nfe003.cod-emitente                       and
                 ordem-compra.num-pedido    = input frame fpage1 tt-nfe013-trad.num-pedido and 
                 ordem-compra.situacao      = 2     and
                 ordem-compra.cod-estabel  >= ""    and 
                 ordem-compra.cod-estabel  <= "ZZZ" and
                 ordem-compra.it-codigo     = input frame fPage1 tt-nfe013-trad.it-codigo,
           first prazo-compra where
                 prazo-compra.numero-ordem = ordem-compra.numero-ordem and
                (prazo-compra.situacao = 2 and (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0) and 
                (prazo-compra.data-entrega <= (today + param-re.variacao)) no-lock
                 by ordem-compra.num-pedido
                 by ordem-compra.numero-ordem:

            assign tt-nfe013-trad.numero-ordem:screen-value in frame fpage1 = string(ordem-compra.numero-ordem).

        end.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.num-pedido wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.num-pedido IN FRAME fPage1 /* Pedido de Compra */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.numero-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.numero-ordem wMaintenance
ON F5 OF tt-nfe013-trad.numero-ordem IN FRAME fPage1 /* Ordem de Compra */
DO:
    {method/zoomfields.i &ProgramZoom="eszoom/z22es274.w"
                         &FieldZoom1="num-pedido"
                         &FieldScreen1="tt-nfe013-trad.num-pedido"
                         &Frame1="fPage1"                          
                         &FieldZoom2="numero-ordem"
                         &FieldScreen2="tt-nfe013-trad.numero-ordem"                                                    
                         &Frame2="fPage1"
                         &RunMethod="Run setParametersPedido in hProgramZoom ( input tt-nfe003.cod-emitente,
                                                                               input input frame fPage1 tt-nfe013-trad.it-codigo,
                                                                               input input frame fPage1 tt-nfe013-trad.num-pedido,
                                                                               input if   input frame fPage1 tt-nfe013-trad.num-pedido = 0 then 99999999
                                                                                     else input frame fPage1 tt-nfe013-trad.num-pedido,
                                                                               input c-seg-usuario,
                                                                               INPUT tt-nfe013-trad.r-rowid,
                                                                               INPUT tt-nfe003.dt-emissao )."


                         &EnableImplant="NO"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.numero-ordem wMaintenance
ON LEAVE OF tt-nfe013-trad.numero-ordem IN FRAME fPage1 /* Ordem de Compra */
DO:
    DEFINE VARIABLE d-qt-saldo AS DECIMAL     NO-UNDO.
  
    FIND ordem-compra
        WHERE ordem-compra.numero-ordem = INPUT FRAME {&FRAME-NAME} tt-nfe013-trad.numero-ordem NO-LOCK NO-ERROR.
    IF AVAIL ordem-compra THEN DO:
        ASSIGN tt-nfe013-trad.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ordem-compra.it-codigo
               tt-nfe013-trad.num-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-compra.num-pedido)
               tt-nfe013-trad.it-codigo                                      = ordem-compra.it-codigo
               tt-nfe013-trad.num-pedido                                     = ordem-compra.num-pedido.
    END.

    APPLY "LEAVE":U TO tt-nfe013-trad.it-codigo    IN FRAME fPage1.

    IF tt-nfe013-trad.numero-ordem <> 0 THEN DO:

        ASSIGN d-qt-saldo = 0 .
        FOR EACH prazo-compra 
           where prazo-compra.numero-ordem = tt-nfe013-trad.numero-ordem
             and prazo-compra.situacao     = 2 NO-LOCK :
    
            ASSIGN d-qt-saldo = d-qt-saldo + prazo-compra.quant-saldo.
    
        END.
    
        /*IF d-qt-saldo < tt-nfe013-trad.qtd-interna THEN DO:
            RUN ze-erro.p ( 1,
                           'Quantidade fora da varia‡Æo permitida.' ,
                           'Quantidade fora da varia‡Æo permitida. Confirma ?').
        END.*/
    END.




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.numero-ordem wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.numero-ordem IN FRAME fPage1 /* Ordem de Compra */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.qtd-interna
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.qtd-interna wMaintenance
ON LEAVE OF tt-nfe013-trad.qtd-interna IN FRAME fPage1 /* Nossa Qtde */
DO:
  DEFINE VARIABLE d-qt-saldo AS DECIMAL     NO-UNDO.

  
  ASSIGN tt-nfe013-trad.preco-total:screen-value in frame fpage1 = string(dec(tt-nfe013-trad.preco-unit:screen-value) * DEC(tt-nfe013-trad.qtd-interna:SCREEN-VALUE IN FRAME fPage1)).

  
  IF tt-nfe013-trad.numero-ordem <> 0 THEN DO:

      ASSIGN d-qt-saldo = 0 .
      FOR EACH prazo-compra 
         where prazo-compra.numero-ordem = tt-nfe013-trad.numero-ordem
           and prazo-compra.situacao     = 2 NO-LOCK :
  
          ASSIGN d-qt-saldo = d-qt-saldo + prazo-compra.quant-saldo.
  
      END.
  
      /*IF d-qt-saldo < DEC(tt-nfe013-trad.qtd-interna:SCREEN-VALUE IN FRAME fPage1) THEN DO:
          RUN ze-erro.p ( 1,
                         'Quantidade fora da varia‡Æo permitida.' ,
                         'Quantidade fora da varia‡Æo permitida. Confirma ?').
      END.*/
  END.

 /* DISP tt-nfe013-trad.preco-total WITH FRAME fPage1.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME tt-nfe013-trad.seq-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.seq-comp wMaintenance
ON F5 OF tt-nfe013-trad.seq-comp IN FRAME fPage3 /* Seq Complem */
DO:
    {method/ZoomFields.i &ProgramZoom   = "dizoom\z05di088.w"
                         &FieldZoom1    = "serie"
                         &FieldScreen1  = "tt-nfe013-trad.serie-comp"
                         &Frame1        = "fPage3"
                         &FieldZoom2    = "nr-nota-fis"
                         &FieldScreen2  = "tt-nfe013-trad.nro-comp"
                         &Frame2        = "fPage3"
                         &FieldZoom3    = "nr-seq-fat"
                         &FieldScreen3  = "tt-nfe013-trad.seq-comp"
                         &Frame3        = "fPage3"
                         &RunMethod     = "run setParameters in hProgramZoom  (input tt-nfe003.cod-emitente, input tt-nfe003.cod-estabel). "
                         &EnableImplant = 'no'}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.seq-comp wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.seq-comp IN FRAME fPage3 /* Seq Complem */
DO:
    apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe013-trad.serie-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.serie-comp wMaintenance
ON F5 OF tt-nfe013-trad.serie-comp IN FRAME fPage3 /* S‚rie */
DO: 
    {method/ZoomFields.i &ProgramZoom   = "dizoom\z05di088.w"
                         &FieldZoom1    = "serie"
                         &FieldScreen1  = "tt-nfe013-trad.serie-comp"
                         &Frame1        = "fPage3"
                         &FieldZoom2    = "nr-nota-fis"
                         &FieldScreen2  = "tt-nfe013-trad.nro-comp"
                         &Frame2        = "fPage3"
                         &FieldZoom3    = "nr-seq-fat"
                         &FieldScreen3  = "tt-nfe013-trad.seq-comp"
                         &Frame3        = "fPage3"
                         &RunMethod     = "run setParameters in hProgramZoom  (input tt-nfe003.cod-emitente, input tt-nfe003.cod-estabel). "
                         &EnableImplant = 'no'}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe013-trad.serie-comp wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe013-trad.serie-comp IN FRAME fPage3 /* S‚rie */
DO:
    apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenance 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{maintenance/mainblock.i}

tt-nfe013-trad.nat-operacao:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage1.
tt-nfe013-trad.num-pedido:LOAD-MOUSE-POINTER("image/lupa.cur":U)     IN FRAME fPage1.
tt-nfe013-trad.numero-ordem:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage1.
tt-nfe013-trad.class-fiscal:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage2.
tt-nfe013-trad.conta-contabil:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage2.
tt-nfe013-trad.centro-custo:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage2.
tt-nfe013-trad.nr-ord-produ:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage2.
tt-nfe013-trad.cod-refer:LOAD-MOUSE-POINTER("image/lupa.cur":U)      IN FRAME fPage2.
tt-nfe013-trad.cod-depos:LOAD-MOUSE-POINTER("image/lupa.cur":U)      IN FRAME fPage2.
tt-nfe013-trad.cod-localiz:LOAD-MOUSE-POINTER("image/lupa.cur":U)    IN FRAME fPage2.
tt-nfe013-trad.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur":U)      IN FRAME fPage1.

&IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
    tt-nfe013-trad.cod-unid-negoc:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage2.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMaintenance 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if not l-aux then do:
    run initializeDBOs in this-procedure.

    assign l-aux = yes.
end.

RUN openQueriesSon IN THIS-PROCEDURE.

ASSIGN tt-nfe013-trad.seq-item      :screen-value in frame fPage1 = string(tt-nfe013-trad.seq-item)      
       tt-nfe013-trad.it-codigo     :screen-value in frame fPage1 = string(tt-nfe013-trad.it-codigo)
       tt-nfe013-trad.nat-operacao  :screen-value in frame fPage1 = string(tt-nfe013-trad.nat-operacao)      
       tt-nfe013-trad.num-pedido    :screen-value in frame fPage1 = string(tt-nfe013-trad.num-pedido)    
       tt-nfe013-trad.numero-ordem  :screen-value in frame fPage1 = string(tt-nfe013-trad.numero-ordem)
       tt-nfe013-trad.int-1         :screen-value in frame fPage1 = string(tt-nfe013-trad.int-1)
       tt-nfe013-trad.qtd-interna   :screen-value in frame fPage1 = string(tt-nfe013-trad.qtd-interna) 
       tt-nfe013-trad.un-interna    :screen-value in frame fPage1 = string(tt-nfe013-trad.un-interna) 
       tt-nfe013-trad.preco-unit    :screen-value in frame fPage1 = string(tt-nfe013-trad.preco-unit)    
       tt-nfe013-trad.preco-total   :screen-value in frame fPage1 = string(tt-nfe013-trad.preco-total) 
       tt-nfe013-trad.class-fiscal  :screen-value in frame fPage2 = string(tt-nfe013-trad.class-fiscal)     
       tt-nfe013-trad.conta-contabil:screen-value in frame fPage2 = string(tt-nfe013-trad.conta-contabil)
       tt-nfe013-trad.centro-custo  :screen-value in frame fPage2 = string(tt-nfe013-trad.centro-custo)
       tt-nfe013-trad.nr-ord-produ  :screen-value in frame fPage2 = string(tt-nfe013-trad.nr-ord-produ)
       tt-nfe013-trad.cod-refer     :screen-value in frame fPage2 = string(tt-nfe013-trad.cod-refer)     
       tt-nfe013-trad.cod-depos     :screen-value in frame fPage2 = string(tt-nfe013-trad.cod-depos)     
       tt-nfe013-trad.cod-localiz   :screen-value in frame fPage2 = string(tt-nfe013-trad.cod-localiz)   
       tt-nfe013-trad.lote-serie    :screen-value in frame fPage2 = string(tt-nfe013-trad.lote-serie)    
       tt-nfe013-trad.dt-vali-lote  :screen-value in frame fPage2 = string(tt-nfe013-trad.dt-vali-lote)
       tt-nfe013-trad.cod-unid-negoc:screen-value in frame fPage2 = string(tt-nfe013-trad.cod-unid-negoc)   
       tt-nfe013.seq-item      :screen-value in frame fPage1 = string(tt-nfe013.seq-item)      
       tt-nfe013.it-codigo     :screen-value in frame fPage1 = string(tt-nfe013.it-codigo)
       tt-nfe013.cod-cfop      :screen-value in frame fPage1 = string(tt-nfe013.cod-cfop)      
       tt-nfe013.num-pedido    :screen-value in frame fPage1 = string(tt-nfe013.num-pedido)    
       tt-nfe013.numero-ordem  :screen-value in frame fPage1 = string(tt-nfe013.numero-ordem)
       tt-nfe013.qtd-comercial :screen-value in frame fPage1 = string(tt-nfe013.qtd-comercial) 
       tt-nfe013.preco-unit    :screen-value in frame fPage1 = string(tt-nfe013.preco-unit)    
       tt-nfe013.preco-total   :screen-value in frame fPage1 = string(tt-nfe013.preco-total) 
       tt-nfe013.class-ipi     :screen-value in frame fPage2 = string(tt-nfe013.class-ipi)     
       tt-nfe013.conta-contabil:screen-value in frame fPage2 = string(tt-nfe013.conta-contabil)
       tt-nfe013.nr-ord-produ  :screen-value in frame fPage2 = string(tt-nfe013.nr-ord-produ)
       tt-nfe013.cod-refer     :screen-value in frame fPage2 = string(tt-nfe013.cod-refer)     
       tt-nfe013.cod-depos     :screen-value in frame fPage2 = string(tt-nfe013.cod-depos)     
       tt-nfe013.cod-localiz   :screen-value in frame fPage2 = string(tt-nfe013.cod-localiz)   
       tt-nfe013.lote-serie    :screen-value in frame fPage2 = string(tt-nfe013.lote-serie)    
       tt-nfe013.dt-vali-lote  :screen-value in frame fPage2 = string(tt-nfe013.dt-vali-lote)
       btSave:SENSITIVE IN FRAME fPage0                      = YES. 

IF AVAIL tt-nfe003 then do:
    IF tt-nfe003.lg-devol THEN
        RUN setEnabled IN hFolder (INPUT 3, INPUT FALSE). 
    ELSE DO:
        RUN setEnabled IN hFolder (INPUT 3, INPUT TRUE). 

        ASSIGN tt-nfe013-trad.serie-comp:sensitive in frame fpage3 = yes
               tt-nfe013-trad.nro-comp  :sensitive in frame fpage3 = yes
               tt-nfe013-trad.seq-comp  :sensitive in frame fpage3 = yes.
        
        if tt-nfe013-trad.serie-comp:load-mouse-pointer("image/lupa.cur":U) IN FRAME fpage3 then.
        if tt-nfe013-trad.nro-comp  :load-mouse-pointer("image/lupa.cur":U) IN FRAME fpage3 then.
        if tt-nfe013-trad.seq-comp  :load-mouse-pointer("image/lupa.cur":U) IN FRAME fpage3 then.
    end.
end.
ELSE
    RUN setEnabled IN hFolder (INPUT 3, INPUT FALSE).

RUN afterEnableFields.

APPLY "LEAVE":U TO tt-nfe013-trad.it-codigo    IN FRAME fPage1.
APPLY "LEAVE":U TO {&ttTable}.class-fiscal IN FRAME fPage2.
APPLY "LEAVE":U TO {&ttTable}.cod-refer    IN FRAME fPage2.
APPLY "LEAVE":U TO {&ttTable}.cod-depos    IN FRAME fPage2.

FIND FIRST estabelec
       WHERE estabelec.cod-estabel = param-estoq.estabel-pad NO-LOCK NO-ERROR.
  ASSIGN i-empresa = IF AVAIL estabelec THEN estabelec.ep-codigo ELSE "".

  /*if p_cod_format_cta_ctbl = "" THEN DO:
       run prgint/utb/utb743za.py persistent set h_api_cta.
       run pi_retorna_formato_cta_ctbl in h_api_cta (input  i-empresa,                 /* EMPRESA EMS 2 */
                                                  input  "",                 /* PLANO CONTAS */
                                                  input  TODAY, /* DATA DE TRANSACAO */
                                                  output p_cod_format_cta_ctbl,   /* FORMATO cta */
                                                  output table tt_log_erro). /* ERROS */
       DELETE OBJECT h_api_cta.
  END.*/

  ASSIGN tt-nfe013-trad.conta-contabil = REPLACE (tt-nfe013-trad.conta-contabil:SCREEN-VALUE, ".", "").
  ASSIGN tt-nfe013-trad.centro-custo = REPLACE (tt-nfe013-trad.centro-custo:SCREEN-VALUE, ".", "").

  IF tt-nfe013-trad.conta-contabil:SCREEN-VALUE IN FRAME fPage2 <> "" THEN
      ASSIGN tt-nfe013-trad.conta-contabil:SCREEN-VALUE IN FRAME fPage2 = string(tt-nfe013-trad.conta-contabil, p_cod_format_cta_ctbl) .
             
   apply "leave" to tt-nfe013-trad.conta-contabil in frame fPage2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterEnableFields wMaintenance 
PROCEDURE afterEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF AVAIL {&ttTable} THEN DO:

       ASSIGN fi-inf-compl:SCREEN-VALUE IN FRAME fpage1 =  {&ttTable}.inf-complementar .

       RUN retornaFormatoConta IN {&hDBOTable} (OUTPUT c-formato-conta).

       APPLY "LEAVE":U TO {&ttTable}.nat-operacao IN FRAME fPage1.

       IF VALID-HANDLE({&hDBOTable}) THEN DO:
           RUN retornaDescItem IN {&hDBOTable} (INPUT {&ttTable}.it-codigo:SCREEN-VALUE IN FRAME fPage1 ,
                                                OUTPUT c-desc-item).
           DISP c-desc-item
               WITH FRAME fPage1.
       END.

       APPLY "LEAVE":U TO {&ttTable}.class-fiscal IN FRAME fPage2.
       APPLY "LEAVE":U TO {&ttTable}.cod-refer    IN FRAME fPage2.
       APPLY "LEAVE":U TO {&ttTable}.cod-depos    IN FRAME fPage2.

       RUN habilitaCampos.

       APPLY "VALUE-CHANGED":U TO {&ttTable}.log-fifo-oc  IN FRAME fPage1.
    END.

    &IF "{&bf_mat_versao_ems}" < "2.062" &THEN
        ASSIGN {&ttTable}.int-1:HIDDEN          IN FRAME fPage1 = YES
               {&ttTable}.cod-unid-negoc:HIDDEN IN FRAME fPage2 = YES
               c-des-unid-negoc:HIDDEN          IN FRAME fPage2 = YES.
    &ELSE
        ASSIGN {&ttTable}.log-fifo-oc:HIDDEN    IN FRAME fPage1 = YES.
    &ENDIF  
    
         
RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wMaintenance 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    tt-nfe013-trad.it-codigo:SENSITIVE IN FRAME fPage1    = YES.
    tt-nfe013-trad.nat-operacao:SENSITIVE IN FRAME fPage1 = YES.
    tt-nfe013-trad.num-pedido:SENSITIVE IN FRAME fPage1   = YES.
    tt-nfe013-trad.numero-ordem:SENSITIVE IN FRAME fPage1 = YES.
    tt-nfe013-trad.int-1:SENSITIVE IN FRAME fPage1        = YES.
    tt-nfe013-trad.qtd-interna:SENSITIVE IN FRAME fPage1  = YES.
    tt-nfe013-trad.un-interna:SENSITIVE IN FRAME fPage1   = YES.
    tt-nfe013-trad.preco-unit:SENSITIVE IN FRAME fPage1   = YES.
    tt-nfe013-trad.preco-total:SENSITIVE IN FRAME fPage1  = YES.
    tt-nfe013-trad.log-fifo-oc:SENSITIVE IN FRAME fPage1  = YES.
    tt-nfe013-trad.class-fiscal:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.conta-contabil:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.centro-custo:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.nr-ord-produ:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.cod-refer:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.cod-depos:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.cod-localiz:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.lote-serie:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.dt-vali-lote:SENSITIVE IN FRAME fPage2 = YES.
    tt-nfe013-trad.cod-unid-negoc:SENSITIVE IN FRAME fPage2 = YES.
    
     
    RUN afterEnableFields.

    RUN repositionRecord IN THIS-PROCEDURE (INPUT prTable).
    ASSIGN btSave:SENSITIVE IN FRAME fPage0   = YES.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDestroyInterface wMaintenance 
PROCEDURE beforeDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF VALID-HANDLE({&hDBOTable}) THEN DO:
        DELETE PROCEDURE {&hDBOTable}.
        ASSIGN {&hDBOTable} = ?.
    END.

    IF VALID-HANDLE({&hDBOTable2}) THEN DO:
        DELETE PROCEDURE {&hDBOTable2}.
        ASSIGN {&hDBOTable2} = ?.
    END.

    IF VALID-HANDLE(h-boin046) THEN DO:
        DELETE PROCEDURE h-boin046.
        ASSIGN h-boin046 = ?.
    END.

    IF VALID-HANDLE(h-boin084) THEN DO:
        DELETE PROCEDURE h-boin084.
        ASSIGN h-boin084 = ?.
    END.

    IF VALID-HANDLE(h-boin245) THEN DO:
        DELETE PROCEDURE h-boin245.
        ASSIGN h-boin245 = ?.
    END.

    IF VALID-HANDLE(h-boin377) THEN DO:
        DELETE PROCEDURE h-boin377.
        ASSIGN h-boin377 = ?.
    END.

    IF VALID-HANDLE(h-boin745) THEN DO:
        DELETE PROCEDURE h-boin745.
        ASSIGN h-boin745 = ?.
    END.

    IF VALID-HANDLE(h-boad049) THEN DO:
        DELETE PROCEDURE h-boad049.
        ASSIGN h-boad049 = ?.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HabilitaCampos wMaintenance 
PROCEDURE HabilitaCampos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST ITEM
        WHERE ITEM.it-codigo = {&ttTable}.it-codigo:SCREEN-VALUE IN FRAME fPage1 NO-LOCK NO-ERROR .

    IF AVAIL ITEM THEN DO:
       
        IF item.tipo-con-est <> 3 THEN DO:
            ASSIGN {&ttTable}.lote-serie:SENSITIVE IN FRAME fPage2   = NO
                   {&ttTable}.dt-vali-lote:SENSITIVE IN FRAME fPage2 = NO 
                   {&ttTable}.lote-serie:BGCOLOR IN FRAME   fPage2   =  ?
                   {&ttTable}.dt-vali-lote:BGCOLOR IN FRAME   fPage2 =  ?.
        END.
        ELSE DO:
            ASSIGN {&ttTable}.lote-serie:SENSITIVE IN FRAME fPage2   = YES
                   {&ttTable}.dt-vali-lote:SENSITIVE IN FRAME fPage2 = YES
                   {&ttTable}.lote-serie:BGCOLOR IN FRAME   fPage2   =  14
                   {&ttTable}.dt-vali-lote:BGCOLOR IN FRAME   fPage2 =  14.
        END.       
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wMaintenance 
PROCEDURE initializeDBOs :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

/*:T--- Verifica se o DBO TABLE j  est  inicializado ---*/

    IF NOT VALID-HANDLE({&hDBOTable}) 
    OR {&hDBOTable}:TYPE      <> "PROCEDURE":U 
    OR {&hDBOTable}:FILE-NAME <> "esbo/boes011.p":U THEN DO:       
       RUN esbo/boes011.p PERSISTENT SET {&hDBOTable}.
    END.

    RUN openQueryStatic  IN {&hDBOTable} (INPUT "Main":U) NO-ERROR. 
    RUN repositionRecord IN {&hDBOTable} (INPUT prTable).
    RUN getRecord        IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).

    
    FIND FIRST {&ttTable} NO-LOCK NO-ERROR.

    RUN setConstraintMonitor IN {&hDBOTable} (INPUT {&ttTable}.ch-acesso-comp-nfe /*tt-nfe013.ch-acesso-comp-nfe*/,
                                              INPUT {&ttTable}.idi-orig-trad).
    RUN openQueryStatic      IN {&hDBOTable} (INPUT "Monitor":U)  NO-ERROR.
    /**/    

/*     RUN openQueryStatic  IN {&hDBOTable} (INPUT "Main":U) NO-ERROR. */
/*     RUN repositionRecord IN {&hDBOTable} (INPUT prTable).            */
/*     RUN getRecord        IN {&hDBOTable} (OUTPUT TABLE {&ttTable2}). */
/*                                                                      */
/*     FIND FIRST {&ttTable2} NO-LOCK NO-ERROR.                         */
/*                                                                      */

    IF NOT VALID-HANDLE({&hDBOTable2}) 
    OR {&hDBOTable2}:TYPE      <> "PROCEDURE":U 
    OR {&hDBOTable2}:FILE-NAME <> "esbo/boes011.p":U THEN DO:       
       RUN esbo/boes011.p PERSISTENT SET {&hDBOTable2}.
    END.

    RUN openQueryStatic  IN {&hDBOTable2} (INPUT "Main":U) NO-ERROR. 
    RUN setConstraintMonitor IN {&hDBOTable2} (INPUT {&ttTable}.ch-acesso-comp-nfe /*tt-nfe013.ch-acesso-comp-nfe*/,
                                              INPUT 1 /*{&ttTable2}.idi-orig-trad)*/ ).
    RUN openQueryStatic      IN {&hDBOTable2} (INPUT "Monitor":U)  NO-ERROR.

    RUN getRecord        IN {&hDBOTable2} (OUTPUT TABLE {&ttTable2}). 
    FIND FIRST {&ttTable2} NO-LOCK NO-ERROR.
/**/    

    IF NOT VALID-HANDLE({&hDBOTable3}) 
    OR {&hDBOTable3}:TYPE      <> "PROCEDURE":U 
    OR {&hDBOTable3}:FILE-NAME <> "esbo/boes003.p":U THEN DO:
        RUN esbo/boes003.p PERSISTENT SET {&hDBOTable3}.
    END.
    
    RUN emptyRowObject  IN {&hDBOTable3}.
    RUN openQueryStatic IN {&hDBOTable3} (INPUT "Main":U).
    RUN goToKey         IN {&hDBOTable3} (INPUT {&ttTable2}.ch-acesso-comp-nfe,
                                          INPUT {&ttTable2}.idi-orig-trad).
    RUN getRecord       IN {&hDBOTable3} (OUTPUT TABLE {&ttTable3}).

    FIND FIRST {&ttTable3} NO-LOCK NO-ERROR.

    IF NOT VALID-HANDLE(h-boin046) 
    OR h-boin046:TYPE      <> "PROCEDURE":U 
    OR h-boin046:FILE-NAME <> "inbo/boin046.p":U THEN DO:
        RUN inbo/boin046.p PERSISTENT SET h-boin046.
    END.
    RUN openQuery IN h-boin046 (INPUT 1) NO-ERROR.

    IF NOT VALID-HANDLE(h-boin084) 
    OR h-boin084:TYPE      <> "PROCEDURE":U 
    OR h-boin084:FILE-NAME <> "inbo/boin084.p":U THEN DO:
        RUN inbo/boin084.p PERSISTENT SET h-boin084.
    END.
    RUN openQueryStatic IN h-boin084 (INPUT "Main":U) NO-ERROR.

    IF NOT VALID-HANDLE(h-boin377) 
    OR h-boin377:TYPE      <> "PROCEDURE":U 
    OR h-boin377:FILE-NAME <> "inbo/boin377na.p":U THEN DO:
        RUN inbo/boin377na.p PERSISTENT SET h-boin377.
    END.
    RUN openQueryStatic IN h-boin377 (INPUT "Main":U) NO-ERROR.

    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
        IF NOT VALID-HANDLE(h-boin745) 
        OR h-boin745:TYPE      <> "PROCEDURE":U 
        OR h-boin745:FILE-NAME <> "inbo/boin745.p":U THEN DO:
            RUN inbo/boin745.p PERSISTENT SET h-boin745.
        END.
        RUN openQueryStatic IN h-boin745 (INPUT "Main":U) NO-ERROR.
    &ENDIF
    
    IF NOT VALID-HANDLE(h-boad049) 
    OR h-boad049:TYPE      <> "PROCEDURE":U 
    OR h-boad049:FILE-NAME <> "adbo/boad049na.p":U THEN DO:
        RUN adbo/boad049na.p PERSISTENT SET h-boad049.
    END.
    RUN openQueryStatic IN h-boad049 (INPUT "Main":U) NO-ERROR.

    /** Natureza de Opera‡Æo **/
    IF NOT VALID-HANDLE(h-boin245) 
    OR h-boin245:TYPE      <> "PROCEDURE":U 
    OR h-boin245:FILE-NAME <> "inbo/boin245na.p":U THEN DO:
        RUN inbo/boin245na.p PERSISTENT SET h-boin245.
    END.
    RUN openQueryStatic IN h-boin245 (INPUT "Main":U) NO-ERROR.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueriesSon wMaintenance 
PROCEDURE openQueriesSon :
/*:T------------------------------------------------------------------------------
  Purpose:     Atualiza browsers filhos
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    RUN openQueryStatic  IN {&hDBOTable2} (INPUT "Main":U) NO-ERROR. 
    RUN goToKey          IN {&hDBOTable2} (INPUT {&ttTable}.ch-acesso-comp-nfe /*tt-nfe013.ch-acesso-comp-nfe*/,
                                           INPUT 1,
                                           INPUT {&ttTable}.seq-item).
    RUN getRecord        IN {&hDBOTable2} (OUTPUT TABLE {&ttTable2}). 
    FIND FIRST {&ttTable2} NO-LOCK NO-ERROR.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

