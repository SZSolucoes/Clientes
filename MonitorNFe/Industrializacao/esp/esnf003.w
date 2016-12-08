&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWindow


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-nfe003 NO-UNDO LIKE nfe003
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-nfe017 NO-UNDO LIKE nfe017
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/


{include/i-prgvrs.i ESNF003 2.00.00.000}
{cdp/cdcfgmat.i}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF003
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Master

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Documentos

&GLOBAL-DEFINE page0Widgets   btParam btSelecao btAtualiza btSimula ~
                              btQueryJoins btReportsJoins btExit btHelp
&GLOBAL-DEFINE page1Widgets   btVaPara brTable1 brMsg btModifica btValida ~
                              btDocumento c-nfe c-msg-nfe ~
                              btReprocessa btReprocessaFis btChave BtElimina btreativa

/* Parameters Definitions ---                                           */
DEFINE NEW GLOBAL SHARED VARIABLE v_log_eai_habilit AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE p-tg-ativa           AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-historico       AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-digitada        AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-erro-neg        AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-atualizada      AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-eliminada       AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-danfe           AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-liberada        AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-tg-conferido       AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE p-chave-acesso       AS CHARACTER FORMAT "x(60)" NO-UNDO.
DEFINE VARIABLE c-depos-aux          AS CHARACTER                NO-UNDO.
DEFINE VARIABLE p-cod-emitente-ini LIKE emitente.cod-emitente    NO-UNDO.
DEFINE VARIABLE p-cod-emitente-fim LIKE emitente.cod-emitente    NO-UNDO.
DEFINE VARIABLE p-cod-estabel-ini  LIKE docum-est.cod-estabel    NO-UNDO.
DEFINE VARIABLE p-cod-estabel-fim  LIKE docum-est.cod-estabel    NO-UNDO.
DEFINE VARIABLE p-serie-ini        LIKE docum-est.serie-docto    NO-UNDO.
DEFINE VARIABLE p-serie-fim        LIKE docum-est.serie-docto    NO-UNDO.
DEFINE VARIABLE p-nro-docto-ini    LIKE docum-est.nro-docto      NO-UNDO.
DEFINE VARIABLE p-nro-docto-fim    LIKE docum-est.nro-docto      NO-UNDO.
DEFINE VARIABLE p-dt-emissao-ini   LIKE docum-est.dt-emissao     NO-UNDO.
DEFINE VARIABLE p-dt-emissao-fim   LIKE docum-est.dt-emissao     NO-UNDO.
DEFINE VARIABLE c-situacao           AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE h-boes003            AS HANDLE                   NO-UNDO.
DEFINE VARIABLE h-boes011            AS HANDLE                   NO-UNDO.
DEFINE VARIABLE h-boes012            AS HANDLE                   NO-UNDO.
DEFINE VARIABLE h-re1001             AS HANDLE                   NO-UNDO.
DEFINE VARIABLE iRowsReturned        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE c-nome-abrev       LIKE emitente.nome-abrev      NO-UNDO.
DEFINE VARIABLE l-reprocessado       AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE r-documento          AS ROWID                    NO-UNDO.
DEFINE VARIABLE h-axsep004           AS HANDLE                   NO-UNDO.
DEFINE VARIABLE d-data               AS DATE  FORMAT 99/99/999   NO-UNDO.
DEFINE VARIABLE c-desc-sit-sefaz     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp              AS HANDLE      NO-UNDO.
define variable c-msg-erro           as character format "x(80)" no-undo.
define variable l-disp               as logical init no no-undo.
define variable c-nome-arq           as char            no-undo.

FORM tt-nfe003.ch-acesso-comp-nfe             COLUMN-LABEL "Chave Acesso" format "x(45)"
     tt-nfe003.nro-docto                      COLUMN-LABEL "Documento" format "x(11)"      
     tt-nfe003.serie-docto                    COLUMN-LABEL "Serie" format "x(2)"
     tt-nfe003.cod-emitente                   COLUMN-LABEL "Emit"
     tt-nfe003.cod-estabel                    COLUMN-LABEL "Estab" format "x(4)"
     c-msg-erro                            COLUMN-LABEL "Erro"
     WITH FRAME f-relatorio WIDTH 300 DOWN STREAM-IO. 

def var c-cnpj as character no-undo.
def var i-cont as integer   no-undo.

DEF TEMP-TABLE tt_log_erro  NO-UNDO 
    FIELD ttv_num_cod_erro  AS INTEGER   INITIAL ?
    FIELD ttv_des_msg_ajuda AS CHARACTER INITIAL ?
    FIELD ttv_des_msg_erro  AS CHARACTER INITIAL ?.

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD identif-segment AS CHAR
    FIELD cd-erro         AS INTEGER
    FIELD desc-erro       AS CHAR FORMAT "x(80)".

DEF TEMP-TABLE tt-natureza NO-UNDO
    FIELD nat-operacao LIKE natur-oper.nat-operacao.

DEFINE TEMP-TABLE tt-doc-fisico NO-UNDO LIKE doc-fisico .

DEFINE TEMP-TABLE tt-it-doc-fisico NO-UNDO LIKE it-doc-fisico.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brMsg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-nfe017 tt-nfe003

/* Definitions for BROWSE brMsg                                         */
&Scoped-define FIELDS-IN-QUERY-brMsg tt-nfe017.dt-msg tt-nfe017.hr-msg ~
tt-nfe017.log-ativo tt-nfe017.cd-msg tt-nfe017.texto-msg 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brMsg 
&Scoped-define QUERY-STRING-brMsg FOR EACH tt-nfe017 NO-LOCK ~
    BY tt-nfe017.dt-msg ~
       BY tt-nfe017.hr-msg ~
        BY tt-nfe017.cd-msg INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brMsg OPEN QUERY brMsg FOR EACH tt-nfe017 NO-LOCK ~
    BY tt-nfe017.dt-msg ~
       BY tt-nfe017.hr-msg ~
        BY tt-nfe017.cd-msg INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brMsg tt-nfe017
&Scoped-define FIRST-TABLE-IN-QUERY-brMsg tt-nfe017


/* Definitions for BROWSE brTable1                                      */
&Scoped-define FIELDS-IN-QUERY-brTable1 fn_situacao (tt-nfe003.idi-situacao) @ c-situacao tt-nfe003.dt-emissao fn_nome_abrev (tt-nfe003.cod-emitente) @ c-nome-abrev tt-nfe003.nro-docto tt-nfe003.serie-docto tt-nfe003.cod-emitente tt-nfe003.cod-estabel fn_sit_sefaz (tt-nfe003.sit-sefaz) @ c-desc-sit-sefaz   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable1   
&Scoped-define SELF-NAME brTable1
&Scoped-define QUERY-STRING-brTable1 FOR EACH tt-nfe003 NO-LOCK     BY tt-nfe003.cod-estabel      BY tt-nfe003.cod-emitente       BY tt-nfe003.serie-docto        BY tt-nfe003.nro-docto INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable1 OPEN QUERY {&SELF-NAME} FOR EACH tt-nfe003 NO-LOCK     BY tt-nfe003.cod-estabel      BY tt-nfe003.cod-emitente       BY tt-nfe003.serie-docto        BY tt-nfe003.nro-docto INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brTable1 tt-nfe003
&Scoped-define FIRST-TABLE-IN-QUERY-brTable1 tt-nfe003


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brMsg}~
    ~{&OPEN-QUERY-brTable1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btSimula rtToolBar-2 btAtualiza btQueryJoins ~
btReportsJoins btExit btHelp btParam btSelecao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_nome_abrev wWindow 
FUNCTION fn_nome_abrev RETURNS CHARACTER
    ( p-cod-emitente AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_situacao wWindow 
FUNCTION fn_situacao RETURNS CHARACTER
  ( p-idi-situacao AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_sit_sefaz wWindow 
FUNCTION fn_sit_sefaz RETURNS CHARACTER
  ( INPUT i-sit-sefaz AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btAtualiza 
     IMAGE-UP FILE "image/toolbar/im-relo.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Atualiza Documentos".

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btParam 
     IMAGE-UP FILE "image/toolbar/im-param.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-param.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Parametros" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros Documentos".

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image\im-joi":U
     IMAGE-INSENSITIVE FILE "image\ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image\im-pri":U
     IMAGE-INSENSITIVE FILE "image\ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSelecao 
     IMAGE-UP FILE "image/toolbar/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-ran.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Seleá∆o" 
     SIZE 5 BY 1.25 TOOLTIP "Seleá∆o Documentos".

DEFINE BUTTON btSimula 
     IMAGE-UP FILE "image/im-chcka.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-chck1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Simula" 
     SIZE 5 BY 1.25 TOOLTIP "Simular Integraá∆o com Recebimento Fiscal".

DEFINE RECTANGLE rtToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 136 BY 1.5
     BGCOLOR 18 .

DEFINE BUTTON btAgregado 
     IMAGE-UP FILE "image/toolbar/im-expan.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-expan.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Nota Agregada" 
     SIZE 5.57 BY 1.5 TOOLTIP "Nota Agregada"
     FONT 4.

DEFINE BUTTON btChave 
     IMAGE-UP FILE "image/toolbar/im-key.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-key.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Chave Doc" 
     SIZE 5.57 BY 1.5 TOOLTIP "Chave Documento".

DEFINE BUTTON btDocumento 
     IMAGE-UP FILE "image/toolbar/im-fold.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-fold.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "RE1001" 
     SIZE 5.57 BY 1.5.

DEFINE BUTTON btElimina 
     IMAGE-UP FILE "image/toolbar/im-era.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-era.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Elimina" 
     SIZE 5.57 BY 1.5 TOOLTIP "Elimina Registro de NFe Selecionado".

DEFINE BUTTON btModifica 
     IMAGE-UP FILE "image/toolbar/im-mod.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-mod.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Modifica" 
     SIZE 5.57 BY 1.5 TOOLTIP "Modifica Documento".

DEFINE BUTTON btReativa 
     IMAGE-UP FILE "image/toolbar/im-undo.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-undo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Reativar" 
     SIZE 5.57 BY 1.5 TOOLTIP "Reativar Documento no Monitor".

DEFINE BUTTON btReprocessa 
     IMAGE-UP FILE "image/toolbar/im-chck1.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-chck1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Proc Fiscal" 
     SIZE 5.57 BY 1.5 TOOLTIP "Processar Documento Recebimento Fiscal".

DEFINE BUTTON btReprocessaFis 
     IMAGE-UP FILE "image/toolbar/im-chck2.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-chck2.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Proc F°sico" 
     SIZE 5.57 BY 1.5 TOOLTIP "Processar Documento Recebimento F°sico".

DEFINE BUTTON btValida 
     IMAGE-UP FILE "image/consultanfe.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-sea.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "ValidaNfe" 
     SIZE 5.57 BY 1.5 TOOLTIP "Valida NFe Sefaz".

DEFINE BUTTON btVaPara 
     IMAGE-UP FILE "image/toolbar/im-enter.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-enter.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "V† Para" 
     SIZE 5.57 BY 1.5 TOOLTIP "V† Para Chave de Acesso".

DEFINE VARIABLE c-msg-nfe AS CHARACTER FORMAT "X(20)":U INITIAL "Mensagens Erro" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE c-nfe AS CHARACTER FORMAT "X(40)":U INITIAL "Notas Fiscais Recebidas" 
      VIEW-AS TEXT 
     SIZE 18 BY .67 NO-UNDO.

DEFINE IMAGE IMAGE-21
     FILENAME "image/nfe.jpg":U
     STRETCH-TO-FIT
     SIZE 25.57 BY 17.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 17.83.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 133 BY 4.5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7 BY 17.21
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28.14 BY 17.83
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brMsg FOR 
      tt-nfe017 SCROLLING.

DEFINE QUERY brTable1 FOR 
      tt-nfe003 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brMsg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brMsg wWindow _STRUCTURED
  QUERY brMsg NO-LOCK DISPLAY
      tt-nfe017.dt-msg
      tt-nfe017.hr-msg WIDTH 7.56
      tt-nfe017.log-ativo FORMAT "Sim/N∆o":U
      tt-nfe017.cd-msg
      tt-nfe017.texto-msg FORMAT "x(500)":U WIDTH 100
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 131 BY 3.71
         FONT 1.

DEFINE BROWSE brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable1 wWindow _FREEFORM
  QUERY brTable1 NO-LOCK DISPLAY
      fn_situacao (tt-nfe003.idi-situacao) @ c-situacao COLUMN-LABEL "Situaá∆o Documento" FORMAT "x(30)":U
            WIDTH 20
      tt-nfe003.dt-emissao FORMAT "99/99/9999":U WIDTH 10
      fn_nome_abrev (tt-nfe003.cod-emitente) @ c-nome-abrev COLUMN-LABEL "Nome Abrev" FORMAT "x(12)":U
      tt-nfe003.nro-docto FORMAT "x(16)":U
      tt-nfe003.serie-docto FORMAT "x(5)":U
      tt-nfe003.cod-emitente COLUMN-LABEL "Emitente" FORMAT ">>>>>>>>9":U
            WIDTH 8
      tt-nfe003.cod-estabel FORMAT "x(3)":U WIDTH 5
      fn_sit_sefaz (tt-nfe003.sit-sefaz) @ c-desc-sit-sefaz COLUMN-LABEL "Situaá∆o SEFAZ" FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 17.21
         FONT 1 ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btSimula AT ROW 1.17 COL 17.43 WIDGET-ID 12
     btAtualiza AT ROW 1.17 COL 12 WIDGET-ID 8
     btQueryJoins AT ROW 1.13 COL 120.14 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 124.14 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 128.14 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 132.14 HELP
          "Ajuda"
     btParam AT ROW 1.13 COL 1.57 WIDGET-ID 2
     btSelecao AT ROW 1.13 COL 6.72 WIDGET-ID 4
     rtToolBar-2 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 136.43 BY 27.04
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage1
     btReprocessaFis AT ROW 8.13 COL 99 WIDGET-ID 34
     btAgregado AT ROW 15.88 COL 99 HELP
          "Notas Fiscais Referenciadas" WIDGET-ID 6
     btValida AT ROW 14.29 COL 99 WIDGET-ID 32
     btChave AT ROW 9.67 COL 99 WIDGET-ID 10
     btDocumento AT ROW 11.21 COL 99 WIDGET-ID 24
     btElimina AT ROW 12.75 COL 99 WIDGET-ID 30
     brTable1 AT ROW 1.79 COL 3 WIDGET-ID 200
     brMsg AT ROW 20 COL 3 WIDGET-ID 300
     btModifica AT ROW 5.04 COL 99 WIDGET-ID 4
     btReativa AT ROW 3.5 COL 99 WIDGET-ID 36
     btReprocessa AT ROW 6.58 COL 99 WIDGET-ID 8
     btVaPara AT ROW 1.96 COL 99 WIDGET-ID 2
     c-nfe AT ROW 1.13 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-msg-nfe AT ROW 19.25 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     RECT-10 AT ROW 1.42 COL 2 WIDGET-ID 12
     RECT-11 AT ROW 19.5 COL 2 WIDGET-ID 14
     RECT-13 AT ROW 1.79 COL 98.29 WIDGET-ID 42
     IMAGE-21 AT ROW 1.79 COL 108.14 WIDGET-ID 52
     RECT-14 AT ROW 1.42 COL 106.72 WIDGET-ID 54
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 4
         SIZE 134.86 BY 23.21
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-nfe003 T "?" NO-UNDO movnfe nfe003
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-nfe017 T "?" NO-UNDO movnfe nfe017
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 26.33
         WIDTH              = 136.72
         MAX-HEIGHT         = 36
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 36
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brTable1 btElimina fPage1 */
/* BROWSE-TAB brMsg brTable1 fPage1 */
/* SETTINGS FOR BUTTON btReprocessaFis IN FRAME fPage1
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brMsg
/* Query rebuild information for BROWSE brMsg
     _TblList          = "Temp-Tables.tt-nfe017"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt-nfe017.dt-msg|yes,Temp-Tables.tt-nfe017.hr-msg|yes,Temp-Tables.tt-nfe017.cd-msg|yes"
     _FldNameList[1]   = Temp-Tables.tt-nfe017.dt-msg
     _FldNameList[2]   > Temp-Tables.tt-nfe017.hr-msg
"tt-nfe017.hr-msg" ? ? "character" ? ? ? ? ? ? no ? no no "7.56" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt-nfe017.log-ativo
"tt-nfe017.log-ativo" ? "Sim/N∆o" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.tt-nfe017.cd-msg
     _FldNameList[5]   > Temp-Tables.tt-nfe017.texto-msg
"tt-nfe017.texto-msg" ? "x(500)" "character" ? ? ? ? ? ? no ? no no "100" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brMsg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable1
/* Query rebuild information for BROWSE brTable1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-nfe003 NO-LOCK
    BY tt-nfe003.cod-estabel
     BY tt-nfe003.cod-emitente
      BY tt-nfe003.serie-docto
       BY tt-nfe003.nro-docto INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt-nfe003.cod-estabel|yes,Temp-Tables.tt-nfe003.cod-emitente|yes,Temp-Tables.tt-nfe003.serie-docto|yes,Temp-Tables.tt-nfe003.nro-docto|yes"
     _Query            is OPENED
*/  /* BROWSE brTable1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brTable1
&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTable1 wWindow
ON ROW-DISPLAY OF brTable1 IN FRAME fPage1
DO:
    IF tt-nfe003.sit-sefaz <> 1 THEN
        ASSIGN tt-nfe003.cod-estabel:FGCOLOR  IN BROWSE brTable1 = 12
               tt-nfe003.cod-emitente:FGCOLOR IN BROWSE brTable1 = 12
               tt-nfe003.serie-docto:FGCOLOR  IN BROWSE brTable1 = 12
               tt-nfe003.nro-docto:FGCOLOR    IN BROWSE brTable1 = 12
               tt-nfe003.dt-emissao:FGCOLOR   IN BROWSE brTable1 = 12
               c-nome-abrev:FGCOLOR           IN BROWSE brTable1 = 12
               c-situacao:FGCOLOR             IN BROWSE brTable1 = 12 
               c-desc-sit-sefaz:FGCOLOR       IN BROWSE brTable1 = 12.
    ELSE
        ASSIGN tt-nfe003.cod-estabel:FGCOLOR  IN BROWSE brTable1 = ?
               tt-nfe003.cod-emitente:FGCOLOR IN BROWSE brTable1 = ?
               tt-nfe003.serie-docto:FGCOLOR  IN BROWSE brTable1 = ?
               tt-nfe003.nro-docto:FGCOLOR    IN BROWSE brTable1 = ?
               tt-nfe003.dt-emissao:FGCOLOR   IN BROWSE brTable1 = ?
               c-nome-abrev:FGCOLOR           IN BROWSE brTable1 = ?
               c-situacao:FGCOLOR             IN BROWSE brTable1 = ?
               c-desc-sit-sefaz:FGCOLOR       IN BROWSE brTable1 = ?.

    IF AVAIL tt-nfe003 AND tt-nfe003.idi-situacao = 7 THEN DO:
        RUN emptyRowObject       IN h-boes012.
        RUN setConstraintMonitor IN h-boes012 (INPUT tt-nfe003.ch-acesso-comp-nfe,
                                               INPUT 2 /* Documento Traduzido */,
                                               INPUT p-tg-ativa,
                                               INPUT p-tg-historico).
        RUN openQueryStatic      IN h-boes012 (INPUT "Monitor":U)  NO-ERROR.
        RUN getBatchRecords      IN h-boes012 (INPUT ?,
                                               INPUT NO,
                                               INPUT ?,
                                               OUTPUT iRowsReturned,
                                               OUTPUT TABLE tt-nfe017).

        IF iRowsReturned > 0 THEN
            ASSIGN tt-nfe003.cod-estabel:FGCOLOR  IN BROWSE brTable1 = 12
                   tt-nfe003.cod-emitente:FGCOLOR IN BROWSE brTable1 = 12
                   tt-nfe003.serie-docto:FGCOLOR  IN BROWSE brTable1 = 12
                   tt-nfe003.nro-docto:FGCOLOR    IN BROWSE brTable1 = 12
                   tt-nfe003.dt-emissao:FGCOLOR   IN BROWSE brTable1 = 12
                   c-nome-abrev:FGCOLOR           IN BROWSE brTable1 = 12
                   c-situacao:FGCOLOR             IN BROWSE brTable1 = 12 
                   c-desc-sit-sefaz:FGCOLOR       IN BROWSE brTable1 = 12.
        ELSE
            ASSIGN tt-nfe003.cod-estabel:FGCOLOR  IN BROWSE brTable1 = 2
                   tt-nfe003.cod-emitente:FGCOLOR IN BROWSE brTable1 = 2
                   tt-nfe003.serie-docto:FGCOLOR  IN BROWSE brTable1 = 2
                   tt-nfe003.nro-docto:FGCOLOR    IN BROWSE brTable1 = 2
                   tt-nfe003.dt-emissao:FGCOLOR   IN BROWSE brTable1 = 2
                   c-nome-abrev:FGCOLOR           IN BROWSE brTable1 = 2
                   c-situacao:FGCOLOR             IN BROWSE brTable1 = 2 
                   c-desc-sit-sefaz:FGCOLOR       IN BROWSE brTable1 = 2.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTable1 wWindow
ON VALUE-CHANGED OF brTable1 IN FRAME fPage1
DO:
    IF brTable1:NUM-SELECTED-ROWS = 1 THEN DO:
        IF  AVAIL tt-nfe003 THEN DO:
            CASE tt-nfe003.idi-situacao:
                WHEN 1 THEN DO:
                    ENABLE btDocumento
                           btElimina
                           WITH FRAME fPage1.

                    DISABLE btModifica
                            btAgregado
                            btReprocessa 
                            /*btReprocessaFis */
/*                             btDeParaItem */
                            WITH FRAME fPage1.
                END.

                WHEN 2 THEN DO:
                    
                    ENABLE btModifica
                        btAgregado
                           btReprocessa
                           /*btReprocessaFis*/
/*                            btDeParaItem */
                           btElimina WITH FRAME fPage1.

                    DISABLE btDocumento
                            WITH FRAME fPage1.
                END.

                WHEN 3 THEN DO:
                    DISABLE btModifica
                        btAgregado
                            btReprocessa
                            /*btReprocessaFis*/
/*                             btDeParaItem */
                            btDocumento
                            btElimina
                            WITH FRAME fPage1.
                END.

                WHEN 4 THEN DO:
                    
                    ENABLE btModifica
                        btAgregado
                           btReprocessa
                           /*btReprocessaFis*/
/*                            btDeParaItem */
                           btElimina WITH FRAME fPage1.

                    DISABLE btDocumento
                            WITH FRAME fPage1.
                END.

                WHEN 5 THEN DO:
                    DISABLE btModifica
                        btAgregado
                            btReprocessa
                            /*btReprocessaFis*/
/*                             btDeParaItem */
                            btDocumento
                            WITH FRAME fPage1.
                END.

                WHEN 6 THEN DO:
                    
                    ENABLE btModifica
                        btAgregado
                           btElimina
                           btReprocessa
                           /*btReprocessaFis*/
/*                            btDeParaItem */
                           WITH FRAME fPage1.

                    DISABLE btDocumento
                            WITH FRAME fPage1.
                END.

                WHEN 7 THEN DO:
                    ENABLE btModifica
                           btAgregado
                           btElimina
                           btReprocessa
                           /*btReprocessaFis*/
                           WITH FRAME fPage1.

                    DISABLE btDocumento
                            WITH FRAME fPage1.
                END.


            END CASE.

            IF tt-nfe003.sit-sefaz <> 1 THEN DO:

                ENABLE btDocumento
                       btElimina
                       WITH FRAME fPage1.

                DISABLE btModifica
                        btAgregado
                        btReprocessa 
                        /*btReprocessaFis */
/*                         btDeParaItem */
                        WITH FRAME fPage1.
                
            END .

            FIND FIRST nfe016 NO-LOCK 
                 WHERE nfe016.cod-emitente = tt-nfe003.cod-emitente 
                   AND nfe016.serie-docto  = tt-nfe003.serie-docto 
                   AND nfe016.nro-docto    = tt-nfe003.nro-docto NO-ERROR.
            IF AVAIL nfe016 THEN
                DISABLE btAgregado 
                        btElimina
                        btReprocessa
                        /*btReprocessaFis*/ WITH FRAME fPage1.

        END.
        RUN carregaBrowseMSG.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAgregado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAgregado wWindow
ON CHOOSE OF btAgregado IN FRAME fPage1 /* Nota Agregada */
DO:
    
    IF AVAIL tt-nfe003 THEN
        RUN esp/esnf004d.w (INPUT tt-nfe003.ch-acesso-comp-nfe).
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btAtualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtualiza wWindow
ON CHOOSE OF btAtualiza IN FRAME fpage0
DO: 

    RUN carregaBrowseNFe.
    RUN carregaBrowseMSG.

    APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
    APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btChave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChave wWindow
ON CHOOSE OF btChave IN FRAME fPage1 /* Chave Doc */
DO:
    IF brTable1:NUM-SELECTED-ROWS = 1 THEN DO:
        {&WINDOW-NAME}:SENSITIVE = FALSE.

        IF AVAIL tt-nfe003 THEN
            RUN esp/esnf003d.w (INPUT tt-nfe003.ch-acesso-comp-nfe).

        {&WINDOW-NAME}:SENSITIVE = TRUE.
    END.
    ELSE
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota foi selecionada! N∆o ser† poss°vel abrir o programa Consulta Chave Acesso.").
END.

/*
&Scoped-define SELF-NAME btDeParaItem
ON CHOOSE OF btDeParaItem IN FRAME fPage1 /* DeParaItem */
DO:
    DEFINE VARIABLE c-aux AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE de-indice AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE c-nr-lote AS CHARACTER   NO-UNDO.
    IF brTable1:NUM-SELECTED-ROWS = 1 THEN DO:
        IF AVAIL tt-nfe003 THEN DO:
    
                FOR EACH nfe017     
                   WHERE nfe017.cd-msg  = 1215 EXCLUSIVE-LOCK :
                    DELETE nfe017.
                END.
    
            
            FOR EACH nfe013 EXCLUSIVE-LOCK
               WHERE nfe013.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe 
               BREAK BY nfe013.it-codigo :
    
                FOR FIRST item-fornec 
                    WHERE item-fornec.item-do-forn = nfe013.it-codigo          
                      AND   item-fornec.cod-emitente = nfe003.cod-emitente NO-LOCK:
                END.
        
                IF AVAIL item-fornec THEN DO:
                  ASSIGN nfe013.it-codigo      = item-fornec.it-codigo
                         nfe013.un-comercial   = item-fornec.unid-med-for
                         de-indice             = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
                         nfe013.qtd-interna    = nfe013.qtd-comercial * de-indice
                         nfe013.preco-unit     = nfe013.preco-total / nfe013.qtd-interna .
                END.
                
                IF NOT AVAIL item-fornec THEN DO:
                    FOR FIRST ITEM 
                         WHERE ITEM.it-codigo = nfe013.it-codigo NO-LOCK :
        
                        ASSIGN nfe013.it-codigo      = item.it-codigo
                               nfe013.un-comercial   = item.un
                               de-indice             = 1
                               nfe013.qtd-interna    = nfe013.qtd-comercial * de-indice
                               nfe013.preco-unit     = nfe013.preco-total / nfe013.qtd-interna .
                    END.
                END.
                
                FOR FIRST ITEM 
                    WHERE ITEM.it-codigo = nfe013.it-codigo NO-LOCK:
                END.
                IF AVAIL ITEM THEN DO:
                     ASSIGN nfe013.class-fiscal = ITEM.class-fiscal
                            nfe013.un-interna   = ITEM.un.
                     
                     FIND FIRST item-uni-estab 
                          WHERE item-uni-estab.it-codigo   =  nfe013.it-codigo
                            AND item-uni-estab.cod-estabel =  nfe003.cod-estabel NO-LOCK NO-ERROR .
        
                     IF AVAIL item-uni-estab THEN DO:
                            ASSIGN nfe013.cod-depos  = item-uni-estab.deposito-pad 
                                   nfe013.cod-localiz = item-uni-estab.cod-localiz .
        
                             IF nfe013.cod-depos = "AL1" OR 
                                nfe013.cod-depos = "AL2" OR
                                nfe013.cod-depos = "A1K" OR
                                nfe013.cod-depos = "A2K" OR
                                nfe013.cod-depos = "CNG" OR
                                nfe013.cod-depos = "FER" OR
                                nfe013.cod-depos = "BNF" THEN DO:
                     
                                  ASSIGN nfe013.cod-depos = "RCB"
                                         nfe013.cod-localiz = "RCB".
                             END.
                     END.
        
        
                     FIND FIRST nfe008
                           WHERE nfe008.cod-emitente = nfe003.cod-emitente NO-LOCK NO-ERROR .
                     IF AVAIL nfe008 THEN DO:
                           ASSIGN nfe013.nat-operacao = nfe008.nat-operacao .
        
                           IF nfe008.serie <> ''  THEN
                                 ASSIGN nfe003.serie = nfe008.serie .
                     END.

                     /*IF item.tipo-con-est = 3 THEN DO:
                        FIND FIRST seq-num-lote WHERE SUBSTRING(STRING(seq-num-lote.lote),1,4) = STRING(YEAR(TODAY),"9999") EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAIL seq-num-lote THEN DO:
                            ASSIGN c-nr-lote                  =  " "
                                   c-nr-lote                 = string(NEXT-VALUE(seq-num-lote),"999999")
                                   nfe013.lote-serie   = string(year(today),"9999") + string(c-nr-lote,"999999")
                                   nfe013.dt-vali-lote = 12/31/2999 .
                        end.           
                        ELSE DO:
                            ASSIGN CURRENT-VALUE(seq-num-lote) = 0.
                            ASSIGN c-nr-lote                  =  " "
                                   c-nr-lote                 = string(NEXT-VALUE(seq-num-lote),"999999")
                                   nfe013.lote-serie   = string(year(today),"9999") + string(c-nr-lote,"999999")
                                   nfe013.dt-vali-lote = 12/31/2999 .
                        END.
                     END.*/

                END.
                ELSE DO:
                    CREATE nfe017.
                    ASSIGN nfe017.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                           nfe017.idi-orig-trad      = 2
                           nfe017.dt-msg             = TODAY
                           nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
                           nfe017.log-ativo          = YES
                           nfe017.cd-msg             = 1215
                           nfe017.texto-msg          = 'Item: ' + nfe013.it-codigo + ' Fornec: ' + STRING(tt-nfe003.cod-emitente) +
                                                               ' n∆o possui relac. itemxfornec CC0105 !'  
                           nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
                END.
            END.
            
            RUN carregaBrowseNFe.
            APPLY "VALUE-CHANGED":U TO BROWSE brTable1.
            
        end.   
    END.
    ELSE
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota foi selecionada! N∆o ser† poss°vel modificar situaá∆o do registro).").
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDocumento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDocumento wWindow
ON CHOOSE OF btDocumento IN FRAME fPage1 /* RE1001 */
DO:
    IF brTable1:NUM-SELECTED-ROWS = 1 THEN DO:
        IF NOT VALID-HANDLE(h-re1001) 
        OR h-re1001:TYPE      <> "PROCEDURE":U 
        OR h-re1001:FILE-NAME <> "rep/re1001.w":U THEN
            RUN rep/re1001.w PERSISTENT SET h-re1001.

        IF VALID-HANDLE(h-re1001) THEN DO:
            RUN initializeInterface IN h-re1001.

            IF AVAIL tt-nfe003 THEN DO:
                RUN retornaDocumento IN h-boes003 (INPUT  tt-nfe003.serie-docto,
                                                   INPUT  tt-nfe003.nro-docto,
                                                   INPUT  tt-nfe003.cod-emitente,
                                                   OUTPUT r-documento).
                IF r-documento <> ? THEN
                    RUN repositionRecord IN h-re1001 (INPUT r-documento).
            END.
        END.
    END.
    ELSE
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota foi selecionada! N∆o ser† poss°vel abrir o programa RE1001 (Manutená∆o de Documentos).").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btElimina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btElimina wWindow
ON CHOOSE OF btElimina IN FRAME fPage1 /* Elimina */
DO:
    IF brTable1:NUM-SELECTED-ROWS = 1 THEN DO:
    IF AVAIL tt-nfe003 THEN DO:

            FOR FIRST nfe003 NO-LOCK
                WHERE ROWID(nfe003) = tt-nfe003.r-rowid
                  AND nfe003.idi-orig-trad      = 2  :

                FIND FIRST docum-est 
                     WHERE docum-est.serie-docto  = nfe003.serie-docto
                       AND docum-est.nro-docto    = nfe003.nro-docto  
                       AND docum-est.cod-emitente = nfe003.cod-emitente   NO-LOCK NO-ERROR .
                IF AVAIL docum-est THEN DO:
                    
                    RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                       INPUT 17006,
                                       INPUT "Nota est† no Recebimento Fiscal!" + "~~" + 
                                             "N∆o Ç possivel eliminar o XML, primeiro deve ser eliminado o documento fiscal! SÇrie: " + nfe003.serie-docto + " Nro: " + string(nfe003.nro-docto) + " Emitente: " +  string(nfe003.cod-emitente)).
                    RETURN NO-APPLY.

                    /*RUN ze-erro.p (1,
                            'Nota Recebimento Fiscal' ,
                            'N∆o Ç possivel eliminar o XML, primeiro deve ser eliminado o documento fiscal! SÇrie: ' + nfe003.serie-docto + ' Nro: ' + string(nfe003.nro-docto) + ' Emitente: ' +  string(nfe003.cod-emitente)  ).
                    RETURN 'nok'.*/
                END.

                FIND FIRST doc-fisico 
                     WHERE doc-fisico.serie-docto  = nfe003.serie-docto
                       AND doc-fisico.nro-docto    = nfe003.nro-docto  
                       AND doc-fisico.cod-emitente = nfe003.cod-emitente 
                       AND doc-fisico.tipo-nota    = 1  NO-LOCK NO-ERROR .
                IF AVAIL doc-fisico THEN DO:
                    RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                       INPUT 17006,
                                       INPUT "Nota est† no Recebimento Fisicol!" + "~~" + 
                                             "N∆o Ç possivel eliminar o XML, primeiro deve ser eliminado o documento fisico! SÇrie: " + nfe003.serie-docto + " Nro: " + string(nfe003.nro-docto) + " Emitente: " +  string(nfe003.cod-emitente)).
                    RETURN NO-APPLY.

                    /*RUN ze-erro.p (1,
                            'Nota Recebimento F°sico' ,
                            'N∆o Ç possivel eliminar o XML, primeiro deve ser eliminado o documento fiscal! SÇrie: ' + nfe003.serie-docto + ' Nro: ' + string(nfe003.nro-docto) + ' Emitente: ' +  string(nfe003.cod-emitente)  ).
                    RETURN 'nok'.*/
                END.

                IF tt-nfe003.idi-situacao = 3 THEN DO:
                    RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                       INPUT 17006,
                                       INPUT "Nota selecionada j† atualizada no Recebimento!!" + "~~" + 
                                             "Nota selecionada j† atualizada no Recebimento. Eliminaá∆o n∆o permitida!").
                    RETURN NO-APPLY.
                END.

            END.

            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 28656,
                               INPUT "Eliminar XML! Deseja eliminar o registro selecionado?").
            
            /*RUN ze-erro.p (3,
                            'Eliminar XML' ,
                            'Deseja eliminar o registro selecionado? ' ).*/

            IF RETURN-VALUE = 'yes' THEN DO:
                FOR EACH nfe003 
                    WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                
                    FOR EACH nfe004 
                        WHERE nfe004.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe004.
                    END.
    
                    FOR EACH nfe005 
                        WHERE nfe005.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe005.
                    END.
    
                    FOR EACH nfe006 
                        WHERE nfe006.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe006.
                    END.
    
                    FOR EACH nfe009 
                        WHERE nfe009.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe009.
                    END.
    
                    FOR EACH nfe010 
                        WHERE nfe010.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe010.
                    END.
    
                    FOR EACH nfe011 
                        WHERE nfe011.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe011.
                    END.
    
                    FOR EACH nfe012 
                        WHERE nfe012.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe012.
                    END.
    
                    FOR EACH nfe013 
                        WHERE nfe013.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe013.
                    END.
    
                    FOR EACH nfe014 
                        WHERE nfe014.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe014.
                    END.
    
                    FOR EACH nfe016 
                        WHERE nfe016.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe016.
                    END.
    
                    FOR EACH nfe017 
                        WHERE nfe017.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe017.
                    END.
    
                    FOR EACH nfe021 
                        WHERE nfe021.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                        DELETE nfe021.
                    END.
    
                    DELETE nfe003.
    
                END.
            

                RUN carregaBrowseNFe.
                RUN carregaBrowseMSG.
    
                APPLY "VALUE-CHANGED":U TO BROWSE brTable1.
            END.
        END.
    END.
    ELSE
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota foi selecionada! N∆o ser† poss°vel modificar situaá∆o do registro).").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wWindow
ON CHOOSE OF btExit IN FRAME fpage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wWindow
ON CHOOSE OF btHelp IN FRAME fpage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btModifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btModifica wWindow
ON CHOOSE OF btModifica IN FRAME fPage1 /* Modifica */
DO:

    IF brTable1:NUM-SELECTED-ROWS IN FRAME fPage1 = 1 THEN DO:

        IF tt-nfe003.idi-situacao = 5 THEN DO:
             RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                INPUT 17006,
                                INPUT "Situaá∆o do Documento n∆o permite modificaá∆o!" + "~~" + 
                                      "Documento com situaá∆o de DANFE n∆o autorizado.Modificaá∆o n∆o permitida.").
             RETURN NO-APPLY.
    
        END.
        else do:
            {&WINDOW-NAME}:SENSITIVE = FALSE.
    
            RUN esp/esnf004.w (INPUT tt-nfe003.r-rowid).
    
            {&WINDOW-NAME}:SENSITIVE = TRUE.
    
            RUN verificaNaturezas IN h-boes003 (INPUT tt-nfe003.r-rowid,
                                                INPUT tt-nfe003.ch-acesso-comp-nfe).
            
            RUN carregaBrowseNFe.
            RUN carregaBrowseMSG.
    
            APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
            APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.
        end.
    END.
    ELSE DO:
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota foi selecionada! N∆o ser† poss°vel abrir o programa ESNF004 (Manutená∆o Notas).").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btParam wWindow
ON CHOOSE OF btParam IN FRAME fpage0 /* Parametros */
DO:
    {&WINDOW-NAME}:SENSITIVE = FALSE.

    RUN esp/esnf003a.w (INPUT-OUTPUT p-tg-digitada,
                        INPUT-OUTPUT p-tg-erro-neg,
                        INPUT-OUTPUT p-tg-atualizada,
                        INPUT-OUTPUT p-tg-eliminada,
                        INPUT-OUTPUT p-tg-danfe,
                        INPUT-OUTPUT p-tg-liberada,
                        INPUT-OUTPUT p-tg-conferido).

    {&WINDOW-NAME}:SENSITIVE = TRUE.

    APPLY 'choose' TO btAtualiza.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wWindow
ON CHOOSE OF btQueryJoins IN FRAME fpage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btReativa
ON CHOOSE OF btReativa IN FRAME fPage1 /* Reativar */
DO:
    IF  brTable1:NUM-SELECTED-ROWS IN FRAME fPage1 = 1 AND AVAIL tt-nfe003 THEN DO:
        
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 28656,
                           INPUT "Deseja reativar o XML ? O XML ser† reativado permitindo gerar novamente a Nota Fiscal!").

        /*RUN ze-erro.p (3,
                       'Deseja reativar o XML ?' ,
                        'O XML ser† reativado permitindo gerar novamente a Nota Fiscal !!!' ).*/

        IF RETURN-VALUE = 'yes' THEN DO:
            FOR FIRST nfe003 EXCLUSIVE-LOCK
                WHERE ROWID(nfe003) = tt-nfe003.r-rowid
                  AND nfe003.idi-orig-trad      = 2  :

                  FIND FIRST docum-est 
                       WHERE docum-est.serie-docto  = nfe003.serie-docto
                         AND docum-est.nro-docto    = nfe003.nro-docto  
                         AND docum-est.cod-emitente = nfe003.cod-emitente   NO-LOCK NO-ERROR .
                  IF AVAIL docum-est THEN DO:
                      
                      RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                         INPUT 17006,
                                         INPUT "Nota est† no Recebimento Fiscal!" + "~~" + 
                                               "Para reativar o XML deve ser eliminado o documento fiscal! SÇrie: " + nfe003.serie-docto + " Nro: " + string(nfe003.nro-docto) + " Emitente: " +  string(nfe003.cod-emitente)).
                      RETURN NO-APPLY.

                      /*RUN ze-erro.p (1,
                              'Nota Recebimento Fiscal' ,
                              'Para reativar o XML deve ser eliminado o documento fiscal! SÇrie: ' + nfe003.serie-docto + ' Nro: ' + string(nfe003.nro-docto) + ' Emitente: ' +  string(nfe003.cod-emitente)  ).
                      RETURN 'nok'.*/
                  END.
                  ELSE DO:
                      ASSIGN nfe003.idi-situacao = 6 .
                      FOR EACH nfe016 
                          WHERE nfe016.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                          DELETE nfe016.
                      END.
                      RUN carregaBrowseNFe.
                      RUN carregaBrowseMSG.
                  END.

                  FIND FIRST doc-fisico 
                       WHERE doc-fisico.serie-docto  = nfe003.serie-docto
                         AND doc-fisico.nro-docto    = nfe003.nro-docto  
                         AND doc-fisico.cod-emitente = nfe003.cod-emitente 
                         AND doc-fisico.tipo-nota    = 1  NO-LOCK NO-ERROR .
                  IF AVAIL doc-fisico THEN DO:

                      RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                         INPUT 17006,
                                         INPUT "Nota est† no Recebimento Fisico!" + "~~" + 
                                               "Para reativar o XML deve ser eliminado o documento fisico! SÇrie: " + nfe003.serie-docto + " Nro: " + string(nfe003.nro-docto) + " Emitente: " +  string(nfe003.cod-emitente)).
                      RETURN NO-APPLY.

                      /*RUN ze-erro.p (1,
                              'Nota Recebimento F°sico' ,
                              'Para reativar o XML deve ser eliminado o documento fiscal! SÇrie: ' + nfe003.serie-docto + ' Nro: ' + string(nfe003.nro-docto) + ' Emitente: ' +  string(nfe003.cod-emitente)  ).
                      RETURN 'nok'.*/
                  END.
                  ELSE DO:
                      ASSIGN nfe003.idi-situacao = 6 .
                      FOR EACH nfe016 
                          WHERE nfe016.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe EXCLUSIVE-LOCK:
                          DELETE nfe016.
                      END.
                      RUN carregaBrowseNFe.
                      RUN carregaBrowseMSG.
                  END.
                     
            END.

        END.
    END.
    ELSE DO:
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota Fiscal foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota Fiscal de Entrada foi selecionada! N∆o ser† poss°vel processar a Nota.").
    END.


     
    APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
    APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.
END.


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wWindow
ON CHOOSE OF btReportsJoins IN FRAME fpage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btReprocessa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReprocessa wWindow
ON CHOOSE OF btReprocessa IN FRAME fPage1 /* Proc Fiscal */
DO:
    IF  brTable1:NUM-SELECTED-ROWS IN FRAME fPage1 = 1 
    AND AVAIL tt-nfe003 THEN DO:

        FOR FIRST funcao 
            WHERE funcao.cd-funcao = "USA-MULT-NAT-RECEB"
              AND funcao.ativo NO-LOCK:    END.
        IF AVAIL funcao THEN
            RUN reprocessaNotaMultiplasNaturezas IN h-boes003 (INPUT  tt-nfe003.r-rowid,
                                                               INPUT YES,
                                                               OUTPUT l-reprocessado).
        ELSE
            RUN reprocessaNota IN h-boes003 (INPUT tt-nfe003.r-rowid,
                                             INPUT YES, /* Yes - Integra / No - Simula */
                                            OUTPUT l-reprocessado).

        IF l-reprocessado THEN DO:
            RUN carregaBrowseNFe.
            RUN carregaBrowseMSG.

            RUN utp/ut-msgs.p (INPUT "SHOW":U,
                               INPUT 15825,
                               INPUT "Nota Fiscal de Entrada Processada com Sucesso!!" + "~~" + 
                                     "A Nota Fiscal foi digitada com sucesso no recebimento. Para maiores informaá‰es, consultar programa RE1001.").
        END.
        ELSE DO:
            RUN carregaBrowseNFe.
            RUN carregaBrowseMSG.
            RUN utp/ut-msgs.p (INPUT "SHOW":U,
                               INPUT 17006,
                               INPUT "Nota Fiscal de Entrada n∆o pode ser Processada!!" + "~~" + 
                                     "A Nota Fiscal n∆o pode ser Processada!! Consultar as Mensagens da Nota no programa ESNF003.").
        END.
    END.
    ELSE DO:
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota Fiscal foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota Fiscal de Entrada foi selecionada! N∆o ser† poss°vel processar a Nota.").
    END.
     
    APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
    APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReprocessaFis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReprocessaFis wWindow
ON CHOOSE OF btReprocessaFis IN FRAME fPage1 /* Proc F°sico */
DO:
    IF brTable1:NUM-SELECTED-ROWS IN FRAME fPage1 = 1 
    AND AVAIL tt-nfe003 THEN DO:
        
        RUN PiProcessaNF (INPUT  tt-nfe003.r-rowid,
                          OUTPUT l-reprocessado).

        IF l-reprocessado THEN DO:
            RUN carregaBrowseNFe.
            RUN carregaBrowseMSG.

            RUN utp/ut-msgs.p (INPUT "SHOW":U,
                               INPUT 15825,
                               INPUT "Nota Fiscal de Entrada Processada com Sucesso!!" + "~~" + 
                                     "A Nota Fiscal foi digitada com sucesso no recebimento. Para maiores informaá‰es, consultar programa RE2001.").
        END.
        ELSE DO:
            RUN carregaBrowseMSG.
            RUN utp/ut-msgs.p (INPUT "SHOW":U,
                               INPUT 17006,
                               INPUT "Nota Fiscal de Entrada n∆o pode ser Processada!!" + "~~" + 
                                     "A Nota Fiscal n∆o pode ser Processada!! Consultar as Mensagens da Nota no programa ESNF003.").
        END.
    END.
    ELSE DO:
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota Fiscal foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota Fiscal de Entrada foi selecionada! N∆o ser† poss°vel processar a Nota.").
    END.

    APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
    APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btSelecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelecao wWindow
ON CHOOSE OF btSelecao IN FRAME fpage0 /* Seleá∆o */
DO:
    {&WINDOW-NAME}:SENSITIVE = FALSE.

    RUN esp/esnf003b.w (INPUT-OUTPUT p-cod-emitente-ini,
                        INPUT-OUTPUT p-cod-emitente-fim,
                        INPUT-OUTPUT p-cod-estabel-ini,
                        INPUT-OUTPUT p-cod-estabel-fim,
                        INPUT-OUTPUT p-serie-ini,
                        INPUT-OUTPUT p-serie-fim,
                        INPUT-OUTPUT p-nro-docto-ini,
                        INPUT-OUTPUT p-nro-docto-fim,
                        INPUT-OUTPUT p-dt-emissao-ini,
                        INPUT-OUTPUT p-dt-emissao-fim).

    {&WINDOW-NAME}:SENSITIVE = TRUE.

    RUN carregaBrowseNFe.
    RUN carregaBrowseMSG.

    APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
    APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimula wWindow
ON CHOOSE OF btSimula IN FRAME fpage0 /* Simula */
DO:
    RUN utp/ut-acomp.p PERSISTEN SET h-acomp.
    RUN pi-inicializar IN h-acomp (INPUT "Simulando integraá∆o e Conferindo Preáos").
    assign c-nome-arq = session:temp-directory + "esnf003.txt".
    OUTPUT TO value(c-nome-arq) no-convert.
    assign l-disp = no.
    FOR EACH tt-nfe003 NO-LOCK :
        RUN pi-acompanhar IN h-acomp (INPUT "Conferindo Documento " + tt-nfe003.nro-docto).

        FOR FIRST funcao WHERE funcao.cd-funcao = "USA-MULT-NAT-RECEB"
        AND funcao.ativo NO-LOCK:    END.
        IF funcao.ativo = YES THEN
            RUN reprocessaNotaMultiplasNaturezas IN h-boes003 (INPUT tt-nfe003.r-rowid,
                                                               INPUT NO, /* Yes - Integra / No - Simula */
                                                               OUTPUT l-reprocessado).                              
        ELSE
            RUN reprocessaNota IN h-boes003 (INPUT tt-nfe003.r-rowid,
                                             INPUT NO, /* Yes - Integra / No - Simula */
                                             OUTPUT l-reprocessado).                              

        FOR EACH nfe017
           WHERE nfe017.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
             AND nfe017.idi-orig-trad      = 2
             AND nfe017.log-ativo          = yes no-lock:
            ASSIGN c-msg-erro = string(nfe017.texto-msg)
                   l-disp = yes. 
            DISPLAY tt-nfe003.ch-acesso-comp-nfe
                    tt-nfe003.nro-docto
                    tt-nfe003.serie-docto
                    tt-nfe003.cod-emitente
                    tt-nfe003.cod-estabel
                    c-msg-erro SKIP WITH FRAME f-relatorio STREAM-IO.
                               DOWN WITH FRAME f-relatorio.
        END.
        FOR EACH nfe013
           WHERE nfe013.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
             AND nfe013.idi-orig-trad      = 2 
             AND nfe013.num-pedido <> 0 NO-LOCK,
           FIRST ordem-compra NO-LOCK
           WHERE ordem-compra.numero-ordem = nfe013.numero-ordem
             AND ordem-compra.num-pedido   = nfe013.num-pedido
             AND ordem-compra.it-codigo    = nfe013.it-codigo
             AND ordem-compra.preco-unit <> nfe013.preco-unit:
            RUN pi-acompanhar IN h-acomp (INPUT "Conferindo Preáo Item/Seq " + nfe013.it-codigo + "/" + STRING(nfe013.seq-item)).
            ASSIGN l-disp = yes.
            ASSIGN c-msg-erro = "O Item: " + ordem-compra.it-codigo + " - Seq: " + string(nfe013.seq-item) + " est† com preáo diferente do Pedido: " + string(nfe013.num-pedido) + " - Ordem: " + string(ordem-compra.numero-ordem).
            DISPLAY tt-nfe003.ch-acesso-comp-nfe
                    tt-nfe003.nro-docto
                    tt-nfe003.serie-docto
                    tt-nfe003.cod-emitente
                    tt-nfe003.cod-estabel
                    c-msg-erro skip WITH FRAME f-relatorio STREAM-IO.
                    DOWN WITH FRAME f-relatorio.   
        END.
    END.
    OUTPUT CLOSE.
    if l-disp = yes then do:        
        os-command no-wait value(c-nome-arq). 
    end.    
    
    RUN pi-finalizar IN h-acomp.
    
    RUN carregaBrowseNFe.
    RUN carregaBrowseMSG.
     
    APPLY "VALUE-CHANGED":U TO brTable1 IN FRAME fPage1.
    APPLY "VALUE-CHANGED":U TO brMSG    IN FRAME fPage1.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btValida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btValida wWindow
ON CHOOSE OF btValida IN FRAME fPage1 /* ValidaNfe */
DO:
    IF brTable1:NUM-SELECTED-ROWS = 1 THEN DO:
        {&WINDOW-NAME}:SENSITIVE = FALSE.

        IF AVAIL tt-nfe003 then do:
            DEF VAR h-axsep004              AS HANDLE  NO-UNDO.

            
            DEFINE VARIABLE l-spp-nfe AS LOGICAL     NO-UNDO.
            DEFINE VARIABLE l-funcao-tss-sincrono AS LOGICAL     NO-UNDO.
            
            
            
            assign l-spp-nfe = can-find(first funcao where 
                                              funcao.cd-funcao = "SPP-NFE":U and funcao.ativo = yes).
            
            ASSIGN l-funcao-tss-sincrono = CAN-FIND(FIRST funcao NO-LOCK
                                                    WHERE funcao.cd-funcao = "SPP-INTEG-TSS-SINCRONO":U
                                                      AND funcao.ativo).
            
            if l-spp-nfe and  (v_log_eai_habilit OR l-funcao-tss-sincrono) then do:

               run adapters/xml/ep2/axsep004.p persistent set h-axsep004 .
               
            end.
            find first estabelec NO-LOCK 
                 WHERE estabelec.cod-estabel = tt-nfe003.cod-estabel NO-ERROR.
            
            empty temp-table tt_log_erro.
            if valid-handle(h-axsep004) and (v_log_eai_habilit OR l-funcao-tss-sincrono) then do:
                 RUN pi-Recebe-Versao-Nfe IN h-axsep004 (INPUT TRIM(SUBSTRING(estabelec.char-1,173,10))). 

                 RUN pi-Recebe-Estab IN h-axsep004 (tt-nfe003.cod-estabel).
                 
                 run piUpsert in h-axsep004 (input "upd":U, 
                                             input tt-nfe003.ch-acesso-comp-nfe, /*Chave NFE*/
                                             output table tt_log_erro).
            end.
            
            for each tt_log_erro:

                FOR FIRST nfe003 EXCLUSIVE-LOCK
                    WHERE ROWID(nfe003) = tt-nfe003.r-rowid.

                    IF  TT_LOG_ERRO.TTV_DES_MSG_ERRO BEGINS "Chave NF-e V" THEN
                        ASSIGN nfe003.sit-sefaz = 1.
                    ELSE
                        ASSIGN nfe003.sit-sefaz = 2.
                END.

                RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                   INPUT 15825,
                                   INPUT " informaá∆o Sobre o  XML!!" + "~~" + ttv_des_msg_erro).
            end.
        end.

        {&WINDOW-NAME}:SENSITIVE = TRUE.
    END.
    ELSE
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT "Nenhuma Nota foi selecionada!!" + "~~" + 
                                 "Nenhuma Nota foi selecionada!").
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVaPara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVaPara wWindow
ON CHOOSE OF btVaPara IN FRAME fPage1 /* V† Para */
DO:
    {&WINDOW-NAME}:SENSITIVE = FALSE.

    RUN esp/esnf003c.w (INPUT  THIS-PROCEDURE,
                             OUTPUT p-chave-acesso).

    {&WINDOW-NAME}:SENSITIVE = TRUE.

    IF p-chave-acesso <> "" THEN DO:
        RUN emptyRowObject  IN h-boes003.
        RUN openQueryStatic IN h-boes003 (INPUT "Traduzido":U)  NO-ERROR.
        RUN goToKey         IN h-boes003 (INPUT p-chave-acesso, INPUT 2).
        
        IF RETURN-VALUE <> "NOK":U THEN DO:

            EMPTY TEMP-TABLE tt-nfe003.

            RUN getRecord IN h-boes003 (OUTPUT TABLE tt-nfe003).

            OPEN QUERY brTable1
                FOR EACH tt-nfe003 NO-LOCK
                    BY tt-nfe003.cod-estabel
                    BY tt-nfe003.cod-emitente
                    BY tt-nfe003.serie-docto
                    BY tt-nfe003.nro-docto.
        END.
        APPLY "VALUE-CHANGED":U TO BROWSE brTable1.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brMsg
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializaá∆o do programam ---*/

{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     RUN btb/btb910zz.p (INPUT c-seg-usuario,           */
/*                         INPUT NO).                     */
/*                                                        */
/*     /*--- caso a senha n∆o seja informada corretamente */
/*       o programa sera finalizado ---*/                 */
/*     IF RETURN-VALUE = "NOK":U THEN                     */
/*         RETURN "NOK":U.                                */

    IF AVAIL tt-nfe003 THEN DO:
        brTable1:SELECT-ROW(1) IN FRAME fPage1.
        APPLY "VALUE-CHANGED":U TO BROWSE brTable1.
    END.
    
    run pi-verific-cnpj.

/*     ASSIGN d-data = 02/13/2015.                                                                                            */
/*                                                                                                                            */
/*     IF d-data < TODAY THEN DO:                                                                                             */
/*         RUN utp/ut-msgs.p (INPUT "SHOW":U,                                                                                 */
/*                            INPUT 17006,                                                                                    */
/*                            INPUT "Data de Validade Expirou!!" + "~~" +                                                     */
/*                                  "A Data de Validade do Programa Expirou, Favor Entrar em Contato com o Administrador!."). */
/*                                                                                                                            */
/*         RETURN "NOK":U.                                                                                                    */
/*     END.  
                                                                                                                 */
    case v_cod_empres_usuar:
        when "3" then
            IMAGE-21:LOAD-IMAGE("image/nfe_koda.jpg":U) in frame fPage1 NO-ERROR. 
        when "4" then
            IMAGE-21:LOAD-IMAGE("image/nfe_ikf.jpg":U) in frame fPage1 NO-ERROR. 
        when "5" then
            IMAGE-21:LOAD-IMAGE("image/nfe_ynowa.jpg":U) in frame fPage1 NO-ERROR. 
        when "6" then
            IMAGE-21:LOAD-IMAGE("image/nfe_ynofer.jpg":U) in frame fPage1 NO-ERROR.
    end case.

    RETURN return-value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDestroyInterface wWindow 
PROCEDURE beforeDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF VALID-HANDLE(h-boes003) THEN DO:
        DELETE PROCEDURE h-boes003.
        ASSIGN h-boes003 = ?.
    END.

    IF VALID-HANDLE(h-boes011) THEN DO:
        DELETE PROCEDURE h-boes011.
        ASSIGN h-boes011 = ?.
    END.

    IF VALID-HANDLE(h-boes012) THEN DO:
        DELETE PROCEDURE h-boes012.
        ASSIGN h-boes012 = ?.
    END.

    IF VALID-HANDLE(h-re1001) THEN DO:
        DELETE PROCEDURE h-re1001.
        ASSIGN h-re1001 = ?.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeInitializeInterface wWindow 
PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN p-tg-digitada      = NO
           p-tg-erro-neg      = NO
           p-tg-atualizada    = NO
           p-tg-eliminada     = NO
           p-tg-danfe         = NO
           p-tg-liberada      = YES
           p-tg-conferido     = NO
           p-cod-emitente-ini = 0
           p-cod-emitente-fim = 999999999
           p-cod-estabel-ini  = ""
           p-cod-estabel-fim  = "ZZZ"
           p-serie-ini        = ""
           p-serie-fim        = "ZZZZZZ"
           p-nro-docto-ini    = ""
           p-nro-docto-fim    = "ZZZZZZZZZZZZZZZZ"
           p-dt-emissao-ini   = TODAY - 30
           p-dt-emissao-fim   = TODAY    
           p-tg-ativa         = YES
           p-tg-historico     = NO.

    RUN initializeDBOs.

    RUN carregaBrowseNFe.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaBrowseMSG wWindow 
PROCEDURE carregaBrowseMSG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-nfe017.
    
    IF AVAIL tt-nfe003 THEN DO:
        RUN emptyRowObject       IN h-boes012.
        RUN setConstraintMonitor IN h-boes012 (INPUT tt-nfe003.ch-acesso-comp-nfe,
                                               INPUT 2 /* Documento Traduzido */,
                                               INPUT p-tg-ativa,
                                               INPUT p-tg-historico).
        RUN openQueryStatic      IN h-boes012 (INPUT "Monitor":U)  NO-ERROR.
        RUN getBatchRecords      IN h-boes012 (INPUT ?,
                                               INPUT NO,
                                               INPUT ?,
                                               OUTPUT iRowsReturned,
                                               OUTPUT TABLE tt-nfe017).
    END.

    OPEN QUERY brMsg
        FOR EACH tt-nfe017 NO-LOCK
            BY tt-nfe017.dt-msg
            BY tt-nfe017.hr-msg
            BY tt-nfe017.cd-msg
            BY tt-nfe017.log-ativo.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaBrowseNFe wWindow 
PROCEDURE carregaBrowseNFe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-nfe003.

    RUN emptyRowObject       IN h-boes003.
    RUN setConstraintMonitor IN h-boes003 (INPUT p-tg-digitada,
                                           INPUT p-tg-erro-neg,
                                           INPUT p-tg-atualizada,
                                           INPUT p-tg-eliminada,
                                           INPUT p-tg-danfe,
                                           INPUT p-tg-liberada,
                                           INPUT p-tg-conferido,
                                           INPUT p-cod-emitente-ini,
                                           INPUT p-cod-emitente-fim,
                                           INPUT p-cod-estabel-ini,
                                           INPUT p-cod-estabel-fim, 
                                           INPUT p-serie-ini,
                                           INPUT p-serie-fim,
                                           INPUT p-nro-docto-ini,
                                           INPUT p-nro-docto-fim,
                                           INPUT p-dt-emissao-ini,
                                           INPUT p-dt-emissao-fim,
                                           INPUT 2 /* Documento Traduzido */ ).
    RUN openQueryStatic      IN h-boes003 (INPUT "Monitor":U)  NO-ERROR.
    RUN getBatchRecords      IN h-boes003 (INPUT ?,
                                           INPUT NO,
                                           INPUT ?,
                                           OUTPUT iRowsReturned,
                                           OUTPUT TABLE tt-nfe003).

   /*
   MESSAGE c-seg-usuario
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


    nfe018.usuario
    nfe018.cod-estabel
    nfe018.permissaoNFe */

   FOR EACH tt-nfe003 NO-LOCK :

       IF CAN-FIND( FIRST nfe018
                    WHERE nfe018.usuario      = c-seg-usuario
                      AND nfe018.permissaoNFe = YES ) THEN DO:

           IF NOT CAN-FIND( FIRST nfe018
                            WHERE nfe018.usuario      = c-seg-usuario
                              AND nfe018.cod-estabel  = tt-nfe003.cod-estabel
                              AND nfe018.permissaoNFe = YES ) THEN DO:
               DELETE tt-nfe003.
           END.

       END.

   END.



    OPEN QUERY brTable1
        FOR EACH tt-nfe003 NO-LOCK 
            BY tt-nfe003.cod-estabel
            BY tt-nfe003.cod-emitente
            BY tt-nfe003.serie-docto
            BY tt-nfe003.nro-docto .

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wWindow 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*:T--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE(h-boes003) 
    OR h-boes003:TYPE      <> "PROCEDURE":U 
    OR h-boes003:FILE-NAME <> "esbo/boes003.p":U THEN DO:
        RUN esbo/boes003.p PERSISTENT SET h-boes003.
    END.

    IF NOT VALID-HANDLE(h-boes011) 
    OR h-boes011:TYPE      <> "PROCEDURE":U 
    OR h-boes011:FILE-NAME <> "esbo/boes011.p":U THEN DO:
        RUN esbo/boes011.p PERSISTENT SET h-boes011.
    END.

    IF NOT VALID-HANDLE(h-boes012) 
    OR h-boes012:TYPE      <> "PROCEDURE":U 
    OR h-boes012:FILE-NAME <> "esbo/boes012.p":U THEN DO:
        RUN esbo/boes012.p PERSISTENT SET h-boes012.
    END.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-erro wWindow 
PROCEDURE pi-cria-erro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAM p-identif-segment LIKE tt-erro.identif-segment NO-UNDO.
    DEFINE INPUT PARAM p-cd-erro         LIKE tt-erro.cd-erro NO-UNDO.
    DEFINE INPUT PARAM p-desc-erro       LIKE tt-erro.desc-erro NO-UNDO.

    CREATE tt-erro.
    ASSIGN tt-erro.identif-segment = p-identif-segment
           tt-erro.cd-erro         = p-cd-erro
           tt-erro.desc-erro       = p-desc-erro.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-nota-fisico wWindow 
PROCEDURE pi-cria-nota-fisico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF OUTPUT PARAM TABLE FOR tt-erro. 

    DEFINE VARIABLE i-nro-docto AS INTEGER FORMAT ">>>>>>>>>>>9" NO-UNDO.
    DEF VAR de-tot-preco             AS DECIMAL          NO-UNDO.
    DEF VAR de-tot-peso              AS DECIMAL          NO-UNDO.

    IF NOT AVAIL param-estoq THEN
        FIND FIRST param-estoq NO-LOCK NO-ERROR.

    ASSIGN i-nro-docto = int(tt-doc-fisico.nro-docto).
    
    IF NOT CAN-FIND (FIRST emitente 
                     WHERE emitente.cod-emitente = tt-doc-fisico.cod-emitente ) THEN DO:
         RUN pi-cria-erro (INPUT '',
                           INPUT 2,
                           INPUT "Fornecedor " + STRING(tt-doc-fisico.cod-emitente) + " n∆o cadastrado.").
         /*RETURN "NOK":U.*/
    END.
    
    IF NOT CAN-FIND(FIRST estabelec 
                    WHERE estabelec.cod-estabel = tt-doc-fisico.cod-estabel) THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 2,
                          INPUT "Estabelecimento " + STRING(tt-doc-fisico.cod-estabel) + " n∆o cadastrado.").
        /*RETURN "NOK":U.*/
    END.
    
    IF NOT CAN-FIND(FIRST serie 
                    WHERE serie.serie = tt-doc-fisico.serie-docto) THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 2,
                          INPUT "SÇrie " + STRING(tt-doc-fisico.serie-docto) + " n∆o cadastrada.").
        /*RETURN "NOK":U.*/

    END.
    
    IF tt-doc-fisico.cod-emitente = 0 THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 6189,
                          INPUT "Emitente n∆o pode ser desconhecido (zero).").
        /*RETURN "NOK":U.*/

    END.
    
    IF tt-doc-fisico.nro-docto = "0000000":U 
    OR tt-doc-fisico.nro-docto = "":U THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 16686,
                          INPUT "N£mero de Documento deve ser diferente de zero").
        /*RETURN "NOK":U.*/

    END.
    
    IF i-nro-docto > 999999 THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 18247,
                          INPUT "N£mero de Documento maior que 999999").
        /*RETURN "NOK":U.*/

    END. 
    
    if tt-doc-fisico.dt-emissao < (TODAY - param-re.var-emis) THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 8824,
                          INPUT "Data de Emiss∆o " + STRING(tt-doc-fisico.dt-emissao) + " fora da variaá∆o permitida.").
        /*RETURN "NOK":U.*/

    END.
    
    IF tt-doc-fisico.dt-trans < (TODAY - param-re.var-atual) THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 8824,
                          INPUT "Data Transaá∆o " + STRING(tt-doc-fisico.dt-trans) + " fora da variaá∆o permitida.").
        /*RETURN "NOK":U.*/

    END.
    
    IF tt-doc-fisico.dt-trans > TODAY THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 1788,
                          INPUT "Data transaá∆o " + STRING(tt-doc-fisico.dt-trans) + " deve ser maior que a data corrente.").
        /*RETURN "NOK":U.*/

    END.
    
    if tt-doc-fisico.dt-trans  < tt-doc-fisico.dt-emissao THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 89,
                          INPUT "Data transaá∆o menor que a data de emiss∆o.").
        /*RETURN "NOK":U.*/

    END .
    
    IF param-estoq.tp-fech = 2 THEN DO:
         FIND estab-mat 
             WHERE estab-mat.cod-estabel = tt-doc-fisico.cod-estabel NO-LOCK NO-ERROR.
         IF AVAIL estab-mat AND tt-doc-fisico.dt-trans <= estab-mat.mensal-ate THEN DO:
              RUN pi-cria-erro (INPUT '',
                                INPUT 1586,
                                INPUT "Data de Transaá∆o deve ser maior que £ltimo mÇdio c†lculado.").
              /*RETURN "NOK":U.*/
    
         END.
         ELSE  
            IF param-estoq.log-1 AND tt-doc-fisico.dt-trans <= param-estoq.mensal-ate THEN DO:
                RUN pi-cria-erro (INPUT '',
                                  INPUT 1586,
                                  INPUT "Data de Transaá∆o deve ser maior que £ltimo mÇdio c†lculado.").
                /*RETURN "NOK":U.*/
            END.
    END.
    
    FIND FIRST doc-fisico
         WHERE doc-fisico.serie-docto  = tt-doc-fisico.serie-docto
           AND doc-fisico.nro-docto    = tt-doc-fisico.nro-docto
           AND doc-fisico.cod-emitente = tt-doc-fisico.cod-emitente
           AND doc-fisico.tipo-nota    = 1 NO-LOCK NO-ERROR.
    
    IF AVAIL doc-fisico THEN DO:
        RUN pi-cria-erro (INPUT '',
                          INPUT 1,
                          INPUT "Registro duplicado.").
        /*RETURN "NOK":U.*/
        
    END.
    ELSE DO:
        CREATE doc-fisico.
        ASSIGN doc-fisico.tipo-nota    = tt-doc-fisico.tipo-nota        
               doc-fisico.nro-docto    = tt-doc-fisico.nro-docto        
               doc-fisico.serie-docto  = tt-doc-fisico.serie-docto      
               doc-fisico.dt-emissao   = tt-doc-fisico.dt-emissao       
               doc-fisico.dt-trans     = tt-doc-fisico.dt-trans         
               doc-fisico.situacao     = tt-doc-fisico.situacao        
               doc-fisico.valor-outras = tt-doc-fisico.valor-outras     
               doc-fisico.despesa-nota = tt-doc-fisico.despesa-nota     
               doc-fisico.tot-desconto = tt-doc-fisico.tot-desconto     
               doc-fisico.cod-estabel  = tt-doc-fisico.cod-estabel      
               doc-fisico.tot-desconto = tt-doc-fisico.tot-desconto     
               doc-fisico.valor-frete  = tt-doc-fisico.valor-frete      
               doc-fisico.valor-seguro = tt-doc-fisico.valor-seguro     
               doc-fisico.valor-mercad = tt-doc-fisico.valor-mercad
               doc-fisico.cod-emitente = tt-doc-fisico.cod-emitente.
    
        ASSIGN de-tot-preco = 0
               de-tot-peso  = 0.

        FOR EACH tt-it-doc-fisico:

            Find FIRST item-uni-estab 
                WHERE item-uni-estab.it-codigo   = tt-it-doc-fisico.it-codigo
                  AND item-uni-estab.cod-estabel = tt-doc-fisico.cod-estabel No-lock No-error.
                
            Find estabelec
                Where estabelec.cod-estabel = tt-doc-fisico.cod-estabel No-lock No-error.
            
            FOR FIRST ordem-compra FIELDS(dep-almoxar)
                WHERE ordem-compra.numero-ordem = tt-it-doc-fisico.numero-ordem NO-LOCK: END.
            IF  AVAIL ordem-compra THEN
                ASSIGN c-depos-aux = ordem-compra.dep-almoxar.
            ELSE DO:
                IF  AVAIL item-uni-estab THEN
                    ASSIGN c-depos-aux = item-uni-estab.deposito-pad.
            END.
            
            ASSIGN de-tot-preco = de-tot-preco + tt-it-doc-fisico.preco-total[1]
                   de-tot-peso  = de-tot-peso  + tt-it-doc-fisico.peso-liquido.

            FIND FIRST rat-lote
                WHERE rat-lote.serie-docto  = tt-it-doc-fisico.serie-docto
                  AND rat-lote.nro-docto    = tt-it-doc-fisico.nro-docto
                  AND rat-lote.cod-emitente = tt-it-doc-fisico.cod-emitente
                  AND rat-lote.nat-operacao = ""
                  AND rat-lote.sequencia    = tt-it-doc-fisico.sequencia NO-LOCK NO-ERROR.
            IF NOT AVAIL rat-lote THEN DO:
                CREATE rat-lote.
                ASSIGN rat-lote.cod-emitente = tt-it-doc-fisico.cod-emitente
                       rat-lote.serie-docto  = tt-it-doc-fisico.serie-docto
                       rat-lote.nro-docto    = tt-it-doc-fisico.nro-docto
                       rat-lote.it-codigo    = tt-it-doc-fisico.it-codigo
                       rat-lote.nat-operacao = ""
                       rat-lote.tipo-nota    = tt-it-doc-fisico.tipo-nota
                       rat-lote.sequencia    = tt-it-doc-fisico.sequencia
                       rat-lote.quantidade   = tt-it-doc-fisico.quantidade
                       rat-lote.cod-depos    = c-depos-aux
                       rat-lote.lote         = tt-it-doc-fisico.lote
                       rat-lote.cod-localiz  = tt-it-doc-fisico.cod-localiz.
            END.
    
            FIND item 
                WHERE item.it-codigo = tt-it-doc-fisico.it-codigo NO-LOCK NO-ERROR.
            IF NOT AVAILABLE item THEN DO:
                RUN pi-cria-erro (INPUT '',
                                  INPUT 47,
                                  INPUT "Item " + STRING(tt-it-doc-fisico.it-codigo) + " n∆o cadastrado.").
/*                 RETURN "NOK":U. */
            END.
    
            /** Verifica Item x Referància - CD1506 **/
            IF AVAIL item AND item.tipo-con-est = 4 /* Referància */ 
            AND NOT CAN-FIND(FIRST ref-item 
                             WHERE ref-item.it-codigo = tt-it-doc-fisico.it-codigo 
                               AND ref-item.cod-refer = tt-it-doc-fisico.cod-refer) THEN DO:   
    
                RUN pi-cria-erro (INPUT '',
                                  INPUT 33928,
                                  INPUT "Referància n∆o cadastrado para o item " + STRING(tt-it-doc-fisico.it-codigo)).
                /*RETURN "NOK":U.*/
    
            END.
    
            FIND FIRST it-doc-fisico
                WHERE it-doc-fisico.serie-docto  = tt-it-doc-fisico.serie-docto
                  AND it-doc-fisico.nro-docto    = tt-it-doc-fisico.nro-docto
                  AND it-doc-fisico.cod-emitente = tt-it-doc-fisico.cod-emitente
                  AND it-doc-fisico.tipo-nota    = 1
                  AND it-doc-fisico.sequencia    = tt-it-doc-fisico.sequencia NO-LOCK NO-ERROR.
            IF AVAIL it-doc-fisico THEN DO:
                RUN pi-cria-erro (INPUT '',
                                  INPUT 424,
                                  INPUT "Sequencia " + STRING(tt-it-doc-fisico.sequencia) +  " do item " + STRING(tt-it-doc-fisico.it-codigo) + "ja cadastrada.").
                /*RETURN "NOK":U.*/
            END.
            ELSE DO:
                
                CREATE it-doc-fisico.
                ASSIGN it-doc-fisico.serie-docto  = tt-it-doc-fisico.serie-docto
                       it-doc-fisico.nro-docto    = tt-it-doc-fisico.nro-docto
                       it-doc-fisico.cod-emitente = tt-it-doc-fisico.cod-emitente
                       it-doc-fisico.tipo-nota    = tt-it-doc-fisico.tipo-nota
                       it-doc-fisico.sequencia    = tt-it-doc-fisico.sequencia
                       it-doc-fisico.num-pedido   = tt-it-doc-fisico.num-pedido
                       it-doc-fisico.it-codigo    = tt-it-doc-fisico.it-codigo
                       it-doc-fisico.un           = tt-it-doc-fisico.un
                       it-doc-fisico.quantidade   = tt-it-doc-fisico.quantidade
                       it-doc-fisico.qt-do-forn   = tt-it-doc-fisico.qt-do-forn
                       it-doc-fisico.preco-unit   = tt-it-doc-fisico.preco-unit[1]
                       it-doc-fisico.desconto     = tt-it-doc-fisico.desconto[1]
                       it-doc-fisico.preco-total  = tt-it-doc-fisico.preco-total[1]
                       it-doc-fisico.numero-ordem = tt-it-doc-fisico.numero-ordem
                       it-doc-fisico.parcela        = tt-it-doc-fisico.parcela
                       it-doc-fisico.conta-contabil = tt-it-doc-fisico.conta-contabil
                       it-doc-fisico.fifo-oc        = it-doc-fisico.fifo-oc /*YES*/
                       it-doc-fisico.nr-ord-prod    = tt-it-doc-fisico.nr-ord-prod  
                       it-doc-fisico.conta-contabil = tt-it-doc-fisico.conta-contabil .


            END.

            /* FIFO no Recebimento Fisico */
            IF it-doc-fisico.numero-ordem = 0 THEN
               RUN rep/re2001p.p (INPUT ROWID(it-doc-fisico)).
        END.
        
        ASSIGN doc-fisico.tot-peso     = de-tot-peso
               doc-fisico.valor-mercad = de-tot-preco.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piProcessaNF wWindow 
PROCEDURE piProcessaNF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pr-nota AS ROWID    NO-UNDO.
    DEFINE OUTPUT PARAMETER l-ok    AS LOGICAL  NO-UNDO.

    DEFINE VARIABLE i-seq-item       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE de-indice        AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-qtd-saldo     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE l-agregado       AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-conta-contabil AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-nfe003 FOR nfe003.
    DEFINE BUFFER bf-nfe013 FOR nfe013.
    
    EMPTY TEMP-TABLE tt-doc-fisico.
    EMPTY TEMP-TABLE tt-it-doc-fisico.
    EMPTY TEMP-TABLE tt-natureza.
    EMPTY TEMP-TABLE tt-erro.

    FOR FIRST param-re 
        WHERE param-re.usuario = c-seg-usuario NO-LOCK:
    END.
    IF NOT AVAIL param-re THEN
        RETURN "NOK":U.

    ASSIGN l-ok = YES.
    FOR FIRST bf-nfe003 NO-LOCK
        WHERE ROWID(bf-nfe003) = pr-nota:

        /** VERIFICA MAIS DE UMA NATUREZA DE OPERACAO **/
        FOR EACH nfe013 FIELDS (nat-operacao)
            WHERE nfe013.ch-acesso-comp-nfe = bf-nfe003.ch-acesso-comp-nfe
              AND nfe013.idi-orig-trad      = 2 NO-LOCK:

            IF NOT CAN-FIND(FIRST tt-natureza
                            WHERE tt-natureza.nat-operacao = nfe013.nat-operacao) THEN DO:
                CREATE tt-natureza.
                ASSIGN tt-natureza.nat-operacao = nfe013.nat-operacao.
            END.
        END.

        /** CRIA CONFORME CADA NATUREZA DE OPERACAO **/
        FOR EACH tt-natureza:
            
            /***** DOCUMENTO FISICO *****/
            IF NOT CAN-FIND (FIRST tt-doc-fisico
                     WHERE tt-doc-fisico.serie-docto  = bf-nfe003.serie-docto
                     AND   tt-doc-fisico.nro-docto    = bf-nfe003.nro-docto
                     AND   tt-doc-fisico.cod-emitente = bf-nfe003.cod-emitente
                     AND   tt-doc-fisico.tipo-nota    = 1) THEN DO:

                    FOR FIRST nfe003 NO-LOCK
                        WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe 
                        AND   nfe003.idi-orig-trad      = 2:
                    END.
        
                    CREATE tt-doc-fisico.
                    ASSIGN tt-doc-fisico.tipo-nota    = 1
                           tt-doc-fisico.nro-docto    = bf-nfe003.nro-docto
                           tt-doc-fisico.serie-docto  = bf-nfe003.serie-docto
                           tt-doc-fisico.dt-emissao   = bf-nfe003.dt-emissao  
                           tt-doc-fisico.dt-trans     = bf-nfe003.dt-transacao
                           tt-doc-fisico.situacao     = 1
                           tt-doc-fisico.valor-outras = 0
                           tt-doc-fisico.despesa-nota = 0
                           tt-doc-fisico.tot-peso     = 0
                           tt-doc-fisico.tot-desconto = 0
                           tt-doc-fisico.valor-mercad = 0
                           tt-doc-fisico.cod-emitente = bf-nfe003.cod-emitente
                           tt-doc-fisico.cod-estabel  = bf-nfe003.cod-estabel.
        
            END.
            
            /***** ITENS DO DOCUMENTO *****/
            ASSIGN i-seq-item = param-re.seq-item-um.
            
            FOR EACH nfe013 NO-LOCK
                WHERE nfe013.ch-acesso-comp-nfe = bf-nfe003.ch-acesso-comp-nfe
                  AND nfe013.idi-orig-trad      = 2:
            
                IF nfe013.nat-operacao <> tt-natureza.nat-operacao THEN
                    NEXT.

                /** DE-PARA DO C‡DIGO DO ITEM **/
            
                FOR FIRST bf-nfe013 FIELDS (it-codigo)
                    WHERE bf-nfe013.ch-acesso-comp-nfe = bf-nfe003.ch-acesso-comp-nfe
                      AND bf-nfe013.idi-orig-trad      = 1 
                      AND bf-nfe013.seq-item           = nfe013.seq-item NO-LOCK:

                    FOR FIRST item-fornec FIELDS (it-codigo)
                        WHERE item-fornec.item-do-forn = bf-nfe013.it-codigo          
                          AND item-fornec.cod-emitente = bf-nfe003.cod-emitente NO-LOCK:
                    END.
                END.
                /** DE-PARA DO C‡DIGO DO ITEM **/

                IF NOT AVAIL natur-oper
                OR (    AVAIL natur-oper
                    AND natur-oper.nat-operacao <> tt-natureza.nat-operacao) THEN
                    FOR FIRST natur-oper FIELDS (tp-oper-terc log-oper-triang nat-operacao)
                        WHERE natur-oper.nat-operacao = tt-natureza.nat-operacao NO-LOCK:
                    END.
                
                IF CAN-FIND(FIRST nfe014
                            WHERE nfe014.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                              AND nfe014.seq-item           = nfe013.seq-item          ) THEN DO:
                
                    FOR EACH nfe014 NO-LOCK
                        WHERE nfe014.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                          AND nfe014.seq-item           = nfe013.seq-item:
                
                        IF NOT CAN-FIND (FIRST tt-it-doc-fisico
                                         WHERE tt-it-doc-fisico.serie-docto  = bf-nfe003.serie-docto 
                                           AND tt-it-doc-fisico.nro-docto    = bf-nfe003.nro-docto   
                                           AND tt-it-doc-fisico.cod-emitente = bf-nfe003.cod-emitente
                                           AND tt-it-doc-fisico.tipo-nota    = 1
                                           AND tt-it-doc-fisico.sequencia    = i-seq-item) THEN DO:

                            FIND ITEM
                                WHERE ITEM.it-codigo = nfe013.it-codigo NO-LOCK NO-ERROR.
                            
                            CREATE tt-it-doc-fisico.
                            ASSIGN tt-it-doc-fisico.serie-docto    = bf-nfe003.serie-docto
                                   tt-it-doc-fisico.nro-docto      = bf-nfe003.nro-docto
                                   tt-it-doc-fisico.cod-emitente   = bf-nfe003.cod-emitente
                                   tt-it-doc-fisico.tipo-nota      = 1
                                   tt-it-doc-fisico.num-pedido     = nfe013.num-pedido
                                   tt-it-doc-fisico.quantidade     = nfe013.qtd-interna
                                   tt-it-doc-fisico.sequencia      = i-seq-item
                                   i-seq-item                      = i-seq-item + IF AVAIL param-re THEN param-re.inc-seq ELSE 1
                                   tt-it-doc-fisico.it-codigo      = nfe013.it-codigo
                                   tt-it-doc-fisico.qt-do-forn     = nfe013.qtd-comercial
                                   tt-it-doc-fisico.desconto[1]    = 0
                                   tt-it-doc-fisico.preco-unit[1]  = nfe013.preco-unit
                                   tt-it-doc-fisico.numero-ordem   = nfe013.numero-ordem
                                   /*tt-it-doc-fisico.parcela        = nfe013.parcela     */
                                   tt-it-doc-fisico.nr-ord-produ   = nfe013.nr-ord-produ
                                   tt-it-doc-fisico.un             = nfe013.un-comercial
                                   tt-it-doc-fisico.preco-total    = nfe013.preco-total
                                   tt-it-doc-fisico.log-1          = nfe013.log-fifo-oc
                                   tt-it-doc-fisico.fifo-oc        = YES /*nfe013.log-fifo-oc*/
                                   tt-it-doc-fisico.conta-contabil = IF AVAIL ITEM THEN ITEM.conta-aplicacao ELSE ""
                                   tt-it-doc-fisico.cod-refer      = nfe013.cod-refer     
                                   tt-it-doc-fisico.cod-depos      = nfe013.cod-depos     
                                   tt-it-doc-fisico.cod-localiz    = nfe013.cod-localiz   
                                   tt-it-doc-fisico.lote           = nfe013.lote          
                                   tt-it-doc-fisico.dt-vali-lote   = nfe013.dt-vali-lote.
                                   /********************/
                                   IF nfe013.numero-ordem <> 0 THEN DO:

                                       FIND FIRST ordem-compra 
                                            WHERE ordem-compra.numero-ordem = nfe013.numero-ordem NO-LOCK NO-ERROR .

                                       IF AVAIL ordem-compra THEN DO:
                                       
                                          assign tt-it-doc-fisico.nr-ord-prod = ordem-compra.ordem-servic. 

                                          if  ordem-compra.ordem-servic <> 0 then do:

                                               FIND FIRST ord-prod 
                                                    where ord-prod.nr-ord-prod = tt-it-doc-fisico.nr-ord-prod no-lock no-error.

                                               if  avail ord-prod then 
                                                   assign tt-it-doc-fisico.conta-contabil = ord-prod.conta-ordem.

                                          END.

                                       END.
                                       
                                       find first prazo-compra 
                                            where prazo-compra.numero-ordem = nfe013.numero-ordem 
                                              and prazo-compra.situacao     = 2 
                                              and ( prazo-compra.quant-saldo - prazo-compra.dec-1 ) > 0 use-index ordem-sit no-lock no-error.
                                              if not avail prazo-compra then 
                                                  find first prazo-compra 
                                                       where prazo-compra.numero-ordem = nfe013.numero-ordem 
                                                         and prazo-compra.situacao     = 2 no-lock no-error.
                                               if available prazo-compra THEN 
                                                  assign tt-it-doc-fisico.parcela = (prazo-compra.parcela).
                                   END.
                                   /*******************/

                        END.
                    END.
                END.
                ELSE DO:
                        IF NOT CAN-FIND (FIRST tt-it-doc-fisico
                                         WHERE tt-it-doc-fisico.serie-docto  = bf-nfe003.serie-docto 
                                           AND tt-it-doc-fisico.nro-docto    = bf-nfe003.nro-docto   
                                           AND tt-it-doc-fisico.cod-emitente = bf-nfe003.cod-emitente
                                           AND tt-it-doc-fisico.tipo-nota    = 1
                                           AND tt-it-doc-fisico.sequencia    = i-seq-item) THEN DO:

                            CREATE tt-it-doc-fisico.
                            ASSIGN tt-it-doc-fisico.serie-docto    = bf-nfe003.serie-docto
                                   tt-it-doc-fisico.nro-docto      = bf-nfe003.nro-docto
                                   tt-it-doc-fisico.cod-emitente   = bf-nfe003.cod-emitente
                                   tt-it-doc-fisico.tipo-nota      = 1
                                   tt-it-doc-fisico.num-pedido     = nfe013.num-pedido
                                   tt-it-doc-fisico.quantidade     = nfe013.qtd-interna
                                   tt-it-doc-fisico.sequencia      = i-seq-item
                                   i-seq-item                      = i-seq-item + IF AVAIL param-re THEN param-re.inc-seq ELSE 1
                                   tt-it-doc-fisico.it-codigo      = nfe013.it-codigo
                                   tt-it-doc-fisico.qt-do-forn     = nfe013.qtd-comercial
                                   tt-it-doc-fisico.desconto[1]    = 0
                                   tt-it-doc-fisico.preco-unit[1]  = nfe013.preco-unit
                                   tt-it-doc-fisico.numero-ordem   = nfe013.numero-ordem
                                   /*tt-it-doc-fisico.parcela        = nfe013.parcela */    
                                   tt-it-doc-fisico.nr-ord-produ   = nfe013.nr-ord-produ
                                   tt-it-doc-fisico.un             = nfe013.un-comercial
                                   tt-it-doc-fisico.preco-total    = nfe013.preco-total
                                   tt-it-doc-fisico.log-1          = nfe013.log-fifo-oc
                                   tt-it-doc-fisico.fifo-oc        = nfe013.log-fifo-oc
                                   tt-it-doc-fisico.conta-contabil = nfe013.conta-contabil
                                   tt-it-doc-fisico.cod-refer      = nfe013.cod-refer     
                                   tt-it-doc-fisico.cod-depos      = nfe013.cod-depos     
                                   tt-it-doc-fisico.cod-localiz    = nfe013.cod-localiz   
                                   tt-it-doc-fisico.lote           = nfe013.lote          
                                   tt-it-doc-fisico.dt-vali-lote   = nfe013.dt-vali-lote.
                                   /********************/
                                   IF nfe013.numero-ordem <> 0 THEN DO:

                                       FIND FIRST ordem-compra 
                                            WHERE ordem-compra.numero-ordem = nfe013.numero-ordem NO-LOCK NO-ERROR .

                                       IF AVAIL ordem-compra THEN DO:
                                       
                                          assign tt-it-doc-fisico.nr-ord-prod = ordem-compra.ordem-servic. 

                                          if  ordem-compra.ordem-servic <> 0 then do:

                                               FIND FIRST ord-prod 
                                                    where ord-prod.nr-ord-prod = tt-it-doc-fisico.nr-ord-prod no-lock no-error.

                                               if  avail ord-prod then 
                                                   assign tt-it-doc-fisico.conta-contabil = ord-prod.conta-ordem.

                                          END.

                                       END.
                                       
                                       find first prazo-compra 
                                            where prazo-compra.numero-ordem = nfe013.numero-ordem 
                                              and prazo-compra.situacao     = 2 
                                              and ( prazo-compra.quant-saldo - prazo-compra.dec-1 ) > 0 use-index ordem-sit no-lock no-error.
                                              if not avail prazo-compra then 
                                                  find first prazo-compra 
                                                       where prazo-compra.numero-ordem = nfe013.numero-ordem 
                                                         and prazo-compra.situacao     = 2 no-lock no-error.
                                               if available prazo-compra THEN 
                                                  assign tt-it-doc-fisico.parcela = (prazo-compra.parcela).
                                   END.
                                   /*******************/
                        END.
                END.
            END.
            /***** ITENS DO DOCUMENTO *****/
        END.

        DO TRANSACTION ON ERROR UNDO, LEAVE:

            RUN pi-cria-nota-fisico (OUTPUT TABLE tt-erro).

            IF CAN-FIND(FIRST tt-erro) THEN
                UNDO,LEAVE.

        END.
/*         IF RETURN-VALUE = "NOK":U THEN                                   */
/*             RETURN "NOK":U.                                              */
/*         MESSAGE "RETURN" RETURN-VALUE VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        /*IF l-ok THEN
            RUN rep/reapi316b.p (INPUT  "ADD",
                                 INPUT  TABLE tt-docum-est,
                                 INPUT  TABLE tt-rat-docum,
                                 INPUT  TABLE tt-item-doc-est,
                                 INPUT  TABLE tt-dupli-apagar,
                                 INPUT  TABLE tt-dupli-imp,
                                 OUTPUT TABLE tt-erro ) NO-ERROR.*/

        FOR EACH nfe017 EXCLUSIVE-LOCK
            WHERE nfe017.ch-acesso-comp-nfe = bf-nfe003.ch-acesso-comp-nfe
              AND nfe017.idi-orig-trad      = 2:
            ASSIGN nfe017.log-ativo = NO.
        END.

        IF CAN-FIND(FIRST tt-erro) THEN DO:
            FOR EACH tt-erro:
                CREATE nfe017.
                ASSIGN nfe017.ch-acesso-comp-nfe = bf-nfe003.ch-acesso-comp-nfe
                       nfe017.idi-orig-trad      = 2
                       nfe017.dt-msg             = TODAY
                       nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
                       nfe017.log-ativo          = YES
                       nfe017.cd-msg             = tt-erro.cd-erro
                       nfe017.texto-msg          = tt-erro.desc-erro
                       nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
            END.

            ASSIGN l-ok = NO.
        END.
    END.

    IF l-ok THEN DO:
        FOR FIRST bf-nfe003 FIELDS (idi-situacao)
            WHERE ROWID(bf-nfe003) = pr-nota EXCLUSIVE-LOCK:
            ASSIGN bf-nfe003.idi-situacao = 1 /* Digitada Recebimento Fisico*/ .
        END.

        RELEASE bf-nfe003.
    END.

    RETURN "OK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vaParaNFe wWindow 
PROCEDURE vaParaNFe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM p-chave-va-para LIKE nfe003.ch-acesso-comp-nfe NO-UNDO.
    DEF OUTPUT PARAM p-encontra        AS LOGICAL                         NO-UNDO.
    
    RUN openQueryStatic IN h-boes003 (INPUT "Traduzido":U)  NO-ERROR.
    RUN goToKey         IN h-boes003 (INPUT p-chave-va-para, INPUT 2).
    IF RETURN-VALUE <> "NOK":U THEN
        ASSIGN p-encontra = YES.
    ELSE 
        ASSIGN p-encontra = NO.

    RETURN "OK":U.

END PROCEDURE.

procedure pi-verific-cnpj:
    assign c-cnpj = "05935151000155,08089201000119,08089188000106,12252688000186".

    find first mgcad.empresa no-lock
         where empresa.ep-codigo = v_cod_empres_usuar no-error.
    if  availa empresa then do:
        do  i-cont = 1 to num-entries(c-cnpj):
            if  entry(i-cont,c-cnpj) = replace(replace(empresa.cgc,'.',''),'/','') then
                return 'ok'.
        end.
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Empresa sem licensa de uso.~~' +
                                 'Sua empresa n∆o tem licensa para utilizar esse produto. Favor entrar em contato com cristiano@szsolucoes.com.br.').
        return 'nok'.
    end.
    else do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Empresa sem licensa de uso.~~' +
                                 'Sua empresa n∆o tem licensa para utilizar esse produto. Favor entrar em contato com cristiano@szsolucoes.com.br.').
        return 'nok'.
    end.
    return 'ok'.
end procedure.

/* ************************  Function Implementations ***************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_nome_abrev wWindow 
FUNCTION fn_nome_abrev RETURNS CHARACTER
    ( p-cod-emitente AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RUN retornaNomeAbrev IN h-boes003 (INPUT  p-cod-emitente,
                                       OUTPUT c-nome-abrev).
        
    
    RETURN c-nome-abrev.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_situacao wWindow 
FUNCTION fn_situacao RETURNS CHARACTER
  ( p-idi-situacao AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    ASSIGN c-situacao = {esinc/i01es003.i 04 p-idi-situacao}.

  RETURN c-situacao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_sit_sefaz wWindow 
FUNCTION fn_sit_sefaz RETURNS CHARACTER
  ( INPUT i-sit-sefaz AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE c-desc  AS CHARACTER   NO-UNDO.

  IF i-sit-sefaz = 1 THEN
      ASSIGN c-desc = "Uso Autorizado".
  ELSE
      ASSIGN c-desc = "Uso N∆o Autorizado".

  RETURN c-desc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

