&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wMaintenance


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-nfe003 NO-UNDO LIKE nfe003
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-nfe003-trad NO-UNDO LIKE nfe003
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-nfe013 NO-UNDO LIKE nfe013
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
{include/i-prgvrs.i ESNF004 2.00.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF004
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    2

&GLOBAL-DEFINE FolderLabels   Itens,Detalhes

&GLOBAL-DEFINE First          NO
&GLOBAL-DEFINE Prev           NO
&GLOBAL-DEFINE Next           NO
&GLOBAL-DEFINE Last           NO
&GLOBAL-DEFINE GoTo           NO
&GLOBAL-DEFINE Search         NO

&GLOBAL-DEFINE Add            NO
&GLOBAL-DEFINE Copy           NO
&GLOBAL-DEFINE Update         YES
&GLOBAL-DEFINE Delete         NO
&GLOBAL-DEFINE Undo           YES
&GLOBAL-DEFINE Cancel         YES
&GLOBAL-DEFINE Save           YES

&GLOBAL-DEFINE ttTable        tt-nfe003-trad
&GLOBAL-DEFINE hDBOTable      h-boes003
&GLOBAL-DEFINE DBOTable       nfe003 

&GLOBAL-DEFINE ttTable2       tt-nfe013
&GLOBAL-DEFINE hDBOTable2     h-boes011
&GLOBAL-DEFINE DBOTable2      nfe013

&GLOBAL-DEFINE page0KeyFields tt-nfe003-trad.ch-acesso-comp-nfe
&GLOBAL-DEFINE page0Fields    
&GLOBAL-DEFINE page1Fields    cb-itens
&GLOBAL-DEFINE page2Fields    c-orig-1 c-trad-1 ~
                              tt-nfe003-trad.cod-emitente  ~
                              tt-nfe003-trad.serie-docto   ~
                              tt-nfe003-trad.cod-estabel   ~
                              tt-nfe003-trad.dt-transacao  ~
                              tt-nfe003-trad.nat-oper-comp ~
                              tt-nfe003-trad.lg-devol      ~
                              cb-observa-trad

&GLOBAL-DEFINE page1Widgets    brTable

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAM p-rw-doc-orig AS ROWID NO-UNDO.
                               
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-desc-item LIKE item.desc-item NO-UNDO.
DEFINE VARIABLE rSon          AS ROWID          NO-UNDO.

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

DEFINE VARIABLE {&hDBOTable}   AS HANDLE      NO-UNDO.
DEFINE VARIABLE {&hDBOTable2}  AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boes003-aux  AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-esnf004      AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boad098na    AS HANDLE      NO-UNDO.
DEFINE VARIABLE wh-pesquisa    AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boad107na    AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin245      AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boes005      AS HANDLE      NO-UNDO.
DEFINE VARIABLE iRowsReturned  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cod-observa  AS INTEGER     NO-UNDO.
define variable c-esp-docto    as character   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Maintenance
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brTable

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-nfe013

/* Definitions for BROWSE brTable                                       */
&Scoped-define FIELDS-IN-QUERY-brTable tt-nfe013.seq-item ~
fn_desc_item(tt-nfe013.it-codigo) @ c-desc-item tt-nfe013.it-codigo ~
tt-nfe013.qtd-comercial tt-nfe013.qtd-interna tt-nfe013.preco-total 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable 
&Scoped-define QUERY-STRING-brTable FOR EACH tt-nfe013 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable OPEN QUERY brTable FOR EACH tt-nfe013 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brTable tt-nfe013
&Scoped-define FIRST-TABLE-IN-QUERY-brTable tt-nfe013


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brTable}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-nfe003-trad.ch-acesso-comp-nfe 
&Scoped-define ENABLED-TABLES tt-nfe003-trad
&Scoped-define FIRST-ENABLED-TABLE tt-nfe003-trad
&Scoped-Define ENABLED-OBJECTS rtToolBar rtKeys btAdd btCopy btUpdate ~
btDelete btUndo btCancel btSave btQueryJoins btReportsJoins btExit btHelp 
&Scoped-Define DISPLAYED-FIELDS tt-nfe003-trad.ch-acesso-comp-nfe 
&Scoped-define DISPLAYED-TABLES tt-nfe003-trad
&Scoped-define FIRST-DISPLAYED-TABLE tt-nfe003-trad


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_desc_item wMaintenance 
FUNCTION fn_desc_item RETURNS CHARACTER
  ( p-it-codigo AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMaintenance AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miUpdate       LABEL "&Alterar"       ACCELERATOR "CTRL-A"
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
DEFINE BUTTON btAdd 
     IMAGE-UP FILE "image\im-add":U
     IMAGE-INSENSITIVE FILE "image\ii-add":U
     LABEL "Add" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btCancel 
     IMAGE-UP FILE "image\im-can":U
     IMAGE-INSENSITIVE FILE "image\im-can":U
     LABEL "Cancel" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btCopy 
     IMAGE-UP FILE "image\im-copy":U
     IMAGE-INSENSITIVE FILE "image\ii-copy":U
     LABEL "Copy" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "image\im-era":U
     IMAGE-INSENSITIVE FILE "image\ii-era":U
     LABEL "Delete" 
     SIZE 4 BY 1.25
     FONT 4.

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

DEFINE BUTTON btSave 
     IMAGE-UP FILE "image\im-sav":U
     IMAGE-INSENSITIVE FILE "image\ii-sav":U
     LABEL "Save" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btUndo 
     IMAGE-UP FILE "image\im-undo":U
     IMAGE-INSENSITIVE FILE "image\ii-undo":U
     LABEL "Undo" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btUpdate 
     IMAGE-UP FILE "image\im-mod":U
     IMAGE-INSENSITIVE FILE "image\ii-mod":U
     LABEL "Update" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 2.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 94 BY 1.5
     BGCOLOR 7 .

DEFINE BUTTON btModifica 
     LABEL "Modifica" 
     SIZE 10 BY 1.

DEFINE VARIABLE cb-itens AS CHARACTER FORMAT "X(256)":U INITIAL "Sequˆncia" 
     LABEL "Classifica por" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Sequˆncia","Item" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-observa-orig AS CHARACTER FORMAT "X(256)":U 
     LABEL "Observa‡Æo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ind£stria","Com‚rcio","Devolu‡Æo Cliente","Servi‡os" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-observa-trad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Observa‡Æo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ind£stria","Com‚rcio","Devolu‡Æo Cliente","Servi‡os" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-cnpj AS CHARACTER FORMAT "x(14)" 
     LABEL "CNPJ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE c-desc-nat-oper-comp AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-oper-comp AS CHARACTER FORMAT "x(6)" 
     LABEL "Nat Complementar" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE c-nome-abrev AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-est AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE c-nro-docto AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R17 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE c-orig-1 AS CHARACTER FORMAT "X(20)":U INITIAL "Dados Originais" 
      VIEW-AS TEXT 
     SIZE 11.29 BY .67 NO-UNDO.

DEFINE VARIABLE c-serie-docto AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE VARIABLE c-trad-1 AS CHARACTER FORMAT "X(20)":U INITIAL "Dados Traduzidos" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE da-dt-emissao AS DATE FORMAT "99/99/9999" 
     LABEL "Data EmissÆo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE da-dt-transacao AS DATE FORMAT "99/99/9999" 
     LABEL "Data Transa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE fi-nr-ord-produ AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Ordem Produ‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 10.58.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 10.58.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTable FOR 
      tt-nfe013 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable wMaintenance _STRUCTURED
  QUERY brTable NO-LOCK DISPLAY
      tt-nfe013.seq-item
      tt-nfe013.it-codigo WIDTH 15
      fn_desc_item(tt-nfe013.it-codigo) @ c-desc-item COLUMN-LABEL "Descri‡Æo" FORMAT "x(30)":U
      tt-nfe013.qtd-comercial
      tt-nfe013.qtd-interna
      tt-nfe013.preco-total
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 7.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btAdd AT ROW 1.13 COL 31 HELP
          "Inclui nova ocorrˆncia"
     btCopy AT ROW 1.13 COL 35 HELP
          "Cria uma c¢pia da ocorrˆncia corrente"
     btUpdate AT ROW 1.13 COL 39 HELP
          "Altera ocorrˆncia corrente"
     btDelete AT ROW 1.13 COL 43 HELP
          "Elimina ocorrˆncia corrente"
     btUndo AT ROW 1.13 COL 47 HELP
          "Desfaz altera‡äes"
     btCancel AT ROW 1.13 COL 51 HELP
          "Cancela altera‡äes"
     btSave AT ROW 1.13 COL 55 HELP
          "Confirma altera‡äes"
     btQueryJoins AT ROW 1.13 COL 78.43 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 82.43 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 86.43 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 90.43 HELP
          "Ajuda"
     tt-nfe003-trad.ch-acesso-comp-nfe AT ROW 3.17 COL 24.29 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 52 BY .88
     rtToolBar AT ROW 1 COL 1
     rtKeys AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94 BY 16.96
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage2
     c-cnpj AT ROW 2.38 COL 15 COLON-ALIGNED WIDGET-ID 24
     tt-nfe003-trad.cod-emitente AT ROW 2.38 COL 55.86 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     c-nome-abrev AT ROW 2.38 COL 66.14 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL WIDGET-ID 36
     c-serie-docto AT ROW 3.38 COL 15 COLON-ALIGNED WIDGET-ID 34
     tt-nfe003-trad.serie-docto AT ROW 3.38 COL 55.86 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
          BGCOLOR 14 
     c-nro-docto AT ROW 4.38 COL 15 COLON-ALIGNED WIDGET-ID 32
     tt-nfe003-trad.nro-docto AT ROW 4.38 COL 55.86 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     c-cod-estabel AT ROW 5.38 COL 15 COLON-ALIGNED WIDGET-ID 26
     tt-nfe003-trad.cod-estabel AT ROW 5.38 COL 55.86 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
          BGCOLOR 14 
     c-nome-est AT ROW 5.38 COL 64.29 COLON-ALIGNED HELP
          "Nome Estabelecimento" NO-LABEL WIDGET-ID 40
     da-dt-emissao AT ROW 6.38 COL 15 COLON-ALIGNED WIDGET-ID 28
     tt-nfe003-trad.dt-emissao AT ROW 6.38 COL 55.86 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     da-dt-transacao AT ROW 7.38 COL 15 COLON-ALIGNED HELP
          "Data Transa‡Æo" WIDGET-ID 48
     tt-nfe003-trad.dt-transacao AT ROW 7.38 COL 55.86 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          BGCOLOR 14 
     c-nat-oper-comp AT ROW 8.38 COL 15 COLON-ALIGNED HELP
          "Nat Complementar" WIDGET-ID 50
     tt-nfe003-trad.nat-oper-comp AT ROW 8.38 COL 55.86 COLON-ALIGNED WIDGET-ID 42
          LABEL "Nat Opera‡Æo"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
          BGCOLOR 14 
     c-desc-nat-oper-comp AT ROW 8.38 COL 64.29 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     cb-observa-orig AT ROW 9.38 COL 15 COLON-ALIGNED WIDGET-ID 52
     cb-observa-trad AT ROW 9.38 COL 55.86 COLON-ALIGNED WIDGET-ID 54
     fi-nr-ord-produ AT ROW 10.38 COL 55.86 COLON-ALIGNED WIDGET-ID 62
     tt-nfe003-trad.lg-devol AT ROW 11.25 COL 74 RIGHT-ALIGNED WIDGET-ID 60
          LABEL "Nota de devolu‡Æo"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .83
     c-orig-1 AT ROW 1.42 COL 2.86 NO-LABEL WIDGET-ID 18
     c-trad-1 AT ROW 1.42 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     RECT-14 AT ROW 1.67 COL 43 WIDGET-ID 14
     RECT-15 AT ROW 1.67 COL 1 WIDGET-ID 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 6.38
         SIZE 88.43 BY 11.38
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage1
     brTable AT ROW 2 COL 4 WIDGET-ID 200
     btModifica AT ROW 9.75 COL 4 WIDGET-ID 2
     cb-itens AT ROW 9.75 COL 66 COLON-ALIGNED WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 6.38
         SIZE 87.43 BY 10.62
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Maintenance
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-nfe003 T "?" NO-UNDO movnfe nfe003
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-nfe003-trad T "?" NO-UNDO movnfe nfe003
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-nfe013 T "?" NO-UNDO movnfe nfe013
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
         HEIGHT             = 16.96
         WIDTH              = 94
         MAX-HEIGHT         = 36
         MAX-WIDTH          = 205.72
         VIRTUAL-HEIGHT     = 36
         VIRTUAL-WIDTH      = 205.72
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
       FRAME fPage2:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brTable 1 fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-cnpj IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-cod-estabel IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-nat-oper-comp IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nat-oper-comp IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-abrev IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-est IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nro-docto IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-orig-1 IN FRAME fPage2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN c-serie-docto IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-trad-1 IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-observa-orig IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN da-dt-emissao IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN da-dt-transacao IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nfe003-trad.dt-emissao IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tt-nfe003-trad.lg-devol IN FRAME fPage2
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN tt-nfe003-trad.nat-oper-comp IN FRAME fPage2
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-nfe003-trad.nro-docto IN FRAME fPage2
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMaintenance)
THEN wMaintenance:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable
/* Query rebuild information for BROWSE brTable
     _TblList          = "Temp-Tables.tt-nfe013"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tt-nfe013.seq-item
     _FldNameList[2]   > "_<CALC>"
"fn_desc_item(tt-nfe013.it-codigo) @ c-desc-item" "Descri‡Æo" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt-nfe013.it-codigo
"tt-nfe013.it-codigo" ? ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.tt-nfe013.qtd-comercial
     _FldNameList[5]   = Temp-Tables.tt-nfe013.qtd-interna
     _FldNameList[6]   = Temp-Tables.tt-nfe013.preco-total
     _Query            is OPENED
*/  /* BROWSE brTable */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
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


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wMaintenance
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancel */
OR CHOOSE OF MENU-ITEM miCancel IN MENU mbMain DO:
    RUN cancelRecord IN THIS-PROCEDURE.

    ASSIGN fi-nr-ord-produ:SENSITIVE IN FRAME fPage2 = NO.
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


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wMaintenance
ON CHOOSE OF btHelp IN FRAME fpage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btModifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btModifica wMaintenance
ON CHOOSE OF btModifica IN FRAME fPage1 /* Modifica */
DO:
    IF brTable:NUM-SELECTED-ROWS IN FRAME fPage1 = 1 THEN DO:
        {&WINDOW-NAME}:SENSITIVE = NO.



        /*--- Seta vari vel rSon com o valor do rowid do registro corrente do browse filho ---*/
        IF AVAILABLE tt-nfe013 THEN
            ASSIGN rSon = tt-nfe013.r-Rowid.
        ELSE
            RETURN "NOK":U.


        RUN carregaCampos.

        
        
        /*--- Executa programa de inclusÆo de filho ---*/
        RUN esp/esnf004a.w PERSISTENT SET h-esnf004 (INPUT rSon,
                                                           INPUT tt-nfe003-trad.r-rowid,
                                                           INPUT "UPDATE":U,
                                                           INPUT THIS-PROCEDURE,
                                                           INPUT 1) .


        

        APPLY "ENTRY":U TO brTable IN FRAME fPage1.
        
        /* alterado por Valdir (tech264) novo m‚todo de teste do valid-handle */
        IF  VALID-HANDLE(h-esnf004) 
        AND h-esnf004:TYPE      = "PROCEDURE":U 
        AND h-esnf004:FILE-NAME = "esp/esnf004a.w":U THEN
            /*--- Inicializa programa de inclusÆo de filho ---*/
            RUN initializeInterface IN h-esnf004.

        {&WINDOW-NAME}:SENSITIVE = YES.

        

        
        
        /*RETURN NO-APPLY .*/

        

        

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wMaintenance
ON CHOOSE OF btQueryJoins IN FRAME fpage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wMaintenance
ON CHOOSE OF btReportsJoins IN FRAME fpage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wMaintenance
ON CHOOSE OF btSave IN FRAME fpage0 /* Save */
OR CHOOSE OF MENU-ITEM miSave IN MENU mbMain DO:
    RUN saveRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUndo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUndo wMaintenance
ON CHOOSE OF btUndo IN FRAME fpage0 /* Undo */
OR CHOOSE OF MENU-ITEM miUndo IN MENU mbMain DO:
    RUN undoRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate wMaintenance
ON CHOOSE OF btUpdate IN FRAME fpage0 /* Update */
OR CHOOSE OF MENU-ITEM miUpdate IN MENU mbMain DO:
    RUN updateRecord IN THIS-PROCEDURE.
    RUN setFolder    IN hFolder (INPUT 2).

    ASSIGN fi-nr-ord-produ:SENSITIVE IN FRAME fPage2 = YES.
    APPLY "ENTRY":U TO tt-nfe003-trad.cod-emitente IN FRAME fPage2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME cb-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-itens wMaintenance
ON VALUE-CHANGED OF cb-itens IN FRAME fPage1 /* Classifica por */
DO:
    RUN openQueryItem (INPUT INPUT FRAME fPage1 cb-itens).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME cb-observa-trad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-observa-trad wMaintenance
ON VALUE-CHANGED OF cb-observa-trad IN FRAME fPage2 /* Observa‡Æo */
DO:
    ASSIGN INPUT FRAME fPage2 cb-observa-trad.
    IF AVAIL tt-nfe003-trad THEN
        ASSIGN tt-nfe003-trad.cod-observa = {ininc/i03in090.i 06 cb-observa-trad}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe003-trad.cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.cod-emitente wMaintenance
ON F5 OF tt-nfe003-trad.cod-emitente IN FRAME fPage2 /* Emitente */
DO:
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=adzoom/z07ad098.w
                       &campo=tt-nfe003-trad.cod-emitente
                       &campozoom=cod-emitente
                       &frame="fPage2"
                       &campo2=c-nome-abrev
                       &campozoom2=nome-abrev
                       &frame2="fPage2"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.cod-emitente wMaintenance
ON LEAVE OF tt-nfe003-trad.cod-emitente IN FRAME fPage2 /* Emitente */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boad098na"
                              &KeyValue1="tt-nfe003-trad.cod-emitente:SCREEN-VALUE IN FRAME fPage2"
                              &FieldName1="nome-abrev"
                              &FieldScreen1="c-nome-abrev"
                              &Frame1="fPage2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.cod-emitente wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe003-trad.cod-emitente IN FRAME fPage2 /* Emitente */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe003-trad.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.cod-estabel wMaintenance
ON F5 OF tt-nfe003-trad.cod-estabel IN FRAME fPage2 /* Estabelecimento */
DO:
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                       &campo=tt-nfe003-trad.cod-estabel
                       &campozoom=cod-estabel
                       &frame="fPage2"
                       &campo2=c-nome-est
                       &campozoom2=nome
                       &frame2="fPage2"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.cod-estabel wMaintenance
ON LEAVE OF tt-nfe003-trad.cod-estabel IN FRAME fPage2 /* Estabelecimento */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boad107na"
                              &KeyValue1="tt-nfe003-trad.cod-estabel:SCREEN-VALUE IN FRAME fPage2"
                              &FieldName1="nome"
                              &FieldScreen1="c-nome-est"
                              &Frame1="fPage2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.cod-estabel wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe003-trad.cod-estabel IN FRAME fPage2 /* Estabelecimento */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-ord-produ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-ord-produ wMaintenance
ON MOUSE-SELECT-DBLCLICK OF fi-nr-ord-produ IN FRAME fPage2 /* Ordem Produ‡Æo */
OR "F5" OF fi-nr-ord-produ in frame fPage2 DO:

    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=inzoom/z01in271.w
                       &campo=fi-nr-ord-produ
                       &campozoom=nr-ord-produ
                       &frame=fPage2}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe003-trad.nat-oper-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.nat-oper-comp wMaintenance
ON F5 OF tt-nfe003-trad.nat-oper-comp IN FRAME fPage2 /* Nat Opera‡Æo */
DO:
    {method/zoomfields.i &ProgramZoom="inzoom/z04in245.w"
                         &FieldZoom1="nat-operacao"
                         &FieldScreen1="tt-nfe003-trad.nat-oper-comp"
                         &Frame1="fPage2"
                         &FieldZoom2="denominacao"
                         &FieldScreen2="c-desc-nat-oper-comp"
                         &Frame2="fPage2"                                                  
                         &EnableImplant="NO"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.nat-oper-comp wMaintenance
ON LEAVE OF tt-nfe003-trad.nat-oper-comp IN FRAME fPage2 /* Nat Opera‡Æo */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boin245"
                              &KeyValue1="tt-nfe003-trad.nat-oper-comp:SCREEN-VALUE IN FRAME fPage2"
                              &FieldName1="denominacao"
                              &FieldScreen1="c-desc-nat-oper-comp"
                              &Frame1="fPage2"}
                              
    run getCharField in h-boin245(input 'especie-doc',
                                  output c-esp-docto) no-error.
    if  c-esp-docto = 'NFD' then
        assign tt-nfe003-trad.lg-devol:checked in frame fPage2 = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.nat-oper-comp wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe003-trad.nat-oper-comp IN FRAME fPage2 /* Nat Opera‡Æo */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-nfe003-trad.serie-docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.serie-docto wMaintenance
ON F5 OF tt-nfe003-trad.serie-docto IN FRAME fPage2 /* S‚rie */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in407.w
                       &campo=tt-nfe003-trad.serie-docto
                       &campozoom=serie
                       &frame="fPage2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-nfe003-trad.serie-docto wMaintenance
ON MOUSE-SELECT-DBLCLICK OF tt-nfe003-trad.serie-docto IN FRAME fPage2 /* S‚rie */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brTable
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenance 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{maintenance/mainblock.i}

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
    /** CARREGA ORIGEM E TRADUZIDO - DOCUMENTO **/
/*     ASSIGN cb-itens:LIST-ITEMS   IN FRAME fPage1 = "Sequˆncia,Item" */
/*            cb-itens:SCREEN-VALUE IN FRAME fPage1 = "Sequˆncia". */
    
    RUN carregaCampos.

    IF AVAIL tt-nfe003-trad THEN DO:
        tt-nfe003-trad.cod-emitente:LOAD-MOUSE-POINTER("image/lupa.cur":U)  IN FRAME fPage2.
        tt-nfe003-trad.serie-docto:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage2.
        tt-nfe003-trad.cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage2.
        tt-nfe003-trad.nat-oper-comp:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage2.
        fi-nr-ord-produ:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage2.

        ASSIGN cb-observa-trad = {ininc/i03in090.i 04 tt-nfe003-trad.cod-observa}.

        DISP tt-nfe003-trad.ch-acesso-comp-nfe
            WITH FRAME fPage0.

        DISP tt-nfe003-trad.cod-emitente
             tt-nfe003-trad.serie-docto   
             tt-nfe003-trad.nro-docto
             tt-nfe003-trad.cod-estabel
             tt-nfe003-trad.dt-emissao
             tt-nfe003-trad.dt-transacao
             tt-nfe003-trad.nat-oper-comp
             tt-nfe003-trad.lg-devol
             cb-observa-trad
            WITH FRAME fPage2.

        APPLY "LEAVE":U TO tt-nfe003-trad.cod-emitente.
        APPLY "LEAVE":U TO tt-nfe003-trad.cod-estabel.
        APPLY "LEAVE":U TO tt-nfe003-trad.nat-oper-comp.
        
        ASSIGN tt-nfe003-trad.ch-acesso-comp-nfe:FORMAT IN FRAME fPage0 = "99.9999.99.999.999/9999-99-99-999-999.999.999-999.999.999-9".
    END.

    IF VALID-HANDLE(h-boes003-aux) THEN DO:
        RUN getCharField IN h-boes003-aux (INPUT "cnpj":U,          OUTPUT c-cnpj).
        RUN getCharField IN h-boes003-aux (INPUT "serie-docto":U,   OUTPUT c-serie-docto).
        RUN getCharField IN h-boes003-aux (INPUT "nro-docto":U,     OUTPUT c-nro-docto).
        RUN getCharField IN h-boes003-aux (INPUT "cod-estabel":U,   OUTPUT c-cod-estabel).
        RUN getCharField IN h-boes003-aux (INPUT "nat-oper-comp":U, OUTPUT c-nat-oper-comp).
        RUN getDateField IN h-boes003-aux (INPUT "dt-emissao":U,    OUTPUT da-dt-emissao).
        RUN getDateField IN h-boes003-aux (INPUT "dt-transacao":U,  OUTPUT da-dt-transacao).
        RUN getIntField  IN h-boes003-aux (INPUT "cod-observa":U,   OUTPUT i-cod-observa).

        ASSIGN cb-observa-orig = {ininc/i03in090.i 04 i-cod-observa}.
        
        DISP c-cnpj
             c-serie-docto
             c-nro-docto
             c-cod-estabel
             c-nat-oper-comp
             da-dt-emissao
             da-dt-transacao
             cb-observa-orig
            WITH FRAME fPage2.
    END.

    FIND FIRST nfe013 NO-LOCK 
         WHERE nfe013.ch-acesso-comp-nfe = tt-nfe003-trad.ch-acesso-comp-nfe NO-ERROR.
    IF AVAIL nfe013 THEN DO:
        ASSIGN fi-nr-ord-produ:SCREEN-VALUE IN FRAME fPage2 = string(nfe013.nr-ord-produ).
    END.
    
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
    IF AVAIL tt-nfe003-trad THEN DO:
        ENABLE btUpdate
            WITH FRAME fPage0.

        ENABLE brTable
            WITH FRAME fPage1.
        
    END.

    IF AVAIL tt-nfe013 THEN
        ENABLE btModifica
               cb-itens
            WITH FRAME fPage1.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterSaveFields wMaintenance 
PROCEDURE afterSaveFields :
IF fi-nr-ord-produ:INPUT-VALUE IN FRAME fPage2 <> 0 THEN DO:
        RUN utp/ut-msgs.p ("show",27100,"Confirma replica‡Æo da Ordem de Produ‡Æo informada para todos os itens?").
        IF RETURN-VALUE = "yes" THEN DO TRANS:
            FOR EACH nfe013 EXCLUSIVE-LOCK 
               WHERE nfe013.ch-acesso-comp-nfe = tt-nfe003-trad.ch-acesso-comp-nfe .
            
                ASSIGN nfe013.nr-ord-produ = fi-nr-ord-produ:INPUT-VALUE IN FRAME fPage2.
            END.

        END.

    END.

    ASSIGN fi-nr-ord-produ:SENSITIVE IN FRAME fPage2 = NO.

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
    
    IF VALID-HANDLE(h-boes003-aux) THEN DO:
        DELETE PROCEDURE h-boes003-aux.
        ASSIGN h-boes003-aux = ?.
    END.

    IF VALID-HANDLE(h-boes005) THEN DO:
        DELETE PROCEDURE h-boes005.
        ASSIGN h-boes005 = ?.
    END.
    
    /** Estabelecimento **/
    IF VALID-HANDLE(h-boad107na) THEN DO:
        DELETE PROCEDURE h-boad107na.
        ASSIGN h-boad107na = ?.
    END.
        
    /** Fornecededor **/
    IF VALID-HANDLE(h-boad098na) THEN DO:
        DELETE PROCEDURE h-boad098na.
        ASSIGN h-boad098na = ?.
    END.
    
    /** Fornecededor **/
    IF VALID-HANDLE(h-boin245) THEN DO:
        DELETE PROCEDURE h-boin245.
        ASSIGN h-boin245 = ?.
    END.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaCampos wMaintenance 
PROCEDURE carregaCampos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*** CARREGA CAMPOS TRADUZIDOS ***/
    EMPTY TEMP-TABLE tt-nfe003-trad.

    RUN emptyRowObject   IN {&hDBOTable}.
    RUN openQueryStatic  IN {&hDBOTable} (INPUT "Main":U)  NO-ERROR.
    RUN repositionRecord IN {&hDBOTable} (INPUT p-rw-doc-orig).
    RUN getRecord        IN {&hDBOTable} (OUTPUT TABLE tt-nfe003-trad).

    FIND FIRST tt-nfe003-trad NO-ERROR.
    /*********************************/    

    EMPTY TEMP-TABLE tt-nfe003.

    IF AVAIL tt-nfe003-trad THEN DO:
        /*** CARREGA CAMPOS ORIGEM ***/
        RUN emptyRowObject  IN h-boes003-aux.
        RUN openQueryStatic IN h-boes003-aux (INPUT "Main":U)  NO-ERROR.
        RUN goToKey         IN h-boes003-aux (INPUT tt-nfe003-trad.ch-acesso-comp-nfe, 
                                              INPUT 1).
        RUN getRecord       IN h-boes003-aux (OUTPUT TABLE tt-nfe003).
    
        FIND FIRST tt-nfe003 NO-ERROR.
        /*********************************/    

        /*** CARREGA ITENS ***/
        RUN carregaItens.
        /*********************/    
    END.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaItens wMaintenance 
PROCEDURE carregaItens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAIL tt-nfe003-trad THEN DO:
    
        EMPTY TEMP-TABLE tt-nfe013.

        RUN emptyRowObject       IN {&hDBOTable2}.
        RUN setConstraintMonitor IN {&hDBOTable2} (INPUT tt-nfe003-trad.ch-acesso-comp-nfe, 
                                                   INPUT 2).
        RUN openQueryStatic      IN {&hDBOTable2} (INPUT "Monitor":U)  NO-ERROR.
        RUN getBatchRecords      IN {&hDBOTable2} (INPUT ?,
                                                   INPUT NO,
                                                   INPUT ?,
                                                   OUTPUT iRowsReturned,
                                                   OUTPUT TABLE tt-nfe013).

        APPLY "VALUE-CHANGED":U TO cb-itens IN FRAME fPage1.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableScrollbar wMaintenance 
PROCEDURE enableScrollbar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDBOSonHandle wMaintenance 
PROCEDURE getDBOSonHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retorna handle do DBO Filho
  Parameters:  recebe nœmero da pÿgina
               retorna handle do DBO
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE  INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER pDBOHandle  AS HANDLE  NO-UNDO.
    
    CASE pPageNumber:
        &IF "{&hDBOTable2}":U <> "":U &THEN
            WHEN 1 THEN ASSIGN pDBOHandle = {&hDBOTable2}.
        &ENDIF
        
    END CASE.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getParentRecord wMaintenance 
PROCEDURE getParentRecord :
/*------------------------------------------------------------------------------
  Purpose:     Retorna temp-table {&ttParent} com o registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR {&ttTable}.
    
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
    
    /*:T--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable}) 
    OR {&hDBOTable}:TYPE      <> "PROCEDURE":U 
    OR {&hDBOTable}:FILE-NAME <> "esbo/boes003.p":U THEN DO:
        RUN esbo/boes003.p PERSISTENT SET {&hDBOTable}.
    END.
    RUN openQueryStatic IN {&hDBOTable2} (INPUT "Main":U)  NO-ERROR.
    
    IF NOT VALID-HANDLE({&hDBOTable2}) 
    OR {&hDBOTable2}:TYPE      <> "PROCEDURE":U 
    OR {&hDBOTable2}:FILE-NAME <> "esbo/boes011.p":U THEN DO:
        RUN esbo/boes011.p PERSISTENT SET {&hDBOTable2}.
    END.
    RUN openQueryStatic      IN {&hDBOTable2} (INPUT "Main":U)  NO-ERROR.

    IF NOT VALID-HANDLE(h-boes003-aux) 
    OR h-boes003-aux:TYPE      <> "PROCEDURE":U 
    OR h-boes003-aux:FILE-NAME <> "esbo/boes003.p":U THEN DO:
        RUN esbo/boes003.p PERSISTENT SET h-boes003-aux.
    END.

    IF NOT VALID-HANDLE(h-boes005) 
    OR h-boes005:TYPE      <> "PROCEDURE":U 
    OR h-boes005:FILE-NAME <> "esbo/boes005.p":U THEN DO:
        RUN esbo/boes005.p PERSISTENT SET h-boes005.
    END.
    
    /** Estabelecimento **/
    IF NOT VALID-HANDLE(h-boad107na) 
    OR h-boad107na:TYPE      <> "PROCEDURE":U 
    OR h-boad107na:FILE-NAME <> "adbo/boad107na.p":U THEN DO:
        RUN adbo/boad107na.p PERSISTENT SET h-boad107na.
    END.
    RUN openQueryStatic IN h-boad107na (INPUT "Main":U) NO-ERROR.

    /** Fornecededor **/
    IF NOT VALID-HANDLE(h-boad098na) 
    OR h-boad098na:TYPE      <> "PROCEDURE":U 
    OR h-boad098na:FILE-NAME <> "adbo/boad098na.p":U THEN DO:
        RUN adbo/boad098na.p PERSISTENT SET h-boad098na.
    END.
    RUN openQueryStatic IN h-boad098na (INPUT "Main":U) NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryItem wMaintenance 
PROCEDURE openQueryItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-classif-item AS CHAR NO-UNDO.
    
    CASE p-classif-item:
        WHEN "Sequˆncia" THEN DO:
            OPEN QUERY brTable
                FOR EACH tt-nfe013 NO-LOCK
                    BY tt-nfe013.seq-item.
        END.

        WHEN "Item" THEN DO:
            OPEN QUERY brTable
                FOR EACH tt-nfe013 NO-LOCK
                    BY tt-nfe013.it-codigo.
        END.
    END CASE.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionRecordSon wMaintenance 
PROCEDURE repositionRecordSon :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona DBO filho atrav?s de um rowid
  Parameters:  recebe rowid
               recebe nœmero da pÿgina
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pRowid      AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    RUN carregaItens.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_desc_item wMaintenance 
FUNCTION fn_desc_item RETURNS CHARACTER
  ( p-it-codigo AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RUN retornaDescItem IN ({&hDBOTable2}) (INPUT p-it-codigo, OUTPUT c-desc-item).
    
    RETURN c-desc-item.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

