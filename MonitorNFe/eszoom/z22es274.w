&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          movind           PROGRESS
*/
&Scoped-define WINDOW-NAME wZoom


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-ordem-compra1 NO-UNDO LIKE ordem-compra
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-ordem-compra2 NO-UNDO LIKE ordem-compra
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-ordem-compra3 NO-UNDO LIKE ordem-compra
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-ordem-compra4 NO-UNDO LIKE ordem-compra
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wZoom 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i Z22ES274 2.00.00.000}  /*** 010000 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i z22in274 MUT}
&ENDIF

/* Template Liberado */

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program           Z22ES274
&GLOBAL-DEFINE Version           2.00.00.000

&GLOBAL-DEFINE Folder            YES
&GLOBAL-DEFINE InitialPage       1

&GLOBAL-DEFINE FolderLabels      Pedido,Ordem,Item,Contrato

&GLOBAL-DEFINE Range             YES
&GLOBAL-DEFINE FieldNames        
&GLOBAL-DEFINE FieldsRangePage1  movind.ordem-compra.num-pedido,movind.ordem-compra.cod-estabel
&GLOBAL-DEFINE FieldsRangePage2  movind.ordem-compra.numero-ordem,movind.ordem-compra.cod-estabel
&GLOBAL-DEFINE FieldsRangePage3  movind.ordem-compra.it-codigo,movind.ordem-compra.cod-estabel
&GLOBAL-DEFINE FieldsRangePage4  movind.ordem-compra.nr-contrato,movind.ordem-compra.cod-estabel
&GLOBAL-DEFINE FieldsAnyKeyPage1 NO,NO
&GLOBAL-DEFINE FieldsAnyKeyPage2 NO,NO
&GLOBAL-DEFINE FieldsAnyKeyPage3 NO,NO
&GLOBAL-DEFINE FieldsAnyKeyPage4 NO,NO

&GLOBAL-DEFINE ttTable1          tt-ordem-compra1
&GLOBAL-DEFINE hDBOTable1        h-boin274q01-a
&GLOBAL-DEFINE DBOTable1         ordem-compra

&GLOBAL-DEFINE ttTable2          tt-ordem-compra2
&GLOBAL-DEFINE hDBOTable2        h-boin274q01-b
&GLOBAL-DEFINE DBOTable2         ordem-compra

&GLOBAL-DEFINE ttTable3          tt-ordem-compra3
&GLOBAL-DEFINE hDBOTable3        h-boin274q01-c
&GLOBAL-DEFINE DBOTable3         ordem-compra

&GLOBAL-DEFINE ttTable4          tt-ordem-compra4
&GLOBAL-DEFINE hDBOTable4        h-boin274q01-d
&GLOBAL-DEFINE DBOTable4         ordem-compra

&GLOBAL-DEFINE page1Browse      brTable1
&GLOBAL-DEFINE page2Browse      brTable2
&GLOBAL-DEFINE page3Browse      brTable3
&GLOBAL-DEFINE page4Browse      brTable4

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var de-qtd-saldo      like prazo-compra.quant-saldo                 no-undo.
def var c-data-entrega    as char format "X(12)" label "Entrega"        no-undo.
def var c-desc-item       as char format "X(60)" label "Descri¯Êo Item" no-undo.
def var cItCodigo         as char                                       no-undo.
def var cSegUsuario       as char                                       no-undo.
def var codNumPedidoIni   as int                                        no-undo.
def var codNumPedidoFim   as int                                        no-undo.
DEF VAR r-item            AS ROWID                                      NO-UNDO.
DEF VAR d-dt-emissao      AS DATE                                       NO-UNDO.

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable1} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTable2} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTable3} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTable4} AS HANDLE NO-UNDO.
DEFINE VARIABLE h-boes011     AS HANDLE NO-UNDO.

/* Include de versÆo */
{cdp/cdcfgmat.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Zoom
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brTable1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ordem-compra1 tt-ordem-compra2 ~
tt-ordem-compra3 tt-ordem-compra4

/* Definitions for BROWSE brTable1                                      */
&Scoped-define FIELDS-IN-QUERY-brTable1 tt-ordem-compra1.num-pedido ~
tt-ordem-compra1.numero-ordem tt-ordem-compra1.it-codigo ~
tt-ordem-compra1.cod-estabel ~
fn-retorna-entrega(tt-ordem-compra1.numero-ordem) @ c-data-entrega ~
fn-quant-saldo(tt-ordem-compra1.numero-ordem) @ de-qtd-saldo ~
tt-ordem-compra1.preco-fornec ~
fn-desc-item-un(tt-ordem-compra1.it-codigo) @ c-desc-item ~
tt-ordem-compra1.cod-comprado tt-ordem-compra1.cod-cond-pag ~
tt-ordem-compra1.nr-contrato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable1 
&Scoped-define QUERY-STRING-brTable1 FOR EACH tt-ordem-compra1 NO-LOCK ~
    BY tt-ordem-compra1.num-pedido ~
       BY tt-ordem-compra1.numero-ordem
&Scoped-define OPEN-QUERY-brTable1 OPEN QUERY brTable1 FOR EACH tt-ordem-compra1 NO-LOCK ~
    BY tt-ordem-compra1.num-pedido ~
       BY tt-ordem-compra1.numero-ordem.
&Scoped-define TABLES-IN-QUERY-brTable1 tt-ordem-compra1
&Scoped-define FIRST-TABLE-IN-QUERY-brTable1 tt-ordem-compra1


/* Definitions for BROWSE brTable2                                      */
&Scoped-define FIELDS-IN-QUERY-brTable2 tt-ordem-compra2.numero-ordem ~
tt-ordem-compra2.it-codigo tt-ordem-compra2.num-pedido ~
tt-ordem-compra2.cod-estabel ~
fn-retorna-entrega(tt-ordem-compra2.numero-ordem) @ c-data-entrega ~
fn-quant-saldo(tt-ordem-compra2.numero-ordem) @ de-qtd-saldo ~
tt-ordem-compra2.preco-fornec ~
fn-desc-item-un-2(tt-ordem-compra2.it-codigo) @ c-desc-item ~
tt-ordem-compra2.cod-comprado tt-ordem-compra2.cod-cond-pag ~
tt-ordem-compra2.nr-contrato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable2 
&Scoped-define QUERY-STRING-brTable2 FOR EACH tt-ordem-compra2 NO-LOCK ~
    BY tt-ordem-compra2.numero-ordem
&Scoped-define OPEN-QUERY-brTable2 OPEN QUERY brTable2 FOR EACH tt-ordem-compra2 NO-LOCK ~
    BY tt-ordem-compra2.numero-ordem.
&Scoped-define TABLES-IN-QUERY-brTable2 tt-ordem-compra2
&Scoped-define FIRST-TABLE-IN-QUERY-brTable2 tt-ordem-compra2


/* Definitions for BROWSE brTable3                                      */
&Scoped-define FIELDS-IN-QUERY-brTable3 tt-ordem-compra3.it-codigo ~
tt-ordem-compra3.num-pedido tt-ordem-compra3.numero-ordem ~
tt-ordem-compra3.cod-estabel ~
fn-retorna-entrega(tt-ordem-compra3.numero-ordem) @ c-data-entrega ~
fn-quant-saldo(tt-ordem-compra3.numero-ordem) @ de-qtd-saldo ~
tt-ordem-compra3.preco-fornec ~
fn-desc-item-un-3(tt-ordem-compra3.it-codigo) @ c-desc-item ~
tt-ordem-compra3.cod-comprado tt-ordem-compra3.cod-cond-pag ~
tt-ordem-compra3.nr-contrato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable3 
&Scoped-define QUERY-STRING-brTable3 FOR EACH tt-ordem-compra3 NO-LOCK ~
    BY tt-ordem-compra3.it-codigo ~
       BY tt-ordem-compra3.numero-ordem
&Scoped-define OPEN-QUERY-brTable3 OPEN QUERY brTable3 FOR EACH tt-ordem-compra3 NO-LOCK ~
    BY tt-ordem-compra3.it-codigo ~
       BY tt-ordem-compra3.numero-ordem.
&Scoped-define TABLES-IN-QUERY-brTable3 tt-ordem-compra3
&Scoped-define FIRST-TABLE-IN-QUERY-brTable3 tt-ordem-compra3


/* Definitions for BROWSE brTable4                                      */
&Scoped-define FIELDS-IN-QUERY-brTable4 tt-ordem-compra4.nr-contrato ~
tt-ordem-compra4.it-codigo tt-ordem-compra4.num-pedido ~
tt-ordem-compra4.numero-ordem tt-ordem-compra4.cod-estabel ~
fn-retorna-entrega(tt-ordem-compra4.numero-ordem) @ c-data-entrega ~
fn-quant-saldo(tt-ordem-compra4.numero-ordem) @ de-qtd-saldo ~
tt-ordem-compra4.preco-fornec ~
fn-desc-item-un-4(tt-ordem-compra4.it-codigo) @ c-desc-item ~
tt-ordem-compra4.cod-comprado tt-ordem-compra4.cod-cond-pag 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable4 
&Scoped-define QUERY-STRING-brTable4 FOR EACH tt-ordem-compra4 NO-LOCK ~
    BY tt-ordem-compra4.nr-contrato ~
       BY tt-ordem-compra4.numero-ordem
&Scoped-define OPEN-QUERY-brTable4 OPEN QUERY brTable4 FOR EACH tt-ordem-compra4 NO-LOCK ~
    BY tt-ordem-compra4.nr-contrato ~
       BY tt-ordem-compra4.numero-ordem.
&Scoped-define TABLES-IN-QUERY-brTable4 tt-ordem-compra4
&Scoped-define FIRST-TABLE-IN-QUERY-brTable4 tt-ordem-compra4


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brTable1}

/* Definitions for FRAME fPage2                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage2 ~
    ~{&OPEN-QUERY-brTable2}

/* Definitions for FRAME fPage3                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage3 ~
    ~{&OPEN-QUERY-brTable3}

/* Definitions for FRAME fPage4                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage4 ~
    ~{&OPEN-QUERY-brTable4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar i-cod-emitente c-nome-emit btOK ~
btCancel btHelp 
&Scoped-Define DISPLAYED-OBJECTS i-cod-emitente c-nome-emit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item-un wZoom 
FUNCTION fn-desc-item-un RETURNS CHARACTER
  ( c-it-codigo as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item-un-2 wZoom 
FUNCTION fn-desc-item-un-2 RETURNS CHARACTER
  ( c-it-codigo as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item-un-3 wZoom 
FUNCTION fn-desc-item-un-3 RETURNS CHARACTER
  ( c-it-codigo as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item-un-4 wZoom 
FUNCTION fn-desc-item-un-4 RETURNS CHARACTER
  ( c-it-codigo as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-preco-forn wZoom 
FUNCTION fn-preco-forn RETURNS DECIMAL
  ( r-rowid as rowid )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-quant-saldo wZoom 
FUNCTION fn-quant-saldo RETURNS DECIMAL
  ( p-numero-ordem AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-retorna-entrega wZoom 
FUNCTION fn-retorna-entrega RETURNS CHARACTER
  ( p-numero-ordem AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wZoom AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-nome-emit AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 52.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Emitente" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON btImplant1 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE BUTTON btNarrativa1 
     LABEL "Narrativa" 
     SIZE 10 BY 1.

DEFINE BUTTON btImplant2 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE BUTTON btNarrativa2 
     LABEL "Narrativa" 
     SIZE 10 BY 1.

DEFINE BUTTON btImplant3 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE BUTTON btNarrativa3 
     LABEL "Narrativa" 
     SIZE 10 BY 1.

DEFINE BUTTON btImplant4 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE BUTTON btNarrativa4 
     LABEL "Narrativa" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTable1 FOR 
      tt-ordem-compra1 SCROLLING.

DEFINE QUERY brTable2 FOR 
      tt-ordem-compra2 SCROLLING.

DEFINE QUERY brTable3 FOR 
      tt-ordem-compra3 SCROLLING.

DEFINE QUERY brTable4 FOR 
      tt-ordem-compra4 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable1 wZoom _STRUCTURED
  QUERY brTable1 NO-LOCK DISPLAY
      tt-ordem-compra1.num-pedido FORMAT ">>>>>,>>9":U
      tt-ordem-compra1.numero-ordem FORMAT "zzzzz9,99":U
      tt-ordem-compra1.it-codigo FORMAT "X(16)":U
      tt-ordem-compra1.cod-estabel FORMAT "x(5)":U
      fn-retorna-entrega(tt-ordem-compra1.numero-ordem) @ c-data-entrega COLUMN-LABEL "Data Entrega"
      fn-quant-saldo(tt-ordem-compra1.numero-ordem) @ de-qtd-saldo COLUMN-LABEL "Qtde Saldo" FORMAT ">>>>,>>>,>>9.99999":U
            WIDTH 20.43
      tt-ordem-compra1.preco-fornec FORMAT ">>>>>,>>>,>>9.99999":U
      fn-desc-item-un(tt-ordem-compra1.it-codigo) @ c-desc-item COLUMN-LABEL "Descri¯Êo Item" FORMAT "x(60)":U
      tt-ordem-compra1.cod-comprado FORMAT "X(12)":U
      tt-ordem-compra1.cod-cond-pag FORMAT ">>>9":U
      tt-ordem-compra1.nr-contrato FORMAT ">>>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.79
         FONT 2.

DEFINE BROWSE brTable2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable2 wZoom _STRUCTURED
  QUERY brTable2 NO-LOCK DISPLAY
      tt-ordem-compra2.numero-ordem FORMAT "zzzzz9,99":U
      tt-ordem-compra2.it-codigo FORMAT "X(16)":U
      tt-ordem-compra2.num-pedido FORMAT ">>>>>,>>9":U
      tt-ordem-compra2.cod-estabel FORMAT "x(5)":U
      fn-retorna-entrega(tt-ordem-compra2.numero-ordem) @ c-data-entrega COLUMN-LABEL "Data Entrega"
      fn-quant-saldo(tt-ordem-compra2.numero-ordem) @ de-qtd-saldo COLUMN-LABEL "Qtde Saldo" FORMAT ">>>>,>>>,>>9.99999":U
            WIDTH 19.43
      tt-ordem-compra2.preco-fornec FORMAT ">>>>>,>>>,>>9.99999":U
      fn-desc-item-un-2(tt-ordem-compra2.it-codigo) @ c-desc-item COLUMN-LABEL "Descri¯Êo Item" FORMAT "x(60)":U
      tt-ordem-compra2.cod-comprado FORMAT "X(12)":U
      tt-ordem-compra2.cod-cond-pag FORMAT ">>>9":U
      tt-ordem-compra2.nr-contrato FORMAT ">>>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.79
         FONT 2.

DEFINE BROWSE brTable3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable3 wZoom _STRUCTURED
  QUERY brTable3 NO-LOCK DISPLAY
      tt-ordem-compra3.it-codigo FORMAT "X(16)":U
      tt-ordem-compra3.num-pedido FORMAT ">>>>>,>>9":U
      tt-ordem-compra3.numero-ordem FORMAT "zzzzz9,99":U
      tt-ordem-compra3.cod-estabel FORMAT "x(5)":U
      fn-retorna-entrega(tt-ordem-compra3.numero-ordem) @ c-data-entrega COLUMN-LABEL "Data Entrega"
      fn-quant-saldo(tt-ordem-compra3.numero-ordem) @ de-qtd-saldo COLUMN-LABEL "Qtde Saldo" FORMAT ">>>>,>>>,>>9.99999":U
            WIDTH 19.43
      tt-ordem-compra3.preco-fornec FORMAT ">>>>>,>>>,>>9.99999":U
      fn-desc-item-un-3(tt-ordem-compra3.it-codigo) @ c-desc-item COLUMN-LABEL "Descri¯Êo Item" FORMAT "x(60)":U
      tt-ordem-compra3.cod-comprado FORMAT "X(12)":U
      tt-ordem-compra3.cod-cond-pag FORMAT ">>>9":U
      tt-ordem-compra3.nr-contrato FORMAT ">>>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.79
         FONT 2.

DEFINE BROWSE brTable4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable4 wZoom _STRUCTURED
  QUERY brTable4 NO-LOCK DISPLAY
      tt-ordem-compra4.nr-contrato FORMAT ">>>>>>>>9":U
      tt-ordem-compra4.it-codigo FORMAT "X(16)":U
      tt-ordem-compra4.num-pedido FORMAT ">>>>>,>>9":U
      tt-ordem-compra4.numero-ordem FORMAT "zzzzz9,99":U
      tt-ordem-compra4.cod-estabel FORMAT "x(5)":U
      fn-retorna-entrega(tt-ordem-compra4.numero-ordem) @ c-data-entrega COLUMN-LABEL "Data Entrega"
      fn-quant-saldo(tt-ordem-compra4.numero-ordem) @ de-qtd-saldo COLUMN-LABEL "Qtde Saldo" FORMAT ">>>>,>>>,>>9.99999":U
            WIDTH 19.14
      tt-ordem-compra4.preco-fornec FORMAT ">>>>>,>>>,>>9.99999":U
      fn-desc-item-un-4(tt-ordem-compra4.it-codigo) @ c-desc-item COLUMN-LABEL "Descri¯Êo Item" FORMAT "x(60)":U
      tt-ordem-compra4.cod-comprado FORMAT "X(12)":U
      tt-ordem-compra4.cod-cond-pag FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.79
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     i-cod-emitente AT ROW 1.29 COL 13.57 COLON-ALIGNED
     c-nome-emit AT ROW 1.29 COL 26.86 COLON-ALIGNED NO-LABEL
     btOK AT ROW 17.71 COL 2
     btCancel AT ROW 17.71 COL 13
     btHelp AT ROW 17.71 COL 80
     rtToolBar AT ROW 17.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 18.13
         FONT 1.

DEFINE FRAME fPage1
     brTable1 AT ROW 3.21 COL 2
     btImplant1 AT ROW 13 COL 2
     btNarrativa1 AT ROW 13 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 3.45
         SIZE 84.43 BY 13.29
         FONT 1.

DEFINE FRAME fPage2
     brTable2 AT ROW 3.21 COL 2
     btImplant2 AT ROW 13 COL 2
     btNarrativa2 AT ROW 13 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 3.45
         SIZE 84.43 BY 13.29
         FONT 1.

DEFINE FRAME fPage3
     brTable3 AT ROW 3.21 COL 2
     btImplant3 AT ROW 13 COL 2
     btNarrativa3 AT ROW 13 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 3.45
         SIZE 84.43 BY 13.29
         FONT 1.

DEFINE FRAME fPage4
     brTable4 AT ROW 3.21 COL 2
     btImplant4 AT ROW 13 COL 2
     btNarrativa4 AT ROW 13 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 3.45
         SIZE 84.43 BY 13.29
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Zoom
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-ordem-compra1 T "?" NO-UNDO movind ordem-compra
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-ordem-compra2 T "?" NO-UNDO movind ordem-compra
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-ordem-compra3 T "?" NO-UNDO movind ordem-compra
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-ordem-compra4 T "?" NO-UNDO movind ordem-compra
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wZoom ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 18.13
         WIDTH              = 90
         MAX-HEIGHT         = 20.96
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.96
         VIRTUAL-WIDTH      = 114.29
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wZoom 
/* ************************* Included-Libraries *********************** */

{zoom/zoom.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wZoom
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE
       FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage3:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brTable1 1 fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* BROWSE-TAB brTable2 1 fPage2 */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* BROWSE-TAB brTable3 1 fPage3 */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* BROWSE-TAB brTable4 1 fPage4 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wZoom)
THEN wZoom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable1
/* Query rebuild information for BROWSE brTable1
     _TblList          = "Temp-Tables.tt-ordem-compra1"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tt-ordem-compra1.num-pedido|yes,Temp-Tables.tt-ordem-compra1.numero-ordem|yes"
     _FldNameList[1]   = Temp-Tables.tt-ordem-compra1.num-pedido
     _FldNameList[2]   = Temp-Tables.tt-ordem-compra1.numero-ordem
     _FldNameList[3]   = Temp-Tables.tt-ordem-compra1.it-codigo
     _FldNameList[4]   > Temp-Tables.tt-ordem-compra1.cod-estabel
"cod-estabel" ? "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"fn-retorna-entrega(tt-ordem-compra1.numero-ordem) @ c-data-entrega" "Data Entrega" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"fn-quant-saldo(tt-ordem-compra1.numero-ordem) @ de-qtd-saldo" "Qtde Saldo" ">>>>,>>>,>>9.99999" ? ? ? ? ? ? ? no ? no no "20.43" yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.tt-ordem-compra1.preco-fornec
     _FldNameList[8]   > "_<CALC>"
"fn-desc-item-un(tt-ordem-compra1.it-codigo) @ c-desc-item" "Descri¯Êo Item" "x(60)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Temp-Tables.tt-ordem-compra1.cod-comprado
     _FldNameList[10]   = Temp-Tables.tt-ordem-compra1.cod-cond-pag
     _FldNameList[11]   = Temp-Tables.tt-ordem-compra1.nr-contrato
     _Query            is OPENED
*/  /* BROWSE brTable1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable2
/* Query rebuild information for BROWSE brTable2
     _TblList          = "Temp-Tables.tt-ordem-compra2"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tt-ordem-compra2.numero-ordem|yes"
     _FldNameList[1]   = Temp-Tables.tt-ordem-compra2.numero-ordem
     _FldNameList[2]   = Temp-Tables.tt-ordem-compra2.it-codigo
     _FldNameList[3]   = Temp-Tables.tt-ordem-compra2.num-pedido
     _FldNameList[4]   > Temp-Tables.tt-ordem-compra2.cod-estabel
"cod-estabel" ? "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"fn-retorna-entrega(tt-ordem-compra2.numero-ordem) @ c-data-entrega" "Data Entrega" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"fn-quant-saldo(tt-ordem-compra2.numero-ordem) @ de-qtd-saldo" "Qtde Saldo" ">>>>,>>>,>>9.99999" ? ? ? ? ? ? ? no ? no no "19.43" yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.tt-ordem-compra2.preco-fornec
     _FldNameList[8]   > "_<CALC>"
"fn-desc-item-un-2(tt-ordem-compra2.it-codigo) @ c-desc-item" "Descri¯Êo Item" "x(60)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Temp-Tables.tt-ordem-compra2.cod-comprado
     _FldNameList[10]   = Temp-Tables.tt-ordem-compra2.cod-cond-pag
     _FldNameList[11]   = Temp-Tables.tt-ordem-compra2.nr-contrato
     _Query            is OPENED
*/  /* BROWSE brTable2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable3
/* Query rebuild information for BROWSE brTable3
     _TblList          = "Temp-Tables.tt-ordem-compra3"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tt-ordem-compra3.it-codigo|yes,Temp-Tables.tt-ordem-compra3.numero-ordem|yes"
     _FldNameList[1]   = Temp-Tables.tt-ordem-compra3.it-codigo
     _FldNameList[2]   = Temp-Tables.tt-ordem-compra3.num-pedido
     _FldNameList[3]   = Temp-Tables.tt-ordem-compra3.numero-ordem
     _FldNameList[4]   > Temp-Tables.tt-ordem-compra3.cod-estabel
"cod-estabel" ? "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"fn-retorna-entrega(tt-ordem-compra3.numero-ordem) @ c-data-entrega" "Data Entrega" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"fn-quant-saldo(tt-ordem-compra3.numero-ordem) @ de-qtd-saldo" "Qtde Saldo" ">>>>,>>>,>>9.99999" ? ? ? ? ? ? ? no ? no no "19.43" yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.tt-ordem-compra3.preco-fornec
     _FldNameList[8]   > "_<CALC>"
"fn-desc-item-un-3(tt-ordem-compra3.it-codigo) @ c-desc-item" "Descri¯Êo Item" "x(60)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Temp-Tables.tt-ordem-compra3.cod-comprado
     _FldNameList[10]   = Temp-Tables.tt-ordem-compra3.cod-cond-pag
     _FldNameList[11]   = Temp-Tables.tt-ordem-compra3.nr-contrato
     _Query            is OPENED
*/  /* BROWSE brTable3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable4
/* Query rebuild information for BROWSE brTable4
     _TblList          = "Temp-Tables.tt-ordem-compra4"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tt-ordem-compra4.nr-contrato|yes,Temp-Tables.tt-ordem-compra4.numero-ordem|yes"
     _FldNameList[1]   = Temp-Tables.tt-ordem-compra4.nr-contrato
     _FldNameList[2]   = Temp-Tables.tt-ordem-compra4.it-codigo
     _FldNameList[3]   = Temp-Tables.tt-ordem-compra4.num-pedido
     _FldNameList[4]   = Temp-Tables.tt-ordem-compra4.numero-ordem
     _FldNameList[5]   > Temp-Tables.tt-ordem-compra4.cod-estabel
"cod-estabel" ? "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"fn-retorna-entrega(tt-ordem-compra4.numero-ordem) @ c-data-entrega" "Data Entrega" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"fn-quant-saldo(tt-ordem-compra4.numero-ordem) @ de-qtd-saldo" "Qtde Saldo" ">>>>,>>>,>>9.99999" ? ? ? ? ? ? ? no ? no no "19.14" yes no no "U" "" ""
     _FldNameList[8]   = Temp-Tables.tt-ordem-compra4.preco-fornec
     _FldNameList[9]   > "_<CALC>"
"fn-desc-item-un-4(tt-ordem-compra4.it-codigo) @ c-desc-item" "Descri¯Êo Item" "x(60)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   = Temp-Tables.tt-ordem-compra4.cod-comprado
     _FldNameList[11]   = Temp-Tables.tt-ordem-compra4.cod-cond-pag
     _Query            is OPENED
*/  /* BROWSE brTable4 */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage3
/* Query rebuild information for FRAME fPage3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage4
/* Query rebuild information for FRAME fPage4
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wZoom wZoom
ON END-ERROR OF wZoom
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wZoom wZoom
ON WINDOW-CLOSE OF wZoom
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wZoom
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wZoom
ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btImplant1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant1 wZoom
ON CHOOSE OF btImplant1 IN FRAME fPage1 /* Implantar */
DO:
/*    {zoom/implant.i &ProgramImplant="<ProgramName>"
 *                     &PageNumber="1"}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btImplant2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant2 wZoom
ON CHOOSE OF btImplant2 IN FRAME fPage2 /* Implantar */
DO:
/*    {zoom/implant.i &ProgramImplant="<ProgramName>"
 *                     &PageNumber="2"}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btImplant3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant3 wZoom
ON CHOOSE OF btImplant3 IN FRAME fPage3 /* Implantar */
DO:
/*    {zoom/implant.i &ProgramImplant="<ProgramName>"
 *                     &PageNumber="1"}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME btImplant4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant4 wZoom
ON CHOOSE OF btImplant4 IN FRAME fPage4 /* Implantar */
DO:
   /* {zoom/implant.i &ProgramImplant="<ProgramName>"
 *                     &PageNumber="1"}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btNarrativa1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNarrativa1 wZoom
ON CHOOSE OF btNarrativa1 IN FRAME fPage1 /* Narrativa */
DO:
    RUN ccp/cc9046.w (INPUT tt-ordem-compra1.numero-ordem,
                      INPUT tt-ordem-compra1.cod-estabel,
                      INPUT i-cod-emitente,
                      INPUT tt-ordem-compra1.it-codigo,
                      INPUT cSegUsuario).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btNarrativa2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNarrativa2 wZoom
ON CHOOSE OF btNarrativa2 IN FRAME fPage2 /* Narrativa */
DO:
    RUN ccp/cc9046.w (INPUT tt-ordem-compra2.numero-ordem,
                      INPUT tt-ordem-compra2.cod-estabel,
                      INPUT i-cod-emitente,
                      INPUT tt-ordem-compra2.it-codigo,
                      INPUT cSegUsuario).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btNarrativa3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNarrativa3 wZoom
ON CHOOSE OF btNarrativa3 IN FRAME fPage3 /* Narrativa */
DO:
    RUN ccp/cc9046.w (INPUT tt-ordem-compra3.numero-ordem,
                      INPUT tt-ordem-compra3.cod-estabel,
                      INPUT i-cod-emitente,
                      INPUT tt-ordem-compra3.it-codigo,
                      INPUT cSegUsuario).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME btNarrativa4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNarrativa4 wZoom
ON CHOOSE OF btNarrativa4 IN FRAME fPage4 /* Narrativa */
DO:
    RUN ccp/cc9046.w (INPUT tt-ordem-compra4.numero-ordem,
                      INPUT tt-ordem-compra4.cod-estabel,
                      INPUT i-cod-emitente,
                      INPUT tt-ordem-compra4.it-codigo,
                      INPUT cSegUsuario).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wZoom
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN returnValues IN THIS-PROCEDURE.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brTable1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wZoom 


/* ***************************  Main Block  *************************** */

/*--- L«gica para inicializa¯Êo do programam ---*/
{zoom/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wZoom 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ENABLE btNarrativa1 WITH FRAME fPage1.
    ENABLE btNarrativa2 WITH FRAME fPage2.
    ENABLE btNarrativa3 WITH FRAME fPage3.
    ENABLE btNarrativa4 WITH FRAME fPage4.

    RUN retornaNomeEmit IN h-boes011 (INPUT  i-cod-emitente, 
                                      OUTPUT c-nome-emit).

    DISP i-cod-emitente
         c-nome-emit
        WITH FRAME fPage0.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDestroyInterface wZoom 
PROCEDURE beforeDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h-boes011) THEN DO:
        DELETE PROCEDURE h-boes011.
        ASSIGN h-boes011 = ?.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wZoom 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(h-boes011) 
    OR h-boes011:TYPE      <> "PROCEDURE":U 
    OR h-boes011:FILE-NAME <> "esbo/boes011.p":U THEN
        RUN esbo/boes011.p PERSISTEN SET h-boes011.

    /*--- Verifica se o DBO j˜ est˜ inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable1}) THEN DO:
        {btb/btb008za.i1 inbo/boin274q01.p }
        {btb/btb008za.i2 inbo/boin274q01.p '' {&hDBOTable1}} 
    END.

    RUN setConstraintByPedido IN {&hDBOTable1} (input cSegUsuario,
                                                input codNumPedidoIni, 
                                                input codNumPedidoFim,
                                                input "", 
                                                input "ZZZ",
                                                input i-cod-emitente,
                                                input cItCodigo) NO-ERROR.

    /*--- Verifica se o DBO j˜ est˜ inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable2}) THEN DO:
        {btb/btb008za.i1 inbo/boin274q01.p }
        {btb/btb008za.i2 inbo/boin274q01.p '' {&hDBOTable2}} 
    END.

    RUN setConstraintByOrdem IN {&hDBOTable2} (input cSegUsuario,
                                               input 0,   
                                               input 99999999,
                                               input "", 
                                               input "ZZZ",
                                               input i-cod-emitente,
                                               input cItCodigo) NO-ERROR.

    /*--- Verifica se o DBO j˜ est˜ inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable3}) THEN DO:
        {btb/btb008za.i1 inbo/boin274q01.p }
        {btb/btb008za.i2 inbo/boin274q01.p '' {&hDBOTable3}} 
    END.

    RUN setConstraintByItem IN {&hDBOTable3} (input cSegUsuario,
                                              input "",
                                              input fill("Z",16),
                                              input "", 
                                              input "ZZZ",
                                              input i-cod-emitente,
                                              input cItCodigo) NO-ERROR.

    /*--- Verifica se o DBO j˜ est˜ inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable4}) THEN DO:
        {btb/btb008za.i1 inbo/boin274q01.p }
        {btb/btb008za.i2 inbo/boin274q01.p '' {&hDBOTable4}} 
    END.

    RUN setConstraintByContrato IN {&hDBOTable4} (input cSegUsuario,
                                                  input 0,
                                                  input 999999999,
                                                  input "", 
                                                  input "ZZZ",
                                                  input i-cod-emitente,
                                                  input cItCodigo) NO-ERROR.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueries wZoom 
PROCEDURE openQueries :
/*------------------------------------------------------------------------------
  Purpose:     Atualiza browsers
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    {zoom/openqueries.i &Query="ByNumPedido"
                        &PageNumber="1"}

    {zoom/openqueries.i &Query="ByNumeroOrdem"
                        &PageNumber="2"}

    {zoom/openqueries.i &Query="byItCodigo"
                        &PageNumber="3"}

    {zoom/openqueries.i &Query="ByNrContrato"
                        &PageNumber="4"}

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage1 wZoom 
PROCEDURE returnFieldsPage1 :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p˜gina 1
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.

    IF AVAILABLE {&ttTable1} THEN DO:
        CASE pcField:
          WHEN "num-pedido":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.num-pedido).
          WHEN "numero-ordem":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.numero-ordem).
          WHEN "it-codigo":U THEN
                ASSIGN pcFieldValue = STRING({&tttable1}.it-codigo).
          WHEN "cod-estabel":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.cod-estabel).
          WHEN "preco-fornec":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.preco-fornec).
          WHEN "cod-comprado":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.cod-comprado).
          WHEN "cod-cond-pag":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.cod-cond-pag).
        END CASE.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage2 wZoom 
PROCEDURE returnFieldsPage2 :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p˜gina 2
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.

    IF AVAILABLE {&ttTable2} THEN DO:
        CASE pcField:
          WHEN "numero-ordem":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.numero-ordem).
          WHEN "num-pedido":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.num-pedido).
          WHEN "it-codigo":U THEN
                ASSIGN pcFieldValue = STRING({&tttable2}.it-codigo).
          WHEN "cod-estabel":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.cod-estabel).
          WHEN "preco-fornec":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.preco-fornec).
          WHEN "cod-comprado":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.cod-comprado).
          WHEN "cod-cond-pag":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.cod-cond-pag).          
        END CASE.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage3 wZoom 
PROCEDURE returnFieldsPage3 :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p˜gina 3
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.

    IF AVAILABLE {&ttTable3} THEN DO:
        CASE pcField:
          WHEN "it-codigo":U THEN
                ASSIGN pcFieldValue = STRING({&tttable3}.it-codigo).
          WHEN "num-pedido":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable3}.num-pedido).
          WHEN "numero-ordem":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable3}.numero-ordem).
          WHEN "cod-estabel":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable3}.cod-estabel).
          WHEN "preco-fornec":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable3}.preco-fornec).
          WHEN "cod-comprado":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable3}.cod-comprado).
          WHEN "cod-cond-pag":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable3}.cod-cond-pag).
        END CASE.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage4 wZoom 
PROCEDURE returnFieldsPage4 :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p˜gina 4
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.

    IF AVAILABLE {&ttTable4} THEN DO:
        CASE pcField:
          WHEN "nr-contrato":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable4}.nr-contrato).
          WHEN "it-codigo":U THEN
                ASSIGN pcFieldValue = STRING({&tttable4}.it-codigo).
          WHEN "num-pedido":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable4}.num-pedido).
          WHEN "numero-ordem":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable4}.numero-ordem).
          WHEN "cod-estabel":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable4}.cod-estabel).
          WHEN "cod-comprado":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable4}.cod-comprado).
          WHEN "cod-cond-pag":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable4}.cod-cond-pag).          
        END CASE.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConstraints wZoom 
PROCEDURE setConstraints :
/*------------------------------------------------------------------------------
  Purpose:     Seta constraints e atualiza o browse, conforme nßmero da p˜gina
               passado como parümetro
  Parameters:  recebe nßmero da p˜gina
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.

    /*--- Seta constraints conforme nßmero da p˜gina ---*/
    CASE pPageNumber:
        WHEN 1 THEN
              /*--- Seta Constraints para o DBO Table1 ---*/
              RUN setConstraintByPedido IN {&hDBOTable1} ( input cSegUsuario,
                                                           input fnIniRangeIntPage(input 1,input 1), 
                                                           input fnEndRangeIntPage(input 1,input 1),
                                                           input fnIniRangeCharPage(input 1,input 2), 
                                                           input fnEndRangeCharPage(input 1,input 2),
                                                           input i-cod-emitente,
                                                           input cItCodigo) NO-ERROR.

        WHEN 2 THEN
              /*--- Seta Constraints para o DBO Table2 ---*/
              RUN setConstraintByOrdem IN {&hDBOTable2} ( input cSegUsuario,
                                                          input fnIniRangeIntPage(input 2,input 1), 
                                                          input fnEndRangeIntPage(input 2,input 1),
                                                          input fnIniRangeCharPage(input 2,input 2), 
                                                          input fnEndRangeCharPage(input 2,input 2),
                                                          input i-cod-emitente,
                                                          input cItCodigo) NO-ERROR.
        WHEN 3 THEN
              /*--- Seta Constraints para o DBO Table3 ---*/
              RUN setConstraintByItem IN {&hDBOTable3} ( input cSegUsuario,
                                                         input fnIniRangeCharPage(input 3,input 1), 
                                                         input fnEndRangeCharPage(input 3,input 1),
                                                         input fnIniRangeCharPage(input 3,input 2), 
                                                         input fnEndRangeCharPage(input 3,input 2),
                                                         input i-cod-emitente,
                                                         input cItCodigo) NO-ERROR.
        WHEN 4 THEN
              /*--- Seta Constraints para o DBO Table4 ---*/
              RUN setConstraintByContrato IN {&hDBOTable4} ( input cSegUsuario,
                                                             input fnIniRangeIntPage(input 4,input 1), 
                                                             input fnEndRangeIntPage(input 4,input 1),
                                                             input fnIniRangeCharPage(input 4,input 2), 
                                                             input fnEndRangeCharPage(input 4,input 2),
                                                             input i-cod-emitente,
                                                             input cItCodigo) NO-ERROR.
    END CASE.

    /*--- Seta vari˜vel iConstraintPageNumber com o nßmero da p˜gina atual 
          Esta vari˜vel ý utilizada no mýtodo openQueries ---*/
    ASSIGN iConstraintPageNumber = pPageNumber.

    /*--- Atualiza browse ---*/
    RUN openQueries IN THIS-PROCEDURE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setParametersPedido wZoom 
PROCEDURE setParametersPedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters: pi-cod-emitente   - c«dido do emitente do documento
              pc-it-codigo      - c«digo do item informado na inclusÊo
              pi-num-pedido-ini - n£mero do inicial pedido 
              pi-num-pedido-fim - n£mero do final pedido 
              phBoin176         - handle da boin176
              pc-seg-usuario    - c«digo do usu˜rio do docum-est
  Notes:       
------------------------------------------------------------------------------*/
    def input param pi-cod-emitente   as int    no-undo.
    def input param pc-it-codigo      as char   no-undo.
    def input param pi-num-pedido-ini as int    no-undo.
    def input param pi-num-pedido-fim as int    no-undo.
    def input param pc-seg-usuario    as char   no-undo.
    DEF INPUT PARAM pr-item           AS ROWID  NO-UNDO.
    DEF INPUT PARAM pdt-emissao       AS DATE   NO-UNDO.

    assign i-cod-emitente  = pi-cod-emitente
           cItCodigo       = pc-it-codigo
           codNumPedidoIni = pi-num-pedido-ini
           codNumPedidoFim = pi-num-pedido-fim
           cSegUsuario     = pc-seg-usuario
           r-item          = pr-item    
           d-dt-emissao    = pdt-emissao
        . 

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item-un wZoom 
FUNCTION fn-desc-item-un RETURNS CHARACTER
  ( c-it-codigo as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

     RUN retornaDescItem IN h-boes011 (INPUT  c-it-codigo,
                                       OUTPUT c-desc-item).

     RETURN string(c-desc-item,"x(60)").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item-un-2 wZoom 
FUNCTION fn-desc-item-un-2 RETURNS CHARACTER
  ( c-it-codigo as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

     RUN retornaDescItem IN h-boes011 (INPUT  c-it-codigo,
                                       OUTPUT c-desc-item).

     RETURN string(c-desc-item,"x(60)").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item-un-3 wZoom 
FUNCTION fn-desc-item-un-3 RETURNS CHARACTER
  ( c-it-codigo as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

     RUN retornaDescItem IN h-boes011 (INPUT  c-it-codigo,
                                       OUTPUT c-desc-item).

     RETURN string(c-desc-item,"x(60)").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item-un-4 wZoom 
FUNCTION fn-desc-item-un-4 RETURNS CHARACTER
  ( c-it-codigo as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

     RUN retornaDescItem IN h-boes011 (INPUT  c-it-codigo,
                                       OUTPUT c-desc-item).

     RETURN string(c-desc-item,"x(60)").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-preco-forn wZoom 
FUNCTION fn-preco-forn RETURNS DECIMAL
  ( r-rowid as rowid ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    def var de-pr-forn like ordem-compra.preco-forn no-undo.
    
    RUN calculaPrecoOC IN h-boes011 (INPUT  r-rowid,
                                     INPUT  r-item,
                                     INPUT d-dt-emissao,
                                     output de-pr-forn ).

    RETURN de-pr-forn.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-quant-saldo wZoom 
FUNCTION fn-quant-saldo RETURNS DECIMAL
  ( p-numero-ordem AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var de-qtde-rec-forn as decimal  no-undo.
    def var de-qtde-saldo    as decimal  NO-UNDO format ">>>>>>>,>>9.9999".

    run calculaQtdSaldo IN h-boes011 (INPUT YES,
                                      INPUT YES,
                                      INPUT p-numero-ordem,
                                      OUTPUT de-qtde-rec-forn,
                                      OUTPUT de-qtde-saldo ).

    RETURN de-qtde-rec-forn.   /* Function return value. */ 
     
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-retorna-entrega wZoom 
FUNCTION fn-retorna-entrega RETURNS CHARACTER
  ( p-numero-ordem AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR c-dt-entrega AS CHAR NO-UNDO.

    RUN calculaDataEntrega IN h-boes011 (INPUT  p-numero-ordem,
                                         OUTPUT c-dt-entrega ).

    RETURN c-dt-entrega.   /* Function return value. */ 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

