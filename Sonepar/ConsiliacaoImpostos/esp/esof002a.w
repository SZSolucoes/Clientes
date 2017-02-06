&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESOF002A 2.12.00.001 } /*** 010001 ***/
/********************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESOF002A
&GLOBAL-DEFINE Version        2.12.00.001

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE page0Widgets   c-cod-emitente-ini c-cod-emitente-fim ~
                              c-cod-estabel-ini c-cod-estabel-fim ~
                              c-serie-ini c-serie-fim ~
                              c-nro-docto-ini c-nro-docto-fim ~
                              d-dt-transacao-ini d-dt-transacao-fim ~
                              btHelp btOK btCancel 

/* Parameters Definitions ---                                           */
DEFINE INPUT        PARAM p-tipo-folder      AS INT                     NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-ini LIKE emitente.cod-emitente NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-fim LIKE emitente.cod-emitente NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-estabel-ini  LIKE docum-est.cod-estabel NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-estabel-fim  LIKE docum-est.cod-estabel NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-serie-ini        LIKE docum-est.serie       NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-serie-fim        LIKE docum-est.serie       NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nro-docto-ini    LIKE docum-est.nro-docto   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nro-docto-fim    LIKE docum-est.nro-docto   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-dt-transacao-ini LIKE docum-est.dt-trans    NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-dt-transacao-fim LIKE docum-est.dt-trans    NO-UNDO.
define input-output param pl-ok              as   logical               no-undo.

DEFINE VARIABLE h-boad098na    AS HANDLE      NO-UNDO.
DEFINE VARIABLE wh-pesquisa    AS HANDLE      NO-UNDO.
/* Local Variable Definitions (DBOs Handles) --- */
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
{cdp/cdcfgmat.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS c-cod-emitente-ini c-cod-emitente-fim ~
c-cod-estabel-ini c-cod-estabel-fim c-serie-ini c-serie-fim c-nro-docto-ini ~
c-nro-docto-fim d-dt-transacao-ini d-dt-transacao-fim btOK btCancel btHelp ~
IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-7 IMAGE-8 ~
IMAGE-9 rtToolBar 
&Scoped-Define DISPLAYED-OBJECTS c-cod-emitente-ini c-cod-emitente-fim ~
c-cod-estabel-ini c-cod-estabel-fim c-serie-ini c-serie-fim c-nro-docto-ini ~
c-nro-docto-fim d-dt-transacao-ini d-dt-transacao-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente":R8 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(5)" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(5)" 
     LABEL "Estabelecimento":R15 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nro-docto-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88.

DEFINE VARIABLE c-nro-docto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R17 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88.

DEFINE VARIABLE c-serie-fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE VARIABLE c-serie-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE VARIABLE d-dt-transacao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE d-dt-transacao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1800 
     LABEL "Data Transa‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     c-cod-emitente-ini AT ROW 1.42 COL 21 COLON-ALIGNED
     c-cod-emitente-fim AT ROW 1.42 COL 46.57 COLON-ALIGNED NO-LABEL
     c-cod-estabel-ini AT ROW 2.42 COL 21 COLON-ALIGNED
     c-cod-estabel-fim AT ROW 2.38 COL 46.57 COLON-ALIGNED NO-LABEL
     c-serie-ini AT ROW 3.42 COL 21 COLON-ALIGNED
     c-serie-fim AT ROW 3.38 COL 46.57 COLON-ALIGNED NO-LABEL
     c-nro-docto-ini AT ROW 4.42 COL 21 COLON-ALIGNED
     c-nro-docto-fim AT ROW 4.42 COL 46.57 COLON-ALIGNED NO-LABEL
     d-dt-transacao-ini AT ROW 5.42 COL 21 COLON-ALIGNED
     d-dt-transacao-fim AT ROW 5.42 COL 46.57 COLON-ALIGNED HELP
          "Data de transa‡Æo da nota fiscal" NO-LABEL
     btOK AT ROW 7 COL 2
     btCancel AT ROW 7 COL 13
     btHelp AT ROW 7 COL 68.29
     IMAGE-1 AT ROW 4.42 COL 40.57
     IMAGE-10 AT ROW 3.42 COL 44.72
     IMAGE-11 AT ROW 5.42 COL 40.57
     IMAGE-12 AT ROW 5.42 COL 44.72
     IMAGE-2 AT ROW 4.42 COL 44.72
     IMAGE-3 AT ROW 1.42 COL 40.72
     IMAGE-4 AT ROW 1.42 COL 44.72
     IMAGE-7 AT ROW 2.42 COL 40.57
     IMAGE-8 AT ROW 2.42 COL 44.72
     IMAGE-9 AT ROW 3.42 COL 40.57
     rtToolBar AT ROW 6.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.57 BY 7.29
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 7.25
         WIDTH              = 78.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
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


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wWindow
ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
   ASSIGN p-cod-emitente-ini = INPUT FRAME fPage0 c-cod-emitente-ini 
          p-cod-emitente-fim = INPUT FRAME fPage0 c-cod-emitente-fim
          p-cod-estabel-ini  = INPUT FRAME fPage0 c-cod-estabel-ini 
          p-cod-estabel-fim  = INPUT FRAME fPage0 c-cod-estabel-fim 
          p-serie-ini        = INPUT FRAME fPage0 c-serie-ini       
          p-serie-fim        = INPUT FRAME fPage0 c-serie-fim       
          p-nro-docto-ini    = INPUT FRAME fPage0 c-nro-docto-ini   
          p-nro-docto-fim    = INPUT FRAME fPage0 c-nro-docto-fim   
          p-dt-transacao-ini = input frame fPage0 d-dt-transacao-ini
          p-dt-transacao-fim = input frame fPage0 d-dt-transacao-fim 
          pl-ok              = yes. 
   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-emitente-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-emitente-fim wWindow
ON F5 OF c-cod-emitente-fim IN FRAME fpage0
DO:
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=adzoom/z07ad098.w
                       &campo=c-cod-emitente-fim
                       &campozoom=cod-emitente
                       &frame="fPage0"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-emitente-fim wWindow
ON MOUSE-SELECT-DBLCLICK OF c-cod-emitente-fim IN FRAME fpage0
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-emitente-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-emitente-ini wWindow
ON F5 OF c-cod-emitente-ini IN FRAME fpage0 /* Emitente */
DO:

    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=adzoom/z07ad098.w
                       &campo=c-cod-emitente-ini
                       &campozoom=cod-emitente
                       &frame="fPage0"}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-emitente-ini wWindow
ON MOUSE-SELECT-DBLCLICK OF c-cod-emitente-ini IN FRAME fpage0 /* Emitente */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose: Atualiza‡Æo de dados de campos em tela    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    ASSIGN c-cod-emitente-ini = p-cod-emitente-ini
           c-cod-emitente-fim = p-cod-emitente-fim
           c-cod-estabel-ini  = p-cod-estabel-ini 
           c-cod-estabel-fim  = p-cod-estabel-fim 
           c-serie-ini        = p-serie-ini       
           c-serie-fim        = p-serie-fim       
           c-nro-docto-ini    = p-nro-docto-ini   
           c-nro-docto-fim    = p-nro-docto-fim 
           d-dt-transacao-ini = p-dt-transacao-ini
           d-dt-transacao-fim = p-dt-transacao-fim             
           pl-ok              = no.

    DISP c-cod-emitente-ini
         c-cod-emitente-fim
         c-cod-estabel-ini 
         c-cod-estabel-fim 
         c-serie-ini       
         c-serie-fim       
         c-nro-docto-ini   
         c-nro-docto-fim   
         d-dt-transacao-ini  
         d-dt-transacao-fim
        WITH FRAME fPage0.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

