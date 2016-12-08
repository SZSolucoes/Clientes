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
{include/i-prgvrs.i ESNF003B 2.00.00.000}  /*** 010000 ***/

/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF003B
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE page0Widgets   c-cod-emitente-ini c-cod-emitente-fim ~
                              c-cod-estabel-ini c-cod-estabel-fim ~
                              c-serie-ini c-serie-fim ~
                              c-nro-docto-ini c-nro-docto-fim ~
                              d-dt-emissao-ini d-dt-emissao-fim ~
                              btHelp btOK btCancel 

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-ini LIKE emitente.cod-emitente NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-fim LIKE emitente.cod-emitente NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-estabel-ini  LIKE docum-est.cod-estabel NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-estabel-fim  LIKE docum-est.cod-estabel NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-serie-ini        LIKE docum-est.serie       NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-serie-fim        LIKE docum-est.serie       NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nro-docto-ini    LIKE docum-est.nro-docto   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nro-docto-fim    LIKE docum-est.nro-docto   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-dt-emissao-ini   LIKE docum-est.dt-emis     NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-dt-emissao-fim   LIKE docum-est.dt-emis     NO-UNDO.

/* Local Variable Definitions ---                                       */

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
c-nro-docto-fim d-dt-emissao-ini d-dt-emissao-fim btOK btCancel btHelp ~
IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-7 IMAGE-8 ~
IMAGE-9 rtToolBar 
&Scoped-Define DISPLAYED-OBJECTS c-cod-emitente-ini c-cod-emitente-fim ~
c-cod-estabel-ini c-cod-estabel-fim c-serie-ini c-serie-fim c-nro-docto-ini ~
c-nro-docto-fim d-dt-emissao-ini d-dt-emissao-fim 

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

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(3)" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
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

DEFINE VARIABLE d-dt-emissao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE d-dt-emissao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1800 
     LABEL "Data EmissÆo":R12 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

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
     c-cod-emitente-ini AT ROW 2.5 COL 21 COLON-ALIGNED
     c-cod-emitente-fim AT ROW 2.5 COL 46.57 COLON-ALIGNED NO-LABEL
     c-cod-estabel-ini AT ROW 3.5 COL 21 COLON-ALIGNED
     c-cod-estabel-fim AT ROW 3.46 COL 46.57 COLON-ALIGNED NO-LABEL
     c-serie-ini AT ROW 4.5 COL 21 COLON-ALIGNED
     c-serie-fim AT ROW 4.46 COL 46.57 COLON-ALIGNED NO-LABEL
     c-nro-docto-ini AT ROW 5.5 COL 21 COLON-ALIGNED
     c-nro-docto-fim AT ROW 5.5 COL 46.57 COLON-ALIGNED NO-LABEL
     d-dt-emissao-ini AT ROW 6.5 COL 21 COLON-ALIGNED HELP
          "Data de emissÆo da nota fiscal"
     d-dt-emissao-fim AT ROW 6.5 COL 46.57 COLON-ALIGNED HELP
          "Data de emissÆo da nota fiscal" NO-LABEL
     btOK AT ROW 9.25 COL 2
     btCancel AT ROW 9.25 COL 13
     btHelp AT ROW 9.25 COL 68.29
     IMAGE-1 AT ROW 5.5 COL 40.57
     IMAGE-10 AT ROW 4.5 COL 44.72
     IMAGE-11 AT ROW 6.5 COL 40.57
     IMAGE-12 AT ROW 6.5 COL 44.72
     IMAGE-2 AT ROW 5.5 COL 44.72
     IMAGE-3 AT ROW 2.5 COL 40.72
     IMAGE-4 AT ROW 2.5 COL 44.72
     IMAGE-7 AT ROW 3.5 COL 40.57
     IMAGE-8 AT ROW 3.5 COL 44.72
     IMAGE-9 AT ROW 4.5 COL 40.57
     rtToolBar AT ROW 9 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.57 BY 9.54
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 9.58
         WIDTH              = 79.14
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
          p-dt-emissao-ini   = INPUT FRAME fPage0 d-dt-emissao-ini  
          p-dt-emissao-fim   = INPUT FRAME fPage0 d-dt-emissao-fim. 
   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{window/mainblock.i}

&IF "{&mguni_version}" >= "2.071" &THEN
    ASSIGN c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage0 = "ZZZZZ".
&ELSE
    ASSIGN c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage0 = "ZZZ".
&ENDIF

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

    ASSIGN c-cod-emitente-ini = p-cod-emitente-ini
           c-cod-emitente-fim = p-cod-emitente-fim
           c-cod-estabel-ini  = p-cod-estabel-ini 
           c-cod-estabel-fim  = p-cod-estabel-fim 
           c-serie-ini        = p-serie-ini       
           c-serie-fim        = p-serie-fim       
           c-nro-docto-ini    = p-nro-docto-ini   
           c-nro-docto-fim    = p-nro-docto-fim   
           d-dt-emissao-ini   = p-dt-emissao-ini 
           d-dt-emissao-fim   = p-dt-emissao-fim. 

    DISP c-cod-emitente-ini
         c-cod-emitente-fim
         c-cod-estabel-ini 
         c-cod-estabel-fim 
         c-serie-ini       
         c-serie-fim       
         c-nro-docto-ini   
         c-nro-docto-fim   
         d-dt-emissao-ini  
         d-dt-emissao-fim  
        WITH FRAME fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

