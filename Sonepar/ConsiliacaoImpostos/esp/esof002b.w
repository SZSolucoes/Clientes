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
{include/i-prgvrs.i ESOF002B 2.12.00.001 } /*** 010001 ***/
/********************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESOF002B
&GLOBAL-DEFINE Version        2.12.00.001

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE page0Widgets   de-total-db-ce de-total-cr-ce ~
                              de-total-db-ft de-total-cr-ft ~
                              de-total-db-of de-total-cr-of ~
                              de-total-ce-ft de-total-of de-total-dif ~
                              btHelp btOK btCancel 

DEFINE TEMP-TABLE tt-concilia-fiscal NO-UNDO 
    field cod-emitente like movto-estoq.cod-emitente
    field cod-estabel  like movto-estoq.cod-estabel
    field serie-docto  like movto-estoq.serie-docto
    field nro-docto    like movto-estoq.nro-docto
    field nat-operacao like movto-estoq.nat-operacao 
    field dt-trans     like movto-estoq.dt-trans
    field valor-ce-db as dec
    field valor-ce-cr as dec
    field valor-ft-cr as dec
    field valor-ft-db as dec
    field valor-of-cr as dec
    field valor-of-db as dec
    field valor-dif   as dec.

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAM TABLE for tt-concilia-fiscal .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS de-total-db-ce btOK btCancel btHelp ~
rtToolBar RECT-16 de-total-cr-ce de-total-db-ft de-total-cr-ft ~
de-total-cr-of de-total-db-of de-total-of de-total-ce-ft de-total-dif 
&Scoped-Define DISPLAYED-OBJECTS de-total-db-ce de-total-cr-ce ~
de-total-db-ft de-total-cr-ft de-total-cr-of de-total-db-of de-total-of ~
de-total-ce-ft de-total-dif 

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

DEFINE VARIABLE de-total-ce-ft AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total CExFT" 
     VIEW-AS FILL-IN 
     SIZE 17.29 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-cr-ce AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cr‚ditos Estoque" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-cr-ft AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cr‚ditos Faturamento" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-cr-of AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sa¡das OF" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-db-ce AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "D‚bitos Estoque" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-db-ft AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "D‚bitos Faturamento" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-db-of AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Entradas OF" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-dif AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Diferen‡a" 
     VIEW-AS FILL-IN 
     SIZE 17.29 BY .79 NO-UNDO.

DEFINE VARIABLE de-total-of AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total OF" 
     VIEW-AS FILL-IN 
     SIZE 17.29 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 6.25.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 99 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     de-total-db-ce AT ROW 2.13 COL 19.72 COLON-ALIGNED WIDGET-ID 6
     btOK AT ROW 8.25 COL 2
     btCancel AT ROW 8.25 COL 13
     btHelp AT ROW 8.25 COL 89.14
     de-total-cr-ce AT ROW 3.13 COL 19.72 COLON-ALIGNED WIDGET-ID 12
     de-total-db-ft AT ROW 4.13 COL 19.72 COLON-ALIGNED WIDGET-ID 16
     de-total-cr-ft AT ROW 5.13 COL 19.72 COLON-ALIGNED WIDGET-ID 14
     de-total-cr-of AT ROW 5.13 COL 51.43 COLON-ALIGNED WIDGET-ID 20
     de-total-db-of AT ROW 4.13 COL 51.43 COLON-ALIGNED WIDGET-ID 18
     de-total-of AT ROW 6.13 COL 51.43 COLON-ALIGNED WIDGET-ID 22
     de-total-ce-ft AT ROW 6.13 COL 19.72 COLON-ALIGNED WIDGET-ID 24
     de-total-dif AT ROW 6.13 COL 78.43 COLON-ALIGNED WIDGET-ID 26
     "Totais" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1.17 COL 2.72 WIDGET-ID 4
     rtToolBar AT ROW 8 COL 1
     RECT-16 AT ROW 1.5 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.72 BY 8.54
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
         HEIGHT             = 8.54
         WIDTH              = 99.72
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 99.72
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 99.72
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
    APPLY "CLOSE":U TO THIS-PROCEDURE.
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
    
    FOR EACH tt-concilia-fiscal:
        assign de-total-db-ce = de-total-db-ce + tt-concilia-fiscal.valor-ce-db
               de-total-cr-ce = de-total-cr-ce + tt-concilia-fiscal.valor-ce-cr
               de-total-db-ft = de-total-db-ft + tt-concilia-fiscal.valor-ft-db
               de-total-cr-ft = de-total-cr-ft + tt-concilia-fiscal.valor-ft-cr
               de-total-db-of = de-total-db-of + tt-concilia-fiscal.valor-of-db
               de-total-cr-of = de-total-cr-of + tt-concilia-fiscal.valor-of-cr.        
    END.
    assign de-total-ce-ft = ((de-total-cr-ft - de-total-db-ft) - de-total-db-ce + de-total-cr-ce)
           de-total-of    = de-total-cr-of - de-total-db-of
           de-total-dif   = abs(de-total-of - de-total-ce-ft).
           
    display de-total-db-ce
            de-total-cr-ce
            de-total-db-ft
            de-total-cr-ft
            de-total-db-of
            de-total-cr-of
            de-total-ce-ft
            de-total-of
            de-total-dif with frame {&frame-name}.   

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

