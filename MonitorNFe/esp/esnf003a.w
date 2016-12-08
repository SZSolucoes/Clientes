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
{include/i-prgvrs.i ESNF003A 2.00.00.000}  /*** 010009 ***/

/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

{cdp/cdcfgdis.i}

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF003A
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE page0Widgets   tg-digitada tg-erro-neg tg-atualizada tg-eliminada ~
                              tg-danfe tg-liberada tg-conferido c-situacao-nfe btHelp btOK btCancel 

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAM p-tg-digitada   AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-tg-erro-neg   AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-tg-atualizada AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-tg-eliminada  AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-tg-danfe      AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-tg-liberada   AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-tg-conferido  AS LOGICAL NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS tg-eliminada tg-digitada tg-erro-neg btOK ~
btCancel btHelp c-situacao-nfe tg-atualizada tg-danfe tg-liberada ~
tg-conferido RECT-1 rtToolBar 
&Scoped-Define DISPLAYED-OBJECTS tg-eliminada tg-digitada tg-erro-neg ~
c-situacao-nfe tg-atualizada tg-danfe tg-liberada tg-conferido 

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

DEFINE VARIABLE c-situacao-nfe AS CHARACTER FORMAT "X(256)":U INITIAL "Situaá∆o Documento" 
      VIEW-AS TEXT 
     SIZE 16 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45.39 BY 7.57.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 45.39 BY 1.43
     BGCOLOR 7 .

DEFINE VARIABLE tg-atualizada AS LOGICAL INITIAL no 
     LABEL "Atualizada no Recebimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .83 NO-UNDO.

DEFINE VARIABLE tg-conferido AS LOGICAL INITIAL no 
     LABEL "Documentos Conferidos" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 TOOLTIP "Documentos Conferidos" NO-UNDO.

DEFINE VARIABLE tg-danfe AS LOGICAL INITIAL no 
     LABEL "DANFE n∆o Autorizado" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 TOOLTIP "DANFE n∆o Autorizado" NO-UNDO.

DEFINE VARIABLE tg-digitada AS LOGICAL INITIAL no 
     LABEL "Digitada no Recebimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE tg-eliminada AS LOGICAL INITIAL no 
     LABEL "Eliminada no Recebimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE tg-erro-neg AS LOGICAL INITIAL no 
     LABEL "Nota com Erro Neg¢cio" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE tg-liberada AS LOGICAL INITIAL no 
     LABEL "Documentos Liberados" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 TOOLTIP "Documentos Liberados" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     tg-eliminada AT ROW 4.73 COL 8 WIDGET-ID 2
     tg-digitada AT ROW 1.73 COL 8
     tg-erro-neg AT ROW 2.73 COL 8
     btOK AT ROW 9.13 COL 2.33
     btCancel AT ROW 9.13 COL 13.33
     btHelp AT ROW 9.13 COL 35.33
     c-situacao-nfe AT ROW 1.03 COL 4 COLON-ALIGNED NO-LABEL
     tg-atualizada AT ROW 3.73 COL 8
     tg-danfe AT ROW 5.73 COL 8 WIDGET-ID 4
     tg-liberada AT ROW 6.73 COL 8 WIDGET-ID 6
     tg-conferido AT ROW 7.73 COL 8 WIDGET-ID 8
     RECT-1 AT ROW 1.27 COL 1.33
     rtToolBar AT ROW 8.9 COL 1.22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.08
         SIZE 45.72 BY 10.38
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 9.5
         WIDTH              = 46.11
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
    ASSIGN p-tg-digitada   = INPUT FRAME fPage0 tg-digitada   
           p-tg-erro-neg   = INPUT FRAME fPage0 tg-erro-neg   
           p-tg-atualizada = INPUT FRAME fPage0 tg-atualizada
           p-tg-eliminada  = INPUT FRAME fPage0 tg-eliminada
           p-tg-danfe      = INPUT FRAME fPage0 tg-danfe
           p-tg-liberada   = INPUT FRAME fPage0 tg-liberada
           p-tg-conferido  = INPUT FRAME fPage0 tg-conferido.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

    ASSIGN tg-digitada   = p-tg-digitada  
           tg-erro-neg   = p-tg-erro-neg  
           tg-atualizada = p-tg-atualizada
           tg-eliminada  = p-tg-eliminada
           tg-danfe      = p-tg-danfe
           tg-liberada   = p-tg-liberada
           tg-conferido  = p-tg-conferido.

    DISP tg-digitada  
         tg-erro-neg  
         tg-atualizada
         tg-eliminada
         tg-danfe
         tg-liberada
         tg-conferido
        WITH FRAME fPage0.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

