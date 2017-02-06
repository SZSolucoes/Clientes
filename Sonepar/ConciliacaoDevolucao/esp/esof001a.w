&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESOF001A 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAM p-cod-estabel-ini   LIKE doc-fiscal.cod-estabel     NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-estabel-fim   LIKE doc-fiscal.cod-estabel     NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-serie-ini         LIKE doc-fiscal.serie           NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-serie-fim         LIKE doc-fiscal.serie           NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nr-doc-fis-ini    LIKE doc-fiscal.nr-doc-fis      NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nr-doc-fis-fim    LIKE doc-fiscal.nr-doc-fis      NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-ini  LIKE doc-fiscal.cod-emitente    NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-fim  LIKE doc-fiscal.cod-emitente    NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nat-operacao-ini  LIKE doc-fiscal.nat-operacao    NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-nat-operacao-fim  LIKE doc-fiscal.nat-operacao    NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-dt-transacao-ini  LIKE doc-fiscal.dt-impl         NO-UNDO.
DEFINE INPUT-OUTPUT PARAM p-dt-transacao-fim  LIKE doc-fiscal.dt-impl         NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-3 IMAGE-4 IMAGE-24 IMAGE-25 ~
IMAGE-26 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 IMAGE-33 ~
c-cod-estabel-ini c-cod-estabel-fim c-serie-ini c-serie-fim ~
c-nr-doc-fis-ini c-nr-doc-fis-fim i-cod-emitente-ini i-cod-emitente-fim ~
c-nat-operacao-ini c-nat-operacao-fim d-dt-transacao-ini d-dt-transacao-fim ~
bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-cod-estabel-ini c-cod-estabel-fim ~
c-serie-ini c-serie-fim c-nr-doc-fis-ini c-nr-doc-fis-fim ~
i-cod-emitente-ini i-cod-emitente-fim c-nat-operacao-ini c-nat-operacao-fim ~
d-dt-transacao-ini d-dt-transacao-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estab.":R8 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-operacao-fim AS CHARACTER FORMAT "x(06)" INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-operacao-ini AS CHARACTER FORMAT "x(06)" 
     LABEL "Natureza Opera‡Æo":R21 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-doc-fis-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-doc-fis-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento Fiscal":R20 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-fim AS CHARACTER FORMAT "X(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-ini AS CHARACTER FORMAT "X(5)" 
     LABEL "S‚rie":R8 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-transacao-fim AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-transacao-ini AS DATE FORMAT "99/99/9999" INITIAL ? 
     LABEL "Dt Implanta‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente":R8 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-fir":U
     SIZE 3.29 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3.29 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3.29 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3.29 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-fir":U
     SIZE 3.29 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod-estabel-ini AT ROW 2.33 COL 21 COLON-ALIGNED WIDGET-ID 72
     c-cod-estabel-fim AT ROW 2.33 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     c-serie-ini AT ROW 3.33 COL 21 COLON-ALIGNED WIDGET-ID 4
     c-serie-fim AT ROW 3.33 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-nr-doc-fis-ini AT ROW 4.33 COL 21 COLON-ALIGNED HELP
          "N£mero do documento fiscal" WIDGET-ID 106
     c-nr-doc-fis-fim AT ROW 4.33 COL 46.57 COLON-ALIGNED HELP
          "N£mero do documento fiscal" NO-LABEL WIDGET-ID 104
     i-cod-emitente-ini AT ROW 5.33 COL 21 COLON-ALIGNED WIDGET-ID 114
     i-cod-emitente-fim AT ROW 5.33 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     c-nat-operacao-ini AT ROW 6.33 COL 21 COLON-ALIGNED HELP
          "Natureza de opera‡Æo" WIDGET-ID 122
     c-nat-operacao-fim AT ROW 6.33 COL 46.57 COLON-ALIGNED HELP
          "Natureza de opera‡Æo" NO-LABEL WIDGET-ID 120
     d-dt-transacao-ini AT ROW 7.33 COL 21 COLON-ALIGNED HELP
          "Data da implanta‡Æo do docto fiscal" WIDGET-ID 130
     d-dt-transacao-fim AT ROW 7.33 COL 46.57 COLON-ALIGNED HELP
          "Data da implanta‡Æo do docto fiscal" NO-LABEL WIDGET-ID 128
     bt-ok AT ROW 9.75 COL 3
     bt-cancelar AT ROW 9.75 COL 14
     bt-ajuda AT ROW 9.75 COL 69
     RECT-1 AT ROW 9.54 COL 2
     IMAGE-3 AT ROW 3.33 COL 40.72 WIDGET-ID 32
     IMAGE-4 AT ROW 3.33 COL 44.72 WIDGET-ID 34
     IMAGE-24 AT ROW 2.33 COL 40.72 WIDGET-ID 74
     IMAGE-25 AT ROW 2.33 COL 44.72 WIDGET-ID 76
     IMAGE-26 AT ROW 4.33 COL 40.72 WIDGET-ID 108
     IMAGE-27 AT ROW 4.33 COL 44.72 WIDGET-ID 110
     IMAGE-28 AT ROW 5.33 COL 40.72 WIDGET-ID 116
     IMAGE-29 AT ROW 5.33 COL 44.72 WIDGET-ID 118
     IMAGE-30 AT ROW 6.33 COL 40.72 WIDGET-ID 124
     IMAGE-31 AT ROW 6.33 COL 44.72 WIDGET-ID 126
     IMAGE-32 AT ROW 7.33 COL 40.72 WIDGET-ID 132
     IMAGE-33 AT ROW 7.33 COL 44.72 WIDGET-ID 134
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.75 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 10.13
         WIDTH              = 80
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
   APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    ASSIGN p-cod-estabel-ini  = INPUT FRAME {&FRAME-NAME} c-cod-estabel-ini         
           p-cod-estabel-fim  = INPUT FRAME {&FRAME-NAME} c-cod-estabel-fim   
           p-serie-ini        = INPUT FRAME {&FRAME-NAME} c-serie-ini         
           p-serie-fim        = INPUT FRAME {&FRAME-NAME} c-serie-fim         
           p-nr-doc-fis-ini   = INPUT FRAME {&FRAME-NAME} c-nr-doc-fis-ini    
           p-nr-doc-fis-fim   = INPUT FRAME {&FRAME-NAME} c-nr-doc-fis-fim    
           p-cod-emitente-ini = INPUT FRAME {&FRAME-NAME} i-cod-emitente-ini 
           p-cod-emitente-fim = INPUT FRAME {&FRAME-NAME} i-cod-emitente-fim 
           p-nat-operacao-ini = INPUT FRAME {&FRAME-NAME} c-nat-operacao-ini 
           p-nat-operacao-fim = INPUT FRAME {&FRAME-NAME} c-nat-operacao-fim 
           p-dt-transacao-ini  = INPUT FRAME {&FRAME-NAME} d-dt-transacao-ini  
           p-dt-transacao-fim  = INPUT FRAME {&FRAME-NAME} d-dt-transacao-fim.
    
           
    APPLY "close":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY c-cod-estabel-ini c-cod-estabel-fim c-serie-ini c-serie-fim 
          c-nr-doc-fis-ini c-nr-doc-fis-fim i-cod-emitente-ini 
          i-cod-emitente-fim c-nat-operacao-ini c-nat-operacao-fim 
          d-dt-transacao-ini d-dt-transacao-fim 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-3 IMAGE-4 IMAGE-24 IMAGE-25 IMAGE-26 IMAGE-27 IMAGE-28 
         IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 IMAGE-33 c-cod-estabel-ini 
         c-cod-estabel-fim c-serie-ini c-serie-fim c-nr-doc-fis-ini 
         c-nr-doc-fis-fim i-cod-emitente-ini i-cod-emitente-fim 
         c-nat-operacao-ini c-nat-operacao-fim d-dt-transacao-ini 
         d-dt-transacao-fim bt-ok bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "ESOF001A" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

   ASSIGN c-cod-estabel-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(p-cod-estabel-ini)  
          c-cod-estabel-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(p-cod-estabel-fim)  
          c-serie-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(p-serie-ini)        
          c-serie-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(p-serie-fim)        
          c-nr-doc-fis-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(p-nr-doc-fis-ini)   
          c-nr-doc-fis-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(p-nr-doc-fis-fim)   
          i-cod-emitente-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-cod-emitente-ini)
          i-cod-emitente-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-cod-emitente-fim)
          c-nat-operacao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-nat-operacao-ini)
          c-nat-operacao-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-nat-operacao-fim)
          d-dt-transacao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(p-dt-transacao-ini) 
          d-dt-transacao-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(p-dt-transacao-fim). 

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

