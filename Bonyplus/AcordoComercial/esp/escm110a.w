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
{include/i-prgvrs.i DWRE013A 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAM p-cod-estab-ini        LIKE estabelec.cod-estabel                NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-estab-fim        LIKE estabelec.cod-estabel                NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-ini     LIKE emitente.cod-emitente                NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-emitente-fim     LIKE emitente.cod-emitente                NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-area-ini         LIKE es-acordo-area.cod-area              NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-area-fim         LIKE es-acordo-area.cod-area              NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-repres-ini       LIKE es-acordo-comerc.cod-repres          NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-cod-repres-fim       LIKE es-acordo-comerc.cod-repres          NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-dt-criacao-ini       LIKE es-acordo-comerc.dt-criacao          NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-dt-criacao-fim       LIKE es-acordo-comerc.dt-criacao          NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-ind-situacao         LIKE es-acordo-pendencia.ind-situacao     NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-sit-acordo-aceite    LIKE es-acordo-comerc.sit-acordo-aceite   NO-UNDO. 
DEFINE INPUT-OUTPUT PARAM p-sit-acordo-contabil  LIKE es-acordo-comerc.sit-acordo-contabil NO-UNDO. 

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
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 ~
IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-9 IMAGE-24 IMAGE-25 RECT-2 RECT-3 RECT-4 ~
c-cod-estab-ini c-cod-estab-fim c-cod-emitente-ini c-cod-emitente-fim ~
c-cod-area-fim c-cod-area-ini c-cod-repres-ini c-cod-repres-fim ~
d-dt-criacao-ini d-dt-criacao-fim rs-acordo-app rs-liber-aceite ~
rs-liber-contabil bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-cod-estab-ini c-cod-estab-fim ~
c-cod-emitente-ini c-cod-emitente-fim c-cod-area-fim c-cod-area-ini ~
c-cod-repres-ini c-cod-repres-fim d-dt-criacao-ini d-dt-criacao-fim ~
rs-acordo-app rs-liber-aceite rs-liber-contabil 

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

DEFINE VARIABLE c-cod-area-fim AS INTEGER FORMAT ">>>>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE c-cod-area-ini AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Area":R7 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE c-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente":R8 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estab-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estab-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estab.":R8 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-repres-fim AS INTEGER FORMAT ">>>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE c-cod-repres-ini AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Representante":R17 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE d-dt-criacao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE d-dt-criacao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1800 
     LABEL "Data Criaá∆o":R12 
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

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3.29 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-acordo-app AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pendente Acordo", 1,
"Aprovado Acordo", 2,
"Reprovado Acordo", 3,
"Todos", 4
     SIZE 20 BY 3 NO-UNDO.

DEFINE VARIABLE rs-liber-aceite AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pendente Aceite", 1,
"Autorizado Aceite", 2,
"Todos", 3
     SIZE 20.29 BY 2.21 NO-UNDO.

DEFINE VARIABLE rs-liber-contabil AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pendente Cont†bil", 1,
"Autorizado Cont†bil", 2,
"Todos", 3
     SIZE 20.29 BY 2.21 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.72 BY 3.79.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.72 BY 3.79.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.72 BY 3.79.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod-estab-ini AT ROW 1.75 COL 21 COLON-ALIGNED WIDGET-ID 72
     c-cod-estab-fim AT ROW 1.75 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     c-cod-emitente-ini AT ROW 2.75 COL 21 COLON-ALIGNED WIDGET-ID 4
     c-cod-emitente-fim AT ROW 2.75 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-cod-area-fim AT ROW 3.67 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     c-cod-area-ini AT ROW 3.71 COL 21 COLON-ALIGNED WIDGET-ID 16
     c-cod-repres-ini AT ROW 4.71 COL 21 COLON-ALIGNED WIDGET-ID 12
     c-cod-repres-fim AT ROW 4.71 COL 46.57 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     d-dt-criacao-ini AT ROW 5.71 COL 21 COLON-ALIGNED HELP
          "Data de emiss∆o da nota fiscal" WIDGET-ID 20
     d-dt-criacao-fim AT ROW 5.71 COL 46.57 COLON-ALIGNED HELP
          "Data de emiss∆o da nota fiscal" NO-LABEL WIDGET-ID 18
     rs-acordo-app AT ROW 8.04 COL 5.14 NO-LABEL WIDGET-ID 90
     rs-liber-aceite AT ROW 8.04 COL 31.72 NO-LABEL WIDGET-ID 96
     rs-liber-contabil AT ROW 8.04 COL 56.43 NO-LABEL WIDGET-ID 100
     bt-ok AT ROW 12.08 COL 3
     bt-cancelar AT ROW 12.08 COL 14
     bt-ajuda AT ROW 12.08 COL 69
     "Liber. Cont†bil" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 7.33 COL 56.14 WIDGET-ID 88
     "Liber. Aceite" VIEW-AS TEXT
          SIZE 15.86 BY .67 AT ROW 7.33 COL 31.29 WIDGET-ID 86
     "Sit.Acordo App" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 7.33 COL 4.57 WIDGET-ID 84
     RECT-1 AT ROW 11.88 COL 2
     IMAGE-1 AT ROW 4.71 COL 40.57 WIDGET-ID 22
     IMAGE-10 AT ROW 3.71 COL 44.72 WIDGET-ID 24
     IMAGE-11 AT ROW 5.71 COL 40.57 WIDGET-ID 26
     IMAGE-12 AT ROW 5.71 COL 44.72 WIDGET-ID 28
     IMAGE-2 AT ROW 4.71 COL 44.72 WIDGET-ID 30
     IMAGE-3 AT ROW 2.75 COL 40.72 WIDGET-ID 32
     IMAGE-4 AT ROW 2.75 COL 44.72 WIDGET-ID 34
     IMAGE-9 AT ROW 3.71 COL 40.57 WIDGET-ID 40
     IMAGE-24 AT ROW 1.75 COL 40.72 WIDGET-ID 74
     IMAGE-25 AT ROW 1.75 COL 44.72 WIDGET-ID 76
     RECT-2 AT ROW 7.71 COL 3.43 WIDGET-ID 78
     RECT-3 AT ROW 7.71 COL 29.43 WIDGET-ID 80
     RECT-4 AT ROW 7.71 COL 55.14 WIDGET-ID 82
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
         HEIGHT             = 12.5
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
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    ASSIGN p-cod-estab-ini       = INPUT FRAME {&FRAME-NAME} c-cod-estab-ini
           p-cod-estab-fim       = INPUT FRAME {&FRAME-NAME} c-cod-estab-fim
           p-cod-emitente-ini    = INPUT FRAME {&FRAME-NAME} c-cod-emitente-ini
           p-cod-emitente-fim    = INPUT FRAME {&FRAME-NAME} c-cod-emitente-fim
           p-cod-area-ini        = INPUT FRAME {&FRAME-NAME} c-cod-area-ini
           p-cod-area-fim        = INPUT FRAME {&FRAME-NAME} c-cod-area-fim
           p-cod-repres-ini      = INPUT FRAME {&FRAME-NAME} c-cod-repres-ini
           p-cod-repres-fim      = INPUT FRAME {&FRAME-NAME} c-cod-repres-fim
           p-dt-criacao-ini      = INPUT FRAME {&FRAME-NAME} d-dt-criacao-ini
           p-dt-criacao-fim      = INPUT FRAME {&FRAME-NAME} d-dt-criacao-fim
           p-ind-situacao        = INPUT FRAME {&FRAME-NAME} rs-acordo-app
           p-sit-acordo-aceite   = INPUT FRAME {&FRAME-NAME} rs-liber-aceite
           p-sit-acordo-contabil = INPUT FRAME {&FRAME-NAME} rs-liber-contabil.
    
           
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
  DISPLAY c-cod-estab-ini c-cod-estab-fim c-cod-emitente-ini c-cod-emitente-fim 
          c-cod-area-fim c-cod-area-ini c-cod-repres-ini c-cod-repres-fim 
          d-dt-criacao-ini d-dt-criacao-fim rs-acordo-app rs-liber-aceite 
          rs-liber-contabil 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-2 IMAGE-3 IMAGE-4 
         IMAGE-9 IMAGE-24 IMAGE-25 RECT-2 RECT-3 RECT-4 c-cod-estab-ini 
         c-cod-estab-fim c-cod-emitente-ini c-cod-emitente-fim c-cod-area-fim 
         c-cod-area-ini c-cod-repres-ini c-cod-repres-fim d-dt-criacao-ini 
         d-dt-criacao-fim rs-acordo-app rs-liber-aceite rs-liber-contabil bt-ok 
         bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESCM110A" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

   ASSIGN c-cod-estab-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(p-cod-estab-ini)     
          c-cod-estab-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(p-cod-estab-fim) 
          c-cod-emitente-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-cod-emitente-ini)     
          c-cod-emitente-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-cod-emitente-fim)
          c-cod-area-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(p-cod-area-ini)      
          c-cod-area-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(p-cod-area-fim)      
          c-cod-repres-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(p-cod-repres-ini)    
          c-cod-repres-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(p-cod-repres-fim)
          d-dt-criacao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(p-dt-criacao-ini)    
          d-dt-criacao-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(p-dt-criacao-fim)
          rs-acordo-app:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(p-ind-situacao)
          rs-liber-aceite:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(p-sit-acordo-aceite)
          rs-liber-contabil:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(p-sit-acordo-contabil).

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

