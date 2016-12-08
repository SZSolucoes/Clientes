&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
*/
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
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE tt-estabel NO-UNDO
    FIELD cod-estabel AS CHAR
    FIELD log-exporta AS LOG FORMAT "*/ "
    INDEX codigo IS PRIMARY UNIQUE cod-estabel
    INDEX exporta log-exporta.

/* Parameters Definitions ---                                           */
DEF NEW GLOBAL SHARED VAR gr-doc-espec AS ROWID NO-UNDO.
DEF BUFFER bnfe008 for nfe008.

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
&Scoped-define BROWSE-NAME br-estab

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-estabel estabelec

/* Definitions for BROWSE br-estab                                      */
&Scoped-define FIELDS-IN-QUERY-br-estab tt-estabel.log-exporta tt-estabel.cod-estabel estabelec.nome (not estabelec.log-1) @ estabelec.log-1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-estab   
&Scoped-define SELF-NAME br-estab
&Scoped-define QUERY-STRING-br-estab FOR EACH tt-estabel, ~
                                   FIRST estabelec                             WHERE estabelec.cod-estabel = tt-estabel.cod-estabel NO-LOCK
&Scoped-define OPEN-QUERY-br-estab OPEN QUERY {&SELF-NAME} FOR EACH tt-estabel, ~
                                   FIRST estabelec                             WHERE estabelec.cod-estabel = tt-estabel.cod-estabel NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-estab tt-estabel estabelec
&Scoped-define FIRST-TABLE-IN-QUERY-br-estab tt-estabel
&Scoped-define SECOND-TABLE-IN-QUERY-br-estab estabelec


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-estab}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 br-estab bt-marca bt-inverte bt-ok ~
bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-nome-estab c-estabelecimento i-emitente ~
c-nome-emit c-natureza c-desc-natur c-serie 

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

DEFINE BUTTON bt-inverte 
     LABEL "&Inverte" 
     SIZE 10 BY 1 TOOLTIP "Inverte".

DEFINE BUTTON bt-marca 
     LABEL "&Marca" 
     SIZE 10 BY 1 TOOLTIP "Marca".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-desc-natur AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 46.72 BY .88.

DEFINE VARIABLE c-estabelecimento AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento":R9 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-natureza AS CHARACTER FORMAT "X(6)":U 
     LABEL "Natureza Opera‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-emit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 46.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-estab AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 46.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "x(3)" 
     LABEL "S‚rie":R9 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-estab FOR 
      tt-estabel, 
      estabelec SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-estab w-window _FREEFORM
  QUERY br-estab DISPLAY
      tt-estabel.log-exporta
      tt-estabel.cod-estabel
      estabelec.nome
      (not estabelec.log-1) @ estabelec.log-1
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77.86 BY 8.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-nome-estab AT ROW 1.25 COL 30.29 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     c-estabelecimento AT ROW 1.29 COL 20 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" WIDGET-ID 8
     i-emitente AT ROW 2.25 COL 20 COLON-ALIGNED WIDGET-ID 6
     c-nome-emit AT ROW 2.25 COL 30.14 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     c-natureza AT ROW 3.25 COL 20 COLON-ALIGNED WIDGET-ID 10
     c-desc-natur AT ROW 3.25 COL 30.14 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     c-serie AT ROW 4.21 COL 20 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" WIDGET-ID 18
     br-estab AT ROW 5.21 COL 2 WIDGET-ID 200
     bt-marca AT ROW 13.71 COL 2 HELP
          "Marca" WIDGET-ID 4
     bt-inverte AT ROW 13.71 COL 12 HELP
          "Inverte" WIDGET-ID 2
     bt-ok AT ROW 15.08 COL 3
     bt-cancelar AT ROW 15.08 COL 14
     bt-ajuda AT ROW 15.08 COL 69
     RECT-1 AT ROW 14.88 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.42 WIDGET-ID 100.


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
         HEIGHT             = 15.5
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
/* BROWSE-TAB br-estab c-serie F-Main */
/* SETTINGS FOR FILL-IN c-desc-natur IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-estabelecimento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-natureza IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-estab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-emitente IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-estab
/* Query rebuild information for BROWSE br-estab
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-estabel,
                            FIRST estabelec
                            WHERE estabelec.cod-estabel = tt-estabel.cod-estabel NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-estab */
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


&Scoped-define BROWSE-NAME br-estab
&Scoped-define SELF-NAME br-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estab w-window
ON MOUSE-SELECT-DBLCLICK OF br-estab IN FRAME F-Main
DO:
    apply "choose" to bt-marca in frame {&frame-name}.
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


&Scoped-define SELF-NAME bt-inverte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inverte w-window
ON CHOOSE OF bt-inverte IN FRAME F-Main /* Inverte */
DO:
    FOR EACH tt-estabel:
        ASSIGN tt-estabel.log-exporta = NOT tt-estabel.log-exporta.
    END.

    {&OPEN-QUERY-{&browse-name}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-window
ON CHOOSE OF bt-marca IN FRAME F-Main /* Marca */
DO:
    IF  AVAIL tt-estabel THEN DO:
        ASSIGN tt-estabel.log-exporta = NOT tt-estabel.log-exporta. /**/
        DISP tt-estabel.log-exporta WITH BROWSE {&browse-name}.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    FOR EACH tt-estabel
        WHERE tt-estabel.log-exporta = YES:

        FIND bnfe008 EXCLUSIVE-LOCK
            WHERE bnfe008.cod-emitente = i-emitente
              AND bnfe008.cod-estabel  = tt-estabel.cod-estabel NO-ERROR.

        IF NOT AVAIL bnfe008 THEN DO:
            CREATE bnfe008.
            ASSIGN bnfe008.cod-estabel  = tt-estabel.cod-estabel
                   bnfe008.cod-emitente = i-emitente
                   bnfe008.nat-operacao = c-natureza
                   bnfe008.serie        = c-serie.
        END.
        ELSE
            ASSIGN bnfe008.nat-operacao = c-natureza
                   bnfe008.serie        = c-serie.
    END.

        
    APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */
{utp/ut-liter.i Marca   * R} 
assign bt-marca:label   in frame {&frame-name} = "&":U + trim(return-value).
{utp/ut-liter.i Inverte * R} 
assign bt-inverte:label in frame {&frame-name} = "&":U + trim(return-value).
{utp/ut-liter.i Mod * R} 
assign tt-estabel.log-exporta:label in browse {&browse-name} = trim(return-value).

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
  DISPLAY c-nome-estab c-estabelecimento i-emitente c-nome-emit c-natureza 
          c-desc-natur c-serie 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 br-estab bt-marca bt-inverte bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESNF001A" "2.06.00.001"}

    FIND nfe008
        WHERE ROWID(nfe008) =  gr-doc-espec NO-LOCK NO-ERROR.
        
    ASSIGN c-estabelecimento  = nfe008.cod-estabel
           i-emitente         = nfe008.cod-emitente
           c-natureza         = nfe008.nat-operacao
           c-serie            = nfe008.serie.

    FOR EACH estabelec NO-LOCK:

        IF estabelec.cod-estabel = c-estabelecimento THEN
            NEXT.

       CREATE tt-estabel.
       BUFFER-COPY estabelec TO tt-estabel
       ASSIGN tt-estabel.log-exporta = YES . 
        
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    FIND FIRST estabelec
        WHERE estabelec.cod-estabel = c-estabelecimento NO-LOCK NO-ERROR .

    IF AVAIL estabelec THEN
        ASSIGN c-nome-estab:SCREEN-VALUE IN FRAME f-main  = estabelec.nome .

    ELSE
        ASSIGN c-nome-estab:SCREEN-VALUE IN FRAME f-main  = "".

    FIND FIRST emitente
        WHERE emitente.cod-emitente = i-emitente NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN
        ASSIGN c-nome-emit:SCREEN-VALUE IN FRAME f-main  = emitente.nome-emit .

    ELSE
        ASSIGN c-nome-emit:SCREEN-VALUE IN FRAME f-main  = "".

    FIND FIRST natur-oper
        WHERE natur-oper.nat-operacao = c-natureza:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR .

    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME f-main  = natur-oper.denominacao .

    ELSE
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME f-main  = "".

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-estabel"}
  {src/adm/template/snd-list.i "estabelec"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

